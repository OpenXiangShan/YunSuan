package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class ExpdOutput extends Bundle {
  val uop = new VUop
  val rs1 = UInt(XLEN.W) 
}

class Expander extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new VIQOutput))
    val out = Decoupled(new ExpdOutput)
    // with Busy Table
    val readBusyTable = new Bundle {
      val req = Output(new ReadReqBusyTable)
      val resp = Input(Bool()) // Stall the expander if true
    }
  })

  val fire = io.in.fire
  val raw_waw_stall = io.readBusyTable.resp
  val canOut = io.out.ready && !raw_waw_stall

  val IDLE = 0.U(1.W)
  val BUSY = 1.U(1.W)
  val state = RegInit(UInt(1.W), IDLE)
  val busy_end = Wire(Bool())

  when (state === IDLE) {
    state := Mux(fire, BUSY, IDLE)
  }.otherwise {
    state := Mux(!fire && busy_end, IDLE, BUSY)
  }

  /**
    * Calculate expanded length
    *   Pls refer to ExpdLen.scala
    */
  val expdInfo_in = io.in.bits.expdInfo
  val expdLen = expdInfo_in.expdLen
  
  /**
   * Expanded length remain and uop index update
   */
  val expdLenRemainReg = Reg(UInt(4.W))
  val expdLenRemain = Mux(fire, expdLen, expdLenRemainReg)
  val expdLenRemainRegIsZero = expdLenRemainReg === 0.U
  val uopIdxReg = Reg(UInt(3.W))
  val uopIdx = Mux(fire, 0.U, uopIdxReg)
  when (fire || canOut && state === BUSY && !expdLenRemainRegIsZero) {
    expdLenRemainReg := expdLenRemain - 1.U
    uopIdxReg := uopIdx + 1.U
  }
  val uopEnd = Mux(expdLen === 1.U, fire, uopIdx === expdLen - 1.U)

  val busy_end_logic = state === BUSY && expdLenRemainRegIsZero
  busy_end := busy_end_logic && canOut

  io.in.ready := busy_end || state === IDLE

  val out_valid = RegInit(false.B)
  when (fire) {
    out_valid := true.B
  }.elsewhen (busy_end) {
    out_valid := false.B
  }
  
  //---- Some ctrl signals should be hold during this instruction expanding process ----
  val v_ext = ctrl.alu && ctrl.funct3 === "b010".U && ctrl.funct6 === "b010010".U
  val v_ext_hold = Mux(fire, v_ext, RegEnable(v_ext, fire))
  val expdInfo_hold = Mux(fire, expdInfo_in, RegEnable(expdInfo_in, fire))
  val mop_hold = Mux(fire, io.in.bits.mop, RegEnable(io.in.bits.mop, fire))
  val rs_hold = Mux(fire, io.in.bits.rs, RegEnable(io.in.bits.rs, fire))

  /**
   * RF addresses update
   */
  val uopOut = Wire(new VUop)
  val rs1Out = Wire(UInt(XLEN.W))
  uopOut.ctrl := mop_hold.ctrl
  uopOut.csr := mop_hold.csr
  uopOut.robIdx := mop_hold.robIdx
  uopOut.veewVd := mop_hold.veewVd
  rs1Out := rs_hold.rs1

  val sew = SewOH(mop_hold.csr.vsew)
  val ctrl = mop_hold.ctrl
    
  // out lsrc(1), which is vs2
  val lsrc1_inc = Wire(UInt(3.W))
  when (ctrl.widen && !ctrl.redu || v_ext_hold && ctrl.lsrc(0)(2,1) === 3.U || expdInfo_hold.gather16 && sew.is8) {
    lsrc1_inc := uopIdx >> 1
  }.elsewhen (v_ext_hold && ctrl.lsrc(0)(2,1) === 2.U) {
    lsrc1_inc := uopIdx >> 2
  }.elsewhen (v_ext_hold && ctrl.lsrc(0)(2,1) === 1.U) {
    lsrc1_inc := uopIdx >> 3
  }.elsewhen (ctrl.funct6 === "b010100".U) { //VMUNARY0
    lsrc1_inc := 0.U
  }.otherwise {
    lsrc1_inc := uopIdx
  }
  uopOut.lsrcUop(1) := ctrl.lsrc(1) + lsrc1_inc
  // out lsrc(1) valid
  uopOut.lsrcValUop(1) := ctrl.lsrcVal(1)

  // out lsrc(0), which is vs1
  uopOut.lsrcUop(0) := ctrl.lsrc(0) +            //vcompress
            Mux(ctrl.redu || (ctrl.funct6 === "b010111".U && ctrl.funct3 === 2.U), 0.U, 
            Mux(ctrl.widen && !ctrl.redu || ctrl.widen2 || ctrl.narrow || expdInfo_hold.gather16 && sew.is32, uopIdx >> 1, 
            Mux(expdInfo_hold.gather16 && sew.is64, uopIdx >> 2, uopIdx)))
  // out lsrc(0) valid
  uopOut.lsrcValUop(0) := ctrl.lsrcVal(0)

  uopOut.lsrcValUop(2) := ctrl.lsrcVal(2)

  // Reg addr increment of SEW part reg
  val sewSide_inc = Mux(expdInfo_hold.veew_minus_vsew === 3.U, 0.U,
                 Mux(expdInfo_hold.veew_minus_vsew === 2.U, uopIdx >> 2,
                 Mux(expdInfo_hold.veew_minus_vsew === 1.U, uopIdx >> 1,
                     uopIdx)))
  val sewSide_valid = Mux(expdInfo_hold.veew_minus_vsew === 3.U, uopIdx === 7.U,
                 Mux(expdInfo_hold.veew_minus_vsew === 2.U, uopIdx(1, 0) === 3.U,
                 Mux(expdInfo_hold.veew_minus_vsew === 1.U, uopIdx(0),
                     true.B)))

  // out ldest (include store vs3)
  val ldest_inc = Wire(UInt(3.W))
  when (expdInfo_hold.ldstCtrl.indexed && ctrl.isLdst) {
    ldest_inc := sewSide_inc
  }.elsewhen (ctrl.narrow || expdInfo_hold.gather16 && sew.is8) {
    ldest_inc := uopIdx >> 1
  }.elsewhen (ctrl.redu || ctrl.narrow_to_1) {
    ldest_inc := 0.U
  }.otherwise {
    ldest_inc := uopIdx
  }
  uopOut.ldestUop := ctrl.ldest + ldest_inc

  when (expdInfo_hold.ldstCtrl.indexed && ctrl.load) {
    uopOut.ldestValUop := uopIdx < expdInfo_hold.expdLen_indexVd
  }.elsewhen (expdInfo_hold.ldstCtrl.indexed && ctrl.load) {
    uopOut.ldestValUop := sewSide_valid
  }.elsewhen (ctrl.narrow_to_1 || ctrl.redu) {
    uopOut.ldestValUop := uopIdx === expdInfo_hold.expdLen - 1.U
  }.elsewhen (ctrl.narrow || expdInfo_hold.gather16 && sew.is8) {
    uopOut.ldestValUop := uopIdx(0) || expdInfo_hold.expdLen === 1.U
  }.otherwise {
    uopOut.ldestValUop := ctrl.ldestVal
  }
  
  uopOut.lmaskValUop := !ctrl.vm

  // Final output
  val out_bits = Reg(new ExpdOutput)
  when (fire || canOut && state === BUSY) {
    out_bits.uop := uopOut
    out_bits.rs1 := rs1Out
    out_bits.uop.uopIdx := uopIdx
    out_bits.uop.uopEnd := uopEnd
  }

  // Busy Table Read
  io.readBusyTable.req.addr(0).valid := uopOut.lsrcValUop(0)
  io.readBusyTable.req.addr(1).valid := uopOut.lsrcValUop(1)
  io.readBusyTable.req.addr(2).valid := uopOut.ldestValUop || uopOut.lsrcValUop(2) // dest or 3rd operand
  io.readBusyTable.req.addr(3).valid := uopOut.lmaskValUop // mask
  io.readBusyTable.req.addr(0).bits := uopOut.lsrcUop(0)
  io.readBusyTable.req.addr(1).bits := uopOut.lsrcUop(1)
  io.readBusyTable.req.addr(2).bits := uopOut.ldestUop // dest or 3rd operand
  io.readBusyTable.req.addr(3).bits := 0.U // mask
}