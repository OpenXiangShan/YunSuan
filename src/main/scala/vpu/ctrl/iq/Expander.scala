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
    // with Scoreboard
    val readScoreboard = new Bundle {
      val req = Output(new VUop)
      val resp = Input(Bool()) // Stall the expander if true
    }
  })

  val fire = io.in.fire
  val stall_scoreboard = io.readScoreboard.resp
  val canOut = io.out.ready && !stall_scoreboard

  /**
    * Calculate expanded length
    *   Pls refer to ExpdLen.scala
    */
  val expdInfo_in = io.in.bits.expdInfo
  val expdLen = expdInfo_in.expdLen

  val IDLE = 0.U(1.W)
  val BUSY = 1.U(1.W)
  val state = RegInit(UInt(1.W), IDLE)
  val busy_end = Wire(Bool())

  when (state === IDLE) {
    state := Mux(fire && expdLen =/= 1.U, BUSY, IDLE)
  }.otherwise {
    state := Mux(busy_end, IDLE, BUSY)
  }
  
  /**
   * Expanded length remain and uop index update
   */
  val expdLenRemainReg = Reg(UInt(4.W))
  val expdLenRemain = Mux(state === IDLE, expdLen, expdLenRemainReg)
  val expdLenRemainRegIsZero = expdLenRemainReg === 0.U
  val uopIdxReg = Reg(UInt(3.W))
  val uopIdx = Mux(state === IDLE, 0.U, uopIdxReg)
  val paddrReg = Reg(UInt(PAddrBits.W)) 
  val paddr = Mux(state === IDLE, io.in.bits.rs.rs1, paddrReg)
  when (state === IDLE || canOut && state === BUSY && !expdLenRemainRegIsZero) {
    expdLenRemainReg := expdLenRemain - 1.U
    uopIdxReg := uopIdx + 1.U
    paddrReg := paddr + vlenb.U // Note: only for unit-stride load/store
  }
  // val uopEnd = uopIdx === expdLen - 1.U

  val busy_end_logic = state === BUSY && expdLenRemainReg === 1.U
  busy_end := busy_end_logic && canOut
  
  val out_valid = RegInit(false.B)
  when (fire || canOut && state === BUSY) {
    out_valid := true.B
  }.elsewhen (io.out.ready) {
    out_valid := false.B
  }
  io.out.valid := out_valid

  io.in.ready := state === IDLE && canOut || state === IDLE && !io.out.valid
  
  //---- Some ctrl signals should be hold during this instruction expanding process ----
  val v_ext = io.in.bits.mop.ctrl.alu && io.in.bits.mop.ctrl.funct3 === "b010".U && io.in.bits.mop.ctrl.funct6 === "b010010".U
  val v_ext_reg = RegEnable(v_ext, fire)
  val expdInfo_reg = RegEnable(expdInfo_in, fire)
  val mop_reg = RegEnable(io.in.bits.mop, fire)
  val rs_reg = RegEnable(io.in.bits.rs, fire)

  val v_ext_hold = Mux(state === IDLE, v_ext, v_ext_reg)
  val expdInfo_hold = Mux(state === IDLE, expdInfo_in, expdInfo_reg)
  val mop_hold = Mux(state === IDLE, io.in.bits.mop, mop_reg)

  /**
   * RF addresses update
   */
  val uopOut = Wire(new VUop)
  uopOut.ctrl := mop_hold.ctrl
  uopOut.csr := mop_hold.csr
  uopOut.robIdx := mop_hold.robIdx
  uopOut.veewVd := mop_hold.veewVd
  uopOut.uopIdx := uopIdx
  uopOut.uopEnd := uopIdx === expdInfo_hold.expdLen - 1.U
  if (debugMode) { uopOut.emulVd.get := mop_hold.emulVd }

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
  val out_bits_reg = Reg(new ExpdOutput)
  val out_paddr_reg = Reg(UInt(PAddrBits.W))
  when (fire || canOut && state === BUSY) {
    out_bits_reg.uop := uopOut
    out_paddr_reg := paddr
  }
  
  io.out.bits.uop.ctrl := mop_reg.ctrl
  io.out.bits.uop.csr := mop_reg.csr
  io.out.bits.rs1 := Mux(mop_reg.ctrl.arith, rs_reg.rs1, out_paddr_reg)
  io.out.bits.uop.robIdx := mop_reg.robIdx
  io.out.bits.uop.veewVd := mop_reg.veewVd
  if (debugMode) { io.out.bits.uop.emulVd.get := mop_reg.emulVd }
  
  // For uop members that updates in each uop-idx
  io.out.bits.uop.uopIdx := out_bits_reg.uop.uopIdx
  io.out.bits.uop.uopEnd := out_bits_reg.uop.uopEnd
  io.out.bits.uop.lsrcUop := out_bits_reg.uop.lsrcUop
  io.out.bits.uop.ldestUop := out_bits_reg.uop.ldestUop
  io.out.bits.uop.lsrcValUop := out_bits_reg.uop.lsrcValUop
  io.out.bits.uop.ldestValUop := out_bits_reg.uop.ldestValUop
  io.out.bits.uop.lmaskValUop := out_bits_reg.uop.lmaskValUop

  // Scoreboard Read
  io.readScoreboard.req := uopOut
}