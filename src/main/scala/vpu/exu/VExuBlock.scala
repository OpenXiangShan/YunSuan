package race.vpu

import chisel3._
import chisel3.util._
import race.vpu.yunsuan._
import race.vpu._
import VParams._
import race.vpu.exu.laneexu.LaneExu
import race.vpu.exu.crosslane._
class VExuBlock extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VExuInput))
    val out = ValidIO(new VExuOutput)
  })

  val in = io.in.bits

  /**
    * Lane Exu
    */
  val laneexu = Module(new LaneExu)
  laneexu.io.in := io.in

  /**
    * Reduction (fp)
    */
  // val vfred_out = Wire(new VExuOutput)
  val vfred     = Module(new Vfreduction)
  val vfred_setuop = Wire(new Vfred_setuop)

  vfred_setuop.funct := Cat(in.uop.ctrl.funct6, in.uop.ctrl.funct3)
  vfred_setuop.vm := in.uop.ctrl.vm
  vfred_setuop.vs1 := in.uop.ctrl.lsrc(0)
  vfred_setuop.vs2 := in.uop.ctrl.lsrc(1)
  vfred_setuop.op := vfred_setuop.op_gen

  vfred.io.in.fire := io.in.valid && in.uop.ctrl.vfred
  vfred.io.in.vs1  := in.vSrc(0)(XLEN-1, 0)
  vfred.io.in.vs2  := in.vSrc(1)

  vfred.io.in.op_code       := vfred_setuop.op
  vfred.io.in.mask          := in.vSrc(3)
  vfred.io.in.uop           := in.uop
  // vfred_out.vd              := Cat(Fill((VLEN-XLEN), 0.U), vfred.io.out.bits.result)
  // vfred_out.fflags          := 0.U.asTypeOf(vfred_out.fflags)
  // vfred_out.fflags(0)       := vfred.io.out.bits.fflags
  // vfred_out.uop             := vfred.io.out.bits.uop

  /**
    * VRGather
    */
  val vrgather = Module(new VRGather)
  vrgather.io.in.valid := io.in.valid && in.uop.ctrl.vrg
  vrgather.io.in.bits.uop := io.in.bits.uop
  vrgather.io.in.bits.vs1 := io.in.bits.vSrc(0)
  vrgather.io.in.bits.vs2 := io.in.bits.vSrc(1)
  vrgather.io.in.bits.rs1 := io.in.bits.rs1

  val crosslaneOut = Wire(ValidIO(new VExuOutput))
  val valid_out_crosslane = vfred.io.out.valid || vrgather.io.out.valid
  crosslaneOut.valid := RegNext(valid_out_crosslane)
  crosslaneOut.bits.uop := RegEnable(Mux(vfred.io.out.valid, vfred.io.out.bits.uop, vrgather.io.out.bits.uop), valid_out_crosslane)
  crosslaneOut.bits.fflags := 0.U.asTypeOf(crosslaneOut.bits.fflags)
  crosslaneOut.bits.fflags(0) := RegEnable(vfred.io.out.bits.fflags, vfred.io.out.valid)
  val vd_crosslane_reg_low64b = Reg(UInt(XLEN.W))
  val vd_crosslane_reg_high = Reg(UInt((VLEN-XLEN).W))
  when (vrgather.io.out.valid) {
    vd_crosslane_reg_high := vrgather.io.out.bits.vd(VLEN-1, XLEN)
  }
  when (valid_out_crosslane) {
    vd_crosslane_reg_low64b := Mux(vfred.io.out.valid, vfred.io.out.bits.result, vrgather.io.out.bits.vd(XLEN-1, 0))
  }
  crosslaneOut.bits.vd := Cat(vd_crosslane_reg_high, vd_crosslane_reg_low64b)
  
  io.out.valid := laneexu.io.out.valid || crosslaneOut.valid
  io.out.bits := Mux1H(
    Seq(
      laneexu.io.out.valid      -> laneexu.io.out.bits,
      crosslaneOut.valid        -> crosslaneOut.bits
    )
  )

}

object VerilogVExu extends App {
  println("Generating the Vector Exu Top hardware")
  emitVerilog(new VExuBlock(), Array("--target-dir", "build/verilog_vexu"))
}