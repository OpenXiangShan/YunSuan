package race.vpu

import chisel3._
import chisel3.util._
import race.vpu.yunsuan._
import race.vpu._
import VParams._
import race.vpu.exu.laneexu.LaneExu

class VExuBlock extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VExuInput))
    val out = ValidIO(new VExuOutput)
  })

  val in = io.in.bits

  val vfred_out = Wire(new VExuOutput)
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
  vfred_out.vd              := Cat(Fill((VLEN-XLEN), 0.U), vfred.io.out.bits.result)
  vfred_out.fflags          := 0.U.asTypeOf(vfred_out.fflags)
  vfred_out.fflags(0)       := vfred.io.out.bits.fflags
  vfred_out.uop             := vfred.io.out.bits.uop

  
  val laneexu = Module(new LaneExu)
  laneexu.io.in := io.in
  
  io.out.valid := vfred.io.out.valid || laneexu.io.out.valid
  
  io.out.bits := Mux1H(
    Seq(
      vfred.io.out.valid        -> vfred_out,
      laneexu.io.out.valid      -> laneexu.io.out.bits
    )
  )

}

object VerilogVExu extends App {
  println("Generating the Vector Exu Top hardware")
  emitVerilog(new VExuBlock(), Array("--target-dir", "build/verilog_vexu"))
}