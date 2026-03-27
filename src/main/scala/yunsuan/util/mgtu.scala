package yunsuan.util

import chisel3._
import chisel3.util._

class Mgtu(vlen: Int) extends  Module {
  val io = IO(new MgtuIO(vlen))

  val in = io.in
  val vd = in.vd
  val vl = in.vl

  /*
   * Mask destination tail elements are always treated as tail-agnostic, regardless of the setting of vta
   */
  private val vdWithTail = Wire(Vec(vlen, UInt(1.W)))
  vdWithTail.zipWithIndex.foreach{ case (bit, idx) =>
    bit := Mux(idx.U < vl, vd(idx), 1.U)
  }

  io.out.vd := vdWithTail.asUInt
}


class MgtuIO(vlen: Int) extends Bundle {
  val in = new Bundle {
    val vd = Input(UInt(vlen.W))
    val vl = Input(UInt(8.W))
  }
  val out = new Bundle {
    val vd = Output(UInt(vlen.W))
  }
}
