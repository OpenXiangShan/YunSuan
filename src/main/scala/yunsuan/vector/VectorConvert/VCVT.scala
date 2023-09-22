package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._

class CVTIO(width: Int) extends Bundle {
  val src = Input(UInt(width.W))
  val opType = Input(UInt(8.W))
  val sew = Input(UInt(2.W))
  val rm = Input(UInt(3.W))
  val input1H = Input(UInt(4.W))
  val output1H = Input(UInt(4.W))
  val result = Output(UInt(width.W))
  val fflags = Output(UInt(5.W))
}

abstract class CVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
}

class VCVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
  val vcvtImpl = width match {
    case 16 => Module(new CVT16(16))
    case 32 => Module(new CVT32(32))
    case 64 => Module(new CVT64(64))
  }
  io <> vcvtImpl.io
}
object VCVT {
  def apply(
             width: Int
           )(input:   UInt,
             opType:  UInt,
             sew:     UInt,
             rm:      UInt,
             input1H:      UInt,
             output1H:      UInt
           ): (UInt, UInt) = {
    val vcvtWraper = Module(new VCVT(width))
    vcvtWraper.io.src := input
    vcvtWraper.io.opType := opType
    vcvtWraper.io.sew := sew
    vcvtWraper.io.rm := rm
    vcvtWraper.io.input1H := input1H
    vcvtWraper.io.output1H := output1H
    (vcvtWraper.io.result, vcvtWraper.io.fflags)
  }
}