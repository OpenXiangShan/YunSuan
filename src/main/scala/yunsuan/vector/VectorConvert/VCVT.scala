package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._
import yunsuan.vector.VectorConvert.Bundles._

class CVTIO(width: Int) extends Bundle {
  val fire     = Input(Bool())
  val src      = Input(UInt(width.W))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())
  val isScalarFpInst = Input(Bool())
  val result   = Output(UInt(width.W))
  val fflags   = Output(Fflags())
}

abstract class CVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
}

class VCVT(width: Int) extends Module{
  val io = IO(new CVTIO(width))
  val vcvtImpl = width match {
    case 16 => Module(new CVT16(16))
    case 32 => Module(new CVT32(32))
    case 64 => Module(new CVT64(64, isVectorCvt=true))
  }
  io <> vcvtImpl.io
}
object VCVT {
  def apply(
             width: Int
           )(fire:    Bool,
             input:   UInt,
             opType:  UInt,
             rm:      UInt,
             inSew1H:      UInt,
             outSew1H:      UInt,
             isScalarFpInst: Bool
           ): (UInt, UInt) = {
    val vcvtWraper = Module(new VCVT(width))
    vcvtWraper.io.fire := fire
    vcvtWraper.io.src := input
    vcvtWraper.io.opType := opType
    vcvtWraper.io.rm := rm
    vcvtWraper.io.inSew1H := inSew1H
    vcvtWraper.io.outSew1H := outSew1H
    vcvtWraper.io.isScalarFpInst := isScalarFpInst
    (vcvtWraper.io.result, vcvtWraper.io.fflags)
  }
}
