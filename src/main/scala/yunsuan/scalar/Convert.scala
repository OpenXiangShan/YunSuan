package yunsuan.scalar

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.util._
import yunsuan.vector.VectorConvert.{CVT64, CVT64NarrowConvert}
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

// Scalar Float to Int or Float Convert.
class FpCvtIO(width: Int) extends Bundle {
  val fire     = Input(Bool())
  val src      = Input(UInt(width.W))
  val opType   = Input(FCvtOpcode())
  val rm       = Input(Frm())
  val inSew1H  = Input(Sew())
  val outSew1H = Input(Sew())

  val result = Output(UInt(width.W))
  val fflags = Output(Fflags())
}

class FPCVT(xlen :Int, isI2F: Boolean = false) extends Module{
  val io = IO(new FpCvtIO(xlen))
  val inSew1H = io.inSew1H
  val outSew1H = io.outSew1H
  val isFpNarrow = FCvtOpcode.isF2F(io.opType) && FCvtOpcode.getInputDataWidth(io.opType) > FCvtOpcode.getOutputDataWidth(io.opType)

  val fcvt = Module(new CVT64(xlen, isVectorCvt=false, isI2F))
  fcvt.io.in.fire := io.fire && (isI2F.B || !isFpNarrow)
  fcvt.io.in.data.src := io.src
  fcvt.io.in.ctrl.opType := io.opType
  fcvt.io.in.ctrl.rm := io.rm
  fcvt.io.in.ctrl.inSew1H := inSew1H
  fcvt.io.in.ctrl.outSew1H := outSew1H
  fcvt.io.in.ctrl.isScalarFpInst := true.B

  if (!isI2F) {
    val narrowCvt = Module(new CVT64NarrowConvert(xlen))
    val fireReg = RegEnable(io.fire, false.B, io.fire)
    val isFpNarrowReg = RegEnable(isFpNarrow, false.B, io.fire)
    val isFpNarrowEx2 = RegEnable(isFpNarrowReg, false.B, fireReg)
    narrowCvt.io.in.fire := io.fire && isFpNarrow
    narrowCvt.io.in.data.src := io.src
    narrowCvt.io.in.ctrl.opType := io.opType
    narrowCvt.io.in.ctrl.rm := io.rm
    narrowCvt.io.in.ctrl.inSew1H := inSew1H
    narrowCvt.io.in.ctrl.outSew1H := outSew1H
    narrowCvt.io.in.ctrl.isScalarFpInst := true.B
    io.fflags := Mux(isFpNarrowEx2, narrowCvt.io.out.ex2.fflags, fcvt.io.out.ex2.fflags)
    io.result := Mux(isFpNarrowEx2, narrowCvt.io.out.ex2.res, fcvt.io.out.ex2.res)
  } else {
    io.fflags := fcvt.io.out.ex2.fflags
    io.result := fcvt.io.out.ex2.res
  }
}