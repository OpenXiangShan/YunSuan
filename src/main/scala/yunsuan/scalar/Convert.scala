package yunsuan.scalar

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.util._
import yunsuan.vector.VectorConvert.CVT64
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

  val fcvt = Module(new CVT64(xlen, isVectorCvt=false, isI2F))
  fcvt.io.fire := io.fire
  fcvt.io.src := io.src
  fcvt.io.opType := io.opType
  fcvt.io.rm := io.rm
  fcvt.io.inSew1H := inSew1H
  fcvt.io.outSew1H := outSew1H
  fcvt.io.isScalarFpInst := true.B

  io.fflags := fcvt.io.fflags
  io.result := fcvt.io.result

}
