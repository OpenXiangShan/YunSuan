package vector

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import yunsuan.vector.VectorConvert.{VectorCvt, VectorCvtIO}

class VFCVTTop(xlen :Int) extends Module{
  val io = IO(new VectorCvtIO(xlen))
  val (src, opType, sew, rm) = (io.src, io.opType, io.sew, io.rm)
  val vfcvtWrapper = Module(new VectorCvt(64))

  val inputNext = Wire(UInt(64.W))
  val inputReg = RegNext(inputNext,0.U)
  inputNext := src

  vfcvtWrapper.io.src := inputReg
  vfcvtWrapper.io.opType := opType
  vfcvtWrapper.io.sew := sew
  vfcvtWrapper.io.rm := rm

  val outputNext = Wire(UInt(64.W))
  val outputReg = RegNext(outputNext, 0.U)
  outputNext := vfcvtWrapper.io.result

  io.result := outputReg
  io.fflags := vfcvtWrapper.io.fflags
}

object CVTTop extends App {
  val path = """./generated/VFCVT"""
  (new ChiselStage).execute(
    Array("--target-dir", path),
    Seq(ChiselGeneratorAnnotation(() => new VFCVTTop(64)), FirtoolOption("--disable-all-randomization"))
  )
}
