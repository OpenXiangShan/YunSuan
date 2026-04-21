package yunsuan.fpu.fmul

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.FloatMulOutput
import yunsuan.fpu.fmul.utils._

class FloatMULS2 extends Module {
  val io = IO(new Bundle() {
    val inFromS1 = Input(new FloatMULS1ToS2Bundle)
    val out = Output(new FloatMulOutput)
  })
  val isF16 = io.inFromS1.isF16
  val isF32 = io.inFromS1.isF32
  val isF64 = io.inFromS1.isF64

  val FMUL16S2 = Module(new FMULS2(16))
  FMUL16S2.io.inFromS1 := io.inFromS1.FMUL16S1ToS2
  val FMUL16OutRes = Cat(Fill(48, 1.U), FMUL16S2.io.outRes)
  val FMUL16FFlags = FMUL16S2.io.outFlags

  val FMUL32S2 = Module(new FMULS2(32))
  FMUL32S2.io.inFromS1 := io.inFromS1.FMUL32S1ToS2
  val FMUL32OutRes = Cat(Fill(32, 1.U), FMUL32S2.io.outRes)
  val FMUL32FFlags = FMUL32S2.io.outFlags

  val FMUL64S2 = Module(new FMULS2(64))
  FMUL64S2.io.inFromS1 := io.inFromS1.FMUL64S1ToS2
  val FMUL64OutRes = FMUL64S2.io.outRes
  val FMUL64FFlags = FMUL64S2.io.outFlags

  io.out.fp_result := Mux1H(Seq(
    isF16 -> FMUL16OutRes,
    isF32 -> FMUL32OutRes,
    isF64 -> FMUL64OutRes
  ))
  io.out.fflags := Mux1H(Seq(
    isF16 -> FMUL16FFlags,
    isF32 -> FMUL32FFlags,
    isF64 -> FMUL64FFlags
  ))
}
