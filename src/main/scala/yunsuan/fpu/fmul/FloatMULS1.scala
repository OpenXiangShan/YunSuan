package yunsuan.fpu.fmul

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.FloatMULtoFloatAdderOutput
import yunsuan.fpu.fmul.utils._

class FloatMULS1ToS2Bundle extends Bundle {
  val FMUL16S1ToS2 = new FMULS1ToS2Bundle(16)
  val FMUL32S1ToS2 = new FMULS1ToS2Bundle(32)
  val FMUL64S1ToS2 = new FMULS1ToS2Bundle(64)
  val isF16        = Bool()
  val isF32        = Bool()
  val isF64        = Bool()
}

class FloatMULS1 extends Module {
  val io = IO(new Bundle() {
    val inFromS0  = Input(new FloatMULS0ToS1Bundle)
    val outToS2   = Output(new FloatMULS1ToS2Bundle)
    val outToFADD = Output(new FloatMULtoFloatAdderOutput)
  })
  val isF16 = io.inFromS0.isF16
  val isF32 = io.inFromS0.isF32
  val isF64 = io.inFromS0.isF64

  val FMUL16S1 = Module(new FMULS1(16))
  FMUL16S1.io.inFromS0    := io.inFromS0.FMUL16S0ToS1
  io.outToS2.FMUL16S1ToS2 := FMUL16S1.io.outToS2
  val FMUL16OutResToFADD   = FMUL16S1.io.outResToFADD
  val FMUL16OutCtrlToFADD  = FMUL16S1.io.outCtrlToFADD

  val FMUL32S1 = Module(new FMULS1(32))
  FMUL32S1.io.inFromS0    := io.inFromS0.FMUL32S0ToS1
  io.outToS2.FMUL32S1ToS2 := FMUL32S1.io.outToS2
  val FMUL32OutResToFADD   = FMUL32S1.io.outResToFADD
  val FMUL32OutCtrlToFADD  = FMUL32S1.io.outCtrlToFADD

  val FMUL64S1 = Module(new FMULS1(64))
  FMUL64S1.io.inFromS0    := io.inFromS0.FMUL64S0ToS1
  io.outToS2.FMUL64S1ToS2 := FMUL64S1.io.outToS2
  val FMUL64OutResToFADD   = FMUL64S1.io.outResToFADD
  val FMUL64OutCtrlToFADD  = FMUL64S1.io.outCtrlToFADD

  io.outToFADD.FMULToFADDCtrl := Mux1H(Seq(
    isF16 -> FMUL16OutCtrlToFADD,
    isF32 -> FMUL32OutCtrlToFADD,
    isF64 -> FMUL64OutCtrlToFADD
  ))
  io.outToFADD.fpA := Mux1H(Seq(
    isF16 -> Cat(Fill(48, 1.U), FMUL16OutResToFADD.head(16)),
    isF32 -> Cat(Fill(32, 1.U), FMUL32OutResToFADD.head(32)),
    isF64 -> FMUL64OutResToFADD.head(64)
  ))
  io.outToFADD.fpAAppend := Mux1H(Seq(
    isF16 -> Cat(Fill(42, 1.U), FMUL16OutResToFADD.tail(16)),
    isF32 -> Cat(Fill(29, 1.U), FMUL32OutResToFADD.tail(32)),
    isF64 -> FMUL64OutResToFADD.tail(64)
  ))
  io.outToS2.isF16 := isF16
  io.outToS2.isF32 := isF32
  io.outToS2.isF64 := isF64
}
