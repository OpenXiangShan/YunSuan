package yunsuan.vector.vfmul

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.utils.{FMULS1, FMULS1ToS2Bundle, FMULToFADDCtrlBundle}
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule, VFAlgoUtils}

class VFloatMULS1ToS2Bundle extends VFBundle {
  val Vfmul_s1_16ToS2 = Vec(elemNum("fp16"), new FMULS1ToS2Bundle(16))
  val Vfmul_s1_32ToS2 = Vec(elemNum("fp32"), new FMULS1ToS2Bundle(32))
  val Vfmul_s1_64ToS2 = new FMULS1ToS2Bundle(64)
  val isF16        = Bool()
  val isF32        = Bool()
  val isF64        = Bool()
}

class VectorFMULS1 extends VFModule {
  val io = IO(new Bundle() {
    val inFromS0  = Input(new VFloatMULS0ToS1Bundle)
    val outToS2   = Output(new VFloatMULS1ToS2Bundle)
    val outToFADD = Output(new VFMul2VFALUOutput)
  })
  val isF16 = io.inFromS0.isF16
  val isF32 = io.inFromS0.isF32
  val isF64 = io.inFromS0.isF64

  val fmulsS1Fp16 = Seq.fill(elemNum("fp16"))(Module(new FMULS1(16)))
  fmulsS1Fp16.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.inFromS0              := io.inFromS0.Vfmul_s0_16ToS1(i)
    io.outToS2.Vfmul_s1_16ToS2(i) := fmul.io.outToS2
  }
  val FMUL16OutResToFADDs   = fmulsS1Fp16.map(_.io.outResToFADD)
  val FMUL16OutCtrlToFADDs  = fmulsS1Fp16.map(_.io.outCtrlToFADD)

  val fmulsS1Fp32 = Seq.fill(elemNum("fp32"))(Module(new FMULS1(32)))
  fmulsS1Fp32.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.inFromS0              := io.inFromS0.Vfmul_s0_32ToS1(i)
    io.outToS2.Vfmul_s1_32ToS2(i) := fmul.io.outToS2
  }
  val FMUL32OutResToFADDs   = fmulsS1Fp32.map(_.io.outResToFADD)
  val FMUL32OutCtrlToFADDs  = fmulsS1Fp32.map(_.io.outCtrlToFADD)

  val fmulS1Fp64 = Module(new FMULS1(64))
  fmulS1Fp64.io.inFromS0    := io.inFromS0.Vfmul_s0_64ToS1
  io.outToS2.Vfmul_s1_64ToS2 := fmulS1Fp64.io.outToS2
  val FMUL64OutResToFADD   = fmulS1Fp64.io.outResToFADD
  val FMUL64OutCtrlToFADD  = fmulS1Fp64.io.outCtrlToFADD

  val outToFADDs: Vec[FMULToFADDCtrlBundle] = Mux1H(Seq(
    isF16 -> VecInit(FMUL16OutCtrlToFADDs),
    isF32 -> VecInit(FMUL32OutCtrlToFADDs ++ Seq.fill(2)(0.U.asTypeOf(new FMULToFADDCtrlBundle(64)))),
    isF64 -> VecInit(FMUL64OutCtrlToFADD +: Seq.fill(3)(0.U.asTypeOf(new FMULToFADDCtrlBundle(64)))),
  ))
  io.outToFADD.FMULToFADDCtrl := VFAlgoUtils.getVfmacCtrlFromFmul(outToFADDs)

  io.outToFADD.fpA := Mux1H(Seq(
    isF16 -> VFAlgoUtils.getFpAFromResults(FMUL16OutResToFADDs)("fp16"),
    isF32 -> VFAlgoUtils.getFpAFromResults(FMUL32OutResToFADDs)("fp32"),
    isF64 -> FMUL64OutResToFADD.head(64)
  ))

  io.outToFADD.fpAAppend := Mux1H(Seq(
    isF16 -> VFAlgoUtils.getFpAAppendFromResults(FMUL16OutResToFADDs)("fp16"), // 42 + ((16 + (16-1- 5) + 1) - 16) => 42 + 11 => 53;   11 * 4 = 44 <= 53
    isF32 -> VFAlgoUtils.getFpAAppendFromResults(FMUL32OutResToFADDs)("fp32"), // 29 + ((32 + (32-1- 8) + 1) - 32) => 29 + 24 => 53;   24 * 2 = 48 <= 53
    isF64 -> FMUL64OutResToFADD.tail(64)                                      //  0 + ((64 + (64-1-11) + 1) - 64) =>  0 + 53 => 53;   53 * 1 = 53 <= 53
  ))

  io.outToS2.isF16 := isF16
  io.outToS2.isF32 := isF32
  io.outToS2.isF64 := isF64
}
