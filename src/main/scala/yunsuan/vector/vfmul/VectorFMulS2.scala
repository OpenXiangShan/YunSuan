package yunsuan.vector.vfmul

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.utils.FMULS2
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule, VFAlgoUtils}

class VectorFMULS2 extends VFModule {
  val io = IO(new Bundle() {
    val inFromS1 = Input(new VFloatMULS1ToS2Bundle)
    val out = Output(new VectorFMulOutput)
  })
  val isF16 = io.inFromS1.isF16
  val isF32 = io.inFromS1.isF32
  val isF64 = io.inFromS1.isF64

  val fmulsS2Fp16 = Seq.fill(elemNum("fp16"))(Module(new FMULS2(16)))
  fmulsS2Fp16.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.inFromS1 := io.inFromS1.Vfmul_s1_16ToS2(i)
  }
  val FMUL16OutRes = Cat(fmulsS2Fp16.map(_.io.outRes).reverse)
  val FMUL16FFlags = fmulsS2Fp16.map(_.io.outFlags)

  val fmulsS2Fp32 = Seq.fill(elemNum("fp32"))(Module(new FMULS2(32)))
  fmulsS2Fp32.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.inFromS1 := io.inFromS1.Vfmul_s1_32ToS2(i)
  }
  val FMUL32OutRes = Cat(fmulsS2Fp32.map(_.io.outRes).reverse)
  val FMUL32FFlags = fmulsS2Fp32.map(_.io.outFlags)

  val fmulS2Fp64 = Module(new FMULS2(64))
  fmulS2Fp64.io.inFromS1 := io.inFromS1.Vfmul_s1_64ToS2
  val FMUL64OutRes = fmulS2Fp64.io.outRes
  val FMUL64FFlags = fmulS2Fp64.io.outFlags

  io.out.fpResult := Mux1H(Seq(
    isF16 -> FMUL16OutRes,
    isF32 -> FMUL32OutRes,
    isF64 -> FMUL64OutRes
  ))

  io.out.fflagsVec := VFAlgoUtils.expandFflags(
    FMUL16FFlags,
    FMUL32FFlags,
    FMUL64FFlags,
    isF16,
    isF32,
    isF64,
  )
}
