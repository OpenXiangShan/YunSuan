package yunsuan.vector.vfalu

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu.utils._
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule, VFAlgoUtils}

class VectorFALUS1 extends VFModule{
  val io = IO(new Bundle() {
    val fromS0 = Input(new VectorFALUS0ToS1Bundle())
    val out    = Output(new VectorFALUOutput())
  })

  val isArithFp16 = io.fromS0.isArithFp16
  val isArithFp32 = io.fromS0.isArithFp32
  val isArithFp64 = io.fromS0.isArithFp64
  val isFaluMisc  = io.fromS0.isFaluMisc
  val isVfMisc    = io.fromS0.isVfMisc
  val isArith = isArithFp16 || isArithFp32 || isArithFp64

  val arithS1Fp16 = Seq.fill(elemNum("fp16"))(Module(new FALUAddS1(16)))
  arithS1Fp16.zipWithIndex.foreach { case (arith, i) =>
    arith.io.fromS0 := io.fromS0.arithS0ToS1Fp16(i)
  }
  val arithFp16OutRes = Cat(arithS1Fp16.map(_.io.outRes).reverse)
  val arithFp16FFlags = arithS1Fp16.map(_.io.outFlags)

  val arithS1Fp32 = Seq.fill(elemNum("fp32"))(Module(new FALUAddS1(32)))
  arithS1Fp32.zipWithIndex.foreach { case (arith, i) =>
    arith.io.fromS0 := io.fromS0.arithS0ToS1Fp32(i)
  }
  val arithFp32OutRes = Cat(arithS1Fp32.map(_.io.outRes).reverse)
  val arithFp32FFlags = arithS1Fp32.map(_.io.outFlags)

  val arithS1Fp64 = Module(new FALUAddS1(64))
  arithS1Fp64.io.fromS0 := io.fromS0.arithS0ToS1Fp64
  val arithFp64OutRes = arithS1Fp64.io.outRes
  val arithFp64FFlags = arithS1Fp64.io.outFlags

  val arithFFlags = VFAlgoUtils.expandFflags(arithFp16FFlags, arithFp32FFlags, arithFp64FFlags, isArithFp16, isArithFp32, isArithFp64)

  val faluMiscOutRes    = io.fromS0.faluMiscS0ToS1.result
  val faluMiscOutFFlags = io.fromS0.faluMiscS0ToS1.fflags
  val vfMiscOutRes      = io.fromS0.vfMiscS0ToS1.result
  val vfMiscOutFFlags   = io.fromS0.vfMiscS0ToS1.fflags

  io.out.fpResult := Mux1H(Seq(
    isArithFp16 -> arithFp16OutRes,
    isArithFp32 -> arithFp32OutRes,
    isArithFp64 -> arithFp64OutRes,
    isFaluMisc  -> faluMiscOutRes,
    isVfMisc    -> vfMiscOutRes
  ))
  io.out.fflagsVec := Mux1H(Seq(
    isArith -> arithFFlags,
    isFaluMisc -> faluMiscOutFFlags,
    isVfMisc -> vfMiscOutFFlags
  ))
}
