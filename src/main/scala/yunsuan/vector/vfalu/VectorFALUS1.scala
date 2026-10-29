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

  // --- for vfredusum ---
  val fpA = io.fromS0.fpA
  val fpB = io.fromS0.fpB
  val maskFpA = io.fromS0.maskFpA
  val maskFpB = io.fromS0.maskFpB
  val isFold = io.fromS0.isFoldFirst

  val isFoldFp16 = io.fromS0.isArithFp16 && isFold
  val isFoldFp32 = io.fromS0.isArithFp32 && isFold
  val isFoldFp64 = io.fromS0.isArithFp64 && isFold
  val isArithOtherFp16 = io.fromS0.isArithFp16 && !isFold
  val isArithOtherFp32 = io.fromS0.isArithFp32 && !isFold
  val isArithOtherFp64 = io.fromS0.isArithFp64 && !isFold
  // --- for vfredusum ---

  val isArithFp16 = io.fromS0.isArithFp16
  val isArithFp32 = io.fromS0.isArithFp32
  val isArithFp64 = io.fromS0.isArithFp64
  val isMisc      = io.fromS0.isMisc
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

  // --- for vfredusum ---
  val maskFpAs = VecInit(Seq.tabulate(4)(i => maskFpA(i)))
  val maskFpBs = VecInit(Seq.tabulate(4)(i => maskFpB(i)))
  // The next tree layers are unmasked vfadd. Empty pairs must supply an
  // additive identity that preserves signed zero: +0 for RDN, -0 otherwise
  // (including RUP). The adder pipeline already carries the common rm.
  val foldZeroSign = io.fromS0.arithS0ToS1Fp64.rm =/= 2.U
  val fpA16s = VFAlgoUtils.split2Vec(fpA, 4)
  val fpB16s = VFAlgoUtils.split2Vec(fpB, 4)
  val fpA32s = VFAlgoUtils.split2Vec(fpA, 2)
  val fpB32s = VFAlgoUtils.split2Vec(fpB, 2)
  val foldFp16OutRes = Cat(Seq.tabulate(4)(i => Mux1H(Seq(
    ( maskFpAs(i) &&  maskFpBs(i)) -> arithS1Fp16(i).io.outRes,
    (!maskFpAs(i) &&  maskFpBs(i)) -> fpB16s(i),
    ( maskFpAs(i) && !maskFpBs(i)) -> fpA16s(i),
    (!maskFpAs(i) && !maskFpBs(i)) -> Cat(foldZeroSign, 0.U(15.W)),
  ))).reverse)

  val foldFp32OutRes = Cat(Seq.tabulate(2)(i => Mux1H(Seq(
    ( maskFpAs(i) &&  maskFpBs(i)) -> arithS1Fp32(i).io.outRes,
    (!maskFpAs(i) &&  maskFpBs(i)) -> fpB32s(i),
    ( maskFpAs(i) && !maskFpBs(i)) -> fpA32s(i),
    (!maskFpAs(i) && !maskFpBs(i)) -> Cat(foldZeroSign, 0.U(31.W)),
  ))).reverse)
  val foldFp64OutRes = Mux1H(Seq(
    ( maskFpAs(0) &&  maskFpBs(0)) -> arithS1Fp64.io.outRes,
    (!maskFpAs(0) &&  maskFpBs(0)) -> fpB,
    ( maskFpAs(0) && !maskFpBs(0)) -> fpA,
    (!maskFpAs(0) && !maskFpBs(0)) -> Cat(foldZeroSign, 0.U(63.W)),
  ))

  val foldFp16FFlags = arithS1Fp16.zipWithIndex.map { case (mod, i) => Mux(maskFpAs(i) && maskFpBs(i), mod.io.outFlags, 0.U(flagsWidth.W)) }
  val foldFp32FFlags = arithS1Fp32.zipWithIndex.map { case (mod, i) => Mux(maskFpAs(i) && maskFpBs(i), mod.io.outFlags, 0.U(flagsWidth.W)) }
  val foldFp64FFlags = Mux(maskFpAs(0) && maskFpBs(0), arithS1Fp64.io.outFlags, 0.U(flagsWidth.W))

  val foldFFlags = VFAlgoUtils.expandFflags(foldFp16FFlags, foldFp32FFlags, foldFp64FFlags, isArithFp16, isArithFp32, isArithFp64)
  // --- for vfredusum ---

  val miscOutRes    = io.fromS0.miscS0ToS1.result
  val miscOutFFlags = io.fromS0.miscS0ToS1.fflags

  io.out.fpResult := Mux1H(Seq(
    isArithOtherFp16 -> arithFp16OutRes,
    isArithOtherFp32 -> arithFp32OutRes,
    isArithOtherFp64 -> arithFp64OutRes,
    isFoldFp16 -> foldFp16OutRes,
    isFoldFp32 -> foldFp32OutRes,
    isFoldFp64 -> foldFp64OutRes,
    isMisc      -> miscOutRes
  ))
  io.out.fflagsVec := Mux1H(Seq(
    (isArith && !isFold) -> arithFFlags,
    (isArith &&  isFold) -> foldFFlags,
    isMisc  -> miscOutFFlags
  ))
}
