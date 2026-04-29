package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._
import yunsuan.vector.LZD

class VectorFMAS2 extends VectorFMA.Module {
  val in = IO(Input(new VectorFMAS1.S1ToS2))
  val out = IO(Output(new VectorFMAS2.S2ToS3))

  private def shiftLeftWithMux(srcValue: UInt, shiftValue: UInt): UInt = {
    val vecLength = shiftValue.getWidth + 1
    val resVec = Wire(Vec(vecLength, UInt(srcValue.getWidth.W)))
    resVec(0) := srcValue
    for (i <- 0 until shiftValue.getWidth) {
      resVec(i + 1) := Mux(
        shiftValue(shiftValue.getWidth - 1 - i),
        resVec(i) << (1 << (shiftValue.getWidth - 1 - i)),
        resVec(i)
      )
    }
    resVec(vecLength - 1)
  }

  private val inCtrl = in.ctrl
  private val inData = in.data
  private val roundMode = inCtrl.round_mode
  private val rne = roundMode === "b000".U
  private val rtz = roundMode === "b001".U
  private val rdn = roundMode === "b010".U
  private val rup = roundMode === "b011".U
  private val rmm = roundMode === "b100".U
  private val resWidening = inCtrl.res_widening
  private val fpAIsFpCanonicalNAN = inCtrl.fp_aIsFpCanonicalNAN
  private val fpBIsFpCanonicalNAN = inCtrl.fp_bIsFpCanonicalNAN
  private val fpCIsFpCanonicalNAN = inCtrl.fp_cIsFpCanonicalNAN

  val isVfmul = inCtrl.isVfmul

  val isFp32 = inCtrl.resultIsFp32
  val isFp64 = inCtrl.resultIsFp64
  val isFp16 = !isFp32 && !isFp64

  val fpCF64 = inData.fpCF64
  val fpCF32_0 = inData.fpCF32_0
  val fpCF32_1 = inData.fpCF32_1
  val fpCF16_0 = inData.fpCF16_0
  val fpCF16_1 = inData.fpCF16_1
  val fpCF16_2 = inData.fpCF16_2
  val fpCF16_3 = inData.fpCF16_3

  val signABF64 = inCtrl.signABF64
  val signABF32_0 = inCtrl.signABF32_0
  val signABF32_1 = inCtrl.signABF32_1
  val signABF16_0 = inCtrl.signABF16_0
  val signABF16_1 = inCtrl.signABF16_1
  val signABF16_2 = inCtrl.signABF16_2
  val signABF16_3 = inCtrl.signABF16_3

  val signCF64 = inCtrl.signCF64
  val signCF32_0 = inCtrl.signCF32_0
  val signCF32_1 = inCtrl.signCF32_1
  val signCF16_0 = inCtrl.signCF16_0
  val signCF16_1 = inCtrl.signCF16_1
  val signCF16_2 = inCtrl.signCF16_2
  val signCF16_3 = inCtrl.signCF16_3

  val eaF64 = inData.eaF64
  val eaF32_0 = inData.eaF32_0
  val eaF32_1 = inData.eaF32_1
  val eaF16_0 = inData.eaF16_0
  val eaF16_1 = inData.eaF16_1
  val eaF16_2 = inData.eaF16_2
  val eaF16_3 = inData.eaF16_3

  val ebF64 = inData.ebF64
  val ebF32_0 = inData.ebF32_0
  val ebF32_1 = inData.ebF32_1
  val ebF16_0 = inData.ebF16_0
  val ebF16_1 = inData.ebF16_1
  val ebF16_2 = inData.ebF16_2
  val ebF16_3 = inData.ebF16_3

  val ecF64 = inData.ecF64
  val ecF32_0 = inData.ecF32_0
  val ecF32_1 = inData.ecF32_1
  val ecF16_0 = inData.ecF16_0
  val ecF16_1 = inData.ecF16_1
  val ecF16_2 = inData.ecF16_2
  val ecF16_3 = inData.ecF16_3

  val widenEaF16_0 = inData.widenEaF16_0
  val widenEbF16_0 = inData.widenEbF16_0
  val widenEaF16_1 = inData.widenEaF16_1
  val widenEbF16_1 = inData.widenEbF16_1
  val widenEaF32_0 = inData.widenEaF32_0
  val widenEbF32_0 = inData.widenEbF32_0

  val fpASignificandF64 = inData.fpASignificandF64
  val fpASignificandF32_0 = inData.fpASignificandF32_0
  val fpASignificandF32_1 = inData.fpASignificandF32_1
  val fpASignificandF16_0 = inData.fpASignificandF16_0
  val fpASignificandF16_1 = inData.fpASignificandF16_1
  val fpASignificandF16_2 = inData.fpASignificandF16_2
  val fpASignificandF16_3 = inData.fpASignificandF16_3

  val fpBSignificandF64 = inData.fpBSignificandF64
  val fpBSignificandF32_0 = inData.fpBSignificandF32_0
  val fpBSignificandF32_1 = inData.fpBSignificandF32_1
  val fpBSignificandF16_0 = inData.fpBSignificandF16_0
  val fpBSignificandF16_1 = inData.fpBSignificandF16_1
  val fpBSignificandF16_2 = inData.fpBSignificandF16_2
  val fpBSignificandF16_3 = inData.fpBSignificandF16_3

  val fpCSignificandF64 = inData.fpCSignificandF64
  val fpCSignificandF32_0 = inData.fpCSignificandF32_0
  val fpCSignificandF32_1 = inData.fpCSignificandF32_1
  val fpCSignificandF16_0 = inData.fpCSignificandF16_0
  val fpCSignificandF16_1 = inData.fpCSignificandF16_1
  val fpCSignificandF16_2 = inData.fpCSignificandF16_2
  val fpCSignificandF16_3 = inData.fpCSignificandF16_3

  val adderF64 = inData.adder(163, 0)
  val adderF32_0 = inData.adder(76, 0)
  val adderF32_1 = inData.adder(153, 77)
  val adderF16_0 = inData.adder(37, 0)
  val adderF16_1 = inData.adder(75, 38)
  val adderF16_2 = inData.adder(113, 76)
  val adderF16_3 = inData.adder(151, 114)

  val adderIsNegativeF64 = inCtrl.adderIsNegative(0)
  val adderIsNegativeF32_0 = inCtrl.adderIsNegative(0)
  val adderIsNegativeF32_1 = inCtrl.adderIsNegative(1)
  val adderIsNegativeF16_0 = inCtrl.adderIsNegative(0)
  val adderIsNegativeF16_1 = inCtrl.adderIsNegative(1)
  val adderIsNegativeF16_2 = inCtrl.adderIsNegative(2)
  val adderIsNegativeF16_3 = inCtrl.adderIsNegative(3)

  val eGreaterF64 = inData.eGreater(11, 0)
  val eGreaterF32_0 = inData.eGreater(8, 0)
  val eGreaterF32_1 = inData.eGreater(17, 9)
  val eGreaterF16_0 = inData.eGreater(5, 0)
  val eGreaterF16_1 = inData.eGreater(11, 6)
  val eGreaterF16_2 = inData.eGreater(17, 12)
  val eGreaterF16_3 = inData.eGreater(23, 18)

  val tzdAdderInputF64 = inData.tzdAdderInput(163, 0)
  val tzdAdderInputF32_0 = inData.tzdAdderInput(76, 0)
  val tzdAdderInputF32_1 = inData.tzdAdderInput(153, 77)
  val tzdAdderInputF16_0 = inData.tzdAdderInput(37, 0)
  val tzdAdderInputF16_1 = inData.tzdAdderInput(75, 38)
  val tzdAdderInputF16_2 = inData.tzdAdderInput(113, 76)
  val tzdAdderInputF16_3 = inData.tzdAdderInput(151, 114)

  val lzdAdderInvMaskInputF64 = inData.lzdAdderInvMaskInput(162, 0)
  val lzdAdderInvMaskInputF32_0 = inData.lzdAdderInvMaskInput(75, 0)
  val lzdAdderInvMaskInputF32_1 = inData.lzdAdderInvMaskInput(151, 76)
  val lzdAdderInvMaskInputF16_0 = inData.lzdAdderInvMaskInput(36, 0)
  val lzdAdderInvMaskInputF16_1 = inData.lzdAdderInvMaskInput(73, 37)
  val lzdAdderInvMaskInputF16_2 = inData.lzdAdderInvMaskInput(110, 74)
  val lzdAdderInvMaskInputF16_3 = inData.lzdAdderInvMaskInput(147, 111)
  val mulRawF64 = inData.mulRaw
  val mulRawF32_0 = inData.mulRaw(48, 0)
  val mulRawF32_1 = inData.mulRaw(106, 58)
  val mulRawF16_0 = inData.mulRaw(22, 0)
  val mulRawF16_1 = inData.mulRaw(48, 26)
  val mulRawF16_2 = inData.mulRaw(80, 58)
  val mulRawF16_3 = inData.mulRaw(106, 84)
  val mulExpBasePlus2F64 = inData.mulExpBasePlus2(11, 0)
  val mulExpBasePlus2F32_0 = inData.mulExpBasePlus2(8, 0)
  val mulExpBasePlus2F32_1 = inData.mulExpBasePlus2(17, 9)
  val mulExpBasePlus2F16_0 = inData.mulExpBasePlus2(5, 0)
  val mulExpBasePlus2F16_1 = inData.mulExpBasePlus2(11, 6)
  val mulExpBasePlus2F16_2 = inData.mulExpBasePlus2(17, 12)
  val mulExpBasePlus2F16_3 = inData.mulExpBasePlus2(23, 18)

  val lshiftMaskValidF64 = inData.lshiftMaskValid(0)
  val lshiftMaskValidF32_0 = inData.lshiftMaskValid(0)
  val lshiftMaskValidF32_1 = inData.lshiftMaskValid(1)
  val lshiftMaskValidF16_0 = inData.lshiftMaskValid(0)
  val lshiftMaskValidF16_1 = inData.lshiftMaskValid(1)
  val lshiftMaskValidF16_2 = inData.lshiftMaskValid(2)
  val lshiftMaskValidF16_3 = inData.lshiftMaskValid(3)

  val tzdAdderF64 = LZD(tzdAdderInputF64.asTypeOf(adderF64))
  val tzdAdderF32_0 = LZD(tzdAdderInputF32_0.asTypeOf(adderF32_0))
  val tzdAdderF32_1 = LZD(tzdAdderInputF32_1.asTypeOf(adderF32_1))
  val tzdAdderF16_0 = LZD(tzdAdderInputF16_0.asTypeOf(adderF16_0))
  val tzdAdderF16_1 = LZD(tzdAdderInputF16_1.asTypeOf(adderF16_1))
  val tzdAdderF16_2 = LZD(tzdAdderInputF16_2.asTypeOf(adderF16_2))
  val tzdAdderF16_3 = LZD(tzdAdderInputF16_3.asTypeOf(adderF16_3))

  val lzdAdderInvMaskF64 = LZD(lzdAdderInvMaskInputF64.asTypeOf(adderF64.tail(1)))
  val lzdAdderInvMaskF32_0 = LZD(lzdAdderInvMaskInputF32_0.asTypeOf(adderF32_0.tail(1)))
  val lzdAdderInvMaskF32_1 = LZD(lzdAdderInvMaskInputF32_1.asTypeOf(adderF32_1.tail(1)))
  val lzdAdderInvMaskF16_0 = LZD(lzdAdderInvMaskInputF16_0.asTypeOf(adderF16_0.tail(1)))
  val lzdAdderInvMaskF16_1 = LZD(lzdAdderInvMaskInputF16_1.asTypeOf(adderF16_1.tail(1)))
  val lzdAdderInvMaskF16_2 = LZD(lzdAdderInvMaskInputF16_2.asTypeOf(adderF16_2.tail(1)))
  val lzdAdderInvMaskF16_3 = LZD(lzdAdderInvMaskInputF16_3.asTypeOf(adderF16_3.tail(1)))

  val lshiftValueF64 = lzdAdderInvMaskF64
  val lshiftValueF32_0 = lzdAdderInvMaskF32_0
  val lshiftValueF32_1 = lzdAdderInvMaskF32_1
  val lshiftValueF16_0 = lzdAdderInvMaskF16_0
  val lshiftValueF16_1 = lzdAdderInvMaskF16_1
  val lshiftValueF16_2 = lzdAdderInvMaskF16_2
  val lshiftValueF16_3 = lzdAdderInvMaskF16_3

  val lshiftAdderF64 = shiftLeftWithMux(adderF64, lshiftValueF64)
  val lshiftAdderF32_0 = shiftLeftWithMux(adderF32_0, lshiftValueF32_0)
  val lshiftAdderF32_1 = shiftLeftWithMux(adderF32_1, lshiftValueF32_1)
  val lshiftAdderF16_0 = shiftLeftWithMux(adderF16_0, lshiftValueF16_0)
  val lshiftAdderF16_1 = shiftLeftWithMux(adderF16_1, lshiftValueF16_1)
  val lshiftAdderF16_2 = shiftLeftWithMux(adderF16_2, lshiftValueF16_2)
  val lshiftAdderF16_3 = shiftLeftWithMux(adderF16_3, lshiftValueF16_3)

  val lshiftAdderInvF64 = Cat(Mux(adderIsNegativeF64, ~lshiftAdderF64.head(significandWidth + 4), lshiftAdderF64.head(significandWidth + 4)), lshiftAdderF64.tail(significandWidth + 4))
  val lshiftAdderInvF32_0 = Cat(Mux(adderIsNegativeF32_0, ~lshiftAdderF32_0.head(24 + 4), lshiftAdderF32_0.head(24 + 4)), lshiftAdderF32_0.tail(24 + 4))
  val lshiftAdderInvF32_1 = Cat(Mux(adderIsNegativeF32_1, ~lshiftAdderF32_1.head(24 + 4), lshiftAdderF32_1.head(24 + 4)), lshiftAdderF32_1.tail(24 + 4))
  val lshiftAdderInvF16_0 = Cat(Mux(adderIsNegativeF16_0, ~lshiftAdderF16_0.head(11 + 4), lshiftAdderF16_0.head(11 + 4)), lshiftAdderF16_0.tail(11 + 4))
  val lshiftAdderInvF16_1 = Cat(Mux(adderIsNegativeF16_1, ~lshiftAdderF16_1.head(11 + 4), lshiftAdderF16_1.head(11 + 4)), lshiftAdderF16_1.tail(11 + 4))
  val lshiftAdderInvF16_2 = Cat(Mux(adderIsNegativeF16_2, ~lshiftAdderF16_2.head(11 + 4), lshiftAdderF16_2.head(11 + 4)), lshiftAdderF16_2.tail(11 + 4))
  val lshiftAdderInvF16_3 = Cat(Mux(adderIsNegativeF16_3, ~lshiftAdderF16_3.head(11 + 4), lshiftAdderF16_3.head(11 + 4)), lshiftAdderF16_3.tail(11 + 4))

  val isFixF64 = (tzdAdderF64 + lzdAdderInvMaskF64) === lzdAdderInvMaskInputF64.getWidth.U
  val isFixF32_0 = (tzdAdderF32_0 + lzdAdderInvMaskF32_0) === lzdAdderInvMaskInputF32_0.getWidth.U
  val isFixF32_1 = (tzdAdderF32_1 + lzdAdderInvMaskF32_1) === lzdAdderInvMaskInputF32_1.getWidth.U
  val isFixF16_0 = (tzdAdderF16_0 + lzdAdderInvMaskF16_0) === lzdAdderInvMaskInputF16_0.getWidth.U
  val isFixF16_1 = (tzdAdderF16_1 + lzdAdderInvMaskF16_1) === lzdAdderInvMaskInputF16_1.getWidth.U
  val isFixF16_2 = (tzdAdderF16_2 + lzdAdderInvMaskF16_2) === lzdAdderInvMaskInputF16_2.getWidth.U
  val isFixF16_3 = (tzdAdderF16_3 + lzdAdderInvMaskF16_3) === lzdAdderInvMaskInputF16_3.getWidth.U
  val lshiftAdderInvFixF64 = Mux(isFixF64, lshiftAdderInvF64.head(lzdAdderInvMaskInputF64.getWidth), lshiftAdderInvF64.tail(1))
  val lshiftAdderInvFixF32_0 = Mux(isFixF32_0, lshiftAdderInvF32_0.head(lzdAdderInvMaskInputF32_0.getWidth), lshiftAdderInvF32_0.tail(1))
  val lshiftAdderInvFixF32_1 = Mux(isFixF32_1, lshiftAdderInvF32_1.head(lzdAdderInvMaskInputF32_1.getWidth), lshiftAdderInvF32_1.tail(1))
  val lshiftAdderInvFixF16_0 = Mux(isFixF16_0, lshiftAdderInvF16_0.head(lzdAdderInvMaskInputF16_0.getWidth), lshiftAdderInvF16_0.tail(1))
  val lshiftAdderInvFixF16_1 = Mux(isFixF16_1, lshiftAdderInvF16_1.head(lzdAdderInvMaskInputF16_1.getWidth), lshiftAdderInvF16_1.tail(1))
  val lshiftAdderInvFixF16_2 = Mux(isFixF16_2, lshiftAdderInvF16_2.head(lzdAdderInvMaskInputF16_2.getWidth), lshiftAdderInvF16_2.tail(1))
  val lshiftAdderInvFixF16_3 = Mux(isFixF16_3, lshiftAdderInvF16_3.head(lzdAdderInvMaskInputF16_3.getWidth), lshiftAdderInvF16_3.tail(1))

  val fractionResultNoRoundF64 = lshiftAdderInvFixF64.tail(1).head(significandWidth - 1)
  val fractionResultNoRoundF32_0 = lshiftAdderInvFixF32_0.tail(1).head(24 - 1)
  val fractionResultNoRoundF32_1 = lshiftAdderInvFixF32_1.tail(1).head(24 - 1)
  val fractionResultNoRoundF16_0 = lshiftAdderInvFixF16_0.tail(1).head(11 - 1)
  val fractionResultNoRoundF16_1 = lshiftAdderInvFixF16_1.tail(1).head(11 - 1)
  val fractionResultNoRoundF16_2 = lshiftAdderInvFixF16_2.tail(1).head(11 - 1)
  val fractionResultNoRoundF16_3 = lshiftAdderInvFixF16_3.tail(1).head(11 - 1)
  val fractionResultRoundF64 = fractionResultNoRoundF64 +& 1.U
  val fractionResultRoundF32_0 = fractionResultNoRoundF32_0 +& 1.U
  val fractionResultRoundF32_1 = fractionResultNoRoundF32_1 +& 1.U
  val fractionResultRoundF16_0 = fractionResultNoRoundF16_0 +& 1.U
  val fractionResultRoundF16_1 = fractionResultNoRoundF16_1 +& 1.U
  val fractionResultRoundF16_2 = fractionResultNoRoundF16_2 +& 1.U
  val fractionResultRoundF16_3 = fractionResultNoRoundF16_3 +& 1.U

  val signResultTempF64 = Mux(adderIsNegativeF64, signCF64, signABF64)
  val signResultTempF32_0 = Mux(adderIsNegativeF32_0, signCF32_0, signABF32_0)
  val signResultTempF32_1 = Mux(adderIsNegativeF32_1, signCF32_1, signABF32_1)
  val signResultTempF16_0 = Mux(adderIsNegativeF16_0, signCF16_0, signABF16_0)
  val signResultTempF16_1 = Mux(adderIsNegativeF16_1, signCF16_1, signABF16_1)
  val signResultTempF16_2 = Mux(adderIsNegativeF16_2, signCF16_2, signABF16_2)
  val signResultTempF16_3 = Mux(adderIsNegativeF16_3, signCF16_3, signABF16_3)

  val rshiftStickyF64 = inCtrl.rshiftSticky(0)
  val rshiftStickyF32_0 = inCtrl.rshiftSticky(0)
  val rshiftStickyF32_1 = inCtrl.rshiftSticky(1)
  val rshiftStickyF16_0 = inCtrl.rshiftSticky(0)
  val rshiftStickyF16_1 = inCtrl.rshiftSticky(1)
  val rshiftStickyF16_2 = inCtrl.rshiftSticky(2)
  val rshiftStickyF16_3 = inCtrl.rshiftSticky(3)
  val stickyF64 = rshiftStickyF64 | (lzdAdderInvMaskF64 + tzdAdderF64 < (lzdAdderInvMaskInputF64.getWidth - significandWidth - 2).U)
  val stickyF32_0 = rshiftStickyF32_0 | (lzdAdderInvMaskF32_0 + tzdAdderF32_0 < (lzdAdderInvMaskInputF32_0.getWidth - 24 - 2).U)
  val stickyF32_1 = rshiftStickyF32_1 | (lzdAdderInvMaskF32_1 + tzdAdderF32_1 < (lzdAdderInvMaskInputF32_1.getWidth - 24 - 2).U)
  val stickyF16_0 = rshiftStickyF16_0 | (lzdAdderInvMaskF16_0 + tzdAdderF16_0 < (lzdAdderInvMaskInputF16_0.getWidth - 11 - 2).U)
  val stickyF16_1 = rshiftStickyF16_1 | (lzdAdderInvMaskF16_1 + tzdAdderF16_1 < (lzdAdderInvMaskInputF16_1.getWidth - 11 - 2).U)
  val stickyF16_2 = rshiftStickyF16_2 | (lzdAdderInvMaskF16_2 + tzdAdderF16_2 < (lzdAdderInvMaskInputF16_2.getWidth - 11 - 2).U)
  val stickyF16_3 = rshiftStickyF16_3 | (lzdAdderInvMaskF16_3 + tzdAdderF16_3 < (lzdAdderInvMaskInputF16_3.getWidth - 11 - 2).U)
  val stickyUfF64 = rshiftStickyF64 | (lzdAdderInvMaskF64 + tzdAdderF64 < (lzdAdderInvMaskInputF64.getWidth - significandWidth - 3).U)
  val stickyUfF32_0 = rshiftStickyF32_0 | (lzdAdderInvMaskF32_0 + tzdAdderF32_0 < (lzdAdderInvMaskInputF32_0.getWidth - 24 - 3).U)
  val stickyUfF32_1 = rshiftStickyF32_1 | (lzdAdderInvMaskF32_1 + tzdAdderF32_1 < (lzdAdderInvMaskInputF32_1.getWidth - 24 - 3).U)
  val stickyUfF16_0 = rshiftStickyF16_0 | (lzdAdderInvMaskF16_0 + tzdAdderF16_0 < (lzdAdderInvMaskInputF16_0.getWidth - 11 - 3).U)
  val stickyUfF16_1 = rshiftStickyF16_1 | (lzdAdderInvMaskF16_1 + tzdAdderF16_1 < (lzdAdderInvMaskInputF16_1.getWidth - 11 - 3).U)
  val stickyUfF16_2 = rshiftStickyF16_2 | (lzdAdderInvMaskF16_2 + tzdAdderF16_2 < (lzdAdderInvMaskInputF16_2.getWidth - 11 - 3).U)
  val stickyUfF16_3 = rshiftStickyF16_3 | (lzdAdderInvMaskF16_3 + tzdAdderF16_3 < (lzdAdderInvMaskInputF16_3.getWidth - 11 - 3).U)

  val roundLshiftF64 = lshiftAdderInvFixF64.tail(significandWidth + 1).head(1)
  val roundLshiftF32_0 = lshiftAdderInvFixF32_0.tail(24 + 1).head(1)
  val roundLshiftF32_1 = lshiftAdderInvFixF32_1.tail(24 + 1).head(1)
  val roundLshiftF16_0 = lshiftAdderInvFixF16_0.tail(11 + 1).head(1)
  val roundLshiftF16_1 = lshiftAdderInvFixF16_1.tail(11 + 1).head(1)
  val roundLshiftF16_2 = lshiftAdderInvFixF16_2.tail(11 + 1).head(1)
  val roundLshiftF16_3 = lshiftAdderInvFixF16_3.tail(11 + 1).head(1)
  val guardLshiftF64 = lshiftAdderInvFixF64.tail(significandWidth).head(1)
  val guardLshiftF32_0 = lshiftAdderInvFixF32_0.tail(24).head(1)
  val guardLshiftF32_1 = lshiftAdderInvFixF32_1.tail(24).head(1)
  val guardLshiftF16_0 = lshiftAdderInvFixF16_0.tail(11).head(1)
  val guardLshiftF16_1 = lshiftAdderInvFixF16_1.tail(11).head(1)
  val guardLshiftF16_2 = lshiftAdderInvFixF16_2.tail(11).head(1)
  val guardLshiftF16_3 = lshiftAdderInvFixF16_3.tail(11).head(1)
  val roundF64 = Mux(adderIsNegativeF64, roundLshiftF64 ^ !stickyF64, roundLshiftF64)
  val roundF32_0 = Mux(adderIsNegativeF32_0, roundLshiftF32_0 ^ !stickyF32_0, roundLshiftF32_0)
  val roundF32_1 = Mux(adderIsNegativeF32_1, roundLshiftF32_1 ^ !stickyF32_1, roundLshiftF32_1)
  val roundF16_0 = Mux(adderIsNegativeF16_0, roundLshiftF16_0 ^ !stickyF16_0, roundLshiftF16_0)
  val roundF16_1 = Mux(adderIsNegativeF16_1, roundLshiftF16_1 ^ !stickyF16_1, roundLshiftF16_1)
  val roundF16_2 = Mux(adderIsNegativeF16_2, roundLshiftF16_2 ^ !stickyF16_2, roundLshiftF16_2)
  val roundF16_3 = Mux(adderIsNegativeF16_3, roundLshiftF16_3 ^ !stickyF16_3, roundLshiftF16_3)
  val guardF64 = Mux(adderIsNegativeF64, guardLshiftF64 ^ (!stickyF64 & roundLshiftF64), guardLshiftF64)
  val guardF32_0 = Mux(adderIsNegativeF32_0, guardLshiftF32_0 ^ (!stickyF32_0 & roundLshiftF32_0), guardLshiftF32_0)
  val guardF32_1 = Mux(adderIsNegativeF32_1, guardLshiftF32_1 ^ (!stickyF32_1 & roundLshiftF32_1), guardLshiftF32_1)
  val guardF16_0 = Mux(adderIsNegativeF16_0, guardLshiftF16_0 ^ (!stickyF16_0 & roundLshiftF16_0), guardLshiftF16_0)
  val guardF16_1 = Mux(adderIsNegativeF16_1, guardLshiftF16_1 ^ (!stickyF16_1 & roundLshiftF16_1), guardLshiftF16_1)
  val guardF16_2 = Mux(adderIsNegativeF16_2, guardLshiftF16_2 ^ (!stickyF16_2 & roundLshiftF16_2), guardLshiftF16_2)
  val guardF16_3 = Mux(adderIsNegativeF16_3, guardLshiftF16_3 ^ (!stickyF16_3 & roundLshiftF16_3), guardLshiftF16_3)
  val guardUfF64 = roundF64
  val guardUfF32_0 = roundF32_0
  val guardUfF32_1 = roundF32_1
  val guardUfF16_0 = roundF16_0
  val guardUfF16_1 = roundF16_1
  val guardUfF16_2 = roundF16_2
  val guardUfF16_3 = roundF16_3
  val roundLshiftUfF64 = lshiftAdderInvFixF64.tail(significandWidth + 2).head(1)
  val roundLshiftUfF32_0 = lshiftAdderInvFixF32_0.tail(24 + 2).head(1)
  val roundLshiftUfF32_1 = lshiftAdderInvFixF32_1.tail(24 + 2).head(1)
  val roundLshiftUfF16_0 = lshiftAdderInvFixF16_0.tail(11 + 2).head(1)
  val roundLshiftUfF16_1 = lshiftAdderInvFixF16_1.tail(11 + 2).head(1)
  val roundLshiftUfF16_2 = lshiftAdderInvFixF16_2.tail(11 + 2).head(1)
  val roundLshiftUfF16_3 = lshiftAdderInvFixF16_3.tail(11 + 2).head(1)
  val roundUfF64 = Mux(adderIsNegativeF64, roundLshiftUfF64 ^ !stickyUfF64, roundLshiftUfF64)
  val roundUfF32_0 = Mux(adderIsNegativeF32_0, roundLshiftUfF32_0 ^ !stickyUfF32_0, roundLshiftUfF32_0)
  val roundUfF32_1 = Mux(adderIsNegativeF32_1, roundLshiftUfF32_1 ^ !stickyUfF32_1, roundLshiftUfF32_1)
  val roundUfF16_0 = Mux(adderIsNegativeF16_0, roundLshiftUfF16_0 ^ !stickyUfF16_0, roundLshiftUfF16_0)
  val roundUfF16_1 = Mux(adderIsNegativeF16_1, roundLshiftUfF16_1 ^ !stickyUfF16_1, roundLshiftUfF16_1)
  val roundUfF16_2 = Mux(adderIsNegativeF16_2, roundLshiftUfF16_2 ^ !stickyUfF16_2, roundLshiftUfF16_2)
  val roundUfF16_3 = Mux(adderIsNegativeF16_3, roundLshiftUfF16_3 ^ !stickyUfF16_3, roundLshiftUfF16_3)

  val roundAdd1F64 = rne & (guardF64 & (fractionResultNoRoundF64(0) | roundF64 | stickyF64)) |
    rdn & signResultTempF64 & (guardF64 | roundF64 | stickyF64) |
    rup & !signResultTempF64 & (guardF64 | roundF64 | stickyF64) |
    rmm & guardF64 |
    adderIsNegativeF64 & !guardF64 & !roundF64 & !stickyF64
  val roundAdd1F32_0 = rne & (guardF32_0 & (fractionResultNoRoundF32_0(0) | roundF32_0 | stickyF32_0)) |
    rdn & signResultTempF32_0 & (guardF32_0 | roundF32_0 | stickyF32_0) |
    rup & !signResultTempF32_0 & (guardF32_0 | roundF32_0 | stickyF32_0) |
    rmm & guardF32_0 |
    adderIsNegativeF32_0 & !guardF32_0 & !roundF32_0 & !stickyF32_0
  val roundAdd1F32_1 = rne & (guardF32_1 & (fractionResultNoRoundF32_1(0) | roundF32_1 | stickyF32_1)) |
    rdn & signResultTempF32_1 & (guardF32_1 | roundF32_1 | stickyF32_1) |
    rup & !signResultTempF32_1 & (guardF32_1 | roundF32_1 | stickyF32_1) |
    rmm & guardF32_1 |
    adderIsNegativeF32_1 & !guardF32_1 & !roundF32_1 & !stickyF32_1
  val roundAdd1F16_0 = rne & (guardF16_0 & (fractionResultNoRoundF16_0(0) | roundF16_0 | stickyF16_0)) |
    rdn & signResultTempF16_0 & (guardF16_0 | roundF16_0 | stickyF16_0) |
    rup & !signResultTempF16_0 & (guardF16_0 | roundF16_0 | stickyF16_0) |
    rmm & guardF16_0 |
    adderIsNegativeF16_0 & !guardF16_0 & !roundF16_0 & !stickyF16_0
  val roundAdd1F16_1 = rne & (guardF16_1 & (fractionResultNoRoundF16_1(0) | roundF16_1 | stickyF16_1)) |
    rdn & signResultTempF16_1 & (guardF16_1 | roundF16_1 | stickyF16_1) |
    rup & !signResultTempF16_1 & (guardF16_1 | roundF16_1 | stickyF16_1) |
    rmm & guardF16_1 |
    adderIsNegativeF16_1 & !guardF16_1 & !roundF16_1 & !stickyF16_1
  val roundAdd1F16_2 = rne & (guardF16_2 & (fractionResultNoRoundF16_2(0) | roundF16_2 | stickyF16_2)) |
    rdn & signResultTempF16_2 & (guardF16_2 | roundF16_2 | stickyF16_2) |
    rup & !signResultTempF16_2 & (guardF16_2 | roundF16_2 | stickyF16_2) |
    rmm & guardF16_2 |
    adderIsNegativeF16_2 & !guardF16_2 & !roundF16_2 & !stickyF16_2
  val roundAdd1F16_3 = rne & (guardF16_3 & (fractionResultNoRoundF16_3(0) | roundF16_3 | stickyF16_3)) |
    rdn & signResultTempF16_3 & (guardF16_3 | roundF16_3 | stickyF16_3) |
    rup & !signResultTempF16_3 & (guardF16_3 | roundF16_3 | stickyF16_3) |
    rmm & guardF16_3 |
    adderIsNegativeF16_3 & !guardF16_3 & !roundF16_3 & !stickyF16_3
  val roundAdd1UfF64 = rne & (guardUfF64 & (guardF64 | roundUfF64 | stickyUfF64)) |
    rdn & signResultTempF64 & (guardUfF64 | roundUfF64 | stickyUfF64) |
    rup & !signResultTempF64 & (guardUfF64 | roundUfF64 | stickyUfF64) |
    rmm & guardUfF64
  val roundAdd1UfF32_0 = rne & (guardUfF32_0 & (guardF32_0 | roundUfF32_0 | stickyUfF32_0)) |
    rdn & signResultTempF32_0 & (guardUfF32_0 | roundUfF32_0 | stickyUfF32_0) |
    rup & !signResultTempF32_0 & (guardUfF32_0 | roundUfF32_0 | stickyUfF32_0) |
    rmm & guardUfF32_0
  val roundAdd1UfF32_1 = rne & (guardUfF32_1 & (guardF32_1 | roundUfF32_1 | stickyUfF32_1)) |
    rdn & signResultTempF32_1 & (guardUfF32_1 | roundUfF32_1 | stickyUfF32_1) |
    rup & !signResultTempF32_1 & (guardUfF32_1 | roundUfF32_1 | stickyUfF32_1) |
    rmm & guardUfF32_1
  val roundAdd1UfF16_0 = rne & (guardUfF16_0 & (guardF16_0 | roundUfF16_0 | stickyUfF16_0)) |
    rdn & signResultTempF16_0 & (guardUfF16_0 | roundUfF16_0 | stickyUfF16_0) |
    rup & !signResultTempF16_0 & (guardUfF16_0 | roundUfF16_0 | stickyUfF16_0) |
    rmm & guardUfF16_0
  val roundAdd1UfF16_1 = rne & (guardUfF16_1 & (guardF16_1 | roundUfF16_1 | stickyUfF16_1)) |
    rdn & signResultTempF16_1 & (guardUfF16_1 | roundUfF16_1 | stickyUfF16_1) |
    rup & !signResultTempF16_1 & (guardUfF16_1 | roundUfF16_1 | stickyUfF16_1) |
    rmm & guardUfF16_1
  val roundAdd1UfF16_2 = rne & (guardUfF16_2 & (guardF16_2 | roundUfF16_2 | stickyUfF16_2)) |
    rdn & signResultTempF16_2 & (guardUfF16_2 | roundUfF16_2 | stickyUfF16_2) |
    rup & !signResultTempF16_2 & (guardUfF16_2 | roundUfF16_2 | stickyUfF16_2) |
    rmm & guardUfF16_2
  val roundAdd1UfF16_3 = rne & (guardUfF16_3 & (guardF16_3 | roundUfF16_3 | stickyUfF16_3)) |
    rdn & signResultTempF16_3 & (guardUfF16_3 | roundUfF16_3 | stickyUfF16_3) |
    rup & !signResultTempF16_3 & (guardUfF16_3 | roundUfF16_3 | stickyUfF16_3) |
    rmm & guardUfF16_3

  val exponentAdd1F64 = fractionResultNoRoundF64.andR & roundAdd1F64.asBool
  val exponentAdd1F32_0 = fractionResultNoRoundF32_0.andR & roundAdd1F32_0.asBool
  val exponentAdd1F32_1 = fractionResultNoRoundF32_1.andR & roundAdd1F32_1.asBool
  val exponentAdd1F16_0 = fractionResultNoRoundF16_0.andR & roundAdd1F16_0.asBool
  val exponentAdd1F16_1 = fractionResultNoRoundF16_1.andR & roundAdd1F16_1.asBool
  val exponentAdd1F16_2 = fractionResultNoRoundF16_2.andR & roundAdd1F16_2.asBool
  val exponentAdd1F16_3 = fractionResultNoRoundF16_3.andR & roundAdd1F16_3.asBool

  val exponentResultAddValueF64 = Mux(exponentAdd1F64 | isFixF64, eGreaterF64 - lshiftValueF64 + 1.U, eGreaterF64 - lshiftValueF64)
  val exponentResultAddValueF32_0 = Mux(exponentAdd1F32_0 | isFixF32_0, eGreaterF32_0 - lshiftValueF32_0 + 1.U, eGreaterF32_0 - lshiftValueF32_0)
  val exponentResultAddValueF32_1 = Mux(exponentAdd1F32_1 | isFixF32_1, eGreaterF32_1 - lshiftValueF32_1 + 1.U, eGreaterF32_1 - lshiftValueF32_1)
  val exponentResultAddValueF16_0 = Mux(exponentAdd1F16_0 | isFixF16_0, eGreaterF16_0 - lshiftValueF16_0 + 1.U, eGreaterF16_0 - lshiftValueF16_0)
  val exponentResultAddValueF16_1 = Mux(exponentAdd1F16_1 | isFixF16_1, eGreaterF16_1 - lshiftValueF16_1 + 1.U, eGreaterF16_1 - lshiftValueF16_1)
  val exponentResultAddValueF16_2 = Mux(exponentAdd1F16_2 | isFixF16_2, eGreaterF16_2 - lshiftValueF16_2 + 1.U, eGreaterF16_2 - lshiftValueF16_2)
  val exponentResultAddValueF16_3 = Mux(exponentAdd1F16_3 | isFixF16_3, eGreaterF16_3 - lshiftValueF16_3 + 1.U, eGreaterF16_3 - lshiftValueF16_3)
  val exponentOverflowF64 = exponentResultAddValueF64.head(1).asBool | exponentResultAddValueF64.tail(1).andR
  val exponentOverflowF32_0 = exponentResultAddValueF32_0.head(1).asBool | exponentResultAddValueF32_0.tail(1).andR
  val exponentOverflowF32_1 = exponentResultAddValueF32_1.head(1).asBool | exponentResultAddValueF32_1.tail(1).andR
  val exponentOverflowF16_0 = exponentResultAddValueF16_0.head(1).asBool | exponentResultAddValueF16_0.tail(1).andR
  val exponentOverflowF16_1 = exponentResultAddValueF16_1.head(1).asBool | exponentResultAddValueF16_1.tail(1).andR
  val exponentOverflowF16_2 = exponentResultAddValueF16_2.head(1).asBool | exponentResultAddValueF16_2.tail(1).andR
  val exponentOverflowF16_3 = exponentResultAddValueF16_3.head(1).asBool | exponentResultAddValueF16_3.tail(1).andR

  val exponentIsMinF64 = !lshiftAdderInvFixF64.head(1).asBool & lshiftMaskValidF64 & !isFixF64
  val exponentIsMinF32_0 = !lshiftAdderInvFixF32_0.head(1).asBool & lshiftMaskValidF32_0 & !isFixF32_0
  val exponentIsMinF32_1 = !lshiftAdderInvFixF32_1.head(1).asBool & lshiftMaskValidF32_1 & !isFixF32_1
  val exponentIsMinF16_0 = !lshiftAdderInvFixF16_0.head(1).asBool & lshiftMaskValidF16_0 & !isFixF16_0
  val exponentIsMinF16_1 = !lshiftAdderInvFixF16_1.head(1).asBool & lshiftMaskValidF16_1 & !isFixF16_1
  val exponentIsMinF16_2 = !lshiftAdderInvFixF16_2.head(1).asBool & lshiftMaskValidF16_2 & !isFixF16_2
  val exponentIsMinF16_3 = !lshiftAdderInvFixF16_3.head(1).asBool & lshiftMaskValidF16_3 & !isFixF16_3

  val exponentResultTempF64 = Mux(exponentIsMinF64, Cat(0.U((exponentWidth - 1).W), exponentAdd1F64), exponentResultAddValueF64(exponentWidth - 1, 0))
  val exponentResultTempF32_0 = Mux(exponentIsMinF32_0, Cat(0.U((8 - 1).W), exponentAdd1F32_0), exponentResultAddValueF32_0(8 - 1, 0))
  val exponentResultTempF32_1 = Mux(exponentIsMinF32_1, Cat(0.U((8 - 1).W), exponentAdd1F32_1), exponentResultAddValueF32_1(8 - 1, 0))
  val exponentResultTempF16_0 = Mux(exponentIsMinF16_0, Cat(0.U((5 - 1).W), exponentAdd1F16_0), exponentResultAddValueF16_0(5 - 1, 0))
  val exponentResultTempF16_1 = Mux(exponentIsMinF16_1, Cat(0.U((5 - 1).W), exponentAdd1F16_1), exponentResultAddValueF16_1(5 - 1, 0))
  val exponentResultTempF16_2 = Mux(exponentIsMinF16_2, Cat(0.U((5 - 1).W), exponentAdd1F16_2), exponentResultAddValueF16_2(5 - 1, 0))
  val exponentResultTempF16_3 = Mux(exponentIsMinF16_3, Cat(0.U((5 - 1).W), exponentAdd1F16_3), exponentResultAddValueF16_3(5 - 1, 0))

  val fractionResultTempF64 = Mux(roundAdd1F64.asBool, fractionResultRoundF64.tail(1), fractionResultNoRoundF64)
  val fractionResultTempF32_0 = Mux(roundAdd1F32_0.asBool, fractionResultRoundF32_0.tail(1), fractionResultNoRoundF32_0)
  val fractionResultTempF32_1 = Mux(roundAdd1F32_1.asBool, fractionResultRoundF32_1.tail(1), fractionResultNoRoundF32_1)
  val fractionResultTempF16_0 = Mux(roundAdd1F16_0.asBool, fractionResultRoundF16_0.tail(1), fractionResultNoRoundF16_0)
  val fractionResultTempF16_1 = Mux(roundAdd1F16_1.asBool, fractionResultRoundF16_1.tail(1), fractionResultNoRoundF16_1)
  val fractionResultTempF16_2 = Mux(roundAdd1F16_2.asBool, fractionResultRoundF16_2.tail(1), fractionResultNoRoundF16_2)
  val fractionResultTempF16_3 = Mux(roundAdd1F16_3.asBool, fractionResultRoundF16_3.tail(1), fractionResultNoRoundF16_3)

  val nxF64 = guardF64 | roundF64 | stickyF64
  val nxF32_0 = guardF32_0 | roundF32_0 | stickyF32_0
  val nxF32_1 = guardF32_1 | roundF32_1 | stickyF32_1
  val nxF16_0 = guardF16_0 | roundF16_0 | stickyF16_0
  val nxF16_1 = guardF16_1 | roundF16_1 | stickyF16_1
  val nxF16_2 = guardF16_2 | roundF16_2 | stickyF16_2
  val nxF16_3 = guardF16_3 | roundF16_3 | stickyF16_3
  val ufF64 = nxF64 & exponentIsMinF64 & (!exponentAdd1F64 | !(guardF64 & roundAdd1UfF64))
  val ufF32_0 = nxF32_0 & exponentIsMinF32_0 & (!exponentAdd1F32_0 | !(guardF32_0 & roundAdd1UfF32_0))
  val ufF32_1 = nxF32_1 & exponentIsMinF32_1 & (!exponentAdd1F32_1 | !(guardF32_1 & roundAdd1UfF32_1))
  val ufF16_0 = nxF16_0 & exponentIsMinF16_0 & (!exponentAdd1F16_0 | !(guardF16_0 & roundAdd1UfF16_0))
  val ufF16_1 = nxF16_1 & exponentIsMinF16_1 & (!exponentAdd1F16_1 | !(guardF16_1 & roundAdd1UfF16_1))
  val ufF16_2 = nxF16_2 & exponentIsMinF16_2 & (!exponentAdd1F16_2 | !(guardF16_2 & roundAdd1UfF16_2))
  val ufF16_3 = nxF16_3 & exponentIsMinF16_3 & (!exponentAdd1F16_3 | !(guardF16_3 & roundAdd1UfF16_3))

  val fpAIsZeroF64 = !fpAIsFpCanonicalNAN & !fpASignificandF64.orR
  val fpAIsZeroF32_0 = !fpAIsFpCanonicalNAN & !fpASignificandF32_0.orR
  val fpAIsZeroF32_1 = !fpAIsFpCanonicalNAN & !fpASignificandF32_1.orR
  val fpAIsZeroF16_0 = !fpAIsFpCanonicalNAN & !fpASignificandF16_0.orR
  val fpAIsZeroF16_1 = !fpAIsFpCanonicalNAN & !fpASignificandF16_1.orR
  val fpAIsZeroF16_2 = !fpAIsFpCanonicalNAN & !fpASignificandF16_2.orR
  val fpAIsZeroF16_3 = !fpAIsFpCanonicalNAN & !fpASignificandF16_3.orR
  val fpBIsZeroF64 = !fpBIsFpCanonicalNAN & !fpBSignificandF64.orR
  val fpBIsZeroF32_0 = !fpBIsFpCanonicalNAN & !fpBSignificandF32_0.orR
  val fpBIsZeroF32_1 = !fpBIsFpCanonicalNAN & !fpBSignificandF32_1.orR
  val fpBIsZeroF16_0 = !fpBIsFpCanonicalNAN & !fpBSignificandF16_0.orR
  val fpBIsZeroF16_1 = !fpBIsFpCanonicalNAN & !fpBSignificandF16_1.orR
  val fpBIsZeroF16_2 = !fpBIsFpCanonicalNAN & !fpBSignificandF16_2.orR
  val fpBIsZeroF16_3 = !fpBIsFpCanonicalNAN & !fpBSignificandF16_3.orR
  val fpCIsZeroF64 = !fpCIsFpCanonicalNAN & !fpCSignificandF64.orR
  val fpCIsZeroF32_0 = !fpCIsFpCanonicalNAN & !fpCSignificandF32_0.orR
  val fpCIsZeroF32_1 = !fpCIsFpCanonicalNAN & !fpCSignificandF32_1.orR
  val fpCIsZeroF16_0 = !fpCIsFpCanonicalNAN & !fpCSignificandF16_0.orR
  val fpCIsZeroF16_1 = !fpCIsFpCanonicalNAN & !fpCSignificandF16_1.orR
  val fpCIsZeroF16_2 = !fpCIsFpCanonicalNAN & !fpCSignificandF16_2.orR
  val fpCIsZeroF16_3 = !fpCIsFpCanonicalNAN & !fpCSignificandF16_3.orR

  val normalResultIsZeroF64 = !adderF64.orR
  val normalResultIsZeroF32_0 = !adderF32_0.orR
  val normalResultIsZeroF32_1 = !adderF32_1.orR
  val normalResultIsZeroF16_0 = !adderF16_0.orR
  val normalResultIsZeroF16_1 = !adderF16_1.orR
  val normalResultIsZeroF16_2 = !adderF16_2.orR
  val normalResultIsZeroF16_3 = !adderF16_3.orR
  val hasZeroF64 = fpAIsZeroF64 | fpBIsZeroF64 | fpCIsZeroF64 | normalResultIsZeroF64
  val hasZeroF32_0 = fpAIsZeroF32_0 | fpBIsZeroF32_0 | fpCIsZeroF32_0 | normalResultIsZeroF32_0
  val hasZeroF32_1 = fpAIsZeroF32_1 | fpBIsZeroF32_1 | fpCIsZeroF32_1 | normalResultIsZeroF32_1
  val hasZeroF16_0 = fpAIsZeroF16_0 | fpBIsZeroF16_0 | fpCIsZeroF16_0 | normalResultIsZeroF16_0
  val hasZeroF16_1 = fpAIsZeroF16_1 | fpBIsZeroF16_1 | fpCIsZeroF16_1 | normalResultIsZeroF16_1
  val hasZeroF16_2 = fpAIsZeroF16_2 | fpBIsZeroF16_2 | fpCIsZeroF16_2 | normalResultIsZeroF16_2
  val hasZeroF16_3 = fpAIsZeroF16_3 | fpBIsZeroF16_3 | fpCIsZeroF16_3 | normalResultIsZeroF16_3

  val normalResultF64 = Cat(signResultTempF64, exponentResultTempF64, fractionResultTempF64)
  val normalResultF32_0 = Cat(signResultTempF32_0, exponentResultTempF32_0, fractionResultTempF32_0)
  val normalResultF32_1 = Cat(signResultTempF32_1, exponentResultTempF32_1, fractionResultTempF32_1)
  val normalResultF16_0 = Cat(signResultTempF16_0, exponentResultTempF16_0, fractionResultTempF16_0)
  val normalResultF16_1 = Cat(signResultTempF16_1, exponentResultTempF16_1, fractionResultTempF16_1)
  val normalResultF16_2 = Cat(signResultTempF16_2, exponentResultTempF16_2, fractionResultTempF16_2)
  val normalResultF16_3 = Cat(signResultTempF16_3, exponentResultTempF16_3, fractionResultTempF16_3)
  val resultOverflowUpF64 = Cat(signResultTempF64, Fill(exponentWidth, 1.U), 0.U((significandWidth - 1).W))
  val resultOverflowUpF32_0 = Cat(signResultTempF32_0, Fill(8, 1.U), 0.U((24 - 1).W))
  val resultOverflowUpF32_1 = Cat(signResultTempF32_1, Fill(8, 1.U), 0.U((24 - 1).W))
  val resultOverflowUpF16_0 = Cat(signResultTempF16_0, Fill(5, 1.U), 0.U((11 - 1).W))
  val resultOverflowUpF16_1 = Cat(signResultTempF16_1, Fill(5, 1.U), 0.U((11 - 1).W))
  val resultOverflowUpF16_2 = Cat(signResultTempF16_2, Fill(5, 1.U), 0.U((11 - 1).W))
  val resultOverflowUpF16_3 = Cat(signResultTempF16_3, Fill(5, 1.U), 0.U((11 - 1).W))
  val resultOverflowDownF64 = Cat(signResultTempF64, Fill(exponentWidth - 1, 1.U), 0.U, Fill(significandWidth - 1, 1.U))
  val resultOverflowDownF32_0 = Cat(signResultTempF32_0, Fill(8 - 1, 1.U), 0.U, Fill(24 - 1, 1.U))
  val resultOverflowDownF32_1 = Cat(signResultTempF32_1, Fill(8 - 1, 1.U), 0.U, Fill(24 - 1, 1.U))
  val resultOverflowDownF16_0 = Cat(signResultTempF16_0, Fill(5 - 1, 1.U), 0.U, Fill(11 - 1, 1.U))
  val resultOverflowDownF16_1 = Cat(signResultTempF16_1, Fill(5 - 1, 1.U), 0.U, Fill(11 - 1, 1.U))
  val resultOverflowDownF16_2 = Cat(signResultTempF16_2, Fill(5 - 1, 1.U), 0.U, Fill(11 - 1, 1.U))
  val resultOverflowDownF16_3 = Cat(signResultTempF16_3, Fill(5 - 1, 1.U), 0.U, Fill(11 - 1, 1.U))

  val fpAIsNanF64 = fpAIsFpCanonicalNAN | Mux(resWidening & isFp64, widenEaF32_0.andR, eaF64.andR) & fpASignificandF64.tail(1).orR
  val fpAIsNanF32_0 = fpAIsFpCanonicalNAN | Mux(resWidening & isFp32, widenEaF16_0.andR, eaF32_0.andR) & fpASignificandF32_0.tail(1).orR
  val fpAIsNanF32_1 = fpAIsFpCanonicalNAN | Mux(resWidening & isFp32, widenEaF16_1.andR, eaF32_1.andR) & fpASignificandF32_1.tail(1).orR
  val fpAIsNanF16_0 = fpAIsFpCanonicalNAN | eaF16_0.andR & fpASignificandF16_0.tail(1).orR
  val fpAIsNanF16_1 = fpAIsFpCanonicalNAN | eaF16_1.andR & fpASignificandF16_1.tail(1).orR
  val fpAIsNanF16_2 = fpAIsFpCanonicalNAN | eaF16_2.andR & fpASignificandF16_2.tail(1).orR
  val fpAIsNanF16_3 = fpAIsFpCanonicalNAN | eaF16_3.andR & fpASignificandF16_3.tail(1).orR
  val fpBIsNanF64 = fpBIsFpCanonicalNAN | Mux(resWidening & isFp64, widenEbF32_0.andR, ebF64.andR) & fpBSignificandF64.tail(1).orR
  val fpBIsNanF32_0 = fpBIsFpCanonicalNAN | Mux(resWidening & isFp32, widenEbF16_0.andR, ebF32_0.andR) & fpBSignificandF32_0.tail(1).orR
  val fpBIsNanF32_1 = fpBIsFpCanonicalNAN | Mux(resWidening & isFp32, widenEbF16_1.andR, ebF32_1.andR) & fpBSignificandF32_1.tail(1).orR
  val fpBIsNanF16_0 = fpBIsFpCanonicalNAN | ebF16_0.andR & fpBSignificandF16_0.tail(1).orR
  val fpBIsNanF16_1 = fpBIsFpCanonicalNAN | ebF16_1.andR & fpBSignificandF16_1.tail(1).orR
  val fpBIsNanF16_2 = fpBIsFpCanonicalNAN | ebF16_2.andR & fpBSignificandF16_2.tail(1).orR
  val fpBIsNanF16_3 = fpBIsFpCanonicalNAN | ebF16_3.andR & fpBSignificandF16_3.tail(1).orR
  val fpCIsNanF64 = fpCIsFpCanonicalNAN | ecF64.andR & fpCSignificandF64.tail(1).orR
  val fpCIsNanF32_0 = fpCIsFpCanonicalNAN | ecF32_0.andR & fpCSignificandF32_0.tail(1).orR
  val fpCIsNanF32_1 = fpCIsFpCanonicalNAN | ecF32_1.andR & fpCSignificandF32_1.tail(1).orR
  val fpCIsNanF16_0 = fpCIsFpCanonicalNAN | ecF16_0.andR & fpCSignificandF16_0.tail(1).orR
  val fpCIsNanF16_1 = fpCIsFpCanonicalNAN | ecF16_1.andR & fpCSignificandF16_1.tail(1).orR
  val fpCIsNanF16_2 = fpCIsFpCanonicalNAN | ecF16_2.andR & fpCSignificandF16_2.tail(1).orR
  val fpCIsNanF16_3 = fpCIsFpCanonicalNAN | ecF16_3.andR & fpCSignificandF16_3.tail(1).orR
  val fpAIsSnanF64 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp64, widenEaF32_0.andR, eaF64.andR) & !fpASignificandF64.tail(1).head(1) & fpASignificandF64.tail(2).orR
  val fpAIsSnanF32_0 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEaF16_0.andR, eaF32_0.andR) & !fpASignificandF32_0.tail(1).head(1) & fpASignificandF32_0.tail(2).orR
  val fpAIsSnanF32_1 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEaF16_1.andR, eaF32_1.andR) & !fpASignificandF32_1.tail(1).head(1) & fpASignificandF32_1.tail(2).orR
  val fpAIsSnanF16_0 = !fpAIsFpCanonicalNAN & eaF16_0.andR & !fpASignificandF16_0.tail(1).head(1) & fpASignificandF16_0.tail(2).orR
  val fpAIsSnanF16_1 = !fpAIsFpCanonicalNAN & eaF16_1.andR & !fpASignificandF16_1.tail(1).head(1) & fpASignificandF16_1.tail(2).orR
  val fpAIsSnanF16_2 = !fpAIsFpCanonicalNAN & eaF16_2.andR & !fpASignificandF16_2.tail(1).head(1) & fpASignificandF16_2.tail(2).orR
  val fpAIsSnanF16_3 = !fpAIsFpCanonicalNAN & eaF16_3.andR & !fpASignificandF16_3.tail(1).head(1) & fpASignificandF16_3.tail(2).orR
  val fpBIsSnanF64 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp64, widenEbF32_0.andR, ebF64.andR) & !fpBSignificandF64.tail(1).head(1) & fpBSignificandF64.tail(2).orR
  val fpBIsSnanF32_0 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEbF16_0.andR, ebF32_0.andR) & !fpBSignificandF32_0.tail(1).head(1) & fpBSignificandF32_0.tail(2).orR
  val fpBIsSnanF32_1 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEbF16_1.andR, ebF32_1.andR) & !fpBSignificandF32_1.tail(1).head(1) & fpBSignificandF32_1.tail(2).orR
  val fpBIsSnanF16_0 = !fpBIsFpCanonicalNAN & ebF16_0.andR & !fpBSignificandF16_0.tail(1).head(1) & fpBSignificandF16_0.tail(2).orR
  val fpBIsSnanF16_1 = !fpBIsFpCanonicalNAN & ebF16_1.andR & !fpBSignificandF16_1.tail(1).head(1) & fpBSignificandF16_1.tail(2).orR
  val fpBIsSnanF16_2 = !fpBIsFpCanonicalNAN & ebF16_2.andR & !fpBSignificandF16_2.tail(1).head(1) & fpBSignificandF16_2.tail(2).orR
  val fpBIsSnanF16_3 = !fpBIsFpCanonicalNAN & ebF16_3.andR & !fpBSignificandF16_3.tail(1).head(1) & fpBSignificandF16_3.tail(2).orR
  val fpCIsSnanF64 = !fpCIsFpCanonicalNAN & ecF64.andR & !fpCSignificandF64.tail(1).head(1) & fpCSignificandF64.tail(2).orR
  val fpCIsSnanF32_0 = !fpCIsFpCanonicalNAN & ecF32_0.andR & !fpCSignificandF32_0.tail(1).head(1) & fpCSignificandF32_0.tail(2).orR
  val fpCIsSnanF32_1 = !fpCIsFpCanonicalNAN & ecF32_1.andR & !fpCSignificandF32_1.tail(1).head(1) & fpCSignificandF32_1.tail(2).orR
  val fpCIsSnanF16_0 = !fpCIsFpCanonicalNAN & ecF16_0.andR & !fpCSignificandF16_0.tail(1).head(1) & fpCSignificandF16_0.tail(2).orR
  val fpCIsSnanF16_1 = !fpCIsFpCanonicalNAN & ecF16_1.andR & !fpCSignificandF16_1.tail(1).head(1) & fpCSignificandF16_1.tail(2).orR
  val fpCIsSnanF16_2 = !fpCIsFpCanonicalNAN & ecF16_2.andR & !fpCSignificandF16_2.tail(1).head(1) & fpCSignificandF16_2.tail(2).orR
  val fpCIsSnanF16_3 = !fpCIsFpCanonicalNAN & ecF16_3.andR & !fpCSignificandF16_3.tail(1).head(1) & fpCSignificandF16_3.tail(2).orR

  val hasNanF64 = fpAIsNanF64 | fpBIsNanF64 | fpCIsNanF64
  val hasNanF32_0 = fpAIsNanF32_0 | fpBIsNanF32_0 | fpCIsNanF32_0
  val hasNanF32_1 = fpAIsNanF32_1 | fpBIsNanF32_1 | fpCIsNanF32_1
  val hasNanF16_0 = fpAIsNanF16_0 | fpBIsNanF16_0 | fpCIsNanF16_0
  val hasNanF16_1 = fpAIsNanF16_1 | fpBIsNanF16_1 | fpCIsNanF16_1
  val hasNanF16_2 = fpAIsNanF16_2 | fpBIsNanF16_2 | fpCIsNanF16_2
  val hasNanF16_3 = fpAIsNanF16_3 | fpBIsNanF16_3 | fpCIsNanF16_3
  val hasSnanF64 = fpAIsSnanF64 | fpBIsSnanF64 | fpCIsSnanF64
  val hasSnanF32_0 = fpAIsSnanF32_0 | fpBIsSnanF32_0 | fpCIsSnanF32_0
  val hasSnanF32_1 = fpAIsSnanF32_1 | fpBIsSnanF32_1 | fpCIsSnanF32_1
  val hasSnanF16_0 = fpAIsSnanF16_0 | fpBIsSnanF16_0 | fpCIsSnanF16_0
  val hasSnanF16_1 = fpAIsSnanF16_1 | fpBIsSnanF16_1 | fpCIsSnanF16_1
  val hasSnanF16_2 = fpAIsSnanF16_2 | fpBIsSnanF16_2 | fpCIsSnanF16_2
  val hasSnanF16_3 = fpAIsSnanF16_3 | fpBIsSnanF16_3 | fpCIsSnanF16_3

  val fpAIsInfF64 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp64, widenEaF32_0.andR, eaF64.andR) & !fpASignificandF64.tail(1).orR
  val fpAIsInfF32_0 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEaF16_0.andR, eaF32_0.andR) & !fpASignificandF32_0.tail(1).orR
  val fpAIsInfF32_1 = !fpAIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEaF16_1.andR, eaF32_1.andR) & !fpASignificandF32_1.tail(1).orR
  val fpAIsInfF16_0 = !fpAIsFpCanonicalNAN & eaF16_0.andR & !fpASignificandF16_0.tail(1).orR
  val fpAIsInfF16_1 = !fpAIsFpCanonicalNAN & eaF16_1.andR & !fpASignificandF16_1.tail(1).orR
  val fpAIsInfF16_2 = !fpAIsFpCanonicalNAN & eaF16_2.andR & !fpASignificandF16_2.tail(1).orR
  val fpAIsInfF16_3 = !fpAIsFpCanonicalNAN & eaF16_3.andR & !fpASignificandF16_3.tail(1).orR
  val fpBIsInfF64 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp64, widenEbF32_0.andR, ebF64.andR) & !fpBSignificandF64.tail(1).orR
  val fpBIsInfF32_0 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEbF16_0.andR, ebF32_0.andR) & !fpBSignificandF32_0.tail(1).orR
  val fpBIsInfF32_1 = !fpBIsFpCanonicalNAN & Mux(resWidening & isFp32, widenEbF16_1.andR, ebF32_1.andR) & !fpBSignificandF32_1.tail(1).orR
  val fpBIsInfF16_0 = !fpBIsFpCanonicalNAN & ebF16_0.andR & !fpBSignificandF16_0.tail(1).orR
  val fpBIsInfF16_1 = !fpBIsFpCanonicalNAN & ebF16_1.andR & !fpBSignificandF16_1.tail(1).orR
  val fpBIsInfF16_2 = !fpBIsFpCanonicalNAN & ebF16_2.andR & !fpBSignificandF16_2.tail(1).orR
  val fpBIsInfF16_3 = !fpBIsFpCanonicalNAN & ebF16_3.andR & !fpBSignificandF16_3.tail(1).orR
  val fpCIsInfF64 = !fpCIsFpCanonicalNAN & ecF64.andR & !fpCSignificandF64.tail(1).orR
  val fpCIsInfF32_0 = !fpCIsFpCanonicalNAN & ecF32_0.andR & !fpCSignificandF32_0.tail(1).orR
  val fpCIsInfF32_1 = !fpCIsFpCanonicalNAN & ecF32_1.andR & !fpCSignificandF32_1.tail(1).orR
  val fpCIsInfF16_0 = !fpCIsFpCanonicalNAN & ecF16_0.andR & !fpCSignificandF16_0.tail(1).orR
  val fpCIsInfF16_1 = !fpCIsFpCanonicalNAN & ecF16_1.andR & !fpCSignificandF16_1.tail(1).orR
  val fpCIsInfF16_2 = !fpCIsFpCanonicalNAN & ecF16_2.andR & !fpCSignificandF16_2.tail(1).orR
  val fpCIsInfF16_3 = !fpCIsFpCanonicalNAN & ecF16_3.andR & !fpCSignificandF16_3.tail(1).orR
  val hasInfF64 = fpAIsInfF64 | fpBIsInfF64 | fpCIsInfF64
  val hasInfF32_0 = fpAIsInfF32_0 | fpBIsInfF32_0 | fpCIsInfF32_0
  val hasInfF32_1 = fpAIsInfF32_1 | fpBIsInfF32_1 | fpCIsInfF32_1
  val hasInfF16_0 = fpAIsInfF16_0 | fpBIsInfF16_0 | fpCIsInfF16_0
  val hasInfF16_1 = fpAIsInfF16_1 | fpBIsInfF16_1 | fpCIsInfF16_1
  val hasInfF16_2 = fpAIsInfF16_2 | fpBIsInfF16_2 | fpCIsInfF16_2
  val hasInfF16_3 = fpAIsInfF16_3 | fpBIsInfF16_3 | fpCIsInfF16_3

  val resultInfF64 = Cat(Fill(exponentWidth, 1.U), Fill(significandWidth - 1, 0.U))
  val resultInfF32_0 = Cat(Fill(8, 1.U), Fill(24 - 1, 0.U))
  val resultInfF32_1 = Cat(Fill(8, 1.U), Fill(24 - 1, 0.U))
  val resultInfF16_0 = Cat(Fill(5, 1.U), Fill(11 - 1, 0.U))
  val resultInfF16_1 = Cat(Fill(5, 1.U), Fill(11 - 1, 0.U))
  val resultInfF16_2 = Cat(Fill(5, 1.U), Fill(11 - 1, 0.U))
  val resultInfF16_3 = Cat(Fill(5, 1.U), Fill(11 - 1, 0.U))
  val resultNanF64 = Cat(0.U, Fill(exponentWidth + 1, 1.U), 0.U((significandWidth - 2).W))
  val resultNanF32_0 = Cat(0.U, Fill(8 + 1, 1.U), 0.U((24 - 2).W))
  val resultNanF32_1 = Cat(0.U, Fill(8 + 1, 1.U), 0.U((24 - 2).W))
  val resultNanF16_0 = Cat(0.U, Fill(5 + 1, 1.U), 0.U((11 - 2).W))
  val resultNanF16_1 = Cat(0.U, Fill(5 + 1, 1.U), 0.U((11 - 2).W))
  val resultNanF16_2 = Cat(0.U, Fill(5 + 1, 1.U), 0.U((11 - 2).W))
  val resultNanF16_3 = Cat(0.U, Fill(5 + 1, 1.U), 0.U((11 - 2).W))

  val zeroFromOperandF64 = Cat(Mux(fpCIsZeroF64, Mux(isVfmul, signABF64, (signABF64 & signCF64) | (rdn & (signABF64 ^ signCF64))), fpCF64.head(1)), fpCF64.tail(1))
  val zeroFromOperandF32_0 = Cat(Mux(fpCIsZeroF32_0, Mux(isVfmul, signABF32_0, (signABF32_0 & signCF32_0) | (rdn & (signABF32_0 ^ signCF32_0))), fpCF32_0.head(1)), fpCF32_0.tail(1))
  val zeroFromOperandF32_1 = Cat(Mux(fpCIsZeroF32_1, Mux(isVfmul, signABF32_1, (signABF32_1 & signCF32_1) | (rdn & (signABF32_1 ^ signCF32_1))), fpCF32_1.head(1)), fpCF32_1.tail(1))
  val zeroFromOperandF16_0 = Cat(Mux(fpCIsZeroF16_0, Mux(isVfmul, signABF16_0, (signABF16_0 & signCF16_0) | (rdn & (signABF16_0 ^ signCF16_0))), fpCF16_0.head(1)), fpCF16_0.tail(1))
  val zeroFromOperandF16_1 = Cat(Mux(fpCIsZeroF16_1, Mux(isVfmul, signABF16_1, (signABF16_1 & signCF16_1) | (rdn & (signABF16_1 ^ signCF16_1))), fpCF16_1.head(1)), fpCF16_1.tail(1))
  val zeroFromOperandF16_2 = Cat(Mux(fpCIsZeroF16_2, Mux(isVfmul, signABF16_2, (signABF16_2 & signCF16_2) | (rdn & (signABF16_2 ^ signCF16_2))), fpCF16_2.head(1)), fpCF16_2.tail(1))
  val zeroFromOperandF16_3 = Cat(Mux(fpCIsZeroF16_3, Mux(isVfmul, signABF16_3, (signABF16_3 & signCF16_3) | (rdn & (signABF16_3 ^ signCF16_3))), fpCF16_3.head(1)), fpCF16_3.tail(1))
  val zeroNormalF64 = Cat(rdn, 0.U((64 - 1).W))
  val zeroNormalF32_0 = Cat(rdn, 0.U((32 - 1).W))
  val zeroNormalF32_1 = Cat(rdn, 0.U((32 - 1).W))
  val zeroNormalF16_0 = Cat(rdn, 0.U((16 - 1).W))
  val zeroNormalF16_1 = Cat(rdn, 0.U((16 - 1).W))
  val zeroNormalF16_2 = Cat(rdn, 0.U((16 - 1).W))
  val zeroNormalF16_3 = Cat(rdn, 0.U((16 - 1).W))

  val hasNanIsNvF64 = hasSnanF64 | (fpAIsInfF64 & fpBIsZeroF64) | (fpAIsZeroF64 & fpBIsInfF64)
  val hasNanIsNvF32_0 = hasSnanF32_0 | (fpAIsInfF32_0 & fpBIsZeroF32_0) | (fpAIsZeroF32_0 & fpBIsInfF32_0)
  val hasNanIsNvF32_1 = hasSnanF32_1 | (fpAIsInfF32_1 & fpBIsZeroF32_1) | (fpAIsZeroF32_1 & fpBIsInfF32_1)
  val hasNanIsNvF16_0 = hasSnanF16_0 | (fpAIsInfF16_0 & fpBIsZeroF16_0) | (fpAIsZeroF16_0 & fpBIsInfF16_0)
  val hasNanIsNvF16_1 = hasSnanF16_1 | (fpAIsInfF16_1 & fpBIsZeroF16_1) | (fpAIsZeroF16_1 & fpBIsInfF16_1)
  val hasNanIsNvF16_2 = hasSnanF16_2 | (fpAIsInfF16_2 & fpBIsZeroF16_2) | (fpAIsZeroF16_2 & fpBIsInfF16_2)
  val hasNanIsNvF16_3 = hasSnanF16_3 | (fpAIsInfF16_3 & fpBIsZeroF16_3) | (fpAIsZeroF16_3 & fpBIsInfF16_3)
  val hasInfIsNvF64 = ((fpAIsInfF64 & fpBIsZeroF64) | (fpAIsZeroF64 & fpBIsInfF64)) | (fpCIsInfF64 & (fpAIsInfF64 | fpBIsInfF64) & (signCF64 ^ signABF64))
  val hasInfIsNvF32_0 = ((fpAIsInfF32_0 & fpBIsZeroF32_0) | (fpAIsZeroF32_0 & fpBIsInfF32_0)) | (fpCIsInfF32_0 & (fpAIsInfF32_0 | fpBIsInfF32_0) & (signCF32_0 ^ signABF32_0))
  val hasInfIsNvF32_1 = ((fpAIsInfF32_1 & fpBIsZeroF32_1) | (fpAIsZeroF32_1 & fpBIsInfF32_1)) | (fpCIsInfF32_1 & (fpAIsInfF32_1 | fpBIsInfF32_1) & (signCF32_1 ^ signABF32_1))
  val hasInfIsNvF16_0 = ((fpAIsInfF16_0 & fpBIsZeroF16_0) | (fpAIsZeroF16_0 & fpBIsInfF16_0)) | (fpCIsInfF16_0 & (fpAIsInfF16_0 | fpBIsInfF16_0) & (signCF16_0 ^ signABF16_0))
  val hasInfIsNvF16_1 = ((fpAIsInfF16_1 & fpBIsZeroF16_1) | (fpAIsZeroF16_1 & fpBIsInfF16_1)) | (fpCIsInfF16_1 & (fpAIsInfF16_1 | fpBIsInfF16_1) & (signCF16_1 ^ signABF16_1))
  val hasInfIsNvF16_2 = ((fpAIsInfF16_2 & fpBIsZeroF16_2) | (fpAIsZeroF16_2 & fpBIsInfF16_2)) | (fpCIsInfF16_2 & (fpAIsInfF16_2 | fpBIsInfF16_2) & (signCF16_2 ^ signABF16_2))
  val hasInfIsNvF16_3 = ((fpAIsInfF16_3 & fpBIsZeroF16_3) | (fpAIsZeroF16_3 & fpBIsInfF16_3)) | (fpCIsInfF16_3 & (fpAIsInfF16_3 | fpBIsInfF16_3) & (signCF16_3 ^ signABF16_3))

  val infResultSignF64 = Mux(fpAIsInfF64 | fpBIsInfF64, signABF64, signCF64)
  val infResultSignF32_0 = Mux(fpAIsInfF32_0 | fpBIsInfF32_0, signABF32_0, signCF32_0)
  val infResultSignF32_1 = Mux(fpAIsInfF32_1 | fpBIsInfF32_1, signABF32_1, signCF32_1)
  val infResultSignF16_0 = Mux(fpAIsInfF16_0 | fpBIsInfF16_0, signABF16_0, signCF16_0)
  val infResultSignF16_1 = Mux(fpAIsInfF16_1 | fpBIsInfF16_1, signABF16_1, signCF16_1)
  val infResultSignF16_2 = Mux(fpAIsInfF16_2 | fpBIsInfF16_2, signABF16_2, signCF16_2)
  val infResultSignF16_3 = Mux(fpAIsInfF16_3 | fpBIsInfF16_3, signABF16_3, signCF16_3)

  val fpAOrBIsZeroF64 = fpAIsZeroF64 | fpBIsZeroF64
  val fpAOrBIsZeroF32_0 = fpAIsZeroF32_0 | fpBIsZeroF32_0
  val fpAOrBIsZeroF32_1 = fpAIsZeroF32_1 | fpBIsZeroF32_1
  val fpAOrBIsZeroF16_0 = fpAIsZeroF16_0 | fpBIsZeroF16_0
  val fpAOrBIsZeroF16_1 = fpAIsZeroF16_1 | fpBIsZeroF16_1
  val fpAOrBIsZeroF16_2 = fpAIsZeroF16_2 | fpBIsZeroF16_2
  val fpAOrBIsZeroF16_3 = fpAIsZeroF16_3 | fpBIsZeroF16_3

  val isOverflowDownF64 = rtz | (rdn & !signResultTempF64.asBool) | (rup & signResultTempF64.asBool)
  val isOverflowDownF32_0 = rtz | (rdn & !signResultTempF32_0.asBool) | (rup & signResultTempF32_0.asBool)
  val isOverflowDownF32_1 = rtz | (rdn & !signResultTempF32_1.asBool) | (rup & signResultTempF32_1.asBool)
  val isOverflowDownF16_0 = rtz | (rdn & !signResultTempF16_0.asBool) | (rup & signResultTempF16_0.asBool)
  val isOverflowDownF16_1 = rtz | (rdn & !signResultTempF16_1.asBool) | (rup & signResultTempF16_1.asBool)
  val isOverflowDownF16_2 = rtz | (rdn & !signResultTempF16_2.asBool) | (rup & signResultTempF16_2.asBool)
  val isOverflowDownF16_3 = rtz | (rdn & !signResultTempF16_3.asBool) | (rup & signResultTempF16_3.asBool)

  val nvF64 = WireInit(false.B)
  val dzF64 = WireInit(false.B)
  val ofF64 = WireInit(false.B)
  val ufFlagF64 = WireInit(false.B)
  val nvF32_0 = WireInit(false.B)
  val dzF32_0 = WireInit(false.B)
  val ofF32_0 = WireInit(false.B)
  val ufFlagF32_0 = WireInit(false.B)
  val nvF32_1 = WireInit(false.B)
  val dzF32_1 = WireInit(false.B)
  val ofF32_1 = WireInit(false.B)
  val ufFlagF32_1 = WireInit(false.B)
  val nvF16_0 = WireInit(false.B)
  val dzF16_0 = WireInit(false.B)
  val ofF16_0 = WireInit(false.B)
  val ufFlagF16_0 = WireInit(false.B)
  val nvF16_1 = WireInit(false.B)
  val dzF16_1 = WireInit(false.B)
  val ofF16_1 = WireInit(false.B)
  val ufFlagF16_1 = WireInit(false.B)
  val nvF16_2 = WireInit(false.B)
  val dzF16_2 = WireInit(false.B)
  val ofF16_2 = WireInit(false.B)
  val ufFlagF16_2 = WireInit(false.B)
  val nvF16_3 = WireInit(false.B)
  val dzF16_3 = WireInit(false.B)
  val ofF16_3 = WireInit(false.B)
  val ufFlagF16_3 = WireInit(false.B)

  ofF64 := exponentOverflowF64
  ofF32_0 := exponentOverflowF32_0
  ofF32_1 := exponentOverflowF32_1
  ofF16_0 := exponentOverflowF16_0
  ofF16_1 := exponentOverflowF16_1
  ofF16_2 := exponentOverflowF16_2
  ofF16_3 := exponentOverflowF16_3
  ufFlagF64 := ufF64
  ufFlagF32_0 := ufF32_0
  ufFlagF32_1 := ufF32_1
  ufFlagF16_0 := ufF16_0
  ufFlagF16_1 := ufF16_1
  ufFlagF16_2 := ufF16_2
  ufFlagF16_3 := ufF16_3

  val normalFFlagsF64 = Cat(nvF64, dzF64, ofF64, ufFlagF64, nxF64)
  val normalFFlagsF32_0 = Cat(nvF32_0, dzF32_0, ofF32_0, ufFlagF32_0, nxF32_0)
  val normalFFlagsF32_1 = Cat(nvF32_1, dzF32_1, ofF32_1, ufFlagF32_1, nxF32_1)
  val normalFFlagsF16_0 = Cat(nvF16_0, dzF16_0, ofF16_0, ufFlagF16_0, nxF16_0)
  val normalFFlagsF16_1 = Cat(nvF16_1, dzF16_1, ofF16_1, ufFlagF16_1, nxF16_1)
  val normalFFlagsF16_2 = Cat(nvF16_2, dzF16_2, ofF16_2, ufFlagF16_2, nxF16_2)
  val normalFFlagsF16_3 = Cat(nvF16_3, dzF16_3, ofF16_3, ufFlagF16_3, nxF16_3)

  out.data.normalResultF64 := normalResultF64
  out.data.normalResultF32_1 := normalResultF32_1
  out.data.normalResultF32_0 := normalResultF32_0
  out.data.normalResultF16_3 := normalResultF16_3
  out.data.normalResultF16_2 := normalResultF16_2
  out.data.normalResultF16_1 := normalResultF16_1
  out.data.normalResultF16_0 := normalResultF16_0

  out.data.overflowUpResultF64 := resultOverflowUpF64
  out.data.overflowUpResultF32_1 := resultOverflowUpF32_1
  out.data.overflowUpResultF32_0 := resultOverflowUpF32_0
  out.data.overflowUpResultF16_3 := resultOverflowUpF16_3
  out.data.overflowUpResultF16_2 := resultOverflowUpF16_2
  out.data.overflowUpResultF16_1 := resultOverflowUpF16_1
  out.data.overflowUpResultF16_0 := resultOverflowUpF16_0

  out.data.overflowDownResultF64 := resultOverflowDownF64
  out.data.overflowDownResultF32_1 := resultOverflowDownF32_1
  out.data.overflowDownResultF32_0 := resultOverflowDownF32_0
  out.data.overflowDownResultF16_3 := resultOverflowDownF16_3
  out.data.overflowDownResultF16_2 := resultOverflowDownF16_2
  out.data.overflowDownResultF16_1 := resultOverflowDownF16_1
  out.data.overflowDownResultF16_0 := resultOverflowDownF16_0

  out.data.zeroFromOperandResultF64 := zeroFromOperandF64
  out.data.zeroFromOperandResultF32_1 := zeroFromOperandF32_1
  out.data.zeroFromOperandResultF32_0 := zeroFromOperandF32_0
  out.data.zeroFromOperandResultF16_3 := zeroFromOperandF16_3
  out.data.zeroFromOperandResultF16_2 := zeroFromOperandF16_2
  out.data.zeroFromOperandResultF16_1 := zeroFromOperandF16_1
  out.data.zeroFromOperandResultF16_0 := zeroFromOperandF16_0

  out.data.zeroNormalResultF64 := zeroNormalF64
  out.data.zeroNormalResultF32_1 := zeroNormalF32_1
  out.data.zeroNormalResultF32_0 := zeroNormalF32_0
  out.data.zeroNormalResultF16_3 := zeroNormalF16_3
  out.data.zeroNormalResultF16_2 := zeroNormalF16_2
  out.data.zeroNormalResultF16_1 := zeroNormalF16_1
  out.data.zeroNormalResultF16_0 := zeroNormalF16_0

  out.data.infResultF64 := Cat(infResultSignF64, resultInfF64)
  out.data.infResultF32_1 := Cat(infResultSignF32_1, resultInfF32_1)
  out.data.infResultF32_0 := Cat(infResultSignF32_0, resultInfF32_0)
  out.data.infResultF16_3 := Cat(infResultSignF16_3, resultInfF16_3)
  out.data.infResultF16_2 := Cat(infResultSignF16_2, resultInfF16_2)
  out.data.infResultF16_1 := Cat(infResultSignF16_1, resultInfF16_1)
  out.data.infResultF16_0 := Cat(infResultSignF16_0, resultInfF16_0)

  out.data.nanResultF64 := resultNanF64
  out.data.nanResultF32_1 := resultNanF32_1
  out.data.nanResultF32_0 := resultNanF32_0
  out.data.nanResultF16_3 := resultNanF16_3
  out.data.nanResultF16_2 := resultNanF16_2
  out.data.nanResultF16_1 := resultNanF16_1
  out.data.nanResultF16_0 := resultNanF16_0

  out.ctrl.resultIsFp32 := inCtrl.resultIsFp32
  out.ctrl.resultIsFp64 := inCtrl.resultIsFp64
  out.ctrl.resultIsFp16 := isFp16
  out.ctrl.normalFFlagsF64 := normalFFlagsF64
  out.ctrl.normalFFlagsF32_1 := normalFFlagsF32_1
  out.ctrl.normalFFlagsF32_0 := normalFFlagsF32_0
  out.ctrl.normalFFlagsF16_3 := normalFFlagsF16_3
  out.ctrl.normalFFlagsF16_2 := normalFFlagsF16_2
  out.ctrl.normalFFlagsF16_1 := normalFFlagsF16_1
  out.ctrl.normalFFlagsF16_0 := normalFFlagsF16_0

  out.ctrl.hasNanF64 := hasNanF64
  out.ctrl.hasNanF32_1 := hasNanF32_1
  out.ctrl.hasNanF32_0 := hasNanF32_0
  out.ctrl.hasNanF16_3 := hasNanF16_3
  out.ctrl.hasNanF16_2 := hasNanF16_2
  out.ctrl.hasNanF16_1 := hasNanF16_1
  out.ctrl.hasNanF16_0 := hasNanF16_0

  out.ctrl.hasNanIsNvF64 := hasNanIsNvF64
  out.ctrl.hasNanIsNvF32_1 := hasNanIsNvF32_1
  out.ctrl.hasNanIsNvF32_0 := hasNanIsNvF32_0
  out.ctrl.hasNanIsNvF16_3 := hasNanIsNvF16_3
  out.ctrl.hasNanIsNvF16_2 := hasNanIsNvF16_2
  out.ctrl.hasNanIsNvF16_1 := hasNanIsNvF16_1
  out.ctrl.hasNanIsNvF16_0 := hasNanIsNvF16_0

  out.ctrl.hasInfF64 := hasInfF64
  out.ctrl.hasInfF32_1 := hasInfF32_1
  out.ctrl.hasInfF32_0 := hasInfF32_0
  out.ctrl.hasInfF16_3 := hasInfF16_3
  out.ctrl.hasInfF16_2 := hasInfF16_2
  out.ctrl.hasInfF16_1 := hasInfF16_1
  out.ctrl.hasInfF16_0 := hasInfF16_0

  out.ctrl.hasInfIsNvF64 := hasInfIsNvF64
  out.ctrl.hasInfIsNvF32_1 := hasInfIsNvF32_1
  out.ctrl.hasInfIsNvF32_0 := hasInfIsNvF32_0
  out.ctrl.hasInfIsNvF16_3 := hasInfIsNvF16_3
  out.ctrl.hasInfIsNvF16_2 := hasInfIsNvF16_2
  out.ctrl.hasInfIsNvF16_1 := hasInfIsNvF16_1
  out.ctrl.hasInfIsNvF16_0 := hasInfIsNvF16_0

  out.ctrl.isOverflowF64 := exponentOverflowF64
  out.ctrl.isOverflowF32_1 := exponentOverflowF32_1
  out.ctrl.isOverflowF32_0 := exponentOverflowF32_0
  out.ctrl.isOverflowF16_3 := exponentOverflowF16_3
  out.ctrl.isOverflowF16_2 := exponentOverflowF16_2
  out.ctrl.isOverflowF16_1 := exponentOverflowF16_1
  out.ctrl.isOverflowF16_0 := exponentOverflowF16_0

  out.ctrl.isOverflowDownF64 := isOverflowDownF64
  out.ctrl.isOverflowDownF32_1 := isOverflowDownF32_1
  out.ctrl.isOverflowDownF32_0 := isOverflowDownF32_0
  out.ctrl.isOverflowDownF16_3 := isOverflowDownF16_3
  out.ctrl.isOverflowDownF16_2 := isOverflowDownF16_2
  out.ctrl.isOverflowDownF16_1 := isOverflowDownF16_1
  out.ctrl.isOverflowDownF16_0 := isOverflowDownF16_0

  out.ctrl.hasZeroF64 := hasZeroF64
  out.ctrl.hasZeroF32_1 := hasZeroF32_1
  out.ctrl.hasZeroF32_0 := hasZeroF32_0
  out.ctrl.hasZeroF16_3 := hasZeroF16_3
  out.ctrl.hasZeroF16_2 := hasZeroF16_2
  out.ctrl.hasZeroF16_1 := hasZeroF16_1
  out.ctrl.hasZeroF16_0 := hasZeroF16_0

  out.ctrl.normalResultIsZeroF64 := normalResultIsZeroF64
  out.ctrl.normalResultIsZeroF32_1 := normalResultIsZeroF32_1
  out.ctrl.normalResultIsZeroF32_0 := normalResultIsZeroF32_0
  out.ctrl.normalResultIsZeroF16_3 := normalResultIsZeroF16_3
  out.ctrl.normalResultIsZeroF16_2 := normalResultIsZeroF16_2
  out.ctrl.normalResultIsZeroF16_1 := normalResultIsZeroF16_1
  out.ctrl.normalResultIsZeroF16_0 := normalResultIsZeroF16_0

  out.ctrl.fpAOrBIsZeroF64 := fpAOrBIsZeroF64
  out.ctrl.fpAOrBIsZeroF32_1 := fpAOrBIsZeroF32_1
  out.ctrl.fpAOrBIsZeroF32_0 := fpAOrBIsZeroF32_0
  out.ctrl.fpAOrBIsZeroF16_3 := fpAOrBIsZeroF16_3
  out.ctrl.fpAOrBIsZeroF16_2 := fpAOrBIsZeroF16_2
  out.ctrl.fpAOrBIsZeroF16_1 := fpAOrBIsZeroF16_1
  out.ctrl.fpAOrBIsZeroF16_0 := fpAOrBIsZeroF16_0
}

object VectorFMAS2 {
  class S2ToS3Ctrl extends VectorFMA.Bundle {
    val resultIsFp16 = Bool()
    val resultIsFp32 = Bool()
    val resultIsFp64 = Bool()
    val normalFFlagsF64 = UInt(5.W)
    val normalFFlagsF32_1 = UInt(5.W)
    val normalFFlagsF32_0 = UInt(5.W)
    val normalFFlagsF16_3 = UInt(5.W)
    val normalFFlagsF16_2 = UInt(5.W)
    val normalFFlagsF16_1 = UInt(5.W)
    val normalFFlagsF16_0 = UInt(5.W)
    val hasNanF64 = Bool()
    val hasNanF32_1 = Bool()
    val hasNanF32_0 = Bool()
    val hasNanF16_3 = Bool()
    val hasNanF16_2 = Bool()
    val hasNanF16_1 = Bool()
    val hasNanF16_0 = Bool()
    val hasNanIsNvF64 = Bool()
    val hasNanIsNvF32_1 = Bool()
    val hasNanIsNvF32_0 = Bool()
    val hasNanIsNvF16_3 = Bool()
    val hasNanIsNvF16_2 = Bool()
    val hasNanIsNvF16_1 = Bool()
    val hasNanIsNvF16_0 = Bool()
    val hasInfF64 = Bool()
    val hasInfF32_1 = Bool()
    val hasInfF32_0 = Bool()
    val hasInfF16_3 = Bool()
    val hasInfF16_2 = Bool()
    val hasInfF16_1 = Bool()
    val hasInfF16_0 = Bool()
    val hasInfIsNvF64 = Bool()
    val hasInfIsNvF32_1 = Bool()
    val hasInfIsNvF32_0 = Bool()
    val hasInfIsNvF16_3 = Bool()
    val hasInfIsNvF16_2 = Bool()
    val hasInfIsNvF16_1 = Bool()
    val hasInfIsNvF16_0 = Bool()
    val isOverflowF64 = Bool()
    val isOverflowF32_1 = Bool()
    val isOverflowF32_0 = Bool()
    val isOverflowF16_3 = Bool()
    val isOverflowF16_2 = Bool()
    val isOverflowF16_1 = Bool()
    val isOverflowF16_0 = Bool()
    val isOverflowDownF64 = Bool()
    val isOverflowDownF32_1 = Bool()
    val isOverflowDownF32_0 = Bool()
    val isOverflowDownF16_3 = Bool()
    val isOverflowDownF16_2 = Bool()
    val isOverflowDownF16_1 = Bool()
    val isOverflowDownF16_0 = Bool()
    val hasZeroF64 = Bool()
    val hasZeroF32_1 = Bool()
    val hasZeroF32_0 = Bool()
    val hasZeroF16_3 = Bool()
    val hasZeroF16_2 = Bool()
    val hasZeroF16_1 = Bool()
    val hasZeroF16_0 = Bool()
    val normalResultIsZeroF64 = Bool()
    val normalResultIsZeroF32_1 = Bool()
    val normalResultIsZeroF32_0 = Bool()
    val normalResultIsZeroF16_3 = Bool()
    val normalResultIsZeroF16_2 = Bool()
    val normalResultIsZeroF16_1 = Bool()
    val normalResultIsZeroF16_0 = Bool()
    val fpAOrBIsZeroF64 = Bool()
    val fpAOrBIsZeroF32_1 = Bool()
    val fpAOrBIsZeroF32_0 = Bool()
    val fpAOrBIsZeroF16_3 = Bool()
    val fpAOrBIsZeroF16_2 = Bool()
    val fpAOrBIsZeroF16_1 = Bool()
    val fpAOrBIsZeroF16_0 = Bool()
  }

  class S2ToS3Data extends VectorFMA.Bundle {
    val normalResultF64 = UInt(64.W)
    val normalResultF32_1 = UInt(32.W)
    val normalResultF32_0 = UInt(32.W)
    val normalResultF16_3 = UInt(16.W)
    val normalResultF16_2 = UInt(16.W)
    val normalResultF16_1 = UInt(16.W)
    val normalResultF16_0 = UInt(16.W)
    val overflowUpResultF64 = UInt(64.W)
    val overflowUpResultF32_1 = UInt(32.W)
    val overflowUpResultF32_0 = UInt(32.W)
    val overflowUpResultF16_3 = UInt(16.W)
    val overflowUpResultF16_2 = UInt(16.W)
    val overflowUpResultF16_1 = UInt(16.W)
    val overflowUpResultF16_0 = UInt(16.W)
    val overflowDownResultF64 = UInt(64.W)
    val overflowDownResultF32_1 = UInt(32.W)
    val overflowDownResultF32_0 = UInt(32.W)
    val overflowDownResultF16_3 = UInt(16.W)
    val overflowDownResultF16_2 = UInt(16.W)
    val overflowDownResultF16_1 = UInt(16.W)
    val overflowDownResultF16_0 = UInt(16.W)
    val zeroFromOperandResultF64 = UInt(64.W)
    val zeroFromOperandResultF32_1 = UInt(32.W)
    val zeroFromOperandResultF32_0 = UInt(32.W)
    val zeroFromOperandResultF16_3 = UInt(16.W)
    val zeroFromOperandResultF16_2 = UInt(16.W)
    val zeroFromOperandResultF16_1 = UInt(16.W)
    val zeroFromOperandResultF16_0 = UInt(16.W)
    val zeroNormalResultF64 = UInt(64.W)
    val zeroNormalResultF32_1 = UInt(32.W)
    val zeroNormalResultF32_0 = UInt(32.W)
    val zeroNormalResultF16_3 = UInt(16.W)
    val zeroNormalResultF16_2 = UInt(16.W)
    val zeroNormalResultF16_1 = UInt(16.W)
    val zeroNormalResultF16_0 = UInt(16.W)
    val infResultF64 = UInt(64.W)
    val infResultF32_1 = UInt(32.W)
    val infResultF32_0 = UInt(32.W)
    val infResultF16_3 = UInt(16.W)
    val infResultF16_2 = UInt(16.W)
    val infResultF16_1 = UInt(16.W)
    val infResultF16_0 = UInt(16.W)
    val nanResultF64 = UInt(64.W)
    val nanResultF32_1 = UInt(32.W)
    val nanResultF32_0 = UInt(32.W)
    val nanResultF16_3 = UInt(16.W)
    val nanResultF16_2 = UInt(16.W)
    val nanResultF16_1 = UInt(16.W)
    val nanResultF16_0 = UInt(16.W)
  }

  class S2ToS3 extends VectorFMA.Bundle {
    val ctrl = new S2ToS3Ctrl
    val data = new S2ToS3Data
  }
}
