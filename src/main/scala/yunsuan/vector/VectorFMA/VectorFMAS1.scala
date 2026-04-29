package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._

class VectorFMAS1 extends VectorFMA.Module {
  val in = IO(Input(new VectorFMAS0.S0ToS1))
  val out = IO(Output(new VectorFMAS1.S1ToS2))

  private val inCtrl = in.ctrl
  private val inData = in.data

  val isFp64 = inCtrl.resultIsFp64
  val isFp32 = inCtrl.resultIsFp32
  val isFp16 = !isFp32 && !isFp64

  val isSubF64 = inCtrl.isSubF64
  val isSubF32_0 = inCtrl.isSubF32_0
  val isSubF32_1 = inCtrl.isSubF32_1
  val isSubF16_0 = inCtrl.isSubF16_0
  val isSubF16_1 = inCtrl.isSubF16_1
  val isSubF16_2 = inCtrl.isSubF16_2
  val isSubF16_3 = inCtrl.isSubF16_3

  val ecFixF64 = inCtrl.ecFixF64
  val ecFixF32_0 = inCtrl.ecFixF32_0
  val ecFixF32_1 = inCtrl.ecFixF32_1
  val ecFixF16_0 = inCtrl.ecFixF16_0
  val ecFixF16_1 = inCtrl.ecFixF16_1
  val ecFixF16_2 = inCtrl.ecFixF16_2
  val ecFixF16_3 = inCtrl.ecFixF16_3

  val eabF64 = inCtrl.eabF64
  val eabF32_0 = inCtrl.eabF32_0
  val eabF32_1 = inCtrl.eabF32_1
  val eabF16_0 = inCtrl.eabF16_0
  val eabF16_1 = inCtrl.eabF16_1
  val eabF16_2 = inCtrl.eabF16_2
  val eabF16_3 = inCtrl.eabF16_3

  val eabIsGreaterF64 = inCtrl.eabIsGreaterF64
  val eabIsGreaterF32_0 = inCtrl.eabIsGreaterF32_0
  val eabIsGreaterF32_1 = inCtrl.eabIsGreaterF32_1
  val eabIsGreaterF16_0 = inCtrl.eabIsGreaterF16_0
  val eabIsGreaterF16_1 = inCtrl.eabIsGreaterF16_1
  val eabIsGreaterF16_2 = inCtrl.eabIsGreaterF16_2
  val eabIsGreaterF16_3 = inCtrl.eabIsGreaterF16_3

  val rshiftGuardF64 = inCtrl.rshiftGuard(0)
  val rshiftGuardF32_0 = inCtrl.rshiftGuard(0)
  val rshiftGuardF32_1 = inCtrl.rshiftGuard(1)
  val rshiftGuardF16_0 = inCtrl.rshiftGuard(0)
  val rshiftGuardF16_1 = inCtrl.rshiftGuard(1)
  val rshiftGuardF16_2 = inCtrl.rshiftGuard(2)
  val rshiftGuardF16_3 = inCtrl.rshiftGuard(3)
  val rshiftRoundF64 = inCtrl.rshiftRound(0)
  val rshiftRoundF32_0 = inCtrl.rshiftRound(0)
  val rshiftRoundF32_1 = inCtrl.rshiftRound(1)
  val rshiftRoundF16_0 = inCtrl.rshiftRound(0)
  val rshiftRoundF16_1 = inCtrl.rshiftRound(1)
  val rshiftRoundF16_2 = inCtrl.rshiftRound(2)
  val rshiftRoundF16_3 = inCtrl.rshiftRound(3)
  val rshiftStickyF64 = inCtrl.rshiftSticky(0)
  val rshiftStickyF32_0 = inCtrl.rshiftSticky(0)
  val rshiftStickyF32_1 = inCtrl.rshiftSticky(1)
  val rshiftStickyF16_0 = inCtrl.rshiftSticky(0)
  val rshiftStickyF16_1 = inCtrl.rshiftSticky(1)
  val rshiftStickyF16_2 = inCtrl.rshiftSticky(2)
  val rshiftStickyF16_3 = inCtrl.rshiftSticky(3)

  val fpCRshiftValueInvF64 = inData.fpCRshiftValueInv
  val fpCRshiftValueInvF32_0 = inData.fpCRshiftValueInv(74, 0)
  val fpCRshiftValueInvF32_1 = inData.fpCRshiftValueInv(149, 75)
  val fpCRshiftValueInvF16_0 = inData.fpCRshiftValueInv(35, 0)
  val fpCRshiftValueInvF16_1 = inData.fpCRshiftValueInv(71, 36)
  val fpCRshiftValueInvF16_2 = inData.fpCRshiftValueInv(107, 72)
  val fpCRshiftValueInvF16_3 = inData.fpCRshiftValueInv(143, 108)

  val csa4to2 = Module(new VectorFMACSA4to2(width = csaWidth))
  csa4to2.io.in_a := inData.csaPreFinal(0)
  csa4to2.io.in_b := inData.csaPreFinal(1)
  csa4to2.io.in_c := inData.csaPreFinal(2)
  csa4to2.io.in_d := inData.csaPreFinal(3)

  val mulRaw = csa4to2.io.out_sum + csa4to2.io.out_car
  val mulExpBasePlus2F64 = ((inCtrl.eabF64.asSInt - (significandWidth + 1).S).asUInt)(inCtrl.eabF64.getWidth - 1, 0)
  val mulExpBasePlus2F32_0 = ((inCtrl.eabF32_0.asSInt - (24 + 1).S).asUInt)(inCtrl.eabF32_0.getWidth - 1, 0)
  val mulExpBasePlus2F32_1 = ((inCtrl.eabF32_1.asSInt - (24 + 1).S).asUInt)(inCtrl.eabF32_1.getWidth - 1, 0)
  val mulExpBasePlus2F16_0 = ((inCtrl.eabF16_0.asSInt - (11 + 1).S).asUInt)(inCtrl.eabF16_0.getWidth - 1, 0)
  val mulExpBasePlus2F16_1 = ((inCtrl.eabF16_1.asSInt - (11 + 1).S).asUInt)(inCtrl.eabF16_1.getWidth - 1, 0)
  val mulExpBasePlus2F16_2 = ((inCtrl.eabF16_2.asSInt - (11 + 1).S).asUInt)(inCtrl.eabF16_2.getWidth - 1, 0)
  val mulExpBasePlus2F16_3 = ((inCtrl.eabF16_3.asSInt - (11 + 1).S).asUInt)(inCtrl.eabF16_3.getWidth - 1, 0)
  val mulRawF64 = mulRaw
  val mulRawF32_0 = mulRaw(48, 0)
  val mulRawF32_1 = mulRaw(106, 58)
  val mulRawF16_0 = mulRaw(22, 0)
  val mulRawF16_1 = mulRaw(48, 26)
  val mulRawF16_2 = mulRaw(80, 58)
  val mulRawF16_3 = mulRaw(106, 84)

  val csa3to2InA = csa4to2.io.out_sum
  val csa3to2InB = Mux(
    isFp64,
    Cat(csa4to2.io.out_car.head(106), isSubF64 & !rshiftGuardF64 & !rshiftRoundF64 & !rshiftStickyF64),
    Mux(
      isFp32,
      Cat(
        csa4to2.io.out_car.head(48),
        isSubF32_1 & !rshiftGuardF32_1 & !rshiftRoundF32_1 & !rshiftStickyF32_1,
        csa4to2.io.out_car(57, 49),
        csa4to2.io.out_car(48, 1),
        isSubF32_0 & !rshiftGuardF32_0 & !rshiftRoundF32_0 & !rshiftStickyF32_0
      ),
      Cat(
        Cat(
          csa4to2.io.out_car.head(22),
          isSubF16_3 & !rshiftGuardF16_3 & !rshiftRoundF16_3 & !rshiftStickyF16_3,
          csa4to2.io.out_car(25 + 58, 23 + 58),
          csa4to2.io.out_car(22 + 58, 1 + 58),
          isSubF16_2 & !rshiftGuardF16_2 & !rshiftRoundF16_2 & !rshiftStickyF16_2
        ),
        csa4to2.io.out_car(57, 49),
        Cat(
          csa4to2.io.out_car(48, 27),
          isSubF16_1 & !rshiftGuardF16_1 & !rshiftRoundF16_1 & !rshiftStickyF16_1,
          csa4to2.io.out_car(25, 23),
          csa4to2.io.out_car(22, 1),
          isSubF16_0 & !rshiftGuardF16_0 & !rshiftRoundF16_0 & !rshiftStickyF16_0
        )
      )
    )
  )
  val csa3to2InC = Mux(
    isFp64,
    Cat(0.U, fpCRshiftValueInvF64(2 * significandWidth - 1, 0)),
    Mux(
      isFp32,
      Cat(0.U, fpCRshiftValueInvF32_1(2 * 24 - 1, 0), 0.U(10.W), fpCRshiftValueInvF32_0(2 * 24 - 1, 0)),
      Cat(
        Cat(0.U, fpCRshiftValueInvF16_3(2 * 11 - 1, 0), 0.U(4.W), fpCRshiftValueInvF16_2(2 * 11 - 1, 0)),
        0.U(10.W),
        Cat(fpCRshiftValueInvF16_1(2 * 11 - 1, 0), 0.U(4.W), fpCRshiftValueInvF16_0(2 * 11 - 1, 0))
      )
    )
  )

  val csa3to2 = Module(new VectorFMACSA3to2(width = csaWidth))
  csa3to2.io.in_a := csa3to2InA
  csa3to2.io.in_b := csa3to2InB
  csa3to2.io.in_c := csa3to2InC

  val adderLowbitF64 = csa3to2.io.out_sum + csa3to2.io.out_car
  val adderLowbitF32_0 = adderLowbitF64(48, 0)
  val adderLowbitF32_1 = adderLowbitF64(106, 58)
  val adderLowbitF16_0 = adderLowbitF32_0(22, 0)
  val adderLowbitF16_1 = adderLowbitF32_0(48, 26)
  val adderLowbitF16_2 = adderLowbitF32_1(22, 0)
  val adderLowbitF16_3 = adderLowbitF32_1(48, 26)

  val fpCRshiftResultHighInvAdd0F64 = fpCRshiftValueInvF64.head(significandWidth + 3)
  val fpCRshiftResultHighInvAdd0F32_0 = fpCRshiftValueInvF32_0.head(24 + 3)
  val fpCRshiftResultHighInvAdd0F32_1 = fpCRshiftValueInvF32_1.head(24 + 3)
  val fpCRshiftResultHighInvAdd0F16_0 = fpCRshiftValueInvF16_0.head(11 + 3)
  val fpCRshiftResultHighInvAdd0F16_1 = fpCRshiftValueInvF16_1.head(11 + 3)
  val fpCRshiftResultHighInvAdd0F16_2 = fpCRshiftValueInvF16_2.head(11 + 3)
  val fpCRshiftResultHighInvAdd0F16_3 = fpCRshiftValueInvF16_3.head(11 + 3)
  val fpCRshiftResultHighInvAdd1 = Mux(
    isFp64,
    Cat(0.U(3.W), fpCRshiftValueInvF64.head(significandWidth + 3)),
    Mux(
      isFp32,
      Cat(fpCRshiftValueInvF32_1.head(24 + 3), 0.U(5.W), fpCRshiftValueInvF32_0.head(24 + 3)),
      Cat(
        fpCRshiftValueInvF16_3.head(11 + 3), 0.U, fpCRshiftValueInvF16_2.head(11 + 3),
        0.U, fpCRshiftValueInvF16_1.head(11 + 3), 0.U, fpCRshiftValueInvF16_0.head(11 + 3)
      )
    )
  ) + Cat(!isFp32 & !isFp64, 0.U(12.W), isFp32, 0.U, !isFp32 & !isFp64, 0.U(14.W), !isFp32 & !isFp64, 0.U(14.W), 1.U)
  val fpCRshiftResultHighInvAdd1F64 = fpCRshiftResultHighInvAdd1(55, 0)
  val fpCRshiftResultHighInvAdd1F32_0 = fpCRshiftResultHighInvAdd1(26, 0)
  val fpCRshiftResultHighInvAdd1F32_1 = fpCRshiftResultHighInvAdd1(58, 32)
  val fpCRshiftResultHighInvAdd1F16_0 = fpCRshiftResultHighInvAdd1(13, 0)
  val fpCRshiftResultHighInvAdd1F16_1 = fpCRshiftResultHighInvAdd1(28, 15)
  val fpCRshiftResultHighInvAdd1F16_2 = fpCRshiftResultHighInvAdd1(43, 30)
  val fpCRshiftResultHighInvAdd1F16_3 = fpCRshiftResultHighInvAdd1(58, 45)

  val adderF64 = Cat(
    Mux(adderLowbitF64.head(1).asBool, fpCRshiftResultHighInvAdd1F64, fpCRshiftResultHighInvAdd0F64),
    adderLowbitF64.tail(1),
    Mux(isSubF64, ((~Cat(rshiftGuardF64, rshiftRoundF64, rshiftStickyF64)).asUInt + 1.U).head(2), Cat(rshiftGuardF64, rshiftRoundF64))
  )
  val adderF32_0 = Cat(
    Mux(adderLowbitF32_0.head(1).asBool, fpCRshiftResultHighInvAdd1F32_0, fpCRshiftResultHighInvAdd0F32_0),
    adderLowbitF32_0.tail(1),
    Mux(isSubF32_0, ((~Cat(rshiftGuardF32_0, rshiftRoundF32_0, rshiftStickyF32_0)).asUInt + 1.U).head(2), Cat(rshiftGuardF32_0, rshiftRoundF32_0))
  )
  val adderF32_1 = Cat(
    Mux(adderLowbitF32_1.head(1).asBool, fpCRshiftResultHighInvAdd1F32_1, fpCRshiftResultHighInvAdd0F32_1),
    adderLowbitF32_1.tail(1),
    Mux(isSubF32_1, ((~Cat(rshiftGuardF32_1, rshiftRoundF32_1, rshiftStickyF32_1)).asUInt + 1.U).head(2), Cat(rshiftGuardF32_1, rshiftRoundF32_1))
  )
  val adderF16_0 = Cat(
    Mux(adderLowbitF16_0.head(1).asBool, fpCRshiftResultHighInvAdd1F16_0, fpCRshiftResultHighInvAdd0F16_0),
    adderLowbitF16_0.tail(1),
    Mux(isSubF16_0, ((~Cat(rshiftGuardF16_0, rshiftRoundF16_0, rshiftStickyF16_0)).asUInt + 1.U).head(2), Cat(rshiftGuardF16_0, rshiftRoundF16_0))
  )
  val adderF16_1 = Cat(
    Mux(adderLowbitF16_1.head(1).asBool, fpCRshiftResultHighInvAdd1F16_1, fpCRshiftResultHighInvAdd0F16_1),
    adderLowbitF16_1.tail(1),
    Mux(isSubF16_1, ((~Cat(rshiftGuardF16_1, rshiftRoundF16_1, rshiftStickyF16_1)).asUInt + 1.U).head(2), Cat(rshiftGuardF16_1, rshiftRoundF16_1))
  )
  val adderF16_2 = Cat(
    Mux(adderLowbitF16_2.head(1).asBool, fpCRshiftResultHighInvAdd1F16_2, fpCRshiftResultHighInvAdd0F16_2),
    adderLowbitF16_2.tail(1),
    Mux(isSubF16_2, ((~Cat(rshiftGuardF16_2, rshiftRoundF16_2, rshiftStickyF16_2)).asUInt + 1.U).head(2), Cat(rshiftGuardF16_2, rshiftRoundF16_2))
  )
  val adderF16_3 = Cat(
    Mux(adderLowbitF16_3.head(1).asBool, fpCRshiftResultHighInvAdd1F16_3, fpCRshiftResultHighInvAdd0F16_3),
    adderLowbitF16_3.tail(1),
    Mux(isSubF16_3, ((~Cat(rshiftGuardF16_3, rshiftRoundF16_3, rshiftStickyF16_3)).asUInt + 1.U).head(2), Cat(rshiftGuardF16_3, rshiftRoundF16_3))
  )

  val adderIsNegativeF64 = adderF64.head(1).asBool
  val adderIsNegativeF32_0 = adderF32_0.head(1).asBool
  val adderIsNegativeF32_1 = adderF32_1.head(1).asBool
  val adderIsNegativeF16_0 = adderF16_0.head(1).asBool
  val adderIsNegativeF16_1 = adderF16_1.head(1).asBool
  val adderIsNegativeF16_2 = adderF16_2.head(1).asBool
  val adderIsNegativeF16_3 = adderF16_3.head(1).asBool

  val adderInvF64 = Mux(adderIsNegativeF64, (~adderF64.tail(1)).asUInt, adderF64.tail(1))
  val adderInvF32_0 = Mux(adderIsNegativeF32_0, (~adderF32_0.tail(1)).asUInt, adderF32_0.tail(1))
  val adderInvF32_1 = Mux(adderIsNegativeF32_1, (~adderF32_1.tail(1)).asUInt, adderF32_1.tail(1))
  val adderInvF16_0 = Mux(adderIsNegativeF16_0, (~adderF16_0.tail(1)).asUInt, adderF16_0.tail(1))
  val adderInvF16_1 = Mux(adderIsNegativeF16_1, (~adderF16_1.tail(1)).asUInt, adderF16_1.tail(1))
  val adderInvF16_2 = Mux(adderIsNegativeF16_2, (~adderF16_2.tail(1)).asUInt, adderF16_2.tail(1))
  val adderInvF16_3 = Mux(adderIsNegativeF16_3, (~adderF16_3.tail(1)).asUInt, adderF16_3.tail(1))

  val eGreaterF64 = Mux(eabIsGreaterF64, eabF64(exponentWidth, 0).asUInt, Cat(0.U(1.W), ecFixF64))
  val eGreaterF32_0 = Mux(eabIsGreaterF32_0, eabF32_0(8, 0).asUInt, Cat(0.U(1.W), ecFixF32_0))
  val eGreaterF32_1 = Mux(eabIsGreaterF32_1, eabF32_1(8, 0).asUInt, Cat(0.U(1.W), ecFixF32_1))
  val eGreaterF16_0 = Mux(eabIsGreaterF16_0, eabF16_0(5, 0).asUInt, Cat(0.U(1.W), ecFixF16_0))
  val eGreaterF16_1 = Mux(eabIsGreaterF16_1, eabF16_1(5, 0).asUInt, Cat(0.U(1.W), ecFixF16_1))
  val eGreaterF16_2 = Mux(eabIsGreaterF16_2, eabF16_2(5, 0).asUInt, Cat(0.U(1.W), ecFixF16_2))
  val eGreaterF16_3 = Mux(eabIsGreaterF16_3, eabF16_3(5, 0).asUInt, Cat(0.U(1.W), ecFixF16_3))

  val lshiftValueMaxF64 = Mux(eabIsGreaterF64, eabF64(exponentWidth, 0).asUInt - 1.U, Cat(0.U, ecFixF64 - 1.U))
  val lshiftValueMaxF32_0 = Mux(eabIsGreaterF32_0, eabF32_0(8, 0).asUInt - 1.U, Cat(0.U, ecFixF32_0 - 1.U))
  val lshiftValueMaxF32_1 = Mux(eabIsGreaterF32_1, eabF32_1(8, 0).asUInt - 1.U, Cat(0.U, ecFixF32_1 - 1.U))
  val lshiftValueMaxF16_0 = Mux(eabIsGreaterF16_0, eabF16_0(5, 0).asUInt - 1.U, Cat(0.U, ecFixF16_0 - 1.U))
  val lshiftValueMaxF16_1 = Mux(eabIsGreaterF16_1, eabF16_1(5, 0).asUInt - 1.U, Cat(0.U, ecFixF16_1 - 1.U))
  val lshiftValueMaxF16_2 = Mux(eabIsGreaterF16_2, eabF16_2(5, 0).asUInt - 1.U, Cat(0.U, ecFixF16_2 - 1.U))
  val lshiftValueMaxF16_3 = Mux(eabIsGreaterF16_3, eabF16_3(5, 0).asUInt - 1.U, Cat(0.U, ecFixF16_3 - 1.U))

  val tzdAdderInputF64 = Reverse(adderF64.asUInt)
  val tzdAdderInputF32_0 = Reverse(adderF32_0.asUInt)
  val tzdAdderInputF32_1 = Reverse(adderF32_1.asUInt)
  val tzdAdderInputF16_0 = Reverse(adderF16_0.asUInt)
  val tzdAdderInputF16_1 = Reverse(adderF16_1.asUInt)
  val tzdAdderInputF16_2 = Reverse(adderF16_2.asUInt)
  val tzdAdderInputF16_3 = Reverse(adderF16_3.asUInt)

  val lzdWidthF64 = adderInvF64.getWidth.U.getWidth
  val lzdWidthF32_0 = adderInvF32_0.getWidth.U.getWidth
  val lzdWidthF32_1 = adderInvF32_1.getWidth.U.getWidth
  val lzdWidthF16_0 = adderInvF16_0.getWidth.U.getWidth
  val lzdWidthF16_1 = adderInvF16_1.getWidth.U.getWidth
  val lzdWidthF16_2 = adderInvF16_2.getWidth.U.getWidth
  val lzdWidthF16_3 = adderInvF16_3.getWidth.U.getWidth
  val lshiftValueMaskF64 = Mux(
    lshiftValueMaxF64.head(lshiftValueMaxF64.getWidth - lzdWidthF64).orR,
    0.U(adderInvF64.getWidth.W),
    Fill(adderInvF64.getWidth, 1.U) >> lshiftValueMaxF64.tail(lshiftValueMaxF64.getWidth - lzdWidthF64)
  ).asUInt
  val lshiftValueMaskF32_0 = Mux(
    lshiftValueMaxF32_0.head(lshiftValueMaxF32_0.getWidth - lzdWidthF32_0).orR,
    0.U(adderInvF32_0.getWidth.W),
    Fill(adderInvF32_0.getWidth, 1.U) >> lshiftValueMaxF32_0.tail(lshiftValueMaxF32_0.getWidth - lzdWidthF32_0)
  ).asUInt
  val lshiftValueMaskF32_1 = Mux(
    lshiftValueMaxF32_1.head(lshiftValueMaxF32_1.getWidth - lzdWidthF32_1).orR,
    0.U(adderInvF32_1.getWidth.W),
    Fill(adderInvF32_1.getWidth, 1.U) >> lshiftValueMaxF32_1.tail(lshiftValueMaxF32_1.getWidth - lzdWidthF32_1)
  ).asUInt
  val lshiftValueMaskF16_0 = Mux(
    lshiftValueMaxF16_0.head(lshiftValueMaxF16_0.getWidth - lzdWidthF16_0).orR,
    0.U(adderInvF16_0.getWidth.W),
    Fill(adderInvF16_0.getWidth, 1.U) >> lshiftValueMaxF16_0.tail(lshiftValueMaxF16_0.getWidth - lzdWidthF16_0)
  ).asUInt
  val lshiftValueMaskF16_1 = Mux(
    lshiftValueMaxF16_1.head(lshiftValueMaxF16_1.getWidth - lzdWidthF16_1).orR,
    0.U(adderInvF16_1.getWidth.W),
    Fill(adderInvF16_1.getWidth, 1.U) >> lshiftValueMaxF16_1.tail(lshiftValueMaxF16_1.getWidth - lzdWidthF16_1)
  ).asUInt
  val lshiftValueMaskF16_2 = Mux(
    lshiftValueMaxF16_2.head(lshiftValueMaxF16_2.getWidth - lzdWidthF16_2).orR,
    0.U(adderInvF16_2.getWidth.W),
    Fill(adderInvF16_2.getWidth, 1.U) >> lshiftValueMaxF16_2.tail(lshiftValueMaxF16_2.getWidth - lzdWidthF16_2)
  ).asUInt
  val lshiftValueMaskF16_3 = Mux(
    lshiftValueMaxF16_3.head(lshiftValueMaxF16_3.getWidth - lzdWidthF16_3).orR,
    0.U(adderInvF16_3.getWidth.W),
    Fill(adderInvF16_3.getWidth, 1.U) >> lshiftValueMaxF16_3.tail(lshiftValueMaxF16_3.getWidth - lzdWidthF16_3)
  ).asUInt

  val lzdAdderInvMaskInputF64 = adderInvF64 | lshiftValueMaskF64
  val lzdAdderInvMaskInputF32_0 = adderInvF32_0 | lshiftValueMaskF32_0
  val lzdAdderInvMaskInputF32_1 = adderInvF32_1 | lshiftValueMaskF32_1
  val lzdAdderInvMaskInputF16_0 = adderInvF16_0 | lshiftValueMaskF16_0
  val lzdAdderInvMaskInputF16_1 = adderInvF16_1 | lshiftValueMaskF16_1
  val lzdAdderInvMaskInputF16_2 = adderInvF16_2 | lshiftValueMaskF16_2
  val lzdAdderInvMaskInputF16_3 = adderInvF16_3 | lshiftValueMaskF16_3

  val lshiftMaskValidF64 = lzdAdderInvMaskInputF64 === lshiftValueMaskF64
  val lshiftMaskValidF32_0 = lzdAdderInvMaskInputF32_0 === lshiftValueMaskF32_0
  val lshiftMaskValidF32_1 = lzdAdderInvMaskInputF32_1 === lshiftValueMaskF32_1
  val lshiftMaskValidF16_0 = lzdAdderInvMaskInputF16_0 === lshiftValueMaskF16_0
  val lshiftMaskValidF16_1 = lzdAdderInvMaskInputF16_1 === lshiftValueMaskF16_1
  val lshiftMaskValidF16_2 = lzdAdderInvMaskInputF16_2 === lshiftValueMaskF16_2
  val lshiftMaskValidF16_3 = lzdAdderInvMaskInputF16_3 === lshiftValueMaskF16_3

  out.ctrl.signABF64 := inCtrl.signABF64
  out.ctrl.signABF32_1 := inCtrl.signABF32_1
  out.ctrl.signABF32_0 := inCtrl.signABF32_0
  out.ctrl.signABF16_3 := inCtrl.signABF16_3
  out.ctrl.signABF16_2 := inCtrl.signABF16_2
  out.ctrl.signABF16_1 := inCtrl.signABF16_1
  out.ctrl.signABF16_0 := inCtrl.signABF16_0
  out.ctrl.signCF64 := inCtrl.signCF64
  out.ctrl.signCF32_1 := inCtrl.signCF32_1
  out.ctrl.signCF32_0 := inCtrl.signCF32_0
  out.ctrl.signCF16_3 := inCtrl.signCF16_3
  out.ctrl.signCF16_2 := inCtrl.signCF16_2
  out.ctrl.signCF16_1 := inCtrl.signCF16_1
  out.ctrl.signCF16_0 := inCtrl.signCF16_0
  out.ctrl.round_mode := inCtrl.round_mode
  out.ctrl.isVfmul := inCtrl.isVfmul
  out.ctrl.resultIsFp16 := inCtrl.resultIsFp16
  out.ctrl.resultIsFp32 := inCtrl.resultIsFp32
  out.ctrl.resultIsFp64 := inCtrl.resultIsFp64
  out.ctrl.res_widening := inCtrl.res_widening
  out.ctrl.fp_aIsFpCanonicalNAN := inCtrl.fp_aIsFpCanonicalNAN
  out.ctrl.fp_bIsFpCanonicalNAN := inCtrl.fp_bIsFpCanonicalNAN
  out.ctrl.fp_cIsFpCanonicalNAN := inCtrl.fp_cIsFpCanonicalNAN
  out.ctrl.rshiftSticky := inCtrl.rshiftSticky

  out.data.fpCF64 := inData.fpCF64
  out.data.fpCF32_1 := inData.fpCF32_1
  out.data.fpCF32_0 := inData.fpCF32_0
  out.data.fpCF16_3 := inData.fpCF16_3
  out.data.fpCF16_2 := inData.fpCF16_2
  out.data.fpCF16_1 := inData.fpCF16_1
  out.data.fpCF16_0 := inData.fpCF16_0
  out.data.eaF64 := inData.eaF64
  out.data.eaF32_1 := inData.eaF32_1
  out.data.eaF32_0 := inData.eaF32_0
  out.data.eaF16_3 := inData.eaF16_3
  out.data.eaF16_2 := inData.eaF16_2
  out.data.eaF16_1 := inData.eaF16_1
  out.data.eaF16_0 := inData.eaF16_0
  out.data.ebF64 := inData.ebF64
  out.data.ebF32_1 := inData.ebF32_1
  out.data.ebF32_0 := inData.ebF32_0
  out.data.ebF16_3 := inData.ebF16_3
  out.data.ebF16_2 := inData.ebF16_2
  out.data.ebF16_1 := inData.ebF16_1
  out.data.ebF16_0 := inData.ebF16_0
  out.data.ecF64 := inData.ecF64
  out.data.ecF32_1 := inData.ecF32_1
  out.data.ecF32_0 := inData.ecF32_0
  out.data.ecF16_3 := inData.ecF16_3
  out.data.ecF16_2 := inData.ecF16_2
  out.data.ecF16_1 := inData.ecF16_1
  out.data.ecF16_0 := inData.ecF16_0
  out.data.widenEaF16_0 := inData.widenEaF16_0
  out.data.widenEaF16_1 := inData.widenEaF16_1
  out.data.widenEbF16_0 := inData.widenEbF16_0
  out.data.widenEbF16_1 := inData.widenEbF16_1
  out.data.widenEaF32_0 := inData.widenEaF32_0
  out.data.widenEbF32_0 := inData.widenEbF32_0
  out.data.fpASignificandF64 := inData.fpASignificandF64
  out.data.fpASignificandF32_1 := inData.fpASignificandF32_1
  out.data.fpASignificandF32_0 := inData.fpASignificandF32_0
  out.data.fpASignificandF16_3 := inData.fpASignificandF16_3
  out.data.fpASignificandF16_2 := inData.fpASignificandF16_2
  out.data.fpASignificandF16_1 := inData.fpASignificandF16_1
  out.data.fpASignificandF16_0 := inData.fpASignificandF16_0
  out.data.fpBSignificandF64 := inData.fpBSignificandF64
  out.data.fpBSignificandF32_1 := inData.fpBSignificandF32_1
  out.data.fpBSignificandF32_0 := inData.fpBSignificandF32_0
  out.data.fpBSignificandF16_3 := inData.fpBSignificandF16_3
  out.data.fpBSignificandF16_2 := inData.fpBSignificandF16_2
  out.data.fpBSignificandF16_1 := inData.fpBSignificandF16_1
  out.data.fpBSignificandF16_0 := inData.fpBSignificandF16_0
  out.data.fpCSignificandF64 := inData.fpCSignificandF64
  out.data.fpCSignificandF32_1 := inData.fpCSignificandF32_1
  out.data.fpCSignificandF32_0 := inData.fpCSignificandF32_0
  out.data.fpCSignificandF16_3 := inData.fpCSignificandF16_3
  out.data.fpCSignificandF16_2 := inData.fpCSignificandF16_2
  out.data.fpCSignificandF16_1 := inData.fpCSignificandF16_1
  out.data.fpCSignificandF16_0 := inData.fpCSignificandF16_0
  out.data.adder := Mux(isFp64, adderF64, Mux(isFp32, Cat(adderF32_1, adderF32_0), Cat(adderF16_3, adderF16_2, adderF16_1, adderF16_0)))
  out.ctrl.adderIsNegative := Cat(
    adderIsNegativeF16_3,
    adderIsNegativeF16_2,
    Mux(isFp32, adderIsNegativeF32_1, adderIsNegativeF16_1),
    Mux(isFp64, adderIsNegativeF64, Mux(isFp32, adderIsNegativeF32_0, adderIsNegativeF16_0))
  )
  out.data.eGreater := Mux(
    isFp64,
    eGreaterF64,
    Mux(
      isFp32,
      Cat(eGreaterF32_1, eGreaterF32_0),
      Cat(eGreaterF16_3, eGreaterF16_2, eGreaterF16_1, eGreaterF16_0)
    )
  )
  out.data.tzdAdderInput := Mux(
    isFp64,
    tzdAdderInputF64,
    Mux(
      isFp32,
      Cat(tzdAdderInputF32_1, tzdAdderInputF32_0),
      Cat(tzdAdderInputF16_3, tzdAdderInputF16_2, tzdAdderInputF16_1, tzdAdderInputF16_0)
    )
  )
  out.data.lzdAdderInvMaskInput := Mux(
    isFp64,
    lzdAdderInvMaskInputF64,
    Mux(
      isFp32,
      Cat(lzdAdderInvMaskInputF32_1, lzdAdderInvMaskInputF32_0),
      Cat(lzdAdderInvMaskInputF16_3, lzdAdderInvMaskInputF16_2, lzdAdderInvMaskInputF16_1, lzdAdderInvMaskInputF16_0)
    )
  )
  out.data.lshiftMaskValid := Cat(
    lshiftMaskValidF16_3,
    lshiftMaskValidF16_2,
    Mux(isFp32, lshiftMaskValidF32_1, lshiftMaskValidF16_1),
    Mux(isFp64, lshiftMaskValidF64, Mux(isFp32, lshiftMaskValidF32_0, lshiftMaskValidF16_0))
  )
  out.data.mulRaw := mulRaw
  out.data.mulExpBasePlus2 := Mux(
    isFp64,
    mulExpBasePlus2F64,
    Mux(
      isFp32,
      Cat(mulExpBasePlus2F32_1, mulExpBasePlus2F32_0),
      Cat(
        mulExpBasePlus2F16_3,
        mulExpBasePlus2F16_2,
        mulExpBasePlus2F16_1,
        mulExpBasePlus2F16_0
      )
    )
  )
}

object VectorFMAS1 {
  class S1ToS2Ctrl extends VectorFMA.Bundle {
    val round_mode = UInt(3.W)
    val isVfmul = Bool()
    val resultIsFp16 = Bool()
    val resultIsFp32 = Bool()
    val resultIsFp64 = Bool()
    val res_widening = Bool()
    val fp_aIsFpCanonicalNAN = Bool()
    val fp_bIsFpCanonicalNAN = Bool()
    val fp_cIsFpCanonicalNAN = Bool()
    val signABF64 = Bool()
    val signABF32_1 = Bool()
    val signABF32_0 = Bool()
    val signABF16_3 = Bool()
    val signABF16_2 = Bool()
    val signABF16_1 = Bool()
    val signABF16_0 = Bool()
    val signCF64 = Bool()
    val signCF32_1 = Bool()
    val signCF32_0 = Bool()
    val signCF16_3 = Bool()
    val signCF16_2 = Bool()
    val signCF16_1 = Bool()
    val signCF16_0 = Bool()
    val rshiftSticky = UInt(laneFlagWidth.W)
    val adderIsNegative = UInt(laneFlagWidth.W)
  }

  class S1ToS2Data extends VectorFMA.Bundle {
    val fpCF64 = UInt(64.W)
    val fpCF32_1 = UInt(32.W)
    val fpCF32_0 = UInt(32.W)
    val fpCF16_3 = UInt(16.W)
    val fpCF16_2 = UInt(16.W)
    val fpCF16_1 = UInt(16.W)
    val fpCF16_0 = UInt(16.W)
    val eaF64 = UInt(exponentWidth.W)
    val eaF32_1 = UInt(8.W)
    val eaF32_0 = UInt(8.W)
    val eaF16_3 = UInt(5.W)
    val eaF16_2 = UInt(5.W)
    val eaF16_1 = UInt(5.W)
    val eaF16_0 = UInt(5.W)
    val ebF64 = UInt(exponentWidth.W)
    val ebF32_1 = UInt(8.W)
    val ebF32_0 = UInt(8.W)
    val ebF16_3 = UInt(5.W)
    val ebF16_2 = UInt(5.W)
    val ebF16_1 = UInt(5.W)
    val ebF16_0 = UInt(5.W)
    val ecF64 = UInt(exponentWidth.W)
    val ecF32_1 = UInt(8.W)
    val ecF32_0 = UInt(8.W)
    val ecF16_3 = UInt(5.W)
    val ecF16_2 = UInt(5.W)
    val ecF16_1 = UInt(5.W)
    val ecF16_0 = UInt(5.W)
    val widenEaF16_0 = UInt(5.W)
    val widenEaF16_1 = UInt(5.W)
    val widenEbF16_0 = UInt(5.W)
    val widenEbF16_1 = UInt(5.W)
    val widenEaF32_0 = UInt(8.W)
    val widenEbF32_0 = UInt(8.W)
    val fpASignificandF64 = UInt(significandWidth.W)
    val fpASignificandF32_1 = UInt(24.W)
    val fpASignificandF32_0 = UInt(24.W)
    val fpASignificandF16_3 = UInt(11.W)
    val fpASignificandF16_2 = UInt(11.W)
    val fpASignificandF16_1 = UInt(11.W)
    val fpASignificandF16_0 = UInt(11.W)
    val fpBSignificandF64 = UInt(significandWidth.W)
    val fpBSignificandF32_1 = UInt(24.W)
    val fpBSignificandF32_0 = UInt(24.W)
    val fpBSignificandF16_3 = UInt(11.W)
    val fpBSignificandF16_2 = UInt(11.W)
    val fpBSignificandF16_1 = UInt(11.W)
    val fpBSignificandF16_0 = UInt(11.W)
    val fpCSignificandF64 = UInt(significandWidth.W)
    val fpCSignificandF32_1 = UInt(24.W)
    val fpCSignificandF32_0 = UInt(24.W)
    val fpCSignificandF16_3 = UInt(11.W)
    val fpCSignificandF16_2 = UInt(11.W)
    val fpCSignificandF16_1 = UInt(11.W)
    val fpCSignificandF16_0 = UInt(11.W)
    val adder = UInt(adderWidth.W)
    val eGreater = UInt(eGreaterWidth.W)
    val tzdAdderInput = UInt(adderWidth.W)
    val lzdAdderInvMaskInput = UInt((adderWidth - 1).W)
    val lshiftMaskValid = UInt(laneFlagWidth.W)
    val mulRaw = UInt(csaWidth.W)
    val mulExpBasePlus2 = UInt(eGreaterWidth.W)
  }

  class S1ToS2 extends VectorFMA.Bundle {
    val ctrl = new S1ToS2Ctrl
    val data = new S1ToS2Data
  }
}
