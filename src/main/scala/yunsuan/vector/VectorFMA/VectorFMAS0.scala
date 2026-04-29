package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._

class VectorFMAS0 extends VectorFMA.Module {
  val in = IO(Input(new VectorFMA.InEx0))
  val out = IO(Output(new VectorFMAS0.S0ToS1))

  private val ctrl = in.ctrl
  private val data = in.data

  private def signInv(src: UInt, sel: Bool): UInt = {
    Cat(Mux(sel, ~src.head(1), src.head(1)), src.tail(1))
  }

  private def shiftRightWithMuxSticky(srcValue: UInt, shiftValue: UInt): UInt = {
    val vecLength = shiftValue.getWidth + 1
    val resVec = Wire(Vec(vecLength, UInt(srcValue.getWidth.W)))
    val stickyVec = Wire(Vec(vecLength, UInt(1.W)))
    resVec(0) := srcValue
    stickyVec(0) := 0.U
    for (i <- 0 until shiftValue.getWidth) {
      resVec(i + 1) := Mux(shiftValue(i), resVec(i) >> (1 << i), resVec(i))
      stickyVec(i + 1) := Mux(shiftValue(i), stickyVec(i) | resVec(i)((1 << i) - 1, 0).orR, stickyVec(i))
    }
    Cat(resVec(vecLength - 1), stickyVec(vecLength - 1))
  }

  val isVfmul = ctrl.isVfmul
  val isVfmacc = ctrl.isVfmacc
  val isVfnmacc = ctrl.isVfnmacc
  val isVfmsac = ctrl.isVfmsac
  val isVfnmsac = ctrl.isVfnmsac
  val isVfmadd = ctrl.isVfmadd
  val isVfnmadd = ctrl.isVfnmadd
  val isVfmsub = ctrl.isVfmsub
  val isVfnmsub = ctrl.isVfnmsub

  val resultIsFp16 = !ctrl.res_widening && ctrl.sel16
  val resultIsFp32 = Mux(ctrl.res_widening, ctrl.sel16, ctrl.sel32)
  val resultIsFp64 = Mux(ctrl.res_widening, ctrl.sel32, ctrl.sel64)
  val isFp64 = resultIsFp64
  val isFp32 = resultIsFp32
  val fpAIsFpCanonicalNAN = data.fp_aIsFpCanonicalNAN
  val fpBIsFpCanonicalNAN = data.fp_bIsFpCanonicalNAN
  val fpCIsFpCanonicalNAN = data.fp_cIsFpCanonicalNAN

  val fpAIsSignInv = isVfnmacc || isVfnmsac || isVfnmadd || isVfnmsub
  val fpCIsSignInv = isVfnmacc || isVfmsac || isVfnmadd || isVfmsub
  val swapFpAFpC = isVfmadd || isVfnmadd || isVfmsub || isVfnmsub

  val fpAF64 = signInv(Mux(swapFpAFpC, data.fp_c(63, 0), data.fp_a(63, 0)), fpAIsSignInv)
  val fpBF64 = data.fp_b(63, 0)
  val fpCF64 = Mux(isVfmul, 0.U(64.W), signInv(Mux(swapFpAFpC, data.fp_a(63, 0), data.fp_c(63, 0)), fpCIsSignInv))
  val fpAF32_0 = signInv(Mux(swapFpAFpC, data.fp_c(31, 0), data.fp_a(31, 0)), fpAIsSignInv)
  val fpAF32_1 = signInv(Mux(swapFpAFpC, data.fp_c(63, 32), data.fp_a(63, 32)), fpAIsSignInv)
  val fpBF32_0 = data.fp_b(31, 0)
  val fpBF32_1 = data.fp_b(63, 32)
  val fpCF32_0 = Mux(isVfmul, 0.U(32.W), signInv(Mux(swapFpAFpC, data.fp_a(31, 0), data.fp_c(31, 0)), fpCIsSignInv))
  val fpCF32_1 = Mux(isVfmul, 0.U(32.W), signInv(Mux(swapFpAFpC, data.fp_a(63, 32), data.fp_c(63, 32)), fpCIsSignInv))
  val fpAF16_0 = signInv(Mux(swapFpAFpC, data.fp_c(15, 0), data.fp_a(15, 0)), fpAIsSignInv)
  val fpAF16_1 = signInv(Mux(swapFpAFpC, data.fp_c(31, 16), data.fp_a(31, 16)), fpAIsSignInv)
  val fpAF16_2 = signInv(Mux(swapFpAFpC, data.fp_c(47, 32), data.fp_a(47, 32)), fpAIsSignInv)
  val fpAF16_3 = signInv(Mux(swapFpAFpC, data.fp_c(63, 48), data.fp_a(63, 48)), fpAIsSignInv)
  val fpBF16_0 = data.fp_b(15, 0)
  val fpBF16_1 = data.fp_b(31, 16)
  val fpBF16_2 = data.fp_b(47, 32)
  val fpBF16_3 = data.fp_b(63, 48)
  val fpCF16_0 = Mux(isVfmul, 0.U(16.W), signInv(Mux(swapFpAFpC, data.fp_a(15, 0), data.fp_c(15, 0)), fpCIsSignInv))
  val fpCF16_1 = Mux(isVfmul, 0.U(16.W), signInv(Mux(swapFpAFpC, data.fp_a(31, 16), data.fp_c(31, 16)), fpCIsSignInv))
  val fpCF16_2 = Mux(isVfmul, 0.U(16.W), signInv(Mux(swapFpAFpC, data.fp_a(47, 32), data.fp_c(47, 32)), fpCIsSignInv))
  val fpCF16_3 = Mux(isVfmul, 0.U(16.W), signInv(Mux(swapFpAFpC, data.fp_a(63, 48), data.fp_c(63, 48)), fpCIsSignInv))

  val signABF16_0 = (fpAF16_0.head(1) ^ fpBF16_0.head(1)).asBool
  val signABF16_1 = (fpAF16_1.head(1) ^ fpBF16_1.head(1)).asBool
  val signABF16_2 = (fpAF16_2.head(1) ^ fpBF16_2.head(1)).asBool
  val signABF16_3 = (fpAF16_3.head(1) ^ fpBF16_3.head(1)).asBool
  val widenAIsSignInv = isVfnmacc || isVfnmsac
  val widenAF16_0 = signInv(data.widen_a(15, 0), widenAIsSignInv)
  val widenBF16_0 = data.widen_b(15, 0)
  val widenAF16_1 = signInv(data.widen_a(31, 16), widenAIsSignInv)
  val widenBF16_1 = data.widen_b(31, 16)
  val widenAF32_0 = signInv(data.widen_a(31, 0), widenAIsSignInv)
  val widenBF32_0 = data.widen_b(31, 0)
  val widenSignABF16_0 = (widenAF16_0.head(1) ^ widenBF16_0.head(1)).asBool
  val widenSignABF16_1 = (widenAF16_1.head(1) ^ widenBF16_1.head(1)).asBool
  val widenSignABF32_0 = (widenAF32_0.head(1) ^ widenBF32_0.head(1)).asBool
  val signABF32_0 = Mux(ctrl.res_widening & isFp32, widenSignABF16_0, (fpAF32_0.head(1) ^ fpBF32_0.head(1)).asBool)
  val signABF32_1 = Mux(ctrl.res_widening & isFp32, widenSignABF16_1, (fpAF32_1.head(1) ^ fpBF32_1.head(1)).asBool)
  val signABF64 = Mux(ctrl.res_widening & isFp64, widenSignABF32_0, (fpAF64(63) ^ fpBF64(63)).asBool)
  val signCF64 = fpCF64(63).asBool
  val signCF32_0 = fpCF32_0.head(1).asBool
  val signCF32_1 = fpCF32_1.head(1).asBool
  val signCF16_0 = fpCF16_0.head(1).asBool
  val signCF16_1 = fpCF16_1.head(1).asBool
  val signCF16_2 = fpCF16_2.head(1).asBool
  val signCF16_3 = fpCF16_3.head(1).asBool
  val isSubF64 = signABF64 ^ signCF64
  val isSubF32_0 = signABF32_0 ^ signCF32_0
  val isSubF32_1 = signABF32_1 ^ signCF32_1
  val isSubF16_0 = signABF16_0 ^ signCF16_0
  val isSubF16_1 = signABF16_1 ^ signCF16_1
  val isSubF16_2 = signABF16_2 ^ signCF16_2
  val isSubF16_3 = signABF16_3 ^ signCF16_3

  val eaF64 = fpAF64.tail(1).head(exponentWidth)
  val ebF64 = fpBF64.tail(1).head(exponentWidth)
  val ecF64 = fpCF64.tail(1).head(exponentWidth)
  val eaF32_0 = fpAF32_0(30, 23)
  val ebF32_0 = fpBF32_0(30, 23)
  val ecF32_0 = fpCF32_0(30, 23)
  val eaF32_1 = fpAF32_1(30, 23)
  val ebF32_1 = fpBF32_1(30, 23)
  val ecF32_1 = fpCF32_1(30, 23)
  val eaF16_0 = fpAF16_0(14, 10)
  val ebF16_0 = fpBF16_0(14, 10)
  val ecF16_0 = fpCF16_0(14, 10)
  val eaF16_1 = fpAF16_1(14, 10)
  val ebF16_1 = fpBF16_1(14, 10)
  val ecF16_1 = fpCF16_1(14, 10)
  val eaF16_2 = fpAF16_2(14, 10)
  val ebF16_2 = fpBF16_2(14, 10)
  val ecF16_2 = fpCF16_2(14, 10)
  val eaF16_3 = fpAF16_3(14, 10)
  val ebF16_3 = fpBF16_3(14, 10)
  val ecF16_3 = fpCF16_3(14, 10)
  val widenEaF16_0 = widenAF16_0(14, 10)
  val widenEbF16_0 = widenBF16_0(14, 10)
  val widenEaF16_1 = widenAF16_1(14, 10)
  val widenEbF16_1 = widenBF16_1(14, 10)
  val widenEaF32_0 = widenAF32_0(30, 23)
  val widenEbF32_0 = widenBF32_0(30, 23)

  val eaF64IsNotZero = eaF64.orR
  val ebF64IsNotZero = ebF64.orR
  val ecF64IsNotZero = ecF64.orR
  val eaF32_0IsNotZero = eaF32_0.orR
  val ebF32_0IsNotZero = ebF32_0.orR
  val ecF32_0IsNotZero = ecF32_0.orR
  val eaF32_1IsNotZero = eaF32_1.orR
  val ebF32_1IsNotZero = ebF32_1.orR
  val ecF32_1IsNotZero = ecF32_1.orR
  val eaF16_0IsNotZero = eaF16_0.orR
  val ebF16_0IsNotZero = ebF16_0.orR
  val ecF16_0IsNotZero = ecF16_0.orR
  val eaF16_1IsNotZero = eaF16_1.orR
  val ebF16_1IsNotZero = ebF16_1.orR
  val ecF16_1IsNotZero = ecF16_1.orR
  val eaF16_2IsNotZero = eaF16_2.orR
  val ebF16_2IsNotZero = ebF16_2.orR
  val ecF16_2IsNotZero = ecF16_2.orR
  val eaF16_3IsNotZero = eaF16_3.orR
  val ebF16_3IsNotZero = ebF16_3.orR
  val ecF16_3IsNotZero = ecF16_3.orR
  val fpASignificandF16_0 = Cat(eaF16_0IsNotZero, fpAF16_0(9, 0))
  val fpBSignificandF16_0 = Cat(ebF16_0IsNotZero, fpBF16_0(9, 0))
  val fpCSignificandF16_0 = Cat(ecF16_0IsNotZero, fpCF16_0(9, 0))
  val fpASignificandF16_1 = Cat(eaF16_1IsNotZero, fpAF16_1(9, 0))
  val fpBSignificandF16_1 = Cat(ebF16_1IsNotZero, fpBF16_1(9, 0))
  val fpCSignificandF16_1 = Cat(ecF16_1IsNotZero, fpCF16_1(9, 0))
  val fpASignificandF16_2 = Cat(eaF16_2IsNotZero, fpAF16_2(9, 0))
  val fpBSignificandF16_2 = Cat(ebF16_2IsNotZero, fpBF16_2(9, 0))
  val fpCSignificandF16_2 = Cat(ecF16_2IsNotZero, fpCF16_2(9, 0))
  val fpASignificandF16_3 = Cat(eaF16_3IsNotZero, fpAF16_3(9, 0))
  val fpBSignificandF16_3 = Cat(ebF16_3IsNotZero, fpBF16_3(9, 0))
  val fpCSignificandF16_3 = Cat(ecF16_3IsNotZero, fpCF16_3(9, 0))
  val widenASignificandF16_0 = Cat(widenAF16_0(14, 10).orR, widenAF16_0(9, 0))
  val widenBSignificandF16_0 = Cat(widenBF16_0(14, 10).orR, widenBF16_0(9, 0))
  val widenASignificandF16_1 = Cat(widenAF16_1(14, 10).orR, widenAF16_1(9, 0))
  val widenBSignificandF16_1 = Cat(widenBF16_1(14, 10).orR, widenBF16_1(9, 0))
  val widenASignificandF32_0 = Cat(widenAF32_0(30, 23).orR, widenAF32_0(22, 0))
  val widenBSignificandF32_0 = Cat(widenBF32_0(30, 23).orR, widenBF32_0(22, 0))
  val fpASignificandF32_0 = Mux(ctrl.res_widening & isFp32, Cat(widenASignificandF16_0, 0.U(13.W)), Cat(eaF32_0IsNotZero, fpAF32_0(22, 0)))
  val fpBSignificandF32_0 = Mux(ctrl.res_widening & isFp32, Cat(widenBSignificandF16_0, 0.U(13.W)), Cat(ebF32_0IsNotZero, fpBF32_0(22, 0)))
  val fpCSignificandF32_0 = Cat(ecF32_0IsNotZero, fpCF32_0(22, 0))
  val fpASignificandF32_1 = Mux(ctrl.res_widening & isFp32, Cat(widenASignificandF16_1, 0.U(13.W)), Cat(eaF32_1IsNotZero, fpAF32_1(22, 0)))
  val fpBSignificandF32_1 = Mux(ctrl.res_widening & isFp32, Cat(widenBSignificandF16_1, 0.U(13.W)), Cat(ebF32_1IsNotZero, fpBF32_1(22, 0)))
  val fpCSignificandF32_1 = Cat(ecF32_1IsNotZero, fpCF32_1(22, 0))
  val fpASignificandF64 = Mux(ctrl.res_widening & isFp64, Cat(widenASignificandF32_0, 0.U(29.W)), Cat(eaF64IsNotZero, fpAF64.tail(exponentWidth + 1)))
  val fpBSignificandF64 = Mux(ctrl.res_widening & isFp64, Cat(widenBSignificandF32_0, 0.U(29.W)), Cat(ebF64IsNotZero, fpBF64.tail(exponentWidth + 1)))
  val fpCSignificandF64 = Cat(ecF64IsNotZero, fpCF64.tail(exponentWidth + 1))

  val rshiftBasicF64 = significandWidth + 3
  val rshiftMaxF64 = 3 * significandWidth + 4
  val rshiftBasicF32 = 24 + 3
  val rshiftMaxF32 = 3 * 24 + 4
  val rshiftBasicF16 = 11 + 3
  val rshiftMaxF16 = 3 * 11 + 4

  val eaFixF64 = Cat(eaF64.head(exponentWidth - 1), !eaF64IsNotZero | eaF64(0))
  val ebFixF64 = Cat(ebF64.head(exponentWidth - 1), !ebF64IsNotZero | ebF64(0))
  val ecFixF64 = Cat(ecF64.head(exponentWidth - 1), !ecF64IsNotZero | ecF64(0))
  val eaFixF32_0 = Cat(eaF32_0.head(8 - 1), !eaF32_0IsNotZero | eaF32_0(0))
  val ebFixF32_0 = Cat(ebF32_0.head(8 - 1), !ebF32_0IsNotZero | ebF32_0(0))
  val ecFixF32_0 = Cat(ecF32_0.head(8 - 1), !ecF32_0IsNotZero | ecF32_0(0))
  val eaFixF32_1 = Cat(eaF32_1.head(8 - 1), !eaF32_1IsNotZero | eaF32_1(0))
  val ebFixF32_1 = Cat(ebF32_1.head(8 - 1), !ebF32_1IsNotZero | ebF32_1(0))
  val ecFixF32_1 = Cat(ecF32_1.head(8 - 1), !ecF32_1IsNotZero | ecF32_1(0))
  val eaFixF16_0 = Cat(eaF16_0.head(5 - 1), !eaF16_0IsNotZero | eaF16_0(0))
  val ebFixF16_0 = Cat(ebF16_0.head(5 - 1), !ebF16_0IsNotZero | ebF16_0(0))
  val ecFixF16_0 = Cat(ecF16_0.head(5 - 1), !ecF16_0IsNotZero | ecF16_0(0))
  val eaFixF16_1 = Cat(eaF16_1.head(5 - 1), !eaF16_1IsNotZero | eaF16_1(0))
  val ebFixF16_1 = Cat(ebF16_1.head(5 - 1), !ebF16_1IsNotZero | ebF16_1(0))
  val ecFixF16_1 = Cat(ecF16_1.head(5 - 1), !ecF16_1IsNotZero | ecF16_1(0))
  val eaFixF16_2 = Cat(eaF16_2.head(5 - 1), !eaF16_2IsNotZero | eaF16_2(0))
  val ebFixF16_2 = Cat(ebF16_2.head(5 - 1), !ebF16_2IsNotZero | ebF16_2(0))
  val ecFixF16_2 = Cat(ecF16_2.head(5 - 1), !ecF16_2IsNotZero | ecF16_2(0))
  val eaFixF16_3 = Cat(eaF16_3.head(5 - 1), !eaF16_3IsNotZero | eaF16_3(0))
  val ebFixF16_3 = Cat(ebF16_3.head(5 - 1), !ebF16_3IsNotZero | ebF16_3(0))
  val ecFixF16_3 = Cat(ecF16_3.head(5 - 1), !ecF16_3IsNotZero | ecF16_3(0))
  val widenEaFixF16_0 = Cat(widenEaF16_0.head(4), (!widenEaF16_0.orR) | widenEaF16_0(0))
  val widenEbFixF16_0 = Cat(widenEbF16_0.head(4), (!widenEbF16_0.orR) | widenEbF16_0(0))
  val widenEaFixF16_1 = Cat(widenEaF16_1.head(4), (!widenEaF16_1.orR) | widenEaF16_1(0))
  val widenEbFixF16_1 = Cat(widenEbF16_1.head(4), (!widenEbF16_1.orR) | widenEbF16_1(0))
  val widenEaFixF32_0 = Cat(widenEaF32_0.head(7), (!widenEaF32_0.orR) | widenEaF32_0(0))
  val widenEbFixF32_0 = Cat(widenEbF32_0.head(7), (!widenEbF32_0.orR) | widenEbF32_0(0))

  val biasF64 = (1 << (exponentWidth - 1)) - 1
  val biasF32 = (1 << (8 - 1)) - 1
  val biasF16 = (1 << (5 - 1)) - 1

  val eaFixF64Widening = Mux(ctrl.res_widening & isFp64, Cat(widenEaFixF32_0.head(1), Fill(3, (~widenEaFixF32_0.head(1)).asUInt), widenEaFixF32_0(6, 0)), eaFixF64)
  val ebFixF64Widening = Mux(ctrl.res_widening & isFp64, Cat(widenEbFixF32_0.head(1), Fill(3, (~widenEbFixF32_0.head(1)).asUInt), widenEbFixF32_0(6, 0)), ebFixF64)
  val eaFixF32Widening_0 = Mux(ctrl.res_widening & isFp32, Cat(widenEaFixF16_0.head(1), Fill(3, (~widenEaFixF16_0.head(1)).asUInt), widenEaFixF16_0(3, 0)), eaFixF32_0)
  val ebFixF32Widening_0 = Mux(ctrl.res_widening & isFp32, Cat(widenEbFixF16_0.head(1), Fill(3, (~widenEbFixF16_0.head(1)).asUInt), widenEbFixF16_0(3, 0)), ebFixF32_0)
  val eaFixF32Widening_1 = Mux(ctrl.res_widening & isFp32, Cat(widenEaFixF16_1.head(1), Fill(3, (~widenEaFixF16_1.head(1)).asUInt), widenEaFixF16_1(3, 0)), eaFixF32_1)
  val ebFixF32Widening_1 = Mux(ctrl.res_widening & isFp32, Cat(widenEbFixF16_1.head(1), Fill(3, (~widenEbFixF16_1.head(1)).asUInt), widenEbFixF16_1(3, 0)), ebFixF32_1)

  val eabF64 = Cat(0.U, eaFixF64Widening +& ebFixF64Widening).asSInt - biasF64.S + rshiftBasicF64.S
  val eabF32_0 = Cat(0.U, eaFixF32Widening_0 +& ebFixF32Widening_0).asSInt - biasF32.S + rshiftBasicF32.S
  val eabF32_1 = Cat(0.U, eaFixF32Widening_1 +& ebFixF32Widening_1).asSInt - biasF32.S + rshiftBasicF32.S
  val eabF16_0 = Cat(0.U, eaFixF16_0 +& ebFixF16_0).asSInt - biasF16.S + rshiftBasicF16.S
  val eabF16_1 = Cat(0.U, eaFixF16_1 +& ebFixF16_1).asSInt - biasF16.S + rshiftBasicF16.S
  val eabF16_2 = Cat(0.U, eaFixF16_2 +& ebFixF16_2).asSInt - biasF16.S + rshiftBasicF16.S
  val eabF16_3 = Cat(0.U, eaFixF16_3 +& ebFixF16_3).asSInt - biasF16.S + rshiftBasicF16.S

  val rshiftValueF64 = eabF64 - Cat(0.U, ecFixF64).asSInt
  val rshiftValueF32_0 = eabF32_0 - Cat(0.U, ecFixF32_0).asSInt
  val rshiftValueF32_1 = eabF32_1 - Cat(0.U, ecFixF32_1).asSInt
  val rshiftValueF16_0 = eabF16_0 - Cat(0.U, ecFixF16_0).asSInt
  val rshiftValueF16_1 = eabF16_1 - Cat(0.U, ecFixF16_1).asSInt
  val rshiftValueF16_2 = eabF16_2 - Cat(0.U, ecFixF16_2).asSInt
  val rshiftValueF16_3 = eabF16_3 - Cat(0.U, ecFixF16_3).asSInt

  val rshiftValueCutF64 = rshiftValueF64(rshiftMaxF64.U.getWidth - 1, 0)
  val rshiftValueCutF32_0 = rshiftValueF32_0(rshiftMaxF32.U.getWidth - 1, 0)
  val rshiftValueCutF32_1 = rshiftValueF32_1(rshiftMaxF32.U.getWidth - 1, 0)
  val rshiftValueCutF16_0 = rshiftValueF16_0(rshiftMaxF16.U.getWidth - 1, 0)
  val rshiftValueCutF16_1 = rshiftValueF16_1(rshiftMaxF16.U.getWidth - 1, 0)
  val rshiftValueCutF16_2 = rshiftValueF16_2(rshiftMaxF16.U.getWidth - 1, 0)
  val rshiftValueCutF16_3 = rshiftValueF16_3(rshiftMaxF16.U.getWidth - 1, 0)

  val fpCSignificandCat0F64 = Cat(fpCSignificandF64, 0.U((rshiftMaxF64 - significandWidth).W))
  val fpCSignificandCat0F32_0 = Cat(fpCSignificandF32_0, 0.U((rshiftMaxF32 - 24).W))
  val fpCSignificandCat0F32_1 = Cat(fpCSignificandF32_1, 0.U((rshiftMaxF32 - 24).W))
  val fpCSignificandCat0F16_0 = Cat(fpCSignificandF16_0, 0.U((rshiftMaxF16 - 11).W))
  val fpCSignificandCat0F16_1 = Cat(fpCSignificandF16_1, 0.U((rshiftMaxF16 - 11).W))
  val fpCSignificandCat0F16_2 = Cat(fpCSignificandF16_2, 0.U((rshiftMaxF16 - 11).W))
  val fpCSignificandCat0F16_3 = Cat(fpCSignificandF16_3, 0.U((rshiftMaxF16 - 11).W))

  val rshiftResultWithGrsF64 = shiftRightWithMuxSticky(fpCSignificandCat0F64, rshiftValueCutF64)
  val rshiftResultWithGrsF64F32_1 = shiftRightWithMuxSticky(
    Mux(isFp64, fpCSignificandCat0F64, fpCSignificandCat0F32_1.asTypeOf(fpCSignificandCat0F64)),
    Mux(isFp64, rshiftValueCutF64, rshiftValueCutF32_1.asTypeOf(rshiftValueCutF64))
  )
  val rshiftResultWithGrsF32_0 = shiftRightWithMuxSticky(fpCSignificandCat0F32_0, rshiftValueCutF32_0)
  val rshiftResultWithGrsF32_1 = rshiftResultWithGrsF64F32_1.asTypeOf(rshiftResultWithGrsF32_0)
  val rshiftResultWithGrsF16_0 = shiftRightWithMuxSticky(fpCSignificandCat0F16_0, rshiftValueCutF16_0)
  val rshiftResultWithGrsF16_1 = shiftRightWithMuxSticky(fpCSignificandCat0F16_1, rshiftValueCutF16_1)
  val rshiftResultWithGrsF16_2 = shiftRightWithMuxSticky(fpCSignificandCat0F16_2, rshiftValueCutF16_2)
  val rshiftResultWithGrsF16_3 = shiftRightWithMuxSticky(fpCSignificandCat0F16_3, rshiftValueCutF16_3)

  val ecIsTooBigF64 = rshiftValueF64 <= 0.S
  val ecIsTooBigF32_0 = rshiftValueF32_0 <= 0.S
  val ecIsTooBigF32_1 = rshiftValueF32_1 <= 0.S
  val ecIsTooBigF16_0 = rshiftValueF16_0 <= 0.S
  val ecIsTooBigF16_1 = rshiftValueF16_1 <= 0.S
  val ecIsTooBigF16_2 = rshiftValueF16_2 <= 0.S
  val ecIsTooBigF16_3 = rshiftValueF16_3 <= 0.S
  val ecIsTooSmallF64 = rshiftValueF64.asSInt > rshiftMaxF64.S
  val ecIsTooSmallF32_0 = rshiftValueF32_0.asSInt > rshiftMaxF32.S
  val ecIsTooSmallF32_1 = rshiftValueF32_1.asSInt > rshiftMaxF32.S
  val ecIsTooSmallF16_0 = rshiftValueF16_0.asSInt > rshiftMaxF16.S
  val ecIsTooSmallF16_1 = rshiftValueF16_1.asSInt > rshiftMaxF16.S
  val ecIsTooSmallF16_2 = rshiftValueF16_2.asSInt > rshiftMaxF16.S
  val ecIsTooSmallF16_3 = rshiftValueF16_3.asSInt > rshiftMaxF16.S
  val ecIsMediumF64 = !ecIsTooBigF64 & !ecIsTooSmallF64
  val ecIsMediumF32_0 = !ecIsTooBigF32_0 & !ecIsTooSmallF32_0
  val ecIsMediumF32_1 = !ecIsTooBigF32_1 & !ecIsTooSmallF32_1
  val ecIsMediumF16_0 = !ecIsTooBigF16_0 & !ecIsTooSmallF16_0
  val ecIsMediumF16_1 = !ecIsTooBigF16_1 & !ecIsTooSmallF16_1
  val ecIsMediumF16_2 = !ecIsTooBigF16_2 & !ecIsTooSmallF16_2
  val ecIsMediumF16_3 = !ecIsTooBigF16_3 & !ecIsTooSmallF16_3

  val rshiftGuard = Cat(
    Mux(ecIsMediumF16_3, rshiftResultWithGrsF16_3(2), 0.U),
    Mux(ecIsMediumF16_2, rshiftResultWithGrsF16_2(2), 0.U),
    Mux(isFp32, Mux(ecIsMediumF32_1, rshiftResultWithGrsF32_1(2), 0.U), Mux(ecIsMediumF16_1, rshiftResultWithGrsF16_1(2), 0.U)),
    Mux(
      isFp64,
      Mux(ecIsMediumF64, rshiftResultWithGrsF64(2), 0.U),
      Mux(isFp32, Mux(ecIsMediumF32_0, rshiftResultWithGrsF32_0(2), 0.U), Mux(ecIsMediumF16_0, rshiftResultWithGrsF16_0(2), 0.U))
    )
  )
  val rshiftRound = Cat(
    Mux(ecIsMediumF16_3, rshiftResultWithGrsF16_3(1), 0.U),
    Mux(ecIsMediumF16_2, rshiftResultWithGrsF16_2(1), 0.U),
    Mux(isFp32, Mux(ecIsMediumF32_1, rshiftResultWithGrsF32_1(1), 0.U), Mux(ecIsMediumF16_1, rshiftResultWithGrsF16_1(1), 0.U)),
    Mux(
      isFp64,
      Mux(ecIsMediumF64, rshiftResultWithGrsF64(1), 0.U),
      Mux(isFp32, Mux(ecIsMediumF32_0, rshiftResultWithGrsF32_0(1), 0.U), Mux(ecIsMediumF16_0, rshiftResultWithGrsF16_0(1), 0.U))
    )
  )
  val rshiftSticky = Cat(
    Mux(ecIsMediumF16_3, rshiftResultWithGrsF16_3(0), Mux(ecIsTooBigF16_3, 0.U, fpCSignificandF16_3.orR)),
    Mux(ecIsMediumF16_2, rshiftResultWithGrsF16_2(0), Mux(ecIsTooBigF16_2, 0.U, fpCSignificandF16_2.orR)),
    Mux(
      isFp32,
      Mux(ecIsMediumF32_1, rshiftResultWithGrsF32_1(0), Mux(ecIsTooBigF32_1, 0.U, fpCSignificandF32_1.orR)),
      Mux(ecIsMediumF16_1, rshiftResultWithGrsF16_1(0), Mux(ecIsTooBigF16_1, 0.U, fpCSignificandF16_1.orR))
    ),
    Mux(
      isFp64,
      Mux(ecIsMediumF64, rshiftResultWithGrsF64(0), Mux(ecIsTooBigF64, 0.U, fpCSignificandF64.orR)),
      Mux(
        isFp32,
        Mux(ecIsMediumF32_0, rshiftResultWithGrsF32_0(0), Mux(ecIsTooBigF32_0, 0.U, fpCSignificandF32_0.orR)),
        Mux(ecIsMediumF16_0, rshiftResultWithGrsF16_0(0), Mux(ecIsTooBigF16_0, 0.U, fpCSignificandF16_0.orR))
      )
    )
  )

  val rshiftResultTempF64 = rshiftResultWithGrsF64.head(rshiftMaxF64 - 2)
  val rshiftResultTempF32_0 = rshiftResultWithGrsF32_0.head(rshiftMaxF32 - 2)
  val rshiftResultTempF32_1 = rshiftResultWithGrsF32_1.head(rshiftMaxF32 - 2)
  val rshiftResultTempF16_0 = rshiftResultWithGrsF16_0.head(rshiftMaxF16 - 2)
  val rshiftResultTempF16_1 = rshiftResultWithGrsF16_1.head(rshiftMaxF16 - 2)
  val rshiftResultTempF16_2 = rshiftResultWithGrsF16_2.head(rshiftMaxF16 - 2)
  val rshiftResultTempF16_3 = rshiftResultWithGrsF16_3.head(rshiftMaxF16 - 2)
  val rshiftResultF64 = Mux(ecIsMediumF64, rshiftResultTempF64, Mux(ecIsTooBigF64, fpCSignificandCat0F64.head(rshiftMaxF64 - 2), 0.U((rshiftMaxF64 - 2).W)))
  val rshiftResultF32_0 = Mux(ecIsMediumF32_0, rshiftResultTempF32_0, Mux(ecIsTooBigF32_0, fpCSignificandCat0F32_0.head(rshiftMaxF32 - 2), 0.U((rshiftMaxF32 - 2).W)))
  val rshiftResultF32_1 = Mux(ecIsMediumF32_1, rshiftResultTempF32_1, Mux(ecIsTooBigF32_1, fpCSignificandCat0F32_1.head(rshiftMaxF32 - 2), 0.U((rshiftMaxF32 - 2).W)))
  val rshiftResultF16_0 = Mux(ecIsMediumF16_0, rshiftResultTempF16_0, Mux(ecIsTooBigF16_0, fpCSignificandCat0F16_0.head(rshiftMaxF16 - 2), 0.U((rshiftMaxF16 - 2).W)))
  val rshiftResultF16_1 = Mux(ecIsMediumF16_1, rshiftResultTempF16_1, Mux(ecIsTooBigF16_1, fpCSignificandCat0F16_1.head(rshiftMaxF16 - 2), 0.U((rshiftMaxF16 - 2).W)))
  val rshiftResultF16_2 = Mux(ecIsMediumF16_2, rshiftResultTempF16_2, Mux(ecIsTooBigF16_2, fpCSignificandCat0F16_2.head(rshiftMaxF16 - 2), 0.U((rshiftMaxF16 - 2).W)))
  val rshiftResultF16_3 = Mux(ecIsMediumF16_3, rshiftResultTempF16_3, Mux(ecIsTooBigF16_3, fpCSignificandCat0F16_3.head(rshiftMaxF16 - 2), 0.U((rshiftMaxF16 - 2).W)))

  val fpCRshiftValueInv = Mux(
    isFp64,
    Mux(isSubF64.asBool, Cat(1.U, ~rshiftResultF64), Cat(0.U, rshiftResultF64)),
    Mux(
      isFp32,
      Cat(
        Mux(isSubF32_1.asBool, Cat(1.U, ~rshiftResultF32_1), Cat(0.U, rshiftResultF32_1)),
        Mux(isSubF32_0.asBool, Cat(1.U, ~rshiftResultF32_0), Cat(0.U, rshiftResultF32_0))
      ),
      Cat(
        Mux(isSubF16_3.asBool, Cat(1.U, ~rshiftResultF16_3), Cat(0.U, rshiftResultF16_3)),
        Mux(isSubF16_2.asBool, Cat(1.U, ~rshiftResultF16_2), Cat(0.U, rshiftResultF16_2)),
        Mux(isSubF16_1.asBool, Cat(1.U, ~rshiftResultF16_1), Cat(0.U, rshiftResultF16_1)),
        Mux(isSubF16_0.asBool, Cat(1.U, ~rshiftResultF16_0), Cat(0.U, rshiftResultF16_0))
      )
    )
  )

  val boothInA = Mux(
    isFp64,
    fpASignificandF64,
    Mux(
      isFp32,
      Cat(fpASignificandF32_1, 0.U(5.W), fpASignificandF32_0),
      Cat(Cat(fpASignificandF16_3, 0.U(2.W), fpASignificandF16_2), 0.U(5.W), Cat(fpASignificandF16_1, 0.U(2.W), fpASignificandF16_0))
    )
  )
  val boothInB = Mux(
    isFp64,
    fpBSignificandF64,
    Mux(
      isFp32,
      Cat(fpBSignificandF32_1, 0.U(5.W), fpBSignificandF32_0),
      Cat(Cat(fpBSignificandF16_3, 0.U(2.W), fpBSignificandF16_2), 0.U(5.W), Cat(fpBSignificandF16_1, 0.U(2.W), fpBSignificandF16_0))
    )
  )

  val boothEncoder = Module(new VectorFMABoothEncoder(width = significandWidth, isAddendExpand1bit = true))
  boothEncoder.io.in_a := boothInA
  boothEncoder.io.in_b := boothInB
  boothEncoder.io.is_fp64 := isFp64
  boothEncoder.io.is_fp32 := isFp32

  val csaPreFinal = Module(new VectorFMACompressTo4(boothEncoder.io.out_pp.length, boothEncoder.io.out_pp.head.getWidth))
  csaPreFinal.io.in := boothEncoder.io.out_pp

  out.data.fpCF64 := fpCF64
  out.data.fpCF32_1 := fpCF32_1
  out.data.fpCF32_0 := fpCF32_0
  out.data.fpCF16_3 := fpCF16_3
  out.data.fpCF16_2 := fpCF16_2
  out.data.fpCF16_1 := fpCF16_1
  out.data.fpCF16_0 := fpCF16_0

  out.ctrl.signABF64 := signABF64
  out.ctrl.signABF32_1 := signABF32_1
  out.ctrl.signABF32_0 := signABF32_0
  out.ctrl.signABF16_3 := signABF16_3
  out.ctrl.signABF16_2 := signABF16_2
  out.ctrl.signABF16_1 := signABF16_1
  out.ctrl.signABF16_0 := signABF16_0

  out.ctrl.signCF64 := signCF64
  out.ctrl.signCF32_1 := signCF32_1
  out.ctrl.signCF32_0 := signCF32_0
  out.ctrl.signCF16_3 := signCF16_3
  out.ctrl.signCF16_2 := signCF16_2
  out.ctrl.signCF16_1 := signCF16_1
  out.ctrl.signCF16_0 := signCF16_0

  out.ctrl.isSubF64 := isSubF64
  out.ctrl.isSubF32_1 := isSubF32_1
  out.ctrl.isSubF32_0 := isSubF32_0
  out.ctrl.isSubF16_3 := isSubF16_3
  out.ctrl.isSubF16_2 := isSubF16_2
  out.ctrl.isSubF16_1 := isSubF16_1
  out.ctrl.isSubF16_0 := isSubF16_0

  out.ctrl.round_mode := ctrl.round_mode
  out.ctrl.isVfmul := ctrl.isVfmul
  out.ctrl.resultIsFp16 := resultIsFp16
  out.ctrl.resultIsFp32 := resultIsFp32
  out.ctrl.resultIsFp64 := resultIsFp64
  out.ctrl.res_widening := ctrl.res_widening
  out.ctrl.fp_aIsFpCanonicalNAN := fpAIsFpCanonicalNAN
  out.ctrl.fp_bIsFpCanonicalNAN := fpBIsFpCanonicalNAN
  out.ctrl.fp_cIsFpCanonicalNAN := fpCIsFpCanonicalNAN

  out.data.eaF64 := eaF64
  out.data.eaF32_1 := eaF32_1
  out.data.eaF32_0 := eaF32_0
  out.data.eaF16_3 := eaF16_3
  out.data.eaF16_2 := eaF16_2
  out.data.eaF16_1 := eaF16_1
  out.data.eaF16_0 := eaF16_0

  out.data.ebF64 := ebF64
  out.data.ebF32_1 := ebF32_1
  out.data.ebF32_0 := ebF32_0
  out.data.ebF16_3 := ebF16_3
  out.data.ebF16_2 := ebF16_2
  out.data.ebF16_1 := ebF16_1
  out.data.ebF16_0 := ebF16_0

  out.data.ecF64 := ecF64
  out.data.ecF32_1 := ecF32_1
  out.data.ecF32_0 := ecF32_0
  out.data.ecF16_3 := ecF16_3
  out.data.ecF16_2 := ecF16_2
  out.data.ecF16_1 := ecF16_1
  out.data.ecF16_0 := ecF16_0

  out.data.fpASignificandF64 := fpASignificandF64
  out.data.fpASignificandF32_1 := fpASignificandF32_1
  out.data.fpASignificandF32_0 := fpASignificandF32_0
  out.data.fpASignificandF16_3 := fpASignificandF16_3
  out.data.fpASignificandF16_2 := fpASignificandF16_2
  out.data.fpASignificandF16_1 := fpASignificandF16_1
  out.data.fpASignificandF16_0 := fpASignificandF16_0

  out.data.fpBSignificandF64 := fpBSignificandF64
  out.data.fpBSignificandF32_1 := fpBSignificandF32_1
  out.data.fpBSignificandF32_0 := fpBSignificandF32_0
  out.data.fpBSignificandF16_3 := fpBSignificandF16_3
  out.data.fpBSignificandF16_2 := fpBSignificandF16_2
  out.data.fpBSignificandF16_1 := fpBSignificandF16_1
  out.data.fpBSignificandF16_0 := fpBSignificandF16_0

  out.data.fpCSignificandF64 := fpCSignificandF64
  out.data.fpCSignificandF32_1 := fpCSignificandF32_1
  out.data.fpCSignificandF32_0 := fpCSignificandF32_0
  out.data.fpCSignificandF16_3 := fpCSignificandF16_3
  out.data.fpCSignificandF16_2 := fpCSignificandF16_2
  out.data.fpCSignificandF16_1 := fpCSignificandF16_1
  out.data.fpCSignificandF16_0 := fpCSignificandF16_0

  out.ctrl.ecFixF64 := ecFixF64
  out.ctrl.ecFixF32_1 := ecFixF32_1
  out.ctrl.ecFixF32_0 := ecFixF32_0
  out.ctrl.ecFixF16_3 := ecFixF16_3
  out.ctrl.ecFixF16_2 := ecFixF16_2
  out.ctrl.ecFixF16_1 := ecFixF16_1
  out.ctrl.ecFixF16_0 := ecFixF16_0

  out.ctrl.eabF64 := eabF64(exponentWidth, 0).asUInt
  out.ctrl.eabF32_1 := eabF32_1(8, 0).asUInt
  out.ctrl.eabF32_0 := eabF32_0(8, 0).asUInt
  out.ctrl.eabF16_3 := eabF16_3(5, 0).asUInt
  out.ctrl.eabF16_2 := eabF16_2(5, 0).asUInt
  out.ctrl.eabF16_1 := eabF16_1(5, 0).asUInt
  out.ctrl.eabF16_0 := eabF16_0(5, 0).asUInt

  out.ctrl.eabIsGreaterF64 := rshiftValueF64 > 0.S
  out.ctrl.eabIsGreaterF32_1 := rshiftValueF32_1 > 0.S
  out.ctrl.eabIsGreaterF32_0 := rshiftValueF32_0 > 0.S
  out.ctrl.eabIsGreaterF16_3 := rshiftValueF16_3 > 0.S
  out.ctrl.eabIsGreaterF16_2 := rshiftValueF16_2 > 0.S
  out.ctrl.eabIsGreaterF16_1 := rshiftValueF16_1 > 0.S
  out.ctrl.eabIsGreaterF16_0 := rshiftValueF16_0 > 0.S

  out.data.widenEaF16_0 := widenEaF16_0
  out.data.widenEaF16_1 := widenEaF16_1
  out.data.widenEbF16_0 := widenEbF16_0
  out.data.widenEbF16_1 := widenEbF16_1
  out.data.widenEaF32_0 := widenEaF32_0
  out.data.widenEbF32_0 := widenEbF32_0

  out.ctrl.rshiftGuard := rshiftGuard
  out.ctrl.rshiftRound := rshiftRound
  out.ctrl.rshiftSticky := rshiftSticky
  out.data.fpCRshiftValueInv := fpCRshiftValueInv
  out.data.csaPreFinal := csaPreFinal.io.out
}

object VectorFMAS0 {
  class S0ToS1Ctrl extends VectorFMA.Bundle {
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
    val isSubF64 = Bool()
    val isSubF32_1 = Bool()
    val isSubF32_0 = Bool()
    val isSubF16_3 = Bool()
    val isSubF16_2 = Bool()
    val isSubF16_1 = Bool()
    val isSubF16_0 = Bool()
    val ecFixF64 = UInt(exponentWidth.W)
    val ecFixF32_1 = UInt(8.W)
    val ecFixF32_0 = UInt(8.W)
    val ecFixF16_3 = UInt(5.W)
    val ecFixF16_2 = UInt(5.W)
    val ecFixF16_1 = UInt(5.W)
    val ecFixF16_0 = UInt(5.W)
    val eabF64 = UInt((exponentWidth + 1).W)
    val eabF32_1 = UInt(9.W)
    val eabF32_0 = UInt(9.W)
    val eabF16_3 = UInt(6.W)
    val eabF16_2 = UInt(6.W)
    val eabF16_1 = UInt(6.W)
    val eabF16_0 = UInt(6.W)
    val eabIsGreaterF64 = Bool()
    val eabIsGreaterF32_1 = Bool()
    val eabIsGreaterF32_0 = Bool()
    val eabIsGreaterF16_3 = Bool()
    val eabIsGreaterF16_2 = Bool()
    val eabIsGreaterF16_1 = Bool()
    val eabIsGreaterF16_0 = Bool()
    val rshiftGuard = UInt(laneFlagWidth.W)
    val rshiftRound = UInt(laneFlagWidth.W)
    val rshiftSticky = UInt(laneFlagWidth.W)
  }

  class S0ToS1Data extends VectorFMA.Bundle {
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
    val fpCRshiftValueInv = UInt(fpCRshiftValueInvWidth.W)
    val csaPreFinal = Vec(4, UInt(csaWidth.W))
  }

  class S0ToS1 extends VectorFMA.Bundle {
    val ctrl = new S0ToS1Ctrl
    val data = new S0ToS1Data
  }
}
