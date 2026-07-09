package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.VfcvtType
import yunsuan.util._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.RoundingUnit

class CVT_fp8_fp4(width: Int = 64) extends CVT(width) {
  require(width >= 16, "CVT_fp8_fp4 needs at least 16 result bits")

  private def padResult(x: UInt, payloadWidth: Int): UInt = {
    require(width >= payloadWidth)
    val payload = x(payloadWidth - 1, 0)
    if (width == payloadWidth) payload else Cat(0.U((width - payloadWidth).W), payload)
  }

  private def srcLo8: UInt = io.src(7, 0)
  private def srcLo16: UInt = io.src(15, 0)
  private def srcLo32: UInt = if (width >= 32) io.src(31, 0) else 0.U(32.W)

  private val canonicalFp8 = "h7f".U(8.W)
  private val canonicalBf16 = "h7fc0".U(16.W)

  private def fp4ToFp8E4M3(x: UInt): UInt = {
    val sign = x(3)
    val exp = x(2, 1)
    val frac = x(0)
    val isZero = exp === 0.U && !frac
    val isSubnormal = exp === 0.U && frac
    val fp8Exp = Mux(isSubnormal, 6.U(4.W), Cat(0.U(2.W), exp) + 6.U(4.W))
    val fp8Frac = Mux(isSubnormal, 0.U(3.W), frac ## 0.U(2.W))
    Mux(isZero, sign ## 0.U(7.W), sign ## fp8Exp ## fp8Frac)
  }

  private def subnormalToBf16(sign: Bool, frac: UInt, fracWidth: Int, bias: Int): UInt = {
    val cases = (0 until fracWidth).map { k =>
      val higherIsZero =
        if (k == fracWidth - 1) true.B else !frac(fracWidth - 1, k + 1).orR
      val isMsb = frac(k) && higherIsZero
      val bfExp = (127 + 1 - bias - fracWidth + k).U(8.W)
      val fracWithoutMsb = frac ^ (BigInt(1) << k).U(fracWidth.W)
      val bfFrac = (fracWithoutMsb << (7 - k))(6, 0)
      isMsb -> (sign ## bfExp ## bfFrac)
    }
    Mux1H(cases)
  }

  private def fp8ToBf16E4M3(x: UInt): UInt = {
    val sign = x(7)
    val exp = x(6, 3)
    val frac = x(2, 0)
    val isZero = !exp.orR && !frac.orR
    val isSubnormal = !exp.orR && frac.orR
    val isNaN = exp.andR && frac.andR
    val normalExp = Cat(0.U(4.W), exp) + (127 - 7).U(8.W)
    val normal = sign ## normalExp ## (frac ## 0.U(4.W))
    Mux1H(Seq(
      isZero -> (sign ## 0.U(15.W)),
      isSubnormal -> subnormalToBf16(sign, frac, 3, 7),
      isNaN -> canonicalBf16,
      (!isZero && !isSubnormal && !isNaN) -> normal
    ))
  }

  private def fp8ToBf16E5M2(x: UInt): UInt = {
    val sign = x(7)
    val exp = x(6, 2)
    val frac = x(1, 0)
    val isZero = !exp.orR && !frac.orR
    val isSubnormal = !exp.orR && frac.orR
    val isInf = exp.andR && !frac.orR
    val isNaN = exp.andR && frac.orR
    val normalExp = Cat(0.U(3.W), exp) + (127 - 15).U(8.W)
    val normal = sign ## normalExp ## (frac ## 0.U(5.W))
    Mux1H(Seq(
      isZero -> (sign ## 0.U(15.W)),
      isSubnormal -> subnormalToBf16(sign, frac, 2, 15),
      isInf -> (sign ## "hff".U(8.W) ## 0.U(7.W)),
      isNaN -> canonicalBf16,
      (!isZero && !isSubnormal && !isInf && !isNaN) -> normal
    ))
  }

  private def roundNormalSig(fullSig: UInt, srcPrecision: Int, dstPrecision: Int, sign: Bool, rm: UInt): (UInt, Bool, Bool) = {
    val in = Wire(UInt(dstPrecision.W))
    val roundBit = Wire(Bool())
    val stickyBit = Wire(Bool())
    if (srcPrecision >= dstPrecision) {
      in := fullSig(srcPrecision - 1, srcPrecision - dstPrecision)
      if (srcPrecision == dstPrecision) {
        roundBit := false.B
        stickyBit := false.B
      } else {
        roundBit := fullSig(srcPrecision - dstPrecision - 1)
        if (srcPrecision - dstPrecision == 1) stickyBit := false.B
        else stickyBit := fullSig(srcPrecision - dstPrecision - 2, 0).orR
      }
    } else {
      in := Cat(fullSig, 0.U((dstPrecision - srcPrecision).W))
      roundBit := false.B
      stickyBit := false.B
    }

    val rounder = Module(new RoundingUnit(dstPrecision))
    rounder.io.in := in
    rounder.io.roundIn := roundBit
    rounder.io.stickyIn := stickyBit
    rounder.io.signIn := sign
    rounder.io.rm := rm
    val rounded = in +& rounder.io.r_up.asUInt
    (rounded, rounded(dstPrecision), rounder.io.inexact)
  }

  private def roundSubnormalSig(fullSig: UInt, srcPrecision: Int, srcFracWidth: Int, dstFracWidth: Int, dstExp: SInt, sign: Bool, rm: UInt): (UInt, Bool, Bool) = {
    val dstPrecision = dstFracWidth + 1
    val shiftS = (srcFracWidth + 1 - dstFracWidth).S - dstExp
    val shift = Mux(shiftS < 0.S, 0.U(6.W), shiftS.asUInt)
    val shifted = fullSig >> shift
    val roundShift = Mux(shift === 0.U, 0.U, shift - 1.U)
    val stickyShift = Mux(shift <= 1.U, 0.U, shift - 1.U)
    val roundBit = Mux(shift === 0.U, false.B, ((fullSig >> roundShift)(0)).asBool)
    val sticky = Mux(shift <= 1.U, false.B, {
      val stickyMask = (1.U(srcPrecision.W) << stickyShift)(srcPrecision - 1, 0) - 1.U
      (fullSig & stickyMask).orR
    })
    val in = shifted(dstPrecision - 1, 0)
    val rounder = Module(new RoundingUnit(dstPrecision))
    rounder.io.in := in
    rounder.io.roundIn := roundBit
    rounder.io.stickyIn := sticky
    rounder.io.signIn := sign
    rounder.io.rm := rm
    val rounded = in +& rounder.io.r_up.asUInt
    (rounded(dstPrecision - 1, 0), rounded(dstFracWidth), rounder.io.inexact)
  }

  private def narrowToFp8(x: UInt, srcExpWidth: Int, srcFracWidth: Int, srcBias: Int, toE5M2: Boolean, sat: Bool): (UInt, UInt) = {
    val dstExpWidth = if (toE5M2) 5 else 4
    val dstFracWidth = if (toE5M2) 2 else 3
    val dstBias = if (toE5M2) 15 else 7
    val dstMaxNormalExp = if (toE5M2) 30 else 15
    val dstMaxFiniteFrac = if (toE5M2) 3 else 6
    val dstPrecision = dstFracWidth + 1

    val sign = x(srcExpWidth + srcFracWidth)
    val exp = x(srcExpWidth + srcFracWidth - 1, srcFracWidth)
    val frac = x(srcFracWidth - 1, 0)
    val expNotZero = exp.orR
    val expIsOnes = exp.andR
    val fracNotZero = frac.orR
    val isZero = !expNotZero && !fracNotZero
    val isInf = expIsOnes && !fracNotZero
    val isNaN = expIsOnes && fracNotZero
    val isSNaN = isNaN && !frac(srcFracWidth - 1)
    val srcExp = Mux(expNotZero, exp.zext - srcBias.S, (1 - srcBias).S)
    val dstExp = srcExp + dstBias.S
    val fullSig = Cat(expNotZero, frac)

    val (normalSigRounded, normalCout, normalInexact) =
      roundNormalSig(fullSig, srcFracWidth + 1, dstPrecision, sign, io.rm)
    val normalExpRounded = dstExp + normalCout.asUInt.zext
    val normalFrac = normalSigRounded(dstFracWidth - 1, 0)
    val normalExpOverflow = normalExpRounded > dstMaxNormalExp.S
    val normalE4ReservedNaN =
      if (toE5M2) false.B else normalExpRounded === dstMaxNormalExp.S && normalFrac > dstMaxFiniteFrac.U
    val normalOverflow = normalExpOverflow || normalE4ReservedNaN
    val normalResult = sign ## normalExpRounded.asUInt(dstExpWidth - 1, 0) ## normalFrac

    val (subSigRounded, subToNormal, subInexact) =
      roundSubnormalSig(fullSig, srcFracWidth + 1, srcFracWidth, dstFracWidth, dstExp, sign, io.rm)
    val subResult = Mux(subToNormal,
      sign ## 1.U(dstExpWidth.W) ## 0.U(dstFracWidth.W),
      sign ## 0.U(dstExpWidth.W) ## subSigRounded(dstFracWidth - 1, 0)
    )

    val rmin = io.rm === RTZ || (sign && io.rm === RUP) || (!sign && io.rm === RDN)
    val maxFinite = sign ## dstMaxNormalExp.U(dstExpWidth.W) ## dstMaxFiniteFrac.U(dstFracWidth.W)
    val inf = sign ## Fill(dstExpWidth, 1.U(1.W)) ## 0.U(dstFracWidth.W)
    val overflowResult = Mux(sat || rmin || io.rm === RTO, maxFinite, if (toE5M2) inf else canonicalFp8)
    val infResult = if (toE5M2) inf else Mux(sat, maxFinite, canonicalFp8)
    val useNormal = dstExp >= 1.S
    val finiteResult = Mux(useNormal, Mux(normalOverflow, overflowResult, normalResult), subResult)
    val finiteInexact = Mux(useNormal, normalInexact || normalOverflow, subInexact)
    val finiteOverflow = useNormal && normalOverflow
    val finiteUnderflow = !useNormal && subInexact && !subToNormal
    val result = Mux1H(Seq(
      isNaN -> canonicalFp8,
      isInf -> infResult,
      isZero -> (sign ## 0.U(7.W)),
      (!isNaN && !isInf && !isZero) -> finiteResult
    ))
    val fflags = Cat(isSNaN, false.B, !isNaN && !isInf && finiteOverflow, !isNaN && !isInf && finiteUnderflow, !isNaN && !isInf && finiteInexact)
    (result, fflags)
  }

  val isVfwCvtBf16Fp8Next = io.opType === VfcvtType.vfwcvtbf16_ffv && io.sew === 0.U
  val isVfnCvtBf16Fp8Next = io.opType === VfcvtType.vfncvtbf16_ffw && io.sew === 0.U
  val isVfnCvtBf16SatFp8Next = io.opType === VfcvtType.vfncvtbf16_sat_ffw && io.sew === 0.U
  val isVfnCvtFfqNext = io.opType === VfcvtType.vfncvt_ffq
  val isVfnCvtSatFfqNext = io.opType === VfcvtType.vfncvt_sat_ffq
  val isVfextVf2Next = io.opType === VfcvtType.vfext_vf2

  val fp8ToBf16 = Mux(io.altfmt, fp8ToBf16E5M2(srcLo8), fp8ToBf16E4M3(srcLo8))
  val (bf16ToFp8E4M3, bf16ToFp8E4M3Fflags) = narrowToFp8(srcLo16, 8, 7, 127, toE5M2 = false, isVfnCvtBf16SatFp8Next)
  val (bf16ToFp8E5M2, bf16ToFp8E5M2Fflags) = narrowToFp8(srcLo16, 8, 7, 127, toE5M2 = true, isVfnCvtBf16SatFp8Next)
  val (fp32ToFp8E4M3, fp32ToFp8E4M3Fflags) = narrowToFp8(srcLo32, 8, 23, 127, toE5M2 = false, isVfnCvtSatFfqNext)
  val (fp32ToFp8E5M2, fp32ToFp8E5M2Fflags) = narrowToFp8(srcLo32, 8, 23, 127, toE5M2 = true, isVfnCvtSatFfqNext)

  val bf16ToFp8 = Mux(io.altfmt, bf16ToFp8E5M2, bf16ToFp8E4M3)
  val bf16ToFp8Fflags = Mux(io.altfmt, bf16ToFp8E5M2Fflags, bf16ToFp8E4M3Fflags)
  val fp32ToFp8 = Mux(io.altfmt, fp32ToFp8E5M2, fp32ToFp8E4M3)
  val fp32ToFp8Fflags = Mux(io.altfmt, fp32ToFp8E5M2Fflags, fp32ToFp8E4M3Fflags)

  val resultNext = Mux1H(Seq(
    isVfwCvtBf16Fp8Next -> padResult(fp8ToBf16, 16),
    (isVfnCvtBf16Fp8Next || isVfnCvtBf16SatFp8Next) -> padResult(bf16ToFp8, 8),
    (isVfnCvtFfqNext || isVfnCvtSatFfqNext) -> padResult(fp32ToFp8, 8),
    isVfextVf2Next -> padResult(fp4ToFp8E4M3(srcLo8(3, 0)), 8),
    (!(isVfwCvtBf16Fp8Next || isVfnCvtBf16Fp8Next || isVfnCvtBf16SatFp8Next || isVfnCvtFfqNext || isVfnCvtSatFfqNext || isVfextVf2Next)) -> 0.U(width.W)
  ))
  val fflagsNext = Mux1H(Seq(
    isVfwCvtBf16Fp8Next -> 0.U(5.W),
    (isVfnCvtBf16Fp8Next || isVfnCvtBf16SatFp8Next) -> bf16ToFp8Fflags,
    (isVfnCvtFfqNext || isVfnCvtSatFfqNext) -> fp32ToFp8Fflags,
    isVfextVf2Next -> 0.U(5.W),
    (!(isVfwCvtBf16Fp8Next || isVfnCvtBf16Fp8Next || isVfnCvtBf16SatFp8Next || isVfnCvtFfqNext || isVfnCvtSatFfqNext || isVfextVf2Next)) -> 0.U(5.W)
  ))

  val fireReg = GatedValidRegNext(io.fire)
  val s1Result = RegEnable(resultNext, 0.U(width.W), io.fire)
  val s1Fflags = RegEnable(fflagsNext, 0.U(5.W), io.fire)

  io.result := RegEnable(s1Result, 0.U(width.W), fireReg)
  io.fflags := RegEnable(s1Fflags, 0.U(5.W), fireReg)
}
