package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.VfcvtType
import yunsuan.vector.VectorConvert.CVT
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.RoundingUnit

class BF16Convert(width: Int = 64) extends CVT(width) {
  require(width >= 32, "CVT_bf16 needs at least 32 result bits")

  private def padResult(x: UInt, payloadWidth: Int, box: Bool): UInt = {
    require(width >= payloadWidth)
    val payload = x(payloadWidth - 1, 0)
    if (width == payloadWidth) {
      payload
    } else {
      Mux(
        box,
        Cat(Fill(width - payloadWidth, 1.U(1.W)), payload),
        Cat(0.U((width - payloadWidth).W), payload)
      )
    }
  }

  val fire = io.fire
  val src = io.src
  val rm = io.rm
  val boxScalarResult = io.isFpToVecInst

  val inIsFpNext = io.opType(7)
  val outIsFpNext = io.opType(6)
  val isFpFpNext = inIsFpNext && outIsFpNext
  val isVfnCvtBf16Next = io.opType === VfcvtType.vfncvtbf16_ffw
  val isVfwCvtBf16Next = io.opType === VfcvtType.vfwcvtbf16_ffv
  val isScalarF32ToBf16Next = io.opType === VfcvtType.fcvt_bf16_s
  val isScalarBf16ToF32Next = io.opType === VfcvtType.fcvt_s_bf16
  val isVectorF32ToBf16Next = (isFpFpNext && io.input1H(2) && io.output1H(1)) || isVfnCvtBf16Next
  val isVectorBf16ToF32Next = (isFpFpNext && io.input1H(1) && io.output1H(2)) || isVfwCvtBf16Next
  val isF32ToBf16Next = isScalarF32ToBf16Next || isVectorF32ToBf16Next
  val isBf16ToF32Next = isScalarBf16ToF32Next || isVectorBf16ToF32Next

  val canonicalBf16 = "h7fc0".U(16.W)
  val canonicalF32 = "h7fc00000".U(32.W)

  val f32BoxOkNext = if (width > 32) src(width - 1, 32).andR else true.B
  val bf16BoxOkNext = if (width > 16) src(width - 1, 16).andR else true.B
  val f32BoxedNaNNext = boxScalarResult && !f32BoxOkNext
  val bf16BoxedNaNNext = boxScalarResult && !bf16BoxOkNext

  /*
   * fcvt.bf16.s
   *
   * BF16 and single precision share the same sign and exponent widths. The
   * narrow path therefore keeps src[31:16] and rounds the discarded 16 bits.
   */
  val f32SrcNext = Mux(f32BoxedNaNNext, canonicalF32, src(31, 0))
  val f32SignNext = f32SrcNext(31)
  val f32ExpNext = f32SrcNext(30, 23)
  val f32FracNext = f32SrcNext(22, 0)
  val f32ExpIsOnesNext = f32ExpNext.andR
  val f32ExpIsZeroNext = !f32ExpNext.orR
  val f32FracNotZeroNext = f32FracNext.orR
  val f32IsNaNNext = f32ExpIsOnesNext && f32FracNotZeroNext
  val f32IsInfNext = f32ExpIsOnesNext && !f32FracNotZeroNext
  val f32IsSNaNNext = f32IsNaNNext && !f32FracNext(22) && !f32BoxedNaNNext

  val f32ToBf16Rounder = Module(new RoundingUnit(16))
  f32ToBf16Rounder.io.in := f32SrcNext(31, 16)
  f32ToBf16Rounder.io.roundIn := f32SrcNext(15)
  f32ToBf16Rounder.io.stickyIn := f32SrcNext(14, 0).orR
  f32ToBf16Rounder.io.signIn := f32SignNext
  f32ToBf16Rounder.io.rm := rm

  val roundedBf16Wide = f32SrcNext(31, 16) +& f32ToBf16Rounder.io.r_up.asUInt
  val roundedBf16 = roundedBf16Wide(15, 0)
  val f32ToBf16OfNext = !f32IsNaNNext && !f32IsInfNext && roundedBf16(14, 7).andR
  val f32ToBf16NxNext = !f32IsNaNNext && !f32IsInfNext && f32ToBf16Rounder.io.inexact
  // Tininess-after-rounding uses BF16 precision with an unbounded exponent,
  // before the final subnormal rounding. Only the top FP32 subnormal binade
  // can round into the normal range: retain frac[22:15], then round bit 14.
  // Its retained LSB is 1 when all eight bits are 1, so RNE and RMM agree.
  val tinyRoundUpNext =
    ((rm === RNE || rm === RMM) && f32SrcNext(14)) ||
      (((rm === RUP && !f32SignNext) || (rm === RDN && f32SignNext)) && f32SrcNext(14, 0).orR)
  val tinyRoundsNormalNext = f32SrcNext(22, 15).andR && tinyRoundUpNext
  val f32ToBf16UfNext =
    !f32IsNaNNext && !f32IsInfNext && f32ExpIsZeroNext &&
      !tinyRoundsNormalNext && f32ToBf16NxNext

  val rminNext = rm === RTZ || (f32SignNext && rm === RUP) || (!f32SignNext && rm === RDN)
  val bf16MaxFiniteNext = f32SignNext ## "hfe".U(8.W) ## "h7f".U(7.W)
  val bf16InfNext = f32SignNext ## "hff".U(8.W) ## 0.U(7.W)
  val f32ToBf16PayloadNext = Mux(
    f32IsNaNNext,
    canonicalBf16,
    Mux(
      f32ToBf16OfNext,
      Mux(rminNext || rm === RTO, bf16MaxFiniteNext, bf16InfNext),
      roundedBf16
    )
  )
  val f32ToBf16ResultNext = padResult(f32ToBf16PayloadNext, 16, boxScalarResult)
  val f32ToBf16FflagsNext = Cat(
    f32IsSNaNNext,
    false.B,
    f32ToBf16OfNext,
    f32ToBf16UfNext,
    f32ToBf16OfNext || f32ToBf16NxNext
  )

  /*
   * fcvt.s.bf16
   *
   * This is an exact widening conversion. Subnormal payloads are preserved by
   * appending sixteen low fraction zeros; NaNs are canonicalized like CVT64.
   */
  val bf16SrcNext = Mux(bf16BoxedNaNNext, canonicalBf16, src(15, 0))
  val bf16SignNext = bf16SrcNext(15)
  val bf16ExpNext = bf16SrcNext(14, 7)
  val bf16FracNext = bf16SrcNext(6, 0)
  val bf16ExpIsOnesNext = bf16ExpNext.andR
  val bf16FracNotZeroNext = bf16FracNext.orR
  val bf16IsNaNNext = bf16ExpIsOnesNext && bf16FracNotZeroNext
  val bf16IsSNaNNext = bf16IsNaNNext && !bf16FracNext(6) && !bf16BoxedNaNNext
  val bf16ToF32PayloadNext = Mux(
    bf16IsNaNNext,
    canonicalF32,
    bf16SignNext ## bf16ExpNext ## bf16FracNext ## 0.U(16.W)
  )
  val bf16ToF32ResultNext = padResult(bf16ToF32PayloadNext, 32, boxScalarResult)
  val bf16ToF32FflagsNext = Cat(bf16IsSNaNNext, false.B, false.B, false.B, false.B)

  val resultNext = Mux1H(
    Seq(
      isF32ToBf16Next -> f32ToBf16ResultNext,
      isBf16ToF32Next -> bf16ToF32ResultNext,
      (!isF32ToBf16Next && !isBf16ToF32Next) -> 0.U(width.W)
    )
  )
  val fflagsNext = Mux1H(
    Seq(
      isF32ToBf16Next -> f32ToBf16FflagsNext,
      isBf16ToF32Next -> bf16ToF32FflagsNext,
      (!isF32ToBf16Next && !isBf16ToF32Next) -> 0.U(5.W)
    )
  )

  io.result := RegEnable(resultNext, 0.U(width.W), fire)
  io.fflags := RegEnable(fflagsNext, 0.U(5.W), fire)
}
