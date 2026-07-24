package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.VectorConvert.util.{CLZ, RoundingUnit, ShiftRightJam}

class FP32ToMXFPFormat(
  expWidth: Int,
  fracWidth: Int,
  bias: Int,
  maxFiniteFrac: Int,
  hasNaN: Boolean
) extends Module {
  private val formatWidth = 1 + expWidth + fracWidth
  private val maxExpField = (1 << expWidth) - 1
  private val minNormalExp = 1 - bias

  val io = IO(new Bundle {
    val src = Input(UInt(32.W))
    val factor = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(formatWidth.W))
    val fflags = Output(UInt(5.W))
  })

  val sign = io.src(31)
  val exp = io.src(30, 23)
  val frac = io.src(22, 0)
  val expIsZero = !exp.orR
  val expIsOnes = exp.andR
  val fracNotZero = frac.orR
  val isZero = expIsZero && !fracNotZero
  val isInf = expIsOnes && !fracNotZero
  val isNaN = expIsOnes && fracNotZero
  val isSNaN = isNaN && !frac(22)
  val factorIsNaN = io.factor.andR

  val subnormalLzc = CLZ(frac)
  val normalizedSubnormal = (Cat(0.U(1.W), frac) << (subnormalLzc + 1.U))(23, 0)
  val normalizedSig = Mux(expIsZero, normalizedSubnormal, Cat(1.U(1.W), frac))

  // Dividing by UE8M0 only subtracts its unbiased exponent from the FP32 exponent.
  val sourceExp = Wire(SInt(12.W))
  sourceExp := Mux(
    expIsZero,
    (-127).S(12.W) - Cat(0.U(7.W), subnormalLzc).asSInt,
    Cat(0.U(4.W), exp).asSInt - 127.S(12.W)
  )
  val scaledExp = Wire(SInt(12.W))
  scaledExp := sourceExp - Cat(0.U(4.W), io.factor).asSInt + 127.S(12.W)
  val useNormal = scaledExp >= minNormalExp.S(12.W)

  val normalRounder = Module(new RoundingUnit(fracWidth + 1))
  normalRounder.io.in := normalizedSig(23, 23 - fracWidth)
  normalRounder.io.roundIn := normalizedSig(22 - fracWidth)
  normalRounder.io.stickyIn := normalizedSig(21 - fracWidth, 0).orR
  normalRounder.io.signIn := sign
  normalRounder.io.rm := io.rm

  val normalRounded = normalRounder.io.in +& normalRounder.io.r_up.asUInt
  val normalCarry = normalRounded(fracWidth + 1)
  val normalFrac = normalRounded(fracWidth - 1, 0)
  val normalExp = Wire(SInt(12.W))
  normalExp := scaledExp + bias.S(12.W) + normalCarry.asUInt.zext
  val normalOverflows = normalExp > maxExpField.S(12.W) ||
    (normalExp === maxExpField.S(12.W) && normalFrac > maxFiniteFrac.U)
  val normalPayload = sign ## normalExp.asUInt(expWidth - 1, 0) ## normalFrac

  val subShiftSigned = (23 + minNormalExp - fracWidth).S(12.W) - scaledExp
  val subShift = Mux(
    subShiftSigned > 31.S,
    31.U(5.W),
    Mux(subShiftSigned < 1.S, 1.U(5.W), subShiftSigned.asUInt(4, 0))
  )
  val (subWithRound, subSticky) = ShiftRightJam(normalizedSig, subShift - 1.U)
  val subRounder = Module(new RoundingUnit(fracWidth))
  subRounder.io.in := subWithRound(fracWidth, 1)
  subRounder.io.roundIn := subWithRound(0)
  subRounder.io.stickyIn := subSticky
  subRounder.io.signIn := sign
  subRounder.io.rm := io.rm

  val subRounded = subRounder.io.in +& subRounder.io.r_up.asUInt
  val subRoundsToNormal = subRounded(fracWidth)
  val subFrac = subRounded(fracWidth - 1, 0)
  val subPayload = sign ##
    Mux(subRoundsToNormal, 1.U(expWidth.W), 0.U(expWidth.W)) ##
    Mux(subRoundsToNormal, 0.U(fracWidth.W), subFrac)

  val maxFinitePayload = sign ## maxExpField.U(expWidth.W) ## maxFiniteFrac.U(fracWidth.W)
  val positiveMaxFinite = 0.U(1.W) ## maxExpField.U(expWidth.W) ## maxFiniteFrac.U(fracWidth.W)
  val nanPayload = if (hasNaN) {
    0.U(1.W) ## maxExpField.U(expWidth.W) ## ((1 << fracWidth) - 1).U(fracWidth.W)
  } else {
    positiveMaxFinite
  }

  val nanLike = isNaN || factorIsNaN
  val overflow = !nanLike && (isInf || (!isZero && useNormal && normalOverflows))
  val underflow = !nanLike && !isInf && !isZero && !useNormal &&
    subRounder.io.inexact && !subRoundsToNormal
  val inexact = overflow || (!nanLike && !isInf && !isZero && Mux(
    useNormal,
    normalRounder.io.inexact,
    subRounder.io.inexact
  ))
  val invalid = if (hasNaN) isSNaN else nanLike

  io.result := Mux(
    nanLike,
    nanPayload,
    Mux(
      overflow,
      maxFinitePayload,
      Mux(isZero, sign ## 0.U((formatWidth - 1).W), Mux(useNormal, normalPayload, subPayload))
    )
  )
  io.fflags := Cat(invalid, false.B, overflow, underflow, inexact)
}

class CVT_mxfp extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(32.W))
    val factor = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val isFp4 = Input(Bool())
    val result = Output(UInt(8.W))
    val fflags = Output(UInt(5.W))
  })

  val e4m3 = Module(new FP32ToMXFPFormat(4, 3, 7, 6, hasNaN = true))
  val e2m1 = Module(new FP32ToMXFPFormat(2, 1, 1, 1, hasNaN = false))
  for (converter <- Seq(e4m3, e2m1)) {
    converter.io.src := io.src
    converter.io.factor := io.factor
    converter.io.rm := io.rm
  }

  val resultNext = Mux(io.isFp4, Cat(0.U(4.W), e2m1.io.result), e4m3.io.result)
  val fflagsNext = Mux(io.isFp4, e2m1.io.fflags, e4m3.io.fflags)
  val fireReg = GatedValidRegNext(io.fire)
  val resultReg = RegEnable(resultNext, 0.U(8.W), io.fire)
  val fflagsReg = RegEnable(fflagsNext, 0.U(5.W), io.fire)

  io.result := RegEnable(resultReg, 0.U(8.W), fireReg)
  io.fflags := RegEnable(fflagsReg, 0.U(5.W), fireReg)
}
