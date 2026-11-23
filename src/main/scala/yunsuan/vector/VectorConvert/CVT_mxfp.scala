package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.{CLZ, RoundingUnit, ShiftRightJam}

class FP32ToMXFPFormat(
  expWidth: Int,
  fracWidth: Int,
  bias: Int,
  maxFiniteFrac: Int,
  hasNaN: Boolean,
  hasInfinity: Boolean = false,
  pipeline: Boolean = false
) extends Module {
  require(!hasInfinity || hasNaN)
  private val formatWidth = 1 + expWidth + fracWidth
  private val maxExpField = (1 << expWidth) - 1
  private val maxFiniteExp = if (hasInfinity) maxExpField - 1 else maxExpField
  private val minNormalExp = 1 - bias

  val io = IO(new Bundle {
    val fire = if (pipeline) Some(Input(Bool())) else None
    val src = Input(UInt(32.W))
    val factor = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(formatWidth.W))
    val fflags = Output(UInt(5.W))
  })

  val signNext = io.src(31)
  val exp = io.src(30, 23)
  val frac = io.src(22, 0)
  val expIsZero = !exp.orR
  val expIsOnes = exp.andR
  val fracNotZero = frac.orR
  val isZeroNext = expIsZero && !fracNotZero
  val isInfNext = expIsOnes && !fracNotZero
  val isNaNNext = expIsOnes && fracNotZero
  val isSNaNNext = isNaNNext && !frac(22)
  val factorIsNaNNext = io.factor.andR

  val subnormalLzc = CLZ(frac)
  val normalizedSubnormal = (Cat(0.U(1.W), frac) << (subnormalLzc + 1.U))(23, 0)
  val normalizedSigNext = Mux(expIsZero, normalizedSubnormal, Cat(1.U(1.W), frac))
  val sourceExp = Wire(SInt(12.W))
  sourceExp := Mux(
    expIsZero,
    (-127).S(12.W) - Cat(0.U(7.W), subnormalLzc).asSInt,
    Cat(0.U(4.W), exp).asSInt - 127.S(12.W)
  )
  val scaledExpNext = Wire(SInt(12.W))
  scaledExpNext := sourceExp - Cat(0.U(4.W), io.factor).asSInt + 127.S(12.W)

  // Optional real arithmetic boundary: prepared value and matching controls.
  def stage[T <: Data](value: T): T = if (pipeline) RegEnable(value, io.fire.get) else value
  val sign = stage(signNext)
  val normalizedSig = stage(normalizedSigNext)
  val scaledExp = stage(scaledExpNext)
  val isZero = stage(isZeroNext)
  val isInf = stage(isInfNext)
  val isNaN = stage(isNaNNext)
  val isSNaN = stage(isSNaNNext)
  val factorIsNaN = stage(factorIsNaNNext)
  val stageRm = stage(io.rm)

  val useNormal = scaledExp >= minNormalExp.S(12.W)

  val normalRounder = Module(new RoundingUnit(fracWidth + 1))
  normalRounder.io.in := normalizedSig(23, 23 - fracWidth)
  normalRounder.io.roundIn := normalizedSig(22 - fracWidth)
  normalRounder.io.stickyIn := normalizedSig(21 - fracWidth, 0).orR
  normalRounder.io.signIn := sign
  normalRounder.io.rm := stageRm
  val normalRounded = normalRounder.io.in +& normalRounder.io.r_up.asUInt
  val normalCarry = normalRounded(fracWidth + 1)
  val normalFrac = normalRounded(fracWidth - 1, 0)
  val normalExp = Wire(SInt(12.W))
  normalExp := scaledExp + bias.S(12.W) + normalCarry.asUInt.zext
  val normalOverflows = normalExp > maxFiniteExp.S(12.W) ||
    (normalExp === maxFiniteExp.S(12.W) && normalFrac > maxFiniteFrac.U)
  val normalPayload = sign ## normalExp.asUInt(expWidth - 1, 0) ## normalFrac

  val subShiftSigned = (23 + minNormalExp - fracWidth).S(12.W) - scaledExp
  val subShift = Mux(subShiftSigned > 31.S, 31.U(5.W),
    Mux(subShiftSigned < 1.S, 1.U(5.W), subShiftSigned.asUInt(4, 0)))
  val (subWithRound, subSticky) = ShiftRightJam(normalizedSig, subShift - 1.U)
  val subRounder = Module(new RoundingUnit(fracWidth))
  subRounder.io.in := subWithRound(fracWidth, 1)
  subRounder.io.roundIn := subWithRound(0)
  subRounder.io.stickyIn := subSticky
  subRounder.io.signIn := sign
  subRounder.io.rm := stageRm
  val subRounded = subRounder.io.in +& subRounder.io.r_up.asUInt
  val subRoundsToNormal = subRounded(fracWidth)
  val subPayload = sign ## Mux(subRoundsToNormal, 1.U(expWidth.W), 0.U(expWidth.W)) ##
    Mux(subRoundsToNormal, 0.U(fracWidth.W), subRounded(fracWidth - 1, 0))

  val maxFinitePayload = sign ## maxFiniteExp.U(expWidth.W) ## maxFiniteFrac.U(fracWidth.W)
  val positiveMaxFinite = 0.U(1.W) ## maxFiniteExp.U(expWidth.W) ## maxFiniteFrac.U(fracWidth.W)
  val nanFraction = if (hasInfinity) 1 << (fracWidth - 1) else (1 << fracWidth) - 1
  val nanPayload = if (hasNaN) 0.U(1.W) ## maxExpField.U(expWidth.W) ## nanFraction.U(fracWidth.W) else positiveMaxFinite
  val infinityPayload = sign ## maxExpField.U(expWidth.W) ## 0.U(fracWidth.W)
  val toInfinity = stageRm === RNE || stageRm === RMM ||
    (stageRm === RUP && !sign) || (stageRm === RDN && sign)
  val overflowPayload = if (hasInfinity) Mux(toInfinity, infinityPayload, maxFinitePayload) else maxFinitePayload
  val infinityResult = if (hasInfinity) infinityPayload else maxFinitePayload
  val nanLike = isNaN || factorIsNaN
  val finiteOverflow = !nanLike && !isInf && !isZero && useNormal && normalOverflows
  val overflow = finiteOverflow || (!nanLike && isInf && !hasInfinity.B)
  val underflow = !nanLike && !isInf && !isZero && !useNormal && subRounder.io.inexact && !subRoundsToNormal
  val inexact = overflow || (!nanLike && !isInf && !isZero && Mux(useNormal, normalRounder.io.inexact, subRounder.io.inexact))
  val invalid = if (hasNaN) isSNaN else nanLike

  io.result := Mux(nanLike, nanPayload,
    Mux(isInf, infinityResult,
      Mux(finiteOverflow, overflowPayload,
        Mux(isZero, sign ## 0.U((formatWidth - 1).W), Mux(useNormal, normalPayload, subPayload)))))
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
  val e4m3 = Module(new FP32ToMXFPFormat(4, 3, 7, 6, hasNaN = true, pipeline = true))
  val e2m1 = Module(new FP32ToMXFPFormat(2, 1, 1, 1, hasNaN = false, pipeline = true))
  for (converter <- Seq(e4m3, e2m1)) {
    converter.io.fire.get := io.fire
    converter.io.src := io.src
    converter.io.factor := io.factor
    converter.io.rm := io.rm
  }
  val isFp4 = RegEnable(io.isFp4, io.fire)
  val resultNext = Mux(isFp4, Cat(0.U(4.W), e2m1.io.result), e4m3.io.result)
  val fflagsNext = Mux(isFp4, e2m1.io.fflags, e4m3.io.fflags)
  val fireReg = GatedValidRegNext(io.fire)
  io.result := RegEnable(resultNext, 0.U(8.W), fireReg)
  io.fflags := RegEnable(fflagsNext, 0.U(5.W), fireReg)
}
