package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.VfcvtType
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.{CLZ, RoundingUnit, ShiftRightJam}

class FP32ToFloat8Format(
  expWidth: Int,
  fracWidth: Int,
  bias: Int,
  maxFiniteExp: Int,
  maxFiniteFrac: Int,
  hasInfinity: Boolean,
  canonicalNaN: Int
) extends Module {
  private val minNormalExp = 1 - bias

  val io = IO(new Bundle {
    val src = Input(UInt(32.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(8.W))
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

  val subnormalLzc = CLZ(frac)
  val normalizedSubnormal = (Cat(0.U(1.W), frac) << (subnormalLzc + 1.U))(23, 0)
  val normalizedSig = Mux(expIsZero, normalizedSubnormal, Cat(1.U(1.W), frac))
  val sourceExp = Wire(SInt(12.W))
  sourceExp := Mux(
    expIsZero,
    (-127).S(12.W) - Cat(0.U(7.W), subnormalLzc).asSInt,
    Cat(0.U(4.W), exp).asSInt - 127.S(12.W)
  )

  val useNormal = sourceExp >= minNormalExp.S(12.W)
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
  normalExp := sourceExp + bias.S(12.W) + normalCarry.asUInt.zext
  val roundedOverflows = normalExp > maxFiniteExp.S(12.W) ||
    (normalExp === maxFiniteExp.S(12.W) && normalFrac > maxFiniteFrac.U)
  val maxFiniteUnbiasedExp = maxFiniteExp - bias
  val maxFiniteSig = (((1 << fracWidth) | maxFiniteFrac) << (23 - fracWidth)).U(24.W)
  val exactMagnitudeOverflows = sourceExp > maxFiniteUnbiasedExp.S(12.W) ||
    (sourceExp === maxFiniteUnbiasedExp.S(12.W) && normalizedSig > maxFiniteSig)
  val normalPayload = sign ## normalExp.asUInt(expWidth - 1, 0) ## normalFrac

  val subShiftSigned = (23 + minNormalExp - fracWidth).S(12.W) - sourceExp
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

  val maxFinitePayload = sign ## maxFiniteExp.U(expWidth.W) ## maxFiniteFrac.U(fracWidth.W)
  val infinityPayload = sign ## ((1 << expWidth) - 1).U(expWidth.W) ## 0.U(fracWidth.W)
  val toInfinity = io.rm === RNE || io.rm === RMM ||
    (io.rm === RUP && !sign) || (io.rm === RDN && sign)
  val overflowPayload = if (hasInfinity) Mux(toInfinity, infinityPayload, maxFinitePayload) else maxFinitePayload
  val infinityResult = if (hasInfinity) infinityPayload else maxFinitePayload

  val finiteOverflow = !isZero && !isInf && !isNaN && useNormal &&
    (exactMagnitudeOverflows || roundedOverflows)
  val infinityOverflow = isInf && !hasInfinity.B
  val overflow = finiteOverflow || infinityOverflow
  val roundedInexact = Mux(useNormal, normalRounder.io.inexact, subRounder.io.inexact)
  val underflow = !isNaN && !isInf && !isZero && !useNormal &&
    subRounder.io.inexact && !subRoundsToNormal
  val inexact = overflow || (!isNaN && !isInf && !isZero && roundedInexact)

  io.result := Mux(
    isNaN,
    canonicalNaN.U(8.W),
    Mux(
      isInf,
      infinityResult,
      Mux(
        finiteOverflow,
        overflowPayload,
        Mux(isZero, sign ## 0.U(7.W), Mux(useNormal, normalPayload, subPayload))
      )
    )
  )
  io.fflags := Cat(isSNaN, false.B, overflow, underflow, inexact)
}

class FP32ToInt8 extends Module {
  val io = IO(new Bundle {
    val src = Input(UInt(32.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(8.W))
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

  val subnormalLzc = CLZ(frac)
  val normalizedSubnormal = (Cat(0.U(1.W), frac) << (subnormalLzc + 1.U))(23, 0)
  val normalizedSig = Mux(expIsZero, normalizedSubnormal, Cat(1.U(1.W), frac))
  val sourceExp = Wire(SInt(12.W))
  sourceExp := Mux(
    expIsZero,
    (-127).S(12.W) - Cat(0.U(7.W), subnormalLzc).asSInt,
    Cat(0.U(4.W), exp).asSInt - 127.S(12.W)
  )

  val shiftSigned = 23.S(12.W) - sourceExp
  val shift = Mux(
    shiftSigned > 31.S,
    31.U(5.W),
    Mux(shiftSigned < 1.S, 1.U(5.W), shiftSigned.asUInt(4, 0))
  )
  val (withRound, sticky) = ShiftRightJam(normalizedSig, shift - 1.U)
  val rounder = Module(new RoundingUnit(9))
  rounder.io.in := Cat(0.U(1.W), withRound(8, 1))
  rounder.io.roundIn := withRound(0)
  rounder.io.stickyIn := sticky
  rounder.io.signIn := sign
  rounder.io.rm := io.rm

  val roundedMagnitude = rounder.io.in +& rounder.io.r_up.asUInt
  val magnitudeLimit = Mux(sign, 128.U, 127.U)
  val exponentOverflow = sourceExp > 7.S
  val finiteInvalid = !isZero && !isInf && !isNaN &&
    (exponentOverflow || roundedMagnitude > magnitudeLimit)
  val invalid = isInf || isNaN || finiteInvalid
  val signedResult = Mux(sign, (~roundedMagnitude(7, 0)).asUInt + 1.U, roundedMagnitude(7, 0))

  io.result := Mux(
    invalid,
    Mux(sign && !isNaN, "h80".U, "h7f".U),
    Mux(isZero, 0.U, signedResult)
  )
  io.fflags := Cat(invalid, false.B, false.B, false.B, !invalid && !isZero && rounder.io.inexact)
}

class CVT_xx8 extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(32.W))
    val opType = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(8.W))
    val fflags = Output(UInt(5.W))
  })

  val int8 = Module(new FP32ToInt8)
  val e4m3 = Module(new FP32ToFloat8Format(4, 3, 7, 15, 6, hasInfinity = false, canonicalNaN = 0x7f))
  val e5m2 = Module(new FP32ToFloat8Format(5, 2, 15, 30, 3, hasInfinity = true, canonicalNaN = 0x7e))
  int8.io.src := io.src
  int8.io.rm := io.rm
  e4m3.io.src := io.src
  e4m3.io.rm := io.rm
  e5m2.io.src := io.src
  e5m2.io.rm := io.rm

  val resultNext = MuxLookup(io.opType, 0.U(8.W))(Seq(
    VfcvtType.vfncvtxx8_int8 -> int8.io.result,
    VfcvtType.vfncvtxx8_e4m3 -> e4m3.io.result,
    VfcvtType.vfncvtxx8_e5m2 -> e5m2.io.result
  ))
  val fflagsNext = MuxLookup(io.opType, 0.U(5.W))(Seq(
    VfcvtType.vfncvtxx8_int8 -> int8.io.fflags,
    VfcvtType.vfncvtxx8_e4m3 -> e4m3.io.fflags,
    VfcvtType.vfncvtxx8_e5m2 -> e5m2.io.fflags
  ))
  val fireReg = GatedValidRegNext(io.fire)
  val resultReg = RegEnable(resultNext, 0.U(8.W), io.fire)
  val fflagsReg = RegEnable(fflagsNext, 0.U(5.W), io.fire)

  io.result := RegEnable(resultReg, 0.U(8.W), fireReg)
  io.fflags := RegEnable(fflagsReg, 0.U(5.W), fireReg)
}
