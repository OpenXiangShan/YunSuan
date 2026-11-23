package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.vector.VectorConvert.util.CLZ

/** Exact signed/unsigned integer expansion. Every integer of up to 24 bits
  * fits the F32 significand, including the most negative signed value.
  * Four-bit instantiations are numeric building blocks, without a host ISA ID.
  */
class IntegerToFP32(val width: Int = 8, pipeline: Boolean = false) extends Module {
  require(width >= 1 && width <= 24, "exact F32 integer decode supports widths 1..24")
  val io = IO(new Bundle {
    val fire = if (pipeline) Some(Input(Bool())) else None
    val src = Input(UInt(width.W))
    val signed = Input(Bool())
    val result = Output(UInt(32.W))
  })
  val signNext = io.signed && io.src(width - 1)
  val magnitudeNext = Mux(signNext, (~io.src).asUInt + 1.U, io.src)
  val lzcNext = CLZ(magnitudeNext)

  // Optional real arithmetic boundary: prepared value and matching controls.
  def stage[T <: Data](value: T): T = if (pipeline) RegEnable(value, io.fire.get) else value
  val sign = stage(signNext)
  val magnitude = stage(magnitudeNext)
  val lzc = stage(lzcNext)

  val normalized = (magnitude << lzc)(width - 1, 0)
  val significand = (normalized.pad(24) << (24 - width))(23, 0)
  val exp = (126 + width).U(8.W) - lzc
  io.result := Mux(!magnitude.orR, 0.U(32.W), sign ## exp ## significand(22, 0))
}

/** Decode a small float, multiply by its UE8M0 scale, then represent it as F32.
  * E4M3 has one NaN magnitude and silently canonicalizes it. E5M2 follows
  * IEEE Inf/NaN classification, including NV for signaling NaNs.
  */
class FloatToFP32(
  expWidth: Int,
  fracWidth: Int,
  bias: Int,
  hasNaN: Boolean,
  hasInfinity: Boolean = false,
  allowScale: Boolean = true,
  pipeline: Boolean = false
) extends Module {
  require(!hasInfinity || hasNaN)
  require(1 + expWidth + fracWidth <= 8)
  // Even the smallest subnormal times 2^-127 fits F32's exact subnormal grid.
  require(bias + fracWidth <= 23)
  private val formatWidth = 1 + expWidth + fracWidth
  private val maxFiniteExp = ((1 << expWidth) - (if (hasInfinity) 2 else 1)) - bias
  require(allowScale || (1 - bias - fracWidth >= -149 && maxFiniteExp <= 127),
    "raw decoder values must fit F32 without rounding or overflow")

  val io = IO(new Bundle {
    val fire = if (pipeline) Some(Input(Bool())) else None
    val src = Input(UInt(formatWidth.W))
    val factor = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
  })

  val signNext = io.src(formatWidth - 1)
  val exp = io.src(formatWidth - 2, fracWidth)
  val frac = io.src(fracWidth - 1, 0)
  val expIsZero = !exp.orR
  val isZeroNext = expIsZero && !frac.orR
  val isNaN = if (!hasNaN) false.B
    else if (hasInfinity) exp.andR && frac.orR
    else exp.andR && frac.andR
  val isInfNext = if (hasInfinity) exp.andR && !frac.orR else false.B
  val isSNaNNext = if (hasInfinity) isNaN && !frac(fracWidth - 1) else false.B
  val factorIsNaNNext = if (allowScale) io.factor.andR else false.B
  val nanLikeNext = isNaN || factorIsNaNNext

  val significand = Cat(!expIsZero, frac)
  val lzc = CLZ(significand)
  val normalizedNext = (significand << lzc)(fracWidth, 0)
  val sourceExp = Wire(SInt(12.W))
  sourceExp := Mux(expIsZero, (1 - bias).S(12.W), exp.zext - bias.S(12.W)) - lzc.zext
  val scaledExpNext = Wire(SInt(12.W))
  scaledExpNext := (if (allowScale) sourceExp + io.factor.zext - 127.S(12.W) else sourceExp)

  // Optional real arithmetic boundary: prepared value and matching controls.
  def stage[T <: Data](value: T): T = if (pipeline) RegEnable(value, io.fire.get) else value
  val sign = stage(signNext)
  val normalized = stage(normalizedNext)
  val scaledExp = stage(scaledExpNext)
  val isZero = stage(isZeroNext)
  val isInf = stage(isInfNext)
  val isSNaN = stage(isSNaNNext)
  val factorIsNaN = stage(factorIsNaNNext)
  val nanLike = stage(nanLikeNext)
  val stageRm = stage(io.rm)

  val encodedExp = Wire(SInt(12.W))
  encodedExp := scaledExp + 127.S(12.W)
  val fullSignificand = normalized ## 0.U((23 - fracWidth).W)
  val finiteResult = if (allowScale || 1 - bias - fracWidth < -126) {
    val subShift = Mux(encodedExp < 1.S,
      (1.S(12.W) - encodedExp).asUInt(4, 0), 0.U(5.W))
    val subSignificand = fullSignificand >> subShift
    Mux(encodedExp >= 1.S,
      sign ## encodedExp.asUInt(7, 0) ## fullSignificand(22, 0),
      sign ## 0.U(8.W) ## subSignificand(22, 0))
  } else sign ## encodedExp.asUInt(7, 0) ## fullSignificand(22, 0)

  // All finite in-range values are exact: E5M2's minimum scaled value is
  // 2^-16 * 2^-127 = 2^-143. Only overflow needs an RM-dependent choice.
  val overflow = if (allowScale) !nanLike && !isInf && !isZero && scaledExp > 127.S else false.B
  val toInfinity = stageRm === RNE || stageRm === RMM ||
    (stageRm === RUP && !sign) || (stageRm === RDN && sign)
  val infinity = sign ## "h7f800000".U(31.W)
  val maxFinite = sign ## "h7f7fffff".U(31.W)
  val overflowResult = Mux(toInfinity, infinity, maxFinite)
  val invalid = if (hasNaN) isSNaN else factorIsNaN

  io.result := Mux(nanLike, "h7fc00000".U(32.W),
    Mux(isInf, infinity,
      Mux(overflow, overflowResult,
        Mux(isZero, sign ## 0.U(31.W), finiteResult))))
  io.fflags := Cat(invalid, false.B, overflow, false.B, overflow)
}
