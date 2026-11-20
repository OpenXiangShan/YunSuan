package yunsuan.vector.cvt

/** Independent exact-integer reference. No host floating point or RTL helpers. */
object Float32IntegerReference {
  case class Result(bits: BigInt, flags: Int)
  private val mask32 = (BigInt(1) << 32) - 1
  private val nx = 1
  private val nv = 16

  private def roundedMagnitude(magnitude: BigInt, discarded: Int, negative: Boolean, rm: Int): (BigInt, Boolean) = {
    require(rm >= 0 && rm <= 4)
    if (discarded <= 0) return (magnitude << -discarded, false)
    val divisor = BigInt(1) << discarded
    val (q, r) = magnitude /% divisor
    val increment = rm match {
      case 0 => r * 2 > divisor || (r * 2 == divisor && q.testBit(0))
      case 1 => false
      case 2 => negative && r != 0
      case 3 => !negative && r != 0
      case 4 => r * 2 >= divisor
    }
    (q + (if (increment) 1 else 0), r != 0)
  }

  def intToFloat(bits: BigInt, signed: Boolean, rm: Int): Result = {
    val raw = bits & mask32
    val value = if (signed && raw.testBit(31)) raw - (BigInt(1) << 32) else raw
    if (value == 0) return Result(0, 0)
    val negative = value < 0
    val magnitude = value.abs
    var exponent = magnitude.bitLength - 1
    val (rounded, inexact) = roundedMagnitude(magnitude, exponent - 23, negative, rm)
    val significand = if (rounded.bitLength > 24) { exponent += 1; rounded >> 1 } else rounded
    Result((if (negative) BigInt(1) << 31 else BigInt(0)) |
      (BigInt(exponent + 127) << 23) | (significand & ((BigInt(1) << 23) - 1)), if (inexact) nx else 0)
  }

  def floatToInt(bits: BigInt, signed: Boolean, rm: Int, rtz: Boolean = false): Result = {
    require(rm >= 0 && rm <= 4)
    val raw = bits & mask32
    val negative = raw.testBit(31)
    val exponent = ((raw >> 23) & 255).toInt
    val fraction = raw & ((BigInt(1) << 23) - 1)
    val lo = if (signed) -(BigInt(1) << 31) else BigInt(0)
    val hi = if (signed) (BigInt(1) << 31) - 1 else mask32
    if (exponent == 255) {
      val clipped = if (fraction != 0 || !negative) hi else lo
      return Result(clipped & mask32, nv)
    }
    val magnitude = if (exponent == 0) fraction else fraction + (BigInt(1) << 23)
    val power = (if (exponent == 0) 1 else exponent) - 127 - 23
    val (rounded, inexact) = roundedMagnitude(magnitude, -power, negative, if (rtz) 1 else rm)
    val result = if (negative) -rounded else rounded
    if (result < lo) Result(lo & mask32, nv)
    else if (result > hi) Result(hi & mask32, nv)
    else Result(result & mask32, if (inexact) nx else 0)
  }

  def i32ToF32(bits: BigInt, signed: Boolean, rm: Int): Result = intToFloat(bits, signed, rm)
  def f32ToI32(bits: BigInt, signed: Boolean, rm: Int, rtz: Boolean = false): Result = floatToInt(bits, signed, rm, rtz)
  def convert(src: BigInt, subop: Int, rm: Int): Result = subop match {
    case 0 => floatToInt(src, signed = false, rm = rm)
    case 1 => floatToInt(src, signed = true, rm = rm)
    case 2 => intToFloat(src, signed = false, rm = rm)
    case 3 => intToFloat(src, signed = true, rm = rm)
    case 6 => floatToInt(src, signed = false, rm = rm, rtz = true)
    case 7 => floatToInt(src, signed = true, rm = rm, rtz = true)
    case _ => throw new IllegalArgumentException(s"unsupported FP32/INT32 subop $subop")
  }
}
