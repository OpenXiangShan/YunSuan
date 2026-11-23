package yunsuan.vector.cvt

/** Exact dyadic oracle for IEEE binary formats and bounded integers.
  * No host floating point, DUT helpers, instruction registry, or intermediate
  * floating-point rounding is used. Flags are NV,DZ,OF,UF,NX, with tininess
  * detected after rounding. RM 6 implements the narrowing round-to-odd rule.
  */
object NativeConvertReference {
  sealed trait Format { def bits: Int }
  case class FloatFormat(exponentBits: Int, fractionBits: Int) extends Format {
    require(exponentBits >= 2 && fractionBits >= 1)
    val bits: Int = 1 + exponentBits + fractionBits
    val bias: Int = (1 << (exponentBits - 1)) - 1
    val minExponent: Int = 1 - bias
    val maxExponent: Int = bias
    val fractionMask: BigInt = (BigInt(1) << fractionBits) - 1
    val exponentMask: Int = (1 << exponentBits) - 1
    val canonicalNaN: BigInt = (BigInt(exponentMask) << fractionBits) | (BigInt(1) << (fractionBits - 1))
    def sign(negative: Boolean): BigInt = if (negative) BigInt(1) << (bits - 1) else BigInt(0)
    def infinity(negative: Boolean): BigInt = sign(negative) | (BigInt(exponentMask) << fractionBits)
    def maxFinite(negative: Boolean): BigInt = sign(negative) | (BigInt(exponentMask - 1) << fractionBits) | fractionMask
  }
  case class IntFormat(bits: Int, signed: Boolean) extends Format { require(bits > 0) }
  val F16 = FloatFormat(5, 10)
  val F32 = FloatFormat(8, 23)
  val F64 = FloatFormat(11, 52)
  val BF16 = FloatFormat(8, 7)
  case class Result(bits: BigInt, flags: Int)
  private val NX = 1
  private val UF = 2
  private val OF = 4
  private val NV = 16
  private def mask(bits: Int): BigInt = (BigInt(1) << bits) - 1
  private def checkRM(rm: Int): Unit = require((0 to 4).contains(rm) || rm == 6, s"unsupported RM=$rm")
  private case class Number(negative: Boolean, magnitude: BigInt, power: Int,
    infinity: Boolean, nan: Boolean, signaling: Boolean)

  private def decode(src: BigInt, format: FloatFormat): Number = {
    val raw = src & mask(format.bits)
    val exponent = ((raw >> format.fractionBits) & format.exponentMask).toInt
    val fraction = raw & format.fractionMask
    val special = exponent == format.exponentMask
    Number(raw.testBit(format.bits - 1),
      if (exponent == 0) fraction else fraction + (BigInt(1) << format.fractionBits),
      (if (exponent == 0) format.minExponent else exponent - format.bias) - format.fractionBits,
      special && fraction == 0, special && fraction != 0,
      special && fraction != 0 && !fraction.testBit(format.fractionBits - 1))
  }

  /** Project a nonnegative exact integer onto a coarser dyadic lattice. */
  private def rounded(magnitude: BigInt, discard: Int, negative: Boolean, rm: Int): (BigInt, Boolean) = {
    if (discard <= 0) return (magnitude << -discard, false)
    val divisor = BigInt(1) << discard
    val (q, r) = magnitude /% divisor
    if (rm == 6) return (if (r != 0) q.setBit(0) else q, r != 0)
    val increment = rm match {
      case 0 => r * 2 > divisor || (r * 2 == divisor && q.testBit(0))
      case 1 => false
      case 2 => negative && r != 0
      case 3 => !negative && r != 0
      case 4 => r * 2 >= divisor
    }
    (q + (if (increment) 1 else 0), r != 0)
  }

  /** Round value * 2^power once; zeroNegative preserves an input negative zero. */
  def round(value: BigInt, power: Int, target: FloatFormat, rm: Int = 0,
    zeroNegative: Boolean = false): Result = {
    checkRM(rm)
    if (value == 0) return Result(target.sign(zeroNegative), 0)
    val negative = value < 0
    val magnitude = value.abs
    val exponent = magnitude.bitLength - 1 + power
    val quantum = math.max(exponent, target.minExponent) - target.fractionBits
    val (significand, inexact) = rounded(magnitude, quantum - power, negative, rm)
    val roundedExponent = if (significand == 0) target.minExponent - 1 else significand.bitLength - 1 + quantum
    if (roundedExponent > target.maxExponent) {
      val toInfinity = rm == 0 || rm == 4 || (rm == 2 && negative) || (rm == 3 && !negative)
      return Result(if (toInfinity) target.infinity(negative) else target.maxFinite(negative), OF | NX)
    }
    val subnormal = roundedExponent < target.minExponent
    // IEEE after-rounding tininess uses target precision with an unbounded
    // exponent range, before the reduced subnormal lattice is applied. A
    // result can therefore round up to minimum normal and still signal UF.
    val unboundedQuantum = exponent - target.fractionBits
    val (unboundedSignificand, _) = rounded(magnitude, unboundedQuantum - power, negative, rm)
    val tiny = unboundedSignificand.bitLength - 1 + unboundedQuantum < target.minExponent
    val payload = if (subnormal) significand else {
      val shift = roundedExponent - target.fractionBits - quantum
      val normalized = if (shift >= 0) significand >> shift else significand << -shift
      (BigInt(roundedExponent + target.bias) << target.fractionBits) | (normalized & target.fractionMask)
    }
    Result(target.sign(negative) | payload, (if (inexact) NX else 0) | (if (tiny && inexact) UF else 0))
  }

  def convert(src: BigInt, source: Format, target: Format, rm: Int = 0): Result = {
    checkRM(rm)
    (source, target) match {
      case (s: IntFormat, t: FloatFormat) =>
        val raw = src & mask(s.bits)
        val value = if (s.signed && raw.testBit(s.bits - 1)) raw - (BigInt(1) << s.bits) else raw
        round(value, 0, t, rm)
      case (s: FloatFormat, t: FloatFormat) =>
        val n = decode(src, s)
        if (n.nan) Result(t.canonicalNaN, if (n.signaling) NV else 0)
        else if (n.infinity) Result(t.infinity(n.negative), 0)
        else round(if (n.negative) -n.magnitude else n.magnitude, n.power, t, rm, n.negative)
      case (s: FloatFormat, t: IntFormat) =>
        require(rm != 6, "round-to-odd is a floating-point narrowing operation")
        val n = decode(src, s)
        val lo = if (t.signed) -(BigInt(1) << (t.bits - 1)) else BigInt(0)
        val hi = if (t.signed) (BigInt(1) << (t.bits - 1)) - 1 else mask(t.bits)
        if (n.nan || n.infinity) Result((if (n.nan || !n.negative) hi else lo) & mask(t.bits), NV)
        else {
          val (magnitude, inexact) = rounded(n.magnitude, -n.power, n.negative, rm)
          val value = if (n.negative) -magnitude else magnitude
          if (value < lo || value > hi) Result(value.max(lo).min(hi) & mask(t.bits), NV)
          else Result(value & mask(t.bits), if (inexact) NX else 0)
        }
      case _ => throw new IllegalArgumentException(s"unsupported conversion $source -> $target")
    }
  }
}
