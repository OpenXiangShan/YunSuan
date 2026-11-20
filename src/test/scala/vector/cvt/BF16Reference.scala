package yunsuan.vector.cvt

/** IEEE reference based on exact signed integer times a power of two.
  *
  * No host floating point, DUT rounding helper, bit truncation conversion, or
  * intermediate product rounding is used. Formats are decoded into mathematical
  * dyadics, arithmetic is exact, then the destination lattice is rounded once.
  * Flag bits are RISC-V NV,DZ,OF,UF,NX; tininess is detected after rounding.
  */
object BF16Reference {
  case class Result(bits: BigInt, flags: Int)

  private case class Format(fraction: Int) {
    val signBit: Int = fraction + 8
    val fractionMask: BigInt = (BigInt(1) << fraction) - 1
    val canonicalNaN: BigInt = (BigInt(255) << fraction) | (BigInt(1) << (fraction - 1))
    def sign(negative: Boolean): BigInt = if (negative) BigInt(1) << signBit else BigInt(0)
    def infinity(negative: Boolean): BigInt = sign(negative) | (BigInt(255) << fraction)
    def maxFinite(negative: Boolean): BigInt = sign(negative) | (BigInt(254) << fraction) | fractionMask
  }
  private val bf16 = Format(7)
  private val fp32 = Format(23)
  private val NX = 1
  private val UF = 2
  private val OF = 4
  private val NV = 16

  private case class Number(negative: Boolean, magnitude: BigInt, power: Int,
      infinity: Boolean, nan: Boolean, signaling: Boolean) {
    def zero: Boolean = !infinity && !nan && magnitude == 0
    def signedMagnitude: BigInt = if (negative) -magnitude else magnitude
  }

  private def decode(raw: BigInt, format: Format): Number = {
    val negative = raw.testBit(format.signBit)
    val exponent = ((raw >> format.fraction) & 255).toInt
    val fraction = raw & format.fractionMask
    val special = exponent == 255
    Number(negative,
      if (exponent == 0) fraction else fraction + (BigInt(1) << format.fraction),
      (if (exponent == 0) -126 else exponent - 127) - format.fraction,
      special && fraction == 0, special && fraction != 0,
      special && fraction != 0 && !fraction.testBit(format.fraction - 1))
  }

  def isNaN16(raw: BigInt): Boolean = decode(raw, bf16).nan
  def isNaN32(raw: BigInt): Boolean = decode(raw, fp32).nan
  def isSignalingNaN16(raw: BigInt): Boolean = decode(raw, bf16).signaling
  def isSignalingNaN32(raw: BigInt): Boolean = decode(raw, fp32).signaling

  /** Round an exact finite dyadic onto a format's integer-significand lattice. */
  private def round(value: BigInt, power: Int, zeroNegative: Boolean, format: Format, rm: Int): Result = {
    require(rm >= 0 && rm <= 4, s"unsupported rounding mode $rm")
    if (value == 0) return Result(format.sign(zeroNegative), 0)
    val negative = value < 0
    val magnitude = value.abs
    val exactExponent = magnitude.bitLength - 1 + power
    // Subnormals share the fixed quantum 2^(emin-fraction).
    val quantum = math.max(exactExponent - format.fraction, -126 - format.fraction)
    val shift = quantum - power
    val divisor = if (shift > 0) BigInt(1) << shift else BigInt(1)
    val scaled = if (shift > 0) magnitude else magnitude << -shift
    val (floor, remainder) = scaled /% divisor
    val inexact = remainder != 0
    def incrementAt(q: BigInt, r: BigInt, d: BigInt): Boolean = rm match {
      case 0 => r * 2 > d || (r * 2 == d && q.testBit(0))
      case 1 => false
      case 2 => negative && r != 0
      case 3 => !negative && r != 0
      case 4 => r * 2 >= d
    }
    val rounded = floor + (if (incrementAt(floor, remainder, divisor)) 1 else 0)
    val roundedExponent = if (rounded == 0) -127 else rounded.bitLength - 1 + quantum
    if (roundedExponent > 127) {
      val toInfinity = rm == 0 || rm == 4 || (rm == 2 && negative) || (rm == 3 && !negative)
      return Result(if (toInfinity) format.infinity(negative) else format.maxFinite(negative), OF | NX)
    }
    val subnormal = roundedExponent < -126
    // IEEE after-rounding tininess uses full destination precision with no
    // exponent limit. The final subnormal lattice can round a tiny value up
    // to minimum normal; that carry does not by itself suppress underflow.
    val unboundedQuantum = exactExponent - format.fraction
    val unboundedShift = unboundedQuantum - power
    val unboundedDivisor = if (unboundedShift > 0) BigInt(1) << unboundedShift else BigInt(1)
    val unboundedScaled = if (unboundedShift > 0) magnitude else magnitude << -unboundedShift
    val (unboundedFloor, unboundedRemainder) = unboundedScaled /% unboundedDivisor
    val unboundedRounded = unboundedFloor +
      (if (incrementAt(unboundedFloor, unboundedRemainder, unboundedDivisor)) 1 else 0)
    val tiny = unboundedRounded.bitLength - 1 + unboundedQuantum < -126
    val payload = if (subnormal) rounded else {
      // Normalization here is exact, including a carry into a new binade.
      val normalizedShift = roundedExponent - format.fraction - quantum
      val significand = if (normalizedShift >= 0) rounded >> normalizedShift else rounded << -normalizedShift
      (BigInt(roundedExponent + 127) << format.fraction) | (significand & format.fractionMask)
    }
    Result(format.sign(negative) | payload, (if (inexact) NX else 0) | (if (inexact && tiny) UF else 0))
  }

  private def convert(raw: BigInt, source: Format, destination: Format, rm: Int): Result = {
    require(rm >= 0 && rm <= 4)
    val number = decode(raw, source)
    if (number.nan) Result(destination.canonicalNaN, if (number.signaling) NV else 0)
    else if (number.infinity) Result(destination.infinity(number.negative), 0)
    else round(number.signedMagnitude, number.power, number.negative, destination, rm)
  }

  def widen(a: BigInt): Result = convert(a, bf16, fp32, 0)
  def narrow(a: BigInt, rm: Int): Result = convert(a, fp32, bf16, rm)

  private def fused(aRaw: BigInt, bRaw: BigInt, cRaw: BigInt,
      multiplicandFormat: Format, rm: Int, destination: Format = fp32): Result = {
    require(rm >= 0 && rm <= 4)
    val a = decode(aRaw, multiplicandFormat)
    val b = decode(bRaw, multiplicandFormat)
    val c = decode(cRaw, fp32)
    val productNegative = a.negative ^ b.negative
    val invalidProduct = (a.infinity && b.zero) || (b.infinity && a.zero)
    val productInfinity = !a.nan && !b.nan && !invalidProduct && (a.infinity || b.infinity)
    val invalidSum = productInfinity && c.infinity && productNegative != c.negative
    val invalid = a.signaling || b.signaling || c.signaling || invalidProduct || invalidSum
    if (a.nan || b.nan || c.nan || invalid) return Result(destination.canonicalNaN, if (invalid) NV else 0)
    if (productInfinity) return Result(destination.infinity(productNegative), 0)
    if (c.infinity) return Result(destination.infinity(c.negative), 0)

    val product = a.signedMagnitude * b.signedMagnitude
    val productPower = a.power + b.power
    val commonPower = math.min(productPower, c.power)
    val sum = (product << (productPower - commonPower)) + (c.signedMagnitude << (c.power - commonPower))
    val sameSignZeros = product == 0 && c.zero && productNegative == c.negative
    val zeroNegative = if (sameSignZeros) productNegative else rm == 2
    round(sum, commonPower, zeroNegative, destination, rm)
  }

  /** BF16 * BF16 + FP32, with a single final FP32 rounding. */
  def fma(a: BigInt, b: BigInt, c: BigInt, rm: Int): Result = fused(a, b, c, bf16, rm)
  def fma32(a: BigInt, b: BigInt, c: BigInt, rm: Int): Result = fused(a, b, c, fp32, rm)
  /** Native BF16 addition, directly rounded from the exact sum. */
  def addBF16(a: BigInt, b: BigInt, rm: Int): Result =
    fused(a, BigInt("3f80", 16), (b & 65535) << 16, bf16, rm, bf16)
  def add32(a: BigInt, b: BigInt, rm: Int): Result = fma32(a, BigInt("3f800000", 16), b, rm)
  def mul32(a: BigInt, b: BigInt, rm: Int): Result = {
    val zero = if (a.testBit(31) ^ b.testBit(31)) BigInt(1) << 31 else BigInt(0)
    fma32(a, b, zero, rm)
  }
}
