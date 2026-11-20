package yunsuan.vector.cvt

/** Independent numerical oracle. Low formats are finite mathematical lattices;
  * conversion searches their adjacent values and applies the selected rounding
  * direction. The RTL's normalization, jam shifts and rounding units are unused.
  * All arithmetic and all halfway decisions use exact BigInt ratios.
  */
object LowpReference {
  case class Result(bits: BigInt, flags: Int)
  val I8 = 0; val U8 = 1; val E4M3 = 2; val E5M2 = 3; val E2M1 = 4
  val NX = 1; val UF = 2; val OF = 4; val DZ = 8; val NV = 16
  val names = Vector("i8", "u8", "e4m3", "e5m2", "e2m1")
  private val canonical32 = BigInt("7fc00000", 16)
  private val sign32 = BigInt(1) << 31

  case class Rational(numerator: BigInt, denominator: BigInt) extends Ordered[Rational] {
    require(denominator > 0)
    def compare(that: Rational): Int = (numerator * that.denominator).compare(that.numerator * denominator)
    def +(that: Rational): Rational = Rational(numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)
    def -(that: Rational): Rational = Rational(numerator * that.denominator - that.numerator * denominator,
      denominator * that.denominator)
    def half: Rational = Rational(numerator, denominator << 1)
    def timesPowerOfTwo(power: Int): Rational = if (power >= 0) Rational(numerator << power, denominator)
      else Rational(numerator, denominator << -power)
  }
  def dyadic(value: BigInt, power: Int): Rational = Rational(value, BigInt(1)).timesPowerOfTwo(power)

  private case class Number(negative: Boolean, magnitude: BigInt, power: Int,
      infinity: Boolean = false, nan: Boolean = false, signaling: Boolean = false) {
    def zero: Boolean = !nan && !infinity && magnitude == 0
    def finiteValue: Rational = dyadic(if (negative) -magnitude else magnitude, power)
    def absoluteValue: Rational = dyadic(magnitude, power)
  }
  private case class LowFormat(fraction: Int, bias: Int, maxCode: Int, infinity: Option[Int], nanCode: Int) {
    val signBit: Int = if (fraction == 1) 3 else 7
    val mask: Int = (1 << fraction) - 1
    def value(code: Int): Rational = {
      val exponent = code >> fraction
      dyadic(BigInt((code & mask) + (if (exponent == 0) 0 else 1 << fraction)),
        (if (exponent == 0) 1 - bias else exponent - bias) - fraction)
    }
    // The first excluded unbounded-exponent value defines the overflow boundary.
    val levels: Vector[Rational] = (0 to maxCode + 1).map(value).toVector
    val minimumNormal: Rational = dyadic(BigInt(1), 1 - bias)
  }
  private val formats = Map(
    E4M3 -> LowFormat(3, 7, 0x7e, None, 0x7f),
    E5M2 -> LowFormat(2, 15, 0x7b, Some(0x7c), 0x7e),
    E2M1 -> LowFormat(1, 1, 0x07, None, 0x07))

  private def f32(raw: BigInt): Number = {
    val exponent = ((raw >> 23) & 255).toInt
    val fraction = raw & 0x7fffff
    Number(raw.testBit(31), fraction + (if (exponent == 0) BigInt(0) else BigInt(1) << 23),
      (if (exponent == 0) -126 else exponent - 127) - 23,
      exponent == 255 && fraction == 0, exponent == 255 && fraction != 0,
      exponent == 255 && fraction != 0 && !fraction.testBit(22))
  }
  private def low(raw: BigInt, format: Int): Number = {
    val spec = formats(format)
    val code = (raw & ((1 << spec.signBit) - 1)).toInt
    val fraction = code & spec.mask
    val exponent = code >> spec.fraction
    val nan = if (format == E4M3) code == 0x7f else format == E5M2 && code > 0x7c
    Number(raw.testBit(spec.signBit), BigInt(fraction + (if (exponent == 0) 0 else 1 << spec.fraction)),
      (if (exponent == 0) 1 - spec.bias else exponent - spec.bias) - spec.fraction,
      spec.infinity.contains(code), nan, format == E5M2 && nan && (fraction & 2) == 0)
  }

  /** Mathematical finite values, including signed zero as a zero rational. */
  def finite32(raw: BigInt): Rational = {
    val n = f32(raw); require(!n.nan && !n.infinity); n.finiteValue
  }
  def finiteLow(raw: BigInt, format: Int): Rational = {
    if (format <= U8) Rational(BigInt(if (format == I8 && raw.testBit(7)) (raw & 255).toInt - 256 else (raw & 255).toInt), BigInt(1))
    else { val n = low(raw, format); require(!n.nan && !n.infinity); n.finiteValue }
  }
  def positiveLevels(format: Int, includeOverflow: Boolean = false): Vector[Rational] = {
    val spec = formats(format)
    if (includeOverflow) spec.levels else spec.levels.dropRight(1)
  }

  private def increment(floor: BigInt, remainder: BigInt, divisor: BigInt, negative: Boolean, rm: Int): Boolean = {
    require(rm >= 0 && rm <= 4)
    rm match {
      case 0 => (remainder << 1) > divisor || ((remainder << 1) == divisor && floor.testBit(0))
      case 1 => false
      case 2 => negative && remainder != 0
      case 3 => !negative && remainder != 0
      case 4 => (remainder << 1) >= divisor
    }
  }
  private def infinity32(negative: Boolean): BigInt = (if (negative) sign32 else BigInt(0)) | BigInt("7f800000", 16)
  private def overflow32(negative: Boolean, rm: Int): Result = {
    val inf = rm == 0 || rm == 4 || (rm == 2 && negative) || (rm == 3 && !negative)
    Result(if (inf) infinity32(negative) else (if (negative) sign32 else BigInt(0)) | BigInt("7f7fffff", 16), OF | NX)
  }

  /** Round an exact ratio once to IEEE F32, with tininess after rounding. */
  def roundRational32(numerator: BigInt, denominator: BigInt, rm: Int = 0,
      zeroNegative: Boolean = false): Result = {
    require(denominator > 0 && rm >= 0 && rm <= 4)
    if (numerator == 0) return Result(if (zeroNegative) sign32 else BigInt(0), 0)
    val negative = numerator < 0
    val magnitude = numerator.abs
    val estimate = magnitude.bitLength - denominator.bitLength
    val smaller = if (estimate >= 0) magnitude < (denominator << estimate)
      else (magnitude << -estimate) < denominator
    val exponent = estimate - (if (smaller) 1 else 0)
    val quantum = math.max(exponent - 23, -149)
    val units = Rational(magnitude, denominator).timesPowerOfTwo(-quantum)
    val (floor, remainder) = units.numerator /% units.denominator
    val rounded = floor + (if (increment(floor, remainder, units.denominator, negative, rm)) 1 else 0)
    val roundedExponent = if (rounded == 0) -150 else rounded.bitLength - 1 + quantum
    if (roundedExponent > 127) return overflow32(negative, rm)
    val subnormal = roundedExponent < -126
    // IEEE after-rounding tininess is a separate rounding to the full F32
    // precision with an unbounded exponent range. The reduced subnormal
    // lattice can carry a tiny value to minimum normal without clearing UF.
    // This is the IEEE F32 helper; low-format encodeFloat keeps its own policy.
    val unboundedQuantum = exponent - 23
    val unboundedUnits = Rational(magnitude, denominator).timesPowerOfTwo(-unboundedQuantum)
    val (unboundedFloor, unboundedRemainder) = unboundedUnits.numerator /% unboundedUnits.denominator
    val unboundedRounded = unboundedFloor +
      (if (increment(unboundedFloor, unboundedRemainder, unboundedUnits.denominator, negative, rm)) 1 else 0)
    val tiny = unboundedRounded.bitLength - 1 + unboundedQuantum < -126
    val fraction = if (subnormal) rounded else {
      val shift = roundedExponent - 23 - quantum
      val significand = if (shift >= 0) rounded >> shift else rounded << -shift
      (BigInt(roundedExponent + 127) << 23) | (significand & 0x7fffff)
    }
    val inexact = remainder != 0
    Result((if (negative) sign32 else BigInt(0)) | fraction,
      (if (inexact) NX else 0) | (if (tiny && inexact) UF else 0))
  }
  def round32(value: BigInt, power: Int, rm: Int = 0, zeroNegative: Boolean = false): Result = {
    val exact = dyadic(value, power)
    roundRational32(exact.numerator, exact.denominator, rm, zeroNegative)
  }
  def mul32(a: BigInt, b: BigInt, rm: Int = 0): Result = {
    val r = BF16Reference.mul32(a, b, rm); Result(r.bits, r.flags)
  }
  def add32(a: BigInt, b: BigInt, rm: Int = 0): Result = {
    val r = BF16Reference.add32(a, b, rm); Result(r.bits, r.flags)
  }
  def div32(a: BigInt, b: BigInt, rm: Int = 0): Result = {
    require(rm >= 0 && rm <= 4)
    val x = f32(a); val y = f32(b); val negative = x.negative ^ y.negative
    val invalid = x.signaling || y.signaling || (x.zero && y.zero) || (x.infinity && y.infinity)
    if (x.nan || y.nan || invalid) return Result(canonical32, if (invalid) NV else 0)
    if (x.infinity) return Result(infinity32(negative), 0)
    if (y.infinity || x.zero) return Result(if (negative) sign32 else BigInt(0), 0)
    if (y.zero) return Result(infinity32(negative), DZ)
    val ratio = Rational(if (negative) -x.magnitude else x.magnitude, y.magnitude)
      .timesPowerOfTwo(x.power - y.power)
    roundRational32(ratio.numerator, ratio.denominator, rm, negative)
  }

  private def encodeInteger(n: Number, format: Int, rm: Int): Result = {
    val lower = if (format == I8) -128 else 0
    val upper = if (format == I8) 127 else 255
    if (n.nan || n.infinity) return Result(BigInt(if (n.negative && !n.nan) lower & 255 else upper), NV)
    val exact = n.absoluteValue
    val (floor, remainder) = exact.numerator /% exact.denominator
    val magnitude = floor + (if (increment(floor, remainder, exact.denominator, n.negative, rm)) 1 else 0)
    val rounded = if (n.negative) -magnitude else magnitude
    if (rounded < lower || rounded > upper) Result(BigInt(if (rounded < lower) lower & 255 else upper), NV)
    else Result(rounded & 255, if (remainder != 0) NX else 0)
  }

  private def encodeFloat(n: Number, format: Int, factorNaN: Boolean, scalePower: Int, rm: Int): Result = {
    val spec = formats(format)
    val sign = if (n.negative) BigInt(1) << spec.signBit else BigInt(0)
    if (n.nan || factorNaN) return Result(BigInt(spec.nanCode), if (format == E2M1 || n.signaling) NV else 0)
    if (n.infinity) return Result(sign | BigInt(spec.infinity.getOrElse(spec.maxCode)), if (spec.infinity.isDefined) 0 else OF | NX)
    if (n.zero) return Result(sign, 0)
    val value = n.absoluteValue.timesPowerOfTwo(scalePower)
    val levels = spec.levels
    var left = 0; var right = levels.size
    while (left < right) {
      val mid = (left + right) / 2
      if (levels(mid) <= value) left = mid + 1 else right = mid
    }
    val lower = left - 1
    // Ratios intentionally keep their original powers of two; mathematical
    // equality must not use case-class numerator/denominator identity.
    val exact = levels(lower).compare(value) == 0
    val rounded = if (lower >= spec.maxCode + 1 || exact) lower else {
      val upper = lower + 1
      val chooseUpper = rm match {
        case 0 =>
          val cmp = (value - levels(lower)).compare(levels(upper) - value)
          cmp > 0 || (cmp == 0 && (lower & 1) != 0)
        case 1 => false
        case 2 => n.negative
        case 3 => !n.negative
        case 4 => (value - levels(lower)) >= (levels(upper) - value)
      }
      if (chooseUpper) upper else lower
    }
    if (rounded > spec.maxCode) {
      val toInfinity = rm == 0 || rm == 4 || (rm == 2 && n.negative) || (rm == 3 && !n.negative)
      Result(sign | BigInt(if (toInfinity) spec.infinity.getOrElse(spec.maxCode) else spec.maxCode), OF | NX)
    } else Result(sign | rounded,
      (if (!exact) NX else 0) | (if (!exact && levels(rounded) < spec.minimumNormal) UF else 0))
  }

  def convert(src: BigInt, format: Int, decode: Boolean, scaled: Boolean = false,
      factor: Int = 127, rm: Int = 0): Result = {
    require(format >= I8 && format <= E2M1 && rm >= 0 && rm <= 4 && factor >= 0 && factor <= 255)
    require(!scaled || format >= E4M3, "UE8M0 scaling is defined only for floating low formats")
    if (decode) {
      if (format <= U8) {
        val integer = (src & 255).toInt - (if (format == I8 && src.testBit(7)) 256 else 0)
        return round32(BigInt(integer), 0, rm)
      }
      val n = low(src, format)
      if (n.nan || (scaled && factor == 255))
        Result(canonical32, if (n.signaling || (scaled && factor == 255 && format == E2M1)) NV else 0)
      else if (n.infinity) Result(infinity32(n.negative), 0)
      else round32(if (n.negative) -n.magnitude else n.magnitude,
        n.power + (if (scaled) factor - 127 else 0), rm, n.negative)
    } else {
      val n = f32(src)
      if (format <= U8) encodeInteger(n, format, rm)
      else encodeFloat(n, format, scaled && factor == 255, if (scaled) 127 - factor else 0, rm)
    }
  }
}
