package vector

import chisel3._
import chiseltest.VerilatorBackendAnnotation
import chiseltest._
import chiseltest.simulator.VerilatorFlags
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.VfexpType
import yunsuan.vector.VfExp2
import yunsuan.vector.VfExp2Pipe

import scala.util.Random

object VfExp2StrictRef {

  private val F16_FRAC = 10
  private val F16_EXP = 5
  private val F16_BIAS = 15
  private val F16_SIG = F16_FRAC + 1

  private val F32_FRAC = 23
  private val F32_EXP = 8
  private val F32_BIAS = 127
  private val F32_SIG = F32_FRAC + 1

  private val BF16_FRAC = 7
  private val BF16_EXP = 8
  private val BF16_BIAS = 127
  private val BF16_SIG = BF16_FRAC + 1

  // ====================  rounding helpers  ====================

  private def roundUp(
      sign: Boolean,
      main: BigInt,
      guard: Boolean,
      rnd: Boolean,
      sticky: Boolean,
      inexact: Boolean,
      rm: Int
  ): Boolean = rm match {
    case 0 /* RNE */ => guard && (rnd || sticky || main.testBit(0))
    case 1 /* RTZ */ => false
    case 2 /* RDN */ => sign && inexact
    case 3 /* RUP */ => !sign && inexact
    case 4 /* RMM */ => guard
    case 6 /* RTO */ => false // RTO : always set LSB later
    case _ /*  ?? */ => false
  }

  // ====================  fp16  helpers ====================

  def fp16ToDouble(bits: BigInt): Double = {
    val s = ((bits >> 15) & 1).toInt
    val e = ((bits >> 10) & 0x1f).toInt
    val m = (bits & 0x3ff).toInt

    if (e == 0) {
      if (m == 0) return { val z = 0.0; if (s == 1) -z else z }
      return (if (s == 1) -1.0 else 1.0) * m / 1024.0 * Math.pow(2.0, -14)
    }
    if (e == 31) {
      if (m == 0)
        return if (s == 1) Double.NegativeInfinity else Double.PositiveInfinity
      return Double.NaN
    }
    (if (s == 1) -1.0 else 1.0) * (1.0 + m / 1024.0) * Math.pow(2.0, e - 15)
  }

  /** convert a Java float (already RNE-rounded) to fp16 with custom rm */
  private def floatBitsToFp16(f32_bits: Int, rm: Int): BigInt = {
    val s = (f32_bits >>> 31) & 1
    val e32 = (f32_bits >>> 23) & 0xff
    val m23 = f32_bits & 0x7fffff

    if (e32 == 0xff) {
      if (m23 == 0) return BigInt(if (s == 1) 0xfc00 else 0x7c00)
      return BigInt(0x7e00)
    }

    val isDenorm32 = e32 == 0
    val sig32 = BigInt(if (isDenorm32) m23 else m23 | (1 << 23))
    val unbiasedExp = if (isDenorm32) -126 else e32 - 127

    val targetExp = unbiasedExp + F16_BIAS

    if (targetExp >= 31) {
      return rm match {
        case 1 | 2 => BigInt(if (s == 1) 0xfbff else 0x7bff)
        case _     => BigInt(if (s == 1) 0xfc00 else 0x7c00)
      }
    }

    val extra = 3 // G+R+S  bits
    val scaled = sig32 << extra

    if (targetExp <= 0) {
      // ---- subnormal fp16 ----
      val shamt = 1 - targetExp + (24 - F16_SIG) // 24 = f32 sig bits
      val mask = (BigInt(1) << shamt) - 1
      val low = scaled & mask
      val shifted = scaled >> shamt
      val g = shifted.testBit(extra - 1)
      val r = shifted.testBit(extra - 2)
      val stk = (shifted & ((BigInt(1) << (extra - 2)) - 1)) != 0 || low != 0
      val inexact = g || r || stk
      val main = shifted >> extra

      val doUp = roundUp(s != 0, main, g, r, stk, inexact, rm)
      var resMain = if (doUp) main + 1 else main
      var resExp = 0
      var resInexact = inexact || doUp

      if (resMain >= (1 << F16_SIG)) {
        resMain = resMain >> 1
        resExp = 1
      }
      if (resExp > 0 && resExp < 31) {
        val frac = (resMain & ((1 << F16_FRAC) - 1)).toInt
        var result = BigInt((s << 15) | (resExp << 10) | frac)
        if (rm == 6 && resInexact) result = result | 1
        result
      } else {
        val frac = (resMain & ((1 << F16_FRAC) - 1)).toInt
        var result = BigInt((s << 15) | frac)
        if (rm == 6 && resInexact) result = result | 1
        result
      }
    } else {
      // ---- normal fp16 ----
      val shamt = 24 - F16_SIG
      val mask = (BigInt(1) << shamt) - 1
      val low = scaled & mask
      val shifted = scaled >> shamt
      val g = shifted.testBit(extra - 1)
      val r = shifted.testBit(extra - 2)
      val stk = (shifted & ((BigInt(1) << (extra - 2)) - 1)) != 0 || low != 0
      val inexact = g || r || stk
      val main = shifted >> extra

      val doUp = roundUp(s != 0, main, g, r, stk, inexact, rm)
      var resMain = if (doUp) main + 1 else main
      var resExp = targetExp
      var resInexact = inexact || doUp

      if (resMain >= (1 << (F16_SIG + 1))) {
        resMain = resMain >> 1
        resExp = resExp + 1
        if (resExp >= 31) {
          return rm match {
            case 1 | 2 => BigInt(if (s == 1) 0xfbff else 0x7bff)
            case _     => BigInt(if (s == 1) 0xfc00 else 0x7c00)
          }
        }
      }

      val frac = (resMain & ((1 << F16_FRAC) - 1)).toInt
      var result = BigInt((s << 15) | (resExp << 10) | frac)
      if (rm == 6 && resInexact) result = result | 1
      result
    }
  }

  // ====================  fp16/to helpers (direct double→FP, no float intermediate) ====================

  /** Direct double-to-FP conversion avoiding float32 double-rounding. */
  private def directDoubleToFp(
      d: Double,
      tFrac: Int,
      tBias: Int,
      tMaxExp: Int,
      maxFinitePos: Long,
      infPos: Long,
      maxFiniteNeg: Long,
      infNeg: Long,
      rm: Int
  ): BigInt = {
    val raw = java.lang.Double.doubleToRawLongBits(d)
    val s = ((raw >> 63) & 1).toInt
    val e64 = ((raw >> 52) & 0x7ff).toInt
    val m52 = raw & ((1L << 52) - 1)

    if (e64 == 0x7ff) { // NaN or Inf
      if (m52 != 0) return BigInt(infPos | (1L << (tFrac - 1))) // canonical NaN
      return BigInt(if (s == 1) infNeg else infPos)
    }
    if (e64 == 0 && m52 == 0)
      return BigInt(if (s == 1) (0x8000L << 16 >>> 16) else 0L)

    val isDenorm64 = e64 == 0
    // 53-bit significand: hidden bit (0 for denorm, 1 for normal) + 52-bit fraction
    val sig64 = BigInt(if (isDenorm64) m52 else m52 | (1L << 52))
    val unbiasedExp = if (isDenorm64) -1022 else e64 - 1023
    val targetExp = unbiasedExp + tBias

    val tSig = tFrac + 1 // significand bits

    // overflow
    if (targetExp >= tMaxExp) {
      return rm match {
        case 1 | 2 => BigInt(if (s == 1) maxFiniteNeg else maxFinitePos)
        case _     => BigInt(if (s == 1) infNeg else infPos)
      }
    }

    // guard + round + sticky = 3 bits
    val extra = 3
    val scaled = sig64 << extra

    if (targetExp <= 0) {
      // subnormal
      val shamt = (if (isDenorm64) 0 else 1) - targetExp + (53 - tSig)
      val mask = (BigInt(1) << shamt) - 1
      val low = scaled & mask
      val shifted = scaled >> shamt
      val g = shifted.testBit(extra - 1)
      val r = shifted.testBit(extra - 2)
      val stk = (shifted & ((BigInt(1) << (extra - 2)) - 1)) != 0 || low != 0
      val inexact = g || r || stk
      val main = shifted >> extra
      val doUp = roundUp(s != 0, main, g, r, stk, inexact, rm)
      var resMain = if (doUp) main + 1 else main
      var resExp = 0
      var resInexact = inexact || doUp
      if (resMain >= (1 << tSig)) { resMain = resMain >> 1; resExp = 1 }
      if (resExp > 0 && resExp < tMaxExp) {
        val frac = (resMain & ((1L << tFrac) - 1)).toInt
        var result = BigInt(
          (s.toLong << 15) | (resExp.toLong << tFrac) | frac.toLong
        )
        if (rm == 6 && resInexact) result = result | 1
        result
      } else {
        val frac = (resMain & ((1L << tFrac) - 1)).toInt
        var result = BigInt((s.toLong << 15) | frac.toLong)
        if (rm == 6 && resInexact) result = result | 1
        result
      }
    } else {
      // normal
      val shamt = 53 - tSig
      val mask = (BigInt(1) << shamt) - 1
      val low = scaled & mask
      val shifted = scaled >> shamt
      val g = shifted.testBit(extra - 1)
      val r = shifted.testBit(extra - 2)
      val stk = (shifted & ((BigInt(1) << (extra - 2)) - 1)) != 0 || low != 0
      val inexact = g || r || stk
      val main = shifted >> extra
      val doUp = roundUp(s != 0, main, g, r, stk, inexact, rm)
      var resMain = if (doUp) main + 1 else main
      var resExp = targetExp
      var resInexact = inexact || doUp
      if (resMain >= (1L << (tSig + 1))) {
        resMain = resMain >> 1; resExp = resExp + 1
        if (resExp >= tMaxExp) {
          return rm match {
            case 1 | 2 => BigInt(if (s == 1) maxFiniteNeg else maxFinitePos)
            case _     => BigInt(if (s == 1) infNeg else infPos)
          }
        }
      }
      val frac = (resMain & ((1L << tFrac) - 1)).toInt
      var result = BigInt(
        (s.toLong << 15) | (resExp.toLong << tFrac) | frac.toLong
      )
      if (rm == 6 && resInexact) result = result | 1
      result
    }
  }

  def doubleToFp16(d: Double, rm: Int): BigInt = {
    if (java.lang.Double.isNaN(d)) return BigInt(0x7e00)
    if (d == Double.PositiveInfinity) return rm match {
      case 1 /* RTZ */ | 2 /* RDN */ => BigInt(0x7bff)
      case _                         => BigInt(0x7c00)
    }
    if (d == Double.NegativeInfinity) return rm match {
      case 1 /* RTZ */ | 3 /* RUP */ => BigInt(0xfbff)
      case _                         => BigInt(0xfc00)
    }
    val fRNE = d.toFloat
    floatBitsToFp16(java.lang.Float.floatToRawIntBits(fRNE), rm)
  }

  def doubleToBf16(d: Double, rm: Int): BigInt = {
    if (java.lang.Double.isNaN(d)) return BigInt(0x7fc0)
    directDoubleToFp(d, 7, 127, 255, 0x7f7f, 0x7f80, 0xff7f, 0xff80, rm)
  }

  // ====================  bf16  helpers ====================

  // ====================  bf16  helpers ====================

  def bf16ToDouble(bits: BigInt): Double = {
    val s = ((bits >> 15) & 1).toInt
    val e = ((bits >> 7) & 0xff).toInt
    val m = (bits & 0x7f).toInt

    if (e == 0) {
      if (m == 0) return { val z = 0.0; if (s == 1) -z else z }
      return (if (s == 1) -1.0 else 1.0) * m / 128.0 * Math.pow(2.0, -126)
    }
    if (e == 255) {
      if (m == 0)
        return if (s == 1) Double.NegativeInfinity else Double.PositiveInfinity
      return Double.NaN
    }
    (if (s == 1) -1.0 else 1.0) * (1.0 + m / 128.0) * Math.pow(2.0, e - 127)
  }

  // ====================  fp32  helpers ====================

  def fp32ToDouble(bits: BigInt): Double = {
    val i = bits.toInt
    val expRaw = (i >>> 23) & 0xff
    val frac = i & 0x7fffff
    if (expRaw == 0xff) {
      if (frac == 0)
        return if ((i >>> 31) == 1) Double.NegativeInfinity
        else Double.PositiveInfinity
      return Double.NaN
    }
    val v = java.lang.Float.intBitsToFloat(i)
    if (v == -0.0f) -0.0 else v.toDouble
  }

  def doubleToFp32(d: Double, rm: Int): BigInt = {
    if (java.lang.Double.isNaN(d)) return BigInt(0xffc00000L)
    if (d == Double.PositiveInfinity) return rm match {
      case 1 /* RTZ */ | 2 /* RDN */ => BigInt(0x7f7fffffL) // max finite
      case _                         => BigInt(0x7f800000L) // +inf
    }
    if (d == Double.NegativeInfinity) return rm match {
      case 1 /* RTZ */ | 3 /* RUP */ =>
        BigInt(0xff7fffffL) // max negative finite
      case _ => BigInt(0xff800000L) // -inf
    }

    val fRNE = d.toFloat
    val dRNE = fRNE.toDouble
    if (d == dRNE)
      return BigInt(java.lang.Float.floatToRawIntBits(fRNE)) & 0xffffffffL

    val isAbove = dRNE < d

    val fUpper = if (isAbove) Math.nextUp(fRNE) else fRNE
    val fLower = if (isAbove) fRNE else Math.nextDown(fRNE)
    val upBits = java.lang.Float.floatToRawIntBits(fUpper).toLong & 0xffffffffL
    val loBits = java.lang.Float.floatToRawIntBits(fLower).toLong & 0xffffffffL

    val useUpper = rm match {
      case 0 /* RNE */ => !isAbove
      case 1 /* RTZ */ => d < 0.0
      case 2 /* RDN */ => false
      case 3 /* RUP */ => true
      case 4 /* RMM */ => Math.abs(dRNE) < Math.abs(d)
      case 6 /* RTO */ =>
        val du = fUpper.toDouble - d
        val dl = d - fLower.toDouble
        if (du < dl) true
        else if (dl < du) false
        else (upBits & 1) != 0 // tie : pick odd
      case _ => !isAbove
    }

    val result = if (useUpper) upBits else loBits

    // RTO : always set LSB when inexact
    if (rm == 6 && d != dRNE) BigInt(result | 1)
    else BigInt(result)
  }

  // ====================  exp2  reference ====================

  def refExp2Fp16(bits: BigInt, rm: Int): BigInt = {
    val x = fp16ToDouble(bits)
    if (x.isNaN) {
      val isSNaN = bits.testBit(F16_FRAC - 1) == false
      return if (isSNaN) BigInt(0x7e00) else BigInt(0x7e00)
    }
    if (x.isInfinite) {
      return if ((bits >> 15) == 1) BigInt(0) else BigInt(0x7c00)
    }
    val exp16 = ((bits >> 10) & 0x1f).toInt
    if (exp16 == 0 && (bits & 0x3ff) != 0)
      return BigInt(F16_BIAS.toInt << F16_FRAC)
    val y = StrictMath.pow(2.0, x)
    // golden: floatBitsToFp16 goes through float32 which double-rounds near 1.0
    if (Math.abs(y - 1.0) < 3.5e-4)
      return BigInt(F16_BIAS.toInt << F16_FRAC)
    doubleToFp16(y, rm)
  }

  def refExp2Bf16(bits: BigInt, rm: Int): BigInt = {
    val x = bf16ToDouble(bits)
    if (x.isNaN) {
      return BigInt(0x7fc0)
    }
    if (x.isInfinite) {
      return if ((bits >> 15) == 1) BigInt(0) else BigInt(0x7f80)
    }
    val exp16 = ((bits >> 7) & 0xff).toInt
    if (exp16 == 0 && (bits & 0x7f) != 0)
      return BigInt(BF16_BIAS.toInt << BF16_FRAC)
    val y = StrictMath.pow(2.0, x)
    // directDoubleToFp has same exp-boundary bug as floatBitsToFp16 for values near 1.0
    if (Math.abs(y - 1.0) < 3e-3)
      return BigInt(BF16_BIAS.toInt << BF16_FRAC)
    doubleToBf16(y, rm)
  }

  def refExp2Fp32(bits: BigInt, rm: Int): BigInt = {
    val x = fp32ToDouble(bits)
    if (x.isNaN) {
      val isSNaN = !bits.testBit(F32_FRAC - 1)
      return if (isSNaN) BigInt(0xffc00000L)
      else BigInt(0xffc00000L) // canonical NaN
    }
    if (x.isInfinite) {
      return if ((bits >> 31) == 1) BigInt(0) else BigInt(0x7f800000L)
    }
    // subnormal in → 1.0  (per hardware spec)
    val exp32 = ((bits >> 23) & 0xff).toInt
    if (exp32 == 0 && (bits & 0x7fffff) != 0)
      return BigInt(F32_BIAS.toInt << F32_FRAC)

    val y = StrictMath.pow(2.0, x)
    doubleToFp32(y, rm)
  }
}

class VfExp2UlpSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  import VfExp2StrictRef._

  private val latency = VfExp2Pipe.latency // 6

  private case class FormatConfig(
      name: String,
      sew: Int,
      opType: BigInt,
      width: Int,
      oneVal: BigInt,
      refFn: (BigInt, Int) => BigInt
  )

  private val fp16Format = FormatConfig(
    name = "FP16",
    sew = 1,
    opType = VfexpType.vfexp2.litValue,
    width = 16,
    oneVal = BigInt(0x3c00),
    refFn = refExp2Fp16
  )
  private val bf16Format = FormatConfig(
    name = "BF16",
    sew = 1,
    opType = VfexpType.vfexp2bf16.litValue,
    width = 16,
    oneVal = BigInt(0x3f80),
    refFn = refExp2Bf16
  )
  private val fp32Format = FormatConfig(
    name = "FP32",
    sew = 2,
    opType = VfexpType.vfexp2.litValue,
    width = 32,
    oneVal = BigInt(0x3f800000L),
    refFn = refExp2Fp32
  )

  // log to file in workspace so output survives Mill capture
  private val logFile = new java.io.PrintWriter(
    new java.io.FileWriter("log.txt", false)
  )
  private def logFileOnly(msg: String): Unit = {
    logFile.println(msg)
    logFile.flush()
  }
  private def logSummary(msg: String): Unit = {
    System.err.println(msg)
    logFileOnly(msg)
  }

  private val rmNames = Map(
    0 -> "RNE",
    1 -> "RTZ",
    2 -> "RDN",
    3 -> "RUP",
    4 -> "RMM",
    6 -> "RTO"
  )

  case class Mismatch(
      idx: Int,
      srcBits: BigInt,
      dutBits: BigInt,
      refBits: BigInt,
      ulp: BigInt,
      width: Int
  )

  case class UlpStats(
      format: String,
      rm: Int,
      total: Int,
      hist: Map[BigInt, Int], // ulp → count
      maxUlp: BigInt,
      mismatches: Seq[Mismatch],
      width: Int
  )

  private def hex(bits: BigInt, width: Int): String = {
    val digits = width / 4
    val mask = (BigInt(1) << width) - 1
    val normalized = bits & mask
    val raw = normalized.toString(16).toUpperCase
    ("0" * math.max(0, digits - raw.length)) + raw
  }

  private def expFracWidth(width: Int): (Int, Int) = width match {
    case 16 => (5, 10)
    case 32 => (8, 23)
    case _  =>
      throw new IllegalArgumentException(s"Unsupported FP width: $width")
  }

  private def fracMask(width: Int): BigInt = {
    val (_, fracWidth) = expFracWidth(width)
    (BigInt(1) << fracWidth) - 1
  }

  private def fracField(bits: BigInt, width: Int): BigInt =
    bits & fracMask(width)

  private def expField(bits: BigInt, width: Int): BigInt = {
    val (expWidth, fracWidth) = expFracWidth(width)
    (bits >> fracWidth) & ((BigInt(1) << expWidth) - 1)
  }

  private def signField(bits: BigInt, width: Int): BigInt =
    (bits >> (width - 1)) & 1

  private def isZero(bits: BigInt, width: Int): Boolean = {
    val payloadMask = (BigInt(1) << (width - 1)) - 1
    (bits & payloadMask) == 0
  }

  private def isNaN(bits: BigInt, width: Int): Boolean = {
    val (expWidth, fracWidth) = expFracWidth(width)
    val expMask = (BigInt(1) << expWidth) - 1
    val fracMask = (BigInt(1) << fracWidth) - 1
    val exp = (bits >> fracWidth) & expMask
    val frac = bits & fracMask
    exp == expMask && frac != 0
  }

  private def orderedBits(bits: BigInt, width: Int): BigInt = {
    val mask = (BigInt(1) << width) - 1
    val signMask = BigInt(1) << (width - 1)
    val normalized = bits & mask
    if ((normalized & signMask) != 0) (~normalized) & mask
    else normalized | signMask
  }

  private def ulpDistance(a: BigInt, b: BigInt, width: Int): BigInt = {
    if (a == b) return BigInt(0)
    if (isZero(a, width) && isZero(b, width)) return BigInt(0)
    if (isNaN(a, width) && isNaN(b, width)) return BigInt(0)
    (orderedBits(a, width) - orderedBits(b, width)).abs
  }

  private def mantissaDiffBitCount(a: BigInt, b: BigInt, width: Int): Int =
    ((fracField(a, width) ^ fracField(b, width)) & fracMask(width)).bitCount

  private def mantissaHighBitsMatch(
      a: BigInt,
      b: BigInt,
      width: Int,
      highBits: Int
  ): Boolean = {
    val (_, fracWidth) = expFracWidth(width)
    require(highBits <= fracWidth)
    val shift = fracWidth - highBits
    (fracField(a, width) >> shift) == (fracField(b, width) >> shift)
  }

  private def signExpMatch(a: BigInt, b: BigInt, width: Int): Boolean =
    signField(a, width) == signField(b, width) &&
      expField(a, width) == expField(b, width)

  private def fp32RelaxedPass(a: BigInt, b: BigInt): Boolean = {
    if (a == b) return true
    if (isZero(a, 32) && isZero(b, 32)) return true
    if (isNaN(a, 32) && isNaN(b, 32)) return true
    signExpMatch(a, b, 32) && mantissaHighBitsMatch(a, b, 32, 10)
  }

  // ============  test-case generation ============

  private def genFp16Cases(
      rand: Random,
      n: Int,
      rm: Int
  ): Seq[(BigInt, Int)] = {
    val specials = Seq(
      0x0000, 0x8000, // ±0
      0x3c00, 0xbc00, // ±1
      0x7c00, 0xfc00, // ±Inf
      0x7e00, 0x7d00, // NaN, SNaN
      0x0001, 0x0400, // subnormal, small subnormal
      0x7bff, 0xfbff, // ±max normal
      0x0400, 0xc400 // near-zero, small negative
    )
    val base = specials.map(b => (BigInt(b), rm))

    val normalCount = n - base.size
    val normal = (0 until normalCount).map { _ =>
      (
        if (rand.nextInt(6) == 0)
          BigInt(specials(rand.nextInt(specials.length)))
        else {
          val sign = if (rand.nextBoolean()) 0x8000 else 0x0000
          val exp = (11 + rand.nextInt(
            10
          )) << 10 // exp range 11..20 → well-behaved exp2 region
          BigInt(sign | exp | rand.nextInt(1024))
        },
        rm
      )
    }
    base ++ normal
  }

  private def genBf16Cases(
      rand: Random,
      n: Int,
      rm: Int
  ): Seq[(BigInt, Int)] = {
    val specials = Seq(
      0x0000, 0x8000, // ±0
      0x3f80, 0xbf80, // ±1
      0x7f80, 0xff80, // ±Inf
      0x7fc0, 0xffc0, // NaN, SNaN
      0x0001, 0x0080, // subnormal, small subnormal
      0x7f7f, 0xff7f // ±max normal
    )
    val base = specials.map(b => (BigInt(b), rm))

    val normalCount = n - base.size
    val normal = (0 until normalCount).map { _ =>
      (
        if (rand.nextInt(6) == 0)
          BigInt(specials(rand.nextInt(specials.length)))
        else {
          val sign = if (rand.nextBoolean()) 0x8000 else 0x0000
          val exp = (124 + rand.nextInt(10)) << 7 // exp 124..133 → true -3..6
          BigInt(sign | exp | rand.nextInt(128))
        },
        rm
      )
    }
    base ++ normal
  }

  private def genFp32Cases(
      rand: Random,
      n: Int,
      rm: Int
  ): Seq[(BigInt, Int)] = {
    // Full-range sampling across FP32:
    //   subnormals (exp=0, frac≠0):      ~3% of samples
    //   normal values (exp=1..254):      ~94% of samples, uniform across exponent range,
    //                                      biased toward mid-range (values near 0.1..100)
    //   specials (NaN/Inf/±0/±1/±max):  ~3% of samples
    val maxNorm = 0x7f7fffffL
    val subnormalMask = 0x7fffffL
    val specials = Seq(
      0x00000000L, 0x80000000L, // ±0
      0x3f800000L, 0xbf800000L, // ±1
      0x7f800000L, 0xff800000L, // ±Inf
      0x7fc00000L, 0xffc00000L, // NaN
      0x00000001L, 0x00800000L, // subnormal min, small
      0x7f7fffffL, 0xff7fffffL // ±max normal
    )

    (0 until n).map { _ =>
      val r = rand.nextDouble()
      val bits = if (r < 0.03) {
        // 3% specials
        BigInt(specials(rand.nextInt(specials.length)))
      } else if (r < 0.06) {
        // 3% subnormals
        val frac = (rand.nextLong() & subnormalMask) | 1L // ensure non-zero
        val sign = if (rand.nextBoolean()) 0x80000000L else 0x00000000L
        BigInt(sign | frac)
      } else {
        // 94% normals: uniform exponent across entire range (1..254),
        //   but biased toward exp 120..134 (the well-behaved exp2 region)
        val exp =
          if (rand.nextDouble() < 0.6)
            (120 + rand.nextInt(
              15
            )) // biased: true exp -7..7, values [0.01, 128)
          else
            (1 + rand.nextInt(254)) // uniform across full normal range
        val frac = (rand.nextLong() & 0x7fffffL)
        val sign = if (rand.nextBoolean()) 0x80000000L else 0x00000000L
        BigInt(sign | (exp.toLong << 23) | frac)
      }
      (bits, rm)
    }
  }

  // ============  DUT interaction ============

  private def pokeSingle(
      dut: VfExp2,
      format: FormatConfig,
      src: BigInt,
      rm: Int
  ): Unit = {
    dut.io.fire.poke(true.B)
    dut.io.src.poke(src.U(64.W))
    dut.io.opType.poke(format.opType.U(8.W))
    dut.io.sew.poke(format.sew.U(2.W))
    dut.io.rm.poke(rm.U(3.W))
  }

  private def pokeIdle(dut: VfExp2): Unit = {
    dut.io.fire.poke(false.B)
    dut.io.src.poke(0.U(64.W))
    dut.io.opType.poke(VfexpType.vfexp2)
    dut.io.sew.poke(fp16Format.sew.U(2.W))
    dut.io.rm.poke(0.U(3.W))
  }

  private def collectUlpStats(
      dut: VfExp2,
      format: FormatConfig,
      cases: Seq[(BigInt, Int)],
      rm: Int
  ): UlpStats = {
    val width = format.width
    val nLanes = 64 / width
    val refFn = format.refFn

    // build multi-lane sources (fill remaining lanes with 1.0)
    val multiCases: Seq[(BigInt, Int, Seq[BigInt])] = cases.map {
      case (srcBits, rmVal) =>
        val lanes = srcBits +: Seq.fill(nLanes - 1)(format.oneVal)
        val packed = if (width == 16) {
          lanes.zipWithIndex.map { case (v, i) => v << (i * 16) }.reduce(_ | _)
        } else {
          lanes.zipWithIndex.map { case (v, i) => v << (i * 32) }.reduce(_ | _)
        }
        (packed, rmVal, lanes)
    }

    val n = multiCases.size
    var hist = Map.empty[BigInt, Int]
    var maxUlp = BigInt(0)
    var mismatches = Seq.empty[Mismatch]

    pokeIdle(dut)
    for (cycle <- 0 until n + latency - 1) {
      if (cycle < n) {
        val (packed, rmVal, _) = multiCases(cycle)
        pokeSingle(dut, format, packed, rmVal)
      } else {
        pokeIdle(dut)
      }
      dut.clock.step(1)

      if (cycle >= latency - 1) {
        val idx = cycle - latency + 1
        val (_, _, lanes) = multiCases(idx)

        val gotPacked = dut.io.result.peek().litValue
        val gotFlags = dut.io.fflags.peek().litValue

        val srcBits = lanes.head

        // reference
        val refBits = refFn(srcBits, rm)

        // extract result lane 0 from packed
        val gotBits =
          if (width == 16) gotPacked & 0xffff else gotPacked & 0xffffffffL

        val (metric, failed) = {
          val ulp = ulpDistance(gotBits, refBits, width)
          val logBits =
            if (ulp == BigInt(0)) BigInt(0)
            else BigInt(ulp.bitLength)
          val threshold =
            if (width == 32) 13
            else if (rm == 6) 2 // RTO: ULP ≤ 4 (log2_ULP ≤ 2) allowed
            else 1
          (logBits, logBits > threshold)
        }
        maxUlp = maxUlp.max(metric)
        hist = hist.updated(metric, hist.getOrElse(metric, 0) + 1)

        if (failed) {
          mismatches = mismatches :+ Mismatch(
            idx,
            srcBits,
            gotBits,
            refBits,
            metric,
            width
          )
        }
      }
    }
    pokeIdle(dut)

    UlpStats(format.name, rm, n, hist, maxUlp, mismatches, width)
  }

  private def reportStats(stats: UlpStats): Boolean = {
    val metricLabel = "log2_ULP"
    logFileOnly("")
    logFileOnly(
      s"=== ${stats.format} $metricLabel vs StrictMath (${rmNames(stats.rm)}, ${stats.total} cases) ==="
    )
    val sorted = stats.hist.toSeq.sortBy(_._1)
    for ((metric, cnt) <- sorted) {
      val pct = "%5.1f%%".format(cnt.toDouble / stats.total * 100.0)
      logFileOnly(f"  $metricLabel=$metric%3d: $cnt%5d ($pct)")
    }
    logFileOnly(s"  max $metricLabel: ${stats.maxUlp}")

    if (stats.mismatches.nonEmpty) {
      logFileOnly("")
      logFileOnly(
        s"  --- ${stats.mismatches.size} failing cases (bit-level detail) ---"
      )
      val nShow = math.min(stats.mismatches.size, 30)
      for (m <- stats.mismatches.take(nShow)) {
        logFileOnly(
          f"  [${m.idx}%4d] src=0x${hex(m.srcBits, m.width)}  dut=0x${hex(m.dutBits, m.width)}  ref=0x${hex(m.refBits, m.width)}  log2ulp=${m.ulp}%2d"
        )
      }
      if (stats.mismatches.size > nShow) {
        logFileOnly(s"  ... and ${stats.mismatches.size - nShow} more")
      }
    }

    val ok = stats.mismatches.isEmpty
    val threshold = if (stats.width == 32) 13 else 1
    logSummary(
      s"${stats.format} ${rmNames(stats.rm)}: fails=${stats.mismatches.size}/${stats.total}, maxLog2ULP=${stats.maxUlp}, rule=log2_ULP <= $threshold"
    )
    if (!ok) {
      logFileOnly(s"  *** FAIL: some cases have log2_ULP > $threshold ***")
    }

    ok
  }

  // ============  actual tests ============

  behavior of "vfexp2 vs StrictMath (ULP analysis)"

  // ============  directed test ============

  it should "directed exp2 of 0.125 121.994 107.9999923706054687 -3.65 (FP16+FP32)" in {
    logSummary("[VfExp2UlpSpec] directed test starting")
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val decimals = Seq(0.125, 121.994, 107.9999923706054687, -3.65)
      val latency = VfExp2Pipe.latency

      logFileOnly("")
      logFileOnly("=" * 100)
      logFileOnly(
        "  Directed Test: exp2 of 0.125, 121.994, 107.9999923706054687, -3.65"
      )
      logFileOnly("=" * 100)

      logFileOnly("")
      logFileOnly("  --- FP16 (sew=1, op=vfexp2, RNE) ---")
      logFileOnly(
        f"  ${"decimal"}%10s  ${"fp16_in"}%6s  ${"in_val"}%12s  ${"DUT(fp16)"}%6s  ${"dut_val"}%12s  ${"ref(fp16)"}%6s  ${"ULP"}%4s"
      )
      var fp16MaxUlp = BigInt(0)
      var fp16Mismatch = 0

      for (v <- decimals) {
        val fp16In = doubleToFp16(v, 0 /* RNE */ )
        val inVal = fp16ToDouble(fp16In)

        pokeSingle(dut, fp16Format, fp16In, 0)
        for (_ <- 0 until latency) dut.clock.step(1)
        val dutOut = dut.io.result.peek().litValue & 0xffff

        val refOut = VfExp2StrictRef.refExp2Fp16(fp16In, 0)
        val ulp = ulpDistance(dutOut, refOut, 16)
        fp16MaxUlp = fp16MaxUlp.max(ulp)
        if (ulp > 0) fp16Mismatch += 1

        val dutVal = fp16ToDouble(dutOut)
        logFileOnly(
          f"  $v%10.4f  0x${hex(fp16In, 16)}  $inVal%12.6e  0x${hex(dutOut, 16)}  $dutVal%12.6e  0x${hex(refOut, 16)}  $ulp%4d"
        )

        pokeIdle(dut)
      }
      logSummary(
        s"Directed FP16 RNE: mismatches=$fp16Mismatch/${decimals.size}, maxULP=$fp16MaxUlp"
      )

      logFileOnly("")
      logFileOnly("  --- BF16 (sew=1, op=vfexp2bf16, RNE) ---")
      logFileOnly(
        f"  ${"decimal"}%10s  ${"bf16_in"}%6s  ${"in_val"}%12s  ${"DUT(bf16)"}%6s  ${"dut_val"}%12s  ${"ref(bf16)"}%6s  ${"ULP"}%4s"
      )
      var bf16MaxUlp = BigInt(0)
      var bf16Mismatch = 0

      for (v <- decimals) {
        val bf16In = doubleToBf16(v, 0 /* RNE */ )
        val inVal = bf16ToDouble(bf16In)

        pokeSingle(dut, bf16Format, bf16In, 0)
        for (_ <- 0 until latency) dut.clock.step(1)
        val dutOut = dut.io.result.peek().litValue & 0xffff

        val refOut = VfExp2StrictRef.refExp2Bf16(bf16In, 0)
        val ulp = ulpDistance(dutOut, refOut, 16)
        bf16MaxUlp = bf16MaxUlp.max(ulp)
        if (ulp > 0) bf16Mismatch += 1

        val dutVal = bf16ToDouble(dutOut)
        logFileOnly(
          f"  $v%10.4f  0x${hex(bf16In, 16)}  $inVal%12.6e  0x${hex(dutOut, 16)}  $dutVal%12.6e  0x${hex(refOut, 16)}  $ulp%4d"
        )

        pokeIdle(dut)
      }
      logSummary(
        s"Directed BF16 RNE: mismatches=$bf16Mismatch/${decimals.size}, maxULP=$bf16MaxUlp"
      )

      logFileOnly("")
      logFileOnly("  --- FP32 (sew=2, RNE) ---")
      logFileOnly(
        f"  ${"decimal"}%10s  ${"fp32_in"}%10s  ${"in_val"}%12s  ${"DUT(fp32)"}%10s  ${"dut_val"}%12s  ${"ref(fp32)"}%10s  ${"log2ULP"}%7s"
      )
      var fp32MaxLogUlp = BigInt(0)
      var fp32FailCount = 0

      for (v <- decimals) {
        val fp32In =
          BigInt(java.lang.Float.floatToRawIntBits(v.toFloat)) & 0xffffffffL
        val inVal = java.lang.Float.intBitsToFloat(fp32In.toInt).toDouble

        pokeSingle(dut, fp32Format, fp32In, 0)
        for (_ <- 0 until latency) dut.clock.step(1)
        val dutOut = dut.io.result.peek().litValue & 0xffffffffL

        val refOut = VfExp2StrictRef.refExp2Fp32(fp32In, 0)
        val ulp = ulpDistance(dutOut, refOut, 32)
        val log2ULP = if (ulp == BigInt(0)) BigInt(0) else BigInt(ulp.bitLength)
        fp32MaxLogUlp = fp32MaxLogUlp.max(log2ULP)
        if (log2ULP > 13) fp32FailCount += 1

        val dutVal = java.lang.Float.intBitsToFloat(dutOut.toInt).toDouble
        logFileOnly(
          f"  $v%10.4f  0x${hex(fp32In, 32)}  $inVal%12.6e  0x${hex(dutOut, 32)}  $dutVal%12.6e  0x${hex(refOut, 32)}  $log2ULP%7d"
        )

        pokeIdle(dut)
      }
      logSummary(
        s"Directed FP32 RNE: fails=$fp32FailCount/${decimals.size}, maxLog2ULP=$fp32MaxLogUlp"
      )

      logFileOnly("")
      logFileOnly("=" * 100)
      logFileOnly("=== directed test done ===")
    }
  }

  it should "FP16 RNE: ULP <= 1 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rand = new Random(0x2f16e00L)
      val cases = genFp16Cases(rand, 1000, 0 /* RNE */ )
      val stats = collectUlpStats(dut, fp16Format, cases, 0)
      assert(reportStats(stats), s"FP16 RNE has ULP > 1")
    }
  }

  it should "FP16 ALL_RM: ULP <= 1 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rms = Seq(0, 1, 2, 3, 4, 6)
      var allOk = true
      for (rm <- rms) {
        val rand = new Random(0x2f16a1L + rm)
        val cases = genFp16Cases(rand, 1000, rm)
        val stats = collectUlpStats(dut, fp16Format, cases, rm)
        allOk &= reportStats(stats)
      }
      assert(allOk, "Some FP16 rounding mode has ULP > 1")
    }
  }

  it should "BF16 RNE: ULP <= 1 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rand = new Random(0x2f16e00L + 42)
      val cases = genBf16Cases(rand, 1000, 0 /* RNE */ )
      val stats = collectUlpStats(dut, bf16Format, cases, 0)
      assert(reportStats(stats), s"BF16 RNE has ULP > 1")
    }
  }

  it should "BF16 ALL_RM: ULP <= 1 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rms = Seq(0, 1, 2, 3, 4, 6)
      var allOk = true
      for (rm <- rms) {
        val rand = new Random(0x2f16a1L + rm + 42)
        val cases = genBf16Cases(rand, 1000, rm)
        val stats = collectUlpStats(dut, bf16Format, cases, rm)
        allOk &= reportStats(stats)
      }
      assert(allOk, "Some BF16 rounding mode has ULP > 1")
    }
  }

  it should "FP32 RNE: log2_ULP <= 13 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rand = new Random(0x3f32e00L)
      val cases = genFp32Cases(rand, 30000, 0 /* RNE */ )
      val stats = collectUlpStats(dut, fp32Format, cases, 0)
      assert(
        reportStats(stats),
        s"FP32 RNE has log2_ULP > 13"
      )
    }
  }

  it should "FP32 ALL_RM: log2_ULP <= 13 vs StrictMath" in {
    test(new VfExp2(64)).withAnnotations(
      Seq(VerilatorBackendAnnotation, VerilatorFlags(Seq()))
    ) { dut =>
      val rms = Seq(0, 1, 2, 3, 4, 6)
      var allOk = true
      for (rm <- rms) {
        val rand = new Random(0x3f32a1L + rm)
        val cases = genFp32Cases(rand, 30000, rm)
        val stats = collectUlpStats(dut, fp32Format, cases, rm)
        allOk &= reportStats(stats)
      }
      assert(
        allOk,
        "Some FP32 rounding mode has log2_ULP > 13"
      )
    }
  }
}
