package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.VfexpType
import yunsuan.util._
import yunsuan.vector.VectorConvert.RoundingModle._

/** EVU EXP2: four FP16 lanes or two FP32 lanes per cycle in a 64-bit slice. */
class VfExp2_EVU(width: Int = 64) extends Module {
  require(width == 64)
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(width.W))
    val opType = Input(UInt(8.W))
    val sew = Input(UInt(2.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(width.W))
    val fflags = Output(UInt(20.W))
    val valid = Output(Bool())
  })

  // Keep the reference range reduction and rounding bit-for-bit. Coefficients
  // come directly from VfExp2Tables; only the two EVU formats are instantiated.
  private val kWidth = 9
  private val specialKindWidth = 3
  private def spNone = 0.U(specialKindWidth.W)
  private def spSNaN = 1.U(specialKindWidth.W)
  private def spQNaN = 2.U(specialKindWidth.W)
  private def spSubnormal = 3.U(specialKindWidth.W)
  private def spPosInf = 4.U(specialKindWidth.W)
  private def spNegInf = 5.U(specialKindWidth.W)

  private def extendUInt(x: UInt, targetWidth: Int): UInt =
    if (x.getWidth >= targetWidth) x(targetWidth - 1, 0)
    else Cat(0.U((targetWidth - x.getWidth).W), x)

  private def shiftLeftTrunc(x: UInt, shamt: UInt, outWidth: Int): UInt = {
    require(outWidth > 0)
    val padded = extendUInt(x, outWidth)
    if (outWidth == 1) {
      Mux(shamt.orR, 0.U(1.W), padded)
    } else {
      val shiftWidth = log2Ceil(outWidth)
      val trimmedShamt = extendUInt(shamt, shiftWidth)
      val tooLarge =
        if (shamt.getWidth > shiftWidth)
          shamt(shamt.getWidth - 1, shiftWidth).orR
        else false.B
      Mux(
        tooLarge || trimmedShamt >= outWidth.U,
        0.U(outWidth.W),
        doShiftLeft(padded, trimmedShamt).asUInt
      )
    }
  }

  private def specialResultFromKind(fmt: VfExp2Format, kind: UInt): UInt = {
    MuxLookup(kind, 0.U(fmt.width.W))(
      Seq(
        spSNaN -> fmt.canonicalNaN.U(fmt.width.W),
        spQNaN -> fmt.canonicalNaN.U(fmt.width.W),
        spSubnormal -> Cat(0.U(1.W), fmt.bias.U(fmt.expWidth.W), 0.U(fmt.fracWidth.W)),
        spPosInf -> fmt.inf.U(fmt.width.W),
        spNegInf -> 0.U(fmt.width.W)
      )
    )
  }

  private def specialFlagsFromKind(kind: UInt): UInt = {
    Mux(
      kind === spSNaN,
      "b10000".U(5.W),
      Mux(kind === spSubnormal, "b00001".U(5.W), 0.U(5.W))
    )
  }

  // Cat(sig, guard) is shifted as one word. Only sticky information is needed
  // below it, including when the shift discards the entire word.
  private def shiftJam(word: UInt, amount: UInt, sticky: Bool): (UInt, Bool) = {
    var data = word
    var lost = sticky
    for (i <- 0 until amount.getWidth) {
      val n = 1 << i
      lost = lost || (amount(i) && data(math.min(n, data.getWidth) - 1, 0).orR)
      data = Mux(amount(i), data >> n, data)
    }
    (data, lost)
  }

  // Stage 1 S1 helper: compute per-format parameters for a given source lane
  private def computeS1ForFmt(
      src: UInt,
      fmt: VfExp2Format,
      coeffA: Vec[UInt]
  ): (UInt, Bool, SInt, UInt, UInt, UInt) = {
    val exp = src(fmt.fracWidth + fmt.expWidth - 1, fmt.fracWidth)
    val frac = src(fmt.fracWidth - 1, 0)
    val sign = src(fmt.width - 1)

    val expIsZero = exp === 0.U
    val expIsOnes = exp.andR
    val fracNotZero = frac.orR
    val isSubnormal = expIsZero && fracNotZero
    val isInf = expIsOnes && !fracNotZero
    val isNaN = expIsOnes && fracNotZero
    val isSNaN = isNaN && !frac(fmt.fracWidth - 1)

    val sig = Mux(expIsZero, 0.U(1.W) ## frac, 1.U(1.W) ## frac)

    // k only needs the saturated magnitude floor(abs(x)) in [0, 255].
    // Decode the <1 and >=256 regions from exp, and use a narrow right
    // shifter only for unbiased exponents 0..7. This removes the signed-p
    // arithmetic, left barrel and overflow cone from the S1 register inputs.
    val intMag = Wire(UInt(kWidth.W))
    val belowOne = expIsZero || exp < fmt.bias.U
    val intMagSaturates = exp >= (fmt.bias + kWidth - 1).U
    val intShiftWidth = log2Ceil(fmt.fracWidth + 1)
    val intRightShift =
      ((fmt.bias + fmt.fracWidth).U - exp)(intShiftWidth - 1, 0)
    val intFromSig = sig >> intRightShift
    when(belowOne) {
      intMag := 0.U
    }.elsewhen(intMagSaturates) {
      intMag := ((BigInt(1) << (kWidth - 1)) - 1).U
    }.otherwise {
      intMag := ZeroExt(intFromSig, kWidth)
    }

    // fracQ = fractional(x) * 2^tFracBits. Decode the three shift regions
    // directly from the raw exponent. This keeps the coefficient address off
    // the signed p -> fracScaleShift -> negate chain.
    val integerExpThreshold = fmt.bias + fmt.fracWidth
    val fracShiftThreshold = fmt.bias + fmt.fracWidth - fmt.tFracBits
    require(fracShiftThreshold > 0)
    val normalInteger = !expIsZero && exp >= integerExpThreshold.U
    val leftShiftRegion = !expIsZero && exp >= fracShiftThreshold.U
    val roundedRightShift = !leftShiftRegion
    val fracQPos = Wire(UInt(fmt.tFracBits.W))
    when(normalInteger) {
      fracQPos := 0.U
    }.elsewhen(leftShiftRegion) {
      fracQPos := shiftLeftTrunc(
        sig,
        exp - fracShiftThreshold.U,
        fmt.tFracBits
      )
    }.otherwise {
      val rightShift = Mux(
        expIsZero,
        (fracShiftThreshold - 1).U,
        fracShiftThreshold.U - exp
      )
      // Range reduction always uses RMM: the guard alone selects the increment.
      val qg = Cat(sig, 0.U(1.W)) >> rightShift
      fracQPos := extendUInt(qg(qg.getWidth - 1, 1) + qg(0), fmt.tFracBits)
    }

    // In the exact left-shift region, fracQPos is zero iff the input has no
    // fractional part. In the rounded right-shift region every nonzero sig
    // is fractional even when it rounds to zero.
    val hasFracExact =
      !normalInteger && (fracQPos.orR || (roundedRightShift && sig.orR))

    // For -m.f, floor is ~m when f != 0, otherwise it is ~m + 1.
    val signedMag = Mux(sign, ~intMag, intMag)
    val k = (signedMag + (sign && !hasFracExact)).asSInt

    // Split the negative modulo subtraction at the segment boundary. With a
    // nonzero local part, the negative segment is just bitwise-not(posSegIdx);
    // only the exact-boundary case needs the short increment carry chain.
    val posSegIdx =
      fracQPos(fmt.tFracBits - 1, fmt.tFracBits - fmt.segmentBits)
    val posLocalT = fracQPos(fmt.tFracBits - fmt.segmentBits - 1, 0)
    val negLocalTWide = (~posLocalT).asUInt +& 1.U
    val invertedSeg = (~posSegIdx).asUInt
    val incrementedSeg = invertedSeg +& 1.U
    val roundedNegativeWrap =
      roundedRightShift && sig.orR && !fracQPos.orR
    val negSegIdx = Mux(
      roundedNegativeWrap,
      Fill(fmt.segmentBits, 1.U(1.W)).asUInt,
      Mux(
        posLocalT.orR,
        invertedSeg,
        incrementedSeg(fmt.segmentBits - 1, 0)
      )
    )
    val negLocalT = Mux(
      roundedNegativeWrap,
      Fill(posLocalT.getWidth, 1.U(1.W)).asUInt,
      negLocalTWide(posLocalT.getWidth - 1, 0)
    )
    val segIdx = Mux(sign.asBool, negSegIdx, posSegIdx)
    val localT = Mux(sign.asBool, negLocalT, posLocalT)

    // At an exact segment boundary localT is zero, so A is multiplied by
    // zero and its index is irrelevant. roundedNegativeWrap also implies a
    // zero posSegIdx. Therefore the A address is simply sign-controlled bit
    // inversion; the exact segIdx above is retained for later B/C lookups.
    // Keep the sign-controlled inversion as an explicit narrow node before
    // Vec indexing. Older Verilator versions widen an XOR that is inlined into
    // a packed-array index; dontTouch preserves this width boundary.
    val coeffASegIdx = Mux(sign.asBool, (~posSegIdx).asUInt, posSegIdx)
    dontTouch(coeffASegIdx)

    val specialKind = WireDefault(spNone)
    when(isSNaN) {
      specialKind := spSNaN
    }.elsewhen(isNaN) {
      specialKind := spQNaN
    }.elsewhen(isSubnormal) {
      specialKind := spSubnormal
    }.elsewhen(isInf && !sign.asBool) {
      specialKind := spPosInf
    }.elsewhen(isInf && sign.asBool) {
      specialKind := spNegInf
    }

    // Only A is consumed in the following multiplier stage. B and C are
    // looked up later from the pipelined segment index, removing two ROM
    // selection cones from the input-to-S1 paths.
    val selectedCoeffA = coeffA(coeffASegIdx)

    (specialKind,
      hasFracExact,
      k,
      localT,
      segIdx,
      selectedCoeffA)
  }

  private val half = VfExp2Tables.fp16
  private val single = VfExp2Tables.fp32
  require(half.segmentBits == single.segmentBits && single.tFracBits >= half.tFracBits)
  // C + H*T/2^(t-1) is monotone in T for every segment. This proves
  // that no pre-round normalization or leading-bit register is needed.
  for (fmt <- Seq(half, single)) {
    val t = fmt.tFracBits
    val localMax = (BigInt(1) << (t - fmt.segmentBits)) - 1
    val one = BigInt(1) << (fmt.coeffFracBits + t - 1)
    require(fmt.coeffs.forall { case (a, b, c) =>
      val hMax = (((BigInt(a) * localMax) >> t) + b) >> 1
      val base = BigInt(c) << (t - 1)
      a >= 0 && b >= 0 && base >= one && base + hMax * localMax < 2 * one
    }, "EXP2 polynomial no longer stays in [1,2)")
  }
  private val issue = io.fire && io.opType === VfexpType.vfexp2
  private val enable = (1 until VfExp2Pipe.latency).scanLeft(issue)((prev, _) => GatedValidRegNext(prev))
  private def delay[T <: Data](x: T, stages: Int): T =
    enable.take(stages).foldLeft(x)((prev, en) => RegEnable(prev, en))
  private val singleMode = io.sew === 2.U
  private val mode = enable.take(5).scanLeft(singleMode)((prev, en) => RegEnable(prev, en))
  private val rm5 = delay(io.rm, 5)
  io.valid := enable.last

  private val halfA = VecInit(half.coeffs.map(_._1.U))
  private val singleA = VecInit(single.coeffs.map(_._1.U))
  private val halfResult = Wire(Vec(4, UInt(16.W)))
  private val halfFlags = Wire(Vec(4, UInt(5.W)))
  private val singleResult = Wire(Vec(2, UInt(32.W)))
  private val singleFlags = Wire(Vec(2, UInt(5.W)))

  for (lane <- 0 until 4) {
    val dual = lane % 2 == 0
    val wide = if (dual) single else half
    val t = wide.tFracBits
    val cf = wide.coeffFracBits
    val dt = t - half.tFracBits
    val dc = cf - half.coeffFracBits
    val p = wide.fracWidth
    val h = computeS1ForFmt(io.src(16 * lane + 15, 16 * lane), half, halfA)
    // Align both the local coordinate and the coefficients to one binary point.
    // Nearby coefficient bit patterns can then share the B/C table decoder.
    val (kind, hasFrac, k, local, addr, a) = if (dual) {
      val s = computeS1ForFmt(io.src(16 * lane + 31, 16 * lane), single, singleA)
      (Mux(singleMode, s._1, h._1), Mux(singleMode, s._2, h._2),
       Mux(singleMode, s._3, h._3), Mux(singleMode, s._4, h._4 << dt),
       Cat(singleMode, Mux(singleMode, s._5, h._5)), Mux(singleMode, s._6, h._6 << dc))
    } else h
    def table(field: ((Int, Int, Int)) => Int): Vec[UInt] = {
      val narrow = half.coeffs.map(row => (BigInt(field(row)) << dc).U)
      VecInit(if (dual) narrow ++ single.coeffs.map(row => field(row).U) else narrow)
    }

    val local1 = RegEnable(local, issue)
    val address1 = RegEnable(addr, issue)
    val a1 = RegEnable(a, issue)
    val productHi2 = RegEnable((a1 * local1) >> t, enable(1))
    val b2 = RegEnable(table(_._2)(address1), enable(1))
    val local2 = RegEnable(local1, enable(1))
    val address2 = RegEnable(address1, enable(1))
    val hValue = (productHi2 +& b2) >> 1
    // floor(2^dc*x) rounded down to a multiple of 2^dc = 2^dc*floor(x).
    // Preserve the original Horner truncation in half mode.
    val horner3 = RegEnable(if (dual)
      Cat(hValue(hValue.getWidth - 1, dc), Mux(mode(2), hValue(dc - 1, 0), 0.U))
      else hValue, enable(2))
    val local3 = RegEnable(local2, enable(2))
    val address3 = RegEnable(address2, enable(2))
    val product = horner3 * local3
    val upper4 = RegEnable(product >> (t - 1), enable(3))
    val tail4 = RegEnable(product(t - 2, 0).orR, enable(3))
    val c4 = RegEnable(table(_._3)(address3), enable(3))

    // C has no bits below this boundary, so the discarded product tail
    // cannot carry into the sum. From now on it contributes only to sticky.
    val sum4 = c4 + upper4
    def prefix(fmt: VfExp2Format): (UInt, Bool) = {
      val cut = cf - fmt.fracWidth - 1
      require(cut >= 0)
      val extra = if (cut == 0) tail4 else tail4 || sum4(cut - 1, 0).orR
      // Explicit leading one, fraction and guard in native precision.
      (Cat(1.U(1.W), sum4(cf - 1, cut)), extra)
    }
    val hh = prefix(half)
    val (prefix4, sticky4) = if (dual) {
      val ss = prefix(single)
      (Mux(mode(4), ss._1, hh._1), Mux(mode(4), ss._2, hh._2))
    } else hh
    val sig5 = RegEnable(prefix4, enable(4))
    val sticky5 = RegEnable(sticky4, enable(4))

    // Prepare exponent/denormal controls alongside the polynomial, ahead of S6.
    val k4 = delay(k, 4)
    def param(hv: Int, sv: Int): SInt =
      if (dual) Mux(mode(4), sv.S, hv.S) else hv.S
    val biased4 = k4 +& param(half.bias, single.bias)
    val sub4 = biased4 <= 0.S
    val over4 = k4 > param(half.bias, single.bias)
    val exp5 = RegEnable(Mux(sub4, 0.U, biased4.asUInt(wide.expWidth - 1, 0)), enable(4))
    val sw = log2Ceil(p + 3)
    val cap = (1 << sw) - 1
    // cap >= the precision + guard width: at saturation only sticky can survive.
    require(cap >= p + 2)
    val amount4 = Mux(!sub4, 0.U, Mux(biased4 <= (1 - cap).S, cap.U,
      (1.S - biased4).asUInt(sw - 1, 0)))
    val shift5 = RegEnable(amount4, enable(4))
    val over5 = RegEnable(over4, enable(4))
    val kind5 = delay(kind, 5)
    val hasFrac5 = delay(hasFrac, 5)
    val (qg, sticky) = shiftJam(sig5, shift5, sticky5)
    val guard = qg(0)
    val inexact = guard || sticky
    val increment = MuxLookup(rm5, guard && (sticky || qg(1)))(
      Seq(RTZ -> false.B, RDN -> false.B, RUP -> inexact, RMM -> guard))

    def encoded(fmt: VfExp2Format): UInt =
      Cat(exp5(fmt.expWidth - 1, 0), qg(fmt.fracWidth, 1))
    val raw = if (dual) Mux(mode(5), encoded(single), encoded(half)) else encoded(half)
    // Increment the packed positive magnitude: a mantissa carry increments
    // the exponent, including subnormal -> minimum normal.
    val rounded = raw + increment
    def finish(fmt: VfExp2Format): (UInt, UInt) = {
      val exp = rounded(fmt.width - 2, fmt.fracWidth)
      val overflow = over5 || exp.andR
      val sat = Mux(rm5 === RTZ || rm5 === RDN, fmt.maxFinite.U, fmt.inf.U)
      val finite = Mux(overflow, sat, rounded(fmt.width - 2, 0))
      val flags = Cat(false.B, false.B, overflow, !over5 && !exp.orR && inexact,
        hasFrac5 || inexact || overflow)
      (Mux(kind5 === spNone, finite, specialResultFromKind(fmt, kind5)),
       Mux(kind5 === spNone, flags, specialFlagsFromKind(kind5)))
    }
    val (hr, hf) = finish(half)
    halfResult(lane) := hr
    halfFlags(lane) := hf
    if (dual) {
      val (sr, sf) = finish(single)
      singleResult(lane / 2) := sr
      singleFlags(lane / 2) := sf
    }
  }
  io.result := RegEnable(Mux(mode.last, singleResult.asUInt, halfResult.asUInt), 0.U(64.W), enable.last)
  io.fflags := RegEnable(Mux(mode.last, singleFlags.asUInt.pad(20), halfFlags.asUInt), 0.U(20.W), enable.last)
}
