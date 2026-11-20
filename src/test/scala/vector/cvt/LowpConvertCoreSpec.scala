package yunsuan.vector.cvt

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import _root_.yunsuan.vector.VectorConvert.{LowpConvert, CVT_xx8, CVT_mxfp}
import _root_.yunsuan.VfcvtType

class LowpReferenceSpec extends AnyFlatSpec {
  behavior of "Independent exact-rational low-precision reference"

  it should "match hand-derived codes, ties, finite overflow, NaNs, integers and F32 arithmetic" in {
    import LowpReference._
    def h(s: String): BigInt = BigInt(s, 16)
    var comparisons = 0
    def check(actual: Result, bits: String, flags: Int)(implicit position: org.scalactic.source.Position): Unit = {
      assert(actual == Result(h(bits), flags),
        s"hand-input=${position.fileName}:${position.lineNumber} actual=$actual expected=0x$bits flags=$flags")
      comparisons += 1
    }
    Seq((E4M3, "01", "3b000000"), (E4M3, "08", "3c800000"), (E4M3, "7e", "43e00000"),
      (E5M2, "01", "37800000"), (E5M2, "04", "38800000"), (E5M2, "7b", "47600000"),
      (E5M2, "7c", "7f800000"), (E5M2, "fc", "ff800000"),
      (I8, "80", "c3000000"), (I8, "7f", "42fe0000"), (U8, "ff", "437f0000")).foreach {
      case (f, a, b) => check(convert(h(a), f, decode = true), b, 0)
    }
    val fp4 = Seq("00000000", "3f000000", "3f800000", "3fc00000", "40000000", "40400000", "40800000", "40c00000")
    // Explicit full E2M1 table, including the preserved sign of zero.
    for (code <- 0 until 16) check(convert(BigInt(code), E2M1, decode = true),
      ((h(fp4(code & 7)) | (if ((code & 8) != 0) BigInt(1) << 31 else BigInt(0)))).toString(16), 0)
    check(convert(h("ff"), E4M3, decode = true), "7fc00000", 0)
    check(convert(h("7d"), E5M2, decode = true), "7fc00000", NV)
    check(convert(h("fe"), E5M2, decode = true), "7fc00000", 0)
    for (rm <- 0 to 4) {
      check(convert(h("3f880000"), E4M3, decode = false, rm = rm),
        Seq("38", "38", "38", "39", "39")(rm), NX)
      check(convert(h("bf880000"), E4M3, decode = false, rm = rm),
        Seq("b8", "b8", "b9", "b8", "b9")(rm), NX)
      check(convert(h("3c700000"), E4M3, decode = false, rm = rm),
        Seq("08", "07", "07", "08", "08")(rm), Seq(1, 3, 3, 1, 1)(rm))
      // 464 is halfway from E4M3 448 (even) to the excluded 480 level.
      check(convert(h("43e80000"), E4M3, decode = false, rm = rm), "7e", Seq(1, 1, 1, 5, 5)(rm))
      check(convert(h("43f00000"), E4M3, decode = false, rm = rm), "7e", OF | NX)
      // 61440 is halfway from E5M2 57344 (odd) to 65536.
      check(convert(h("47700000"), E5M2, decode = false, rm = rm),
        Seq("7c", "7b", "7b", "7c", "7c")(rm), Seq(5, 1, 1, 5, 5)(rm))
      check(convert(h("40e00000"), E2M1, decode = false, rm = rm), "7", Seq(5, 1, 1, 5, 5)(rm))
      check(convert(h("3e800000"), E2M1, decode = false, rm = rm),
        Seq("0", "0", "0", "1", "1")(rm), UF | NX)
      check(convert(h("7f800000"), E4M3, decode = false, rm = rm), "7e", OF | NX)
      check(convert(h("ff800000"), E2M1, decode = false, rm = rm), "f", OF | NX)
      check(convert(h("7f800000"), E5M2, decode = false, rm = rm), "7c", 0)
      check(convert(h("7f800001"), E4M3, decode = false, rm = rm), "7f", NV)
      check(convert(h("7fc00000"), E5M2, decode = false, rm = rm), "7e", 0)
      check(convert(h("7fc00000"), E2M1, decode = false, rm = rm), "7", NV)
      check(convert(h("42ff0000"), I8, decode = false, rm = rm), "7f", Seq(16, 1, 1, 16, 16)(rm))
      check(convert(h("c3008000"), I8, decode = false, rm = rm), "80", Seq(1, 1, 16, 1, 16)(rm))
      check(convert(h("be800000"), U8, decode = false, rm = rm), "0", Seq(1, 1, 16, 1, 1)(rm))
      check(convert(h("437f8000"), U8, decode = false, rm = rm), "ff", Seq(16, 1, 1, 16, 16)(rm))
      check(convert(h("7fc00000"), I8, decode = false, rm = rm), "7f", NV)
      check(convert(h("ffc00000"), U8, decode = false, rm = rm), "ff", NV)
      check(convert(h("ff800000"), U8, decode = false, rm = rm), "0", NV)
      for (f <- E4M3 to E2M1) {
        check(convert(h("3f800000"), f, decode = false, scaled = true, factor = 255, rm = rm),
          Seq("7f", "7e", "7")(f - E4M3), if (f == E2M1) NV else 0)
        check(convert(BigInt(0), f, decode = true, scaled = true, factor = 255, rm = rm),
          "7fc00000", if (f == E2M1) NV else 0)
      }
      check(convert(h("01"), E5M2, decode = true, scaled = true, factor = 0, rm = rm), "40", 0)
      check(convert(h("7b"), E5M2, decode = true, scaled = true, factor = 254, rm = rm),
        Seq("7f800000", "7f7fffff", "7f7fffff", "7f800000", "7f800000")(rm), OF | NX)
      check(convert(h("01"), E2M1, decode = true, scaled = true, factor = 0, rm = rm), "200000", 0)
      check(convert(h("7d"), E5M2, decode = true, scaled = true, factor = 255, rm = rm), "7fc00000", NV)
      check(round32(BigInt(1), -150, rm), Seq("0", "0", "0", "1", "1")(rm), UF | NX)
    }
    check(convert(h("40800000"), E2M1, decode = false, scaled = true, factor = 128), "4", 0)
    check(convert(h("4"), E2M1, decode = true, scaled = true, factor = 128), "40800000", 0)
    check(div32(h("3f800000"), h("40400000")), "3eaaaaab", NX)
    check(div32(h("3f800000"), h("40400000"), 1), "3eaaaaaa", NX)
    check(div32(h("3f800000"), h("00000000")), "7f800000", DZ)
    check(div32(h("00000000"), h("00000000")), "7fc00000", NV)
    check(div32(h("7f800000"), h("7f800000")), "7fc00000", NV)
    check(div32(h("80000000"), h("3f800000")), "80000000", 0)
    check(div32(h("00000001"), h("40000000")), "00000000", UF | NX)
    check(mul32(h("3fc00000"), h("40000000")), "40400000", 0)
    check(add32(h("3f800000"), h("bf800000")), "00000000", 0)

    // Hand anchors around minimum-normal F32. Let U=2^-149 be the
    // subnormal ULP. At N-U/2, the final encoding rounds to N under RNE,
    // but full 24-bit precision with an unbounded exponent is still tiny.
    // At N-U/4 the unbounded tie rounds to N, so only NX is raised.
    val beforeTininess = comparisons
    var f32TininessCrossChecks = 0
    for (negative <- Seq(false, true); rm <- 0 to 4) {
      val sign = if (negative) BigInt(1) << 31 else BigInt(0)
      val magnitudeRM = if (negative && rm == 2) 3 else if (negative && rm == 3) 2 else rm
      def signedBits(bits: String): String = (h(bits) | sign).toString(16)
      val nearestNormal = Seq("00800000", "007fffff", "007fffff", "00800000", "00800000")(magnitudeRM)
      val belowHalf = Seq("007fffff", "007fffff", "007fffff", "00800000", "007fffff")(magnitudeRM)
      val nearFlags = Seq(NX, UF | NX, UF | NX, NX, NX)(magnitudeRM)
      val threeEighthFlags = Seq(UF | NX, UF | NX, UF | NX, NX, UF | NX)(magnitudeRM)
      // Numerator / 2^152 = N - delta*(U/8); each expected result is explicit.
      val anchors = Seq(
        (1, nearestNormal, nearFlags), (2, nearestNormal, nearFlags),
        (3, nearestNormal, threeEighthFlags), (4, nearestNormal, UF | NX),
        (5, belowHalf, UF | NX), (8, "007fffff", 0))
      for ((delta, expectedBits, expectedFlags) <- anchors) {
        val magnitude = (BigInt(1) << 26) - delta
        val numerator = if (negative) -magnitude else magnitude
        val actual = roundRational32(numerator, BigInt(1) << 152, rm)
        check(actual, signedBits(expectedBits), expectedFlags)
        val independent = NativeConvertReference.round(numerator, -152, NativeConvertReference.F32, rm)
        assert(actual.bits == independent.bits && actual.flags == independent.flags,
          s"F32 ratio/dyadic tininess disagreement sign=$negative rm=$rm delta=$delta")
        f32TininessCrossChecks += 1
      }
      // Different exact arithmetic routes reach the same N-U/2 midpoint.
      val dividedMidpoint = div32(h("00ffffff") | sign, h("40000000"), rm)
      val multipliedMidpoint = mul32(h("00800000") | sign, h("3f7fffff"), rm)
      check(dividedMidpoint, signedBits(nearestNormal), UF | NX)
      check(multipliedMidpoint, signedBits(nearestNormal), UF | NX)
      val independentProduct = BF16Reference.mul32(h("00800000") | sign, h("3f7fffff"), rm)
      assert(dividedMidpoint.bits == independentProduct.bits && dividedMidpoint.flags == independentProduct.flags,
        s"F32 division/exact-product tininess disagreement sign=$negative rm=$rm")
      f32TininessCrossChecks += 1
      // (N+U)*(1-2^-23) = N-2^-172: near-normal must not spuriously raise UF.
      check(mul32(h("00800001") | sign, h("3f7ffffe"), rm), signedBits(nearestNormal), nearFlags)
    }
    val f32TininessAnchors = comparisons - beforeTininess
    assert(f32TininessAnchors == 90 && f32TininessCrossChecks == 70)
    println(s"LOWP_REFERENCE_PASS comparisons=$comparisons rounding_modes=5 f32_ieee_tininess_anchors=$f32TininessAnchors f32_tininess_crosschecks=$f32TininessCrossChecks oracle=exact_bigint_ratio_and_finite_lattice")
  }
}

class LowpConvertCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Actual two-cycle YunSuan LowpConvert"

  private case class Input(raw: BigInt, format: Int, decode: Boolean, scaled: Boolean = false,
      factor: Int = 127, rm: Int = 0, category: String = "directed")
  private case class Expected(input: Input, result: LowpReference.Result)
  private case class Bucket(format: Int, decode: Boolean, scaled: Boolean, rm: Int)

  private def check(actual: BigInt, expected: BigInt, context: => String): Unit =
    if (actual != expected) throw new AssertionError(s"$context actual=0x${actual.toString(16)} expected=0x${expected.toString(16)}")

  it should "check all codes, midpoint neighborhoods, every scale, flags and streaming control" in {
    import LowpReference._
    test(new LowpConvert).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      val seed = 0x8f432026L
      val random = new Random(seed)
      val injectedFault = sys.env.get("LOWP_CORE_INJECT_FAULT")
      require(injectedFault.forall(Set("result", "flags").contains))
      var injected = false
      var cycle = 0; var accepted = 0; var compared = 0; var aborted = 0
      var bubbles = 0; var run = 0; var maxRun = 0; var checkerFaults = 0
      var last = Result(BigInt(0), 0)
      val pending = mutable.Map.empty[Int, Expected]
      val coverage = mutable.Map.empty[Bucket, Int].withDefaultValue(0)
      val factors = mutable.Map.empty[Bucket, mutable.Set[Int]]
      val categories = mutable.Map.empty[String, Int].withDefaultValue(0)
      val flags = mutable.Map.empty[Int, mutable.Set[Int]]

      def drive(in: Input, fire: Boolean = true): Unit = {
        dut.io.fire.poke(fire.B)
        dut.io.src.poke((in.raw & BigInt("ffffffff", 16)).U)
        dut.io.format.poke(in.format.U)
        dut.io.decode.poke(in.decode.B)
        dut.io.scaled.poke(in.scaled.B)
        dut.io.factor.poke(in.factor.U)
        dut.io.rm.poke(in.rm.U)
      }
      drive(Input(BigInt(0), I8, decode = false), fire = false)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)

      def tick(job: Option[Input]): Unit = {
        pending.remove(cycle) match {
          case Some(wanted) =>
            val raw = dut.io.result.peek().litValue
            val rawFlags = dut.io.fflags.peek().litValue
            val injectNow = injectedFault.nonEmpty && !injected
            if (injectNow) {
              println(s"LOWP_CORE_FAULT_INJECT kind=${injectedFault.get} cycle=$cycle")
              injected = true
            }
            check(if (injectNow && injectedFault.contains("result")) raw ^ 1 else raw,
              wanted.result.bits, s"cycle=$cycle ${wanted.input} result")
            check(if (injectNow && injectedFault.contains("flags")) rawFlags ^ 1 else rawFlags,
              BigInt(wanted.result.flags), s"cycle=$cycle ${wanted.input} flags")
            if (checkerFaults == 0) {
              intercept[AssertionError] { check(raw ^ 1, wanted.result.bits, "corrupt result") }
              intercept[AssertionError] { check(rawFlags ^ 1, BigInt(wanted.result.flags), "corrupt flags") }
              checkerFaults = 2
            }
            val in = wanted.input
            val bucket = Bucket(in.format, in.decode, in.scaled, in.rm)
            coverage(bucket) += 1
            if (in.scaled) factors.getOrElseUpdate(bucket, mutable.Set.empty[Int]) += in.factor
            categories(in.category) += 1
            flags.getOrElseUpdate(in.format, mutable.Set.empty[Int]) += wanted.result.flags
            last = wanted.result; compared += 1
          case None =>
            check(dut.io.result.peek().litValue, last.bits, s"cycle=$cycle bubble result hold")
            check(dut.io.fflags.peek().litValue, BigInt(last.flags), s"cycle=$cycle bubble flags hold")
        }
        job match {
          case Some(in) =>
            drive(in)
            pending(cycle + 2) = Expected(in, convert(in.raw, in.format, in.decode, in.scaled, in.factor, in.rm))
            accepted += 1; run += 1; maxRun = math.max(maxRun, run)
          case None =>
            // Poison live inputs in bubbles to detect controls used after acceptance.
            drive(Input(BigInt("ff800001", 16), E4M3 + cycle % 3, cycle % 2 == 0,
              scaled = cycle % 3 != 0, factor = cycle % 256, rm = cycle % 5), fire = false)
            bubbles += 1; run = 0
        }
        dut.clock.step(); cycle += 1
      }
      def submit(in: Input): Unit = {
        if (accepted % 193 == 192) tick(None)
        tick(Some(in))
        if (accepted % 65536 == 0) println(s"LOWP_CORE_PROGRESS accepted=$accepted compared=$compared cycles=$cycle")
      }

      // Every encoding in every raw format and RM. High bits must be ignored.
      for (f <- I8 to E2M1; raw <- 0 until (if (f == E2M1) 16 else 256); rm <- 0 to 4) {
        submit(Input(BigInt("a519c000", 16) | (if (f == E2M1) 0xf0 else 0) | raw,
          f, decode = true, rm = rm, category = "exhaustive_decode"))
        val expanded = convert(BigInt(raw), f, decode = true)
        if (((expanded.bits >> 23) & 255) != 255)
          submit(Input(expanded.bits, f, decode = false, rm = rm, category = "all_exact_encode"))
      }

      def neighborhood(midpoint: Rational, f: Int, scaled: Boolean, factor: Int, category: String): Unit = {
        val bits = roundRational32(midpoint.numerator, midpoint.denominator).bits
        if (((bits >> 23) & 255) != 255) {
          for (offset <- -1 to 1 if bits + offset >= 0 && bits + offset < BigInt("7f800000", 16);
               negative <- Seq(false, true); rm <- 0 to 4) {
            val raw = (bits + offset) | (if (negative) BigInt(1) << 31 else BigInt(0))
            submit(Input(raw, f, decode = false, scaled = scaled, factor = factor, rm = rm, category = category))
          }
        }
      }
      // All adjacent finite levels and the finite/overflow edge, three F32
      // neighbors around each exact midpoint, both signs and all five RMs.
      for (f <- E4M3 to E2M1) {
        val levels = positiveLevels(f, includeOverflow = true)
        for (index <- 0 until levels.size - 1)
          neighborhood((levels(index) + levels(index + 1)).half, f, false, 127, "all_midpoints")
      }
      for (f <- I8 to U8; integer <- (if (f == I8) -130 to 130 else -2 to 257); rm <- 0 to 4) {
        val midpoint = roundRational32(BigInt(2 * integer + 1), BigInt(2)).bits
        for (offset <- -1 to 1)
          submit(Input(midpoint + offset, f, decode = false, rm = rm, category = "integer_midpoints"))
      }

      val specials = Seq("00000000", "80000000", "00000001", "80000001", "007fffff", "807fffff",
        "00800000", "80800000", "3f800000", "bf800000", "7f7fffff", "ff7fffff",
        "7f800000", "ff800000", "7f800001", "ff800001", "7fc00000", "ffc00001").map(BigInt(_, 16))
      for (f <- I8 to E2M1; raw <- specials; rm <- 0 to 4)
        submit(Input(raw, f, decode = false, rm = rm, category = "raw_specials"))

      for (f <- E4M3 to E2M1; factor <- 0 to 255) {
        // Every low code at every UE8M0 encoding, with all RMs represented
        // independently in every format/factor and with sign/special coverage.
        for (raw <- 0 until (if (f == E2M1) 16 else 256))
          submit(Input(BigInt(raw), f, decode = true, scaled = true, factor = factor,
            rm = (raw + factor) % 5, category = "scaled_all_codes"))
        for (raw <- specials; rm <- 0 to 4)
          submit(Input(raw, f, decode = false, scaled = true, factor = factor, rm = rm, category = "scaled_extremes"))
        if (factor < 255) {
          val levels = positiveLevels(f, includeOverflow = true)
          val normalIndex = if (f == E4M3) 8 else if (f == E5M2) 4 else 2
          val boundaries = Seq(0, normalIndex - 1, levels.size / 2, levels.size - 2).distinct
          for (index <- boundaries) {
            val midpoint = (levels(index) + levels(index + 1)).half.timesPowerOfTwo(factor - 127)
            neighborhood(midpoint, f, true, factor, "scaled_midpoints")
          }
        }
      }
      for (index <- 0 until 8192) {
        val f = index % 5
        submit(Input(BigInt(32, random), f, index % 3 == 0,
          scaled = f >= E4M3 && index % 2 == 0, factor = random.nextInt(256), rm = index % 5, category = "mixed_random"))
      }
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && accepted == compared)

      // Two accepted transactions are aborted in flight; neither may emerge.
      tick(Some(Input(BigInt("7f800001", 16), E5M2, decode = false, category = "reset_aborted")))
      tick(Some(Input(BigInt("ff", 16), I8, decode = true, category = "reset_aborted")))
      assert(pending.size == 2)
      aborted += pending.size; pending.clear()
      dut.io.fire.poke(false.B); dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      cycle += 2; last = Result(BigInt(0), 0)
      for (_ <- 0 until 3) tick(None)
      submit(Input(BigInt("38", 16), E4M3, decode = true, category = "after_reset"))
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && accepted == compared + aborted && aborted == 2)
      assert(categories("reset_aborted") == 0 && categories("after_reset") == 1)
      assert(categories("exhaustive_decode") == (4 * 256 + 16) * 5)
      assert(categories("all_exact_encode") == (256 + 256 + 254 + 248 + 16) * 5)
      assert(categories("scaled_all_codes") == (2 * 256 + 16) * 256)
      assert(checkerFaults == 2 && maxRun > 2 && bubbles > 0)
      for (f <- I8 to E2M1; decode <- Seq(false, true); scaled <- (if (f >= E4M3) Seq(false, true) else Seq(false)); rm <- 0 to 4) {
        val bucket = Bucket(f, decode, scaled, rm)
        assert(coverage(bucket) > 0, s"missing bucket $bucket")
        val factorCount = factors.get(bucket).map(_.size).getOrElse(0)
        if (scaled) assert(factorCount == 256, s"missing scale coverage $bucket: $factorCount")
        println(s"LOWP_CORE_COVERAGE format=${names(f)} direction=${if (decode) "decode" else "encode"} scaled=$scaled rm=$rm comparisons=${coverage(bucket)} factor_codes=$factorCount")
      }
      for (f <- I8 to E2M1) {
        assert(flags(f).contains(0) && flags(f).contains(NV), s"format ${names(f)} missed exact/invalid")
        if (f <= U8) assert(flags(f).contains(NX))
        else assert(Seq(NX, UF | NX, OF | NX).forall(flags(f).contains))
        println(s"LOWP_CORE_FLAGS format=${names(f)} patterns=${flags(f).toSeq.sorted.mkString(",")}")
      }
      categories.toSeq.sortBy(_._1).foreach { case (name, count) => println(s"LOWP_CORE_CATEGORY name=$name comparisons=$count") }
      println(s"LOWP_CORE_PASS seed=$seed accepted=$accepted retired=$compared aborted=$aborted element_comparisons=$compared flag_comparisons=$compared latency=2 max_consecutive=$maxRun bubbles=$bubbles checker_faults_caught=$checkerFaults coverage_buckets=${coverage.size}")
    }
  }
}

/** Differential compatibility check; the separate core suite supplies the
  * independent numeric oracle, while this harness tests preserved interfaces.
  */
class LowpLegacyCompatibilityHarness extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val src = Input(UInt(32.W))
    val format = Input(UInt(3.W))
    val scaled = Input(Bool())
    val factor = Input(UInt(8.W))
    val rm = Input(UInt(3.W))
    val result = Output(UInt(32.W))
    val fflags = Output(UInt(5.W))
    val legacyResult = Output(UInt(32.W))
    val legacyFlags = Output(UInt(5.W))
  })
  private val core = Module(new LowpConvert)
  private val xx8 = Module(new CVT_xx8)
  private val mx = Module(new CVT_mxfp)
  core.io.fire := io.fire
  core.io.src := io.src
  core.io.format := io.format
  core.io.decode := false.B
  core.io.scaled := io.scaled
  core.io.factor := io.factor
  core.io.rm := io.rm
  xx8.io.fire := io.fire
  xx8.io.src := io.src
  xx8.io.rm := io.rm
  xx8.io.opType := MuxLookup(io.format, VfcvtType.vfncvtxx8_int8)(Seq(
    2.U -> VfcvtType.vfncvtxx8_e4m3, 3.U -> VfcvtType.vfncvtxx8_e5m2))
  mx.io.fire := io.fire
  mx.io.src := io.src
  mx.io.factor := Mux(io.scaled, io.factor, 127.U)
  mx.io.rm := io.rm
  mx.io.isFp4 := io.format === 4.U
  // Factor127 must also match ordinary XX8 for both E4M3 and E5M2.
  private val useMx = io.format === 4.U || (io.format === 2.U && io.scaled && io.factor =/= 127.U)
  private val useMxStage = RegEnable(useMx, false.B, io.fire)
  private val fireStage = RegNext(io.fire, false.B)
  private val useMxOut = RegEnable(useMxStage, false.B, fireStage)
  io.result := core.io.result
  io.fflags := core.io.fflags
  io.legacyResult := Mux(useMxOut, mx.io.result, xx8.io.result)
  io.legacyFlags := Mux(useMxOut, mx.io.fflags, xx8.io.fflags)
}

class LowpLegacyCompatibilitySpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "LowpConvert legacy XX8 and MX compatibility"

  it should "preserve raw and given-scale results and flags, including factor127 identity" in {
    test(new LowpLegacyCompatibilityHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      val random = new Random(0x1e9ac8L)
      case class Job(raw: BigInt, format: Int, scaled: Boolean, factor: Int, rm: Int)
      val pending = mutable.Map.empty[Int, Job]
      val coverage = mutable.Map.empty[(Int, Boolean, Int), Int].withDefaultValue(0)
      var cycle = 0; var accepted = 0; var compared = 0; var aborted = 0
      def drive(job: Job, fire: Boolean): Unit = {
        dut.io.fire.poke(fire.B); dut.io.src.poke(job.raw.U)
        dut.io.format.poke(job.format.U); dut.io.scaled.poke(job.scaled.B)
        dut.io.factor.poke(job.factor.U); dut.io.rm.poke(job.rm.U)
      }
      drive(Job(BigInt(0), 0, false, 127, 0), false)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      def tick(job: Option[Job]): Unit = {
        val newResult = dut.io.result.peek().litValue
        val newFlags = dut.io.fflags.peek().litValue
        assert(newResult == dut.io.legacyResult.peek().litValue,
          s"legacy result cycle=$cycle pending=${pending.get(cycle)} new=0x${newResult.toString(16)}")
        assert(newFlags == dut.io.legacyFlags.peek().litValue,
          s"legacy flags cycle=$cycle pending=${pending.get(cycle)} new=$newFlags")
        pending.remove(cycle).foreach { completed =>
          compared += 1; coverage((completed.format, completed.scaled, completed.rm)) += 1
        }
        job match {
          case Some(in) => drive(in, true); pending(cycle + 2) = in; accepted += 1
          case None => drive(Job(BigInt("ff800001", 16), 4, true, cycle % 256, cycle % 5), false)
        }
        dut.clock.step(); cycle += 1
      }
      def submit(job: Job): Unit = { if (accepted % 11 == 10) tick(None); tick(Some(job)) }
      val profiles = Seq((0, false), (2, false), (3, false), (4, false), (2, true), (3, true), (4, true))
      val specials = Seq("00000000", "80000000", "00000001", "007fffff", "00800000", "3f880000",
        "43e80000", "47700000", "40e00000", "7f7fffff", "ff7fffff", "7f800000", "ff800000",
        "7f800001", "ff800001", "7fc00000", "ffc00001").map(BigInt(_, 16))
      for ((format, scaled) <- profiles; factor <- (if (scaled && format != 3) Seq(0, 1, 126, 127, 128, 254, 255) else Seq(127));
           rm <- 0 to 4; raw <- specials)
        submit(Job(raw, format, scaled, factor, rm))
      for (index <- 0 until 4096) {
        val (format, scaled) = profiles(index % profiles.size)
        val factor = if (scaled && format != 3) random.nextInt(256) else 127
        submit(Job(BigInt(32, random), format, scaled, factor, index % 5))
      }
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && accepted == compared)
      tick(Some(Job(BigInt("7f800001", 16), 2, true, 255, 4)))
      aborted += pending.size; pending.clear()
      dut.io.fire.poke(false.B); dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      cycle += 2
      assert(dut.io.result.peek().litValue == 0 && dut.io.fflags.peek().litValue == 0)
      for (_ <- 0 until 3) tick(None)
      submit(Job(BigInt("3f800000", 16), 3, true, 127, 0))
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && accepted == compared + aborted && aborted == 1)
      for ((format, scaled) <- profiles; rm <- 0 to 4) {
        val count = coverage((format, scaled, rm))
        assert(count > 0)
        println(s"LOWP_LEGACY_COVERAGE format=${LowpReference.names(format)} scaled=$scaled rm=$rm comparisons=$count")
      }
      println(s"LOWP_LEGACY_PASS accepted=$accepted retired=$compared aborted=$aborted element_comparisons=$compared flag_comparisons=$compared factor127_raw_equivalence=e4m3,e5m2")
    }
  }
}
