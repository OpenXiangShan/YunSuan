package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import _root_.yunsuan.vector.VectorConvert.{CVTIO, CVT_bf16}

/** Only wires the actual conversion module; no arithmetic model in hardware. */
class BF16ConvertCoreHarness extends Module {
  val io = IO(new CVTIO(64))
  private val core = Module(new CVT_bf16(64))
  io <> core.io
}

class BF16ReferenceSpec extends AnyFlatSpec {
  behavior of "Independent exact-dyadic BF16 reference"

  it should "match hand-derived ties, extremes, invalid operations and fused cancellation" in {
    import BF16Reference._
    def h(value: String): BigInt = BigInt(value, 16)
    var comparisons = 0
    def check(actual: Result, bits: String, flags: Int): Unit = {
      assert(actual == Result(h(bits), flags), s"actual=$actual expected=0x$bits flags=$flags")
      comparisons += 1
    }
    Seq("0000" -> "00000000", "8000" -> "80000000", "0001" -> "00010000",
      "007f" -> "007f0000", "0080" -> "00800000", "3f80" -> "3f800000",
      "7f7f" -> "7f7f0000", "ff7f" -> "ff7f0000", "7f80" -> "7f800000",
      "ff80" -> "ff800000").foreach { case (a, result) => check(widen(h(a)), result, 0) }
    check(widen(h("7fc1")), "7fc00000", 0)
    check(widen(h("ff81")), "7fc00000", 16)
    for (rm <- 0 to 4) {
      // 1 + 2^-8 lies halfway between BF16 1 and 1 + 2^-7.
      check(narrow(h("3f808000"), rm), Seq("3f80", "3f80", "3f80", "3f81", "3f81")(rm), 1)
      check(narrow(h("bf808000"), rm), Seq("bf80", "bf80", "bf81", "bf80", "bf81")(rm), 1)
      // 2^-134 is half a BF16 subnormal; a zero rounded result remains tiny.
      check(narrow(h("00008000"), rm), Seq("0000", "0000", "0000", "0001", "0001")(rm), 3)
      check(narrow(h("80008000"), rm), Seq("8000", "8000", "8001", "8000", "8001")(rm), 3)
      // The unbounded-exponent precision rounding is still tiny here, even
      // when the final subnormal rounding carries into minimum normal.
      check(narrow(h("007f8000"), rm), Seq("0080", "007f", "007f", "0080", "0080")(rm), 3)
      check(narrow(h("807f8000"), rm), Seq("8080", "807f", "8080", "807f", "8080")(rm), 3)
      check(narrow(h("007f8001"), rm), Seq("0080", "007f", "007f", "0080", "0080")(rm), Seq(3, 3, 3, 1, 3)(rm))
      check(narrow(h("807f8001"), rm), Seq("8080", "807f", "8080", "807f", "8080")(rm), Seq(3, 3, 1, 3, 3)(rm))
      check(narrow(h("007fc000"), rm), Seq("0080", "007f", "007f", "0080", "0080")(rm), Seq(1, 3, 3, 1, 1)(rm))
      check(narrow(h("807fc000"), rm), Seq("8080", "807f", "8080", "807f", "8080")(rm), Seq(1, 3, 1, 3, 1)(rm))
      // Overflow tests the rounded unbounded-exponent result: RTZ remains max finite.
      check(narrow(h("7f7f8000"), rm), Seq("7f80", "7f7f", "7f7f", "7f80", "7f80")(rm), Seq(5, 1, 1, 5, 5)(rm))
      check(narrow(h("7f7f0000"), rm), "7f7f", 0)
      check(narrow(h("007f0000"), rm), "007f", 0)
      check(narrow(h("ff800001"), rm), "7fc0", 16)
      check(narrow(h("ffc00001"), rm), "7fc0", 0)
      check(fma(h("3f80"), h("3f80"), h("bf800000"), rm), if (rm == 2) "80000000" else "00000000", 0)
      check(fma(h("8000"), h("3f80"), h("80000000"), rm), "80000000", 0)
      check(fma(h("0001"), h("0001"), h("00000000"), rm), if (rm == 3) "00000001" else "00000000", 3)
      // Exact BF16 product adds half an FP32 subnormal ulp to max-subnormal C.
      // FP32 output can become min-normal while unbounded-precision tininess remains.
      check(fma(h("0001"), h("3700"), h("007fffff"), rm),
        Seq("00800000", "007fffff", "007fffff", "00800000", "00800000")(rm), 3)
      check(fma(h("8001"), h("3700"), h("807fffff"), rm),
        Seq("80800000", "807fffff", "80800000", "807fffff", "80800000")(rm), 3)
      // Three quarters of an ulp instead crosses the unbounded rounding threshold.
      check(fma(h("0003"), h("3680"), h("007fffff"), rm),
        Seq("00800000", "007fffff", "007fffff", "00800000", "00800000")(rm), Seq(1, 3, 3, 1, 1)(rm))
      check(fma(h("8003"), h("3680"), h("807fffff"), rm),
        Seq("80800000", "807fffff", "80800000", "807fffff", "80800000")(rm), Seq(1, 3, 1, 3, 1)(rm))
      check(fma(h("7f7f"), h("4000"), h("00000000"), rm),
        if (rm == 1 || rm == 2) "7f7fffff" else "7f800000", 5)
      // Product exceeds FP32 range but exact product-plus-addend is representable.
      check(fma(h("7f7f"), h("4000"), h("ff7fffff"), rm), "7f7e0001", 0)
      check(fma(h("3f81"), h("3f81"), h("bf820000"), rm), "38800000", 0)
      check(fma(h("7f80"), h("0000"), h("7fc00000"), rm), "7fc00000", 16)
      check(fma(h("7f80"), h("3f80"), h("ff800000"), rm), "7fc00000", 16)
      check(fma(h("7fc1"), h("3f80"), h("00000000"), rm), "7fc00000", 0)
      check(fma(h("7f81"), h("3f80"), h("00000000"), rm), "7fc00000", 16)
      check(addBF16(h("3f80"), h("3b80"), rm), Seq("3f80", "3f80", "3f80", "3f81", "3f81")(rm), 1)
    }
    check(narrow(h("3f818000"), 0), "3f82", 1)
    check(fma32(h("3f800001"), h("3f800001"), h("bf800002"), 0), "28800000", 0)
    check(add32(h("00000001"), h("00000001"), 0), "00000002", 0)
    check(mul32(h("80000000"), h("3f800000"), 0), "80000000", 0)
    println(s"BF16_REFERENCE_PASS comparisons=$comparisons rounding_modes=5 oracle=exact_bigint_dyadic")
  }
}

class BF16ConvertCoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Actual YunSuan CVT_bf16 core"

  private case class Input(raw: BigInt, widen: Boolean, rm: Int, mode: Int,
      category: String, badBox: Boolean = false)
  private case class Expected(result: BF16Reference.Result, input: Input)

  private def expected(in: Input): BF16Reference.Result = {
    val ref = if (in.badBox) {
      if (in.widen) BF16Reference.Result(BigInt("7fc00000", 16), 0)
      else BF16Reference.Result(BigInt("7fc0", 16), 0)
    } else if (in.widen) BF16Reference.widen(in.raw) else BF16Reference.narrow(in.raw, in.rm)
    val payload = if (in.widen) 32 else 16
    val box = if (in.mode == 1) ((BigInt(1) << (64 - payload)) - 1) << payload else BigInt(0)
    ref.copy(bits = ref.bits | box)
  }

  private def check(actual: BigInt, wanted: BigInt, context: => String): Unit =
    if (actual != wanted) throw new AssertionError(s"$context actual=0x${actual.toString(16)} expected=0x${wanted.toString(16)}")

  it should "check every widening encoding and every finite narrowing midpoint in five rounding modes" in {
    test(new BF16ConvertCoreHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      val seed = 0x16bf32L
      val random = new Random(seed)
      val injectedFault = sys.env.get("BF16_CVT_INJECT_FAULT")
      require(injectedFault.forall(Set("result", "flags").contains))
      var injected = false
      val pending = mutable.Map.empty[Int, Expected]
      val counts = mutable.Map.empty[String, Int].withDefaultValue(0)
      val rmCoverage = Array.fill(5)(0)
      val modeCoverage = Array.fill(3)(0)
      val flagsCoverage = mutable.Set.empty[Int]
      var cycle = 0
      var accepted = 0
      var comparisons = 0
      var bubbles = 0
      var currentRun = 0
      var maxRun = 0
      var checkerFaults = 0
      var last = BF16Reference.Result(BigInt(0), 0)

      dut.io.fire.poke(false.B)
      dut.io.src.poke(0.U)
      dut.io.opType.poke(0.U)
      dut.io.sew.poke(0.U)
      dut.io.rm.poke(0.U)
      dut.io.input1H.poke(0.U)
      dut.io.output1H.poke(0.U)
      dut.io.isFpToVecInst.poke(false.B)
      dut.io.isFround.poke(0.U)
      dut.io.isFcvtmod.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)

      def drive(in: Input): Unit = {
        val inputWidth = if (in.widen) 16 else 32
        val mask = (BigInt(1) << inputWidth) - 1
        val high = if (in.mode == 1 && !in.badBox) ((BigInt(1) << (64 - inputWidth)) - 1) << inputWidth
          else if (in.mode != 1) BigInt("a5195a5a6e93c700", 16) & ~mask else BigInt(0)
        dut.io.fire.poke(true.B)
        dut.io.src.poke((high | (in.raw & mask)).U)
        // Explicit vector, explicit scalar and format-selected paths alternate.
        val op = if (in.mode == 0) { if (in.widen) 0xcd else 0xdd }
          else if (in.mode == 1) { if (in.widen) 0xca else 0xd2 } else 0xc0
        dut.io.opType.poke(op.U)
        dut.io.sew.poke((if (in.widen) 1 else 2).U)
        dut.io.rm.poke(in.rm.U)
        dut.io.input1H.poke((if (in.mode == 1) 0 else if (in.widen) 2 else 4).U)
        dut.io.output1H.poke((if (in.mode == 1) 0 else if (in.widen) 4 else 2).U)
        dut.io.isFpToVecInst.poke((in.mode == 1).B)
      }

      def tick(job: Option[Input]): Unit = {
        pending.remove(cycle) match {
          case Some(wanted) =>
            val rawResult = dut.io.result.peek().litValue
            val rawFlags = dut.io.fflags.peek().litValue
            val injectNow = injectedFault.nonEmpty && !injected
            if (injectNow) {
              println(s"BF16_CORE_CVT_FAULT_INJECT kind=${injectedFault.get} cycle=$cycle")
              injected = true
            }
            val observedResult = if (injectNow && injectedFault.contains("result")) rawResult ^ 1 else rawResult
            val observedFlags = if (injectNow && injectedFault.contains("flags")) rawFlags ^ 1 else rawFlags
            lazy val context = s"cycle=$cycle input=${wanted.input}"
            check(observedResult, wanted.result.bits, s"$context result")
            check(observedFlags, BigInt(wanted.result.flags), s"$context flags")
            if (checkerFaults == 0) {
              intercept[AssertionError] { check(rawResult ^ 1, wanted.result.bits, "result checker corruption") }
              intercept[AssertionError] { check(rawFlags ^ 1, BigInt(wanted.result.flags), "flags checker corruption") }
              checkerFaults = 2
            }
            last = wanted.result
            comparisons += 1
            counts(wanted.input.category) += 1
            flagsCoverage += wanted.result.flags
          case None =>
            check(dut.io.result.peek().litValue, last.bits, s"cycle=$cycle bubble result retention")
            check(dut.io.fflags.peek().litValue, BigInt(last.flags), s"cycle=$cycle bubble flags retention")
        }
        job match {
          case Some(in) =>
            drive(in)
            pending(cycle + 2) = Expected(expected(in), in)
            accepted += 1
            rmCoverage(in.rm) += 1
            modeCoverage(in.mode) += 1
            currentRun += 1
            maxRun = math.max(maxRun, currentRun)
          case None =>
            dut.io.fire.poke(false.B)
            // Change every control even in bubbles, detecting unregistered use.
            dut.io.src.poke(BigInt("ff8000017f800001", 16).U)
            dut.io.rm.poke(((cycle + 2) % 5).U)
            dut.io.opType.poke((if (cycle % 2 == 0) 0xcd else 0xdd).U)
            dut.io.input1H.poke(0.U)
            dut.io.output1H.poke(0.U)
            dut.io.isFpToVecInst.poke((cycle % 2 == 0).B)
            bubbles += 1
            currentRun = 0
        }
        dut.clock.step()
        cycle += 1
      }

      def submit(in: Input): Unit = {
        if (accepted % 257 == 256) tick(None)
        tick(Some(in))
        if (accepted % 131072 == 0)
          println(s"BF16_CORE_CVT_PROGRESS accepted=$accepted compared=$comparisons cycles=$cycle")
      }

      for (raw <- 0 until 65536) {
        submit(Input(BigInt(raw), true, raw % 5, raw % 3, "exhaustive_widen"))
        submit(Input(BigInt(raw) << 16, false, raw % 5, (raw + 1) % 3, "exact_narrow"))
      }

      // Every finite BF16 upper encoding, including both signs, zero/subnormal,
      // normal binade changes and the maximum-finite/overflow boundary.
      for (upper <- 0 until 65536 if ((upper >> 7) & 255) != 255;
           low <- Seq(0x7fff, 0x8000, 0x8001); rm <- 0 to 4) {
        submit(Input((BigInt(upper) << 16) | low, false, rm,
          accepted % 3, "midpoint_narrow"))
      }

      val specials = Seq("00000000", "80000000", "00000001", "80000001",
        "007fffff", "00800000", "7f7fffff", "ff7fffff", "7f800000", "ff800000",
        "7f800001", "ff800001", "7fc00000", "ffc12345", "7fffffff", "ffffffff").map(BigInt(_, 16))
      for (raw <- specials; rm <- 0 to 4; mode <- 0 to 2)
        submit(Input(raw, false, rm, mode, "specials"))
      for (widen <- Seq(false, true); rm <- 0 to 4;
           raw <- Seq(BigInt(0), BigInt("7f800001", 16), BigInt("7f81", 16), BigInt("ffffffff", 16)))
        submit(Input(raw, widen, rm, 1, "bad_box", badBox = true))

      for (i <- 0 until 4096) {
        val widen = i % 2 == 0
        submit(Input(BigInt(if (widen) 16 else 32, random), widen, i % 5, i % 3, "mixed_stream"))
      }
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && comparisons == accepted)
      assert(counts("exhaustive_widen") == 65536)
      assert(counts("exact_narrow") == 65536)
      assert(counts("midpoint_narrow") == 65280 * 3 * 5)
      assert(rmCoverage.forall(_ > 0) && modeCoverage.forall(_ > 0))
      assert(Seq(0, 1, 3, 5, 16).forall(flagsCoverage.contains))
      assert(checkerFaults == 2 && maxRun > 2 && bubbles > 0)

      // Reset while the real core holds an accepted operation. Both registers
      // must reset, then a fresh operation must still retire at latency two.
      drive(Input(BigInt("7f800001", 16), false, 3, 0, "reset_discard"))
      dut.clock.step()
      dut.io.fire.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(2)
      dut.reset.poke(false.B)
      last = BF16Reference.Result(BigInt(0), 0)
      for (_ <- 0 until 3) tick(None)
      submit(Input(BigInt("3f80", 16), true, 0, 0, "after_reset"))
      for (_ <- 0 until 3) tick(None)
      assert(pending.isEmpty && comparisons == accepted && counts("after_reset") == 1)
      println(s"BF16_CORE_CVT_PASS seed=$seed accepted=$accepted retired=$comparisons element_comparisons=$comparisons flag_comparisons=$comparisons widen_exhaustive=${counts("exhaustive_widen")} exact_narrow=${counts("exact_narrow")} finite_boundaries=65280 midpoint_comparisons=${counts("midpoint_narrow")} rounding_modes=5 min_rm=${rmCoverage.min} operand_modes=3 min_mode=${modeCoverage.min} specials=${counts("specials")} bad_boxes=${counts("bad_box")} mixed_stream=${counts("mixed_stream")} flag_patterns=${flagsCoverage.toSeq.sorted.mkString(",")} latency=2 max_consecutive=$maxRun bubbles=$bubbles reset_discarded=1 after_reset=${counts("after_reset")} checker_faults_caught=$checkerFaults")
    }
  }
}
