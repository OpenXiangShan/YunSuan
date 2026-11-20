package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import _root_.yunsuan.vector.cvt.{NvPayloadPipeline, NvPayloadProfile}

/** Direct payload-stream tests; all numeric expectations use independent exact
  * integer/rational references, with no RTL helper or host floating-point oracle.
  */
class NvPayloadPipelineSpec extends AnyFlatSpec with ChiselScalatestTester {
  import LowpReference.{Result, Rational}
  private def h(s: String): BigInt = BigInt(s, 16)
  private val one = h("3f800000")
  private val minimumScale = h("3b000000")
  private val sign = h("80000000")
  private val alphas = Seq(one, h("3f000000"), h("40400000"), h("3b124925"),
    h("0d800000"), h("71800000"))
  private val finiteEdges = Seq("00000000", "80000000", "00000001", "80000001", "007fffff",
    "00800000", "00800001", "3e7fffff", "3e800000", "3e800001", "3f7fffff", "3f800000",
    "3f800001", "40dfffff", "40e00000", "40e00001", "7f7fffff", "ff7fffff").map(h)
  private val specialEdges = Seq("7f800000", "ff800000", "7fc00000", "7f800001", "ff800001").map(h)

  private case class Job(tag: Int, label: String, decode: Boolean, src: Seq[BigInt],
      inverse: BigInt, scale: Int, alpha: BigInt, zeroBlock: Boolean = false) {
    val expected: Seq[Result] = src.map { x =>
      if (decode) {
        val q = LowpReference.convert(x & 15, 4, decode = true)
        val s = LowpReference.convert(BigInt(scale), 2, decode = true)
        val p = LowpReference.mul32(q.bits, s.bits)
        val r = LowpReference.mul32(p.bits, alpha)
        Result(r.bits, q.flags | s.flags | p.flags | r.flags)
      } else {
        val p = LowpReference.mul32(x, inverse)
        val q = LowpReference.convert(if (zeroBlock) BigInt(0) else p.bits, 4, decode = false)
        Result(q.bits, p.flags | q.flags)
      }
    }
  }
  private def inverse(scale: Int, alpha: BigInt): BigInt = {
    val s = LowpReference.convert(BigInt(scale), 2, decode = true).bits
    val denominator = LowpReference.mul32(alpha, s.max(minimumScale)).bits
    LowpReference.div32(one, denominator).bits
  }
  private def multiply(a: Rational, b: Rational): Rational =
    Rational(a.numerator * b.numerator, a.denominator * b.denominator)
  private def rounded(r: Rational): BigInt = LowpReference.roundRational32(r.numerator, r.denominator).bits

  private def checkStream(profile: NvPayloadProfile, lanes: Int, jobs: Seq[Job],
      label: String, witnesses: Int = 0): Unit = {
    require(jobs.nonEmpty && jobs.forall(_.src.size == lanes))
    val input = jobs.zipWithIndex.flatMap { case (job, i) =>
      // A continuous prefix gives a measured interval of at least 256 groups.
      if (i >= 512 && i % 13 == 0) Seq(None, None, Some(job)) else Seq(Some(job))
    }
    test(new NvPayloadPipeline(lanes, 24, profile)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      def poke(job: Job, valid: Boolean): Unit = {
        dut.io.in.valid.poke(valid.B)
        dut.io.in.bits.src.zip(job.src).foreach { case (port, value) => port.poke(value.U) }
        dut.io.in.bits.decode.poke(job.decode.B)
        dut.io.in.bits.inverse.poke(job.inverse.U)
        dut.io.in.bits.scale.poke(job.scale.U)
        dut.io.in.bits.alpha.poke(job.alpha.U)
        dut.io.in.bits.zeroBlock.poke(job.zeroBlock.B)
        dut.io.in.bits.tag.poke(job.tag.U)
      }
      poke(jobs.head, valid = false)
      dut.io.out.valid.expect(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val pending = mutable.Map.empty[Int, Job]
      val flagsSeen = mutable.Set.empty[Int]
      var accepted = 0; var retired = 0; var aborted = 0; var comparisons = 0
      var encodeElements = 0; var decodeElements = 0; var bubbles = 0
      var outputRun = 0; var maxOutputRun = 0; var resetCount = 1
      val fault = sys.env.get("NV_PAYLOAD_INJECT_FAULT")
      require(fault.forall(Set("result", "flags").contains))
      var injected = false
      def check(job: Job, cycle: Int): Unit = {
        dut.io.out.bits.tag.expect(job.tag.U)
        dut.io.out.bits.decode.expect(job.decode.B)
        val inject = fault.nonEmpty && !injected
        if (inject) { injected = true; println(s"NV_PAYLOAD_FAULT_INJECT kind=${fault.get}") }
        for (lane <- 0 until lanes) {
          val rawData = dut.io.out.bits.result(lane).peek().litValue
          val rawFlags = dut.io.out.bits.laneFlags(lane).peek().litValue.toInt
          val data = if (inject && lane == 0 && fault.contains("result")) rawData ^ 1 else rawData
          val flags = if (inject && lane == 0 && fault.contains("flags")) rawFlags ^ 1 else rawFlags
          val expected = job.expected(lane)
          assert(data == expected.bits && flags == expected.flags,
            s"NV_PAYLOAD_MISMATCH profile=$label cycle=$cycle tag=${job.tag} case=${job.label} " +
              s"decode=${job.decode} lane=$lane src=0x${job.src(lane).toString(16)} " +
              s"inverse=0x${job.inverse.toString(16)} scale=${job.scale} alpha=0x${job.alpha.toString(16)} " +
              s"got=0x${data.toString(16)}/$flags expected=0x${expected.bits.toString(16)}/${expected.flags}")
          flagsSeen += flags; comparisons += 1
          if (job.decode) decodeElements += 1 else encodeElements += 1
        }
        dut.io.out.bits.flags.expect(job.expected.map(_.flags).reduce(_ | _).U)
        retired += 1
      }
      def batch(stream: Seq[Option[Job]]): Unit = {
        for (cycle <- 0 until stream.size + NvPayloadPipeline.latency(profile)) {
          val due = pending.remove(cycle)
          dut.io.out.valid.expect(due.nonEmpty.B)
          due match {
            case Some(job) =>
              check(job, cycle); outputRun += 1; maxOutputRun = maxOutputRun.max(outputRun)
            case None => outputRun = 0
          }
          val next = if (cycle < stream.size) stream(cycle) else None
          next match {
            case Some(job) =>
              poke(job, valid = true)
              pending(cycle + NvPayloadPipeline.latency(profile)) = job; accepted += 1
            case None =>
              // Change context during bubbles; it must not affect any valid group.
              poke(jobs((cycle * 7) % jobs.size), valid = false); bubbles += 1
          }
          dut.clock.step()
        }
        assert(pending.isEmpty)
      }
      val split = input.size / 2
      batch(input.take(split))
      // Cancel two groups at different internal stages, including one multiply
      // already retired into the conversion/second multiply boundary.
      poke(jobs.head, valid = true); dut.clock.step(); accepted += 1; aborted += 1
      poke(jobs.last, valid = false); dut.clock.step(if (NvPayloadPipeline.latency(profile) == 4) 1 else 2)
      poke(jobs.last, valid = true); dut.clock.step(); accepted += 1; aborted += 1
      poke(jobs.head, valid = false)
      dut.io.out.valid.expect(false.B)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B); resetCount += 1
      batch(input.drop(split))
      assert(accepted == retired + aborted && retired == jobs.size && comparisons == lanes * jobs.size)
      if (jobs.size >= 1024) assert(maxOutputRun >= 256 && bubbles > 2 * NvPayloadPipeline.latency(profile))
      if (label == "both") assert(Seq(0, 1, 3, 5, 16).forall(flagsSeen.contains), s"flags=$flagsSeen")
      println(s"NV_PAYLOAD_PASS profile=$label lanes=$lanes comparisons=$comparisons " +
        s"encode_elements=$encodeElements decode_elements=$decodeElements accepted=$accepted retired=$retired " +
        s"aborted=$aborted resets=$resetCount bubbles=$bubbles max_output_run=$maxOutputRun " +
        s"latency=${NvPayloadPipeline.latency(profile)} ii=${NvPayloadPipeline.initiationInterval} " +
        s"staged_witnesses=$witnesses flag_classes=${flagsSeen.size} seed=1314279476")
    }
  }

  behavior of "NV fixed-scale payload pipeline"
  it should "preserve staged rounding, stored scale, flags and II1 across changing contexts" in {
    val jobs = mutable.ArrayBuffer.empty[Job]
    def add(label: String, decode: Boolean, src: Seq[BigInt], inv: BigInt, scale: Int,
        alpha: BigInt, zero: Boolean = false): Unit =
      jobs += Job(jobs.size, label, decode, src, inv, scale, alpha, zero)

    // Full finite nonnegative stored E4 scale x all E2M1 payload x six alphas.
    for (alpha <- alphas; scale <- 0 to 0x7e; codes <- (0 until 16).grouped(2)) {
      add("stored-codebook", true, codes.map(c => h("deadbee0") | c).toSeq,
        h("7f800001"), scale, alpha, zero = true) // ignored encode context is poisoned
    }
    assert(jobs.size * 2 == 6 * 127 * 16)

    // Every E2M1 midpoint and both adjacent FP32 values, both signs. The x
    // neighborhood is chosen around midpoint * prepared denominator, then
    // the oracle still executes the separately rounded reciprocal multiply.
    val levels = LowpReference.positiveLevels(4, includeOverflow = true)
    for (alpha <- alphas; scale <- Seq(0, 1, 7, 0x38, 0x5e, 0x7e)) {
      val s = LowpReference.convert(BigInt(scale), 2, decode = true).bits.max(minimumScale)
      val d = LowpReference.mul32(alpha, s).bits
      val inv = inverse(scale, alpha)
      for (pair <- levels.sliding(2); offset <- -1 to 1) {
        val center = rounded(multiply((pair(0) + pair(1)).half, LowpReference.finite32(d)))
        val x = (center + offset).max(BigInt(0)).min(h("7f7fffff"))
        add("midpoint-neighbor", false, Seq(x, x | sign), inv, scale, alpha)
      }
      for (pair <- (finiteEdges ++ specialEdges).grouped(2)) {
        add("ieee-edge", false, pair.padTo(2, BigInt(0)), inv, scale, alpha)
      }
      add("zero-block", false, Seq(BigInt(0), sign), inv, scale, alpha, zero = true)
    }
    // The primitive accepts any precomputed FP32 inverse. These probe flags
    // and overflow/subnormal rounding beyond the finite profile's usual range.
    for (inv <- finiteEdges ++ specialEdges; pair <- (finiteEdges ++ specialEdges).grouped(2))
      add("multiply-edge", false, pair.padTo(2, BigInt(0)), inv, 0, one)
    add("zero-mask-keeps-multiply-flags", false, Seq(BigInt(0), sign), h("7f800000"), 0, one, zero = true)

    // A fixed witness detects an incorrect replacement of multiply-by-rounded-
    // reciprocal by one division. The resulting E2M1 code changes 4 -> 3.
    val x = h("3cc3ffff"); val d = h("3c600000"); val inv = inverse(7, one)
    val staged = LowpReference.convert(LowpReference.mul32(x, inv).bits, 4, decode = false)
    val direct = LowpReference.convert(LowpReference.div32(x, d).bits, 4, decode = false)
    assert(inv == h("42924925") && staged.bits == 4 && direct.bits == 3)
    add("reciprocal-rounding-witness", false, Seq(x, x | sign), inv, 7, one)

    // E2M1*E4M3 is exact in F32 (at most six significand bits), so a finite
    // decode cannot expose double rounding in that first product. Reassociation
    // through RN32(scale*alpha) does change the contract and this case catches it.
    val alpha = h("23bbda3f"); val scale = 0x5e; val code = 5
    val qs = LowpReference.mul32(LowpReference.convert(BigInt(code), 4, true).bits,
      LowpReference.convert(BigInt(scale), 2, true).bits)
    val expected = LowpReference.mul32(qs.bits, alpha)
    val sa = LowpReference.mul32(LowpReference.convert(BigInt(scale), 2, true).bits, alpha)
    val reassociated = LowpReference.mul32(LowpReference.convert(BigInt(code), 4, true).bits, sa.bits)
    assert(qs.flags == 0 && expected.bits == h("26f68e73") && reassociated.bits == h("26f68e72"))
    add("decode-association-witness", true, Seq(BigInt(code), BigInt(code | 8)), one, scale, alpha)

    val random = new Random(0x4e565034L)
    for (i <- 0 until 2048) {
      val decoding = i % 2 == 0
      val sc = random.nextInt(127)
      val a = if (i % 7 == 0) BigInt(32, random) else alphas(i % alphas.size)
      val in = Seq.fill(2)(BigInt(32, random))
      add("random-mixed", decoding, in, if (decoding) BigInt(32, random) else inverse(sc, alphas(i % alphas.size)),
        sc, a, zero = i % 29 == 0)
    }
    // Interleave direction/context transitions from the start instead of only
    // exercising each direction in a separate long batch.
    val encodes = jobs.filterNot(_.decode).toVector
    val decodes = jobs.filter(_.decode).toVector
    val interleaved = (0 until math.max(encodes.size, decodes.size)).flatMap { i =>
      encodes.lift(i).toSeq ++ decodes.lift(i).toSeq
    }
    checkStream(NvPayloadProfile.Both, 2, interleaved, "both", witnesses = 2)
  }

  it should "elaborate and stream an encode-only odd lane profile" in {
    val jobs = (0 until 96).map { i =>
      val scale = i % 127; val alpha = alphas(i % alphas.size)
      Job(i, "encode-only", false, Seq(finiteEdges(i % finiteEdges.size)), inverse(scale, alpha), scale, alpha)
    }
    checkStream(NvPayloadProfile.EncodeOnly, 1, jobs, "encode-only")
  }

  it should "elaborate and stream a decode-only odd lane profile" in {
    val jobs = (0 until 96).map { i =>
      Job(i, "decode-only", true, Seq(BigInt(i & 15)), h("7f800001"), i % 127, alphas(i % alphas.size), true)
    }
    checkStream(NvPayloadProfile.DecodeOnly, 1, jobs, "decode-only")
  }
}
