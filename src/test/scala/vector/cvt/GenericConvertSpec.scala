package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import circt.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random
import _root_.yunsuan.vector.cvt._

class GenericConvertSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Parameter-pruned YunSuan conversion families"

  private case class Job(raw: BigInt, format: Int, decode: Boolean,
    scaled: Boolean = false, factor: Int = 127, rm: Int = 0)
  private val profiles = Seq(
    "all" -> ConvertParams(),
    "integer_encode" -> ConvertParams(ConvertFormat.integers, Set(ConvertDirection.Encode), Set(ConvertScale.Raw)),
    "raw_fp8_encode" -> ConvertParams(Set(ConvertFormat.E4M3, ConvertFormat.E5M2),
      Set(ConvertDirection.Encode), Set(ConvertScale.Raw)),
    "scaled_fp4" -> ConvertParams(Set(ConvertFormat.E2M1), scales = Set(ConvertScale.UE8M0)),
    "raw_float_decode" -> ConvertParams(ConvertFormat.floats, Set(ConvertDirection.Decode), Set(ConvertScale.Raw)),
    "integer_decode" -> ConvertParams(ConvertFormat.integers, Set(ConvertDirection.Decode), Set(ConvertScale.Raw)))

  private def jobs(params: ConvertParams): Seq[Job] = {
    val result = mutable.ArrayBuffer.empty[Job]
    val random = new Random(0x637674L)
    val specials = Seq("00000000", "80000000", "00000001", "007fffff", "00800000",
      "3e800000", "3f000000", "3f800000", "3f880000", "3c700000", "43e80000",
      "47700000", "42ff0000", "c3008000", "437f8000", "7f7fffff", "ff7fffff",
      "7f800000", "ff800000", "7f800001", "7fc00000", "ffc00001").map(BigInt(_, 16))
    for (format <- params.orderedFormats;
         decode <- Seq(false, true) if (if (decode) params.decode else params.encode);
         scaled <- Seq(false, true) if params.supports(format,
           if (decode) ConvertDirection.Decode else ConvertDirection.Encode,
           if (scaled) ConvertScale.UE8M0 else ConvertScale.Raw)) {
      val factors = if (scaled) Seq(0, 1, 64, 126, 127, 128, 190, 254, 255) else Seq(127)
      if (decode) {
        for (factor <- factors; code <- 0 until (1 << format.width))
          result += Job(BigInt("a519c000", 16) | (if (format.width == 4) 0xf0 else 0) | code,
            format.id, true, scaled, factor, (code + factor) % 5)
      } else {
        for (factor <- factors; raw <- specials; rm <- 0 to 4)
          result += Job(raw, format.id, false, scaled, factor, rm)
        // Every finite low code maps back to an exact raw F32 value.
        if (!scaled) for (code <- 0 until (1 << format.width)) {
          val expanded = LowpReference.convert(BigInt(code), format.id, decode = true)
          if (((expanded.bits >> 23) & 255) != 255)
            result += Job(expanded.bits, format.id, decode = false, rm = code % 5)
        }
      }
      for (index <- 0 until 256)
        result += Job(BigInt(32, random), format.id, decode, scaled,
          random.nextInt(256), index % 5)
    }
    random.shuffle(result.toSeq)
  }

  for ((profileName, params) <- profiles; pipeline <- Seq(true, false)) {
    val name = if (pipeline) profileName else profileName + "_single"
    val expectedLatency = if (!pipeline || Set("raw_float_decode", "integer_decode").contains(profileName)) 1 else 2
    it should s"match independent numeric results with declared-cycle II1 behavior for $name" in {
      test(new GenericConvert(params, pipeline)).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(0)
        val pending = mutable.Map.empty[Int, LowpReference.Result]
        var cycle = 0
        var accepted = 0
        var compared = 0
        var bubbles = 0
        var last = LowpReference.Result(BigInt(0), 0)
        val coverage = mutable.Set.empty[(Int, Boolean, Boolean, Int)]

        def tick(job: Option[Job]): Unit = {
          pending.remove(cycle).foreach { expected =>
            assert(dut.io.result.peek().litValue == expected.bits,
              s"profile=$name cycle=$cycle actual=${dut.io.result.peek().litValue.toString(16)} expected=${expected.bits.toString(16)}")
            assert(dut.io.fflags.peek().litValue == expected.flags,
              s"profile=$name cycle=$cycle actualFlags=${dut.io.fflags.peek().litValue} expectedFlags=${expected.flags}")
            last = expected
            compared += 1
          }
          dut.io.result.expect(last.bits.U)
          dut.io.fflags.expect(last.flags.U)
          // Disabled selectors may toggle freely while fire is low.
          val input = job.getOrElse(Job(BigInt("ff800001", 16), 7, decode = true, scaled = true, factor = 255, rm = 7))
          dut.io.fire.poke(job.nonEmpty.B)
          dut.io.src.poke(input.raw.U)
          dut.io.format.poke(input.format.U)
          dut.io.decode.poke(input.decode.B)
          dut.io.scaled.poke(input.scaled.B)
          dut.io.factor.poke(input.factor.U)
          dut.io.rm.poke(input.rm.U)
          job match {
            case Some(in) =>
              pending(cycle + expectedLatency) = LowpReference.convert(in.raw, in.format, in.decode, in.scaled, in.factor, in.rm)
              coverage += ((in.format, in.decode, in.scaled, in.rm))
              accepted += 1
            case None => bubbles += 1
          }
          dut.clock.step()
          cycle += 1
        }

        dut.io.fire.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        val inputs = jobs(params)
        inputs.zipWithIndex.foreach { case (job, index) =>
          tick(Some(job))
          if (index % 97 == 96) tick(None)
        }
        for (_ <- 0 until 3) tick(None)
        assert(pending.isEmpty && accepted == inputs.size && compared == accepted && compared > 0)
        for (format <- params.formats; decode <- Seq(false, true); scaled <- Seq(false, true); rm <- 0 to 4
             if params.supports(format, if (decode) ConvertDirection.Decode else ConvertDirection.Encode,
               if (scaled) ConvertScale.UE8M0 else ConvertScale.Raw)) {
          assert(coverage.contains((format.id, decode, scaled, rm)), s"missing coverage $name $format $decode $scaled $rm")
        }

        // Reset aborts outstanding unchecked requests and clears the held result.
        tick(Some(inputs.head))
        tick(Some(inputs.last))
        val aborted = pending.size
        assert(aborted == expectedLatency)
        pending.clear()
        dut.io.fire.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        cycle += 2
        last = LowpReference.Result(BigInt(0), 0)
        for (_ <- 0 until 3) tick(None)
        tick(Some(inputs.head))
        for (_ <- 0 until 3) tick(None)
        assert(pending.isEmpty && accepted == compared + aborted)
        println(s"GENERIC_CVT_PROFILE_PASS profile=$name accepted=$accepted compared=$compared aborted=$aborted bubbles=$bubbles latency=$expectedLatency ii=1 coverage_bins=${coverage.size}")
      }
    }
  }

  it should "omit disabled families and numeric format modules before lowering" in {
    var checks = 0
    for ((name, params) <- profiles) {
      val ir = ChiselStage.emitCHIRRTL(new GenericConvert(params))
      def instance(suffix: String): Boolean =
        ("(?m)\\binst [A-Za-z0-9_]*" + suffix + " of ").r.findFirstIn(ir).nonEmpty
      assert(ir.contains("module EncodeFamily") == params.encode, name)
      assert(ir.contains("module DecodeFamily") == params.decode, name)
      checks += 2
      for (format <- ConvertFormat.all) {
        assert(instance(s"encode_$format") == (params.encode && params.formats.contains(format)), s"encode $name $format")
        checks += 1
        if (ConvertFormat.floats.contains(format)) {
          assert(instance(s"decode_$format") == (params.decode && params.formats.contains(format)), s"decode $name $format")
          checks += 1
        }
      }
      assert(instance("decode_integer") ==
        (params.decode && (params.formats & ConvertFormat.integers).nonEmpty), name)
      checks += 1
      if (name == "raw_fp8_encode") {
        assert(ir.contains("module FP32ToFloat8Format") && !ir.contains("module FP32ToMXFPFormat"))
        checks += 1
      }
      if (name == "raw_float_decode") {
        val scaleReferences = ir.linesIterator.filter(line =>
          line.contains("io.factor") && !line.trim.startsWith("connect")).toSeq
        assert(scaleReferences.isEmpty, s"raw decode uses a scale: ${scaleReferences.mkString("; ")}")
        checks += 1
      }
    }
    intercept[IllegalArgumentException] { ConvertParams(formats = Set.empty) }
    intercept[IllegalArgumentException] { ConvertParams(directions = Set.empty) }
    intercept[IllegalArgumentException] { ConvertParams(scales = Set.empty) }
    intercept[IllegalArgumentException] { ConvertParams(ConvertFormat.integers, scales = Set(ConvertScale.UE8M0)) }
    intercept[IllegalArgumentException] { ConvertParams(ConvertFormat.integers) }
    println(s"GENERIC_CVT_PRUNING_PASS profiles=${profiles.size} structural_checks=$checks invalid_profiles=5")
  }

  it should "reject disabled formats, directions, scale modes and unsupported rounding on fire" in {
    val params = ConvertParams(Set(ConvertFormat.E4M3), Set(ConvertDirection.Encode), Set(ConvertScale.Raw))
    val invalid = Seq(
      Job(0, ConvertFormat.E5M2.id, decode = false),
      Job(0, ConvertFormat.E4M3.id, decode = true),
      Job(0, ConvertFormat.E4M3.id, decode = false, scaled = true),
      Job(0, ConvertFormat.E4M3.id, decode = false, rm = 5))
    for (input <- invalid) {
      intercept[ChiselAssertionError] {
        test(new GenericConvert(params)) { dut =>
          dut.io.fire.poke(false.B)
          dut.io.src.poke(input.raw.U)
          dut.io.format.poke(input.format.U)
          dut.io.decode.poke(input.decode.B)
          dut.io.scaled.poke(input.scaled.B)
          dut.io.factor.poke(input.factor.U)
          dut.io.rm.poke(input.rm.U)
          dut.reset.poke(true.B)
          dut.clock.step(2)
          dut.reset.poke(false.B)
          dut.io.fire.poke(true.B)
          dut.clock.step()
        }
      }
    }
    println(s"GENERIC_CVT_REQUEST_ASSERT_PASS rejected=${invalid.size}")
  }

  it should "decode generic signed and unsigned integers exactly including four-bit endpoints" in {
    var comparisons = 0
    for (width <- Seq(1, 4, 8, 16, 24)) {
      test(new IntegerToFP32(width)) { dut =>
        val mask = (BigInt(1) << width) - 1
        val random = new Random(width)
        val values = if (width <= 8) (BigInt(0) to mask).toSeq else
          (Seq(BigInt(0), BigInt(1), mask, mask - 1, BigInt(1) << (width - 1), (BigInt(1) << (width - 1)) - 1) ++
            Seq.fill(128)(BigInt(width, random))).distinct
        for (signed <- Seq(false, true); raw <- values) {
          val integer = raw - (if (signed && raw.testBit(width - 1)) BigInt(1) << width else BigInt(0))
          val expected = LowpReference.round32(integer, 0)
          assert(expected.flags == 0)
          dut.io.src.poke(raw.U)
          dut.io.signed.poke(signed.B)
          dut.io.result.expect(expected.bits.U)
          comparisons += 1
        }
      }
    }
    assert(comparisons >= 1000)
    println(s"GENERIC_CVT_INTEGER_PASS widths=1,4,8,16,24 comparisons=$comparisons")
  }
}
