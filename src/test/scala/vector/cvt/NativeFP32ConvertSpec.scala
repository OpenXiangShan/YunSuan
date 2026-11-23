package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random

class NativeFP32ConvertSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Standalone FP32 and integer conversion"

  it should "preserve signedness rounding flags and two-cycle streaming latency" in {
    test(new NativeFP32Convert).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      assert(NativeFP32Convert.latency == 2 && dut.latency == 2)
      assert(NativeFP32Convert.initiationInterval == 1 && dut.initiationInterval == 1)
      assert(NativeConvertCapabilities.fp32Integer.coreLatency == 2)
      assert(NativeConvertCapabilities.fp32Integer.initiationInterval == 1)
      dut.clock.setTimeout(0)
      dut.io.fire.poke(false.B)
      dut.io.src.poke(0.U)
      dut.io.toFloat.poke(false.B)
      dut.io.signed.poke(false.B)
      dut.io.rm.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val random = new Random(0x32c07L)
      val pending = mutable.Map.empty[Int, Float32IntegerReference.Result]
      var cycle = 0
      var compared = 0
      var accepted = 0
      def tick(job: Option[(BigInt, Boolean, Boolean, Int)]): Unit = {
        pending.remove(cycle).foreach { expected =>
          dut.io.result.expect(expected.bits.U)
          dut.io.fflags.expect(expected.flags.U)
          compared += 1
        }
        dut.io.fire.poke(job.nonEmpty.B)
        job.foreach { case (src, toFloat, signed, rm) =>
          dut.io.src.poke(src.U); dut.io.toFloat.poke(toFloat.B)
          dut.io.signed.poke(signed.B); dut.io.rm.poke(rm.U)
          pending(cycle + 2) = if (toFloat) Float32IntegerReference.intToFloat(src, signed, rm)
            else Float32IntegerReference.floatToInt(src, signed, rm)
          accepted += 1
        }
        dut.clock.step(); cycle += 1
      }
      val edges = Seq("00000000", "80000000", "00000001", "ffffffff", "7fffffff",
        "00800000", "007fffff", "3f000000", "bf000000", "3fc00000", "bfc00000",
        "4effffff", "4f000000", "4f7fffff", "4f800000", "cf000000", "cf000001",
        "7f800000", "ff800000", "7fc00000", "7f800001").map(BigInt(_, 16))
      for (toFloat <- Seq(false, true); signed <- Seq(false, true); rm <- 0 to 4;
           src <- edges ++ Seq.fill(256)(BigInt(32, random))) {
        tick(Some((src, toFloat, signed, rm)))
        if (accepted % 29 == 0) tick(None)
      }
      for (_ <- 0 until 4) tick(None)
      assert(pending.isEmpty && accepted == compared && compared == 5540)
      println(s"NATIVE_FP32_CORE_PASS accepted=$accepted compared=$compared latency=2 ii=1 modes=5")
    }
  }
}
