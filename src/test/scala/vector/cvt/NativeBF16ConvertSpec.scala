package yunsuan.vector.cvt

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class NativeBF16ConvertSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Standalone one-cycle BF16 conversion"

  it should "return native BF16 results on the first capture with streaming control and reset" in {
    test(new NativeBF16Convert).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      assert(BF16Convert.latency == 1 && NativeBF16Convert.latency == 1 && dut.latency == 1)
      assert(BF16Convert.initiationInterval == 1 && NativeBF16Convert.initiationInterval == 1 && dut.initiationInterval == 1)
      assert(NativeConvertCapabilities.brainFloat.coreLatency == 1)
      assert(NativeConvertCapabilities.brainFloat.initiationInterval == 1)
      dut.clock.setTimeout(0)
      dut.io.fire.poke(false.B); dut.io.src.poke(0.U); dut.io.narrow.poke(false.B); dut.io.rm.poke(0.U)
      dut.reset.poke(true.B); dut.clock.step(2); dut.reset.poke(false.B)
      val random = new Random(0x5132bf16L)
      var compared = 0; var bubbles = 0
      var held = BF16Reference.Result(BigInt(0), 0)
      def submit(raw: BigInt, narrow: Boolean, rm: Int): Unit = {
        dut.io.fire.poke(true.B); dut.io.src.poke(raw.U); dut.io.narrow.poke(narrow.B); dut.io.rm.poke(rm.U)
        val expected = if (narrow) BF16Reference.narrow(raw, rm) else BF16Reference.widen(raw)
        dut.clock.step()
        dut.io.result.expect(expected.bits.U); dut.io.fflags.expect(expected.flags.U)
        held = expected; compared += 1
      }
      for (code <- 0 until 65536) {
        submit(BigInt(code), false, code % 5)
        submit(BigInt(code) << 16, true, code % 5)
        if (code % 257 == 0) {
          dut.io.fire.poke(false.B); dut.io.src.poke(BigInt("ff800001",16).U)
          dut.io.narrow.poke(true.B); dut.io.rm.poke(4.U); dut.clock.step()
          dut.io.result.expect(held.bits.U); dut.io.fflags.expect(held.flags.U); bubbles += 1
        }
      }
      for (i <- 0 until 4096) submit(BigInt(32, random), true, i % 5)
      dut.io.fire.poke(false.B); dut.reset.poke(true.B); dut.clock.step(); dut.reset.poke(false.B)
      dut.io.result.expect(0.U); dut.io.fflags.expect(0.U)
      submit(BigInt("3f818000",16), true, 0)
      println(s"CVT_SINGLE_BF16_PASS compared=$compared bubbles=$bubbles latency=1 ii=1 reset=1")
    }
  }

}
