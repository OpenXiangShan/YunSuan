package yunsuan.vector

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.experimental.BoringUtils.bore
import org.scalatest.funspec.AnyFunSpec
import yunsuan.fpu.FloatDivider

class VectorFloatDividerAckHarness extends Module {
  private val core = Module(new VectorFloatDivider)
  val io = IO(chiselTypeOf(core.io))
  io <> core.io
  val rawReady = IO(Output(Bool()))
  val naturalAhead = IO(Output(Bool()))
  private val div = core.u_vector_float_divider_r64
  private val sqrt = core.u_vector_float_sqrt_r16
  private val divState = bore(div.fsm_q)
  private val sqrtState = bore(sqrt.fsm_q)
  private val selectedSqrt = bore(core.ownerIsSqrt)
  rawReady := bore(core.ownerActive) && Mux(selectedSqrt, sqrtState(3), divState(5))
  naturalAhead := bore(div.naturalAhead) || bore(sqrt.naturalAhead)
}

class VectorFloatDividerWbAckSpec extends AnyFunSpec with ChiselSim {
  private def pack(value: BigInt, format: Int): BigInt = {
    val width = Seq(0, 16, 32, 64)(format)
    (0 until 64 / width).map(i => value << (i * width)).reduce(_ | _)
  }

  private def flags(value: Int, format: Int): BigInt = {
    val width = Seq(0, 16, 32, 64)(format)
    (0 until 64 / width).map(i => BigInt(value) << (i * 5)).reduce(_ | _)
  }

  private def init(dut: VectorFloatDividerAckHarness): Unit = {
    dut.io.start_valid_i.poke(false.B)
    dut.io.flush_i.poke(false.B)
    dut.io.finish_ready_i.poke(false.B)
    dut.io.wakeupSuccess.poke(true.B)
    dut.io.fp_format_i.poke(3.U)
    dut.io.opa_i.poke(0.U)
    dut.io.opb_i.poke(0.U)
    dut.io.frs1_i.poke(0.U)
    dut.io.frs2_i.poke(0.U)
    dut.io.is_frs1_i.poke(false.B)
    dut.io.is_frs2_i.poke(false.B)
    dut.io.is_sqrt_i.poke(false.B)
    dut.io.is_vec_i.poke(true.B)
    dut.io.rm_i.poke(0.U)
    dut.io.fp_aIsFpCanonicalNAN.poke(false.B)
    dut.io.fp_bIsFpCanonicalNAN.poke(false.B)
    dut.clock.step()
  }

  private def launch(dut: VectorFloatDividerAckHarness, format: Int, sqrt: Boolean,
                     a: BigInt, b: BigInt, rm: Int = 0): Unit = {
    dut.io.start_ready_o.expect(true.B)
    dut.io.start_valid_i.poke(true.B)
    dut.io.fp_format_i.poke(format.U)
    dut.io.opa_i.poke(pack(a, format).U)
    dut.io.opb_i.poke(pack(b, format).U)
    dut.io.is_sqrt_i.poke(sqrt.B)
    dut.io.rm_i.poke(rm.U)
    dut.io.finish_ready_i.poke(false.B)
    dut.io.wakeupSuccess.poke(false.B)
    dut.clock.step()
    dut.io.start_valid_i.poke(false.B)
    // Later input traffic must not change the active operation or rounding.
    dut.io.fp_format_i.poke((if (format == 3) 1 else 3).U)
    dut.io.is_sqrt_i.poke((!sqrt).B)
    dut.io.rm_i.poke(4.U)
    dut.io.opa_i.poke(0.U)
    dut.io.opb_i.poke(0.U)
  }

  private def waitAhead(dut: VectorFloatDividerAckHarness): Int = {
    var elapsed = 1
    while (!dut.io.outValidAhead3Cycle.peek().litToBoolean && elapsed < 80) {
      dut.io.finish_valid_o.expect(false.B)
      dut.io.start_ready_o.expect(false.B)
      dut.clock.step()
      elapsed += 1
    }
    assert(elapsed < 80, "natural ahead was lost")
    dut.naturalAhead.expect(true.B)
    dut.rawReady.expect(false.B)
    elapsed
  }

  private def finish(dut: VectorFloatDividerAckHarness, expected: BigInt,
                     expectedFlags: BigInt, naturalDistance: Boolean = false): Unit = {
    dut.io.outValidAhead3Cycle.expect(true.B)
    dut.io.wakeupSuccess.poke(true.B)
    dut.io.finish_valid_o.expect(false.B)
    dut.clock.step()
    // A later busy table may now contain this owner's own reservation.
    dut.io.wakeupSuccess.poke(false.B)
    for (_ <- 1 to 2) {
      if (naturalDistance) dut.rawReady.expect(false.B)
      dut.io.outValidAhead3Cycle.expect(false.B)
      dut.io.finish_valid_o.expect(false.B)
      dut.io.start_ready_o.expect(false.B)
      dut.clock.step()
    }
    dut.rawReady.expect(true.B)
    dut.io.finish_valid_o.expect(true.B)
    dut.io.outValidAhead3Cycle.expect(false.B)
    dut.io.fpdiv_res_o.expect(expected.U)
    dut.io.fflags_o.expect(expectedFlags.U)
    for (_ <- 0 until 4) {
      dut.clock.step()
      dut.io.start_ready_o.expect(false.B)
      dut.io.finish_valid_o.expect(true.B)
      dut.io.fpdiv_res_o.expect(expected.U)
      dut.io.fflags_o.expect(expectedFlags.U)
    }
    dut.io.finish_ready_i.poke(true.B)
    dut.clock.step()
    dut.io.finish_ready_i.poke(false.B)
    dut.io.finish_valid_o.expect(false.B)
    dut.io.outValidAhead3Cycle.expect(false.B)
    dut.io.start_ready_o.expect(true.B)
  }

  for (format <- 1 to 3) {
    it(s"promises FP${Seq(0, 16, 32, 64)(format)} div ITER1 and sqrt ITER2 exactly three cycles before raw completion") {
      simulate(new VectorFloatDividerAckHarness) { dut =>
        init(dut)
        val one = Seq(BigInt(0), BigInt("3c00", 16), BigInt("3f800000", 16), BigInt("3ff0000000000000", 16))(format)
        val two = Seq(BigInt(0), BigInt("4000", 16), BigInt("40000000", 16), BigInt("4000000000000000", 16))(format)
        val three = Seq(BigInt(0), BigInt("4200", 16), BigInt("40400000", 16), BigInt("4008000000000000", 16))(format)
        val third = Seq(BigInt(0), BigInt("3555", 16), BigInt("3eaaaaab", 16), BigInt("3fd5555555555555", 16))(format)
        val sqrtTwo = Seq(BigInt(0), BigInt("3da8", 16), BigInt("3fb504f3", 16), BigInt("3ff6a09e667f3bcd", 16))(format)
        for ((sqrt, a, b, expected) <- Seq((false, one, three, third), (true, two, one, sqrtTwo))) {
          launch(dut, format, sqrt, a, b)
          val cycle = waitAhead(dut)
          println(s"VFDiv format=$format sqrt=$sqrt naturalAhead=$cycle rawCompletion=${cycle + 3}")
          finish(dut, pack(expected, format), flags(1, format), naturalDistance = true)
        }
      }
    }

    it(s"keeps FP${Seq(0, 16, 32, 64)(format)} denormal data and flags stable through rejected requests") {
      simulate(new VectorFloatDividerAckHarness) { dut =>
        init(dut)
        val minNormal = Seq(BigInt(0), BigInt("0400", 16), BigInt("00800000", 16), BigInt("0010000000000000", 16))(format)
        val two = Seq(BigInt(0), BigInt("4000", 16), BigInt("40000000", 16), BigInt("4000000000000000", 16))(format)
        val sqrtMinSub = Seq(BigInt(0), BigInt("0c00", 16),
          BigInt(java.lang.Float.floatToRawIntBits(math.sqrt(java.lang.Float.intBitsToFloat(1).toDouble).toFloat) & 0xffffffffL),
          BigInt(java.lang.Double.doubleToRawLongBits(math.sqrt(java.lang.Double.longBitsToDouble(1)))))(format)
        val sqrtFlags = if (format == 2) 1 else 0
        for ((sqrt, a, b, expected, fflags) <- Seq(
          (false, minNormal, two, minNormal >> 1, 0),
          (true, BigInt(1), two, sqrtMinSub, sqrtFlags))) {
          launch(dut, format, sqrt, a, b)
          waitAhead(dut)
          for (cycle <- 1 to 8) {
            dut.io.outValidAhead3Cycle.expect(true.B)
            dut.io.finish_valid_o.expect(false.B)
            dut.clock.step()
            if (cycle < 3) {
              dut.rawReady.expect(false.B)
            } else {
              dut.rawReady.expect(true.B)
              dut.io.fpdiv_res_o.expect(pack(expected, format).U)
              dut.io.fflags_o.expect(flags(fflags, format).U)
            }
          }
          finish(dut, pack(expected, format), flags(fflags, format))
        }
      }
    }
  }

  it("handles packed special values without raw finish escaping a denied reservation") {
    simulate(new VectorFloatDividerAckHarness) { dut =>
      init(dut)
      for (format <- 1 to 3) {
        val one = Seq(BigInt(0), BigInt("3c00", 16), BigInt("3f800000", 16), BigInt("3ff0000000000000", 16))(format)
        val inf = Seq(BigInt(0), BigInt("7c00", 16), BigInt("7f800000", 16), BigInt("7ff0000000000000", 16))(format)
        val nan = Seq(BigInt(0), BigInt("7e00", 16), BigInt("7fc00000", 16), BigInt("7ff8000000000000", 16))(format)
        val sign = BigInt(1) << (Seq(0, 16, 32, 64)(format) - 1)
        for ((sqrt, a, b, expected, fflags) <- Seq(
          (false, one, BigInt(0), inf, 8),
          (false, BigInt(0), BigInt(0), nan, 16),
          (false, inf, one, inf, 0),
          (false, one, inf, BigInt(0), 0),
          (true, one | sign, one, nan, 16),
          (true, sign, one, sign, 0))) {
          launch(dut, format, sqrt, a, b)
          waitAhead(dut)
          dut.clock.step(5)
          dut.io.outValidAhead3Cycle.expect(true.B)
          dut.io.finish_valid_o.expect(false.B)
          finish(dut, pack(expected, format), flags(fflags, format))
        }
      }
    }
  }

  it("preserves distinct FP16 element flags and results while the aggregate request is denied") {
    simulate(new VectorFloatDividerAckHarness) { dut =>
      init(dut)
      val a = BigInt("7c003c0000003c00", 16)
      val b = BigInt("3c00420000000000", 16)
      val expected = BigInt("7c0035557e007c00", 16)
      val expectedFlags = BigInt(8) | (BigInt(16) << 5) | (BigInt(1) << 10)
      dut.io.start_valid_i.poke(true.B)
      dut.io.fp_format_i.poke(1.U)
      dut.io.opa_i.poke(a.U)
      dut.io.opb_i.poke(b.U)
      dut.io.wakeupSuccess.poke(false.B)
      dut.clock.step()
      dut.io.start_valid_i.poke(false.B)
      waitAhead(dut)
      dut.clock.step(5)
      for (_ <- 0 until 4) {
        dut.io.outValidAhead3Cycle.expect(true.B)
        dut.io.finish_valid_o.expect(false.B)
        dut.io.fpdiv_res_o.expect(expected.U)
        dut.io.fflags_o.expect(expectedFlags.U)
        dut.clock.step()
      }
      finish(dut, expected, expectedFlags)
    }
  }

  it("cancels compute, pending, accepted and due owners without producing idle retry requests") {
    simulate(new VectorFloatDividerAckHarness) { dut =>
      init(dut)
      for (sqrt <- Seq(false, true); phase <- Seq("compute", "pending", "accepted", "d1", "d2", "due")) {
        launch(dut, 3, sqrt, BigInt("4000000000000000", 16), BigInt("4008000000000000", 16))
        if (phase == "compute") {
          dut.clock.step()
        } else {
          waitAhead(dut)
          if (phase == "pending") {
            dut.clock.step(7)
          } else if (phase != "accepted") {
            dut.io.wakeupSuccess.poke(true.B)
            dut.clock.step()
            dut.io.wakeupSuccess.poke(false.B)
            if (phase == "d2") dut.clock.step()
            if (phase == "due") dut.clock.step(2)
          }
        }
        dut.io.flush_i.poke(true.B)
        dut.io.wakeupSuccess.poke(true.B)
        dut.io.outValidAhead3Cycle.expect(false.B)
        dut.io.finish_valid_o.expect(false.B)
        dut.clock.step()
        dut.io.flush_i.poke(false.B)
        dut.io.wakeupSuccess.poke(false.B)
        for (_ <- 0 until 4) {
          dut.io.start_ready_o.expect(true.B)
          dut.io.outValidAhead3Cycle.expect(false.B)
          dut.io.finish_valid_o.expect(false.B)
          dut.clock.step()
        }
      }
    }
  }

  it("requests short div and sqrt special results at start and still waits accepted plus three") {
    simulate(new VectorFloatDividerAckHarness) { dut =>
      init(dut)
      dut.io.is_vec_i.poke(false.B)
      for (sqrt <- Seq(false, true)) {
        dut.io.start_valid_i.poke(true.B)
        dut.io.is_sqrt_i.poke(sqrt.B)
        dut.io.fp_format_i.poke(3.U)
        dut.io.opa_i.poke(0.U)
        dut.io.opb_i.poke(BigInt("3ff0000000000000", 16).U)
        dut.io.wakeupSuccess.poke(false.B)
        dut.io.outValidAhead3Cycle.expect(true.B)
        dut.clock.step()
        dut.io.start_valid_i.poke(false.B)
        for (_ <- 0 until 5) {
          dut.io.outValidAhead3Cycle.expect(true.B)
          dut.io.finish_valid_o.expect(false.B)
          dut.rawReady.expect(true.B)
          dut.io.fpdiv_res_o.expect(0.U)
          dut.io.fflags_o.expect(0.U)
          dut.clock.step()
        }
        finish(dut, BigInt(0), BigInt(0))
      }
    }
  }

  it("retains the scalar FDiv reference request, retry and accepted-plus-three contract") {
    simulate(new FloatDivider) { dut =>
      dut.io.start_valid_i.poke(false.B)
      dut.io.flush_i.poke(false.B)
      dut.io.finish_ready_i.poke(false.B)
      dut.io.wakeupSuccess.poke(true.B)
      dut.io.fp_format_i.poke(3.U)
      dut.io.rm_i.poke(0.U)
      dut.io.is_sqrt_i.poke(false.B)
      dut.io.opa_i.poke(0.U)
      dut.io.opb_i.poke(0.U)
      dut.io.fp_aIsFpCanonicalNAN.poke(false.B)
      dut.io.fp_bIsFpCanonicalNAN.poke(false.B)
      dut.clock.step()
      for (sqrt <- Seq(false, true)) {
        dut.io.start_ready_o.expect(true.B)
        dut.io.start_valid_i.poke(true.B)
        dut.io.is_sqrt_i.poke(sqrt.B)
        dut.io.opa_i.poke(BigInt("4000000000000000", 16).U)
        dut.io.opb_i.poke(BigInt("4008000000000000", 16).U)
        if (dut.io.outValidAhead3Cycle.peek().litToBoolean) {
          dut.io.wakeupSuccess.poke(false.B)
        }
        dut.clock.step()
        dut.io.start_valid_i.poke(false.B)
        var elapsed = 0
        while (!dut.io.outValidAhead3Cycle.peek().litToBoolean && elapsed < 80) {
          dut.clock.step()
          elapsed += 1
        }
        assert(elapsed < 80)
        dut.io.wakeupSuccess.poke(false.B)
        for (_ <- 0 until 6) {
          dut.io.outValidAhead3Cycle.expect(true.B)
          dut.io.finish_valid_o.expect(false.B)
          dut.clock.step()
        }
        dut.io.wakeupSuccess.poke(true.B)
        dut.clock.step()
        for (_ <- 1 to 2) {
          dut.io.finish_valid_o.expect(false.B)
          dut.clock.step()
        }
        dut.io.finish_valid_o.expect(true.B)
        dut.io.finish_ready_i.poke(true.B)
        dut.clock.step()
        dut.io.finish_ready_i.poke(false.B)
      }
    }
  }
}
