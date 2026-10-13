package yunsuan.vector

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.funspec.AnyFunSpec

class VectorIdivTimingProbe extends Module {
  val core = Module(new VectorIdiv)
  val in = IO(new VectorIdiv.In)
  val out = IO(new VectorIdiv.Out)
  val rawReady = IO(Output(Bool()))
  core.in <> in
  out <> core.out
  private val rawState = BoringUtils.bore(core.stateReg)
  rawReady := rawState(core.output)
}

class VIDivChildTimingProbe extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val flush = Input(Bool())
    val signed = Input(Bool())
    val x = Input(Vec(4, UInt(64.W)))
    val d = Input(Vec(4, UInt(64.W)))
    val ready = Input(Bool())
    val latency = Output(Vec(4, Valid(UInt(VectorIdiv.NonFixedLatencyWidth.W))))
    val resultValid = Output(Vec(4, Bool()))
    val q = Output(Vec(4, UInt(64.W)))
    val rem = Output(Vec(4, UInt(64.W)))
  })
  for ((width, i) <- Seq(8, 16, 32, 64).zipWithIndex) {
    if (width == 8) {
      val child = Module(new I8DivNr4)
      child.io.sign := io.signed
      child.io.flush := io.flush
      child.io.div_in_valid := io.valid
      child.io.dividend := io.x(i)(width - 1, 0)
      child.io.divisor := io.d(i)(width - 1, 0)
      child.io.div_out_ready := io.ready
      io.latency(i) := child.io.div_latency
      io.resultValid(i) := child.io.div_out_valid
      io.q(i) := child.io.div_out_q
      io.rem(i) := child.io.div_out_rem
    } else {
      val child = Module(new SRT16Divint(width))
      child.io.sign := io.signed
      child.io.flush := io.flush
      child.io.div_in_valid := io.valid
      child.io.sel8 := false.B
      child.io.sel16 := (width == 16).B
      child.io.sel32 := (width == 32).B
      child.io.sel64 := (width == 64).B
      child.io.dividend := io.x(i)(width - 1, 0)
      child.io.divisor := io.d(i)(width - 1, 0)
      child.io.div_out_ready := io.ready
      io.latency(i) := child.io.div_latency
      io.resultValid(i) := child.io.div_out_valid
      io.q(i) := child.io.div_out_q
      io.rem(i) := child.io.div_out_rem
    }
  }
}

class VectorIdivWritebackSpec extends AnyFunSpec with ChiselSim {
  private val widths = Seq(8, 16, 32, 64)
  private def mask(w: Int): BigInt = (BigInt(1) << w) - 1
  private def signedValue(x: BigInt, w: Int): BigInt =
    if (x.testBit(w - 1)) x - (BigInt(1) << w) else x
  private def expected(x: BigInt, d: BigInt, w: Int, signed: Boolean): (BigInt, BigInt) = {
    val a = if (signed) signedValue(x, w) else x
    val b = if (signed) signedValue(d, w) else d
    if (b == 0) (mask(w), x)
    else ((a / b) & mask(w), (a % b) & mask(w))
  }
  private def pack(xs: Seq[BigInt], width: Int): BigInt =
    xs.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (x, i)) => acc | ((x & mask(width)) << (i * width)) }

  private def init(dut: VectorIdivTimingProbe): Unit = {
    dut.in.ex0.valid.poke(false.B)
    dut.in.ex0.bits.ctrl.flush.poke(false.B)
    dut.in.wakeupSuccess.poke(true.B)
    dut.out.ex0.ready.poke(true.B)
    dut.clock.step()
  }
  private def launch(dut: VectorIdivTimingProbe, width: Int, signed: Boolean,
                     xs: Seq[BigInt], ds: Seq[BigInt]): Unit = {
    dut.in.ex0.ready.expect(true.B)
    dut.in.ex0.bits.ctrl.sign.poke(signed.B)
    dut.in.ex0.bits.ctrl.sel8.poke((width == 8).B)
    dut.in.ex0.bits.ctrl.sel16.poke((width == 16).B)
    dut.in.ex0.bits.ctrl.sel32.poke((width == 32).B)
    dut.in.ex0.bits.ctrl.sel64.poke((width == 64).B)
    dut.in.ex0.bits.data.dividend_v.poke(pack(xs, width).U)
    dut.in.ex0.bits.data.divisor_v.poke(pack(ds, width).U)
    dut.in.ex0.valid.poke(true.B)
    dut.clock.step()
    dut.in.ex0.valid.poke(false.B)
  }
  private def checkResult(dut: VectorIdivTimingProbe, width: Int, signed: Boolean,
                          xs: Seq[BigInt], ds: Seq[BigInt]): Unit = {
    val results = xs.zip(ds).map { case (x, d) => expected(x, d, width, signed) }
    dut.out.ex0.bits.q_v.expect(pack(results.map(_._1), width).U)
    dut.out.ex0.bits.rem_v.expect(pack(results.map(_._2), width).U)
  }
  private def operands(width: Int): (Seq[BigInt], Seq[BigInt]) = {
    val n = 128 / width
    (Seq.tabulate(n)(i => mask(width) - i * 17), Seq.tabulate(n)(i => BigInt(3 + i % 3)))
  }

  it("calibrates each child report against its actual raw output cycle") {
    simulate(new VIDivChildTimingProbe) { dut =>
      dut.io.valid.poke(false.B)
      dut.io.flush.poke(false.B)
      dut.io.ready.poke(false.B)
      dut.clock.step()
      for (signed <- Seq(false, true); kind <- 0 until 5) {
        val inputs = widths.map { w =>
          kind match {
            case 0 => (mask(w) - 17, BigInt(3))
            case 1 => (BigInt(1) << (w - 1), mask(w))
            case 2 => (mask(w), BigInt(0))
            case 3 => (BigInt(0), BigInt(9))
            case _ => (mask(w) - 123, BigInt(1))
          }
        }
        dut.io.signed.poke(signed.B)
        for (i <- widths.indices) {
          dut.io.x(i).poke(inputs(i)._1.U)
          dut.io.d(i).poke(inputs(i)._2.U)
        }
        dut.io.valid.poke(true.B)
        dut.clock.step()
        dut.io.valid.poke(false.B)
        val reported = Array.fill(4)(-1)
        val completed = Array.fill(4)(-1)
        for (cycle <- 1 to 40 if completed.contains(-1)) {
          for ((w, i) <- widths.zipWithIndex) {
            if (dut.io.latency(i).valid.peek().litToBoolean) reported(i) = dut.io.latency(i).bits.peek().litValue.toInt
            if (dut.io.resultValid(i).peek().litToBoolean && completed(i) < 0) {
              completed(i) = cycle
              assert(reported(i) == cycle, s"child $w signed=$signed kind=$kind reported=${reported(i)} raw=$cycle")
              val (q, rem) = expected(inputs(i)._1, inputs(i)._2, w, signed)
              dut.io.q(i).expect(q.U)
              dut.io.rem(i).expect(rem.U)
            }
          }
          dut.clock.step()
        }
        assert(completed.forall(_ > 0))
        dut.io.ready.poke(true.B)
        dut.clock.step()
        dut.io.ready.poke(false.B)
      }
    }
  }

  it("accepts long unsigned divisions three cycles before raw completion at every SEW") {
    simulate(new VectorIdivTimingProbe) { dut =>
      init(dut)
      for (w <- widths) {
        val (xs, ds) = operands(w)
        launch(dut, w, signed = false, xs, ds)
        var accepted = -1
        var raw = -1
        var completed = -1
        var count = 0
        for (cycle <- 1 to 50 if completed < 0) {
          if (dut.out.ex0.bits.div_latency.valid.peek().litToBoolean) {
            assert(accepted < 0)
            accepted = cycle
            count += 1
            dut.out.ex0.bits.div_latency.bits.expect(3.U)
            dut.rawReady.expect(false.B)
          }
          if (dut.rawReady.peek().litToBoolean && raw < 0) raw = cycle
          if (dut.out.ex0.valid.peek().litToBoolean) {
            completed = cycle
            checkResult(dut, w, signed = false, xs, ds)
          }
          dut.clock.step()
        }
        assert(count == 1 && accepted > 0)
        assert(raw == accepted + 3, s"SEW=$w accepted=$accepted raw=$raw completion=$completed")
        assert(completed == raw)
        info(s"VIDiv SEW=$w start=0 accepted=$accepted raw=$raw completion=$completed")
        dut.in.ex0.ready.expect(true.B)
      }
    }
  }

  it("preserves signed/unsigned quotient and remainder, zero and overflow for every SEW") {
    simulate(new VectorIdivTimingProbe) { dut =>
      init(dut)
      val random = new scala.util.Random(20261013L)
      for (w <- widths; signed <- Seq(false, true); kind <- 0 until 10) {
        val pairs = Seq.tabulate(128 / w) { i =>
          (kind + i) % 10 match {
            case 0 => (mask(w), BigInt(0))
            case 1 => (BigInt(1) << (w - 1), mask(w))
            case 2 => (BigInt(0), BigInt(3))
            case 3 => (mask(w) - 41, BigInt(1))
            case 4 => (BigInt(1), mask(w))
            case _ => (BigInt(w, random), BigInt(w, random))
          }
        }
        val (xs, ds) = (pairs.map(_._1), pairs.map(_._2))
        launch(dut, w, signed, xs, ds)
        var accepted = -1
        var raw = -1
        var completed = -1
        for (cycle <- 1 to 50 if completed < 0) {
          if (dut.out.ex0.bits.div_latency.valid.peek().litToBoolean) {
            assert(accepted < 0)
            accepted = cycle
          }
          if (dut.rawReady.peek().litToBoolean && raw < 0) raw = cycle
          if (dut.out.ex0.valid.peek().litToBoolean) {
            completed = cycle
            checkResult(dut, w, signed, xs, ds)
          }
          dut.clock.step()
        }
        assert(accepted > 0 && completed == accepted + 3, s"SEW=$w signed=$signed kind=$kind accepted=$accepted completion=$completed")
        if (raw >= 7) assert(completed == raw, s"long SEW=$w signed=$signed kind=$kind raw=$raw completion=$completed")
      }
    }
  }

  it("continues computation under denied requests, then holds the accepted output until ready") {
    simulate(new VectorIdivTimingProbe) { dut =>
      init(dut)
      val (xs, ds) = operands(64)
      dut.in.wakeupSuccess.poke(false.B)
      dut.out.ex0.ready.poke(false.B)
      launch(dut, 64, signed = false, xs, ds)
      var requested = false
      for (_ <- 0 until 40) {
        requested ||= dut.out.outValidAhead3Cycle.peek().litToBoolean
        if (requested) dut.out.outValidAhead3Cycle.expect(true.B)
        dut.out.ex0.bits.div_latency.valid.expect(false.B)
        dut.out.ex0.valid.expect(false.B)
        dut.in.ex0.ready.expect(false.B)
        dut.clock.step()
      }
      assert(requested)
      dut.rawReady.expect(true.B)
      dut.in.wakeupSuccess.poke(true.B)
      dut.out.ex0.bits.div_latency.valid.expect(true.B)
      dut.clock.step()
      dut.in.wakeupSuccess.poke(false.B)
      for (_ <- 1 until 3) {
        dut.out.outValidAhead3Cycle.expect(false.B)
        dut.out.ex0.valid.expect(false.B)
        dut.clock.step()
      }
      for (_ <- 0 until 5) {
        dut.out.ex0.valid.expect(true.B)
        dut.out.ex0.bits.div_latency.valid.expect(false.B)
        dut.in.ex0.ready.expect(false.B)
        checkResult(dut, 64, signed = false, xs, ds)
        dut.clock.step()
      }
      dut.out.ex0.ready.poke(true.B)
      dut.clock.step()
      dut.in.ex0.ready.expect(true.B)
      dut.out.ex0.valid.expect(false.B)
    }
  }

  it("flushes compute, request and accepted T through T+3, then accepts a fresh owner") {
    simulate(new VectorIdivTimingProbe) { dut =>
      init(dut)
      val (xs, ds) = operands(64)
      for (offset <- Seq(-2, -1, 0, 1, 2, 3)) {
        dut.in.wakeupSuccess.poke(false.B)
        launch(dut, 64, signed = false, xs, ds)
        if (offset == -2) dut.clock.step(2)
        else {
          var waiting = 0
          while (!dut.out.outValidAhead3Cycle.peek().litToBoolean && waiting < 40) {
            dut.clock.step()
            waiting += 1
          }
          dut.out.outValidAhead3Cycle.expect(true.B)
          if (offset >= 0) {
            dut.in.wakeupSuccess.poke(true.B)
            dut.clock.step(offset)
          } else dut.clock.step(8)
        }
        dut.in.ex0.bits.ctrl.flush.poke(true.B)
        dut.out.outValidAhead3Cycle.expect(false.B)
        dut.out.ex0.bits.div_latency.valid.expect(false.B)
        dut.out.ex0.valid.expect(false.B)
        dut.clock.step()
        dut.in.ex0.bits.ctrl.flush.poke(false.B)
        dut.in.wakeupSuccess.poke(true.B)
        for (_ <- 0 until 5) {
          dut.out.outValidAhead3Cycle.expect(false.B)
          dut.out.ex0.valid.expect(false.B)
          dut.in.ex0.ready.expect(true.B)
          dut.clock.step()
        }
      }
      launch(dut, 64, signed = false, xs, ds)
      var completed = false
      for (_ <- 0 until 50 if !completed) {
        completed = dut.out.ex0.valid.peek().litToBoolean
        if (completed) checkResult(dut, 64, signed = false, xs, ds)
        dut.clock.step()
      }
      assert(completed)
    }
  }
}
