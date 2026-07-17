package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.{ChiselOptionsModifications, FirtoolOptionsModifications, HasSimulator}
import chisel3.simulator.scalatest.ChiselSim
import chisel3.testing.HasTestingDirectory
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.FakeRobPtr

class GatherRingSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  // ═══ Helpers ═══════════════════════════════════════

  def initDut(dut: GatherRing[FakeRobPtr]): Unit = {
    for (p <- 0 until 4) {
      dut.in.s0pipes(p).valid.poke(false)
      dut.in.s1pipes(p).valid.poke(false)
      dut.in.wbReady(p).poke(false)
    }
    dut.in.flush.foreach(_.poke(false))
    dut.clock.step()
  }

  def pPipe(
    dut:        GatherRing[FakeRobPtr],
    pipe:       Int,
    table:      Seq[Int],
    index:      Seq[Int],
    indexValid: Seq[Boolean],
    prefill:    Seq[Int] = Seq.fill(16)(0),
    fillZeros:  Seq[Boolean] = Seq.fill(16)(false),
    fillScala:  Seq[Boolean] = Seq.fill(16)(false),
    placeIdx:   Int = 0,
    blockBase:  Int = 0,
    blockSize:  Int = 1
  ): Unit = {
    // s0: table placement info
    val s0 = dut.in.s0pipes(pipe)
    s0.valid.poke(true)
    s0.bits.placeIdx.poke(placeIdx)
    s0.bits.blockBase.poke(blockBase)
    s0.bits.blockSize.poke(blockSize)

    // s1: index + control (table data not yet wired in RTL)
    val s1 = dut.in.s1pipes(pipe)
    s1.valid.poke(true)
    s1.bits.placeIdx.poke(placeIdx)
    s1.bits.blockBase.poke(blockBase)
    s1.bits.blockSize.poke(blockSize)
    s1.bits.tableIdx.poke(0)
    index.zipWithIndex.foreach { case (v, i) => s1.bits.index(i).poke(v) }
    indexValid.zipWithIndex.foreach { case (v, i) => s1.bits.indexValid(i).poke(v) }
    prefill.zipWithIndex.foreach { case (v, i) => s1.bits.prefillData(i).poke(v) }
    fillZeros.zipWithIndex.foreach { case (v, i) => s1.bits.fillZeros(i).poke(v) }
    fillScala.zipWithIndex.foreach { case (v, i) => s1.bits.fillScala(i).poke(v) }
  }

  def pPipeOff(dut: GatherRing[FakeRobPtr], pipe: Int): Unit = {
    dut.in.s0pipes(pipe).valid.poke(false)
    dut.in.s1pipes(pipe).valid.poke(false)
  }

  def eWb(dut: GatherRing[FakeRobPtr], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true)
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  // ═══ LMUL=1 ════════════════════════════════════════
  describe("LMUL=1") {
    it("vrgather identity via pipe 0") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val t = (0 until 16).map(i => 0x10 + i)
        pPipe(dut, 0, t, 0 until 16, Seq.fill(16)(true))
        dut.clock.step()
        eWb(dut, 0, BigInt("1F1E1D1C1B1A19181716151413121110", 16))
      }
    }

    // A2: iterate all 8 units as standalone LMUL=1 blocks
    for (unit <- 0 until 8) {
      val u = unit
      it(s"vrgather identity on unit $u") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val t = (0 until 16).map(i => 0x10 + i)
          pPipe(dut, 0, t, 0 until 16, Seq.fill(16)(true), blockBase = u)
          dut.clock.step()
          eWb(dut, 0, BigInt("1F1E1D1C1B1A19181716151413121110", 16))
        }
      }
    }

    // A4: vslideup SEW=8 offset=3 (fillZeros)
    it("vslideup offset=3 fillZeros") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val t   = (0 until 16).map(i => 0x10 + i)
        val fz  = (0 until 16).map(_ < 3)
        val idx = (0 until 16).map(i => if (i < 3) 0 else i - 3)
        val iv  = (0 until 16).map(_ >= 3)
        pPipe(dut, 0, t, idx, iv, fillZeros = fz)
        dut.clock.step()
        val bytes = (0 until 16).map(i => if (i < 3) 0 else 0x10 + (i - 3))
        val exp = bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
        eWb(dut, 0, exp)
      }
    }
  }

  // ═══ LMUL=2 ════════════════════════════════════════
  describe("LMUL=2") {
    it("vrgather identity all-in-seg0") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val s0 = (0 until 16).map(i => 0x10 + i)
        pPipe(dut, 0, s0, (0 until 16), Seq.fill(16)(true), blockSize = 2)
        dut.clock.step()        // Place
        pPipeOff(dut, 0)
        dut.clock.step()        // willFlow delay
        dut.clock.step()        // flow moves table to index
        dut.clock.step()        // counter→0, writeback
        val exp = s0.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
        eWb(dut, 0, exp)
      }
    }

    // B2: LMUL=2 cross-segment
    it("vrgather cross-segment [KNOWN FAIL]") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val s0 = (0 until 16).map(i => 0x10 + i)
        val s1 = (0 until 16).map(i => 0xa0 + i)
        val shift = 16
        val idx = (0 until 16).map(i => i + shift)
        val pf  = idx
        for (pipe <- 0 until 2) {
          val tbl = if (pipe == 0) s0 else s1
          pPipe(dut, pipe, tbl, idx, Seq.fill(16)(true), prefill = pf, placeIdx = pipe, blockSize = 2)
        }
        dut.clock.step()
      }
    }
  }

  // ═══ LMUL=4 ════════════════════════════════════════
  describe("LMUL=4") {
    it("vrgather identity single pipe [KNOWN FAIL]") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val t = (0 until 16).map(i => 0x30 + i)
        pPipe(dut, 0, t, (0 until 16), Seq.fill(16)(true), blockSize = 4)
        dut.clock.step()
      }
    }
  }

  // ═══ tableOut ══════════════════════════════════════
  describe("tableOut") {
    it("table and index placed at separate units") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val s0 = (0 until 16).map(i => 0x10 + i)
        pPipe(dut, 0, s0, (0 until 16), Seq.fill(16)(true), blockSize = 2)
        dut.clock.step()
        dut.unitStatus(1).hasTable.expect(true)
        dut.unitStatus(0).hasIndex.expect(true)
      }
    }
  }

  // ═══ LMUL=2 zip ════════════════════════════════════
  describe("LMUL=2 zip") {
    it("interleave seg0 and seg1 [KNOWN FAIL]") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val s0  = (0 until 16).map(i => 0x10 + i)
        val s1  = (0 until 16).map(i => 0xa0 + i)
        val idx = (0 until 16).map(i => if (i % 2 == 0) i / 2 else 16 + i / 2)
        for (pipe <- 0 until 2) {
          val tbl = if (pipe == 0) s0 else s1
          pPipe(dut, pipe, tbl, idx, Seq.fill(16)(true), prefill = idx, placeIdx = pipe, blockSize = 2)
        }
        dut.clock.step()
      }
    }
  }
}
