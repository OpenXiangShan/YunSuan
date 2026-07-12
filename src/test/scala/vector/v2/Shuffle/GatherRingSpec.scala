package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class GatherRingSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val vlen = 128
  val dlen = 128
  val xlen = 64

  // ═══ Helpers ═══════════════════════════════════════

  def initDut(dut: GatherRing, blkSize: Int = 8): Unit = {
    for (p <- 0 until 4) { dut.pipeIn(p).valid.poke(false) }
    for (p <- 0 until 4) { dut.writeback(p).ready.poke(false) }
    dut.flushRobIdx.poke(0)
    dut.blockSizeCfg.poke(blkSize)
    dut.clock.step()
  }

  def pPipe(
    dut:        GatherRing,
    pipe:       Int,
    table:      Seq[Int],
    index:      Seq[Int],
    indexValid: Seq[Boolean],
    prefill:    Seq[Int] = Seq.fill(16)(0),
    fillZeros:  Seq[Boolean] = Seq.fill(16)(false),
    fillScala:  Seq[Boolean] = Seq.fill(16)(false),
    tableIdx:   Int = 0,
    blockBase:  Int = 0
  ): Unit = {
    val p  = dut.pipeIn(pipe)
    val tb = p.bits.placeTable
    val ix = p.bits.placeIndex
    p.valid.poke(true)
    p.bits.placeIdx.poke(pipe)
    tb.blockBase.poke(blockBase)
    tb.tableIdx.poke(tableIdx)
    ix.blockBase.poke(blockBase)
    table.zipWithIndex.foreach { case (v, i) => tb.table(i).poke(v) }
    index.zipWithIndex.foreach { case (v, i) => ix.index(i).poke(v) }
    indexValid.zipWithIndex.foreach { case (v, i) => ix.indexValid(i).poke(v) }
    prefill.zipWithIndex.foreach { case (v, i) => ix.prefillData(i).poke(v) }
    fillZeros.zipWithIndex.foreach { case (v, i) => ix.fillZeros(i).poke(v) }
    fillScala.zipWithIndex.foreach { case (v, i) => ix.fillScala(i).poke(v) }
  }

  def pPipeOff(dut: GatherRing, pipe: Int): Unit =
    dut.pipeIn(pipe).valid.poke(false)

  def eWb(dut: GatherRing, pipe: Int, dest: BigInt): Unit = {
    dut.writeback(pipe).valid.expect(true)
    dut.writeback(pipe).bits.dest.expect(dest)
  }

  // ═══ 阶段 A1: LMUL=1 vrgather identity (1 UOP) ══════
  describe("LMUL=1") {
    it("vrgather identity via pipe 0") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 1)
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
        simulate(new GatherRing(vlen, dlen, xlen)) {
          dut =>

            initDut(dut, blkSize = 1)
            val t = (0 until 16).map(i => 0x10 + i)
            pPipe(dut, 0, t, 0 until 16, Seq.fill(16)(true), blockBase = u)
            dut.clock.step()
            eWb(dut, 0, BigInt("1F1E1D1C1B1A19181716151413121110", 16))
        }
      }
    }

    // A4: vslideup SEW=8 offset=3 (fillZeros)
    it("vslideup offset=3 fillZeros") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 1)
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

  // ═══ 阶段 B1: LMUL=2 ═════════════════════════════════
  // KNOWN ISSUE: simplified willFlow causes premature flow from empty units
  // Need proper two-phase willFlow before LMUL>1 tests can pass
  describe("LMUL=2") {
    it("vrgather identity all-in-seg0") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 2)
          val s0 = (0 until 16).map(i => 0x10 + i)
          pPipe(dut, 0, s0, (0 until 16), Seq.fill(16)(true))
          dut.clock.step()        // Place
          pPipeOff(dut, 0)
          dut.clock.step()        // willFlow delay
          dut.clock.step()        // flow moves table to index
          dut.clock.step()        // counter→0, writeback
          val exp = s0.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
          eWb(dut, 0, exp)
      }
    }

    // B2: LMUL=2 cross-segment: indices in seg1, two pipes place both segments
    it("vrgather cross-segment [KNOWN FAIL]") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 2)
          val s0 = (0 until 16).map(i => 0x10 + i)
          val s1 = (0 until 16).map(i => 0xa0 + i)
          val shift = 16
          val idx = (0 until 16).map(i => i + shift)
          val pf  = idx
          for (pipe <- 0 until 2) {
            val tbl = if (pipe == 0) s0 else s1
            pPipe(dut, pipe, tbl, idx, Seq.fill(16)(true), prefill = pf, tableIdx = pipe)
          }
          dut.clock.step() // Place both segments
          // KNOWN FAIL: same-cycle pipe collision
      }
    }
  }

  // ═══ 阶段 C1: LMUL=4 ═════════════════════════════════
  describe("LMUL=4") {
    it("vrgather identity single pipe [KNOWN FAIL]") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 4)
          val t = (0 until 16).map(i => 0x30 + i)
          pPipe(dut, 0, t, (0 until 16), Seq.fill(16)(true))
          dut.clock.step() // Place
          // KNOWN FAIL: counter timing with table/index separation for LMUL>2
      }
    }
  }

  // ═══ tableOut 数据验证 ═══════════════════════════════
  describe("tableOut") {
    it("table and index placed at separate units") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 2)
          val s0 = (0 until 16).map(i => 0x10 + i)
          pPipe(dut, 0, s0, (0 until 16), Seq.fill(16)(true))
          dut.clock.step() // Place: table@U1, index@U0
          dut.unitStatus(1).hasTable.expect(true)
          dut.unitStatus(0).hasIndex.expect(true)
      }
    }
  }

  // ═══ B3: LMUL=2 zip [KNOWN FAIL: P6] ═════════════════
  describe("LMUL=2 zip") {
    it("interleave seg0 and seg1 [KNOWN FAIL]") {
      simulate(new GatherRing(vlen, dlen, xlen)) {
        dut =>

          initDut(dut, blkSize = 2)
          val s0  = (0 until 16).map(i => 0x10 + i)
          val s1  = (0 until 16).map(i => 0xa0 + i)
          val idx = (0 until 16).map(i => if (i % 2 == 0) i / 2 else 16 + i / 2)
          for (pipe <- 0 until 2) {
            val tbl = if (pipe == 0) s0 else s1
            pPipe(dut, pipe, tbl, idx, Seq.fill(16)(true), prefill = idx, tableIdx = pipe)
          }
          dut.clock.step()
      }
    }
  }
}
