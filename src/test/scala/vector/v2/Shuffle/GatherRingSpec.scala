package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.ExpectationValueFormat
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.FakeRobPtr

class GatherRingSpec extends AnyFunSpec with ChiselSim with Matchers /* with ParallelTestExecution */ {

  // ═══ Helpers ═══════════════════════════════════════

  val NumPipes = 4

  def initDut(dut: GatherRing[FakeRobPtr]): Unit = {
    for (p <- 0 until NumPipes) {
      dut.in.s0pipes(p).valid.poke(false)
      dut.in.s1pipes(p).valid.poke(false)
      dut.in.wbReady(p).poke(false)
    }
    dut.in.flush.foreach(_.poke(false))
    dut.clock.step()
  }

  // s0: placement routing
  def pS0(dut: GatherRing[FakeRobPtr], pipe: Int, blockBase: Int, blockSize: Int, placeIdx: Int = 0): Unit = {
    val s0 = dut.in.s0pipes(pipe)
    s0.valid.poke(true)
    s0.bits.placeIdx.poke(placeIdx)
    s0.bits.blockBase.poke(blockBase)
    s0.bits.blockSize.poke(blockSize)
  }

  def pS0Off(dut: GatherRing[FakeRobPtr], pipe: Int): Unit = {
    dut.in.s0pipes(pipe).valid.poke(false)
  }

  // s1: deliver table + index data
  def pS1(
    dut:        GatherRing[FakeRobPtr],
    pipe:       Int,
    table:      Seq[Int],
    index:      Seq[Int],
    indexValid: Seq[Boolean],
    prefill:    Seq[Int],
    fillZeros:  Seq[Boolean] = Seq.fill(16)(false),
    fillScala:  Seq[Boolean] = Seq.fill(16)(false),
    placeIdx:   Int = 0,
    blockBase:  Int = 0,
    blockSize:  Int = 1,
    tableIdx:   Int = 0
  ): Unit = {
    val s1 = dut.in.s1pipes(pipe)
    s1.valid.poke(true)
    s1.bits.placeIdx.poke(placeIdx)
    s1.bits.blockBase.poke(blockBase)
    s1.bits.blockSize.poke(blockSize)
    s1.bits.tableIdx.poke(tableIdx)
    index.zipWithIndex.foreach { case (v, i) => s1.bits.index(i).poke(v) }
    indexValid.zipWithIndex.foreach { case (v, i) => s1.bits.indexValid(i).poke(v) }
    prefill.zipWithIndex.foreach { case (v, i) => s1.bits.prefillData(i).poke(v) }
    fillZeros.zipWithIndex.foreach { case (v, i) => s1.bits.fillZeros(i).poke(v) }
    fillScala.zipWithIndex.foreach { case (v, i) => s1.bits.fillScala(i).poke(v) }
    // table data
    table.zipWithIndex.foreach { case (v, i) => s1.bits.table(i).poke(v) }
  }

  def pS1Off(dut: GatherRing[FakeRobPtr], pipe: Int): Unit = {
    dut.in.s1pipes(pipe).valid.poke(false)
  }

  def eWb(dut: GatherRing[FakeRobPtr], pipe: Int, dest: BigInt)(implicit sourceInfo: SourceInfo): Unit = {
    dut.out.pipes(pipe).valid.expect(true.B, s"pipe $pipe wb.valid")
    dut.out.pipes(pipe).bits.data.expect(dest, ExpectationValueFormat.Hex)
  }

  // ═══ Test Objects (reuse from GatherRingUnitSpec) ═══

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  object Table {
    def inc(numTable: Int = 1): Seq[Int] = Seq.tabulate(numTable * 16)(identity)
  }

  object Index {
    def allInTable0(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => 15 - x % 16)
    def allInTable1(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => 16 + 15 - x % 16)
    def unzip2(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => x / 2 + (x % 2) * 16)
    def unzip4(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => x / 4 + x % 4 * 16)
  }

  object IndexValid {
    def allTrue(numTable: Int = 1): Seq[Boolean] = Seq.fill(numTable * 16)(true)
  }

  object Prefill {
    def default(numTable: Int = 1) = Seq.tabulate(numTable * 16)(0xff - _)
  }

  object Res {
    def gather(table: Seq[Int], index: Seq[Int], tableRange: Seq[Int]): BigInt = {
      index.indices.map { i =>
        if (tableRange.contains(index(i) / 16)) table(index(i))
        else index(i)
      }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
    }
  }

  // ═══ Phase 1: Sanity — single unit, no flow ════════

  describe("Phase 1: Sanity") {
    it("T1: LMUL=1, unit 0") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("T2: LMUL=1, unit 4") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 4, blockSize = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 4)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        dut.clock.step()

        // unit 4 -> pipe 0 (4 % 4 == 0)
        eWb(dut, pipe = 0, golden)
      }
    }

    it("T3: two LMUL=1 in parallel (U0 + U2)") {
      // U0 -> pipe0, U2 -> pipe2 (interleaved: 0%4=0, 2%4=2)
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        pS0(dut, pipe = 2, blockBase = 2, blockSize = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS0Off(dut, pipe = 2)

        pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 0)
        pS1(dut, pipe = 2, table, index, iValid, pf, blockBase = 2)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        pS1Off(dut, pipe = 2)

        dut.clock.step()

        eWb(dut, pipe = 0, golden)
        eWb(dut, pipe = 2, golden)
      }
    }
  }

  // ═══ Phase 2: Flow — LMUL=2 ════════════════════════

  describe("Phase 2: Flow") {
    it("T4: LMUL=2 identity") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16)
        val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        pS1Off(dut, pipe = 1)

        dut.clock.step()
        dut.clock.step()
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("T5: LMUL=2, all indices in seg1") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val index = Index.allInTable1(1) // indices 16-31, in seg1 range
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16)
        val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        pS1Off(dut, pipe = 1)

        dut.clock.step()
        dut.clock.step()
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("T6: LMUL=2 zip interleave") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val index = Index.unzip2(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16)
        val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        pS1Off(dut, pipe = 1)

        dut.clock.step()
        dut.clock.step()
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // ═══ Phase 3: LMUL=4 / LMUL=8 ══════════════════════

  describe("Phase 3: LMUL=4/8") {
    it("T7: LMUL=4 identity") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(4)
        val index = Index.unzip4(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 4)

        // s0: route 4 segments
        for (p <- 0 until 4) {
          pS0(dut, pipe = p, blockBase = 0, blockSize = 4, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS0Off(dut, pipe = p)

        // s1: deliver 4 segments
        for (p <- 0 until 4) {
          val seg = table.slice(p * 16, (p + 1) * 16)
          pS1(dut, pipe = p, seg, index, iValid, pf, blockSize = 4, tableIdx = p, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS1Off(dut, pipe = p)

        // flow + wb
        for (_ <- 0 until 5) dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("T8: LMUL=8 identity (2 batches)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(8)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 8)

        // s0: route batch 1 (placeIdx 0-3)
        for (p <- 0 until 4) {
          pS0(dut, pipe = p, blockBase = 0, blockSize = 8, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS0Off(dut, pipe = p)

        // s1: deliver batch 1
        for (p <- 0 until 4) {
          val seg = table.slice(p * 16, (p + 1) * 16)
          pS1(dut, pipe = p, seg, index, iValid, pf, blockSize = 8, tableIdx = p, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS1Off(dut, pipe = p)

        // s0: route batch 2 (placeIdx 4-7)
        for (p <- 0 until 4) {
          pS0(dut, pipe = p, blockBase = 0, blockSize = 8, placeIdx = p + 4)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS0Off(dut, pipe = p)

        // s1: deliver batch 2
        for (p <- 0 until 4) {
          val seg = table.slice((p + 4) * 16, (p + 5) * 16)
          pS1(dut, pipe = p, seg, index, iValid, pf, blockSize = 8, tableIdx = p + 4, placeIdx = p + 4)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS1Off(dut, pipe = p)

        // flow + wb
        for (_ <- 0 until 9) dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // ═══ Phase 4: Writeback mapping ═══════════════════

  describe("Phase 4: Writeback") {
    it("T9: U0+U2 done -> different pipes") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        pS0(dut, pipe = 2, blockBase = 2, blockSize = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 2)

        pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 0)
        pS1(dut, pipe = 2, table, index, iValid, pf, blockBase = 2)
        dut.clock.step()
        pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 2)

        dut.clock.step()
        eWb(dut, pipe = 0, golden)
        eWb(dut, pipe = 2, golden)
      }
    }

    it("T10: U0+U4 done -> same pipe arbitration") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        pS0(dut, pipe = 1, blockBase = 4, blockSize = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 0)
        pS1(dut, pipe = 1, table, index, iValid, pf, blockBase = 4)
        dut.clock.step()
        pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // ═══ Phase 5: Corner cases ═════════════════════════

  describe("Phase 5: Corner cases") {
    it("T11: back-to-back instructions") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("T12: flush clears state") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)
        val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        dut.in.flush.foreach(_.poke(true))
        dut.clock.step()
        dut.in.flush.foreach(_.poke(false))
        dut.clock.step()

        for (u <- 0 until 8) {
          dut.out.unitStatus(u).hasTable.expect(false.B, s"unit $u hasTable after flush")
          dut.out.unitStatus(u).hasIndex.expect(false.B, s"unit $u hasIndex after flush")
        }
      }
    }

    it("T13: wb.valid holds when wbReady=0") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)

        dut.clock.step()
        dut.out.pipes(0).valid.expect(true.B, "wb.valid after placement")

        dut.clock.step()
        dut.out.pipes(0).valid.expect(true.B, "wb.valid holds cycle 1")

        dut.clock.step()
        dut.out.pipes(0).valid.expect(true.B, "wb.valid holds cycle 2")

        dut.in.wbReady(0).poke(true)
        dut.clock.step()
        dut.in.wbReady(0).poke(false)
        dut.out.pipes(0).valid.expect(true.B, "unitDone persists after wb (pipe valid stays)")
      }
    }
  }
}
