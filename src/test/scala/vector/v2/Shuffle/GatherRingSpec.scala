package yunsuan.vector.v2.Shuffle

import chisel3._
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

  def eWb(dut: GatherRing[FakeRobPtr], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true)
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  // ═══ Test Objects (reuse from GatherRingUnitSpec) ═══

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  object Table {
    def inc(numTable: Int = 1): Seq[Int] = Seq.tabulate(numTable * 16)(identity)
  }

  object Index {
    def allInTable0(numTable: Int = 1) = Seq.tabulate(numTable * 16)(x => 15 - x % 16)
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

        // s0: route both tables (same blockSize, different placeIdx)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0)
        pS0Off(dut, pipe = 1)

        // s1: deliver seg0 (pipe0, tableIdx=0) and seg1 (pipe1, tableIdx=1)
        pS1(dut, pipe = 0, table, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, table, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0)
        pS1Off(dut, pipe = 1)

        // counter=1 -> flow -> counter=0 -> done -> wb
        dut.clock.step()
        dut.clock.step()
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }
  }
}
