package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.ExpectationValueFormat
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.FakeRobPtr

class GatherRingSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  // === Helpers =======================================

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
    scalarData: BigInt = BigInt(0),
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
    s1.bits.scalarData.poke(scalarData)
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

  // === Test Objects (reuse from GatherRingUnitSpec) ===

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
    def slideup(sew: Int, numTable: Int = 1, slide: Int = 1) = Seq.tabulate(numTable * 16)(x => x - sew / 8 * slide)
    def slidedown(sew: Int, numTable: Int = 1, slide: Int = 1) = Seq.tabulate(numTable * 16)(x => x + sew / 8 * slide)
  }

  object IndexValid {
    def allTrue(numTable: Int = 1): Seq[Boolean] = Seq.fill(numTable * 16)(true)
  }

  object Prefill {
    def default(numTable: Int = 1) = Seq.tabulate(numTable * 16)(0xff - _)
  }

  object ScalarTable {
    def default(sew: Int) = sew match {
      case 8  => Seq.tabulate(8)(x => x + 0xCA)
      case 16 => Seq.tabulate(8)(x => (x % 2) + 0xCA)
      case 32 => Seq.tabulate(8)(x => (x % 4) + 0xCA)
      case 64 => Seq.tabulate(8)(x => (x % 8) + 0xCA)
    }
  }

  object Res {
    def gather(table: Seq[Int], index: Seq[Int], tableRange: Seq[Int]): BigInt = {
      index.indices.map { i =>
        if (tableRange.contains(index(i) / 16)) table(index(i))
        else index(i)
      }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
    }

    def gather(
      table:       Seq[Int],
      index:       Seq[Int],
      indexValid:  Seq[Boolean],
      prefill:     Seq[Int],
      tableRange:  Seq[Int],
      zeroValid:   Seq[Boolean] = Seq.fill(16)(false),
      scalarValid: Seq[Boolean] = Seq.fill(16)(false),
      scalarTable: Seq[Int] = ScalarTable.default(8)
    ): BigInt = {
      index.indices.map { i =>
        if (indexValid(i)) {
          if (tableRange.contains(index(i) / 16)) table(index(i))
          else index(i)
        } else if (scalarValid(i)) scalarTable(i % scalarTable.size)
        else if (zeroValid(i)) 0
        else prefill(i)
      }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
    }
  }

  // === Phase 1: Sanity -- single unit, no flow ========

  describe("Scaling") {
    it("LMUL=1, unit 0") {
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

    it("LMUL=1, unit 4") {
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

    it("two LMUL=1 in parallel (U0 + U2)") {
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

  // === Phase 2: Flow -- LMUL=2 ========================

  describe("Flow") {
    it("LMUL=2 identity") {
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

    it("LMUL=2, all indices in seg1") {
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

    it("LMUL=2 zip interleave") {
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

  // === Phase 3: LMUL=4 / LMUL=8 ======================

  describe("Scaling (LMUL>2)") {
    it("LMUL=4 identity") {
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

    it("LMUL=8 identity (2 batches)") {
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

  // === Phase 4: Writeback mapping ===================

  describe("Writeback") {
    it("U0+U2 done -> different pipes") {
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

    it("U0+U4 done -> same pipe arbitration") {
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

  // === Phase 5: Corner cases =========================

  describe("Fill + Robustness") {
    it("back-to-back instructions") {
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

    it("flush clears state") {
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

    it("wb.valid holds when wbReady=0") {
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

    it("vslideup offset=3 fillZeros") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew, numTable = 1, slide = 3)
        val iValid = (0 until 16).map(_ >= 3)
        val zValid = (0 until 16).map(_ < 3)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1up fillScala SEW=8") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew, numTable = 1)
        val iValid = (0 until 16).map(_ != 0)
        val sValid = (0 until 16).map(_ == 0)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillScala = sValid,
          scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1down fillScala SEW=8") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slidedown(sew, numTable = 1)
        val iValid = (0 until 16).map(_ != 15)
        val sValid = (0 until 16).map(_ == 15)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillScala = sValid,
          scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }

    it("LMUL=4 identity, all 4 pipes used") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(4)
        val index = Index.unzip4(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 4)

        for (p <- 0 until 4) {
          pS0(dut, pipe = p, blockBase = 0, blockSize = 4, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS0Off(dut, pipe = p)
        for (p <- 0 until 4) {
          pS1(dut, pipe = p, table.slice(p * 16, (p + 1) * 16), index, iValid, pf,
            blockSize = 4, tableIdx = p, placeIdx = p)
        }
        dut.clock.step()
        for (p <- 0 until 4) pS1Off(dut, pipe = p)

        for (_ <- 0 until 5) dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Scaling -- remaining ============================

  it("LMUL=2, blockBase=2") {
    simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
      initDut(dut)
      val table = Table.inc(2)
      val index = Index.allInTable0(1)
      val iValid = IndexValid.allTrue(1)
      val pf = Prefill.default(1)
      val golden = Res.gather(table, index, 0 until 2)

      pS0(dut, pipe = 0, blockBase = 2, blockSize = 2, placeIdx = 0)
      pS0(dut, pipe = 1, blockBase = 2, blockSize = 2, placeIdx = 1)
      dut.clock.step()
      pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

      val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
      pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0,
        placeIdx = 0, blockBase = 2)
      pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1,
        placeIdx = 1, blockBase = 2)
      dut.clock.step()
      pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

      dut.clock.step(); dut.clock.step(); dut.clock.step()
      // U2 -> pipe 2
      eWb(dut, pipe = 2, golden)
    }
  }

  // === Flow -- remaining ===============================

  describe("Flow -- notfound") {
    it("LMUL=2 notfound resolve") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        // all indices in seg1 -> miss seg0, fillIdx stores index, flow to seg1 resolves
        val index = Index.allInTable1(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        // seg0: all miss -> fillIdx stores index; seg1: flow in, resolve hits
        for (_ <- 0 until 4) dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- vrgather.vv =============================

  describe("Fill -- vrgather.vv") {
    it("vrgather.vv: all indices in range -> fillRes") {
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
      }
    }

    it("vrgather.vv: index out of range -> fillZeros") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        // out-of-range: indexValid=F + fillZeros=T (IndexGen sets both)
        val index = Seq.tabulate(16)(i => if (i % 2 == 0) 100 else 15 - i % 16)
        val iValid = Seq.tabulate(16)(i => i % 2 != 0) // only odd elements have valid index
        val zValid = Seq.tabulate(16)(i => i % 2 == 0) // even -> fillZeros
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vrgather.vv: inactive elements -> prefill") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = Seq.tabulate(16)(i => i >= 3) // first 3 inactive
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vrgather.vv: tail elements -> prefill (vl=12)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = Seq.tabulate(16)(i => i < 12) // tail i>=12 inactive
        val pf = Prefill.default(1).map(b => if (b < 12) 0 else b) // distinct prefill for tail
        val golden = Res.gather(table, index, iValid, pf.map(_ & 0xFF), 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- vslideup ================================

  describe("Fill -- vslideup") {
    it("vslideup: offset=3 -> fillZeros [F-up1]") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew, slide = 3)
        val iValid = Seq.tabulate(16)(i => i >= 3 && index(i) >= 0)
        val zValid = Seq.tabulate(16)(i => i < 3 || index(i) < 0)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslideup: offset=1, partial vl=14 -> prefill tail") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew, slide = 1)
        val iValid = Seq.tabulate(16)(i => i >= 1 && i < 14 && index(i) >= 0)
        val zValid = Seq.tabulate(16)(i => i == 0 || index(i) < 0)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslideup: masked-off -> prefill ((5) unchanged)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew, slide = 4)
        val zValid = Seq.tabulate(16)(i => i < 4) // offset 4 -> first 4 zero
        val iValid = Seq.tabulate(16)(i => i >= 4 && i % 2 != 0 && index(i) >= 0) // odd -> active
        // even i >=4: indexValid=F, prefill -> old vd ((5) unchanged)
        val pf = Seq.tabulate(16)(i => 0xAA + i) // distinct oldVd per element
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- vslidedown ==============================

  describe("Fill -- vslidedown") {
    it("vslidedown: offset=2, all active -> fillZeros for overflow") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slidedown(sew, slide = 2)
        // i>=14: index >= 16 -> out of range
        val iValid = Seq.tabulate(16)(i => index(i) < 16)
        val zValid = Seq.tabulate(16)(i => index(i) >= 16)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslidedown: tail elements -> prefill (vl=10, (6) all-1s)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slidedown(sew, slide = 1)
        val iValid = Seq.tabulate(16)(i => i < 10 && index(i) < 16)
        val zValid = Seq.tabulate(16)(i => i < 10 && index(i) >= 16)
        val pf = Seq.fill(16)(0xFF) // all-1s for tail ((6))
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- vslide1up ===============================

  describe("Fill -- vslide1up") {
    // helper: determine indexValid for slideup: i>0 && index >=0
    def slideupIValid(sew: Int): Seq[Boolean] = {
      val idx = Index.slideup(sew, slide = 1)
      Seq.tabulate(16)(i => i >= sew / 8 && idx(i) >= 0)
    }
    def slideupSValid(sew: Int): Seq[Boolean] =
      Seq.tabulate(16)(i => i < sew / 8)

    it("vslide1up: SEW=8 fillScala") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew)
        val iValid = slideupIValid(sew)
        val sValid = slideupSValid(sew)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1up: SEW=16 fillScala") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 16
        val table = Table.inc(1)
        val index = Index.slideup(sew)
        val iValid = slideupIValid(sew)
        val sValid = slideupSValid(sew)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1up: SEW=32 fillScala") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 32
        val table = Table.inc(1)
        val index = Index.slideup(sew)
        val iValid = slideupIValid(sew)
        val sValid = slideupSValid(sew)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1up: SEW=64 fillScala") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 64
        val table = Table.inc(1)
        val index = Index.slideup(sew)
        val iValid = slideupIValid(sew)
        val sValid = slideupSValid(sew)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1up: tail elements -> prefill (vl=14, (5) unchanged)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slideup(sew)
        val sValid = Seq.tabulate(16)(_ == 0) // i=0 -> scalar
        val iValid = Seq.tabulate(16)(i => i >= 1 && i < 14 && index(i) >= 0)
        val pf = Seq.tabulate(16)(i => 0xBB + i) // distinct oldVd
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- vslide1down =============================

  describe("Fill -- vslide1down") {
    def slidedownIValid(sew: Int): Seq[Boolean] = {
      val idx = Index.slidedown(sew, slide = 1)
      Seq.tabulate(16)(i => i < 16 - sew / 8 && idx(i) < 16)
    }
    def slidedownSValid(sew: Int): Seq[Boolean] =
      Seq.tabulate(16)(i => i >= 16 - sew / 8)

    it("vslide1down: SEW=8 fillScala") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slidedown(sew)
        val iValid = slidedownIValid(sew)
        val sValid = slidedownSValid(sew)
        val pf = Prefill.default(1)
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("vslide1down: tail elements -> prefill (vl=12, (6) all-1s)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Index.slidedown(sew)
        val sValid = Seq.tabulate(16)(_ == 15) // i=15 -> scalar
        val iValid = Seq.tabulate(16)(i => i < 12 && index(i) < 16)
        val pf = Seq.fill(16)(0xFF) // all-1s for tail
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Fill -- prefill (sources (4)(5)(6)) =================

  describe("Fill -- prefill ((4)(5)(6))") {
    it("prefill -> (5) unchanged (interleaved mask=0, vta=0)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = Seq.tabulate(16)(i => i % 2 == 0) // even active
        val pf = Seq.tabulate(16)(i => 0xCC + i) // distinct oldVd per element
        val golden = Res.gather(table, index, iValid, pf, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("prefill -> (6) all-1s (interleaved mask=0, vta=1)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        val iValid = Seq.tabulate(16)(i => i % 2 == 0) // even active
        val pf = Seq.fill(16)(0xFF) // all-1s for masked-off
        val golden = Res.gather(table, index, iValid, pf, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("prefill -> (4) f[rs1] (vfrgather.vf scalar at i=0)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 32
        val table = Table.inc(1)
        val index = Index.allInTable0(1)
        // i=0..3: prefill (float scalar, SEW=32 -> 4 bytes)
        val iValid = Seq.tabulate(16)(i => i >= 4)
        val fScalar = Seq(0xDE, 0xAD, 0xBE, 0xEF) // float scalar bytes
        val pf = Seq.tabulate(16)(i => if (i < 4) fScalar(i % 4) else Prefill.default(1)(i))
        val golden = Res.gather(table, index, iValid, pf, 0 until 1)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("prefill lower priority than fillZeros (same element)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1)
        val index = Seq.fill(16)(0)
        // i=0: indexValid=F, fillZeros=T -> zero wins over prefill
        val iValid = Seq.tabulate(16)(_ != 0)
        val zValid = Seq.tabulate(16)(_ == 0)
        val pf = Seq.fill(16)(0xDD) // prefillData for all elements
        val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("prefill lower priority than fillScala (same element)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 8
        val table = Table.inc(1)
        val index = Seq.fill(16)(0)
        // i=0: indexValid=F, fillScala=T -> scalar wins over prefill
        val iValid = Seq.tabulate(16)(_ != 0)
        val sValid = Seq.tabulate(16)(_ == 0)
        val pf = Seq.fill(16)(0xEE) // prefillData
        val sTable = ScalarTable.default(sew)
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }

    it("all 6 sources mixed ((1)(2)(3) direct, (4)(5)(6) prefill)") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val sew = 16
        val table = Table.inc(1)
        // 6 groups of elements exercising all 6 sources
        // (1) vs2: i=0,1 -> full gather (indexValid=T, fillRes)
        // (2) 0:   i=2,3 -> fillZeros
        // (3) x[rs1]: i=4,5 -> fillScala
        // (4) f[rs1]: i=6,7 -> prefill (float scalar pattern)
        // (5) unchanged: i=8,9 -> prefill (oldVd pattern)
        // (6) all-1s: i=10,11 -> prefill (allOnes pattern)
        val index = Index.allInTable0(1)
        val iValid = Seq.tabulate(16)(i => i < 2) // only i=0,1 active
        val zValid = Seq.tabulate(16)(i => i >= 2 && i < 4)
        val sValid = Seq.tabulate(16)(i => i >= 4 && i < 6)
        val sTable = ScalarTable.default(sew)
        val pf = Seq.tabulate(16) { i =>
          if (i >= 6 && i < 8) Seq(0xDE, 0xAD, 0xBE, 0xEF)(i % 4) // (4) f[rs1]
          else if (i >= 8 && i < 10) 0xAB + i // (5) unchanged
          else 0xFF // (6) all-1s + rest
        }
        val golden = Res.gather(table, index, iValid, pf, 0 until 1,
          zeroValid = zValid, scalarValid = sValid, scalarTable = sTable)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)
        pS1(dut, pipe = 0, table, index, iValid, pf,
          fillZeros = zValid, fillScala = sValid, scalarData = foldBytes(sTable))
        dut.clock.step(); pS1Off(dut, pipe = 0)
        dut.clock.step()
        eWb(dut, pipe = 0, golden)
      }
    }
  }

  // === Writeback -- remaining ==========================

  describe("Writeback -- timing") {
    it("wbReady before done: no spurious wb") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2)
        // all indices in seg1: unit 0 must wait for seg1 to flow in
        val index = Index.allInTable1(1)
        val iValid = IndexValid.allTrue(1)
        val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)

        // wbReady set before any placement
        dut.in.wbReady(0).poke(true)

        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step()
        pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step()
        pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        // wb should NOT assert yet: unit 0 has seg0 but all indices miss -> needs flow
        dut.out.pipes(0).valid.expect(false.B, "wb not done until flow resolves")

        // flow: seg0->U1, seg1->U0; need several cycles for seg1 to reach U0
        for (_ <- 0 until 3) dut.clock.step()
        // now done -> wb
        eWb(dut, pipe = 0, golden)
      }
    }
  }
}
