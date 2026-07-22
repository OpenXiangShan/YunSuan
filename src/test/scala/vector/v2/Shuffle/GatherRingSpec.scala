package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.ExpectationValueFormat
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.Cli.{Coverage, DefaultTimescale, EmitFst}
import yunsuan.vector.v2.Shuffle.GatherCommon.FakeRobPtr

class GatherRingSpec
  extends AnyFunSpec
    with ChiselSim
    with Matchers
    with ParallelTestExecution
    with Coverage
    with EmitFst
    with DefaultTimescale {

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

  // s1: deliver table + merged flags+data
  def pS1(
    dut:        GatherRing[FakeRobPtr],
    pipe:       Int,
    table:      Seq[Int],
    index:      Seq[Int],
    indexValid: Seq[Boolean],
    prefill:    Seq[Int],
    fillZeros:  Seq[Boolean] = Seq.fill(16)(false),
    fillScala:  Seq[Boolean] = Seq.fill(16)(false),
    scalaBytes: Seq[Int] = Seq.fill(8)(0),
    sew:        Int = 64,
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
    // Encode to merged flags+data; scalar byte computed here (IndexGen equivalent)
    for (i <- 0 until 16) {
      val flags = if (fillZeros(i)) ElemFlags.ZERO else if (indexValid(i)) ElemFlags.LOOKUP else ElemFlags.PREFILL
      val sByte = sew match { case 8 => scalaBytes(0); case 16 => scalaBytes(i % 2); case 32 => scalaBytes(i % 4); case _ => scalaBytes(i % 8) }
      val data  = if (fillZeros(i)) 0 else if (fillScala(i)) sByte else if (indexValid(i)) index(i) else prefill(i)
      s1.bits.elemFlags(i).poke(flags)
      s1.bits.elemData(i).poke(data.U)
    }
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
    def sequential(numTable: Int = 1) = Seq.tabulate(numTable * 16)(identity)
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
      scalarTable: Seq[Int] = ScalarTable.default(8),
      sew: Int = 64
    ): BigInt = {
      def scalarByte(i: Int): Int = sew match {
        case 8 => scalarTable(0); case 16 => scalarTable(i % 2)
        case 32 => scalarTable(i % 4); case _ => scalarTable(i % 8)
      }
      index.indices.map { i =>
        if (indexValid(i)) {
          if (tableRange.contains(index(i) / 16)) table(index(i))
          else index(i)
        } else if (scalarValid(i)) scalarByte(i)
        else if (zeroValid(i)) 0
        else prefill(i)
      }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
    }
  }

  describe("Scaling") {
    describe("LMUL=1") {
      it("unit 0") {
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
      it("unit 4") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val index = Index.allInTable0(1)
          val iValid = IndexValid.allTrue(1)
          val pf = Prefill.default(1)
          val golden = Res.gather(table, index, 0 until 1)
          pS0(dut, pipe = 0, blockBase = 4, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 4)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("two parallel") {
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
    }
    describe("LMUL=2") {
      it("identity") {
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

          pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)
          val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
          pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
          pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
          dut.clock.step()

          pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)
          dut.clock.step(); dut.clock.step(); dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("all in seg1") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
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
          dut.clock.step(); dut.clock.step(); dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("zip interleave") {
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

          pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)
          val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
          pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
          pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
          dut.clock.step()

          pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)
          dut.clock.step(); dut.clock.step(); dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("notfound resolve") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(2)
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
          for (_ <- 0 until 4) dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("blockBase=2") {
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

          eWb(dut, pipe = 2, golden)
        }
      }
    }
    describe("LMUL=4") {
      it("identity") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(4)
          val index = Index.unzip4(1)
          val iValid = IndexValid.allTrue(1)
          val pf = Prefill.default(1)
          val golden = Res.gather(table, index, 0 until 4)
          for (p <- 0 until 4) { pS0(dut, pipe = p, blockBase = 0, blockSize = 4, placeIdx = p) }
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
    describe("LMUL=8") {
      it("identity") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(8)
          val index = Index.allInTable0(1)
          val iValid = IndexValid.allTrue(1)
          val pf = Prefill.default(1)
          val golden = Res.gather(table, index, 0 until 8)
          for (p <- 0 until 4) { pS0(dut, pipe = p, blockBase = 0, blockSize = 8, placeIdx = p) }
          dut.clock.step()

          for (p <- 0 until 4) pS0Off(dut, pipe = p)
          for (p <- 0 until 4) {
            pS1(dut, pipe = p, table.slice(p * 16, (p + 1) * 16), index, iValid, pf,
              blockSize = 8, tableIdx = p, placeIdx = p)
          }
          dut.clock.step()

          for (p <- 0 until 4) pS1Off(dut, pipe = p)
          for (p <- 0 until 4) { pS0(dut, pipe = p, blockBase = 0, blockSize = 8, placeIdx = p + 4) }
          dut.clock.step()

          for (p <- 0 until 4) pS0Off(dut, pipe = p)
          for (p <- 0 until 4) {
            pS1(dut, pipe = p, table.slice((p + 4) * 16, (p + 5) * 16), index, iValid, pf,
              blockSize = 8, tableIdx = p + 4, placeIdx = p + 4)
          }
          dut.clock.step()

          for (p <- 0 until 4) pS1Off(dut, pipe = p)
          for (_ <- 0 until 9) dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
    }
  }

  describe("Fill") {
    describe("vrgather.vv") {
      it("all in range") {
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
      it("index out of range") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val index = Seq.tabulate(16)(i => if (i % 2 == 0) 100 else 15 - i % 16)
          val iValid = Seq.tabulate(16)(i => i % 2 != 0)
          val zValid = Seq.tabulate(16)(i => i % 2 == 0)
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
      it("inactive elements") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i >= 3)
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
      it("tail elements") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1)
          val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i < 12)
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
    }
    describe("vslideup") {
      it("offset fillZeros") {
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
      it("partial vl") {
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
      it("masked-off") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8
          val table = Table.inc(1)
          val index = Index.slideup(sew, slide = 4)
          val zValid = Seq.tabulate(16)(i => i < 4)
          val iValid = Seq.tabulate(16)(i => i >= 4 && i % 2 != 0 && index(i) >= 0)
          val pf = Seq.tabulate(16)(i => 0xAA + i)
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
    describe("vslidedown") {
      it("offset overflow") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8
          val table = Table.inc(1)
          val index = Index.slidedown(sew, slide = 2)
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
      it("tail elements") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8
          val table = Table.inc(1)
          val index = Index.slidedown(sew, slide = 1)
          val iValid = Seq.tabulate(16)(i => i < 10 && index(i) < 16)
          val zValid = Seq.tabulate(16)(i => i < 10 && index(i) >= 16)
          val pf = Seq.fill(16)(0xFF)
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
    describe("vslide1up") {
      def iValid(sew: Int) = { val idx = Index.slideup(sew); Seq.tabulate(16)(i => i >= sew / 8 && idx(i) >= 0) }
      def sValid(sew: Int) = Seq.tabulate(16)(i => i < sew / 8)
      it("SEW=8") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8; val table = Table.inc(1); val index = Index.slideup(sew)
          val pf = Prefill.default(1); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid(sew), pf, 0 until 1, scalarValid = sValid(sew), scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid(sew), pf, fillScala = sValid(sew), scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("SEW=16") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 16; val table = Table.inc(1); val index = Index.slideup(sew)
          val pf = Prefill.default(1); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid(sew), pf, 0 until 1, scalarValid = sValid(sew), scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid(sew), pf, fillScala = sValid(sew), scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("SEW=32") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 32; val table = Table.inc(1); val index = Index.slideup(sew)
          val pf = Prefill.default(1); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid(sew), pf, 0 until 1, scalarValid = sValid(sew), scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid(sew), pf, fillScala = sValid(sew), scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("SEW=64") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 64; val table = Table.inc(1); val index = Index.slideup(sew)
          val pf = Prefill.default(1); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid(sew), pf, 0 until 1, scalarValid = sValid(sew), scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid(sew), pf, fillScala = sValid(sew), scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("tail elements") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8; val table = Table.inc(1); val index = Index.slideup(sew)
          val sValid = Seq.tabulate(16)(_ == 0)
          val iValid = Seq.tabulate(16)(i => i >= 1 && i < 14 && index(i) >= 0)
          val pf = Seq.tabulate(16)(i => 0xBB + i); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1, scalarValid = sValid, scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, fillScala = sValid, scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
    }
    describe("vslide1down") {
      def iValid(sew: Int) = { val idx = Index.slidedown(sew); Seq.tabulate(16)(i => i < 16 - sew / 8 && idx(i) < 16) }
      def sValid(sew: Int) = Seq.tabulate(16)(i => i >= 16 - sew / 8)
      it("SEW=8") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8; val table = Table.inc(1); val index = Index.slidedown(sew)
          val pf = Prefill.default(1); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid(sew), pf, 0 until 1, scalarValid = sValid(sew), scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid(sew), pf, fillScala = sValid(sew), scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("tail elements") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8; val table = Table.inc(1); val index = Index.slidedown(sew)
          val sValid = Seq.tabulate(16)(_ == 15)
          val iValid = Seq.tabulate(16)(i => i < 12 && index(i) < 16)
          val pf = Seq.fill(16)(0xFF); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1, scalarValid = sValid, scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, fillScala = sValid, scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
    }
    describe("prefill") {
      it("unchanged") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1); val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i % 2 == 0)
          val pf = Seq.tabulate(16)(i => 0xCC + i)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("all-1s") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1); val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i % 2 == 0)
          val pf = Seq.fill(16)(0xFF)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("f[rs1]") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1); val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i >= 4)
          val fScalar = Seq(0xDE, 0xAD, 0xBE, 0xEF)
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
      it("fillZeros priority") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val table = Table.inc(1); val index = Seq.fill(16)(0)
          val iValid = Seq.tabulate(16)(_ != 0); val zValid = Seq.tabulate(16)(_ == 0)
          val pf = Seq.fill(16)(0xDD)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1, zeroValid = zValid)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("fillScala priority") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 8; val table = Table.inc(1); val index = Seq.fill(16)(0)
          val iValid = Seq.tabulate(16)(_ != 0); val sValid = Seq.tabulate(16)(_ == 0)
          val pf = Seq.fill(16)(0xEE); val sTable = ScalarTable.default(sew)
          val golden = Res.gather(table, index, iValid, pf, 0 until 1, scalarValid = sValid, scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, fillScala = sValid, scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
      it("all 6 sources") {
        simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
          initDut(dut)
          val sew = 16; val table = Table.inc(1); val index = Index.allInTable0(1)
          val iValid = Seq.tabulate(16)(i => i < 2)
          val zValid = Seq.tabulate(16)(i => i >= 2 && i < 4)
          val sValid = Seq.tabulate(16)(i => i >= 4 && i < 6)
          val sTable = ScalarTable.default(sew)
          val pf = Seq.tabulate(16) { i =>
            if (i >= 6 && i < 8) Seq(0xDE, 0xAD, 0xBE, 0xEF)(i % 4)
            else if (i >= 8 && i < 10) 0xAB + i
            else 0xFF
          }
          val golden = Res.gather(table, index, iValid, pf, 0 until 1,
            zeroValid = zValid, scalarValid = sValid, scalarTable = sTable, sew = sew)
          pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
          dut.clock.step(); pS0Off(dut, pipe = 0)

          pS1(dut, pipe = 0, table, index, iValid, pf, fillZeros = zValid, fillScala = sValid, scalaBytes = sTable, sew = sew)
          dut.clock.step(); pS1Off(dut, pipe = 0)

          dut.clock.step()

          eWb(dut, pipe = 0, golden)
        }
      }
    }
  }

  describe("Writeback") {
    it("interleaved pipes") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1); val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 1)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        pS0(dut, pipe = 2, blockBase = 2, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 2)

        pS1(dut, pipe = 0, table, index, iValid, pf, blockBase = 0)
        pS1(dut, pipe = 2, table, index, iValid, pf, blockBase = 2)
        dut.clock.step(); pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 2)

        dut.clock.step()

        eWb(dut, pipe = 0, golden); eWb(dut, pipe = 2, golden)
      }
    }
    it("same pipe arbitration") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1); val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)
        val idx0 = Index.allInTable0(1); val golden0 = Res.gather(table, idx0, 0 until 1)
        val idx4 = Index.sequential(1); val golden4 = Res.gather(table, idx4, 0 until 1)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        pS0(dut, pipe = 1, blockBase = 4, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        pS1(dut, pipe = 0, table, idx0, iValid, pf, blockBase = 0)
        pS1(dut, pipe = 1, table, idx4, iValid, pf, blockBase = 4)
        dut.clock.step(); pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        eWb(dut, pipe = 0, golden0)
        dut.in.wbReady(0).poke(true)
        dut.clock.step()

        dut.in.wbReady(0).poke(false)
        eWb(dut, pipe = 0, golden4)
      }
    }
    it("valid holds") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1); val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0)

        pS1(dut, pipe = 0, table, index, iValid, pf)
        dut.clock.step(); pS1Off(dut, pipe = 0)

        dut.clock.step()

        dut.out.pipes(0).valid.expect(true.B)
        dut.clock.step()

        dut.out.pipes(0).valid.expect(true.B)
        dut.clock.step()

        dut.out.pipes(0).valid.expect(true.B)
        dut.in.wbReady(0).poke(true)
        dut.clock.step()

        dut.in.wbReady(0).poke(false)
        dut.out.pipes(0).valid.expect(false.B)
      }
    }
    it("early wbReady") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2); val index = Index.allInTable1(1)
        val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)
        val golden = Res.gather(table, index, 0 until 2)
        dut.in.wbReady(0).poke(true)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step(); pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        dut.out.pipes(0).valid.expect(false.B)
        for (_ <- 0 until 2) dut.clock.step()

        eWb(dut, pipe = 0, golden)
      }
    }
  }

  describe("Robustness") {
    it("back-to-back") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(1); val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)

        val idx0 = Index.allInTable0(1)
        val idx1 = (0 until 16).map(x => (x + 4) % 16)
        val idx2 = (0 until 16).map(x => (x + 8) % 16)
        val idx3 = (0 until 16).map(x => (x + 12) % 16)

        val idx = Seq(idx0, idx1, idx2, idx3)
        val golden = idx.map(i => Res.gather(table, i, 0 until 1))

        // cycle 0

        for (p <- 0 until 4) pS0(dut, pipe = p, blockBase = p * 2, blockSize = 1)
        dut.clock.step()

        // cycle 1: r1 s0 placed

        for (p <- 0 until 4) pS0Off(dut, pipe = p)
        for (p <- 0 until 4) pS1(dut, pipe = p, table, idx(p), iValid, pf, blockBase = p * 2)
        for (p <- 0 until 4) pS0(dut, pipe = p, blockBase = p * 2 + 1, blockSize = 1)
        dut.clock.step()

        // cycle 2: r1 s1 delivered, r2 s0 placed

        for (p <- 0 until 4) pS1Off(dut, pipe = p)
        for (p <- 0 until 4) pS0Off(dut, pipe = p)
        eWb(dut, pipe = 0, golden(0)); eWb(dut, pipe = 2, golden(1))
        for (p <- 0 until 4) pS1(dut, pipe = p, table, idx(p), iValid, pf, blockBase = p * 2 + 1)
        dut.in.wbReady(0).poke(true); dut.in.wbReady(2).poke(true)
        dut.clock.step()

        // cycle 3: r1 wb accepted, r2 s1 delivered

        dut.in.wbReady(0).poke(false); dut.in.wbReady(2).poke(false)
        for (p <- 0 until 4) pS1Off(dut, pipe = p)
        eWb(dut, pipe = 0, golden(2)); eWb(dut, pipe = 2, golden(3))
        eWb(dut, pipe = 1, golden(0)); eWb(dut, pipe = 3, golden(1))
        dut.in.wbReady(0).poke(true); dut.in.wbReady(2).poke(true)
        dut.in.wbReady(1).poke(true); dut.in.wbReady(3).poke(true)
        dut.clock.step()

        // cycle 4: r1 done, r2 wb accepted

        dut.in.wbReady(0).poke(false); dut.in.wbReady(2).poke(false)
        dut.in.wbReady(1).poke(false); dut.in.wbReady(3).poke(false)
        eWb(dut, pipe = 1, golden(2)); eWb(dut, pipe = 3, golden(3))
      }
    }
    it("flush") {
      simulate(new GatherRing[FakeRobPtr](FakeRobPtr())(GatherCommon.GatherParam.default)) { dut =>
        initDut(dut)
        val table = Table.inc(2); val index = Index.allInTable0(1)
        val iValid = IndexValid.allTrue(1); val pf = Prefill.default(1)
        pS0(dut, pipe = 0, blockBase = 0, blockSize = 2, placeIdx = 0)
        pS0(dut, pipe = 1, blockBase = 0, blockSize = 2, placeIdx = 1)
        dut.clock.step(); pS0Off(dut, pipe = 0); pS0Off(dut, pipe = 1)

        val seg0 = table.slice(0, 16); val seg1 = table.slice(16, 32)
        pS1(dut, pipe = 0, seg0, index, iValid, pf, blockSize = 2, tableIdx = 0, placeIdx = 0)
        pS1(dut, pipe = 1, seg1, index, iValid, pf, blockSize = 2, tableIdx = 1, placeIdx = 1)
        dut.clock.step(); pS1Off(dut, pipe = 0); pS1Off(dut, pipe = 1)

        dut.in.flush.foreach(_.poke(true))
        dut.clock.step()

        dut.in.flush.foreach(_.poke(false))
        dut.clock.step()

        for (u <- 0 until 8) {
          dut.out.unitStatus(u).hasTable.expect(false.B)
          dut.out.unitStatus(u).hasIndex.expect(false.B)
        }
      }
    }
  }
}
