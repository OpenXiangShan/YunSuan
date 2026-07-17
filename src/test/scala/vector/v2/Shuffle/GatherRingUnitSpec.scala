package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.FakeRobPtr
import chisel3.simulator.{ChiselOptionsModifications, ExpectationValueFormat, FirtoolOptionsModifications, HasSimulator}
import chisel3.testing.HasTestingDirectory

class GatherRingUnitSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val vlen        = 128
  val dlen        = 128
  val maxTableNum = 8

  class Cycle(val pokes: (GatherRingUnit[FakeRobPtr] => Unit)*)(val expects: (GatherRingUnit[FakeRobPtr] => Unit)*)
  object Cycle {
    def apply(pokes: (GatherRingUnit[FakeRobPtr] => Unit)*)(expects: (GatherRingUnit[FakeRobPtr] => Unit)*)(implicit sourceInfo: SourceInfo): Cycle =
      new Cycle(pokes: _*)(expects: _*)
  }

  def runCycles(dut: GatherRingUnit[FakeRobPtr], init: GatherRingUnit[FakeRobPtr] => Unit, cycles: Cycle*): Unit = {
    init(dut)
    for (c <- cycles) {
      c.pokes.foreach(_(dut))
      dut.clock.step()
      c.expects.foreach(_(dut))
    }
  }

  def runCycles(duts: Seq[GatherRingUnit[FakeRobPtr]], init: GatherRingUnit[FakeRobPtr] => Unit, cycles: Seq[Cycle]*): Unit = {
    duts.foreach(init(_))
    for (c <- cycles) {
      duts.zipWithIndex.foreach {
        case (dut, i) =>
          c(i).pokes.foreach(_(dut))
          dut.clock.step()
          c(i).expects.foreach(_(dut))
      }
    }
  }

  def initUnit: GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.placeIndex.bits.blockBase.poke(0)
      dut.in.placeIndex.bits.blockSize.poke(8)
      dut.in.willFlow.poke(false)
      dut.in.flow.valid.poke(false)
      dut.in.flow.bits.tableIdx.poke(0)
      dut.in.flow.bits.data.foreach(_.poke(0))
      dut.in.flow.bits.isFirstTable.poke(false)
      dut.in.flow.bits.passedEntry.poke(false)
      dut.in.placeTable.valid.poke(false)
      dut.in.placeIndex.valid.poke(false)
      dut.in.wbReady.poke(false)
      dut.in.flush.poke(true)
      dut.clock.step()
      dut.in.flush.poke(false)
      dut.clock.step()
  }

  def eHasTable(v: Boolean)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = _.out.status.hasTable.expect(v.B, "hasTable mismatch")

  def eHasIndex(v: Boolean)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = _.out.status.hasIndex.expect(v.B, "hasIndex mismatch")

  def eWb(valid: Boolean, dest: BigInt = 0)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.out.wb.valid.expect(valid.B, "wb.valid mismatch")
      if (valid) dut.out.wb.bits.data.expect(dest, ExpectationValueFormat.Hex)
  }

  def eCounter(value: BigInt)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    dut => dut.out.status.counter.expect(value.U, "counter mismatch")
  }

  def eIndexOrRes(value: BigInt)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    dut => dut.out.debug.foreach {
      x =>
        x.indexOrRes.expect(value.U, "indexOrRes mismatch")
    }
  }

  def ePassedEntry(value: Boolean)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    _.out.flow.bits.passedEntry.expect(value.B, "passedEntry mismatch")
  }

  def eFirstTable(value: Boolean)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    _.out.status.isFirstTable.expect(value.B, "isFirstTable mismatch")
  }

  def pFlush: GatherRingUnit[FakeRobPtr] => Unit = _.in.flush.poke(true)

  def pDrop: GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.willFlow.poke(true)
      dut.in.flow.valid.poke(false)
  }

  def pPlaceTable(blockSize: Int, tblIdx: Int, tbl: Seq[Int]): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      val isFirst = dut.unitIdx % blockSize == 0
      dut.in.placeTable.valid.poke(true)
      dut.in.placeTable.bits.tableIdx.poke(tblIdx)
      tbl.grouped(16).toSeq(tblIdx) .zipWithIndex.foreach {
        case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
      }
      dut.in.placeTable.bits.isFirstTable.poke(isFirst)
      dut.in.placeTable.bits.blockSize.poke(blockSize)
  }

  def pPlaceIndex(
    blkSize: Int,
    idx:     Seq[Int],
    iValid:  Seq[Boolean],
    prefill: Seq[Int] = Seq.fill(16)(0xFF),
    fScala:  Seq[Boolean] = Seq.fill(16)(false),
    fZeros:  Seq[Boolean] = Seq.fill(16)(false)
  ): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.placeIndex.bits.blockSize.poke(blkSize)
      dut.in.placeIndex.valid.poke(true)
      idx.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.index(i).poke(v)
      }
      iValid.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.indexValid(i).poke(v)
      }
      prefill.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.prefillData(i).poke(v)
      }
      fScala.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.fillScala(i).poke(v)
      }
      fZeros.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.fillZeros(i).poke(v)
      }
  }

  def pPlace(
    blkSize    : Int,
    tblIdx     : Int,
    tbl        : Seq[Int],
    idx        : Seq[Int],
    iValid     : Seq[Boolean],
    prefill    : Seq[Int] = Seq.fill(16)(0xFF),
    scalarValid: Seq[Boolean] = Seq.fill(16)(false),
    zeroValid  : Seq[Boolean] = Seq.fill(16)(false),
  ): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>
      pPlaceTable(blkSize, tblIdx, tbl)(dut)
      pPlaceIndex(blkSize, idx, iValid, prefill, scalarValid, zeroValid)(dut)
  }

  def pNoPlace: GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.placeTable.valid.poke(false)
      dut.in.placeIndex.valid.poke(false)
  }

  def pNoFlow: GatherRingUnit[FakeRobPtr] => Unit = _.in.willFlow.poke(false)

  def pFlow: GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.willFlow.poke(true)
  }

  def pFlowTbl(tblIdx: Int, tbl: Seq[Int], passedEntry: Boolean = false, isFirstTable: Boolean = false): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.flow.valid.poke(true)
      dut.in.flow.bits.tableIdx.poke(tblIdx)
      tbl.grouped(16).toSeq(tblIdx).zipWithIndex.foreach {
        case (v, i) => dut.in.flow.bits.data(i).poke(v)
      }
      dut.in.flow.bits.isFirstTable.poke(isFirstTable)
      dut.in.flow.bits.passedEntry.poke(passedEntry)
  }

  def pNoFlowTbl(): GatherRingUnit[FakeRobPtr] => Unit = _.in.flow.valid.poke(false)

  def pScala(v: BigInt): GatherRingUnit[FakeRobPtr] => Unit = _.in.placeIndex.bits.scalarData.poke(v)

  def pScala(v: Seq[Int]): GatherRingUnit[FakeRobPtr] => Unit = {
    _.in.placeIndex.bits.scalarData.poke(Int8SeqToBigInt(v))
  }

  def pWbReady(v: Boolean = true): GatherRingUnit[FakeRobPtr] => Unit = _.in.wbReady.poke(v)

  private def eT1  (implicit sourceInfo: SourceInfo)    = eHasTable(true)
  private def eT0  (implicit sourceInfo: SourceInfo)    = eHasTable(false)
  private def eI1  (implicit sourceInfo: SourceInfo)    = eHasIndex(true)
  private def eI0  (implicit sourceInfo: SourceInfo)    = eHasIndex(false)
  private def eWb0 (implicit sourceInfo: SourceInfo)    = eWb(false)
  private def eWb1(exp: BigInt)(implicit sourceInfo: SourceInfo) = eWb(true, exp)

  def Int8SeqToBigInt(v: Seq[Int]): BigInt = {
    v.foldRight(BigInt(0))(
      (b, a) => (a << 8) | BigInt(b)
    )
  }

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
    def default(sew: Int) = {
      sew match {
        case 1 => Seq.tabulate(8)(x => x % 1 + 0xCA)
        case 2 => Seq.tabulate(8)(x => x % 2 + 0xCA)
        case 4 => Seq.tabulate(8)(x => x % 4 + 0xCA)
        case 8 => Seq.tabulate(8)(x => x % 8 + 0xCA)
      }
    }
  }

  object Res {
    def gather(table: Seq[Int], index: Seq[Int]): BigInt = {
      index.map(idx => table(idx)).foldRight(BigInt(0))(
        (b, a) => (a << 8) | BigInt(b)
      )
    }

    def gather(table: Seq[Int], index: Seq[Int], indexValid: Seq[Boolean], prefill: Seq[Int]): BigInt = {
      val tableRange = 0 until (table.size / 16)

      gather(table, index, indexValid, prefill, tableRange)
    }

    def gather(
      table: Seq[Int],
      index: Seq[Int],
      indexValid: Seq[Boolean] = Seq.fill(16)(true),
      prefill: Seq[Int] = Prefill.default(1),
      tableRange: Seq[Int] = Seq.tabulate(8)(identity),
      scalarValid: Seq[Boolean] = Seq.fill(16)(false),
      zeroValid: Seq[Boolean] = Seq.fill(16)(false),
      scalarTable: Seq[Int] = ScalarTable.default(8),
    ): BigInt = {
      require(index.size == indexValid.size)
      require(index.size == prefill.size)
      require(scalarTable.size == 8)

      index.indices.map(
        i =>
          if (indexValid(i)) {
            if (tableRange.contains(i / 16))
              table(index(i))
            else
              index(i)
          }
          else if (scalarValid(i)) {
            scalarTable(i % scalarTable.size)
          }
          else if (zeroValid(i)) {
            0
          }
          else
            prefill(i)
      ).foldRight(BigInt(0))(
        (b, a) => (a << 8) | BigInt(b)
      )
    }
  }

  def simulator(unitIdx: Int = 0)(stimulus: GatherRingUnit[FakeRobPtr] => Unit)(
    implicit hasSimulator:        HasSimulator,
    testingDirectory:             HasTestingDirectory,
    chiselOptsModifications:      ChiselOptionsModifications,
    firtoolOptsModifications:     FirtoolOptionsModifications,
    commonSettingsModifications:  svsim.CommonSettingsModifications,
    backendSettingsModifications: svsim.BackendSettingsModifications
  ): Unit = {
    simulate(new GatherRingUnit(unitIdx = unitIdx, FakeRobPtr())(GatherCommon.GatherParam.debug))(stimulus)(
      hasSimulator,
      testingDirectory,
      chiselOptsModifications,
      firtoolOptsModifications,
      commonSettingsModifications,
      backendSettingsModifications,
    )
  }

  describe("FSM") {
    it("Place sets hasTable and hasIndex seperately") {
      simulator() {
        dut =>

          val tblSize = 8
          val table = Table.inc(tblSize)
          val index = Index.allInTable0()
          val indexValid = IndexValid.allTrue()
          val prefill = Prefill.default()
          val res = Res.gather(table, index, indexValid, prefill)

          runCycles(
            dut,
            initUnit,
            Cycle(pNoFlow)(eT0, eI0),
            Cycle(pPlaceTable(tblSize, 0, table))(eT1, eI0, eWb0),
            Cycle(pPlaceIndex(tblSize, index, indexValid, prefill))(eT1, eI1, eWb1(res)),
          )
      }
    }
    it("Place sets hasTable and hasIndex at the same time") {
      simulator() {
        dut =>

          val tblSize = 8
          val table = Table.inc(tblSize)
          val index = Index.allInTable0()
          val indexValid = IndexValid.allTrue()
          val prefill = Prefill.default()
          val res = Res.gather(table, index, indexValid, prefill)

          runCycles(
            dut,
            initUnit,
            Cycle(pNoFlow)(eT0, eI0),
            Cycle(pPlace(tblSize, 0, table, index, indexValid, prefill))(eT1, eI1, eWb1(res)),
          )
      }
    }
    it("Flow decrements counter") {
      simulator() {
        dut =>

          val tblSize = 2
          val table = Table.inc(tblSize)
          val index = Index.unzip2(1)
          val indexValid = IndexValid.allTrue()
          val prefill = Prefill.default(1)
          val res = Res.gather(table, index, indexValid, prefill, Seq(0))

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(tblSize, 0, table, index, indexValid, prefill))(eWb0, eCounter(1), eIndexOrRes(res)),
            Cycle(pNoPlace, pFlow)(eWb0),
          )
      }
    }
    it("Hold keeps state") {
      simulator() {
        dut =>

          val table = Table.inc()
          val index = Index.unzip2()
          val indexValid = IndexValid.allTrue()
          val prefill = Prefill.default()
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(8, 0, table, index, indexValid))(eT1, eI1, eCounter(7)),
            Cycle(pNoPlace, pNoFlow)(eT1, eI1),
          )
      }
    }
    it("Flush clears") {
      simulator() {
        dut =>

          val e = Seq.fill(16)(0)
          val f = Seq.fill(16)(false)
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(8, 0, e, e, f))(eT1, eI1),
            Cycle(pNoPlace, pNoFlow, pFlush)(eT0, eI0),
          )
      }
    }
  }

  describe("LMUL1") {
    it("identity 16 elem") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val exp = BigInt("1F1E1D1C1B1A19181716151413121110", 16)
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, 0 until 16, Seq.fill(16)(true)))(eWb1(exp)),
          )
      }
    }
    it("reverse order gather") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          ) // table: 0x10..0x1F
          val idx = (0 until 16).map(
            i => 15 - i
          ) // index: 15,14,...,0
          val bytes = (0 until 16).map(
            i => 0x10 + (15 - i)
          ) // expected: reversed
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, Seq.fill(16)(true)))(eWb1(exp)),
          )
      }
    }
    it("shuffled gather") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          ) // table: 0x10..0x1F
          val perm = Seq(3, 7, 0, 11, 15, 2, 5, 9, 13, 1, 8, 14, 4, 6, 10, 12) // shuffled order
          val bytes = perm.map(
            i => 0x10 + i
          ) // expected: permuted
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, perm, Seq.fill(16)(true)))(eWb1(exp)),
          )
      }
    }
    it("mixed found/notfound") {
      simulator() {
        dut =>

          val table = Table.inc(2)
          val index = Index.unzip2(1)
          val prefill = Prefill.default(1)
          val indexValid = IndexValid.allTrue(1)
          val res = Res.gather(table, index, indexValid, prefill, 0 to 0)
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, prefill))(eWb0, eIndexOrRes(res)),
          )
      }
    }
  }

  describe("vslide1down") {
    implicit var sew = 8
    def sewByte = sew / 8
    val table = Table.inc()
    def index = Index.slidedown(sew)
    def indexValid(vl: Int) = (0 until 16).map(_ / sewByte < vl)
    val prefill = Prefill.default()
    def scalarValid(vl: Int) = (0 until 16).map(_ / sewByte == vl)
    val scalarTable = ScalarTable.default(1)
    def res(vl: Int) = Res.gather(table, index, indexValid(vl), prefill, 0 to 0, scalarValid = scalarValid(vl), scalarTable = scalarTable)

    it("SEW=8") {
      sew = 8
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid(15), prefill = prefill, scalarValid = scalarValid(15)), pScala(scalarTable))(eWb1(res(15))),
            Cycle(pPlace(1, 0, table, index, indexValid(14), prefill = prefill, scalarValid = scalarValid(14)), pScala(scalarTable), pWbReady())(eWb1(res(14))),
            Cycle(pPlace(1, 0, table, index, indexValid(13), prefill = prefill, scalarValid = scalarValid(13)), pScala(scalarTable), pWbReady())(eWb1(res(13))),
            Cycle(pPlace(1, 0, table, index, indexValid(12), prefill = prefill, scalarValid = scalarValid(12)), pScala(scalarTable), pWbReady())(eWb1(res(12))),
            Cycle(pPlace(1, 0, table, index, indexValid(11), prefill = prefill, scalarValid = scalarValid(11)), pScala(scalarTable), pWbReady())(eWb1(res(11))),
            Cycle(pPlace(1, 0, table, index, indexValid(10), prefill = prefill, scalarValid = scalarValid(10)), pScala(scalarTable), pWbReady())(eWb1(res(10))),
            Cycle(pPlace(1, 0, table, index, indexValid( 9), prefill = prefill, scalarValid = scalarValid( 9)), pScala(scalarTable), pWbReady())(eWb1(res( 9))),
            Cycle(pPlace(1, 0, table, index, indexValid( 8), prefill = prefill, scalarValid = scalarValid( 8)), pScala(scalarTable), pWbReady())(eWb1(res( 8))),
            Cycle(pNoPlace, pWbReady())(eWb0)
          )
      }
    }
    it("SEW=16") {
      sew = 16
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid(7), prefill = prefill, scalarValid = scalarValid(7)), pScala(scalarTable))(eWb1(res(7))),
            Cycle(pPlace(1, 0, table, index, indexValid(6), prefill = prefill, scalarValid = scalarValid(6)), pScala(scalarTable), pWbReady())(eWb1(res(6))),
            Cycle(pPlace(1, 0, table, index, indexValid(5), prefill = prefill, scalarValid = scalarValid(5)), pScala(scalarTable), pWbReady())(eWb1(res(5))),
            Cycle(pPlace(1, 0, table, index, indexValid(4), prefill = prefill, scalarValid = scalarValid(4)), pScala(scalarTable), pWbReady())(eWb1(res(4))),
            Cycle(pNoPlace, pWbReady())(eWb0)
          )
      }
    }
    it("SEW=32") {
      sew = 32
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid(3), prefill = prefill, scalarValid = scalarValid(3)), pScala(scalarTable))(eWb1(res(3))),
            Cycle(pPlace(1, 0, table, index, indexValid(2), prefill = prefill, scalarValid = scalarValid(2)), pScala(scalarTable), pWbReady())(eWb1(res(2))),
            Cycle(pPlace(1, 0, table, index, indexValid(1), prefill = prefill, scalarValid = scalarValid(1)), pScala(scalarTable), pWbReady())(eWb1(res(1))),
            Cycle(pNoPlace, pWbReady())(eWb0)
          )
      }
    }
    it("SEW=64") {
      sew = 64
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid(1), prefill = prefill, scalarValid = scalarValid(1)), pScala(scalarTable))(eWb1(res(1))),
            Cycle(pPlace(1, 0, table, index, indexValid(0), prefill = prefill, scalarValid = scalarValid(0)), pScala(scalarTable), pWbReady())(eWb1(res(0))),
            Cycle(pNoPlace, pWbReady())(eWb0)
          )
      }

    }
  }

  describe("vslide1up") {
    implicit var sew = 8
    def sewByte = sew / 8

    val table = Table.inc()
    def index = Index.slideup(sew)
    def indexValid = (0 until 16).map(_ / sewByte != 0)
    val prefill = Prefill.default()
    def scalarValid = (0 until 16).map(_ / sewByte == 0)
    val scalarTable = ScalarTable.default(1)
    def res = Res.gather(table, index, indexValid, prefill, 0 to 0, scalarValid = scalarValid, scalarTable = scalarTable)

    it("SEW=8") {
      sew = 8
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid), pScala(scalarTable))(eWb1(res)),
          )
      }
    }
    it("SEW=16") {
      sew = 16
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid), pScala(scalarTable))(eWb1(res)),
          )
      }
    }
    it("SEW=32") {
      sew = 32
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid), pScala(scalarTable))(eWb1(res)),
          )
      }
    }
    it("SEW=64") {
      sew = 64
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid), pScala(scalarTable))(eWb1(res)),
          )
      }
    }
  }

  describe("SEW=16 fillScala") {
    val sb = 2
    it("vslide1up") {
      simulator() {
        dut =>

          val table = (0 until 16).map(
            i => 0x10 + i
          )
          val index = (0 until 16).map(
            i => if (i < sb) 0 else i - sb
          )
          val indexValid = (0 until 16).map(_ >= sb)
          val scalarValid = (0 until 16).map(_ < sb)
          val prefill = Prefill.default()
          val scalarTable = ScalarTable.default(2)
          val res = Res.gather(table, index, indexValid, prefill, Seq(0), scalarValid = scalarValid, scalarTable = scalarTable)
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid), pScala(scalarTable))(eWb1(res)),
          )
      }
    }
    it("vslide1down") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i >= 16 - sb) 0 else i + sb
          )
          val iv = (0 until 16).map(_ < 16 - sb)
          val fs = (0 until 16).map(_ >= 16 - sb)
          val bytes = (0 until 16).map(
            i => if (i >= 16 - sb) 0x5a else 0x10 + (i + sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16)))(eWb1(exp)),
          )
      }
    }
  }

  describe("SEW=32 fillScala") {
    val sb = 4
    it("vslide1up") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i < sb) 0 else i - sb
          )
          val iv = (0 until 16).map(_ >= sb)
          val fs = (0 until 16).map(_ < sb)
          val bytes = (0 until 16).map(
            i => if (i < sb) 0x5a else 0x10 + (i - sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16)))(eWb1(exp)),
          )
      }
    }
    it("vslide1down") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i >= 16 - sb) 0 else i + sb
          )
          val iv = (0 until 16).map(_ < 16 - sb)
          val fs = (0 until 16).map(_ >= 16 - sb)
          val bytes = (0 until 16).map(
            i => if (i >= 16 - sb) 0x5a else 0x10 + (i + sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16)))(eWb1(exp)),
          )
      }
    }
  }

  describe("fillZeros") {
    it("vslideup offset=3") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val fz = (0 until 16).map(_ < 3)
          val idx = (0 until 16).map(
            i => if (i < 3) 0 else i - 3
          )
          val iv = (0 until 16).map(_ >= 3)
          val bytes = (0 until 16).map(
            i => if (i < 3) 0 else 0x10 + (i - 3)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, iv, zeroValid = fz))(eWb1(exp)),
          )
      }
    }
    it("vslidedown offset=2") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val fz = (0 until 16).map(_ >= 14)
          val idx = (0 until 16).map(
            i => if (i >= 14) 0 else i + 2
          )
          val iv = (0 until 16).map(_ < 14)
          val bytes = (0 until 16).map(
            i => if (i >= 14) 0 else 0x10 + (i + 2)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t, idx, iv, zeroValid = fz))(eWb1(exp)),
          )
      }
    }
    it("fillZeros + fillScala mixed") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val sb = 4
          val fz = (0 until 16).map(_ < sb)
          val fs = (0 until 16).map(
            i => i >= sb && i < sb * 2
          )
          val iv = (0 until 16).map(_ >= sb * 2)
          val idx = (0 until 16).map(
            i => if (i >= sb * 2) i - sb * 2 else 0
          )
          val bytes = (0 until 16).map(
            i => if (i < sb) 0 else if (i < sb * 2) 0x5a else 0x10 + (i - sb * 2)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(
              pPlace(1, 0, t, idx, iv, scalarValid = fs, zeroValid = fz), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))
            )(eWb1(exp))
          )
      }
    }
  }

  describe("LMUL2") {
    describe("all in table0") {
      val table = Table.inc(2)
      val index = Index.allInTable0(1)
      it("case 0") {
        simulator(unitIdx = 0) {
          dut =>

            val res = Res.gather(table, index)
            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)))(eCounter(1), eWb1(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table), pWbReady())(eCounter(0), eI1, eT1, eWb0),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(false))(eI0, eT0, eWb0)
            )
        }

        simulator(unitIdx = 1) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = 1 to 1)
            val res1 = Res.gather(table, index, tableRange = 0 to 0)

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)))(eCounter(1), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table))(eCounter(0), eI1, eT1, eWb1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady())(eI0, eT0, eWb0)
            )
        }
      }

      it("case 1") {
        simulator(unitIdx = 0) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = 1 to 1)
            val res1 = Res.gather(table, index, tableRange = 0 to 0)

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)), pWbReady())(eCounter(1), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table), pWbReady(false))(eCounter(0), eI1, eT1, eWb1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady())(eI0, eT0, eWb0)
            )
        }

        simulator(unitIdx = 1) {
          dut =>

            val res = Res.gather(table, index)

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)), pWbReady(false))(eCounter(1), eWb1(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table), pWbReady())(eCounter(0), eI1, eT1, eWb0),
              Cycle(pFlow, pNoFlowTbl())(eI0, eT0, eWb0)
            )
        }
      }

      it("case 2") {
        simulator(unitIdx = 0) {
          dut =>

            val res = Res.gather(table, index)
            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)))(eCounter(1), eWb1(res)),
              Cycle(pNoPlace, pWbReady())(eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table), pWbReady())(eCounter(0), eI1, eT1, eWb0),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(false))(eI0, eT0, eWb0)
            )
        }

        simulator(unitIdx = 1) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = 1 to 1)
            val res1 = Res.gather(table, index, tableRange = 0 to 0)

            runCycles(
              dut,
              initUnit,
              Cycle()(eT0, eI0, eWb0),
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)))(eCounter(1), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table))(eCounter(0), eI1, eT1, eWb1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady())(eI0, eT0, eWb0)
            )
        }
      }
    }

    describe("all in table1") {
      val table = Table.inc(2)
      val index = Index.allInTable1(1)

      it("case 0") {
        simulator(unitIdx = 0) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = Seq(0))
            val res1 = Res.gather(table, index, tableRange = Seq(1))

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)), pWbReady(false))(eCounter(1), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table), pWbReady())(eCounter(0), eI1, eT1, eWb1(res1)),
              Cycle(pFlow, pNoFlowTbl())(eI0, eT0, eWb0)
            )
        }

        simulator(unitIdx = 1) {
          dut =>

            val res = Res.gather(table, index)
            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)), pWbReady())(eCounter(1), eWb1(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table), pWbReady())(eCounter(0), eI1, eT1, eWb0),
              Cycle(pFlow, pNoFlowTbl())(eI0, eT0, eWb0)
            )
        }
      }
    }

    it("LMUL=2 zip interleave") {
      simulator() {
        dut =>

          val table = Table.inc(2)
          // zip: even elements from table 0, odd from table 1
          val index = (0 until 16).map(
            i => if (i % 2 == 0) i / 2 else 16 + i / 2
          )
          val res = Res.gather(table, index)
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)))(eWb0),
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table))(eWb1(res)),
            Cycle(pNoFlow, pNoFlowTbl, pWbReady())(eWb0),
          )
      }
    }
  }

  describe("LMUL4") {
    val table = Table.inc(4)
    val index = Index.unzip4(1)
    val prefill = Prefill.default(1)
    val indexValid = Seq.tabulate(16)(_ % 5 != 0)

    it("unzip4") {
      simulator() {
        dut =>

          val res0 = Res.gather(table, index, indexValid, prefill, Seq(0))
          val res1 = Res.gather(table, index, indexValid, prefill, Seq(0, 1))
          val res2 = Res.gather(table, index, indexValid, prefill, Seq(0, 1, 2))
          val res3 = Res.gather(table, index, indexValid, prefill)

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(4, 0, table, index, indexValid, prefill = prefill))(eWb0, eIndexOrRes(res0)),
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table))(eWb0, eIndexOrRes(res1)),
            Cycle(pNoPlace, pFlow, pFlowTbl(2, table))(eWb0, eIndexOrRes(res2)),
            Cycle(pNoPlace, pFlow, pFlowTbl(3, table))(eWb1(res3)),
          )
      }
    }
  }

  describe("Tags") {
    it("tableOut valid after Place") {
      simulator() {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => 15 - i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, t, idx, Seq.fill(16)(true)))(),
            Cycle(pNoPlace, pNoFlow)(_.out.flow.valid.expect(true)),
          )
      }
    }
    it("passedEntry at entry unit after flow") {
      simulator() {
        dut =>

          val table = Table.inc(2)
          val index = Index.allInTable0()
          val indexValid = IndexValid.allTrue()
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, table, index, indexValid))(),
            Cycle(pNoPlace, pFlowTbl(1, table))(ePassedEntry(true)),
          )
      }
    }
    it("tag.isFirst set on Place") {
      def test(unitIdx: Int, isFirstTable: Boolean) = simulator(unitIdx) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, t, 0 until 16, Seq.fill(16)(true)))(),
            Cycle(pNoPlace, pNoFlow)(eFirstTable(isFirstTable)),
          )
      }

      test(0, true)
      test(1, false)
    }
    it("flowIn.bits.passedEntry propagates") {
      simulator() {
        dut =>

          val table = Table.inc(2)
          val index = Index.allInTable0()
          val indexValid = IndexValid.allTrue()
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, table, index, indexValid))(),
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true))(ePassedEntry(true)),
          )
      }
    }
    it("flowIn.bits.isFirstTable propagates") {
      simulator() {
        dut =>

          val table = Table.inc(2)
          val index = Index.allInTable0()
          val indexValid = IndexValid.allTrue()
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(2, 0, table, index, indexValid))(),
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table, isFirstTable = true))(
              _.out.flow.bits.isFirstTable.expect(true)
            ),
          )
      }
    }
  }

  describe("Reuse") {
    it("second instruction after flush") {
      simulator() {
        dut =>

          val t1 = (0 until 16).map(
            i => 0x10 + i
          )
          val e1 = t1.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          val t2 = (0 until 16).map(
            i => 0xa0 + i
          )
          val e2 = t2.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true)))(eWb1(e1)),
            Cycle(pNoPlace, pNoFlow, pFlush)(),
            Cycle(pNoPlace, pNoFlow, _.in.flush.poke(false))(),
            Cycle(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true)))(eWb1(e2)),
          )
      }
    }
    it("back-to-back without flush") {
      simulator() {
        dut =>

          val t1 = (0 until 16).map(
            i => 0x10 + i
          )
          val e1 = t1.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          val t2 = (0 until 16).map(
            i => 0xa0 + i
          )
          val e2 = t2.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true)))(eWb1(e1)),
            Cycle(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true)))(eWb1(e2)),
          )
      }
    }
  }

  describe("Writeback") {
    it("wb.valid holds until wbReady") {
      simulator() {
        dut =>

          val tblSize = 1
          val table = Table.inc(tblSize)
          val index = Index.allInTable0(tblSize)
          val indexValid = IndexValid.allTrue(tblSize)
          val prefill = Prefill.default(tblSize)
          val res = Res.gather(table, index, indexValid, prefill, Seq(0))

          runCycles(
            dut,
            initUnit,
            // Place table+index, blockSize=1 → counter=0 immediately
            Cycle(pPlace(tblSize, 0, table, index, indexValid, prefill))(eWb1(res)),
            // wbReady still false — wb.valid should HOLD
            Cycle(pNoPlace, pNoFlow)(eWb1(res)),
            Cycle(pNoPlace, pNoFlow)(eWb1(res)),
            // Now assert wbReady — wb should be accepted
            Cycle(pNoPlace, pNoFlow, pWbReady())(eWb0),
          )
      }
    }
  }
}
