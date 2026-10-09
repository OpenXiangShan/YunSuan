package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.simulator.{ChiselOptionsModifications, ExpectationValueFormat, FirtoolOptionsModifications, HasSimulator}
import chisel3.testing.HasTestingDirectory
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.{FakeRobPtr, GatherParam}

class GatherRingUnitSpec extends SpecBase {

  val vlen        = 128
  val dlen        = 128
  val maxTableNum = 8
  val issue2exu = GatherParam.default.issue2exu

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
      dut.in.flow.bits.blockSize.poke(8)
      dut.in.placeTable.valid.poke(false)
      dut.in.placeIndex.valid.poke(false)
      dut.in.wbReady.poke(false)
      dut.in.allTableValid.poke(false)
      dut.in.wakeAck.poke(false)
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

  def eResult(dest: BigInt)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>
      dut.out.status.done.expect(true.B, "result must be complete")
      dut.out.wb.bits.data.expect(dest, ExpectationValueFormat.Hex)
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

  def pPlaceTable(blockSize: Int, tblIdx: Int, tbl: Seq[Int], fieldNum: Int = -1): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      val isFirst = dut.unitIdx % blockSize == 0
      dut.in.placeTable.valid.poke(true)
      dut.in.placeTable.bits.tableIdx.poke(tblIdx)
      tbl.grouped(16).toSeq(tblIdx) .zipWithIndex.foreach {
        case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
      }
      dut.in.placeTable.bits.isFirstTable.poke(isFirst)
      dut.in.placeTable.bits.blockSize.poke(blockSize)
      dut.in.placeTable.bits.blockFieldNum.poke(if (fieldNum < 0) blockSize else fieldNum)
  }

  def pPlaceIndex(
    blkSize: Int,
    idx:     Seq[Int],
    iValid:  Seq[Boolean],
    prefill: Seq[Int] = Seq.fill(16)(0xFF),
    fScala:  Seq[Boolean] = Seq.fill(16)(false),
    fZeros:  Seq[Boolean] = Seq.fill(16)(false),
    scalarData: Seq[Int] = Seq.fill(8)(0),
    sew:     Int = 64,
    fieldNum: Int = -1
  ): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.placeIndex.bits.blockSize.poke(blkSize)
      dut.in.placeIndex.bits.blockFieldNum.poke(if (fieldNum < 0) blkSize else fieldNum)
      dut.in.placeIndex.valid.poke(true)
      for (i <- 0 until 16) {
        val flags = if (fZeros(i)) ElemFlags.ZERO else if (iValid(i)) ElemFlags.LOOKUP else ElemFlags.PREFILL
        val sByte = sew match { case 8 => scalarData(0); case 16 => scalarData(i % 2); case 32 => scalarData(i % 4); case _ => scalarData(i % 8) }
        val data  = if (fZeros(i)) 0 else if (fScala(i)) sByte else if (iValid(i)) idx(i) else prefill(i)
        dut.in.placeIndex.bits.elemFlags(i).poke(flags)
        dut.in.placeIndex.bits.elemData(i).poke(data.U)
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
    scalarData : Seq[Int] = Seq.fill(8)(0),
    sew        : Int = 64,
    fieldNum   : Int = -1
  ): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>
      pPlaceTable(blkSize, tblIdx, tbl, fieldNum)(dut)
      pPlaceIndex(blkSize, idx, iValid, prefill, scalarValid, zeroValid, scalarData, sew, fieldNum)(dut)
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

  def pFlowTbl(tblIdx: Int, tbl: Seq[Int], passedEntry: Boolean = false, isFirstTable: Boolean = false, blockSize: Int = -1): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.flow.valid.poke(true)
      dut.in.flow.bits.tableIdx.poke(tblIdx)
      tbl.grouped(16).toSeq(tblIdx).zipWithIndex.foreach {
        case (v, i) => dut.in.flow.bits.data(i).poke(v)
      }
      dut.in.flow.bits.isFirstTable.poke(isFirstTable)
      dut.in.flow.bits.passedEntry.poke(passedEntry)
      if (blockSize >= 0) dut.in.flow.bits.blockSize.poke(blockSize)
  }

  def pNoFlowTbl(): GatherRingUnit[FakeRobPtr] => Unit = _.in.flow.valid.poke(false)

  def pWakeCtx(allTableValid: Boolean): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.in.allTableValid.poke(allTableValid)
      dut.in.wakeAck.poke(false)
  }

  def eWake(valid: Boolean, delay: Int = 0)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit = {
    dut =>

      dut.out.wake.valid.expect(valid.B, "wake.valid mismatch")
      if (valid) dut.out.wake.bits.delay.expect(delay.U, "wake.delay mismatch")
  }

  def eFlowOut(valid: Boolean)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit =
    _.out.flow.valid.expect(valid.B, "flow.valid mismatch")

  def eBlockSize(v: Int)(implicit sourceInfo: SourceInfo): GatherRingUnit[FakeRobPtr] => Unit =
    _.out.status.blockSize.expect(v.U, "status.blockSize mismatch")

  // Controller-driven table drop: hasTable clears only when clearTable && passedEntry.
  def pClearTbl(v: Boolean = true): GatherRingUnit[FakeRobPtr] => Unit = _.in.clearTable.poke(v)

  def pWbReady(v: Boolean = true): GatherRingUnit[FakeRobPtr] => Unit = _.in.wbReady.poke(v)

  private def acceptWakeAndWriteback(dut: GatherRingUnit[FakeRobPtr], expected: BigInt)(implicit sourceInfo: SourceInfo): Unit = {
    pNoPlace(dut)
    pNoFlow(dut)
    pNoFlowTbl().apply(dut)
    pClearTbl().apply(dut)
    dut.in.wakeAck.poke(false.B)
    dut.in.wbReady.poke(true.B)
    for (_ <- 0 until 3) {
      dut.out.wake.valid.expect(true.B)
      dut.out.wake.bits.delay.expect(0.U)
      dut.out.status.hasIndex.expect(true.B, "ready alone cannot retire an unacked result")
      eWb(false).apply(dut)
      dut.clock.step()
    }

    dut.in.wbReady.poke(false.B)
    dut.in.wakeAck.poke(true.B)
    dut.clock.step()
    dut.in.wakeAck.poke(false.B)
    dut.out.wake.valid.expect(false.B)
    for (_ <- 0 until issue2exu - 1) {
      dut.out.status.hasIndex.expect(true.B)
      eWb(false).apply(dut)
      dut.clock.step()
    }
    for (_ <- 0 until 3) {
      eWb(true, expected).apply(dut)
      dut.out.status.hasIndex.expect(true.B, "writeback waits for ready")
      dut.clock.step()
    }
    dut.in.wbReady.poke(true.B)
    dut.clock.step()
    eWb(false).apply(dut)
    dut.out.status.hasIndex.expect(false.B)
    dut.out.status.hasTable.expect(false.B)
    dut.out.meta.valid.expect(false.B)
  }

  private def eT1  (implicit sourceInfo: SourceInfo)    = eHasTable(true)
  private def eT0  (implicit sourceInfo: SourceInfo)    = eHasTable(false)
  private def eI1  (implicit sourceInfo: SourceInfo)    = eHasIndex(true)
  private def eI0  (implicit sourceInfo: SourceInfo)    = eHasIndex(false)
  private def eWb0 (implicit sourceInfo: SourceInfo)    = eWb(false)
  private def eResult1(exp: BigInt)(implicit sourceInfo: SourceInfo) = eResult(exp)

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
      sew: Int = 64,
    ): BigInt = {
      require(index.size == indexValid.size)
      require(index.size == prefill.size)
      require(scalarTable.size == 8)

      def scalarByte(i: Int): Int = sew match {
        case 8 => scalarTable(0); case 16 => scalarTable(i % 2)
        case 32 => scalarTable(i % 4); case _ => scalarTable(i % 8)
      }

      index.indices.map(
        i =>
          if (indexValid(i)) {
            if (tableRange.contains(i / 16))
              table(index(i))
            else
              index(i)
          }
          else if (scalarValid(i)) {
            scalarByte(i)
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
            Cycle(pPlaceIndex(tblSize, index, indexValid, prefill))(eT1, eI1, eWb0, eIndexOrRes(res)),
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
            Cycle(pPlace(tblSize, 0, table, index, indexValid, prefill))(eT1, eI1, eWb0),
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

  describe("hole block (segment fieldNum < size)") {

    it("counter initializes from blockFieldNum with first-table discount (seg3: f=3 in size 4)") {
      simulator() {
        dut =>

          val table = Table.inc(4)
          val index = Index.unzip4(1)

          runCycles(
            dut,
            initUnit,
            // entry unit (unitIdx=0): first table + index same cycle -> f-1 discount
            Cycle(pPlace(4, 0, table, index, IndexValid.allTrue(), Prefill.default(1), fieldNum = 3))(eT1, eI1, eCounter(2)),
          )
      }
    }

    it("counter initializes to fieldNum without a first table (no discount)") {
      simulator() {
        dut =>

          val index = Index.unzip4(1)

          runCycles(
            dut,
            initUnit,
            Cycle(pPlaceIndex(4, index, IndexValid.allTrue(), Prefill.default(1), fieldNum = 3))(eI1, eCounter(3)),
          )
      }
    }

    it("does not wake a hole block entry before its result completes") {
      simulator() {
        dut =>

          val table = Table.inc(4)
          val index = Index.unzip4(1)

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(4, 0, table, index, IndexValid.allTrue(), Prefill.default(1), fieldNum = 3))(eWake(false)),
            // counter=2 <= issue2exu with allTableValid set: a full block would wake here
            Cycle(pNoPlace, pNoFlow, pWakeCtx(true))(eWake(false)),
          )
      }
    }

    it("full block does not wake before its result completes") {
      simulator() {
        dut =>

          val table = Table.inc(4)
          val index = Index.unzip4(1)

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(4, 0, table, index, IndexValid.allTrue(), Prefill.default(1)))(eWake(false), eCounter(3)),
            Cycle(pNoPlace, pNoFlow, pWakeCtx(true))(eWake(false), eCounter(3)),
          )
      }
    }

    it("holds a completed result while the ring waits for wake ack") {
      simulator() { dut =>
        val table = Table.inc()
        val index = Index.allInTable0(1)
        val noLookups = Seq.fill(16)(false)
        initUnit(dut)
        pPlace(1, 0, table, index, noLookups)(dut)
        pWbReady(false)(dut)
        pWakeCtx(true)(dut)
        dut.clock.step()
        pNoPlace(dut)

        for (_ <- 0 until 6) {
          dut.out.status.hasIndex.expect(true.B)
          dut.out.status.counter.expect(0.U)
          dut.out.wake.valid.expect(true.B)
          dut.out.wake.bits.delay.expect(0.U)
          dut.out.wb.valid.expect(false.B, "result waits for an accepted wake")
          dut.clock.step()
        }

        dut.in.wakeAck.poke(true)
        dut.out.wake.valid.expect(true.B)
        dut.out.wake.bits.delay.expect(0.U)
        dut.out.wb.valid.expect(false.B)
        dut.clock.step()
        dut.in.wakeAck.poke(false)
        dut.out.wake.valid.expect(false.B)
        for (_ <- 0 until 2) {
          dut.out.wb.valid.expect(false.B)
          dut.clock.step()
        }
        pWbReady()(dut)
        dut.out.wb.valid.expect(true.B)
        dut.clock.step()
        dut.out.status.hasIndex.expect(false.B)
      }
    }

    it("tail hole derives isLastUnit from the flowing table's blockSize (unitIdx=3, size=4)") {
      simulator(unitIdx = 3) {
        dut =>

          val table = Table.inc()

          runCycles(
            dut,
            initUnit,
            // virgin unit (never placed): latch blockSize from the flow (D7)
            Cycle(pFlow, pFlowTbl(0, table, passedEntry = false, blockSize = 4))(eT1, eBlockSize(4), eFlowOut(true)),
            // D8: isLastUnit derived = true -> after passedEntry the tail stops offering
            Cycle(pFlow, pFlowTbl(0, table, passedEntry = true, blockSize = 4))(eT1, eFlowOut(false)),
          )
      }
    }

    it("mid-ring unit keeps forwarding after passedEntry (unitIdx=1, size=4)") {
      simulator(unitIdx = 1) {
        dut =>

          val table = Table.inc()

          runCycles(
            dut,
            initUnit,
            Cycle(pFlow, pFlowTbl(0, table, passedEntry = false, blockSize = 4))(eT1, eFlowOut(true)),
            Cycle(pFlow, pFlowTbl(0, table, passedEntry = true, blockSize = 4))(eT1, eFlowOut(true)),
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
            Cycle(pPlace(1, 0, t, 0 until 16, Seq.fill(16)(true)))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, idx, Seq.fill(16)(true)))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, perm, Seq.fill(16)(true)))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, table, index, indexValid, prefill))(eResult1(res), eIndexOrRes(res)),
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
    def res(vl: Int) = Res.gather(table, index, indexValid(vl), prefill, 0 to 0, scalarValid = scalarValid(vl), scalarTable = scalarTable, sew = sew)

    it("SEW=8") {
      sew = 8
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid(15), prefill = prefill, scalarValid = scalarValid(15), scalarData = scalarTable, sew = sew))(eResult1(res(15))),
            Cycle(pPlace(1, 0, table, index, indexValid(14), prefill = prefill, scalarValid = scalarValid(14), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(14))),
            Cycle(pPlace(1, 0, table, index, indexValid(13), prefill = prefill, scalarValid = scalarValid(13), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(13))),
            Cycle(pPlace(1, 0, table, index, indexValid(12), prefill = prefill, scalarValid = scalarValid(12), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(12))),
            Cycle(pPlace(1, 0, table, index, indexValid(11), prefill = prefill, scalarValid = scalarValid(11), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(11))),
            Cycle(pPlace(1, 0, table, index, indexValid(10), prefill = prefill, scalarValid = scalarValid(10), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(10))),
            Cycle(pPlace(1, 0, table, index, indexValid( 9), prefill = prefill, scalarValid = scalarValid( 9), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res( 9))),
            Cycle(pPlace(1, 0, table, index, indexValid( 8), prefill = prefill, scalarValid = scalarValid( 8), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res( 8))),
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
            Cycle(pPlace(1, 0, table, index, indexValid(7), prefill = prefill, scalarValid = scalarValid(7), scalarData = scalarTable, sew = sew))(eResult1(res(7))),
            Cycle(pPlace(1, 0, table, index, indexValid(6), prefill = prefill, scalarValid = scalarValid(6), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(6))),
            Cycle(pPlace(1, 0, table, index, indexValid(5), prefill = prefill, scalarValid = scalarValid(5), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(5))),
            Cycle(pPlace(1, 0, table, index, indexValid(4), prefill = prefill, scalarValid = scalarValid(4), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(4))),
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
            Cycle(pPlace(1, 0, table, index, indexValid(3), prefill = prefill, scalarValid = scalarValid(3), scalarData = scalarTable, sew = sew))(eResult1(res(3))),
            Cycle(pPlace(1, 0, table, index, indexValid(2), prefill = prefill, scalarValid = scalarValid(2), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(2))),
            Cycle(pPlace(1, 0, table, index, indexValid(1), prefill = prefill, scalarValid = scalarValid(1), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(1))),
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
            Cycle(pPlace(1, 0, table, index, indexValid(1), prefill = prefill, scalarValid = scalarValid(1), scalarData = scalarTable, sew = sew))(eResult1(res(1))),
            Cycle(pPlace(1, 0, table, index, indexValid(0), prefill = prefill, scalarValid = scalarValid(0), scalarData = scalarTable, sew = sew), pWbReady())(eResult1(res(0))),
            Cycle(pNoPlace, pWbReady())(eWb0)
          )
      }

    }

    it("SEW=8 scalar at vl-1=14: rs1[7:0] should replicate to position 14") {
      sew = 8
      // Distinct scalarTable bytes: [0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, 0xD0, 0xD1]
      val distinctST = ScalarTable.default(8)
      // vslide1down: scalar at position vl-1 = 14
      val sAt14 = (0 until 16).map(i => i / sewByte == 14)
      val allInvalid = Seq.fill(16)(false)

      // Golden: SEW=8 scalar is rs1[7:0] (byte 0), replicated to position 14.
      // RTL currently picks scalaTableBytes(14 % 8) = rs1[55:48] (byte 6) — BUG.
      val golden = (0 until 16).map(i =>
        if (sAt14(i)) distinctST(0) // correct: rs1[7:0]
        else prefill(i)
      ).foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

      simulator() { dut =>
        runCycles(
          dut,
          initUnit,
          Cycle(
            pPlace(1, 0, table, index, allInvalid, prefill = prefill, scalarValid = sAt14, scalarData = distinctST, sew = 8)
          )(eResult1(golden)),
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
    def res = Res.gather(table, index, indexValid, prefill, 0 to 0, scalarValid = scalarValid, scalarTable = scalarTable, sew = sew)

    it("SEW=8") {
      sew = 8
      simulator() {
        dut =>

          runCycles(
            dut,
            initUnit,
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid, scalarData = scalarTable, sew = sew))(eResult1(res)),
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
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid, scalarData = scalarTable, sew = sew))(eResult1(res)),
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
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid, scalarData = scalarTable, sew = sew))(eResult1(res)),
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
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid, scalarData = scalarTable, sew = sew))(eResult1(res)),
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
            Cycle(pPlace(1, 0, table, index, indexValid, scalarValid = scalarValid, scalarData = scalarTable, sew = 16))(eResult1(res)),
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
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs, scalarData = Seq.fill(8)(0x5A), sew = 16))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs, scalarData = Seq.fill(8)(0x5A), sew = 16))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, idx, iv, scalarValid = fs, scalarData = Seq.fill(8)(0x5A), sew = 16))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, idx, iv, zeroValid = fz))(eResult1(exp)),
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
            Cycle(pPlace(1, 0, t, idx, iv, zeroValid = fz))(eResult1(exp)),
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
              pPlace(1, 0, t, idx, iv, scalarValid = fs, zeroValid = fz, scalarData = Seq.fill(8)(0x5A), sew = 16)
            )(eResult1(exp))
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
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)))(eCounter(1), eWb0, eIndexOrRes(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(false), pClearTbl())(eI1, eT0, eResult1(res)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(), pClearTbl())(eI1, eT0, eWb0),
            )
            acceptWakeAndWriteback(dut, res)
        }

        simulator(unitIdx = 1) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = 1 to 1)
            val res1 = Res.gather(table, index, tableRange = 0 to 0)

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)))(eCounter(2), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table, passedEntry = true))(eCounter(1), eI1, eT1, eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res1)
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
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table, passedEntry = true), pWbReady(false))(eCounter(0), eI1, eT1, eResult1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res1)
        }

        simulator(unitIdx = 1) {
          dut =>

            val res = Res.gather(table, index)

            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)), pWbReady(false))(eCounter(2), eWb0, eIndexOrRes(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady(false))(eCounter(1), eI1, eT1, eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res)),
              Cycle(pFlow, pNoFlowTbl(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res)
        }
      }

      it("case 2") {
        simulator(unitIdx = 0) {
          dut =>

            val res = Res.gather(table, index)
            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 0, table, index, Seq.fill(16)(true)))(eCounter(1), eWb0, eIndexOrRes(res)),
              Cycle(pNoPlace, pWbReady())(eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(false), pClearTbl())(eI1, eT0, eResult1(res)),
              Cycle(pNoFlow, pNoFlowTbl(), pWbReady(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res)
        }

        simulator(unitIdx = 1) {
          dut =>

            val res0 = Res.gather(table, index, tableRange = 1 to 1)
            val res1 = Res.gather(table, index, tableRange = 0 to 0)

            runCycles(
              dut,
              initUnit,
              Cycle()(eT0, eI0, eWb0),
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)))(eCounter(2), eWb0, eIndexOrRes(res0)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table, passedEntry = true))(eCounter(1), eI1, eT1, eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pWbReady(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res1)
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
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res1)),
              Cycle(pFlow, pNoFlowTbl(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res1)
        }

        simulator(unitIdx = 1) {
          dut =>

            val res = Res.gather(table, index)
            runCycles(
              dut,
              initUnit,
              Cycle(pPlace(2, 1, table, index, Seq.fill(16)(true)), pWbReady())(eCounter(2), eWb0, eIndexOrRes(res)),
              Cycle(pNoPlace, pFlow, pFlowTbl(0, table, passedEntry = true), pWbReady(false))(eCounter(1), eI1, eT1, eWb0),
              Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true), pWbReady())(eCounter(0), eI1, eT1, eResult1(res)),
              Cycle(pFlow, pNoFlowTbl(), pClearTbl())(eI1, eT0, eWb0)
            )
            acceptWakeAndWriteback(dut, res)
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
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true))(eResult1(res)),
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
            Cycle(pNoPlace, pFlow, pFlowTbl(1, table, passedEntry = true))(eWb0, eIndexOrRes(res1)),
            Cycle(pNoPlace, pFlow, pFlowTbl(2, table, passedEntry = true))(eWb0, eIndexOrRes(res2)),
            Cycle(pNoPlace, pFlow, pFlowTbl(3, table, passedEntry = true))(eResult1(res3)),
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
            Cycle(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true)))(eResult1(e1)),
            Cycle(pNoPlace, pNoFlow, pFlush)(),
            Cycle(pNoPlace, pNoFlow, _.in.flush.poke(false))(),
            Cycle(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true)))(eResult1(e2)),
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
            Cycle(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true)))(eResult1(e1)),
            Cycle(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true)))(eResult1(e2)),
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
            // Place table+index, blockSize=1 -> counter=0 immediately
            Cycle(pPlace(tblSize, 0, table, index, indexValid, prefill))(eResult1(res)),
          )
          acceptWakeAndWriteback(dut, res)
      }
    }
  }
}
