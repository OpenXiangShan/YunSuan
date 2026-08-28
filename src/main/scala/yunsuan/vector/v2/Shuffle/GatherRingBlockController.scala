package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherCommon._


class GatherRingBlockController(val size: Int, val blockBase: Int)(implicit val param: GatherParam) extends GatherModule {
  require(Seq(2, 4, 8).contains(size))
  require(blockBase % size == 0)

  override def desiredName: String = super.desiredName + s"_size${size}" + s"_base${blockBase}"

  val localIndexWidth = log2Ceil(size)

  val in = IO(Input(new GatherRingBlockController.In(size)))
  val out = IO(Output(new GatherRingBlockController.Out(size)))

  val unitTableValid = in.unitTableValid
  val unitIndexValid = in.unitIndexValid

  val unitTableValidNextCycle = Wire(UInt(size.W))

  // Merged occupancy for placement: registered (unit) + in-flight (s1).
  // clearTable only trusts the registered (unit) indices, never the in-flight
  // s1 ones (see the per-j unitIndexValid gate below).
  val tableValid = unitTableValidNextCycle | in.s1TableValid
  val indexValid = unitIndexValid | in.s1IndexValid

  val inPipePlaceIdx: Vec[ValidIO[UInt]] = VecInit(in.pipePlaceIdx.map(_.map(x => x.take(log2Ceil(size) min x.getWidth))))

  // size = 2, tableSeq should be Seq(0, 1)
  // size = 4, tableSeq should be Seq(0, 3, 2, 1)
  // size = 8, tableSeq should be Seq(0, 7, 6, ..., 1)
  val tableMap = VecInit((0 +: Seq.tabulate(size - 1)(x => size - 1 - x)).map(_.U))

  // will flow
  // The least significant free position of tableValid less than the most significant valid position of indexValid
  val tableLeastFreeOH = PriorityEncoderOH(~tableValid)
  // Same contiguous-run-from-0 semantics as unitIndexMaskValidOH: indices are
  // placed contiguously from unit 0, so "most valid index" must be the LAST set
  // bit of the run starting at 0, not the raw MSB (which over-reports on a
  // gapped indexValid like {0,2} and wrongly keeps s1WillFlow alive / skews
  // entryTable placement). First 0 up from bit 0 (extra high 0 catches the
  // all-ones case), one-hot it, then Cat(1, OH) >> 1 keeps the run prefix.
  val firstZeroOHIndex = PriorityEncoderOH(~Cat(0.U(1.W), indexValid))
  val indexMostValidOH = (Cat(1.U, firstZeroOHIndex) >> 1).take(size)
  // Either one suffices: a uop entering GatherRing always carries both table and index groups
  // allFull: when all table slots are occupied, existing tables must still circulate
  // indexHasAllValid: latched "all indices of this block are placed". Unlike the
  // level unitIndexValid.andR (which collapses as soon as an early unit clears
  // its index), it stays set so flow keeps circulating until every unit's table
  // traversal completes, even when SEW=8 ei16 indices are filled by uopIdx
  // (out-of-order). Set once every unit holds an index; cleared once every
  // index is gone (block done). Replaces the old counter.last>0 flow gate,
  // which assumed indices are placed in arrival order (counter.last is the
  // slowest only then).
  val indexHasAllValid = RegInit(false.B)
  when(indexValid.andR) {
    indexHasAllValid := true.B
  }.elsewhen(unitIndexValid === 0.U) {
    indexHasAllValid := false.B
  }

  val s1TableFirstValid = PriorityEncoderOH(~(tableValid).rotateRight(1))

  val s1WillFlow = (tableValid.orR || indexValid.orR) && (
    indexHasAllValid ||
    !(s1TableFirstValid & indexMostValidOH).orR && (s1TableFirstValid & indexValid).orR
  )

  val unitTableFirstValid = PriorityEncoderOH(~(unitTableValid).rotateRight(1))
  // unitIndexMaskValidOH: one-hot at the LAST set bit of the contiguous run of
  // placed indices starting at unit 0 (the filled-region boundary).
  val firstZeroOH = PriorityEncoderOH(~Cat(0.U(1.W), unitIndexValid))
  val unitIndexMaskValidOH = (Cat(1.U, firstZeroOH) >> 1).take(size)
  // unitWillFlow: keep flowing while (a) not all indices are placed yet, or
  // (b) the first table does NOT sit on a unit that both lies at the index
  // region boundary AND holds a valid index. Flow stops only when both hold:
  // the first table reached the boundary unit AND that unit's index is valid
  // (its traversal can complete). Checking the first table's own unit index
  // directly (instead of only the contiguous-run boundary) is robust to
  // out-of-order / gapped index placement (SEW=8 ei16 fills indices by uopIdx).
  val unitWillFlow = unitTableValid.orR && (
    indexHasAllValid ||
    !(unitTableFirstValid & unitIndexMaskValidOH).orR && (unitTableFirstValid & unitIndexValid).orR
  )

  // Predict table occupancy next cycle (if flowing, tables rotate right by 1):
  // placement must not target a slot that the flow will occupy in the same cycle
  // (fixes the placement-vs-flow collision, Q18/M10).
  unitTableValidNextCycle := Mux(
    unitWillFlow,
    unitTableValid.rotateLeft(1),
    unitTableValid,
  )

  val tableBusy = unitTableValidNextCycle | in.s1TableValid

  val tableFree = ~Cat(Reverse(tableBusy.drop(1)), tableBusy.take(1))
  val indexFree = ~indexValid

  val entryTable: UInt = WireInit(tableMap(PriorityEncoder(tableFree)))
  val entryIndex: UInt = WireInit(PriorityEncoder(indexFree))

  // clearTable(i): the table in unit i is useless if every unit j>i finishes
  // (counter(j) < j-i) before unit i's table can reach it (distance j-i in the
  // ring). Empty set (last unit) => true; the unit's hasTable clear is
  // additionally gated by passedEntry.
  // Each j>i must have its index PLACED (registered unitIndexValid(j)): an
  // in-flight s1 index has counter still unset, so without this it would be
  // misread as "finished" and clear an earlier unit's table prematurely
  // (Q7/Q10). Unit i's own index is intentionally not required, so a unit can
  // still clear its table after it writes back (Q14).
  val clearTable = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    clearTable(i) := (i + 1 until size).map { j =>
      in.counter(j) < (j - i).U && unitIndexValid(j)
    }.fold(true.B)(_ && _)
  }

  // Per-pipe table/index position outputs
  for (i <- 0 until NumPipeIn) {
    // A table is placed only by the gather uop: an rvs3 (isRvs3=1) carries
    // old-data (PREFILL index only), so it must not assert tablePos.valid.
    out.tablePos(i).valid := inPipePlaceIdx(i).valid && !in.pipeIsRvs3(i)
    out.indexPos(i).valid := inPipePlaceIdx(i).valid

    out.tablePos(i).bits := blockBase.U |
      (entryTable + s1WillFlow - inPipePlaceIdx(i).bits)
    // SEW=8 ei16: deterministic same-unit routing for gather i / rvs3 i
    // (indexPos = uopIdx % blockSize = low log2(size) bits of uopIdx), bypassing
    // the dynamic entryIndex free-slot search.
    out.indexPos(i).bits := blockBase.U |
      Mux(in.pipeIsEi16E8(i), in.pipeUopIdx(i).take(log2Ceil(size)), entryIndex + inPipePlaceIdx(i).bits)
  }

  // willFlow: block-level signal, computed once outside per-pipe loop
  out.willFlow := Cat((unitTableValid | unitIndexValid).map(_ && unitWillFlow).reverse)

  out.clearTable := clearTable.asUInt

  // allTableHasFilled: latched "all tables of the block are placed". Unlike the
  // pure level unitTableValid.andR (which collapses as soon as an early unit
  // clears its table), it stays set so uops that arrive late (out-of-order /
  // multi-cycle dispatch) can still wake once their counter drops to <=3.
  // Cleared once every unit's flow has completed (counter == 0), so a reused
  // block always re-assembles from 0 (default 0).
  val allTableHasFilled = RegInit(false.B)

  when(unitTableValid.andR) {
    allTableHasFilled := true.B
  }
  // All counters zero = every unit's flow has completed (block done). Clearing
  // here rather than on per-unit wake keeps the latch set through the late uops'
  // wake window, and the tables clear in the same cycle (clearTable depends on
  // counter), so the level set cannot re-fire afterwards.
  when(in.counter.map(_ === 0.U).reduce(_ && _)) {
    allTableHasFilled := false.B
  }

  // allTableValid: block-level, all tables of this block are valid (gates the vperm wakeup)
  out.allTableValid := Fill(size, allTableHasFilled)

  dontTouch(s1WillFlow)
  dontTouch(entryIndex)
  dontTouch(entryTable)
  dontTouch(out.clearTable)
  dontTouch(indexMostValidOH)
}

object GatherRingBlockController {
  class In(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val pipePlaceIdx: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(PipeInIdx()))
    // SEW=8 vrgatherei16: 1 = route this pipe's index deterministically
    // (blockBase | placeIdx), bypassing the entryIndex free-slot search.
    val pipeIsEi16E8: Vec[Bool] = Vec(NumPipeIn, Bool())
    // SEW=8 vrgatherei16: 1 = this pipe's uop is an rvs3 (old-data) half; it
    // places an index but no table, so tablePos.valid must stay 0 for it.
    val pipeIsRvs3: Vec[Bool] = Vec(NumPipeIn, Bool())
    // SEW=8 vrgatherei16: per-pipe uop index, used for the deterministic
    // same-unit routing (indexPos = uopIdx % blockSize).
    val pipeUopIdx: Vec[UInt] = Vec(NumPipeIn, UopIdx())

    // Registered (in-unit) and in-flight (s1) occupancy, kept separate so that
    // placement can use both but clearTable only trusts registered indices.
    val unitTableValid: UInt = UInt(size.W)
    val unitIndexValid: UInt = UInt(size.W)
    val s1TableValid: UInt = UInt(size.W)
    val s1IndexValid: UInt = UInt(size.W)
    val indexValidNext: UInt = UInt(size.W)
    val counter: Vec[UInt] = Vec(size, UInt(log2Up(NumUnits + 1).W))
  }

  class Out(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val tablePos: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val indexPos: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val willFlow: UInt = UInt(size.W)
    val clearTable: UInt = UInt(size.W)
    val allTableValid: UInt = UInt(size.W)
  }

  def main(args: Array[String]): Unit = {

  }
}
