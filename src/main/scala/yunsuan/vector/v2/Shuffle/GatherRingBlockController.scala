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
  val indexMostValidOH = Reverse(PriorityEncoderOH(Reverse(indexValid)))
  // Either one suffices: a uop entering GatherRing always carries both table and index groups
  // allFull: when all table slots are occupied, existing tables must still circulate
  val s1WillFlow = (tableValid.orR || indexValid.orR) &&
    (tableValid.head(1).asBool || !(tableLeastFreeOH.rotateRight(1) & indexMostValidOH).orR)

  val unitTableFirstValid = PriorityEncoderOH(~(unitTableValid).rotateRight(1))
  val unitIndexMaskValidOH = Reverse(PriorityEncoderOH(Reverse(unitIndexValid)))
  val unitWillFlow = unitTableValid.orR &&
    (in.counter.last > 0.U || !(unitTableFirstValid & unitIndexMaskValidOH).orR)

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
    out.tablePos(i).valid := inPipePlaceIdx(i).valid
    out.indexPos(i).valid := inPipePlaceIdx(i).valid

    out.tablePos(i).bits := blockBase.U |
      (entryTable + s1WillFlow - inPipePlaceIdx(i).bits)
    // SEW=8 vrgatherei16: deterministic same-unit routing for gather i / rvs3 i
    // (placeIdx = uopIdx % blockSize), independent of the dynamic entryIndex.
    out.indexPos(i).bits := blockBase.U |
      Mux(in.pipeIsEi16E8(i), inPipePlaceIdx(i).bits, entryIndex + inPipePlaceIdx(i).bits)
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
}

object GatherRingBlockController {
  class In(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val pipePlaceIdx: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(PipeInIdx()))
    // SEW=8 vrgatherei16: 1 = route this pipe's index deterministically
    // (blockBase | placeIdx), bypassing the entryIndex free-slot search.
    val pipeIsEi16E8: Vec[Bool] = Vec(NumPipeIn, Bool())

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
