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
  
  // Merged occupancy for placement: registered (unit) + in-flight (s1).
  // clearTable only trusts the registered (unit) indices, never the in-flight
  // s1 ones (see the per-j unitIndexValid gate below).
  val tableValid = unitTableValid | in.s1TableValid
  val indexValid = unitIndexValid | in.s1IndexValid

  val inPipePlaceIdx: Vec[ValidIO[UInt]] = VecInit(in.pipePlaceIdx.map(_.map(x => x.take(log2Ceil(size) min x.getWidth))))

  val tableFree = ~Cat(Reverse(tableValid.drop(1)), tableValid.take(1))
  val indexFree = ~indexValid

  // size = 2, tableSeq should be Seq(0, 1)
  // size = 4, tableSeq should be Seq(0, 3, 2, 1)
  // size = 8, tableSeq should be Seq(0, 7, 6, ..., 1)
  val tableMap = VecInit((0 +: Seq.tabulate(size - 1)(x => size - 1 - x)).map(_.U))

  val entryTable: UInt = WireInit(tableMap(PriorityEncoder(tableFree)))
  val entryIndex: UInt = WireInit(PriorityEncoder(indexFree))

  // will flow
  // The least significant free position of tableValid less than the most significant valid position of indexValid
  val tableLeastFreeOH = PriorityEncoderOH(~tableValid)
  val indexMostValidOH = Reverse(PriorityEncoderOH(Reverse(indexValid)))
  // Either one suffices: a uop entering GatherRing always carries both table and index groups
  // allFull: when all table slots are occupied, existing tables must still circulate
  val s1WillFlow = (tableValid.orR || indexValid.orR) &&
    (tableValid.head(1).asBool || !(tableLeastFreeOH.rotateRight(1) & indexMostValidOH).orR)

  val unitTableLeastFreeOH = PriorityEncoderOH(~unitTableValid)
  val unitIndexMaskValidOH = Reverse(PriorityEncoderOH(Reverse(unitIndexValid)))
  val unitWillFlow = (unitTableValid.orR || unitIndexValid.orR) &&
    (in.counter.last > 0.U || !(unitTableLeastFreeOH.rotateRight(1) & unitIndexMaskValidOH).orR)

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
    out.indexPos(i).bits := blockBase.U |
      (entryIndex + inPipePlaceIdx(i).bits)
  }

  // willFlow: block-level signal, computed once outside per-pipe loop
  out.willFlow := Cat((unitTableValid | unitIndexValid).map(_ && unitWillFlow).reverse)

  out.clearTable := clearTable.asUInt

  // allTableValid: block-level, all tables of this block are valid (gates the vperm wakeup)
  out.allTableValid := Fill(size, tableValid.andR)

  dontTouch(entryIndex)
  dontTouch(entryTable)
  dontTouch(out.clearTable)
}

object GatherRingBlockController {
  class In(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val pipePlaceIdx: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(PipeInIdx()))

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
