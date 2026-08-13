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

  val inPipePlaceIdx: Vec[ValidIO[UInt]] = VecInit(in.pipePlaceIdx.map(_.map(x => x.take(log2Ceil(size) min x.getWidth))))

  val tableFree = ~Cat(Reverse(in.tableValid.drop(1)), in.tableValid.take(1))
  val indexFree = ~in.indexValid

  // size = 2, tableSeq should be Seq(0, 1)
  // size = 4, tableSeq should be Seq(0, 3, 2, 1)
  // size = 8, tableSeq should be Seq(0, 7, 6, ..., 1)
  val tableMap = VecInit((0 +: Seq.tabulate(size - 1)(x => size - 1 - x)).map(_.U))

  val entryTable: UInt = tableMap(PriorityEncoder(tableFree))
  val entryIndex: UInt = PriorityEncoder(indexFree)

  // will flow
  // The least significant free position of tableValid less than the most significant valid position of indexValid
  val tableLeastFreeOH = PriorityEncoderOH(~in.tableValid)
  val indexMostValidOH = Reverse(PriorityEncoderOH(Reverse(in.indexValid)))
  val allFull = in.tableValid.andR
  // Either one suffices: a uop entering GatherRing always carries both table and index groups
  // allFull: when all table slots are occupied, existing tables must still circulate
  val willFlow = (in.tableValid.orR || in.indexValid.orR) &&
    (allFull || !(tableLeastFreeOH.rotateRight(1) & indexMostValidOH).orR)

  // clearTable(i): the table in unit i is useless if every unit j>i finishes
  // (counter(j) < j-i) before unit i's table can reach it (distance j-i in the ring).
  // Empty set (last unit) => true; the unit's hasTable clear is additionally gated by passedEntry.
  // All clears require the whole block's indices to be placed (indexValid.andR):
  // a not-yet-placed uop has counter=0 which would otherwise be misread as
  // "that unit has finished" and prematurely clear an earlier unit's table.
  val allIndexPlaced = in.indexValid.andR
  val clearTable = Wire(Vec(size, Bool()))
  for (i <- 0 until size) {
    clearTable(i) := allIndexPlaced && (i + 1 until size).map { j =>
      in.counter(j) < (j - i).U
    }.fold(true.B)(_ && _)
  }

  // Per-pipe table/index position outputs
  for (i <- 0 until NumPipeIn) {
    out.tablePos(i).valid := inPipePlaceIdx(i).valid
    out.indexPos(i).valid := inPipePlaceIdx(i).valid

    out.tablePos(i).bits := blockBase.U |
      (entryTable + willFlow - inPipePlaceIdx(i).bits)
    out.indexPos(i).bits := blockBase.U |
      (entryIndex + inPipePlaceIdx(i).bits)
  }

  // willFlow: block-level signal, computed once outside per-pipe loop
  out.willFlow := Cat((in.tableValid & in.indexValid).map(_ && willFlow).reverse)

  out.clearTable := clearTable.asUInt

  // allTableValid: block-level, all tables of this block are valid (gates the vperm wakeup)
  out.allTableValid := Fill(size, in.tableValid.andR)
}

object GatherRingBlockController {
  class In(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val pipePlaceIdx: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(PipeInIdx()))

    val tableValid: UInt = UInt(size.W)
    val indexValid: UInt = UInt(size.W)
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
