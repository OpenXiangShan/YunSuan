package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._


class GatherRingBlockController(val size: Int, val blockBase: Int)(implicit val param: GatherParam) extends GatherModule {
  require(Seq(2, 4, 8).contains(size))
  require(blockBase % size == 0)

  val localIndexWidth = log2Ceil(size)

  val in = IO(Input(new GatherRingBlockController.In(size)))
  val out = IO(Output(new GatherRingBlockController.Out(size)))

  val tableFree = ~Cat(Reverse(in.tableValid.drop(1)), in.tableValid.take(1))
  val indexFree = ~in.indexValid

  // size = 2, tableSeq should be Seq(0, 1)
  // size = 4, tableSeq should be Seq(0, 3, 2, 1)
  // size = 8, tableSeq should be Seq(0, 7, 6, ..., 1)
  val tableMap = VecInit((0 +: Seq.tabulate(size - 1)(x => size - x)).map(_.U))

  val entryTable: UInt = tableMap(PriorityEncoder(tableFree))
  val entryIndex: UInt = PriorityEncoder(indexFree)

  // will flow
  // The least significant free position of tableValid less than the most significant valid position of indexValid
  val tableLeastFreeOH = PriorityEncoderOH(~in.tableValid)
  val indexMostValidOH = Reverse(PriorityEncoderOH(Reverse(in.indexValid)))
  val willFlow = !(tableLeastFreeOH.rotateRight(1) & indexMostValidOH).orR

  // Per-pipe table/index position outputs
  for (i <- 0 until NumPipeIn) {
    out.tablePos(i).valid := in.pipePlaceIdx(i).valid
    out.indexPos(i).valid := in.pipePlaceIdx(i).valid

    out.tablePos(i).bits := blockBase.U |
      (entryTable - in.pipePlaceIdx(i).bits).ensuring(_.getWidth == localIndexWidth)
    out.indexPos(i).bits := blockBase.U |
      (entryIndex + willFlow + in.pipePlaceIdx(i).bits).ensuring(_.getWidth == localIndexWidth)
  }

  // willFlow: block-level signal, computed once outside per-pipe loop
  out.willFlow := Cat(in.tableValid.map(_ && willFlow).reverse)
}

object GatherRingBlockController {
  class In(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val pipePlaceIdx: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(PipeInIdx()))

    val tableValid: UInt = UInt(size.W)
    val indexValid: UInt = UInt(size.W)

    // Assert: placeIdx must be less than block size
    for (i <- 0 until NumPipeIn) {
      when(pipePlaceIdx(i).valid) {
        assert(pipePlaceIdx(i).bits < size.U, "placeIdx must be < blockSize")
      }
    }
  }

  class Out(size: Int)(implicit val param: GatherParam) extends GatherBundle {
    val tablePos: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val indexPos: Vec[ValidIO[UInt]] = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val willFlow: UInt = UInt(size.W)
  }

  def main(args: Array[String]): Unit = {

  }
}
