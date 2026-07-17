package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._


class GatherRingController(implicit val param: GatherParam) extends GatherModule {
  val in = IO(Input(new GatherRingController.In))
  val out = IO(Output(new GatherRingController.Out))

  // ── Instantiate block controllers ────────────────────
  private val ring2Controllers = Seq.tabulate(NumUnits / 2)(i => Module(new GatherRingBlockController(2, 2 * i)))
  private val ring4Controllers = Seq.tabulate(NumUnits / 4)(i => Module(new GatherRingBlockController(4, 4 * i)))
  private val ring8Controllers = Seq.tabulate(NumUnits / 8)(i => Module(new GatherRingBlockController(8, 8 * i)))

  // ── Wire block controllers ───────────────────────────
  for ((bc, i) <- ring2Controllers.zipWithIndex) {
    val base = 2 * i
    bc.in.tableValid := in.hasTable.asUInt(base + 1, base)
    bc.in.indexValid := in.hasIndex.asUInt(base + 1, base)
    for (j <- 0 until NumPipeIn) {
      bc.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockBase.drop(1) === bc.blockBase.U(UnitIdx().getWidth.W).drop(1)
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  for ((bc, i) <- ring4Controllers.zipWithIndex) {
    val base = 4 * i
    bc.in.tableValid := in.hasTable.asUInt(base + 3, base)
    bc.in.indexValid := in.hasIndex.asUInt(base + 3, base)
    for (j <- 0 until NumPipeIn) {
      bc.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockBase.drop(2) === bc.blockBase.U(UnitIdx().getWidth.W).drop(2)
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  for ((bc, i) <- ring8Controllers.zipWithIndex) {
    bc.in.tableValid := in.hasTable.asUInt
    bc.in.indexValid := in.hasIndex.asUInt
    for (j <- 0 until NumPipeIn) {
      bc.in.pipePlaceIdx(j).valid := in.s0pipes(j).valid
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  // ── Aggregate block controller outputs ───────────────
  for (i <- 0 until NumPipeIn) {
    out.pipeTablePos(i) := Mux1HValidIO(
      (ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.tablePos(i))
    )
    out.pipeIndexPos(i) := Mux1HValidIO(
      (ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.indexPos(i))
    )
  }

  private val ring2WillFlowVec = Cat(ring2Controllers.map(_.out.willFlow).reverse)
  private val ring4WillFlowVec = Cat(ring4Controllers.map(_.out.willFlow).reverse)
  private val ring8WillFlowVec = Cat(ring8Controllers.map(_.out.willFlow).reverse)
  out.willFlow := VecInit((ring2WillFlowVec | ring4WillFlowVec | ring8WillFlowVec).asBools)

  // ── Writeback arbitration ────────────────────────────
  private val numElemInGroup = NumUnits / NumPipeOut
  private val groupFrom: Seq[Int] = Seq.tabulate(NumPipeOut)(_ * numElemInGroup)
  private val groupTo: Seq[Int] = groupFrom.map(begin => math.min(begin + numElemInGroup, NumUnits - 1))
  private val groupedValids: Seq[UInt] = groupFrom.zip(groupTo).map { case(from, to) => in.unitDone(to, from) }

  private val localSelOH: Seq[UInt] = groupedValids.map(PriorityEncoderOH.apply)

  out.wbSelOH := localSelOH.zipWithIndex.map {
    case (oh, i) => Cat(
      0.U((NumUnits - groupTo(i) - 1).W),
      oh,
      0.U(groupFrom(i).W)
    )
  }
}

object GatherRingController {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val s0pipes  = Vec(NumPipeIn, ValidIO(new GatherRing.PipeInS0))
    val hasTable = Input(Vec(NumUnits, Bool()))
    val hasIndex = Input(Vec(NumUnits, Bool()))
    val unitDone = UInt(NumUnits.W)
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    val pipeTablePos = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val pipeIndexPos = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val willFlow     = Output(Vec(NumUnits, Bool()))
    val wbSelOH      = Vec(NumPipeOut, UInt(NumUnits.W))
  }

  def main(args: Array[String]): Unit = {

  }
}
