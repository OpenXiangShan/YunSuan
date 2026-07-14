package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._


class GatherRingController(implicit val param: Param) extends GatherModule {
  val in = IO(Input(new GatherRingController.In))
  val out = IO(Output(new GatherRingController.Out))

  val numElemInGroup = NumUnits / NumPipeOut

  val groupFrom: Seq[Int] = Seq.tabulate(NumPipeOut)(_ * numElemInGroup)
  val groupTo: Seq[Int] = groupFrom.map(begin => math.min(begin + numElemInGroup, NumUnits - 1))
  val groupedValids: Seq[UInt] = groupFrom.zip(groupTo).map { case(from, to) => in.unitDone(to, from) }

  val localSelOH: Seq[UInt] = groupedValids.map(PriorityEncoderOH.apply)

  val wbSelOH: Seq[UInt] = localSelOH.zipWithIndex.map {
    case (oh, i) => Cat(
      0.U((NumUnits - groupTo(i)).W),
      oh,
      0.U(groupFrom(i).W)
    ).ensuring(_.getWidth == NumUnits)
  }

  out.wbSelOH := wbSelOH
}

object GatherRingController {
  class In(implicit val param: Param) extends GatherBundle {
    val unitDone = UInt(NumUnits.W)
  }

  class Out(implicit val param: Param) extends GatherBundle {
    val wbSelOH = Vec(NumPipeOut, UInt(NumUnits.W))
  }

  def main(args: Array[String]): Unit = {

  }
}
