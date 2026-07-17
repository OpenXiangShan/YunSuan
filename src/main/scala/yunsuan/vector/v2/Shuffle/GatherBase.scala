package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.vector.Common.caseToUIntUtil
import yunsuan.vector.v2.Shuffle.GatherCommon._

class GatherBase(implicit val param: GatherParam) extends GatherModule {

  override def desiredName: String = super.desiredName + s"_dlen${DLEN}b_table${NumUnits}"

  val LookUpIndexWidth = log2Ceil(NumElem)

  val in = IO(Input(new GatherBase.In))

  val out = IO(Output(new GatherBase.Out))

  val found = Wire(Vec(NumElem, Bool()))

  val notfound = Wire(Vec(NumElem, Bool()))

  val inactive = Wire(Vec(NumElem, Bool()))

  val indices = VecInit(
    in.index
      .map {
        case idx => idx.take(LookUpIndexWidth)
      }
  )

  for (i <- 0 until NumElem) {
    // fill res
    found(i) := in.indexValid(i) && in.index(i).drop(LookUpIndexWidth) === in.tableIdx
    // fill idx
    notfound(i) := in.indexValid(i) && in.index(i).drop(LookUpIndexWidth) =/= in.tableIdx
    inactive(i) := !in.indexValid(i)

    out.resultValid(i) := found(i)
    out.result(i)      := in.table(indices(i))
  }

  out.notfound := notfound
  out.inactive := inactive
}

class GatherIndexBundle(NumElem: Int, IndexWidth: Int) extends Bundle {

  // if true, this index need be looked up in GatherBase.
  // And the result may be filled with res or idx(if out of range of the table)
  val indexValid = Vec(NumElem, Bool())

  val index = Vec(NumElem, UInt(IndexWidth.W))

  val fillZeros = Vec(NumElem, Bool())

  val fillScala = Vec(NumElem, Bool())

}


object GatherBase {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val tableIdx   = UnitIdx()
    val table      = Vec(NumElem, UInt(MinDataWidth.W))
    val indexValid = Vec(NumElem, Bool())
    val index      = Vec(NumElem, UInt(IndexWidth.W))
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    // found
    val resultValid = Vec(NumElem, Bool())
    val result      = Vec(NumElem, UInt(MinDataWidth.W))
    val inactive    = Vec(NumElem, Bool())
    val notfound    = Vec(NumElem, Bool())
  }

  def main(args: Array[String]): Unit = {
    println("Generating the GatherBase hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )

    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage
        .ChiselGeneratorAnnotation(
          () => new GatherBase()(GatherParam.default)
        ) +: firtoolAnno
    )

    println("done")
  }
}
