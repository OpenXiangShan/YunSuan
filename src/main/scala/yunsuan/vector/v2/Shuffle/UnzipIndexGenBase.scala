package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeBundle, DecodeField, DecodePattern, DecodeTable}
import yunsuan.vector.v2.Shuffle.GatherCommon._

class UnzipIndexGenBase(printDetail: Boolean = false)(implicit val param: GatherParam) extends GatherModule {

  override def desiredName: String = super.desiredName + s"_${DLEN}b"

  val in = IO(Input(new UnzipIndexGenBase.In))
  val out = IO(Output(new UnzipIndexGenBase.Out))

  val SplittedParts = VLEN / DLEN * 8

  val sews = Seq(8, 16, 32, 64)

  val unzipns = Seq(2, 3, 4, 5, 6, 7, 8)

}

object UnzipIndexGenBase {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val zipNum = UInt(3.W)
    val sew    = new GatherSewBundle
    val uopIdx = UInt(log2Up(VLEN / DLEN * 8).W)
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    val index = Vec(NumElem, ValidIO(UInt(IndexWidth.W)))
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    println("Generating the UnzipIndexGen hardware")

    val firtoolOpts = Array(
      "--target",
      "systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )

    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage
        .ChiselGeneratorAnnotation(
          () => new UnzipIndexGenDecodeTable()(GatherParam.default)
        ) +: firtoolAnno
    )

    println("done")
  }
}

class UnzipIndexGenDecodeTable()(implicit param: GatherParam)
    extends UnzipIndexGenBase(param.debug) {

  private val debug = param.debug
  private val vlenb = VLENB

  val patterns: Seq[UnzipnSewUopIdxPattern] = for {
    (unzipn, _) <- unzipns.zipWithIndex
    (sew, _)    <- sews.zipWithIndex
    uopIdx      <- 0 until SplittedParts
  } yield {
    new UnzipnSewUopIdxPattern(unzipn, sew, uopIdx, SplittedParts)
  }

  val fields = (0 until NumElem).map(
    elemIdx =>
      new UnzipIndexField(
        elemIdx = elemIdx,
        indexWidth = IndexWidth,
        vlenb = vlenb,
        NumElem = NumElem
      )
  )

  var lastUopIdx = 0

  var lastSew = 8

  for (pattern <- patterns) {
    val unzipn = pattern.unzipn
    val sew    = pattern.sew
    val uopIdx = pattern.uopIdx
    if (sew != lastSew) {
      lastSew = sew
      if (debug) println()
      if (debug) println(s"unzipn = $unzipn, sew = $sew")
    }
    if (uopIdx != lastUopIdx) {
      lastUopIdx = uopIdx
    }
    for ((field, elemIdx) <- fields.zipWithIndex) {
      val index = field.calculateIndex(pattern, elemIdx, vlenb, NumElem)
      if (debug)
        index.foreach(
          i => print(f"${i}%2x ")
        )
    }
  }

  val decodeTable = new DecodeTable(patterns, fields)

  val decodeBundle: DecodeBundle = decodeTable.decode(in.zipNum ## in.sew.toOH ## in.uopIdx)

  val indices: Vec[UInt] = VecInit(
    fields.map(
      field => decodeBundle(field)
    )
  )

  for (i <- out.index.indices) {
    out.index(i).valid := false.B
    out.index(i).bits  := indices(i)
  }
}

class UnzipnSewUopIdxPattern(val unzipn: Int, val sew: Int, val uopIdx: Int, val uopNum: Int) extends DecodePattern {

  override def bitPat: BitPat = BitPat(
    (((unzipn - 1) << (4 + log2Up(uopNum))) | ((sew / 8) << log2Up(uopNum)) | uopIdx).U((3 + 4 + log2Up(uopNum)).W)
  )

}

class UnzipIndexField(elemIdx: Int, indexWidth: Int, vlenb: Int, NumElem: Int)
    extends DecodeField[UnzipnSewUopIdxPattern, UInt] {

  override def name: String = s"index_${elemIdx}"

  override def chiselType: UInt = UInt(indexWidth.W)

  override def genTable(op: UnzipnSewUopIdxPattern): BitPat = {
    val index = this.calculateIndex(op, elemIdx, vlenb, NumElem)
    if (index.nonEmpty) {
      BitPat(index.get.U(indexWidth.W))
    } else {
      dc
    }
  }

  def calculateIndex(op: UnzipnSewUopIdxPattern, elemIdx: Int, vlenb: Int, NumElem: Int): Option[Int] = {
    val unzipn = op.unzipn
    val sew    = op.sew
    val uopIdx = op.uopIdx
    val sewb   = sew / 8
    val times  = unzipn * sewb
    val i      = uopIdx * NumElem + elemIdx
    val index  = i % vlenb / sewb * times + i / vlenb * sewb + i % sewb
    Option.when(i < unzipn * vlenb)(index)
  }

}
