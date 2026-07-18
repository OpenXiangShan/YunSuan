package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeField, DecodePattern, DecodeTable}
import yunsuan.vector.v2.Shuffle.GatherCommon._

class ZipIndexGenBase(printDetail: Boolean = false)(implicit val param: GatherParam) extends GatherModule {

  override def desiredName: String = super.desiredName + s"_${DLEN}b"

  val in = IO(Input(new ZipIndexGenBase.In))
  val out = IO(Output(new ZipIndexGenBase.Out))

  val SplittedParts = VLEN / DLEN * 8

  val sews = Seq(8, 16, 32, 64)

  val zipns = Seq(2, 3, 4, 5, 6, 7, 8)

}

object ZipIndexGenBase {
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

    println("Generating the ZipIndexGen hardware")

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
          () => new ZipIndexGenDecodeTable()(GatherParam.default)
        ) +: firtoolAnno
    )

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage
        .ChiselGeneratorAnnotation(
          () => new ZipIndexGenHandWrite(printDetail = true)(GatherParam.default)
        ) +: firtoolAnno
    )

    println("done")
  }
}

class ZipIndexGenHandWrite(printDetail: Boolean = false)(implicit param: GatherParam)
    extends ZipIndexGenBase(printDetail) {

  private val vlenb = VLENB

  // indices(zipn)(sew)(uop)(elem)
  val indices: Vec[Vec[Vec[Vec[UInt]]]] =
    WireInit(VecInit.fill(zipns.length, sews.length)(VecInit.fill(SplittedParts, NumElem)(0.U(IndexWidth.W))))

  val indexValids: Vec[Vec[Vec[Vec[Bool]]]] =
    WireInit(VecInit.fill(zipns.length, sews.length)(VecInit.fill(SplittedParts, NumElem)(false.B)))

  for {
    (zipn, zipnIdx) <- zipns.zipWithIndex
    (sew, sewIdx)   <- sews.zipWithIndex
  } yield {
    val loopLength = zipn * sew / 8
    val sewb       = sew / 8
    if (printDetail) println(s"zipn = $zipn, sew = $sew")
    for (i <- 0 until vlenb * zipn) {
      val r     = i                               % loopLength
      val q     = i / loopLength
      val index = q * sewb + r / sewb * vlenb + i % sewb
      indices(zipnIdx)(sewIdx)(i / NumElem)(i % NumElem)     := index.U
      indexValids(zipnIdx)(sewIdx)(i / NumElem)(i % NumElem) := true.B
      if (printDetail) print(f"${index}%2x ")
    }
    if (printDetail) println()
  }

  private val sewOH = in.sew.toOH

  val selectedIndexValid = Mux1H(sewOH, indexValids(in.zipNum - 1.U))(in.uopIdx)

  val selectedIndex = Mux1H(sewOH, indices(in.zipNum - 1.U)).suggestName("index")(in.uopIdx).suggestName("index")

  for (i <- out.index.indices) {
    out.index(i).valid := selectedIndexValid(i)
    out.index(i).bits  := selectedIndex(i)
  }

}

class ZipIndexGenDecodeTable()(implicit param: GatherParam)
    extends ZipIndexGenBase(param.debug) {

  private val debug = param.debug
  private val vlenb = VLENB

  val patterns: Seq[ZipnSewPattern] = for {
    (zipn, zipnIdx) <- zipns.zipWithIndex
    (sew, sewIdx)   <- sews.zipWithIndex
    uopIdx          <- 0 until SplittedParts
  } yield {
    new ZipnSewPattern(zipn, sew, uopIdx, SplittedParts)
  }

  val fields = (0 until NumElem).map(
    elemIdx =>
      new ZipIndexField(
        elemIdx = elemIdx,
        indexWidth = IndexWidth,
        vlenb = vlenb,
        NumElem = NumElem
      )
  )

  var lastUopIdx = 0

  var lastSew = 8

  for (pattern <- patterns) {
    val zipn   = pattern.zipn
    val sew    = pattern.sew
    val uopIdx = pattern.uopIdx
    if (sew != lastSew) {
      lastSew = sew
      if (debug) println()
      if (debug) println(s"zipn = $zipn, sew = $sew")
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

  val decodeBundle = decodeTable.decode(in.zipNum ## in.sew.toOH ## in.uopIdx)

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

class ZipnSewPattern(val zipn: Int, val sew: Int, val uopIdx: Int, val uopNum: Int) extends DecodePattern {

  override def bitPat: BitPat = BitPat(
    (((zipn - 1) << (4 + log2Up(uopNum))) | ((sew / 8) << log2Up(uopNum)) | uopIdx).U((3 + 4 + log2Up(uopNum)).W)
  )

}

class ZipIndexField(elemIdx: Int, indexWidth: Int, vlenb: Int, NumElem: Int) extends DecodeField[ZipnSewPattern, UInt] {

  override def name: String = s"index_${elemIdx}"

  override def chiselType: UInt = UInt(indexWidth.W)

  override def genTable(op: ZipnSewPattern): BitPat = {
    val index = this.calculateIndex(op, elemIdx, vlenb, NumElem)
    if (index.nonEmpty) {
      BitPat(index.get.U(indexWidth.W))
    } else {
      dc
    }
  }

  def calculateIndex(op: ZipnSewPattern, elemIdx: Int, vlenb: Int, NumElem: Int): Option[Int] = {
    val zipn       = op.zipn
    val sew        = op.sew
    val uopIdx     = op.uopIdx
    val loopLength = zipn * sew / 8
    val sewb       = sew / 8
    val i          = uopIdx * NumElem + elemIdx
    val r          = i                               % loopLength
    val q          = i / loopLength
    val index      = q * sewb + r / sewb * vlenb + i % sewb
    Option.when(i < zipn * vlenb)(index)
  }

}
