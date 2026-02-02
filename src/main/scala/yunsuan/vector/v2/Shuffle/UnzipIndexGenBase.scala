package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeBundle, DecodeField, DecodePattern, DecodeTable}

class UnzipIndexGenBase(
  vlen: Int,
  dlen: Int,
  printDetail: Boolean = false
) extends Module {

  val vlenb = vlen / 8
  val dlenb = dlen / 8
  val MinDataWidth = 8
  val NumElem = dlen / MinDataWidth
  val IndexWidth = log2Ceil(dlen)
  val VlWidth = log2Ceil(vlen) + 1
  val SplittedParts = vlen / dlen * 8

  val in = IO(Input(new Bundle {
    val zipNum = UInt(3.W) // hold 1-7, represent 2-8
    val sew = new GatherSewBundle
    val uopIdx = UInt(log2Up(SplittedParts).W)
  }))

  val out = IO(Output(new Bundle {
    val index = Vec(NumElem, ValidIO(UInt(IndexWidth.W)))
  }))

  val sews = Seq(8, 16, 32, 64)
  val unzipns = Seq(2, 3, 4, 5, 6, 7, 8)

}

class UnzipIndexGenDecodeTable(
  vlen: Int,
  dlen: Int,
  printDetail: Boolean = false
) extends UnzipIndexGenBase(
  vlen, dlen, printDetail
) {
  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val patterns: Seq[UnzipnSewUopIdxPattern] = for {
    (unzipn, _) <- unzipns.zipWithIndex
    (sew, _) <- sews.zipWithIndex
    uopIdx <- 0 to 7
  } yield {
    new UnzipnSewUopIdxPattern(unzipn, sew, uopIdx, SplittedParts)
  }

  val fields = (0 until NumElem).map(elemIdx => new UnzipIndexField(
    elemIdx = elemIdx, indexWidth = IndexWidth, vlenb = vlenb, NumElem = NumElem
  ))

  var lastUopIdx = 0
  var lastSew = 8
  for (pattern <- patterns) {
    val unzipn = pattern.unzipn
    val sew = pattern.sew
    val uopIdx = pattern.uopIdx
    if (sew != lastSew) {
      lastSew = sew
      if (printDetail) println()
      if (printDetail) println(s"unzipn = $unzipn, sew = $sew")
    }
    if (uopIdx != lastUopIdx) {
      lastUopIdx = uopIdx
    }
    for ((field, elemIdx) <- fields.zipWithIndex) {
      val index = field.calculateIndex(pattern, elemIdx, vlenb, NumElem)
      if (printDetail) index.foreach(i => print(f"${i}%2x "))
    }
  }

  val decodeTable = new DecodeTable(patterns, fields)

  val decodeBundle: DecodeBundle = decodeTable.decode(in.zipNum ## in.sew.toOH)

  val indices: Vec[UInt] = VecInit(fields.map(field => decodeBundle(field)))

  for (i <- out.index.indices) {
    out.index(i).valid := false.B
    out.index(i).bits := indices(i)
  }
}

class UnzipnSewUopIdxPattern(val unzipn: Int, val sew: Int, val uopIdx: Int, val uopNum: Int) extends DecodePattern {
  override def bitPat: BitPat = BitPat(
    (
      ((unzipn - 1) << (4 + log2Up(uopNum))) |
      ((sew / 8) << log2Up(uopNum)) |
      uopIdx
    ).U((3 + 4 + log2Up(uopNum)).W)
  )
}

class UnzipIndexField(elemIdx: Int, indexWidth: Int, vlenb: Int, NumElem: Int) extends DecodeField[UnzipnSewUopIdxPattern, UInt] {

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
    val sew = op.sew
    val uopIdx = op.uopIdx
    val sewb = sew / 8
    val times = unzipn * sewb
    val i = uopIdx * NumElem + elemIdx
    val index = i % vlenb / sewb * times + i / vlenb * sewb + i % sewb
    Option.when(i < unzipn * vlenb)(index)
  }
}

object UnzipIndexGenMain extends App {
  println("Generating the UnzipIndexGen hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new UnzipIndexGenDecodeTable(vlen = 128, dlen = 128, printDetail = true)) +: firtoolAnno
  )

  println("done")
}