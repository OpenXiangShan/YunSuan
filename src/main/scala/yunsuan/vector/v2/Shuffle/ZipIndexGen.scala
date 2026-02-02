package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{DecodeField, DecodePattern, DecodeTable}

class ZipIndexGenBase(
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
  val zipns = Seq(2, 3, 4, 5, 6, 7, 8)

}

class ZipIndexGenHandWrite(
  vlen: Int,
  dlen: Int,
  printDetail: Boolean = false
) extends ZipIndexGenBase(
  vlen, dlen, printDetail
) {
  override def desiredName: String = super.desiredName + s"_${dlen}b"

  // indices(zipn)(sew)(elemIdx)
  val indices: Vec[Vec[Vec[Vec[UInt]]]] = WireInit(
    VecInit.fill(zipns.length, sews.length)(VecInit.fill(SplittedParts, NumElem)(0.U(IndexWidth.W)))
  )
  val indexValids: Vec[Vec[Vec[Vec[Bool]]]] = WireInit(
    VecInit.fill(zipns.length, sews.length)(VecInit.fill(SplittedParts, NumElem)(false.B))
  )

  for {
    (zipn, zipnIdx) <- zipns.zipWithIndex
    (sew, sewIdx) <- sews.zipWithIndex
  } yield {
    val loopLength = zipn * sew / 8
    val sewb = sew / 8
    if (printDetail) println(s"zipn = $zipn, sew = $sew")
    for (i <- 0 until vlenb * zipn) {
      val r = i % loopLength
      val q = i / loopLength
      val index = q * sewb + r / sewb * vlenb + i % sewb
      indices(zipnIdx)(sewIdx)(i / NumElem)(i % NumElem) := index.U
      indexValids(zipnIdx)(sewIdx)(i / NumElem)(i % NumElem) := true.B
      if (printDetail) print(f"${index}%2x ")
    }
    if (printDetail) println()
  }

  private val sewOH = in.sew.toOH

  val selectedIndexValid = Mux1H(
    sewOH,
    indexValids(in.zipNum)
  )(in.uopIdx)

  val selectedIndex = Mux1H(
    sewOH,
    indices(in.zipNum)
  ).suggestName("index")(in.uopIdx).suggestName("index")

  for (i <- out.index.indices) {
    out.index(i).valid := selectedIndexValid(i)
    out.index(i).bits := selectedIndex(i)
  }

}

class ZipIndexGenDecodeTable(
  vlen: Int,
  dlen: Int,
  printDetail: Boolean = false
) extends ZipIndexGenBase(
  vlen, dlen, printDetail
) {
  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val patterns: Seq[ZipnSewPattern] = for {
    (zipn, zipnIdx) <- zipns.zipWithIndex
    (sew, sewIdx) <- sews.zipWithIndex
    uopIdx <- 0 to 7
  } yield {
    new ZipnSewPattern(zipn, sew, uopIdx, SplittedParts)
  }

  val fields = (0 until NumElem).map(elemIdx => new ZipIndexField(
    elemIdx = elemIdx, indexWidth = IndexWidth, vlenb = vlenb, NumElem = NumElem
  ))

  var lastUopIdx = 0
  var lastSew = 8
  for (pattern <- patterns) {
    val zipn = pattern.zipn
    val sew = pattern.sew
    val uopIdx = pattern.uopIdx
    if (sew != lastSew) {
      lastSew = sew
      if (printDetail) println()
      if (printDetail) println(s"zipn = $zipn, sew = $sew")
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

  val decodeBundle = decodeTable.decode(in.zipNum ## in.sew.toOH)

  val indices: Vec[UInt] = VecInit(fields.map(field => decodeBundle(field)))

  for (i <- out.index.indices) {
    out.index(i).valid := false.B
    out.index(i).bits := indices(i)
  }
}

class ZipnSewPattern(val zipn: Int, val sew: Int, val uopIdx: Int, val uopNum: Int) extends DecodePattern {
  override def bitPat: BitPat = BitPat((
    ((zipn - 1) << (4 + log2Up(uopNum))) |
    ((sew / 8) << log2Up(uopNum)) |
    uopIdx
  ).U((3 + 4 + log2Up(uopNum)).W))
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
    val zipn = op.zipn
    val sew = op.sew
    val uopIdx = op.uopIdx
    val loopLength = zipn * sew / 8
    val sewb = sew / 8
    val i = uopIdx * NumElem + elemIdx
    val r = i % loopLength
    val q = i / loopLength
    val index = q * sewb + r / sewb * vlenb + i % sewb
    Option.when(i < zipn * vlenb)(index)
  }
}

object ZipIndexGenMain extends App {
  println("Generating the ZipIndexGen hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new ZipIndexGenDecodeTable(vlen = 128, dlen = 128, printDetail = true)) +: firtoolAnno
  )

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new ZipIndexGenHandWrite(vlen = 128, dlen = 128, printDetail = true)) +: firtoolAnno
  )

  println("done")
}