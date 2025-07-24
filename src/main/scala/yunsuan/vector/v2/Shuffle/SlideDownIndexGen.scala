package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption

class SlideDownIndexGen(
  val vlen: Int,
  val dlen: Int,
) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)

  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val in = IO(Input(ValidIO(new Bundle {
    val isSlide1Down = Bool()
    val tableIdx = UInt(TableIdxWidth.W)
    val lmul = new GatherLmulBundle
    val sew = new GatherSewBundle
    // if slideNum is greater than or equal to dlen
    val offsetOverflow = Bool()
    val offset = UInt(IndexWidth.W)
    val vl = UInt(VlWidth.W)
  })))

  val out = IO(Output(new Bundle {
    val slideDown = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
    val slide1Down = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
  }))

  val lmul = in.bits.lmul
  val sew = in.bits.sew
  val offset = in.bits.offset
  val offsetOverflow = in.bits.offsetOverflow
  val tableIdx: UInt = in.bits.tableIdx
  val tableOffset = tableIdx ## 0.U(IndexWidth.W)

  private val e8Off = Mux1H(Seq(
    sew.e8  -> offset,
    sew.e16 -> Cat(offset.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(offset.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(offset.tail(3), 0.U(3.W)),
  )).ensuring(_.getWidth == IndexWidth)

  private val iIsVlM1: Vec[Bool] = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    in.bits.vl,
    in.bits.sew,
    in.bits.tableIdx,
    (i, vl) => Mux(vl === 0.U, false.B, i === (vl - 1.U)),
    "EqualVlM1",
  )

  out.slideDown.valid := in.valid && !in.bits.isSlide1Down
  // 0     <= i + offset < vlmax --> src[i] = vs2[i + offset]
  // vlmax <= i + offset         --> src[i] = 0
  for (i <- out.slideDown.bits.index.indices) {
    val iPlusOffset = ((tableIdx ## i.U(IndexWidthInDLEN.W)) +& e8Off).suggestName("iPlusOffsetSlideDown")
    out.slideDown.bits.index(i) := iPlusOffset.tail(1) // drop carry bit
    out.slideDown.bits.fillZeros(i) := Mux1H(Seq(
      lmul.mf8 -> (iPlusOffset.head(7) =/= 0.U),
      lmul.mf4 -> (iPlusOffset.head(6) =/= 0.U),
      lmul.mf2 -> (iPlusOffset.head(5) =/= 0.U),
      lmul.m1  -> (iPlusOffset.head(4) =/= 0.U),
      lmul.m2  -> (iPlusOffset.head(3) =/= 0.U),
      lmul.m4  -> (iPlusOffset.head(2) =/= 0.U),
      lmul.m8  -> (iPlusOffset.head(1) =/= 0.U), // >= VLENB * 8
    ))
    out.slideDown.bits.indexValid(i) := !out.slideDown.bits.fillZeros(i)
    out.slideDown.bits.fillScala(i) := false.B
  }

  out.slide1Down.valid := in.valid && in.bits.isSlide1Down
  // 0 <= i <  vl - 1  -->  vd[i] = vs2[i + 1]
  //      i == vl - 1  -->  vd[i] = x[rs1]
  for (i <- out.slide1Down.bits.index.indices) {
    val iPlusOffset = Mux1H(Seq(
      sew.e8  -> ((tableIdx ## i.U(IndexWidthInDLEN.W)) +& 1.U),
      sew.e16 -> ((tableIdx ## i.U(IndexWidthInDLEN.W)) +& 2.U),
      sew.e32 -> ((tableIdx ## i.U(IndexWidthInDLEN.W)) +& 4.U),
      sew.e64 -> ((tableIdx ## i.U(IndexWidthInDLEN.W)) +& 8.U),
    )).suggestName("iPlusOffsetSlide1Down")
    out.slide1Down.bits.index(i) := iPlusOffset.tail(1) // drop carry bit
    out.slide1Down.bits.fillZeros(i) := false.B
    out.slide1Down.bits.indexValid(i) := !iIsVlM1(i)
    out.slide1Down.bits.fillScala(i) := iIsVlM1(i)
  }
}

object SlideDownIndexGenMain extends App {
  println("Generating the SlideDownIndexGen hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new SlideDownIndexGen(128, 128)) +: firtoolAnno
  )

  println("done")
}