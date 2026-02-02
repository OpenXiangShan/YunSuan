package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage._

/**
 * This module is used to generate the indices for slideup instruction.
 * The unchanged part is handled in this module, but tail part is not handled.
 */
class SlideUpIndexGen(val vlen: Int, val dlen: Int) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)

  override def desiredName: String = super.desiredName + s"_${dlen}b"

  val in = IO(Input(ValidIO(new Bundle {
    val isSlide1Up = Bool()
    val tableIdx = UInt(TableIdxWidth.W)
    val sew = new GatherSewBundle
    // if slideNum is greater than or equal to dlen
    val offsetOverflow = Bool()
    val offset = UInt(IndexWidth.W)
    val vl = UInt(VlWidth.W)
  })))

  val out = IO(Output(new Bundle {
    val slideup = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
    val slide1up = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
  }))

  val sew = in.bits.sew
  val vl = in.bits.vl
  val offset = in.bits.offset
  val offsetOverflow = in.bits.offsetOverflow

  private val e8Vl = Mux1H(Seq(
    sew.e8  -> vl,
    sew.e16 -> Cat(vl.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(vl.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(vl.tail(3), 0.U(3.W)),
  ))

  private val e8Off = Mux1H(Seq(
    sew.e8  -> offset,
    sew.e16 -> Cat(offset.tail(1), 0.U(1.W)),
    sew.e32 -> Cat(offset.tail(2), 0.U(2.W)),
    sew.e64 -> Cat(offset.tail(3), 0.U(3.W)),
  ))

  private val iIsZero = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    0.U,
    sew,
    in.bits.tableIdx,
    (i, vl) => i === vl,
    "EqualZero",
  )

  out.slideup.valid := in.valid && !in.bits.isSlide1Up
  // i < min(vl, max(vstart, offset))
  // Since vstart is always 0, we get
  // i < min(vl, offset) <=>
  // i < vl && i < offset
  for (i <- out.slideup.bits.index.indices) {
    out.slideup.bits.index(i) := Mux(
      offsetOverflow || i.U < e8Vl && i.U < e8Off,
      i.U,
      i.U - e8Off,
    )
    out.slideup.bits.indexValid(i) := true.B
    out.slideup.bits.fillZeros(i) := false.B
    out.slideup.bits.fillScala(i) := false.B
  }
  out.slideup.bits.indexValid.foreach(_ := true.B)

  out.slide1up.valid := in.valid && in.bits.isSlide1Up
  for (i <- out.slide1up.bits.index.indices) {
    out.slide1up.bits.index(i) := Mux1H(Seq(
      sew.e8  -> (i - 1).max(0).U,
      sew.e16 -> (i - 2).max(0).U,
      sew.e32 -> (i - 4).max(0).U,
      sew.e64 -> (i - 8).max(0).U,
    ))
    out.slide1up.bits.indexValid(i) := !iIsZero(i)
    out.slide1up.bits.fillScala(i) := iIsZero(i)
    out.slide1up.bits.fillZeros(i) := false.B
  }
}

object SlideUpIndexGenMain extends App {
  println("Generating the SlideIndexGen hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new SlideUpIndexGen(dlen = 128, vlen = 128)) +: firtoolAnno
  )

  println("done")
}