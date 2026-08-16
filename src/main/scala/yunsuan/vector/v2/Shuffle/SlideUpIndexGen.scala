package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherCommon._

/**
 * This module is used to generate the indices for slideup instruction.
 * The unchanged part is handled in this module, but tail part is not handled.
 */
class SlideUpIndexGen(implicit val param: GatherParam) extends GatherModule {

  override def desiredName: String = super.desiredName + s"_${DLEN}b"

  val in = IO(Input(ValidIO(new SlideUpIndexGen.In)))
  val out = IO(Output(new SlideUpIndexGen.Out))

  val sew = in.bits.sew

  val vl = in.bits.vl

  val offset = in.bits.offset

  val offsetOverflow = in.bits.offsetOverflow

  private val e8Vl = Mux1H(
    Seq(
      sew.e8  -> vl,
      sew.e16 -> Cat(vl.tail(1), 0.U(1.W)),
      sew.e32 -> Cat(vl.tail(2), 0.U(2.W)),
      sew.e64 -> Cat(vl.tail(3), 0.U(3.W))
    )
  )

  private val e8Off = Mux1H(
    Seq(
      sew.e8  -> offset,
      sew.e16 -> Cat(offset.tail(1), 0.U(1.W)),
      sew.e32 -> Cat(offset.tail(2), 0.U(2.W)),
      sew.e64 -> Cat(offset.tail(3), 0.U(3.W))
    )
  )

  private val iIsZero = VlCompareBitVecModule(VLEN, DLEN, MinDataWidth)(
    0.U,
    sew,
    in.bits.tableIdx,
    (i, _) => i === 0.U,
    "EqualZero"
  )

  out.slideup.valid := in.valid && !in.bits.isSlide1Up
  // i < min(vl, max(vstart, offset))
  // Since vstart is always 0, we get
  // i < min(vl, offset) <=>
  // i < vl && i < offset
  for (i <- out.slideup.bits.index.indices) {
    val isStay = offsetOverflow || (i.U < e8Vl && i.U < e8Off)
    out.slideup.bits.index(i) :=
      Mux(isStay, i.U, i.U - e8Off)
    // The stay elements (i < offset, or all when offset >= vl) are "unchanged"
    // (old vd), so they must be PREFILL (iv=false -> vs3), not a LOOKUP of
    // vs2[i] (emu vslideup_vi-0 block3: offset=3>=vl=2, byte1 got 0x02=vs2[1]).
    out.slideup.bits.indexValid(i) := !isStay
    out.slideup.bits.fillZeros(i)  := false.B
    out.slideup.bits.fillScala(i)  := false.B
  }
  out.slide1up.valid := in.valid && in.bits.isSlide1Up
  for (i <- out.slide1up.bits.index.indices) {
    out.slide1up.bits.index(i) :=
      Mux1H(
        Seq(
          sew.e8  -> (i - 1).max(0).U,
          sew.e16 -> (i - 2).max(0).U,
          sew.e32 -> (i - 4).max(0).U,
          sew.e64 -> (i - 8).max(0).U
        )
      )
    out.slide1up.bits.indexValid(i) := !iIsZero(i)
    out.slide1up.bits.fillScala(i)  := iIsZero(i)
    out.slide1up.bits.fillZeros(i)  := false.B
  }
}

object SlideUpIndexGen {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val isSlide1Up     = Bool()
    val tableIdx       = UInt(TableIdxWidth.W)
    val sew            = new GatherSewBundle
    val offsetOverflow = Bool()
    val offset         = UInt(IndexWidth.W)
    val vl             = UInt(VlWidth.W)
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    val slideup  = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
    val slide1up = ValidIO(new GatherIndexBundle(NumElem, IndexWidth))
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    val targetDir = "build/vector"

    println(s"Generating the ${this.getClass} hardware in $targetDir")

    val firtoolOpts = Array(
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array(
        "--target",
        "systemverilog",
        "--target-dir",
        targetDir,
      ) ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(
        () => new SlideUpIndexGen()(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
