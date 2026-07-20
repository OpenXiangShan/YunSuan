package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherCommon._

/** Unified IndexGen: aggregates all shuffle index generation sub-modules.
  *
  * Sub-modules:
  *   - GatherIndexGen     (vrgather.vv / vrgatherei16)
  *   - SlideUpIndexGen    (vslideup / vslide1up)
  *   - SlideDownIndexGen  (vslidedown / vslide1down)
  *   - ZipIndexGen        (multi-way interleave)
  *   - UnzipIndexGen      (multi-way deinterleave)
  *   - CompressIndexGen   (vcompress)
  */
class IndexGen(useLZA: Boolean = false)(implicit val param: GatherParam) extends GatherModule {
  override def desiredName: String = super.desiredName + s"_${DLEN}b"

  val in  = IO(Input(ValidIO(new IndexGen.In)))
  val out = IO(Output(new IndexGen.Out))

  /*********************** Module Instantiations *****************************/

  private val gatherMod     = Module(new GatherIndexGen)
  private val slideUpMod    = Module(new SlideUpIndexGen)
  private val slideDownMod  = Module(new SlideDownIndexGen)
  private val zipMod        = Module(new ZipIndexGenHandWrite)
  private val unzipMod      = Module(new UnzipIndexGenDecodeTable)
  private val compressMod   = Module(new CompressIndexGen(useLZA))

  /********************** Internal Derivations *******************************/

  private val op = in.bits.op

  private val isGather    = op.isGather || op.isGatherEI16
  private val isSlideUp   = op.isSlideUp || op.isSlide1Up
  private val isSlideDown = op.isSlideDown || op.isSlide1Down
  private val isZip       = op.isZip
  private val isUnzip     = op.isUnzip
  private val isCompress  = op.isCompress

  // Derived from operands (not external inputs)
  private val tableIdx       = in.bits.uopIdx
  private val offset         = in.bits.rs1
  private val offsetOverflow = in.bits.rs1 >= in.bits.vl
  private val byteMask       = in.bits.vs1 // compress mask

  /******************** Sub-module Input Connections *************************/

  // --- GatherIndexGen ---
  gatherMod.in.valid              := in.valid && isGather
  gatherMod.in.bits.isGatherEI16  := op.isGatherEI16
  gatherMod.in.bits.uopIdx        := in.bits.uopIdx
  gatherMod.in.bits.lmul          := in.bits.lmul
  gatherMod.in.bits.sew           := in.bits.sew
  gatherMod.in.bits.ei            := in.bits.ei
  gatherMod.in.bits.vs1           := in.bits.vs1
  gatherMod.in.bits.mask          := in.bits.mask

  // --- SlideUpIndexGen ---
  slideUpMod.in.valid              := in.valid && isSlideUp
  slideUpMod.in.bits.isSlide1Up    := op.isSlide1Up
  slideUpMod.in.bits.tableIdx      := tableIdx
  slideUpMod.in.bits.sew           := in.bits.sew
  slideUpMod.in.bits.offsetOverflow := offsetOverflow
  slideUpMod.in.bits.offset        := offset
  slideUpMod.in.bits.vl            := in.bits.vl

  // --- SlideDownIndexGen ---
  slideDownMod.in.valid             := in.valid && isSlideDown
  slideDownMod.in.bits.isSlide1Down := op.isSlide1Down
  slideDownMod.in.bits.tableIdx     := tableIdx
  slideDownMod.in.bits.lmul         := in.bits.lmul
  slideDownMod.in.bits.sew          := in.bits.sew
  slideDownMod.in.bits.offsetOverflow := offsetOverflow
  slideDownMod.in.bits.offset        := offset
  slideDownMod.in.bits.vl           := in.bits.vl

  // --- ZipIndexGen ---
  zipMod.in.zipNum := in.bits.zipNum
  zipMod.in.sew    := in.bits.sew
  zipMod.in.uopIdx := in.bits.uopIdx

  // --- UnzipIndexGen ---
  unzipMod.in.zipNum := in.bits.zipNum
  unzipMod.in.sew    := in.bits.sew
  unzipMod.in.uopIdx := in.bits.uopIdx

  // --- CompressIndexGen ---
  compressMod.in.valid    := in.valid && isCompress
  compressMod.in.uopIdx   := in.bits.uopIdx
  compressMod.in.byteMask := byteMask
  compressMod.in.data     := in.bits.vs2.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))
  compressMod.in.vs3      := in.bits.vs3

  /********************* Output: GatherIndexBundle Mux ***********************/

  private val gatherIdx:    GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))
  private val slideUpIdx:   GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))
  private val slideDownIdx: GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))
  private val zipIdx:       GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))
  private val unzipIdx:     GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))
  private val compressIdx:  GatherIndexBundle = Wire(new GatherIndexBundle(NumElem, IndexWidth))

  // GatherIndexGen output
  for (i <- 0 until NumElem) {
    gatherIdx.indexValid(i) := gatherMod.out.gather.bits.indexValid(i)
    gatherIdx.index(i)      := gatherMod.out.gather.bits.index(i)
    gatherIdx.fillZeros(i)  := gatherMod.out.gather.bits.fillZeros(i)
    gatherIdx.fillScala(i)  := gatherMod.out.gather.bits.fillScala(i)
  }

  // SlideUp: select between slideup and slide1up variants
  for (i <- 0 until NumElem) {
    slideUpIdx.indexValid(i) := Mux(op.isSlide1Up,
      slideUpMod.out.slide1up.bits.indexValid(i),
      slideUpMod.out.slideup.bits.indexValid(i))
    slideUpIdx.index(i) := Mux(op.isSlide1Up,
      slideUpMod.out.slide1up.bits.index(i),
      slideUpMod.out.slideup.bits.index(i))
    slideUpIdx.fillZeros(i) := Mux(op.isSlide1Up,
      slideUpMod.out.slide1up.bits.fillZeros(i),
      slideUpMod.out.slideup.bits.fillZeros(i))
    slideUpIdx.fillScala(i) := Mux(op.isSlide1Up,
      slideUpMod.out.slide1up.bits.fillScala(i),
      slideUpMod.out.slideup.bits.fillScala(i))
  }

  // SlideDown: select between slidedown and slide1down variants
  for (i <- 0 until NumElem) {
    slideDownIdx.indexValid(i) := Mux(op.isSlide1Down,
      slideDownMod.out.slide1Down.bits.indexValid(i),
      slideDownMod.out.slideDown.bits.indexValid(i))
    slideDownIdx.index(i) := Mux(op.isSlide1Down,
      slideDownMod.out.slide1Down.bits.index(i),
      slideDownMod.out.slideDown.bits.index(i))
    slideDownIdx.fillZeros(i) := Mux(op.isSlide1Down,
      slideDownMod.out.slide1Down.bits.fillZeros(i),
      slideDownMod.out.slideDown.bits.fillZeros(i))
    slideDownIdx.fillScala(i) := Mux(op.isSlide1Down,
      slideDownMod.out.slide1Down.bits.fillScala(i),
      slideDownMod.out.slideDown.bits.fillScala(i))
  }

  // Zip: convert Vec[ValidIO[UInt]] to GatherIndexBundle
  for (i <- 0 until NumElem) {
    zipIdx.indexValid(i) := zipMod.out.index(i).valid
    zipIdx.index(i)      := zipMod.out.index(i).bits
    zipIdx.fillZeros(i)  := false.B
    zipIdx.fillScala(i)  := false.B
  }

  // Unzip: compute valid from element range (DecodeTable hardcodes valid=false)
  private val unzipValidWidth = log2Ceil(VLEN / DLEN * 8 * NumElem + 1)
  for (i <- 0 until NumElem) {
    val globalIdx = (in.bits.uopIdx * NumElem.U)(unzipValidWidth - 1, 0) + i.U
    val maxIdx    = ((in.bits.zipNum +& 1.U) * VLENB.U)(unzipValidWidth - 1, 0)
    unzipIdx.indexValid(i) := globalIdx < maxIdx
    unzipIdx.index(i)      := unzipMod.out.index(i).bits
    unzipIdx.fillZeros(i)  := false.B
    unzipIdx.fillScala(i)  := false.B
  }

  // Compress: from CompressIndexGen output
  for (i <- 0 until NumElem) {
    compressIdx.indexValid(i) := compressMod.out.index.indexValid(i)
    compressIdx.index(i)      := compressMod.out.index.index(i)
    compressIdx.fillZeros(i)  := compressMod.out.index.fillZeros(i)
    compressIdx.fillScala(i)  := compressMod.out.index.fillScala(i)
  }

  // -- Final output mux --
  for (i <- 0 until NumElem) {
    out.index.indexValid(i) := Mux1H(Seq(
      isGather    -> gatherIdx.indexValid(i),
      isSlideUp   -> slideUpIdx.indexValid(i),
      isSlideDown -> slideDownIdx.indexValid(i),
      isZip       -> zipIdx.indexValid(i),
      isUnzip     -> unzipIdx.indexValid(i),
      isCompress  -> compressIdx.indexValid(i),
    ))
    out.index.index(i) := Mux1H(Seq(
      isGather    -> gatherIdx.index(i),
      isSlideUp   -> slideUpIdx.index(i),
      isSlideDown -> slideDownIdx.index(i),
      isZip       -> zipIdx.index(i),
      isUnzip     -> unzipIdx.index(i),
      isCompress  -> compressIdx.index(i),
    ))
    out.index.fillZeros(i) := Mux1H(Seq(
      isGather    -> gatherIdx.fillZeros(i),
      isSlideUp   -> slideUpIdx.fillZeros(i),
      isSlideDown -> slideDownIdx.fillZeros(i),
      isZip       -> zipIdx.fillZeros(i),
      isUnzip     -> unzipIdx.fillZeros(i),
      isCompress  -> compressIdx.fillZeros(i),
    ))
    out.index.fillScala(i) := Mux1H(Seq(
      isGather    -> gatherIdx.fillScala(i),
      isSlideUp   -> slideUpIdx.fillScala(i),
      isSlideDown -> slideDownIdx.fillScala(i),
      isZip       -> zipIdx.fillScala(i),
      isUnzip     -> unzipIdx.fillScala(i),
      isCompress  -> compressIdx.fillScala(i),
    ))
  }

  /******************* Compress-specific Outputs *****************************/

  out.compressData     := compressMod.out.data
  out.compressCount1s  := compressMod.out.count1s

  /*********************** Prefill Data from vs3 *****************************/

  // Inactive/tail elements get corresponding byte from vs3 (old vd)
  for (i <- 0 until NumElem) {
    out.prefillData(i) := in.bits.vs3.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))(i)
  }

  /**************************** Valid Signal *********************************/

  out.valid := Mux1H(Seq(
    isGather    -> (in.valid && isGather),
    isSlideUp   -> (in.valid && isSlideUp),
    isSlideDown -> (in.valid && isSlideDown),
    isZip       -> (in.valid && isZip),
    isUnzip     -> (in.valid && isUnzip),
    isCompress  -> compressMod.out.valid,
  ))
}

object IndexGen {

  /** One-hot operation select flags. */
  class OpSel extends Bundle {
    val isGather     = Bool()  // vrgather.vv
    val isGatherEI16 = Bool()  // vrgatherei16
    val isSlideUp    = Bool()  // vslideup
    val isSlide1Up   = Bool()  // vslide1up
    val isSlideDown  = Bool()  // vslidedown
    val isSlide1Down = Bool()  // vslide1down
    val isZip        = Bool()  // multi-way interleave
    val isUnzip      = Bool()  // multi-way deinterleave
    val isCompress   = Bool()  // vcompress
  }

  /** Unified input bundle for IndexGen.
    * Only includes signals not derivable from other inputs.
    */
  class In(implicit val param: GatherParam) extends GatherBundle {
    val op = new OpSel

    // Configuration
    val uopIdx = UInt(log2Ceil(VLEN / DLEN * 8).W)
    val sew    = new GatherSewBundle
    val ei     = new GatherSewBundle
    val lmul   = new GatherLmulBundle
    val vl     = UInt(VlWidth.W)

    // Register operands
    val vs1 = UInt(DLEN.W)    // gather indices / compress mask
    val vs2 = UInt(DLEN.W)    // source data table
    val vs3 = UInt(DLEN.W)    // old vd -> prefillData source
    val rs1 = UInt(XLEN.W)    // slide offset / slide1 scalar fill

    // Control
    val mask   = UInt(VLENB.W)           // element mask (non-compress)
    val zipNum = UInt(3.W)               // zip/unzip factor - 1
  }

  /** Unified output bundle for IndexGen. */
  class Out(implicit val param: GatherParam) extends GatherBundle {
    val valid = Bool()

    /** GatherIndexBundle: indexValid / index / fillZeros / fillScala */
    val index = new GatherIndexBundle(NumElem, IndexWidth)

    /** Prefill data from vs3 for inactive/tail elements */
    val prefillData = Vec(NumElem, UInt(MinDataWidth.W))

    // Compress-specific
    val compressData    = Vec(NumElem, UInt(MinDataWidth.W))  // compressed vs2 (becomes table)
    val compressCount1s = UInt(VlWidth.W)                     // total popcount
  }
}

/** Opcode enumeration for IndexGen operation selection.
  * Kept for internal use by legacy sub-module decode.
  */
object IndexGenOpcode {
  def apply(): UInt = UInt(3.W)

  def gather:    UInt = 0.U(3.W)
  def slideUp:   UInt = 1.U(3.W)
  def slideDown: UInt = 2.U(3.W)
  def zip:       UInt = 3.U(3.W)
  def unzip:     UInt = 4.U(3.W)
  def compress:  UInt = 5.U(3.W)
}
