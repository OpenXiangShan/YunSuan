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
  *
  * All non-compress paths are combinational. CompressIndexGen has an internal
  * 1-cycle pipeline; its output is delayed by 1 cycle relative to input.
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

  /************************** Opcode Decode **********************************/

  private val isGather    = in.bits.opcode === IndexGenOpcode.gather
  private val isSlideUp   = in.bits.opcode === IndexGenOpcode.slideUp
  private val isSlideDown = in.bits.opcode === IndexGenOpcode.slideDown
  private val isZip       = in.bits.opcode === IndexGenOpcode.zip
  private val isUnzip     = in.bits.opcode === IndexGenOpcode.unzip
  private val isCompress  = in.bits.opcode === IndexGenOpcode.compress

  /******************** Sub-module Input Connections *************************/

  // --- GatherIndexGen ---
  gatherMod.in.valid              := in.valid && isGather
  gatherMod.in.bits.isGatherEI16  := in.bits.isGatherEI16
  gatherMod.in.bits.uopIdx        := in.bits.uopIdx
  gatherMod.in.bits.lmul          := in.bits.lmul
  gatherMod.in.bits.sew           := in.bits.sew
  gatherMod.in.bits.ei            := in.bits.ei
  gatherMod.in.bits.vs1           := in.bits.vs1
  gatherMod.in.bits.mask          := in.bits.mask

  // --- SlideUpIndexGen ---
  slideUpMod.in.valid              := in.valid && isSlideUp
  slideUpMod.in.bits.isSlide1Up    := in.bits.isSlide1Up
  slideUpMod.in.bits.tableIdx      := in.bits.tableIdx
  slideUpMod.in.bits.sew           := in.bits.sew
  slideUpMod.in.bits.offsetOverflow := in.bits.offsetOverflow
  slideUpMod.in.bits.offset        := in.bits.offset
  slideUpMod.in.bits.vl            := in.bits.vl

  // --- SlideDownIndexGen ---
  slideDownMod.in.valid              := in.valid && isSlideDown
  slideDownMod.in.bits.isSlide1Down  := in.bits.isSlide1Down
  slideDownMod.in.bits.tableIdx      := in.bits.tableIdx
  slideDownMod.in.bits.lmul          := in.bits.lmul
  slideDownMod.in.bits.sew           := in.bits.sew
  slideDownMod.in.bits.offsetOverflow := in.bits.offsetOverflow
  slideDownMod.in.bits.offset        := in.bits.offset
  slideDownMod.in.bits.vl            := in.bits.vl

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
  compressMod.in.byteMask := in.bits.byteMask
  compressMod.in.data     := in.bits.compressData
  compressMod.in.vs3      := in.bits.vs3

  /********************* Output: GatherIndexBundle Mux ***********************/

  // Per-submodule index bundles (for slideUp/Down, select between main and *1 variant)
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
    slideUpIdx.indexValid(i) := Mux(in.bits.isSlide1Up,
      slideUpMod.out.slide1up.bits.indexValid(i),
      slideUpMod.out.slideup.bits.indexValid(i))
    slideUpIdx.index(i) := Mux(in.bits.isSlide1Up,
      slideUpMod.out.slide1up.bits.index(i),
      slideUpMod.out.slideup.bits.index(i))
    slideUpIdx.fillZeros(i) := Mux(in.bits.isSlide1Up,
      slideUpMod.out.slide1up.bits.fillZeros(i),
      slideUpMod.out.slideup.bits.fillZeros(i))
    slideUpIdx.fillScala(i) := Mux(in.bits.isSlide1Up,
      slideUpMod.out.slide1up.bits.fillScala(i),
      slideUpMod.out.slideup.bits.fillScala(i))
  }

  // SlideDown: select between slidedown and slide1down variants
  for (i <- 0 until NumElem) {
    slideDownIdx.indexValid(i) := Mux(in.bits.isSlide1Down,
      slideDownMod.out.slide1Down.bits.indexValid(i),
      slideDownMod.out.slideDown.bits.indexValid(i))
    slideDownIdx.index(i) := Mux(in.bits.isSlide1Down,
      slideDownMod.out.slide1Down.bits.index(i),
      slideDownMod.out.slideDown.bits.index(i))
    slideDownIdx.fillZeros(i) := Mux(in.bits.isSlide1Down,
      slideDownMod.out.slide1Down.bits.fillZeros(i),
      slideDownMod.out.slideDown.bits.fillZeros(i))
    slideDownIdx.fillScala(i) := Mux(in.bits.isSlide1Down,
      slideDownMod.out.slide1Down.bits.fillScala(i),
      slideDownMod.out.slideDown.bits.fillScala(i))
  }

  // Zip: convert Vec[ValidIO[UInt]] to GatherIndexBundle
  // indexValid=true for active elements (within zip range); fillZeros/fillScala always false
  for (i <- 0 until NumElem) {
    zipIdx.indexValid(i) := zipMod.out.index(i).valid
    zipIdx.index(i)      := zipMod.out.index(i).bits
    zipIdx.fillZeros(i)  := false.B
    zipIdx.fillScala(i)  := false.B
  }

  // Unzip: compute valid from element range (DecodeTable hardcodes valid=false)
  // Valid elements: those where global index < unzipn * vlenb
  // unzipn = zipNum + 1
  // Per-element: valid if (uopIdx * NumElem + i) < (zipNum + 1) * VLENB
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

  /********************* Compress-specific Outputs ***************************/

  // Passthrough from CompressIndexGen; valid only when isCompress
  out.compressData  := compressMod.out.data
  out.compressVs3   := compressMod.out.vs3
  out.compressCount1s := compressMod.out.count1s

  /**************************** Valid Signal *********************************/

  // valid: asserted when this IndexGen output is meaningful
  // For most ops, output is valid in the same cycle as input.
  // CompressIndexGen has 1-cycle internal pipeline, so its output out.valid
  // is delayed. We expose separate per-opcode valid signals so PermUnit
  // can handle the timing difference.
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
  /** Unified input bundle for IndexGen.
    * Not all fields are used by every opcode; unused fields tied to 0.
    */
  class In(implicit val param: GatherParam) extends GatherBundle {
    /** Selects which IndexGen sub-module to activate */
    val opcode = IndexGenOpcode()

    // -- Common --
    val uopIdx = UInt(log2Ceil(VLEN / DLEN * 8).W)
    val sew    = new GatherSewBundle
    val ei     = new GatherSewBundle
    val lmul   = new GatherLmulBundle
    val vl     = UInt(VlWidth.W)

    // -- GatherIndexGen --
    val isGatherEI16 = Bool()
    val vs1          = UInt(DLEN.W)
    /** 1-bit per MinDataWidth element */
    val mask         = UInt(NumElem.W)

    // -- SlideUp / SlideDown --
    val tableIdx       = UnitIdx()
    val offset         = UInt(IndexWidth.W)
    val offsetOverflow = Bool()
    val isSlide1Up     = Bool()
    val isSlide1Down   = Bool()

    // -- Zip / Unzip --
    /** zipn - 1 (1-based); actual zip factor = zipNum + 1 */
    val zipNum = UInt(3.W)

    // -- Compress --
    val byteMask     = UInt(VLEN.W)
    val compressData = Vec(NumElem, UInt(MinDataWidth.W))
    val vs3          = UInt(DLEN.W)
  }

  /** Unified output bundle for IndexGen. */
  class Out(implicit val param: GatherParam) extends GatherBundle {
    val valid = Bool()
    /** GatherIndexBundle: index / indexValid / fillZeros / fillScala */
    val index = new GatherIndexBundle(NumElem, IndexWidth)

    // -- Compress-specific (valid only when opcode = compress) --
    /** Compressed data after CompressDataPreProcess (becomes table for GatherRing) */
    val compressData  = Vec(NumElem, UInt(MinDataWidth.W))
    /** Passthrough vs3 (original vs3 value) */
    val compressVs3   = UInt(DLEN.W)
    /** Total popcount of byteMask (used for VL update) */
    val compressCount1s = UInt(VlWidth.W)
  }
}

/** Opcode enumeration for IndexGen operation selection. */
object IndexGenOpcode {
  def apply(): UInt = UInt(3.W)

  def gather:    UInt = 0.U(3.W)
  def slideUp:   UInt = 1.U(3.W)
  def slideDown: UInt = 2.U(3.W)
  def zip:       UInt = 3.U(3.W)
  def unzip:     UInt = 4.U(3.W)
  def compress:  UInt = 5.U(3.W)
}
