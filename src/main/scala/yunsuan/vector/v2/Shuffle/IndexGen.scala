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

  private val opReg    = RegEnable(op, in.valid)
  private val validReg = RegNext(in.valid, false.B)

  when(in.valid) {
    assert(PopCount(op.asUInt) === 1.U, "IndexGen operation must be one-hot")
  }

  // Derived from operands (not external inputs)
  private val tableIdx       = in.bits.uopIdx
  // Slide offsets are element counts: truncate rs1 to SEW bits. The dispatch
  // may carry the .vi immediate replicated across the whole XLEN scalar
  // (emu vslideup_vi-0, rs1=0x0101_0101_0101_0101), which would otherwise trip
  // offsetOverflow and turn every element into an identity (no slide).
  private val offsetSew = Mux1H(Seq(
    in.bits.sew.e8  -> in.bits.rs1(7, 0),
    in.bits.sew.e16 -> in.bits.rs1(15, 0),
    in.bits.sew.e32 -> in.bits.rs1(31, 0),
    in.bits.sew.e64 -> in.bits.rs1(63, 0),
  ))
  private val offset         = offsetSew
  private val offsetOverflow = offset >= in.bits.vl
  // vcompress mask: only elements within vl can be selected (RISC-V V spec:
  // vcompress packs "the first vl elements of vs2"). Gate byteMask to the low
  // e8vl (= vl in byte-elements) bits; otherwise beyond-vl mask bits inflate
  // popcount and turn tail elements into a bogus LOOKUP (emu vcompress_vm-0).
  private val e8vlComb = Mux1H(Seq(
    in.bits.sew.e8  -> in.bits.vl,
    in.bits.sew.e16 -> Cat(in.bits.vl, 0.U(1.W)),
    in.bits.sew.e32 -> Cat(in.bits.vl, 0.U(2.W)),
    in.bits.sew.e64 -> Cat(in.bits.vl, 0.U(3.W)),
  ))
  private val byteMask = VecInit((0 until VLEN).map { i =>
    // SEW-expand the element-granular vs1: mask bit i selects byte i (e8) or
    // the SEW/8 bytes of element i/sewShift (e16/e32/e64). Without this,
    // CompressIndexGen packs 1 byte per element instead of SEW/8 (Q27).
    val srcBit = Mux1H(Seq(
      in.bits.sew.e8  -> in.bits.vs1(i),
      in.bits.sew.e16 -> in.bits.vs1(i >> 1),
      in.bits.sew.e32 -> in.bits.vs1(i >> 2),
      in.bits.sew.e64 -> in.bits.vs1(i >> 3),
    ))
    Mux(i.U < e8vlComb, srcBit, false.B)
  }).asUInt

  /******************** Sub-module Input Connections *************************/

  // --- GatherIndexGen ---
  gatherMod.in.valid              := in.valid && isGather
  gatherMod.in.bits.isGatherEI16  := op.isGatherEI16
  gatherMod.in.bits.uopIdx        := in.bits.uopIdx
  gatherMod.in.bits.lmul          := in.bits.lmul
  gatherMod.in.bits.sew           := in.bits.sew
  gatherMod.in.bits.ei            := in.bits.ei
  gatherMod.in.bits.vs1           := in.bits.vs1

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

  private val gatherIdxReg    = RegEnable(gatherIdx, in.valid && isGather)
  private val slideUpIdxReg   = RegEnable(slideUpIdx, in.valid && isSlideUp)
  private val slideDownIdxReg = RegEnable(slideDownIdx, in.valid && isSlideDown)
  private val zipIdxReg       = RegEnable(zipIdx, in.valid && isZip)
  private val unzipIdxReg     = RegEnable(unzipIdx, in.valid && isUnzip)

  private val sewReg    = RegEnable(in.bits.sew, in.valid)
  private val uopIdxReg = RegEnable(in.bits.uopIdx, in.valid)
  private val vlReg     = RegEnable(in.bits.vl, in.valid)
  private val vtaReg    = RegEnable(in.bits.vta, in.valid)
  private val vmaReg    = RegEnable(in.bits.vma, in.valid)
  private val vs3Reg    = RegEnable(in.bits.vs3, in.valid)
  private val rs1Reg    = RegEnable(in.bits.rs1, in.valid)
  private val maskReg   = RegEnable(in.bits.mask, in.valid)

  private val outIsGather    = opReg.isGather || opReg.isGatherEI16
  private val outIsSlideUp   = opReg.isSlideUp || opReg.isSlide1Up
  private val outIsSlideDown = opReg.isSlideDown || opReg.isSlide1Down
  private val outIsZip       = opReg.isZip
  private val outIsUnzip     = opReg.isUnzip
  private val outIsCompress  = opReg.isCompress

  // -- Final output: encode to merged flags+data --
  // e8vl = vl in 8b elements = vl << sewShift(sew)
  private val e8vl = Mux1H(Seq(
    sewReg.e8  -> vlReg,
    sewReg.e16 -> Cat(vlReg, 0.U(1.W)),
    sewReg.e32 -> Cat(vlReg, 0.U(2.W)),
    sewReg.e64 -> Cat(vlReg, 0.U(3.W)),
  ))
  private val vs3Bytes = vs3Reg.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))
  private val rs1Bytes = rs1Reg.asTypeOf(Vec(XLEN / MinDataWidth, UInt(MinDataWidth.W)))
  private val dlenBits = log2Ceil(DLEN / MinDataWidth) // bits per uop-internal idx

  // Pre-allocated Vec for better Verilog naming
  private val isTail      = Wire(Vec(NumElem, Bool()))
  private val prefillByte = Wire(Vec(NumElem, UInt(MinDataWidth.W)))

  for (i <- 0 until NumElem) {
    val iv = Wire(Bool()); val idx = Wire(Index()); val fz = Wire(Bool()); val fs = Wire(Bool())
    iv := Mux1H(Seq(
      outIsGather    -> gatherIdxReg.indexValid(i),
      outIsSlideUp   -> slideUpIdxReg.indexValid(i),
      outIsSlideDown -> slideDownIdxReg.indexValid(i),
      outIsZip       -> zipIdxReg.indexValid(i),
      outIsUnzip     -> unzipIdxReg.indexValid(i),
      outIsCompress  -> compressIdx.indexValid(i),
    ))
    idx := Mux1H(Seq(
      outIsGather    -> gatherIdxReg.index(i),
      outIsSlideUp   -> slideUpIdxReg.index(i),
      outIsSlideDown -> slideDownIdxReg.index(i),
      outIsZip       -> zipIdxReg.index(i),
      outIsUnzip     -> unzipIdxReg.index(i),
      outIsCompress  -> compressIdx.index(i),
    ))
    fz := Mux1H(Seq(
      outIsGather    -> gatherIdxReg.fillZeros(i),
      outIsSlideUp   -> slideUpIdxReg.fillZeros(i),
      outIsSlideDown -> slideDownIdxReg.fillZeros(i),
      outIsZip       -> zipIdxReg.fillZeros(i),
      outIsUnzip     -> unzipIdxReg.fillZeros(i),
      outIsCompress  -> compressIdx.fillZeros(i),
    ))
    fs := Mux1H(Seq(
      outIsGather    -> gatherIdxReg.fillScala(i),
      outIsSlideUp   -> slideUpIdxReg.fillScala(i),
      outIsSlideDown -> slideDownIdxReg.fillScala(i),
      outIsZip       -> zipIdxReg.fillScala(i),
      outIsUnzip     -> unzipIdxReg.fillScala(i),
      outIsCompress  -> compressIdx.fillScala(i),
    ))

    // Mask-inactive elements (mask bit = 0) are PREFILL for all ops except
    // compress, even when the index is out of range (fz): vma undisturbed must
    // win over fill-zero (see Q12).
    val maskInactive = !outIsCompress && !maskReg(i)
    val isPrefill = maskInactive || (!fz && !fs && !iv)

    // PREFILL data: tail elements follow vta, mask-inactive follow vma;
    // compress has no mask, all PREFILL are tail -> vta
    isTail(i) := Cat(uopIdxReg, i.U(dlenBits.W)) >= e8vl
    // vslideup stay (i < offset) elements are "unchanged" (old vd = vs3),
    // independent of vma/vta: ma must not clobber them with 0xFF (Q26).
    val slideStay = opReg.isSlideUp && !iv && !maskInactive && !isTail(i)
    val agnostic = Mux(slideStay, false.B, Mux(outIsCompress || isTail(i), vtaReg, vmaReg))
    prefillByte(i) := Mux(agnostic, Fill(8, 1.B), vs3Bytes(i))

    val effectiveFs = fs && !maskInactive
    out.elemFlags(i) := Mux1H(Seq(
      (fz && !maskInactive) -> ElemFlags.ZERO,
      (effectiveFs || isPrefill) -> ElemFlags.PREFILL,
      (iv && !maskInactive) -> ElemFlags.LOOKUP,
    ))
    out.elemData(i) := Mux1H(Seq(
      (fz && !maskInactive) -> 0.U,
      (effectiveFs || isPrefill) -> Mux(effectiveFs,
        Mux1H(Seq(  // SCALAR: SEW-aware rs1 byte
          sewReg.e8  -> rs1Bytes(0),
          sewReg.e16 -> rs1Bytes(i % 2),
          sewReg.e32 -> rs1Bytes(i % 4),
          sewReg.e64 -> rs1Bytes(i % (XLEN / MinDataWidth)),
        )),
        prefillByte(i)  // PREFILL: vs3 byte or agnostic
      ),
      (iv && !maskInactive) -> idx,
    ))
    // Tail elements (i >= e8vl) are not active gather elements;
    // override to PREFILL regardless of index validity
    when(isTail(i)) {
      out.elemFlags(i) := ElemFlags.PREFILL
      out.elemData(i)  := prefillByte(i)
    }
  }

  /******************* Compress-specific Outputs *****************************/

  out.compressData     := compressMod.out.data
  out.compressCount1s  := compressMod.out.count1s

  /**************************** Valid Signal *********************************/

  out.valid := validReg

  when(validReg && outIsCompress) {
    assert(compressMod.out.valid, "compress payload must align with IndexGen valid")
  }
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
    val vta    = Bool()                 // tail agnostic (reserved)
    val vma    = Bool()                 // mask agnostic

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

    // Per-element merged: 2b flags + 8b data (index or prefill)
    // flags: 00=PREFILL 01=LOOKUP 10=ZERO 11=SCALAR
    val elemFlags = Vec(NumElem, ElemFlags())
    val elemData  = Vec(NumElem, UInt(8.W))

    // Compress-specific
    val compressData    = Vec(NumElem, UInt(MinDataWidth.W))  // compressed vs2 (becomes table)
    val compressCount1s = UInt(VlWidth.W)                     // total popcount
  }
}

/** Per-element flag encoding: 3 states in 2 bits.
  * ZERO=00, PREFILL=01 (includes old SCALAR), LOOKUP=10.
  * Bit0=1 means PREFILL; Bit1=1 means LOOKUP.
  */
object ElemFlags {
  def apply(): UInt = UInt(2.W)
  def ZERO:    UInt = 0.U(2.W)
  def PREFILL: UInt = 1.U(2.W)
  def LOOKUP:  UInt = 2.U(2.W)

  def isZERO(flags:    UInt): Bool = flags === 0.U(2.W)
  def isPREFILL(flags: UInt): Bool = flags(0)
  def isLOOKUP(flags:  UInt): Bool = flags(1)
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
