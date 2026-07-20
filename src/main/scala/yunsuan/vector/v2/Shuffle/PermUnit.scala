package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.v2.Shuffle.GatherRing._
import yunsuan.vector.v2.Shuffle.GatherRingUnit.Status

/** PermUnit: top-level vector permutation unit integrating IndexGen + GatherRing.
  *
  * Pipeline (1-cycle uniform register stage between s0 and s1):
  * {{{
  *   s0: in.s0pipes ---> GatherRing.in.s0pipes (block placement)
  *                    --> IndexGen[i].in          (index computation, one per pipe)
  *       [Register stage: IndexGen output registered per pipe]
  *   s1: in.s1pipes + registered index -> GatherRing.in.s1pipes
  * }}}
  *
  * One IndexGen instance per s0 pipe so that all NumPipeIn pipes can be
  * valid simultaneously.
  *
  * @tparam T meta type (pdest, robIdx, etc.), propagated to GatherRing
  * @param useLZA use LZA variant for CompressDataPreProcess (default: LOA)
  */
class PermUnit[T <: Data](
  metaGen: => T,
  useLZA: Boolean = false
)(
  implicit val param: GatherParam
) extends GatherModule {
  override def desiredName: String = super.desiredName + s"_vlen${VLEN}b_dlen${DLEN}b"

  val in  = IO(Input(new PermUnit.In(metaGen)))
  val out = IO(Output(new PermUnit.Out(metaGen)))

  /*********************** Module Instantiations *****************************/

  /** One IndexGen per s0 pipe -- supports simultaneous valid on all pipes. */
  private val indexGens = Seq.tabulate(NumPipeIn) { i =>
    Module(new IndexGen(useLZA)).suggestName(s"indexGen_$i")
  }

  private val gatherRing = Module(new GatherRing(metaGen))

  /*********************** IndexGen Connections ******************************/

  for (i <- 0 until NumPipeIn) {
    val ig = indexGens(i)
    val s0 = in.s0pipes(i)

    ig.in.valid := s0.valid
    ig.in.bits.opcode        := s0.bits.opcode
    ig.in.bits.uopIdx        := s0.bits.uopIdx
    ig.in.bits.sew           := s0.bits.sew
    ig.in.bits.ei            := s0.bits.ei
    ig.in.bits.lmul          := s0.bits.lmul
    ig.in.bits.vl            := s0.bits.vl
    ig.in.bits.isGatherEI16  := s0.bits.isGatherEI16
    ig.in.bits.vs1           := s0.bits.vs1
    ig.in.bits.mask          := s0.bits.mask
    ig.in.bits.tableIdx      := s0.bits.tableIdx
    ig.in.bits.offset        := s0.bits.offset
    ig.in.bits.offsetOverflow := s0.bits.offsetOverflow
    ig.in.bits.isSlide1Up    := s0.bits.isSlide1Up
    ig.in.bits.isSlide1Down  := s0.bits.isSlide1Down
    ig.in.bits.zipNum        := s0.bits.zipNum
    ig.in.bits.byteMask      := s0.bits.byteMask
    ig.in.bits.compressData  := s0.bits.compressData
    ig.in.bits.vs3           := s0.bits.vs3
  }

  /*********************** Registered IndexGen Outputs ***********************/

  // Per-pipe registered IndexGen outputs
  private val igOutValid    = Wire(Vec(NumPipeIn, Bool()))
  private val igOutIndex    = Wire(Vec(NumPipeIn, new GatherIndexBundle(NumElem, IndexWidth)))
  private val igOutCompData = Wire(Vec(NumPipeIn, Vec(NumElem, UInt(MinDataWidth.W))))
  private val igOutCompVs3  = Wire(Vec(NumPipeIn, UInt(DLEN.W)))
  private val igOutCompCnt  = Wire(Vec(NumPipeIn, UInt(VlWidth.W)))
  private val igOpcodeReg   = Wire(Vec(NumPipeIn, IndexGenOpcode()))

  for (i <- 0 until NumPipeIn) {
    val ig = indexGens(i)
    igOutValid(i)    := RegNext(ig.out.valid, false.B)
    igOutIndex(i)    := RegEnable(ig.out.index, ig.out.valid)
    igOutCompData(i) := RegEnable(ig.out.compressData, ig.out.valid)
    igOutCompVs3(i)  := RegEnable(ig.out.compressVs3, ig.out.valid)
    igOutCompCnt(i)  := RegEnable(ig.out.compressCount1s, ig.out.valid)
    igOpcodeReg(i)   := RegEnable(ig.in.bits.opcode, ig.in.valid)
  }

  /*********************** GatherRing s0 Connections *************************/

  for (i <- 0 until NumPipeIn) {
    gatherRing.in.s0pipes(i).valid          := in.s0pipes(i).valid
    gatherRing.in.s0pipes(i).bits.placeIdx  := in.s0pipes(i).bits.placeIdx
    gatherRing.in.s0pipes(i).bits.blockBase := in.s0pipes(i).bits.blockBase
    gatherRing.in.s0pipes(i).bits.blockSize := in.s0pipes(i).bits.blockSize
  }

  /*********************** GatherRing s1 Connections *************************/

  for (i <- 0 until NumPipeIn) {
    val s1 = in.s1pipes(i)
    val isCompressOp = igOpcodeReg(i) === IndexGenOpcode.compress

    gatherRing.in.s1pipes(i).valid := s1.valid

    // Block placement
    gatherRing.in.s1pipes(i).bits.placeIdx  := s1.bits.placeIdx
    gatherRing.in.s1pipes(i).bits.blockBase := s1.bits.blockBase
    gatherRing.in.s1pipes(i).bits.blockSize := s1.bits.blockSize

    // Index from registered IndexGen output (per pipe)
    gatherRing.in.s1pipes(i).bits.index      := igOutIndex(i).index
    gatherRing.in.s1pipes(i).bits.indexValid := igOutIndex(i).indexValid
    gatherRing.in.s1pipes(i).bits.fillZeros  := igOutIndex(i).fillZeros
    gatherRing.in.s1pipes(i).bits.fillScala  := igOutIndex(i).fillScala

    // Table: for compress, use CompressDataPreProcess result; otherwise from s1 input
    gatherRing.in.s1pipes(i).bits.table := Mux(
      isCompressOp,
      igOutCompData(i),
      s1.bits.table
    )

    // Fill data from s1 input
    gatherRing.in.s1pipes(i).bits.prefillData := s1.bits.prefillData
    gatherRing.in.s1pipes(i).bits.scalarData  := s1.bits.scalarData
    gatherRing.in.s1pipes(i).bits.tableIdx    := s1.bits.tableIdx

    // Meta from s1 input
    gatherRing.in.s1pipes(i).bits.meta := s1.bits.meta
  }

  /*********************** GatherRing Control Connections ********************/

  gatherRing.in.flush   := in.flush
  gatherRing.in.wbReady := in.wbReady

  /************************** Output Connections *****************************/

  out.pipes      := gatherRing.out.pipes
  out.metas      := gatherRing.out.metas
  out.unitStatus := gatherRing.out.unitStatus

  /*********************** Compress-specific Outputs *************************/

  // Expose compress vs3 and count1s from the pipe that has a compress op.
  // In practice only one compress uop is in-flight at a time per pipe group.
  private val compressPipeOH = VecInit(igOpcodeReg.map(_ === IndexGenOpcode.compress)).asUInt
  out.compressVs3    := Mux1H(compressPipeOH, igOutCompVs3)
  out.compressCount1s := Mux1H(compressPipeOH, igOutCompCnt)
}

object PermUnit {

  /** PermUnit top-level input bundle. */
  class In[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val flush   = Vec(NumUnits, Bool())
    val s0pipes = Vec(NumPipeIn, ValidIO(new PipeInS0))
    val s1pipes = Vec(NumPipeIn, ValidIO(new PipeInS1(metaGen)))
    val wbReady = Vec(NumPipeOut, Bool())
  }

  /** PermUnit top-level output bundle. */
  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val pipes      = Vec(NumPipeOut, ValidIO(new GatherRing.PipeOut(metaGen)))
    val metas      = Vec(NumUnits, metaGen)
    val unitStatus = Vec(NumUnits, new Status)

    // Compress-specific passthrough
    val compressVs3    = UInt(DLEN.W)
    val compressCount1s = UInt(VlWidth.W)
  }

  /** s0 pipe: block placement info + IndexGen operands.
    *
    * This bundle is a superset of GatherRing.PipeInS0, adding all fields
    * needed by IndexGen to compute the gather indices.
    */
  class PipeInS0(implicit val param: GatherParam) extends GatherBundle {
    // -- Block placement (for GatherRing s0) --
    val placeIdx  = PipeInIdx()
    val blockBase = UnitIdx()
    val blockSize = BlockSize()

    // -- IndexGen: operation select --
    val opcode = IndexGenOpcode()

    // -- IndexGen: common --
    val uopIdx = UInt(log2Ceil(VLEN / DLEN * 8).W)
    val sew    = new GatherSewBundle
    val ei     = new GatherSewBundle
    val lmul   = new GatherLmulBundle
    val vl     = UInt(VlWidth.W)

    // -- IndexGen: GatherIndexGen --
    val isGatherEI16 = Bool()
    val vs1          = UInt(DLEN.W)
    val mask         = UInt(NumElem.W)

    // -- IndexGen: SlideUp / SlideDown --
    val tableIdx       = UnitIdx()
    val offset         = UInt(IndexWidth.W)
    val offsetOverflow = Bool()
    val isSlide1Up     = Bool()
    val isSlide1Down   = Bool()

    // -- IndexGen: Zip / Unzip --
    val zipNum = UInt(3.W)

    // -- IndexGen: Compress --
    val byteMask     = UInt(VLEN.W)
    val compressData = Vec(NumElem, UInt(MinDataWidth.W))
    val vs3          = UInt(DLEN.W)
  }

  /** s1 pipe: table data + fill data + meta (payload for GatherRing s1).
    *
    * Mirrors GatherRing.PipeInS1 minus the index fields (which come from
    * the registered IndexGen output instead).
    */
  class PipeInS1[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    // -- Block placement --
    val placeIdx  = PipeInIdx()
    val blockBase = UnitIdx()
    val blockSize = BlockSize()

    // -- Table data (for GatherRingUnit lookup) --
    val table    = Vec(NumElem, Elem())
    val tableIdx = UnitIdx()

    // -- Fill data --
    /** Inactive/tail element data */
    val prefillData = Vec(NumElem, Elem())
    /** Scalar register data (x[rs1] or f[fs1]) */
    val scalarData = UInt(XLEN.W)

    // -- Meta --
    val meta = metaGen
  }
}
