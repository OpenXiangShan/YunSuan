package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.v2.Shuffle.GatherRing._
import yunsuan.vector.v2.Shuffle.GatherRingUnit.Status

/** PermUnit: top-level vector permutation unit integrating IndexGen + GatherRing.
  *
  * Pipeline (1-cycle register stage, VIAlu-style single input pipe):
  * {{{
  *   Cycle 0: in.pipes --+--> GatherRing.in.s0pipes  (block placement)
  *                        +--> IndexGen[i].in         (index computation)
  *                        +--> RegEnable(table, prefill, scalar, meta)
  *       [Register stage]
  *   Cycle 1: registered index + table/fill/meta --> GatherRing.in.s1pipes
  * }}}
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

  private val indexGens = Seq.tabulate(NumPipeIn) { i =>
    Module(new IndexGen(useLZA)).suggestName(s"indexGen_$i")
  }

  private val gatherRing = Module(new GatherRing(metaGen))

  /*********************** IndexGen Connections ******************************/

  for (i <- 0 until NumPipeIn) {
    val ig = indexGens(i)
    val pi = in.pipes(i)

    ig.in.valid       := pi.valid
    ig.in.bits.op     := pi.bits.op
    ig.in.bits.uopIdx := pi.bits.uopIdx
    ig.in.bits.sew    := pi.bits.sew
    ig.in.bits.ei     := pi.bits.ei
    ig.in.bits.lmul   := pi.bits.lmul
    ig.in.bits.vl     := pi.bits.vl
    ig.in.bits.vs1    := pi.bits.vs1
    ig.in.bits.vs2    := pi.bits.vs2
    ig.in.bits.vs3    := pi.bits.vs3
    ig.in.bits.rs1    := pi.bits.rs1
    ig.in.bits.mask   := pi.bits.mask
    ig.in.bits.zipNum := pi.bits.zipNum
  }

  /*********************** Registered Pipeline Stage *************************/

  // IndexGen output (combinational) -> registered
  private val igOutValid   = Wire(Vec(NumPipeIn, Bool()))
  private val igOutIndex   = Wire(Vec(NumPipeIn, new GatherIndexBundle(NumElem, IndexWidth)))
  private val igOutPrefill = Wire(Vec(NumPipeIn, Vec(NumElem, UInt(MinDataWidth.W))))
  private val igOutCompDat = Wire(Vec(NumPipeIn, Vec(NumElem, UInt(MinDataWidth.W))))
  private val igOutCompCnt = Wire(Vec(NumPipeIn, UInt(VlWidth.W)))
  private val igOpReg      = Wire(Vec(NumPipeIn, new IndexGen.OpSel))

  // Pipe payload -> registered (aligned with IndexGen latency)
  private val regValid     = Wire(Vec(NumPipeIn, Bool()))
  private val regPlaceIdx  = Wire(Vec(NumPipeIn, PipeInIdx()))
  private val regBlockBase = Wire(Vec(NumPipeIn, UnitIdx()))
  private val regBlockSize = Wire(Vec(NumPipeIn, BlockSize()))
  private val regVs2       = Wire(Vec(NumPipeIn, Vec(NumElem, Elem())))
  private val regTableIdx  = Wire(Vec(NumPipeIn, UnitIdx()))
  private val regScalar    = Wire(Vec(NumPipeIn, UInt(XLEN.W)))
  private val regMeta      = Wire(Vec(NumPipeIn, metaGen.cloneType))

  for (i <- 0 until NumPipeIn) {
    val ig = indexGens(i)
    val pi = in.pipes(i)

    igOutValid(i)  := RegNext(ig.out.valid, false.B)
    igOutIndex(i)  := RegEnable(ig.out.index, ig.out.valid)
    igOutPrefill(i):= RegEnable(ig.out.prefillData, ig.out.valid)
    igOutCompDat(i):= RegEnable(ig.out.compressData, ig.out.valid)
    igOutCompCnt(i):= RegEnable(ig.out.compressCount1s, ig.out.valid)
    igOpReg(i)     := RegEnable(ig.in.bits.op, ig.in.valid)

    regValid(i)     := RegNext(pi.valid, false.B)
    regPlaceIdx(i)  := RegEnable(pi.bits.placeIdx, pi.valid)
    regBlockBase(i) := RegEnable(pi.bits.blockBase, pi.valid)
    regBlockSize(i) := RegEnable(pi.bits.blockSize, pi.valid)
    regVs2(i)       := RegEnable(pi.bits.vs2.asTypeOf(Vec(NumElem, Elem())), pi.valid)
    regTableIdx(i)  := RegEnable(pi.bits.tableIdx, pi.valid)
    regScalar(i)    := RegEnable(pi.bits.rs1, pi.valid)
    regMeta(i)      := RegEnable(pi.bits.meta, pi.valid)
  }

  /*********************** GatherRing s0 Connections *************************/

  for (i <- 0 until NumPipeIn) {
    gatherRing.in.s0pipes(i).valid          := in.pipes(i).valid
    gatherRing.in.s0pipes(i).bits.placeIdx  := in.pipes(i).bits.placeIdx
    gatherRing.in.s0pipes(i).bits.blockBase := in.pipes(i).bits.blockBase
    gatherRing.in.s0pipes(i).bits.blockSize := in.pipes(i).bits.blockSize
  }

  /*********************** GatherRing s1 Connections *************************/

  for (i <- 0 until NumPipeIn) {
    val isCompressOp = igOpReg(i).isCompress

    gatherRing.in.s1pipes(i).valid := regValid(i)

    gatherRing.in.s1pipes(i).bits.placeIdx  := regPlaceIdx(i)
    gatherRing.in.s1pipes(i).bits.blockBase := regBlockBase(i)
    gatherRing.in.s1pipes(i).bits.blockSize := regBlockSize(i)

    // Index from registered IndexGen output
    gatherRing.in.s1pipes(i).bits.index      := igOutIndex(i).index
    gatherRing.in.s1pipes(i).bits.indexValid := igOutIndex(i).indexValid
    gatherRing.in.s1pipes(i).bits.fillZeros  := igOutIndex(i).fillZeros
    gatherRing.in.s1pipes(i).bits.fillScala  := igOutIndex(i).fillScala

    // Table: for compress, use IndexGen's compressData; otherwise registered vs2
    gatherRing.in.s1pipes(i).bits.table := Mux(isCompressOp, igOutCompDat(i), regVs2(i))

    // Prefill from IndexGen (derived from vs3)
    gatherRing.in.s1pipes(i).bits.prefillData := igOutPrefill(i)
    gatherRing.in.s1pipes(i).bits.scalarData  := regScalar(i)
    gatherRing.in.s1pipes(i).bits.tableIdx    := regTableIdx(i)
    gatherRing.in.s1pipes(i).bits.meta        := regMeta(i)
  }

  /*********************** GatherRing Control Connections ********************/

  gatherRing.in.flush   := in.flush
  gatherRing.in.wbReady := in.wbReady

  /************************** Output Connections *****************************/

  out.pipes      := gatherRing.out.pipes
  out.metas      := gatherRing.out.metas
  out.unitStatus := gatherRing.out.unitStatus
}

object PermUnit {

  /** PermUnit top-level input bundle. */
  class In[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val flush   = Vec(NumUnits, Bool())
    val pipes   = Vec(NumPipeIn, ValidIO(new PipeIn(metaGen)))
    val wbReady = Vec(NumPipeOut, Bool())
  }

  /** PermUnit top-level output bundle. */
  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val pipes      = Vec(NumPipeOut, ValidIO(new GatherRing.PipeOut(metaGen)))
    val metas      = Vec(NumUnits, metaGen)
    val unitStatus = Vec(NumUnits, new Status)
  }

  /** Single pipe input: block placement + IndexGen operands + table/fill/meta.
    *
    * All information arrives in the same cycle (VIAlu style).
    * PermUnit internally registers the data payload to align with IndexGen latency.
    */
  class PipeIn[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    // -- Block placement --
    val placeIdx  = PipeInIdx()
    val blockBase = UnitIdx()

    // -- Operation select (one-hot) --
    val op = new IndexGen.OpSel

    // -- IndexGen: common --
    val uopIdx = UopIdx()
    val sew    = new GatherSewBundle
    val ei     = new GatherSewBundle
    val lmul   = new GatherLmulBundle
    val vl     = UInt(VlWidth.W)
    val vs1    = UInt(DLEN.W)
    val vs2    = UInt(DLEN.W)
    val vs3    = UInt(DLEN.W)
    val rs1    = UInt(XLEN.W)
    val mask   = UInt(NumElem.W)

    // -- IndexGen: Zip / Unzip --
    val zipNum = UInt(3.W)

    // -- Meta --
    val meta = metaGen

    def blockSize: UInt = Cat(lmul.m8, lmul.m4, lmul.m2, lmul.mf8 || lmul.mf4 || lmul.mf2 || lmul.m1)

    def tableIdx = uopIdx
  }
}
