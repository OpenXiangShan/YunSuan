package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.v2.Shuffle.GatherRing._
import yunsuan.vector.v2.Shuffle.GatherRingUnit.{Status, Wake}

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
    ig.flush          := in.flush.s1(i)
    ig.in.bits.op     := pi.bits.op
    // A segment zip numbers its uops chunk-major (`uopIdx = chunk * seg + field`), but the zip
    // IndexGen tables only hold the slices of ONE chunk, indexed by the field number
    // `uopIdx % seg` (seg = zipNum + 1). Passing the chunk-inclusive uopIdx through selects an
    // unfilled slice for chunk > 0: every index comes out invalid and the ring emits PREFILL
    // zeros (emu vsseg2e8_v-0, 2026-09-16: chunk-1 store data all zero, FST seg2e8_uopidx.fst).
    // The ring itself keeps the chunk-inclusive uopIdx for block placement; only the IndexGen
    // slice and table tag are reduced to the field number.
    val segmentSliceIdx = PermUnit.segmentFieldIdx(pi.bits.uopIdx, pi.bits.zipNum)
    val isSegPerm = (pi.bits.op.isZip || pi.bits.op.isUnzip) && pi.bits.zipNum =/= 0.U
    ig.in.bits.uopIdx := Mux(isSegPerm, segmentSliceIdx, pi.bits.uopIdx)
    ig.in.bits.sew    := pi.bits.sew
    ig.in.bits.ei     := pi.bits.ei
    ig.in.bits.lmul   := pi.bits.lmul
    ig.in.bits.vl     := pi.bits.vl
    ig.in.bits.vta    := pi.bits.vta
    ig.in.bits.vma    := pi.bits.vma
    ig.in.bits.vs1    := pi.bits.vs1
    ig.in.bits.vs2    := pi.bits.vs2
    ig.in.bits.vs3    := pi.bits.vs3
    ig.in.bits.rs1    := pi.bits.rs1
    ig.in.bits.mask   := pi.bits.mask
    ig.in.bits.zipNum := pi.bits.zipNum
  }

  /*********************** Registered Pipeline Stage *************************/

  // Pipe payload -> registered (aligned with IndexGen latency)
  private val regPlaceIdx  = Wire(Vec(NumPipeIn, PipeInIdx()))
  private val regBlockBase = Wire(Vec(NumPipeIn, UnitIdx()))
  private val regBlockSize = Wire(Vec(NumPipeIn, BlockSize()))
  private val regBlockFieldNum = Wire(Vec(NumPipeIn, BlockFieldNum()))
  private val regVs2       = Wire(Vec(NumPipeIn, UInt(DLEN.W)))
  private val regCompress  = Wire(Vec(NumPipeIn, Bool()))
  private val regTableIdx  = Wire(Vec(NumPipeIn, UnitIdx()))
  private val regMeta      = RegInit(VecInit.fill(NumPipeIn)(0.U.asTypeOf(Valid(metaGen))))

  for (i <- 0 until NumPipeIn) {
    val pi = in.pipes(i)

    regPlaceIdx(i)  := RegEnable(pi.bits.placeIdx, pi.valid)
    regBlockBase(i) := RegEnable(pi.bits.blockBase, pi.valid)
    regBlockSize(i) := RegEnable(pi.bits.blockSize, pi.valid)
    regBlockFieldNum(i) := RegEnable(pi.bits.blockFieldNum, pi.valid)
    regVs2(i)       := RegEnable(pi.bits.vs2, pi.valid)
    regCompress(i)  := RegEnable(pi.bits.op.isCompress, pi.valid)
    regTableIdx(i)  := RegEnable(pi.bits.tableIdx, pi.valid)

    when (pi.valid || regMeta(i).valid) {
      regMeta(i).valid := pi.valid
    }
    when (pi.valid) {
      regMeta(i).bits := pi.bits.meta
    }

    when(pi.valid) {
      assert(pi.bits.placeIdx < pi.bits.blockSize,
        "PermUnit placeIdx must be inside the selected block")
      assert((pi.bits.blockBase & (pi.bits.blockSize - 1.U)) === 0.U,
        "PermUnit blockBase must align to blockSize")
    }
  }

  /*********************** GatherRing s0 Connections *************************/

  for (i <- 0 until NumPipeIn) {
    gatherRing.in.s0pipes(i).valid          := in.pipes(i).valid
    gatherRing.in.s0pipes(i).bits.placeIdx  := in.pipes(i).bits.placeIdx
    gatherRing.in.s0pipes(i).bits.blockBase := in.pipes(i).bits.blockBase
    gatherRing.in.s0pipes(i).bits.blockSize := in.pipes(i).bits.blockSize
    gatherRing.in.s0pipes(i).bits.blockFieldNum := in.pipes(i).bits.blockFieldNum
    // SEW=8 vrgatherei16: route index deterministically by uopIdx%blockSize
    // (blockBase | placeIdx) so gather i and rvs3 i share a unit regardless of
    // the ring's dynamic entryIndex / hasIndex.
    gatherRing.in.s0pipes(i).bits.isEi16E8 :=
      (in.pipes(i).bits.op.isGatherEI16 || in.pipes(i).bits.op.isGatherEI16Rvs3) &&
        in.pipes(i).bits.sew.e8
    // rvs3 (old-data) uop places an index but no table: tell the block controller
    // so it does not assert tablePos.valid for it.
    gatherRing.in.s0pipes(i).bits.isRvs3 := in.pipes(i).bits.op.isGatherEI16Rvs3
    gatherRing.in.s0pipes(i).bits.uopIdx := in.pipes(i).bits.uopIdx
  }

  /*********************** GatherRing s1 Connections *************************/

  for (i <- 0 until NumPipeIn) {

    gatherRing.in.s1pipes(i).valid := indexGens(i).out.valid

    gatherRing.in.s1pipes(i).bits.placeIdx  := regPlaceIdx(i)
    gatherRing.in.s1pipes(i).bits.blockBase := regBlockBase(i)
    gatherRing.in.s1pipes(i).bits.blockSize := regBlockSize(i)
    gatherRing.in.s1pipes(i).bits.blockFieldNum := regBlockFieldNum(i)

    // Merged flags+data passed directly to GatherRing
    gatherRing.in.s1pipes(i).bits.elemFlags := indexGens(i).out.elemFlags
    gatherRing.in.s1pipes(i).bits.elemData  := indexGens(i).out.elemData

    // SEW=8 vrgatherei16 placement kind (gather vs rvs3) for the unit merge
    gatherRing.in.s1pipes(i).bits.isRvs3   := indexGens(i).out.isRvs3
    gatherRing.in.s1pipes(i).bits.waitRvs3 := indexGens(i).out.waitRvs3

    // Table: merged register (compressData or vs2)
    gatherRing.in.s1pipes(i).bits.table := Mux(
      regCompress(i),
      indexGens(i).out.compressData,
      regVs2(i).asTypeOf(Vec(NumElem, Elem()))
    )

    gatherRing.in.s1pipes(i).bits.tableIdx    := regTableIdx(i)
    // SEW=8 vrgatherei16 gather (waitRvs3) has no writeback and carries no meta:
    // zero it so the same-unit merge's meta OR yields the rvs3's (whose pdest is
    // the writeback target). The rvs3 uop carries the real meta.
    gatherRing.in.s1pipes(i).bits.meta := Mux(indexGens(i).out.waitRvs3,
      0.U.asTypeOf(chiselTypeOf(regMeta(i).bits)), regMeta(i).bits)
  }

  /*********************** GatherRing Control Connections ********************/

  gatherRing.in.flush   := in.unitFlush
  gatherRing.in.wbReady := in.wbReady
  gatherRing.in.wakeAck := in.wakeAck

  /************************** Output Connections *****************************/

  out.pipes      := gatherRing.out.pipes
  out.release    := gatherRing.out.release
  out.metas.unit := gatherRing.out.metas
  out.metas.s1   := regMeta
  out.unitStatus := gatherRing.out.unitStatus
  out.wake       := gatherRing.out.wake
  out.debug.foreach { dbg => dbg := gatherRing.out.debug.get }
}

object PermUnit {

  /** Convert a chunk-major segment uop index to its field number.
    *
    * `zipNum` encodes `seg - 1` for seg 2 through 8. List every legal `(zipNum, uopIdx)` key so
    * the generated hardware is a one-hot constant lookup and contains no remainder operator.
    */
  def segmentFieldIdx(uopIdx: UInt, zipNum: UInt): UInt = {
    val uopIdxWidth = uopIdx.getWidth
    val zipNumWidth = zipNum.getWidth
    val key = Cat(zipNum, uopIdx)
    def matchKey(zipNumValue: Int, uopIdxValue: Int): Bool =
      key === Cat(zipNumValue.U(zipNumWidth.W), uopIdxValue.U(uopIdxWidth.W))

    val mapping = for {
      seg         <- 2 to 8
      field       <- 0 until seg
      rawUopIdx   <- field until (1 << uopIdxWidth) by seg
    } yield matchKey(seg - 1, rawUopIdx) -> field.U(uopIdxWidth.W)
    Mux1H(mapping)
  }

  /** PermUnit top-level input bundle. */
  class In[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val flush = new FlushIn
    val pipes   = Vec(NumPipeIn, ValidIO(new PipeIn(metaGen)))
    val wbReady = Vec(NumPipeOut, Bool())
    val wakeAck = Vec(NumPipeOut, Bool())

    def unitFlush: Vec[Bool] = this.flush.unit
  }

  /** PermUnit top-level output bundle. */
  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val pipes      = Vec(NumPipeOut, ValidIO(new GatherRing.PipeOut(metaGen)))
    val metas      = new MetaOut(metaGen)
    val unitStatus = Vec(NumUnits, new Status)
    val release    = UInt(NumUnits.W)
    val wake       = Vec(NumPipeOut, ValidIO(new GatherRingUnit.Wake(metaGen)))
    val debug = Option.when(param.debug)(Vec(NumUnits, new GatherRingUnit.Debug))
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
    /** Real field count of this uop's ring block: segFields for segment
      * zip/unzip (a 3/5/6/7-field block rounds up to 4/8 units with the tail
      * units as passive index holes); == block size for full blocks. */
    val blockFieldNum = BlockFieldNum()

    // -- Operation select (one-hot) --
    val op = new IndexGen.OpSel

    // -- IndexGen: common --
    val uopIdx = UopIdx()
    val sew    = new GatherSewBundle
    val ei     = new GatherSewBundle
    val lmul   = new GatherLmulBundle
    val vl     = UInt(VlWidth.W)
    val vta    = Bool()                 // tail agnostic (reserved)
    val vma    = Bool()                 // mask agnostic
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

    // For a segment zip the table tag must be block-local: GatherBase matches an index to a
    // table by `index.drop(LookUpIndexWidth) === tableIdx`, and the zip IndexGen emits
    // chunk-relative indices (table tags 0..seg-1 within one chunk's block). A chunk-inclusive
    // uopIdx would tag chunk c's tables c*seg+..., which no index ever matches, so the unit's
    // indexDone never resolves (GatherRingUnit wb assertion, chunk-inclusive replay 2026-09-16).
    def tableIdx = Mux(
      (op.isZip || op.isUnzip) && zipNum =/= 0.U,
      segmentFieldIdx(uopIdx, zipNum),
      uopIdx,
    )
  }

  class FlushIn(implicit val param: GatherParam) extends GatherBundle {
    val s1 = Vec(NumPipeIn, Bool())
    val unit = Vec(NumUnits, Bool())
  }

  class MetaOut[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val s1 = Vec(NumPipeIn, ValidIO(metaGen))
    val unit = Vec(NumUnits, ValidIO(metaGen))
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    val targetDir = "build/vector"

    println(s"Generating the $this hardware in $targetDir")

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
        () => new PermUnit(FakeRobPtr())(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
