package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherCommon._

class GatherRingUnit[T <: Data](
  val unitIdx: Int,
  metaGen    : => T,
)(
  implicit val param: GatherParam
) extends GatherModule {

  override def desiredName: String =
    super.desiredName +
    s"_vlen${VLEN}b_dlen${DLEN}b_table${NumUnits}"

  val in = IO(Input(new GatherRingUnit.In(metaGen)))
  val out = IO(Output(new GatherRingUnit.Out(metaGen)))

  private val flush = in.flush

  // ── Local Position in Block ───────────────────────────
  private val inBlockBase = in.placeIndex.bits.blockBase
  private val inBlockSize = Mux1H(Seq(
    in.placeIndex.valid -> in.placeIndex.blockSize,
    in.placeTable.valid -> in.placeTable.blockSize,
  ))
  private val inBlockFieldNum = Mux1H(Seq(
    in.placeIndex.valid -> in.placeIndex.bits.blockFieldNum,
    in.placeTable.valid -> in.placeTable.bits.blockFieldNum,
  ))

  // ── Table Input MUX ──────────────────────────────────
  private val doPlaceTable = in.placeTable.valid
  private val doPlaceIndex = in.placeIndex.valid
  private val doFlow       = in.willFlow
  private val doFlowTable  = in.willFlow && in.flow.valid

  assert(!(doPlaceTable && doFlowTable),
    s"GatherRingUnit $unitIdx: placeTable and flowTable are both valid")

  // ── Registers ────────────────────────────────────────
  private val tableData    = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableIdx     = Reg(UInt(TableIdxWidth.W))
  // blockSize must start at 0: the ring flow masks (crossesBlockBoundary and the
  // ring2/4/8 loopback enables) read it before a never-placed hole unit has learned
  // its block's size from a flowing table, so an uninitialized (random) value can
  // spuriously enable a loopback and inject a stale table into a hole unit.
  private val blockSize    = RegInit(0.U(BlockSize().getWidth.W))
  private val blockFieldNum = RegInit(0.U((TableIdxWidth + 1).W))
  private val isFirstTable = RegInit(false.B)
  private val isLastUnit   = RegInit(false.B)
  private val passedEntry  = RegInit(false.B)

  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView  = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView    = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val meta      = Reg(metaGen)

  private val hasTable  = RegInit(false.B)
  private val hasIndex  = RegInit(false.B)
  private val counter   = RegInit(0.U(log2Up(NumUnits + 1).W))
  private val indexDone = RegInit(VecInit.fill(NumElem)(false.B))
  private val hasWb     = RegInit(false.B)
  // SEW=8 ei16 gather+rvs3 pair: which half has been placed in the current block.
  // Kept so the slot merge and hasIndex are order-independent under out-of-order
  // dispatch (an rvs3 may arrive before its gather).
  private val gatherPlaced = RegInit(false.B)
  private val rvs3Placed   = RegInit(false.B)
  // done waits on hasIndex, which is only set once both the index (gather) and the
  // old-data (rvs3) are in place (see the hasIndex state machine below).
  private val done      = counter === 0.U && hasIndex

  // ── hasTable State Machine ───────────────────────────
  private val hasTableNext = WireInit(hasTable)

  when(flush) {
    hasTable := false.B
  }.otherwise {
    hasTable := hasTableNext
  }

  when(doPlaceTable) {
    hasTableNext := true.B
  }.elsewhen(in.clearTable && passedEntry || hasIndex && counter === 0.U && (hasWb || in.wbReady)) {
    hasTableNext := false.B
  }.elsewhen(doFlow) {
    hasTableNext := in.flow.valid
  }

  // ── hasIndex State Machine ───────────────────────────
  private val hasIndexNext = WireInit(hasIndex)

  when(flush) {
    hasIndex := false.B
  }.otherwise {
    hasIndex := hasIndexNext
  }

  when(doPlaceIndex) {
    // hasIndex = "both the index (gather) and its old-data (rvs3) are in place",
    // robust to arrival order: a gather completes when it is not waiting for an
    // rvs3 or its rvs3 already arrived; an rvs3 completes only when its gather is
    // present. A lone rvs3 (out-of-order) must not set hasIndex by itself.
    hasIndexNext := Mux(
      in.placeIndex.bits.isRvs3,
      gatherPlaced,
      !in.placeIndex.bits.waitRvs3 || rvs3Placed
    )
  }.elsewhen(hasIndex && counter === 0.U && (hasWb || in.wbReady)) {
    // hasWb || in.wbReady means already wb or wb now
    hasIndexNext := false.B
  }

  // ── Pair-half Placed State Machine ───────────────────
  when (flush) {
    gatherPlaced := false.B
    rvs3Placed   := false.B
  }.elsewhen (doPlaceIndex) {
    gatherPlaced := gatherPlaced || in.placeIndex.bits.placesGather
    rvs3Placed   := rvs3Placed || in.placeIndex.bits.placesRvs3
  }.elsewhen (hasIndex && counter === 0.U && (hasWb || in.wbReady)) {
    gatherPlaced := false.B
    rvs3Placed   := false.B
  }

  when (doPlaceTable || doPlaceIndex) {
    blockSize := inBlockSize
    blockFieldNum := inBlockFieldNum
  }.elsewhen (doFlowTable) {
    // D7: a hole unit (never placed) learns the block's size from the first table
    // flowing through, becoming visible to the ring masks (crossesBlockBoundary).
    // Real units only ever receive their own block's tables (same value rewrite).
    blockSize := in.flow.blockSize
  }

  private val isFirstTableNext = Mux1H(Seq(
    doPlaceTable -> in.placeTable.isFirstTable,
    doFlowTable  -> in.flow.isFirstTable,
  ))
  when(doPlaceTable || doFlowTable) {
    isFirstTable := isFirstTableNext
  }.otherwise {
    // hold
  }

  private val isLastUnitNext: Bool = unitIdx.U === (inBlockBase + inBlockSize - 1.U)

  when(in.placeIndex.valid) {
    isLastUnit := isLastUnitNext
  }.elsewhen(in.placeTable.valid) {
    isLastUnit := false.B
  }.elsewhen(doFlowTable) {
    // D8: a hole unit (never placed) derives tail-ness from the flowing table's
    // block; aligned blocks => tail iff (unitIdx + 1) % blockSize == 0. Real
    // units recompute the same value (flow carries their own block's size).
    isLastUnit := GatherRingUnit.pow2remIsZero(unitIdx + 1, in.flow.blockSize)
  }

  when(doPlaceTable) {
    passedEntry := in.placeTable.isFirstTable
  }.elsewhen(doFlowTable) {
    // passedEntry travels with the table: propagate the incoming tag. The ring
    // loopback into the entry unit already forces passedEntry=1, so the entry
    // unit needs no special-casing here.
    passedEntry := in.flow.passedEntry
  }.elsewhen((in.clearTable && passedEntry) || doFlow) {
    passedEntry := false.B
  }.otherwise {
    // hold
  }

  private val tableDataNext = Mux1H(Seq(
    doPlaceTable -> in.placeTable.table,
    doFlowTable -> in.flow.data,
    (!doPlaceTable && !doFlowTable && hasTable) -> tableData,
  ))
  when (doPlaceTable || doFlowTable) {
    tableData := tableDataNext
  }

  private val tableIdxNext = Mux1H(Seq(
    doPlaceTable ->  in.placeTable.tableIdx,
    doFlowTable -> in.flow.tableIdx,
    (!doPlaceTable && !doFlowTable) -> tableIdx,
  ))
  when (doPlaceTable || doFlowTable) {
    tableIdx := tableIdxNext
  }

  // ── Counter ─────────────────────────────────────────
  when(flush) {
    counter := 0.U
  }.elsewhen(doPlaceIndex && !in.placeIndex.bits.isRvs3) {
    // Only the gather (index) placement initializes the counter; the paired rvs3
    // (old-data) placement is a continuation of the same block and must not.
    // The first table of the block (isFirstTable, at the entry unit) is already
    // in place, so one fewer table needs to arrive; a non-first table placement
    // (or index-only) still needs the full blockFieldNum tables to flow in.
    // D2: count real tables (blockFieldNum), not the ring width (blockSize) — a
    // hole block (segment 3/5/6/7) has fewer tables than units.
    when ((hasTable && isFirstTable) || (doPlaceTable && in.placeTable.bits.isFirstTable)) {
      counter := inBlockFieldNum - 1.U
    }.otherwise {
      counter := inBlockFieldNum
    }
  }.elsewhen(doFlowTable && counter > 0.U && in.flow.passedEntry) {
    // Only count tables that carry passedEntry: a table that has not passed the
    // entry has not started a real traversal, so re-circulating partial-block
    // tables (progressive dispatch) would otherwise exhaust the counter and
    // fire done before the late tables arrive (M13/emu 20260815-145113).
    counter := counter - 1.U
  }

  when (doPlaceIndex && !in.placeIndex.bits.isRvs3) {
    hasWb := false.B
  }.elsewhen(hasIndex && out.wb.valid && in.wbReady) {
    hasWb := true.B
  }

  // ── GatherBase Instance ──────────────────────────────
  private val gatherMod = Module(new GatherBase)

  gatherMod.in.tableIdx := tableIdxNext
  gatherMod.in.table    := tableDataNext

  for (i <- 0 until NumElem) {
    val shouldQuery = Mux1H(Seq(
      doPlaceIndex -> (hasTable || doPlaceTable),
      hasIndex -> ((hasTable || doPlaceTable || doFlowTable) && !indexDone(i)),
    ))
    gatherMod.in.indexValid(i) := shouldQuery && Mux(
      doPlaceIndex,
      ElemFlags.isLOOKUP(in.placeIndex.bits.elemFlags(i)),
      true.B
    )
    gatherMod.in.index(i) := Mux(
      doPlaceIndex,
      in.placeIndex.bits.elemData(i),
      indexView(i)
    )
  }

  // ── Index/Result Update ──────────────────────────────
  private val placeIndexFillIdx = Wire(Vec(NumElem, Bool()))
  private val placeIndexFillRes = Wire(Vec(NumElem, Bool()))
  private val placeIndexPrefill   = Wire(Vec(NumElem, Bool()))
  private val placeIndexFillZeros = Wire(Vec(NumElem, Bool()))
  private val fillRes   = Wire(Vec(NumElem, Bool()))
  private val fillIdx   = Wire(Vec(NumElem, Bool()))

  import ElemFlags._

  private val placeIsRvs3   = in.placeIndex.bits.isRvs3
  private val placeWaitRvs3 = in.placeIndex.bits.waitRvs3

  for (i <- 0 until NumElem) {
    val f = in.placeIndex.bits.elemFlags(i)
    placeIndexFillIdx(i) := isLOOKUP(f) && !gatherMod.in.indexValid(i) || gatherMod.in.indexValid(i) && gatherMod.out.notfound(i)
    placeIndexFillRes(i) := isLOOKUP(f) && gatherMod.in.indexValid(i) && gatherMod.out.resultValid(i)
    placeIndexPrefill(i) := isPREFILL(f)
    placeIndexFillZeros(i) := isZERO(f) && doPlaceIndex
    fillRes(i)   := gatherMod.in.indexValid(i) && gatherMod.out.resultValid(i)
    fillIdx(i)   := gatherMod.in.indexValid(i) && gatherMod.out.notfound(i)

    when(flush) {
      indexDone(i)  := false.B
    }.elsewhen(doPlaceIndex && !placeIsRvs3) {
      // gather placement: LOOKUP -> lookup result, ZERO -> 0, PREFILL -> elemData.
      // For a gather with paired rvs3 uops (waitRvs3) the PREFILL slots carry a
      // 0 placeholder and stay pending (indexDone=0) until the rvs3 merges; if the
      // rvs3 already merged (out-of-order arrival, rvs3Placed) OR preserves it.
      indexOrRes(i) := Mux1H(Seq(
        placeIndexFillIdx(i) -> in.placeIndex.bits.elemData(i),
        placeIndexFillRes(i) -> gatherMod.out.result(i),
        placeIndexPrefill(i) -> Mux(rvs3Placed,
          indexOrRes(i) | in.placeIndex.bits.elemData(i), in.placeIndex.bits.elemData(i)),
        placeIndexFillZeros(i) -> 0.U,
      ))
      // PREFILL slots of a waitRvs3 gather stay pending (indexDone=0) until the
      // rvs3 (old-data) placement merges; if the rvs3 already finalized them
      // (rvs3Placed), keep indexDone. Other slots are resolved here (found /
      // notfound lookups, ZERO, and PREFILL without a paired rvs3).
      indexDone(i) := placeIndexFillRes(i) ||
        (isPREFILL(f) && !placeWaitRvs3) || placeIndexFillZeros(i) ||
        (isPREFILL(f) && rvs3Placed)
      // The rvs3's meta (writeback target) wins once it has merged.
      when (!rvs3Placed) { meta := in.placeIndex.meta }
    }.elsewhen(doPlaceIndex && placeIsRvs3) {
      // rvs3 (old-data) placement: bit-OR the incoming PREFILL data into the
      // pending PREFILL slots (gather's 0 | old/0xFF = old/0xFF) and finalize;
      // if the rvs3 arrives before its gather (gatherPlaced=0) it seeds the slots
      // instead. ZERO slots (active) are resolved by the gather lookup. The meta
      // is updated to the rvs3's (its pdest is the writeback target).
      when(isPREFILL(f)) {
        indexOrRes(i) := Mux(gatherPlaced,
          indexOrRes(i) | in.placeIndex.bits.elemData(i), in.placeIndex.bits.elemData(i))
        indexDone(i)  := true.B
      }
      meta := in.placeIndex.meta
    }.elsewhen(doFlowTable) {
      when (fillRes(i)) {
        indexOrRes(i) := gatherMod.out.result(i)
      }
      indexDone(i) := indexDone(i) | fillRes(i)
    }.elsewhen(fillRes(i)) {
      indexOrRes(i) := gatherMod.out.result(i)
      indexDone(i)  := true.B
    }
  }

  // ── Writeback ───────────────────────────────────────
  // All tables have flowed through when counter==0, ready to writeback.
  // Does not depend on out.ready
  out.wb.valid := !hasWb && hasIndex && done
  out.wb.bits.data := resView.asUInt
  out.wb.bits.meta := meta

  // Safety: writeback must never fire while any element's index is unresolved
  // (indexDone all-1). A late-arriving table (progressive / out-of-order
  // dispatch) would otherwise leave lookups pending and corrupt the result.
  assert(!out.wb.valid || indexDone.asUInt.andR, "GatherRingUnit wb with unresolved indexDone")

  // ── VPerm Wakeup (early wake, req-ack) ─────────────
  // delay = issue2exu - counter: counter == issue2exu -> delay=0 (wbM3, earliest wakeup),
  // counter == 0 -> delay=issue2exu (writeback is happening).
  // The request is kept asserted until it is acked (wakedUp set).
  private val wakedUp = RegInit(false.B)
  private val wakeBlocked = RegInit(false.B)

  when (flush || doPlaceIndex) {
    wakedUp := false.B
  }.elsewhen (out.wake.valid && in.wakeAck) {
    wakedUp := true.B
  }

  when (flush || doPlaceIndex) {
    wakeBlocked := false.B
  }.elsewhen (out.wake.valid && !in.wakeAck && in.blockedByEqCounter) {
    wakeBlocked := true.B
  }

  // D3: hole blocks (segment 3/5/6/7, blockFieldNum != block size) never wake —
  // the gapped table train breaks the delay = issue2exu - counter dense-train
  // assumption, so consumers rely on the wb broadcast instead.
  private val hasHole = blockFieldNum =/= blockSizeCount(blockSize)

  out.wake.valid := in.allTableValid && hasIndex && counter <= param.issue2exu.U && !wakedUp && !hasHole
  out.wake.bits.delay := (param.issue2exu.U - counter)(WakeDelayWidth - 1, 0) - wakeBlocked
  out.wake.bits.meta := meta

  // ── Table Output (to next Unit) ──────────────────────
  out.flow.valid := hasTable && !flush && Mux(passedEntry, !isLastUnit, true.B)
  out.flow.data  := tableData
  out.flow.tableIdx := tableIdx
  out.flow.isFirstTable := isFirstTable
  out.flow.passedEntry := passedEntry
  out.flow.blockSize := blockSize

  // ── Status Output (for top-level vacancy detection) ───
  out.status.hasTable     := hasTable
  out.status.hasIndex     := hasIndex
  out.status.hasIndexNext := hasIndexNext
  out.status.counter      := counter
  out.status.blockSize    := blockSize
  out.status.blockFieldNum := blockFieldNum
  out.status.isFirstTable := isFirstTable
  out.status.passedEntry  := passedEntry
  out.status.gatherPlaced := gatherPlaced
  out.status.rvs3Placed   := rvs3Placed
  out.status.done         := done
  out.meta.valid          := hasIndex
  out.meta.bits           := meta

  out.debug.foreach {
    debug =>
      debug.indexOrRes := indexOrRes.asUInt
      debug.indexDone := indexDone.asUInt
  }

  dontTouch(in)
}

object GatherRingUnit {
  class In[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val flush = Bool()
    val placeTable = ValidIO(new PlaceTableRequest)
    val placeIndex = ValidIO(new PlaceIndexRequest(metaGen))
    val flow = ValidIO(new RingFlow)
    val willFlow = Bool()
    val clearTable = Bool()
    val wbReady = Bool()
    // Gate from the ring controller: all tables of the corresponding block are valid
    val allTableValid = Bool()
    // Req-ack: set wakedUp once the wakeup has been accepted
    val wakeAck = Bool()
    // From the ring controller: another unit on the same wake pipe has an equal
    // counter (an equal-delay wake would collide on the arbitrated pipe)
    val blockedByEqCounter = Bool()
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val status = new Status
    val flow = ValidIO(new RingFlow)
    val wb = ValidIO(new Res(metaGen))
    val wake = ValidIO(new Wake(metaGen))
    val meta = ValidIO(metaGen)
    val debug = Option.when(param.debug)(new Debug)
  }

  /**
   *  [[PlaceTableRequest]] is used as table placement bundle
   */
  class PlaceTableRequest(implicit val param: GatherParam) extends GatherBundle {
    /**
     * the index of this table in v[vs2]
     */
    val tableIdx = UnitIdx()

    /**
     * The data of table
     */
    val table = Vec(NumElem, Elem())

    /**
     * If the table is the first table of this block in [[GatherRing]]
     */
    val isFirstTable = Bool()

    /**
     * The size of the block in [[GatherRing]], should be equal to [[PlaceIndexRequest]].blockSize if valid
     */
    val blockSize   = BlockSize()

    /** Real field count of the block (== block size for full blocks, 2..size-1 for hole blocks) */
    val blockFieldNum = BlockFieldNum()
  }

  object PlaceTableRequest {
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T], unitIdx: Int)(implicit param: GatherParam): PlaceTableRequest = {
      val req = Wire(new PlaceTableRequest)
      req.tableIdx := s1.tableIdx
      req.table    := s1.table
      req.isFirstTable := GatherRingUnit.pow2remIsZero(unitIdx, s1.blockSize)
      req.blockSize    := s1.blockSize
      req.blockFieldNum := s1.blockFieldNum
      req
    }
  }

  /**
   * [[PlaceIndexRequest]] is used as index placement bundle.
   * This bundle contains some control signals
   */
  class PlaceIndexRequest[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val blockBase = UnitIdx()
    val blockSize = BlockSize()

    /** Real field count of the block (== block size for full blocks, 2..size-1 for hole blocks) */
    val blockFieldNum = BlockFieldNum()

    // Per-element merged: 2b flags + 8b data (index or prefill)
    val elemFlags = Vec(NumElem, ElemFlags())
    val elemData  = Vec(NumElem, UInt(8.W))

    // SEW=8 vrgatherei16: 1 = this placement is the rvs3 (old-data) uop of a gather
    val isRvs3   = Bool()
    // The placement may merge a same-cycle gather+rvs3 pair, so retain which
    // halves are present independently of the merged request's isRvs3 value.
    val placesGather = Bool()
    val placesRvs3   = Bool()
    // 1 = this gather (isRvs3=0) has paired rvs3 uops whose old-data must be merged
    // before writeback (PREFILL slots are left pending until the rvs3 arrives)
    val waitRvs3 = Bool()

    val meta       = metaGen
  }

  object PlaceIndexRequest {
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T])(implicit param: GatherParam): PlaceIndexRequest[T] = {
      val req = Wire(new PlaceIndexRequest(chiselTypeOf(s1.meta)))
      req.blockBase  := s1.blockBase
      req.blockSize  := s1.blockSize
      req.blockFieldNum := s1.blockFieldNum
      req.elemFlags  := s1.elemFlags
      req.elemData   := s1.elemData
      req.isRvs3     := s1.isRvs3
      req.placesGather := !s1.isRvs3
      req.placesRvs3   := s1.isRvs3
      req.waitRvs3   := s1.waitRvs3
      req.meta       := s1.meta
      req
    }

  }

  class RingFlow(implicit val param: GatherParam) extends GatherBundle {
    val data        = Vec(NumElem, Elem())
    val tableIdx    = UnitIdx()
    val isFirstTable = Bool()
    val passedEntry = Bool()
    // D7: the forwarding unit's block size, so a hole unit can latch it and
    // become visible to the ring masks while the block is alive.
    val blockSize   = BlockSize()
  }

  class Status(implicit val param: GatherParam) extends GatherBundle {
    val hasTable = Bool()
    val hasIndex = Bool()
    val hasIndexNext = Bool()
    val counter = UInt(log2Ceil(NumUnits + 1).W)
    val blockSize = BlockSize()
    val blockFieldNum = BlockFieldNum()
    val isFirstTable = Bool()
    val passedEntry = Bool()
    val gatherPlaced = Bool()
    val rvs3Placed = Bool()
    val done = Bool()
  }

  class Res[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val data = UInt(DLEN.W)
    val meta = metaGen
  }

  /** VPerm wakeup request: the gather result will be available after `delay` bypass stages.
    * delay=0 corresponds to wbM3 (issue2exu-cycle early wakeup), delay=issue2exu to wb.
    */
  class Wake[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val meta = metaGen
    val delay = UInt(WakeDelayWidth.W)
  }

  class Debug()(implicit val param: GatherParam) extends GatherBundle {
    val indexOrRes = UInt(DLEN.W)
    val indexDone = UInt(NumElem.W)
  }

  def pow2remIsZero(dividend: Int, divisor: UInt): Bool = {
    Mux1H(Seq(
      (divisor & 1.U).orR -> true.B,
      (divisor & 2.U).orR -> (dividend % 2 == 0).B,
      (divisor & 4.U).orR -> (dividend % 4 == 0).B,
      (divisor & 8.U).orR -> (dividend % 8 == 0).B,
    ))
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    val targetDir = "build/vector"

    println(s"Generating the GatherRing hardware in $targetDir")

    val firtoolOpts = Array(
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    def RobIdx = UInt(9.W)

    (new ChiselStage).execute(
      Array(
        "--target",
        "systemverilog",
        "--target-dir",
        targetDir,
      ) ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(
        () => new GatherRingUnit(0, RobIdx)(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
