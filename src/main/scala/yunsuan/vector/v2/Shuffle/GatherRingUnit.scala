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

  // ── Table Input MUX ──────────────────────────────────
  private val doPlaceTable = in.placeTable.valid
  private val doPlaceIndex = in.placeIndex.valid
  private val doFlow       = in.willFlow
  private val doFlowTable  = in.willFlow && in.flow.valid

  // ── Registers ────────────────────────────────────────
  private val tableData    = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableIdx     = Reg(UInt(TableIdxWidth.W))
  private val blockSize    = Reg(BlockSize())
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
  private val done      = counter === 0.U && hasIndex

  // ── hasTable State Machine ───────────────────────────
  private val hasTableNext = WireInit(hasTable)

  when(flush) {
    hasTable := false.B
  }.otherwise {
    hasTable := hasTableNext
  }

  when(in.clearTable) {
    hasTableNext := false.B
  }.elsewhen(doPlaceTable || doFlowTable) {
    hasTableNext := true.B
  }.elsewhen(doFlow && !in.flow.valid) {
    hasTableNext := false.B
  }

  // ── hasIndex State Machine ───────────────────────────
  private val hasIndexNext = WireInit(hasIndex)

  when(flush) {
    hasIndex := false.B
  }.otherwise {
    hasIndex := hasIndexNext
  }

  when(doPlaceIndex) {
    hasIndexNext := true.B
  }.elsewhen(hasIndex && counter === 0.U && (hasWb || in.wbReady)) {
    // hasWb || in.wbReady means already wb or wb now
    hasIndexNext := false.B
  }

  when (doPlaceTable || doPlaceIndex) {
    blockSize := inBlockSize
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
  }

  when(doPlaceTable) {
    passedEntry := in.placeTable.isFirstTable
  }.elsewhen(doFlowTable) {
    passedEntry := GatherRingUnit.pow2remIsZero(unitIdx, blockSize)
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
    doFlowTable -> in.flow.tableIdx
  ))
  when (doPlaceTable || doFlowTable) {
    tableIdx := tableIdxNext
  }

  // ── Counter ─────────────────────────────────────────
  when(flush) {
    counter := 0.U
  }.elsewhen(doPlaceIndex) {
    when (doPlaceTable || hasTable) {
      counter := inBlockSize - 1.U
    }.otherwise {
      counter := inBlockSize
    }
  }.elsewhen(doFlowTable && counter > 0.U) {
    counter := counter - 1.U
  }

  when (doPlaceIndex) {
    hasWb := false.B
  }.elsewhen(hasIndex && out.wb.valid && in.wbReady) {
    hasWb := true.B
  }

  // ── GatherBase Instance ──────────────────────────────
  private val gatherMod = Module(new GatherBase)

  gatherMod.in.tableIdx := tableIdxNext
  gatherMod.in.table    := tableDataNext

  for (i <- 0 until NumElem) {
    val shouldQuery = (hasIndex || doPlaceIndex) && (doPlaceIndex || !indexDone(i)) &&
      (hasTable || doPlaceTable || doFlowTable)
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
  private val fillRes   = Wire(Vec(NumElem, Bool()))
  private val fillIdx   = Wire(Vec(NumElem, Bool()))
  private val fillZeros = Wire(Vec(NumElem, Bool()))
  private val prefill   = Wire(Vec(NumElem, Bool()))

  import ElemFlags._

  for (i <- 0 until NumElem) {
    val f = in.placeIndex.bits.elemFlags(i)
    prefill(i)   := isPREFILL(f)
    fillZeros(i) := isZERO(f) && doPlaceIndex
    fillRes(i)   := (doPlaceIndex || hasIndex) && gatherMod.out.resultValid(i)
    fillIdx(i)   := (doPlaceIndex || hasIndex) && gatherMod.out.notfound(i)

    when(flush) {
      indexDone(i)  := false.B
    }.elsewhen(doPlaceIndex) {
      indexOrRes(i) := Mux1H(Seq(
        prefill(i)   -> in.placeIndex.bits.elemData(i),
        fillZeros(i) -> 0.U,
        fillIdx(i)   -> in.placeIndex.bits.elemData(i),
        fillRes(i)   -> gatherMod.out.result(i),
      ))
      indexDone(i) := !fillIdx(i)
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

  // ── VPerm Wakeup (early wake, req-ack) ─────────────
  // delay = issue2exu - counter: counter == issue2exu -> delay=0 (wbM3, earliest wakeup),
  // counter == 0 -> delay=issue2exu (writeback is happening).
  // The request is kept asserted until it is acked (wakedUp set).
  private val wakedUp = RegInit(false.B)

  when (flush || doPlaceIndex) {
    wakedUp := false.B
  }.elsewhen (out.wake.valid && in.wakeAck) {
    wakedUp := true.B
  }

  out.wake.valid := in.allTableValid && hasIndex && counter <= param.issue2exu.U && !wakedUp
  out.wake.bits.delay := (param.issue2exu.U - counter)(WakeDelayWidth - 1, 0)
  out.wake.bits.meta := meta

  // ── Table Output (to next Unit) ──────────────────────
  out.flow.valid := hasTable && Mux(passedEntry, !isLastUnit, true.B)
  out.flow.data  := tableData
  out.flow.tableIdx := tableIdx
  out.flow.isFirstTable := isFirstTable
  out.flow.passedEntry := passedEntry

  // ── Status Output (for top-level vacancy detection) ───
  out.status.hasTable     := hasTable
  out.status.hasIndex     := hasIndex
  out.status.hasIndexNext := hasIndexNext
  out.status.counter      := counter
  out.status.blockSize    := blockSize
  out.status.isFirstTable := isFirstTable
  out.status.passedEntry  := passedEntry
  out.status.done         := done

  out.debug.foreach {
    debug => debug.indexOrRes := indexOrRes.asUInt
  }
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
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val status = new Status
    val flow = ValidIO(new RingFlow)
    val wb = ValidIO(new Res(metaGen))
    val wake = ValidIO(new Wake(metaGen))
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
  }

  object PlaceTableRequest {
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T], unitIdx: Int)(implicit param: GatherParam): PlaceTableRequest = {
      val req = Wire(new PlaceTableRequest)
      req.tableIdx := s1.tableIdx
      req.table    := s1.table
      req.isFirstTable := GatherRingUnit.pow2remIsZero(unitIdx, s1.blockSize)
      req.blockSize    := s1.blockSize
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

    // Per-element merged: 2b flags + 8b data (index or prefill)
    val elemFlags = Vec(NumElem, ElemFlags())
    val elemData  = Vec(NumElem, UInt(8.W))

    val meta       = metaGen
  }

  object PlaceIndexRequest {
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T])(implicit param: GatherParam): PlaceIndexRequest[T] = {
      val req = Wire(new PlaceIndexRequest(chiselTypeOf(s1.meta)))
      req.blockBase  := s1.blockBase
      req.blockSize  := s1.blockSize
      req.elemFlags  := s1.elemFlags
      req.elemData   := s1.elemData
      req.meta       := s1.meta
      req
    }
  }

  class RingFlow(implicit val param: GatherParam) extends GatherBundle {
    val data        = Vec(NumElem, Elem())
    val tableIdx    = UnitIdx()
    val isFirstTable = Bool()
    val passedEntry = Bool()
  }

  class Status(implicit val param: GatherParam) extends GatherBundle {
    val hasTable = Bool()
    val hasIndex = Bool()
    val hasIndexNext = Bool()
    val counter = UInt(log2Ceil(NumUnits + 1).W)
    val blockSize = BlockSize()
    val isFirstTable = Bool()
    val passedEntry = Bool()
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
