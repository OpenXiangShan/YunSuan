package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import GatherCommon._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherRingUnit.PlaceTableRequest

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
  private val blockBase = in.placeIndex.bits.blockBase
  private val blockSize = Mux1H(Seq(
    in.placeIndex.valid -> in.placeIndex.blockSize,
    in.placeTable.valid -> in.placeTable.blockSize,
  ))
  private val localPos = Mux1H(Seq(
    (blockSize & 1.U).orR -> 0.U,
    (blockSize & 2.U).orR -> (unitIdx % 2).U,
    (blockSize & 4.U).orR -> (unitIdx % 4).U,
    (blockSize & 8.U).orR -> (unitIdx % 8).U,
  ))
  private val isEntryNext = localPos === 0.U

  // ── Scala Table Byte Split ───────────────────────────
  private val scalaTableBytes: Vec[UInt] = in.placeIndex.scalarData.splitToVecByWidth(MinDataWidth)

  // ── Table Input MUX ──────────────────────────────────
  private val doPlaceTable = in.placeTable.valid
  private val doPlaceIndex = in.placeIndex.valid
  private val doFlow       = in.willFlow
  private val doFlowTable  = in.willFlow && in.flow.valid

  // ── Registers ────────────────────────────────────────
  private val tableData    = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableIdx     = Reg(UInt(TableIdxWidth.W))
  private val isFirstTable = RegInit(false.B)
  private val passedEntry  = RegInit(false.B)

  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView  = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView    = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val meta      = RegInit(0.U.asTypeOf(metaGen))

  private val hasTable  = RegInit(false.B)
  private val hasIndex  = RegInit(false.B)
  private val isEntry   = RegInit(false.B)
  private val counter   = RegInit(0.U(log2Up(NumUnits + 1).W))
  private val indexDone = RegInit(VecInit.fill(NumElem)(false.B))
  private val hasWb     = RegInit(false.B)
  private val done      = indexDone.asUInt.andR

  // ── hasTable State Machine ───────────────────────────
  when(flush) {
    hasTable := false.B
  }.elsewhen(doPlaceTable || doFlowTable) {
    hasTable := true.B
  }.elsewhen(doFlow && !in.flow.valid) {
    hasTable := false.B
  }.otherwise {
    // hold
  }

  // ── hasIndex State Machine ───────────────────────────
  when(doPlaceIndex) {
    hasIndex := true.B
  }.elsewhen(flush || (hasIndex && counter === 0.U && (hasWb || in.wbReady))) {
    // hasWb || in.wbReady means already wb or wb now
    hasIndex := false.B
  }.otherwise {
    // hold
  }

  when(doPlaceIndex) {
    isEntry := isEntryNext
  }

  private val isFirstTableNext = Mux1H(Seq(
    doPlaceTable -> in.placeTable.isFirstTable,
    doFlow       -> in.flow.isFirstTable,
  ))
  when(doPlaceTable || doFlow) {
    isFirstTable := isFirstTableNext
  }.otherwise {
    // hold
  }

  when(doPlaceTable) {
    passedEntry := in.placeTable.isFirstTable
  }.elsewhen(doFlow) {
    passedEntry := isEntryNext
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
      counter := blockSize - 1.U
    }.otherwise {
      counter := blockSize
    }
  }.elsewhen(doFlow && hasTable) {
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
      in.placeIndex.bits.elemFlags(i) === ElemFlags.LOOKUP,
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
  private val fillScala = Wire(Vec(NumElem, Bool()))
  private val prefill   = Wire(Vec(NumElem, Bool()))

  import ElemFlags._

  for (i <- 0 until NumElem) {
    val f = in.placeIndex.bits.elemFlags(i)
    prefill(i)   := f === PREFILL
    fillScala(i) := f === SCALAR && doPlaceIndex
    fillZeros(i) := f === ZERO && doPlaceIndex
    fillRes(i)   := (doPlaceIndex || hasIndex) && gatherMod.out.resultValid(i)
    fillIdx(i)   := (doPlaceIndex || hasIndex) && gatherMod.out.notfound(i)

    when(flush) {
      indexDone(i)  := false.B
    }.elsewhen(doPlaceIndex) {
      indexOrRes(i) := Mux1H(Seq(
        prefill(i)   -> in.placeIndex.bits.elemData(i),
        fillScala(i) -> scalaTableBytes(i % (XLEN / MinDataWidth)),
        fillZeros(i) -> 0.U,
        fillIdx(i)   -> in.placeIndex.bits.elemData(i),
        fillRes(i)   -> gatherMod.out.result(i),
      ))
      indexDone(i) := !fillIdx(i)
      meta := in.placeIndex.meta
    }.elsewhen(doFlow) {
      when (fillRes(i)) {
        indexOrRes(i) := gatherMod.out.result(i)
      }
      indexDone(i) := indexDone(i) | fillRes(i)
    }.elsewhen(fillRes(i)) {
      indexOrRes(i) := gatherMod.out.result(i)
      indexDone(i)  := true.B
    }.elsewhen(counter === 0.U && doFlow) {
      indexDone(i) := true.B
    }
  }

  // ── Writeback ───────────────────────────────────────
  // All tables have flowed through when counter==0, ready to writeback.
  // Does not depend on out.ready
  out.wb.valid := !hasWb && hasIndex && done
  out.wb.bits.data := resView.asUInt
  out.wb.bits.meta := meta

  // ── Table Output (to next Unit) ──────────────────────
  out.flow.valid := hasTable
  out.flow.data  := tableData
  out.flow.tableIdx := tableIdx
  out.flow.isFirstTable := isFirstTable
  out.flow.passedEntry := passedEntry

  // ── Status Output (for top-level vacancy detection) ───
  out.status.hasTable     := hasTable
  out.status.hasIndex     := hasIndex
  out.status.counter      := counter
  out.status.blockBase    := blockBase
  out.status.blockSize    := blockSize
  out.status.isEntry      := isEntry
  out.status.isFirstTable := isFirstTable
  out.status.passedEntry  := passedEntry
  out.status.done         := indexDone.asUInt.andR

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
    val wbReady = Bool()
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val status = new Status
    val flow = ValidIO(new RingFlow)
    val wb = ValidIO(new Res(metaGen))
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
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T])(implicit param: GatherParam): PlaceTableRequest = {
      val req = Wire(new PlaceTableRequest)
      req.tableIdx := s1.tableIdx
      req.table    := s1.table
      req.isFirstTable := s1.placeIdx === 0.U
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

    val scalarData = UInt(XLEN.W)
    val meta       = metaGen
  }

  object PlaceIndexRequest {
    def fromPipeInS1[T <: Data](s1: GatherRing.PipeInS1[T])(implicit param: GatherParam): PlaceIndexRequest[T] = {
      val req = Wire(new PlaceIndexRequest(s1.meta.cloneType))
      req.blockBase  := s1.blockBase
      req.blockSize  := s1.blockSize
      req.elemFlags  := s1.elemFlags
      req.elemData   := s1.elemData
      req.scalarData := s1.scalarData
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
    val counter = UInt(log2Ceil(NumUnits + 1).W)
    val blockBase = UnitIdx()
    val blockSize = BlockSize()
    val isEntry = Bool()
    val isFirstTable = Bool()
    val passedEntry = Bool()
    val done = Bool()
  }

  class Res[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val data = UInt(DLEN.W)
    val meta = metaGen
  }

  class Debug()(implicit val param: GatherParam) extends GatherBundle {
    val indexOrRes = UInt(DLEN.W)
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
