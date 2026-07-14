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
  implicit val param: Param
) extends GatherModule {
  
  override def desiredName: String = 
    super.desiredName + 
    s"_vlen${VLEN}b_dlen${DLEN}b_table${NumUnits}"

  val ResWidth = MinDataWidth

  val in = IO(Input(new GatherRingUnit.In(metaGen)))
  val out = IO(Output(new GatherRingUnit.Out(metaGen)))

  private val flush = in.flush

  // ── 块内局部坐标 ────────────────────────────────────
  private val blockBase = in.placeIndex.bits.blockBase
  private val blockSize = in.placeIndex.bits.blockSize
  private val localPos = Mux1H(Seq(
    (blockSize & 1.U).orR -> 0.U,
    (blockSize & 2.U).orR -> (unitIdx % 2).U,
    (blockSize & 4.U).orR -> (unitIdx % 4).U,
    (blockSize & 8.U).orR -> (unitIdx % 8).U,
  ))
  private val isEntryNext = localPos === 0.U
  private val isRingEnd   = localPos === (blockSize - 1.U)

  // ── scalaTable 字节拆分 ──────────────────────────────
  private val scalaTableBytes: Vec[UInt] = in.placeIndex.scalarData.splitToVecByWidth(MinDataWidth)

  // ── Table 输入 MUX ──────────────────────────────────
  private val doPlaceTable = in.placeTable.valid
  private val doPlaceIndex = in.placeIndex.valid
  private val doFlow       = in.willFlow
  private val doFlowTable  = in.willFlow && in.flow.valid

  // ── 寄存器 ──────────────────────────────────────────
  private val tableValid   = RegInit(false.B)
  private val tableData    = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableIdx     = Reg(UInt(TableIdxWidth.W))
  private val isFirstTable = Reg(Bool())
  private val passedEntry  = Reg(Bool())

  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView  = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView    = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val meta      = Reg(metaGen)

  private val hasTable  = RegInit(false.B)
  private val hasIndex  = RegInit(false.B)
  private val isEntry   = Reg(Bool())
  private val counter   = RegInit(0.U(log2Up(NumUnits).W))
  private val indexDone = Reg(Vec(NumElem, Bool()))

  // ── hasTable 状态机 ─────────────────────────────────
  when(flush) {
    hasTable := false.B
  }.elsewhen(doPlaceTable) {
    hasTable := true.B
  }.elsewhen(doFlow && in.flow.valid) {
    hasTable := true.B
  }.elsewhen(doFlow && !in.flow.valid) {
    hasTable := false.B
  }.elsewhen(!doFlow && !doPlaceTable) {
    // hold
  }

  // ── hasIndex 状态机 ─────────────────────────────────
  when(flush) {
    hasIndex := false.B
  }.elsewhen(doPlaceIndex) {
    hasIndex := true.B
  }.otherwise {
    // hold
  }

  when(doPlaceIndex) {
    isEntry := isEntryNext
  }

  private val isFirstTableNext = Mux(
    doPlaceTable,
    in.placeTable.isFirstTable,
    in.flow.isFirstTable,
  )
  isFirstTable := isFirstTableNext

  private val passedEntryNext = Mux1H(Seq(
    doPlaceTable -> in.placeTable.isFirstTable,
    doFlow -> isEntryNext,
  ))
  passedEntry := passedEntryNext

  private val tableDataNext = Mux1H(Seq(
    doPlaceTable -> in.placeTable.table,
    doFlowTable -> in.flow.data,
  ))
  when (doPlaceTable || doFlowTable) {
    tableData := tableDataNext
  }

  private val tableValidNext = Mux1H(Seq(
    doPlaceTable -> true.B,
    doFlowTable -> true.B,
  ))
  when (flush) {
    tableValid := false.B
  }.elsewhen (tableValid || tableValidNext) {
    tableValid := tableValidNext
  }

  private val tableIdxNext = Mux1H(Seq(
    doPlaceTable ->  in.placeTable.tableIdx,
    doFlowTable -> in.flow.tableIdx
  ))
  when (tableValidNext) {
    tableIdx := tableIdxNext
  }

  // ── Counter ─────────────────────────────────────────
  when(flush) {
    counter := 0.U
  }.elsewhen(doPlaceIndex) {
    counter := blockSize - 1.U
  }.elsewhen(doFlow && hasTable) {
    counter := counter - 1.U
  }

  // ── GatherBase 实例 ─────────────────────────────────
  private val gatherMod = Module(new GatherBase)

  gatherMod.in.tableIdx := tableIdxNext
  gatherMod.in.table    := tableDataNext
  gatherMod.in.lmul     := 0.U.asTypeOf(new GatherLmulBundle)

  for (i <- 0 until NumElem) {
    val shouldQuery = (hasIndex || doPlaceIndex) && (doPlaceIndex || !indexDone(i))
    gatherMod.in.indexValid(i) := shouldQuery && Mux(
      doPlaceIndex,
      in.placeIndex.bits.indexValid(i),
      true.B
    )
    gatherMod.in.index(i) := Mux(
      doPlaceIndex,
      in.placeIndex.bits.index(i),
      indexView(i)
    )
  }

  // ── Index/Result 更新 ───────────────────────────────
  private val fillRes   = Wire(Vec(NumElem, Bool()))
  private val fillIdx   = Wire(Vec(NumElem, Bool()))
  private val fillZeros = Wire(Vec(NumElem, Bool()))
  private val fillScala = Wire(Vec(NumElem, Bool()))

  for (i <- 0 until NumElem) {
    fillRes(i)   := (hasIndex || doPlaceIndex) && gatherMod.out.resultValid(i)
    fillIdx(i)   := doPlaceIndex && hasIndex && gatherMod.out.notfound(i)
    fillZeros(i) := doPlaceIndex && in.placeIndex.bits.fillZeros(i)
    fillScala(i) := doPlaceIndex && in.placeIndex.bits.fillScala(i)

    when(flush) {
      indexDone(i)  := false.B
    }.elsewhen(fillRes(i)) {
      indexOrRes(i) := gatherMod.out.result(i)
      indexDone(i)  := true.B
    }.elsewhen(doPlaceIndex) {
      indexOrRes(i) := Mux1H(
        Seq(
          fillScala(i)                      -> scalaTableBytes(i % (XLEN / MinDataWidth)),
          fillZeros(i)                      -> 0.U,
          !in.placeIndex.bits.indexValid(i) -> in.placeIndex.bits.prefillData(i),
          fillIdx(i)                        -> in.placeIndex.bits.index(i),
        )
      )
      indexDone(i) := false.B
      meta := in.placeIndex.meta
    }.elsewhen(counter === 0.U && doFlow) {
      indexDone(i) := true.B
    }
  }

  // ── Writeback ───────────────────────────────────────
  // counter==0 时所有 table 已流过，可写回
  // 不依赖 out.ready
  out.wb.valid := hasIndex && counter === 0.U
  out.wb.bits.data := resView.asUInt

  // ── Table 输出（流向下一个 Unit）─────────────────────
  out.flow.valid := tableValid && hasTable
  out.flow.data  := tableData
  out.flow.tableIdx := tableIdx
  out.flow.isFirstTable := isFirstTable
  out.flow.passedEntry := passedEntry

  // ── 状态输出（给顶层做空位检测）─────────────────────
  out.status.hasTable := hasTable
  out.status.hasIndex := hasIndex
}

object GatherRingUnit {
  class In[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    val flush = Bool()
    val placeTable = ValidIO(new PlaceTableRequest)
    val placeIndex = ValidIO(new PlaceIndexRequest(metaGen))
    val flow = ValidIO(new RingFlow)
    val willFlow = Bool()
    val wbReady = Bool()
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    val status = new Status
    val flow = ValidIO(new RingFlow)
    val wb = ValidIO(new Res(metaGen))
  }

  class Res[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    val data = UInt(DLEN.W)
    val meta = metaGen
  }

  /**
   *  [[PlaceTableRequest]] is used as table placement bundle
   */
  class PlaceTableRequest(implicit val param: Param) extends GatherBundle {
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
  }

  /**
   * [[PlaceIndexRequest]] is used as index placement bundle.
   * This bundle contains some control signals
   */
  class PlaceIndexRequest[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    /**
     * The start index of the block in [[GatherRing]]
     */
    val blockBase   = UnitIdx()

    /**
     * The size of the block in [[GatherRing]]
     */
    val blockSize   = BlockSize()

    /**
     * Precalculated index vector
     */
    val index       = Vec(NumElem, Index())

    /**
     * If the index need to do gather
     */
    val indexValid  = Vec(NumElem, Bool())

    /**
     * The result need to be filled with zero, when the index of vrgather is greater than vlmax
     */
    val fillZeros   = Vec(NumElem, Bool())

    /**
     * The result need to be filled with scalar value(x[rs1]), when the executed uop is vslide1up/down
     */
    val fillScala   = Vec(NumElem, Bool())

    /**
     * The inactive/tail data
     */
    val prefillData = Vec(NumElem, Elem())

    /**
     * The scalar data x[rs1] or f[fs1]
     */
    val scalarData = UInt(XLEN.W)

    /**
     * The meta data, contains pdest and robIdx
     */
    val meta = metaGen
  }

  class RingFlow(implicit val param: Param) extends GatherBundle {
    val data        = Vec(NumElem, Elem())
    val tableIdx    = UnitIdx()
    val isFirstTable = Bool()
    val passedEntry = Bool()
  }

  class Status(implicit val param: Param) extends GatherBundle {
    val hasTable = Bool()
    val hasIndex = Bool()
    val blockBase = UnitIdx()
    val blockSize = BlockSize()
    val isEntry = Bool()
    val isFirstTable = Bool()
    val passedEntry = Bool()
    val done = Bool()
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
        () => new GatherRingUnit(0, RobIdx)(Param.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
