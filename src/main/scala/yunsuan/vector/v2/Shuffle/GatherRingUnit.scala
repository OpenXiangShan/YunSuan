package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._

class GatherRingUnit(
  val vlen:    Int,
  val dlen:    Int,
  maxTableNum: Int,
  xlen:        Int = 64,
  val unitIdx: Int = 0 // 此 Unit 在阵列中的全局编号
) extends Module
    with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)
  require(isPow2(maxTableNum))

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b_table${maxTableNum}"

  val ResWidth = MinDataWidth

  // ── IO ──────────────────────────────────────────────
  val in = IO(Input(new Bundle {
    // 块配置
    val blockConfig = Input(new BlockConfig(MaxTableNum))

    // Table 输入 MUX（4 选 1，AND-OR 逻辑）
    val placeTable = Flipped(ValidIO(new PlaceTableRequest(NumElem, MaxTableNum, TableIdxWidth)))
    val placeIndex = Flipped(ValidIO(new PlaceIndexRequest(NumElem, IndexWidth, MaxTableNum)))
    val flowIn     = Input(new RingTable(NumElem, TableIdxWidth)) // 环形流入

    // 外部控制
    val willFlow = Input(Bool())

    // 查询辅助
    val scalaTable = Input(UInt(xlen.W))
    val oldTable   = Input(Vec(NumElem, UInt(MinDataWidth.W)))
  }))

  val flush = IO(Input(Bool()))

  val out = IO(new Bundle {
    val status   = Output(new UnitStatus)
    val tableOut = Output(new RingTable(NumElem, TableIdxWidth))
    val writeback = DecoupledIO(new Bundle {
      val dest = UInt(dlen.W)
    })
  })

  // ── 块内局部坐标 ────────────────────────────────────
  private val blockConfig = in.blockConfig
  private val localPos    = (unitIdx.U - blockConfig.blockBase + MaxTableNum.U) % MaxTableNum.U
  private val isEntry     = localPos === 0.U
  private val isRingEnd   = localPos === (blockConfig.blockSize - 1.U)

  // ── scalaTable 字节拆分 ──────────────────────────────
  private val scalaTableBytes = in.scalaTable.asTypeOf(Vec(xlen / MinDataWidth, UInt(MinDataWidth.W)))

  // ── Table 输入 MUX ──────────────────────────────────
  private val doPlaceTable = in.placeTable.valid
  private val doPlaceIndex = in.placeIndex.valid
  private val doPlace      = doPlaceTable || doPlaceIndex
  private val doFlow       = in.willFlow
  private val tableInData = Mux(
    doPlaceTable,
    in.placeTable.bits.table,
    Mux(
      doFlow,
      in.flowIn.data,
      0.U.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))
    )
  )
  private val tableInValid = Mux(
    doPlaceTable,
    true.B,
    Mux(doFlow, in.flowIn.valid, false.B)
  )
  private val tableInIdx = Mux(
    doPlaceTable,
    in.placeTable.bits.tableIdx,
    Mux(doFlow, in.flowIn.idx, 0.U)
  )
  private val tableInTag = Wire(new TableTag)
  tableInTag := Mux(
    doPlaceTable,
    0.U.asTypeOf(new TableTag),
    in.flowIn.tag
  )

  // ── 寄存器 ──────────────────────────────────────────
  private val tableReg   = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableValid = RegInit(false.B)
  private val tableIdx   = Reg(UInt(TableIdxWidth.W))
  private val tableTag   = Reg(new TableTag)

  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView  = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView    = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val hasTable  = RegInit(false.B)
  private val hasIndex  = RegInit(false.B)
  private val counter   = RegInit(0.U(log2Up(MaxTableNum).W))
  private val indexDone = Reg(Vec(NumElem, Bool()))

  // ── hasTable 状态机 ─────────────────────────────────
  when(flush) {
    hasTable := false.B
  }.elsewhen(doPlaceTable) {
    hasTable := true.B
  }.elsewhen(doFlow && in.flowIn.valid) {
    hasTable := true.B
  }.elsewhen(doFlow && !in.flowIn.valid) {
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

  // ── Table 寄存器更新 ────────────────────────────────
  when(doPlaceTable) {
    tableReg   := tableInData
    tableValid := tableInValid
    tableIdx   := tableInIdx
    tableTag   := tableInTag
  }.elsewhen(doFlow && in.flowIn.valid) {
    tableReg   := tableInData
    tableValid := tableInValid
    tableIdx   := tableInIdx
    tableTag   := tableInTag
  }.elsewhen(flush) {
    tableValid := false.B
  }

  // ── Tag 更新：passedEntry / isFirst ─────────────────
  when(doPlaceTable) {
    tableTag.passedEntry := false.B
    tableTag.isFirst     := in.placeTable.bits.isFirstTable
  }.elsewhen(doFlow && isEntry && in.flowIn.valid) {
    tableTag.passedEntry := true.B
  }

  // ── Counter ─────────────────────────────────────────
  when(flush) {
    counter := 0.U
  }.elsewhen(doPlaceIndex) {
    counter := blockConfig.blockSize - 1.U
  }.elsewhen(doFlow && hasTable) {
    counter := counter - 1.U
  }

  // ── GatherBase 实例 ─────────────────────────────────
  private val gatherMod = Module(
    new GatherBase(
      vlen = vlen,
      dlen = dlen,
      maxTableNum = maxTableNum
    )
  )

  gatherMod.in.tableIdx := tableInIdx
  gatherMod.in.table    := tableInData
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
      indexOrRes(i) := 0.U
      indexDone(i)  := false.B
    }.elsewhen(fillRes(i)) {
      indexOrRes(i) := gatherMod.out.result(i)
      indexDone(i)  := true.B
    }.elsewhen(doPlaceIndex) {
      indexOrRes(i) := Mux1H(
        Seq(
          fillScala(i)                      -> scalaTableBytes(i % (xlen / MinDataWidth)),
          fillZeros(i)                      -> 0.U,
          !in.placeIndex.bits.indexValid(i) -> in.oldTable(i),
          fillIdx(i)                        -> in.placeIndex.bits.index(i),
          true.B                            -> in.placeIndex.bits.prefillData(i)
        )
      )
      indexDone(i) := false.B
    }.elsewhen(counter === 0.U && doFlow) {
      indexDone(i) := true.B
    }
  }

  // ── Writeback ───────────────────────────────────────
  // counter==0 时所有 table 已流过，可写回
  // 不依赖 out.ready
  out.writeback.valid     := hasIndex && counter === 0.U
  out.writeback.bits.dest := resView.asUInt

  // ── Table 输出（流向下一个 Unit）─────────────────────
  out.tableOut.data  := tableReg
  out.tableOut.valid := tableValid && hasTable
  out.tableOut.idx   := tableIdx
  out.tableOut.tag   := tableTag

  // ── 状态输出（给顶层做空位检测）─────────────────────
  out.status.hasTable := hasTable
  out.status.hasIndex := hasIndex
}
