package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._

class GatherRingUnit(
  val vlen: Int,
  val dlen: Int,
  maxTableNum: Int,
  xlen: Int = 64,
  val unitIdx: Int = 0,  // 此 Unit 在阵列中的全局编号
) extends Module with GatherConfig {
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
    val place      = Flipped(ValidIO(new PlaceRequest(NumElem, IndexWidth, MaxTableNum, TableIdxWidth)))
    val flowIn     = Input(new RingTable(NumElem, TableIdxWidth))  // 环形流入（前驱或回环，由顶层 MUX 决定）

    // 外部控制
    val willFlow   = Input(Bool())

    // 查询辅助
    val scalaTable = Input(UInt(xlen.W))
    val oldTable   = Input(Vec(NumElem, UInt(MinDataWidth.W)))
  }))

  val flush = IO(Input(Bool()))

  val out = IO(new Bundle {
    val status    = Output(new UnitStatus)
    val tableOut  = Output(new RingTable(NumElem, TableIdxWidth))
    val writeback = DecoupledIO(new Bundle {
      val dest = UInt(dlen.W)
    })
  })

  // ── 块内局部坐标 ────────────────────────────────────
  private val blockConfig = in.blockConfig
  private val localPos = (unitIdx.U - blockConfig.blockBase + MaxTableNum.U) % MaxTableNum.U
  private val isEntry   = localPos === 0.U
  private val isRingEnd = localPos === (blockConfig.blockSize - 1.U)

  // ── scalaTable 字节拆分 ──────────────────────────────
  private val scalaTableBytes = in.scalaTable.asTypeOf(Vec(xlen / MinDataWidth, UInt(MinDataWidth.W)))

  // ── Table 输入 MUX ──────────────────────────────────
  private val doPlace = in.place.valid
  private val doFlow  = in.willFlow
  private val tableInData  = Mux(doPlace, in.place.bits.table, Mux(doFlow, in.flowIn.data, 0.U.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))))
  private val tableInValid = Mux(doPlace, true.B, Mux(doFlow, in.flowIn.valid, false.B))
  private val tableInIdx   = Mux(doPlace, in.place.bits.tableIdx, Mux(doFlow, in.flowIn.idx, 0.U))
  private val tableInTag   = Wire(new TableTag)
  tableInTag := Mux(doPlace, 0.U.asTypeOf(new TableTag), in.flowIn.tag)

  // ── 寄存器 ──────────────────────────────────────────
  private val tableReg    = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val tableValid  = RegInit(false.B)
  private val tableIdx    = Reg(UInt(TableIdxWidth.W))
  private val tableTag    = Reg(new TableTag)

  private val indexOrRes  = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView   = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView     = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val hasTable    = RegInit(false.B)
  private val hasIndex    = RegInit(false.B)
  private val counter     = RegInit(0.U(log2Up(MaxTableNum).W))
  private val indexDone   = Reg(Vec(NumElem, Bool()))

  // ── hasTable 状态机 ─────────────────────────────────
  when (flush) {
    hasTable := false.B
  }.elsewhen (doFlow && in.flowIn.valid) {
    hasTable := true.B
  }.elsewhen (doFlow && !in.flowIn.valid) {
    hasTable := false.B
  }.elsewhen (doPlace && in.place.bits.isTableTarget) {
    hasTable := true.B
  }.elsewhen (!doFlow && !doPlace) {
    // hold
  }

  // ── hasIndex 状态机 ─────────────────────────────────
  when (flush) {
    hasIndex := false.B
  }.elsewhen (doPlace && in.place.bits.isIndexTarget) {
    hasIndex := true.B
  }.otherwise {
    // hold
  }

  // ── Table 寄存器更新 ────────────────────────────────
  when (doFlow && in.flowIn.valid) {
    tableReg   := tableInData
    tableValid := tableInValid
    tableIdx   := tableInIdx
    tableTag   := tableInTag
  }.elsewhen (doPlace && in.place.bits.isTableTarget) {
    tableReg   := tableInData
    tableValid := tableInValid
    tableIdx   := tableInIdx
    tableTag   := tableInTag
  }.elsewhen (flush) {
    tableValid := false.B
  }

  // ── Tag 更新：passedEntry / isFirst ─────────────────
  when (doPlace) {
    tableTag.passedEntry := false.B
    tableTag.isFirst     := in.place.bits.isFirstTable
  }.elsewhen (doFlow && isEntry && in.flowIn.valid) {
    tableTag.passedEntry := true.B
  }

  // ── Counter ─────────────────────────────────────────
  when (flush) {
    counter := 0.U
  }.elsewhen (doPlace) {
    counter := blockConfig.blockSize - 1.U
  }.elsewhen (doFlow && hasTable) {
    counter := counter - 1.U
  }

  // ── GatherBase 实例 ─────────────────────────────────
  private val gatherMod = Module(new GatherBase(vlen = vlen, dlen = dlen, maxTableNum = maxTableNum))

  gatherMod.in.tableIdx := tableInIdx
  gatherMod.in.table    := tableInData
  gatherMod.in.lmul     := 0.U.asTypeOf(new GatherLmulBundle)  // LMUL 不影响查找，indexValid 已处理越界

  for (i <- 0 until NumElem) {
    // 查询条件：有 index 且该元素尚未完成
    val shouldQuery = (hasIndex || doPlace) && (doPlace || !indexDone(i))
    gatherMod.in.indexValid(i) := shouldQuery && Mux(doPlace, in.place.bits.indexValid(i), true.B)
    gatherMod.in.index(i) := Mux(doPlace, in.place.bits.index(i), indexView(i))
  }

  // ── Index/Result 更新 ───────────────────────────────
  // Inactive/tail 元素在 Place 时就确定最终值
  // Active 元素：Place 时加载 index，后续 flow 命中后覆写为 result
  private val fillRes   = Wire(Vec(NumElem, Bool()))
  private val fillIdx   = Wire(Vec(NumElem, Bool()))
  private val fillZeros = Wire(Vec(NumElem, Bool()))
  private val fillScala = Wire(Vec(NumElem, Bool()))

  for (i <- 0 until NumElem) {
    fillRes(i)   := (hasIndex || doPlace) && gatherMod.out.resultValid(i)
    fillIdx(i)   := doPlace && hasIndex && gatherMod.out.notfound(i)  // Place 时 notfound → 存 index
    fillZeros(i) := doPlace && in.place.bits.fillZeros(i)
    fillScala(i) := doPlace && in.place.bits.fillScala(i)

    when (flush) {
      indexOrRes(i) := 0.U
      indexDone(i)  := false.B
    }.elsewhen (fillRes(i)) {
      indexOrRes(i) := gatherMod.out.result(i)
      indexDone(i)  := true.B
    }.elsewhen (doPlace) {
      // Place 时：inactive/tail 取 oldTable，active 元素按优先级使用各填充源
      indexOrRes(i) := Mux1H(Seq(
        fillScala(i) -> scalaTableBytes(i % (xlen / MinDataWidth)),
        fillZeros(i) -> 0.U,
        !in.place.bits.indexValid(i) -> in.oldTable(i),
        fillIdx(i)   -> in.place.bits.index(i),
        // 默认：prefillData（active 元素 notfound 时的暂存 index）
        true.B       -> in.place.bits.prefillData(i),
      ))
      indexDone(i) := false.B
    }.elsewhen (counter === 0.U && doFlow) {
      // 完成：indexDone 标记，不再更新
      indexDone(i) := true.B
    }
  }

  // ── Writeback ───────────────────────────────────────
  // counter==0 时所有 table 已流过，可写回
  // 不依赖 out.ready
  out.writeback.valid := hasIndex && counter === 0.U
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
