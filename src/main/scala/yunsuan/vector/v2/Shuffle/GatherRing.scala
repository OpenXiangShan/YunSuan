package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._

class GatherRing(
  val vlen: Int,
  val dlen: Int,
  xlen: Int = 64,
) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b"

  val numPipes = 4
  val numUnits = MaxTableNum

  // ── IO ──────────────────────────────────────────────
  // Flush（选择性清空 robIdx 新于 flushRobIdx 的块）
  val flushRobIdx = IO(Input(UInt(8.W)))

  // 块大小配置（从 Issue Queue / VOQ 传入）
  val blockSizeCfg = IO(Input(UInt(log2Ceil(numUnits + 1).W)))

  // 多 pipe 输入（每 pipe 已过 IndexGen+Mask 预处理）
  val pipeIn = IO(Vec(numPipes, Flipped(ValidIO(new GatherRingPipeInput(NumElem, IndexWidth, MaxTableNum, TableIdxWidth)))))

  // 写回总线（≤4 条）
  val writeback = IO(Vec(numPipes, DecoupledIO(new Bundle {
    val dest = UInt(dlen.W)
  })))

  // 状态上报（给 VOQ/wakeup 做空位检测和 Gn 更新）
  val unitStatus = IO(Output(Vec(numUnits, new UnitStatus)))

  // ── 实例化 GatherRingUnit ───────────────────────────
  private val units = Seq.tabulate(numUnits) { i =>
    val u = Module(new GatherRingUnit(
      vlen = vlen, dlen = dlen,
      maxTableNum = numUnits,
      xlen = xlen,
      unitIdx = i,
    ))
    u.suggestName(s"ringUnit_$i")
    u
  }

  // ── 每 Unit 的 block 配置 ───────────────────────────
  for (i <- 0 until numUnits) {
    units(i).in.blockConfig.blockBase := 0.U
    units(i).in.blockConfig.blockSize := blockSizeCfg
  }

  // ── 回环源预计算（每 Unit 的 3 选 1 MUX 源）─────────
  // LMUL=2: 块内配对单元的另一端
  // LMUL=4: 块内四元组的最后一元
  // LMUL=8: 全局环尾 (U7)
  def loopbackLmul2(i: Int): Int  = (i / 2) * 2 + (1 - i % 2)  // 0↔1, 2↔3, 4↔5, 6↔7
  def loopbackLmul4(i: Int): Int  = (i / 4) * 4 + 3             // 0→3, 1→3, 2→3, 3→3, 4→7, ...
  def loopbackLmul8(i: Int): Int  = numUnits - 1                 // all → 7

  // ── 环形连线：顶层 MUX 选前驱或回环 ─────────────────
  for (i <- 0 until numUnits) {
    val prevIdx = (i - 1 + numUnits) % numUnits
    val srcLmul2 = units(loopbackLmul2(i)).out.tableOut
    val srcLmul4 = units(loopbackLmul4(i)).out.tableOut
    val srcLmul8 = units(loopbackLmul8(i)).out.tableOut

    // blockSize 决定回环源（3 选 1），非入口 Unit 走物理前驱
    val blockWrapSrc = Mux1H(Seq(
      (units(i).in.blockConfig.blockSize === 2.U) -> srcLmul2,
      (units(i).in.blockConfig.blockSize === 4.U) -> srcLmul4,
      (units(i).in.blockConfig.blockSize === 8.U) -> srcLmul8,
    ))
    val isEntry = (i.U - units(i).in.blockConfig.blockBase + numUnits.U) % numUnits.U === 0.U
    units(i).in.flowIn := Mux(isEntry, blockWrapSrc, units(prevIdx).out.tableOut)
  }

  // ── E_table / E_index 优先编码器（7 组 × 2）─────────
  // 按 LMUL 粒度分组：1 组 LMUL=8 + 2 组 LMUL=4 + 4 组 LMUL=2
  // E_table: 逆序搜索（0, N-1, N-2, ..., 2, 1）
  // E_index: 顺序搜索（0, 1, 2, ..., N-1）

  def priorityEncodeReverse(statuses: Seq[Bool]): UInt = {
    val n = statuses.size
    val searchOrder = 0 +: ((n - 1) to 1 by -1)
    PriorityMux(searchOrder.map { idx =>
      (!statuses(idx)) -> idx.U(log2Ceil(n).W)
    })
  }

  def priorityEncodeForward(statuses: Seq[Bool]): UInt = {
    PriorityMux(statuses.zipWithIndex.map { case (s, i) =>
      (!s) -> i.U(log2Ceil(statuses.size).W)
    })
  }

  // ── will_flow 两阶段计算 ──────────────────────────
  private val willFlow = RegInit(false.B)

  // 信号向量化
  private val hasIndexVec    = VecInit(units.map(_.out.status.hasIndex))
  private val hasTableVec    = VecInit(units.map(_.out.status.hasTable))
  private val isFirstVec     = VecInit(units.map(_.out.tableOut.tag.isFirst))
  private val tableValidVec  = VecInit(units.map(_.out.tableOut.valid))
  private val passedEntryVec = VecInit(units.map(_.out.tableOut.tag.passedEntry))

  // ── E_table / E_index 寄存器（Q25：从 hasTable/hasIndex 状态计算，寄存一拍）──
  // 寄存器值在同周期内稳定，多 pipe 用同一 base 值 + placeIdx 偏移
  private val hasTableInBlock = VecInit((0 until numUnits).map { j =>
    Mux(j.U < blockSizeCfg, hasTableVec(j), true.B)
  })
  private val hasIndexInBlock = VecInit((0 until numUnits).map { j =>
    Mux(j.U < blockSizeCfg, hasIndexVec(j), true.B)
  })
  private val E_table_reg = RegInit(0.U(log2Ceil(numUnits).W))
  private val E_index_reg = RegInit(0.U(log2Ceil(numUnits).W))
  E_table_reg := priorityEncodeReverse(hasTableInBlock)
  E_index_reg := priorityEncodeForward(hasIndexInBlock)

  // Phase detection：环尾 Unit 的 hasIndex=true → 满配阶段
  private val isFullPhase = Mux1H(Seq(
    (blockSizeCfg === 1.U) -> hasIndexVec(0),
    (blockSizeCfg === 2.U) -> hasIndexVec(1),
    (blockSizeCfg === 4.U) -> hasIndexVec(3),
    (blockSizeCfg === 8.U) -> hasIndexVec(7),
  ))

  // ── 填充阶段 will_flow ─────────────────────────────
  // firstTablePos：块内首个 table 的局部坐标（PriorityMux 从低到高扫描）
  private val firstTablePos = PriorityMux(
    (0 until numUnits).map { j =>
      (j.U < blockSizeCfg && isFirstVec(j)) -> j.U(log2Ceil(numUnits).W)
    } :+ (true.B -> 0.U(log2Ceil(numUnits).W))
  )

  // lastValidIdxPos：块内最后 valid index 的局部坐标（PriorityMux 从高到低扫描）
  private val lastValidIdxPos = PriorityMux(
    (0 until numUnits).reverse.map { j =>
      (j.U < blockSizeCfg && hasIndexVec(j)) -> j.U(log2Ceil(numUnits).W)
    } :+ (true.B -> 0.U(log2Ceil(numUnits).W))
  )

  private val anyHasIndexInBlock = VecInit(
    (0 until numUnits).map(j => j.U < blockSizeCfg && hasIndexVec(j))
  ).asUInt.orR

  private val fillingWillFlow = anyHasIndexInBlock && (firstTablePos =/= lastValidIdxPos)

  // ── 满配阶段 will_flow ─────────────────────────────
  // 任一 table：valid && 未到环尾 && (已过入口 || 当前在入口)
  private val fullWillFlow = VecInit(
    (0 until numUnits).map { j =>
      j.U < blockSizeCfg &&
        tableValidVec(j) &&
        (passedEntryVec(j) || j.U === 0.U) &&
        j.U =/= (blockSizeCfg - 1.U)
    }
  ).asUInt.orR

  willFlow := Mux(isFullPhase, fullWillFlow, fillingWillFlow)

  for (i <- 0 until numUnits) {
    units(i).in.willFlow := willFlow
  }

  // ── placeIdx 计算（同 robIdx 组内 pipe 顺序计数）────
  // 已移到 GatherRing 外部（Issue Queue 阶段计算），通过 pipeIn.bits.placeIdx 传入
  // 此处直接使用外部预计算的值

  // ── 放置位置计算 + 分发到 Unit ──────────────────────
  // 先清零所有 place 信号，再聚合有效 pipe 的放置
  for (u <- 0 until numUnits) {
    units(u).in.place.valid := false.B
    units(u).in.place.bits   := 0.U.asTypeOf(units(u).in.place.bits.cloneType)
  }

  for (p <- 0 until numPipes) {
    when (pipeIn(p).valid) {
      val req  = pipeIn(p).bits.placeReq
      val pIdx = pipeIn(p).bits.placeIdx
      val blkBase = req.blockBase
      val blkSize = blockSizeCfg
      val flow = Mux(willFlow, 1.U, 0.U)

      val tPos = (E_table_reg + 1.U - pIdx + flow) % blkSize + blkBase
      val iPos = (E_index_reg + pIdx) % blkSize + blkBase

      // 标记首个 table（placeIdx==0 的 UOP 为块内第一个 table）
      val reqWire = WireInit(req)
      reqWire.isFirstTable := false.B  // default false，仅 table 目标置 true

      // 1-hot 解码，分离 table 和 index 目标
      for (u <- 0 until numUnits) {
        val isTableTgt = tPos === u.U
        val isIndexTgt = iPos === u.U
        when (isTableTgt || isIndexTgt) {
          units(u).in.place.valid := true.B
          units(u).in.place.bits := reqWire
          units(u).in.place.bits.isTableTarget := isTableTgt
          units(u).in.place.bits.isIndexTarget := isIndexTgt
          when (isTableTgt && pIdx === 0.U) {
            units(u).in.place.bits.isFirstTable := true.B
          }
        }
      }
    }
  }

  // ── 辅助输入 ────────────────────────────────────────
  for (i <- 0 until numUnits) {
    units(i).in.scalaTable := 0.U  // TODO: 从 pipe 传入
    units(i).in.oldTable   := 0.U.asTypeOf(Vec(NumElem, UInt(MinDataWidth.W)))  // TODO: vs3 数据
    units(i).flush := false.B      // TODO: flush 逻辑
  }

  // ── 写回仲裁（集中式，≤4 总线，单元序号小的优先）────
  // 收集所有可写回的 Unit
  private val wbValid = VecInit(units.map(_.out.writeback.valid))
  private val wbBits  = VecInit(units.map(_.out.writeback.bits))

  for (p <- 0 until numPipes) {
    // 优先级编码：找第一个有效写回
    val sel = PriorityEncoderOH(wbValid)
    writeback(p).valid := VecInit(sel).asUInt.orR
    writeback(p).bits  := Mux1H(sel, wbBits)

    // 屏蔽已被选中的（简化版：仅屏蔽最低位）
    // TODO: 完整的多总线仲裁
  }

  // 默认 ready
  for (p <- 0 until numPipes) {
    for (i <- 0 until numUnits) {
      units(i).out.writeback.ready := false.B
    }
  }
  // 简化：只连第一个写回总线
  when (writeback(0).ready) {
    val sel = PriorityEncoderOH(wbValid)
    for (i <- 0 until numUnits) {
      units(i).out.writeback.ready := sel(i)
    }
  }

  // ── 状态上报 ────────────────────────────────────────
  for (i <- 0 until numUnits) {
    unitStatus(i) := units(i).out.status
  }
}

object GatherRingMain extends App {
  import _root_.circt.stage._

  println("Generating the GatherRing hardware")

  val firtoolOpts = Array(
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherRing(vlen = 128, dlen = 128, xlen = 64)) +: firtoolAnno
  )

  println("done")
}
