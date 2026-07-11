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
  // 暂用缺省配置（全阵列单指令模式），后续由 VOQ 动态分配时广播
  // 当前实现：所有 Unit 默认 block_base=0, block_size=numUnits
  for (i <- 0 until numUnits) {
    units(i).in.blockConfig.blockBase := 0.U
    units(i).in.blockConfig.blockSize := numUnits.U
  }

  // ── 回环源预计算（每 Unit 的 3 选 1 MUX 源）─────────
  // LMUL=2: 块内配对单元的另一端
  // LMUL=4: 块内四元组的最后一元
  // LMUL=8: 全局环尾 (U7)
  def loopbackLmul2(i: Int): Int  = (i / 2) * 2 + (1 - i % 2)  // 0↔1, 2↔3, 4↔5, 6↔7
  def loopbackLmul4(i: Int): Int  = (i / 4) * 4 + 3             // 0→3, 1→3, 2→3, 3→3, 4→7, ...
  def loopbackLmul8(i: Int): Int  = numUnits - 1                 // all → 7

  // ── 环形 + 回环连线 ─────────────────────────────────
  for (i <- 0 until numUnits) {
    // 物理前驱（线性链，U(N-1) 的前驱是 U(N-2)，U0 的前驱是 U(N-1)）
    val prevIdx = (i - 1 + numUnits) % numUnits
    units(i).in.flowPrev := units(prevIdx).out.tableOut

    // 回环源（3 选 1，按 LMUL 选择）
    val srcLmul2 = units(loopbackLmul2(i)).out.tableOut
    val srcLmul4 = units(loopbackLmul4(i)).out.tableOut
    val srcLmul8 = units(loopbackLmul8(i)).out.tableOut

    // 暂用 LMUL=8（全阵列），后续根据 blockConfig.blockSize 动态选择
    units(i).in.flowWrap := srcLmul8
  }

  // ── E_table / E_index 优先编码器（7 组 × 2）─────────
  // 按 LMUL 粒度分组：1 组 LMUL=8 + 2 组 LMUL=4 + 4 组 LMUL=2
  // E_table: 逆序搜索（0, N-1, N-2, ..., 2, 1）
  // E_index: 顺序搜索（0, 1, 2, ..., N-1）

  def priorityEncodeReverse(statuses: Seq[Bool]): UInt = {
    // 返回逆序第一个为 false 的索引（即第一个空位）
    // 搜索顺序: 0, N-1, N-2, ..., 2, 1
    val n = statuses.size
    val searchOrder = 0 +: ((n - 1) to 1 by -1)
    PriorityMux(searchOrder.map { idx =>
      (!statuses(idx)) -> idx.U(log2Ceil(n).W)
    })
  }

  def priorityEncodeForward(statuses: Seq[Bool]): UInt = {
    // 返回顺序第一个为 false 的索引
    PriorityMux(statuses.zipWithIndex.map { case (s, i) =>
      (!s) -> i.U(log2Ceil(statuses.size).W)
    })
  }

  // ── will_flow 计算 ──────────────────────────────────
  // 填充阶段：首个 table (isFirst) 未到达最后 valid index
  // 满配阶段：任一 table passedEntry 且未到环尾
  // 暂用简化的全局 will_flow（需后续按分块细化）
  private val willFlow = RegInit(false.B)

  // 简化版：有 table 在环内就流动
  // 后续替换为两阶段逻辑
  private val anyHasTable = Cat(units.map(_.out.status.hasTable)).orR
  private val anyHasIndex = Cat(units.map(_.out.status.hasIndex)).orR
  when (anyHasTable && anyHasIndex) {
    willFlow := true.B
  }.otherwise {
    willFlow := false.B
  }

  for (i <- 0 until numUnits) {
    units(i).in.willFlow := willFlow
  }

  // ── placeIdx 计算（同 robIdx 组内 pipe 顺序计数）────
  // 对每个 pipe，统计 0..i-1 pipe 中同 robIdx 的 UOP 个数
  private val placeIdx = Wire(Vec(numPipes, UInt(log2Ceil(numUnits).W)))
  for (i <- 0 until numPipes) {
    val sameRobIdx = Wire(Vec(i, Bool()))
    for (j <- 0 until i) {
      sameRobIdx(j) := pipeIn(j).valid && pipeIn(i).valid &&
        pipeIn(j).bits.placeReq.robIdx === pipeIn(i).bits.placeReq.robIdx
    }
    placeIdx(i) := PopCount(sameRobIdx)
  }

  // ── 放置位置计算 + 分发到 Unit ──────────────────────
  // tablePos = (E_table - placeIdx + flow) % blockSize + blockBase
  // indexPos = E_index + placeIdx + blockBase
  // 暂用简化版
  for (p <- 0 until numPipes) {
    when (pipeIn(p).valid) {
      val req = pipeIn(p).bits.placeReq
      val blkBase = req.blockBase
      val blkSize = numUnits.U  // TODO: 从 VOQ 获取实际 blockSize
      val flow = Mux(willFlow, 1.U, 0.U)

      // 简化版：假设全空状态，E_table=E_index=0
      val tPos = (0.U - placeIdx(p) + flow) % blkSize + blkBase
      val iPos = (0.U + placeIdx(p)) % blkSize + blkBase

      // 1-hot 解码，将 place 分发到目标 Unit
      for (u <- 0 until numUnits) {
        val isTarget = tPos === u.U || iPos === u.U
        when (isTarget) {
          units(u).in.place.valid := true.B
          units(u).in.place.bits := req
        }.otherwise {
          units(u).in.place.valid := false.B
          units(u).in.place.bits := 0.U.asTypeOf(req.cloneType)
        }
      }
    }.otherwise {
      for (u <- 0 until numUnits) {
        units(u).in.place.valid := false.B
        units(u).in.place.bits := 0.U.asTypeOf(pipeIn(p).bits.placeReq.cloneType)
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
