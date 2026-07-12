package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util.log2Ceil

// 块配置：每个 Unit 需要知道的块信息
class BlockConfig(val numUnits: Int) extends Bundle {
  val blockBase = UInt(log2Ceil(numUnits).W)           // 块起始位置 (0..7)
  val blockSize = UInt(log2Ceil(numUnits + 1).W)       // 块大小 (1/2/4/8)，需多 1 位表示 8
}

// Table 携带的控制标记
class TableTag extends Bundle {
  val isFirst     = Bool()  // 首个 table（填充阶段 will_flow 追踪）
  val passedEntry = Bool()  // 已过入口单元（满配阶段遍历判断）
}

// Ring 内 flowing 的 table 数据包
// valid 在 Bundle 内部——它是随 table 逐拍传递的数据流控制信号，不是接口握手
class RingTable(val numElem: Int, val tableIdxWidth: Int) extends Bundle {
  val data  = Vec(numElem, UInt(8.W))          // table 数据（128b = 16×8b）
  val valid = Bool()                            // 有效位
  val idx   = UInt(tableIdxWidth.W)            // table segment 编号
  val tag   = new TableTag                      // 控制标记
}

// 每 Unit 的状态输出（用于 E_table/E_index 空位检测）
class UnitStatus extends Bundle {
  val hasTable = Bool()
  val hasIndex = Bool()
}

// Table 放置请求（仅 table 相关字段，发往 tPos Unit）
class PlaceTableRequest(val numElem: Int, val numUnits: Int, val tableIdxWidth: Int) extends Bundle {
  val blockBase  = UInt(log2Ceil(numUnits).W)   // 块起始位置
  val tableIdx   = UInt(tableIdxWidth.W)         // 此 UOP 携带的 vs2 segment 编号
  val table      = Vec(numElem, UInt(8.W))       // table 数据
  val isFirstTable = Bool()                      // 此 UOP 的 table 是否为块内首个
}

// Index 放置请求（index / fill / prefillData，发往 iPos Unit）
class PlaceIndexRequest(val numElem: Int, val indexWidth: Int, val numUnits: Int) extends Bundle {
  val blockBase  = UInt(log2Ceil(numUnits).W)   // 块起始位置
  val index      = Vec(numElem, UInt(indexWidth.W))  // 预计算的 index bundle
  val indexValid = Vec(numElem, Bool())          // 逐元素是否需查找
  val fillZeros  = Vec(numElem, Bool())          // 越界填 0
  val fillScala  = Vec(numElem, Bool())          // slide1 边界填 rs1
  val prefillData = Vec(numElem, UInt(indexWidth.W)) // mask/tail 打底数据
}

// 每 pipe 的输入（已过 IndexGen + Mask 预处理）
class GatherRingPipeInput(val numElem: Int, val indexWidth: Int, val numUnits: Int, val tableIdxWidth: Int) extends Bundle {
  val placeTable = new PlaceTableRequest(numElem, numUnits, tableIdxWidth)
  val placeIndex = new PlaceIndexRequest(numElem, indexWidth, numUnits)
  val placeIdx   = UInt(log2Ceil(numUnits).W)  // 外部预计算的放置序号
}
