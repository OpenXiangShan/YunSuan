package yunsuan.vector.v2.Shuffle

import chisel3.util.log2Ceil

trait GatherConfig {
  val vlen: Int
  val dlen: Int
  def MaxLMUL: Int = 8
  def MinDataWidth = 8
  def NumElem = dlen / MinDataWidth
  def IndexWidthInDLEN = log2Ceil(NumElem)
  def IndexWidth = log2Ceil(vlen * MaxLMUL / MinDataWidth)
  def MaxTableNum = vlen / dlen * MaxLMUL
  def TableIdxWidth = log2Ceil(MaxTableNum)
  def VlWidth = IndexWidth + 1
  def vlenb = vlen / 8
  def dlenb = dlen / 8
}
