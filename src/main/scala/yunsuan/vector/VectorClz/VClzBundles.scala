package yunsuan.vector.VectorClz

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode._


class VClzOpcode extends Bundle {
  val op = UInt(2.W)
  def isClz = op === VclzOpcode.vclz
  def isCtz = op === VclzOpcode.vctz
  def isFirst = op === VclzOpcode.vfirst
}
