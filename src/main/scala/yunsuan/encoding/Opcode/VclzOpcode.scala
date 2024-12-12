package yunsuan.encoding.Opcode

import chisel3._

object VclzOpcode {
  def width = 2

  def vclz = 1.U(width.W)
  def vctz = 2.U(width.W)
  def vfirst = 3.U(width.W)
}
