package yunsuan.encoding.Opcode

import chisel3._

object VmoveOpcode {
  def width = 4
  def vmerge_vvm = "b0000".U(width.W)
  def vfmerge  = "b0001".U(width.W)
  def vmv_v_v  = "b0010".U(width.W)
  def vfmv     = "b0011".U(width.W)
  def vmv_x_s  = "b0100".U(width.W)
  def vmv_s_x  = "b0101".U(width.W)
  def vfmv_f_s = "b0110".U(width.W)
  def vfmv_s_f = "b0111".U(width.W)
  def vmv1r    = "b1000".U(width.W)
  def vmv2r    = "b1001".U(width.W)
  def vmv4r    = "b1010".U(width.W)
  def vmv8r    = "b1011".U(width.W)
}
