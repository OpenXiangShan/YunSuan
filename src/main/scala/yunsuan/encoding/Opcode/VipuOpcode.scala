package yunsuan.encoding.Opcode

import chisel3._

class VipuOpcode {
  def width = 6

  def vredsum = 33.U(width.W)
  def vredmax = 34.U(width.W)
  def vredmin = 35.U(width.W)
  def vredand = 36.U(width.W)
  def vredor  = 37.U(width.W)
  def vredxor = 38.U(width.W)
  def vcpop   = 39.U(width.W)
  def vfirst  = 40.U(width.W)
  def vmsbf   = 41.U(width.W)
  def vmsif   = 42.U(width.W)
  def vmsof   = 43.U(width.W)
  def viota   = 44.U(width.W)
  def vid     = 45.U(width.W)
}
