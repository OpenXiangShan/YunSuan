package yunsuan.encoding.Opcode

import chisel3._

object VimacOpcode {
  def width = 3
  def vmul    = "b000".U(width.W)
  def vmulh   = "b001".U(width.W)
  def vmacc   = "b010".U(width.W)
  def vnmsac  = "b011".U(width.W)
  def vmadd   = "b100".U(width.W)
  def vnmsub  = "b101".U(width.W)
  def vsmul   = "b110".U(width.W)

  def highHalf(opcode: UInt) = Seq(vmulh).map(_ === opcode).reduce(_ || _)

  def isMacc(opcode: UInt) = Seq(vmacc, vnmsac, vmadd, vnmsub).map(_ === opcode).reduce(_ || _)

  def isSub(opcode: UInt) = Seq(vnmsac, vnmsub).map(_ === opcode).reduce(_ || _)

  def isFixP(opcode: UInt) = Seq(vsmul).map(_ === opcode).reduce(_ || _)

  def overWriteMultiplicand(opcode: UInt) = Seq(vmadd, vnmsub).map(_ === opcode).reduce(_ || _)
}
