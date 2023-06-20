
package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.vector.perm.VPermOpcode._

class VPermOpcode extends Bundle {
  val op = UInt(6.W)

  def isVslideup = op === vslideup

  def isVslidedown = op === vslidedown

  def isVslide1up = op === vslide1up

  def isVslide1down = op === vslide1down

  def isVrgather = op === vrgather

  def isVrgather_vx = op === vrgather_vx

  def isVcompress = op === vcompress

  def isVmvnr = op === vmvnr
}

class VPermInput extends Bundle {
  val opcode = new VPermOpcode
  val info = new VIFuInfo
  val srcType = Vec(2, UInt(4.W)) // 0: vs2   1: vs1
  val vdType = UInt(4.W)
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val old_vd = UInt(128.W)
  val mask = UInt(128.W)
}

