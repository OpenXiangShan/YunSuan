package yunsuan.vector.perm

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

object VPermOpcode {
  val vslideup    = 0.U(6.W)
  val vslidedown  = 1.U(6.W)
  val vslide1up   = 2.U(6.W)
  val vslide1down = 3.U(6.W)
  val vrgather    = 4.U(6.W)
  val vrgather_vx = 5.U(6.W)
  val vcompress   = 6.U(6.W)
  val vmvnr       = 7.U(6.W)
}

