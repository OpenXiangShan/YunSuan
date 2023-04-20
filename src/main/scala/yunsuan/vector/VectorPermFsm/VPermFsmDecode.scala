package yunsuan.vector.permfsm

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

object VPermFsmOpcode {
  val vslideup    = 0.U(3.W)  
  val vslidedn    = 1.U(3.W)
  val vrgather    = 2.U(3.W)
  val vrgather_vx = 3.U(3.W)
  val vrgather16  = 4.U(3.W)
  val vcompress   = 5.U(3.W)
  val dummy       = 7.U(3.W)
}

