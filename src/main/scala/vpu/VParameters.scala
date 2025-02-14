package race.vpu
import chisel3._
import chisel3.util._

object VParams {
  val xLen = 64
  val XLEN = xLen
  val VRobSize = 192

  val VLEN = 2048  // Must be power of 2
  val bVL = log2Up(VLEN) + 1
  val bVstart = bVL - 1
}
