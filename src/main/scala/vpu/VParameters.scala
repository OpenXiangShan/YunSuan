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

  val VIQSize = 32
  val VSbSize = 32 // Scoreboard (Only 32 logical regs, suppose there is no WAR hazard)
  
  val nVRFWritePorts = 2
  val bNVRFWritePorts = log2Up(nVRFWritePorts) 

  val LaneWidth = 64  // constant
  val NLanes = VLEN / LaneWidth

  val bMaxFuDelay = 3 // Exclude long-latency (div) operations
}

