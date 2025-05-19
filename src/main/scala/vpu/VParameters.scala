package race.vpu
import chisel3._
import chisel3.util._

object VParams {
  val xLen = 64
  val XLEN = xLen
  val FLEN = 64
  val VRobSize = 192

  val VLEN = 1024  // Must be power of 2
  val vlenb = VLEN/8
  val bVL = log2Up(VLEN) + 1
  val bVstart = bVL - 1

  val VIQSize = 32
  val VSbSize = 32 // Scoreboard (Only 32 logical regs, suppose there is no WAR hazard)
  
  val nVRFWritePortsExu = 1  // Number of write-back ports of EXU
  val nVRFWritePorts = nVRFWritePortsExu + 1 // Load uses one port
  val bNVRFWritePortsExu = log2Up(nVRFWritePortsExu) 

  val LaneWidth = 64  // constant
  val NByteLane = LaneWidth / 8
  val NLanes = VLEN / LaneWidth

  // Execution delays
  val bMaxFuDelay = 4 // Exclude long-latency (div) operations
  val issueDelay = 1  // Read RF
  val wbDelay = 0  // Write back to RF
  val delayBias = issueDelay + wbDelay
  require(delayBias >= 1)
  // Concrete execution delays
  val aluDelay = 1 + delayBias
  val faddDelay = 1 + delayBias
  val fmaDelay = 3 + delayBias
  val fcvtDelay = 2 + delayBias
  val fredFp16Delay = log2Up(VLEN/32) + 2 + delayBias
  val fredFp32Delay = log2Up(VLEN/32) + 1 + delayBias
  val vrgatherDelay = 1 + delayBias

  // Load/Store
  val PAddrBits = XLEN
  // L2 Cache
  val CachelineBits = 512
  val nPortsL2 = VLEN / CachelineBits

  /**
    * Debug (or difftest)
    */
  val debugMode = true
}

