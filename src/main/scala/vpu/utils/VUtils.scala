package race.vpu

import chisel3._
import chisel3.util._
import VParams._

object Vlmul_to_lmul {
// vlmul --> LMUL --> max(LMUL, 1)
// Note: set result to 1 if LMUL < 1
  def apply(vlmul: UInt): UInt = {
    val y0 = !vlmul(1) && !vlmul(0) || vlmul(2)
    val y1 = !vlmul(2) && !vlmul(1) && vlmul(0)
    val y2 = !vlmul(2) && vlmul(1) && !vlmul(0)
    val y3 = !vlmul(2) && vlmul(1) && vlmul(0)
    Cat(y3, y2, y1, y0)
  }
}

object UIntSplit {
  // Split a UInt(VLEN.W) into lanes
  def vlen_splitTo_lanes(data: UInt): Seq[UInt] = {
    Seq.tabulate(NLanes)(i => data(LaneWidth*i+LaneWidth-1, LaneWidth*i))
  }

  def apply(data: UInt, width: Int): Seq[UInt] = {
    Seq.tabulate(data.getWidth / width)(i => data(width*i+width-1, width*i))
  }
}