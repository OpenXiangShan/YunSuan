package race.vpu.yunsuan.util

import chisel3._
import chisel3.util._

object LZD {
  def apply(in: UInt): UInt = PriorityEncoder(Reverse(Cat(in, 1.U)))
}

object GatedValidRegNext {
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = Wire(next.cloneType)
    last := RegEnable(next, init, next || last)
    last
  }
}