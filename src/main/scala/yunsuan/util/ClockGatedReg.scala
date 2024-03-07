package yunsuan.util

import chisel3._
import chisel3.util._

object GatedValidRegNext {
  def apply(next: Bool, init: Bool = false.B): Bool = {
    val last = Wire(next.cloneType)
    last := RegEnable(next, init, next || last)
    last
  }
}