package yunsuan.util

import chisel3._
import chisel3.util._

object Pipeline {
  /*
   * Hold the data when it is valid and bypass latest data
   */
  object DataHoldBypass {
    def apply[T <: Data](data: T, valid: Bool): T = {
      Mux(valid, data, RegEnable(data, valid))
    }

    def apply[T <: Data](data: T, init: T, valid: Bool): T = {
      Mux(valid, data, RegEnable(data, init, valid))
    }
  }
}
