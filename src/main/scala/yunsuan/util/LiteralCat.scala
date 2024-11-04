package yunsuan.util

import chisel3._

object LiteralCat {
  def apply(dataVec: UInt*): UInt = {
    var res = BigInt(0)
    var width = 0
    for (data <- dataVec) {
      res <<= data.getWidth
      res |= data.litValue
      width += data.getWidth
    }
    res.U(width.W)
  }
}
