package yunsuan.util

import chisel3._
import chisel3.util._
import yunsuan.util.Utils.{NOnes, NZeros}

class UIntToContLow1s(uintWidth: Int) extends Module {
  private val outWidth = (1 << uintWidth) - 1 // 2^n - 1

  val io = IO(new Bundle {
    val dataIn = Input(UInt(uintWidth.W))
    val dataOut = Output(UInt(outWidth.W))
  })

  io.dataOut := helper(io.dataIn)

  private def helper(data: UInt): UInt = data.getWidth match {
    case 1 => Mux(data(0), 1.U(1.W), 0.U(1.W))
    case w => Mux(
      data(w - 1),
      Cat(helper(data(w - 2, 0)), NOnes(1 << (w - 1))),
      Cat(NZeros(1 << (w - 1)), helper(data(w - 2, 0)))
    )
  }
}

object UIntToContLow1s {
  def apply(uint: UInt): UInt = apply(uint, uint.getWidth)

  def apply(uint: UInt, width: Int): UInt = {
    val uintToContTail1sMod = Module(new UIntToContLow1s(uint.getWidth)).suggestName(s"uintToContTail1sMod${width}Bits")
    uintToContTail1sMod.io.dataIn := uint
    val dataOutWidth = uintToContTail1sMod.io.dataOut.getWidth
    if (width <= dataOutWidth) {
      uintToContTail1sMod.io.dataOut(width - 1, 0)
    } else {
      Cat(0.U((width - dataOutWidth).W), uintToContTail1sMod.io.dataOut)
    }
  }
}
