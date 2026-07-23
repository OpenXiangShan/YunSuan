package yunsuan.vector.VectorConvert.util

import chisel3._
import chisel3.util._

/** in => shift | collect sticky bit => {in_shifted, sticky}
  */
class ShiftRightJam(val len: Int, val shamtWidth: Int) extends Module {
  val max_shift_width = log2Up(len + 1)
  require(shamtWidth >= max_shift_width)
  val io = IO(new Bundle() {
    val in = Input(UInt(len.W))
    val shamt = Input(UInt(shamtWidth.W))
    val out = Output(UInt(len.W))
    val sticky = Output(Bool())
  })
  val exceed_max_shift = io.shamt > len.U
  val shamt = io.shamt(max_shift_width - 1, 0)
  val sticky_mask =
    ((1.U << shamt).asUInt - 1.U)(len - 1, 0) | Fill(len, exceed_max_shift)
  io.out := Mux(exceed_max_shift, 0.U, io.in >> shamt)
  io.sticky := (io.in & sticky_mask).orR
}

object ShiftRightJam {
  def apply(x: UInt, shamt: UInt): (UInt, Bool) = {
    val maxShiftWidth = log2Up(x.getWidth + 1)
    val reducedShamt =
      if (shamt.getWidth <= maxShiftWidth + 1) {
        shamt
      } else {
        Mux(
          shamt(shamt.getWidth - 1, maxShiftWidth + 1).orR,
          (x.getWidth + 1).U((maxShiftWidth + 1).W),
          shamt(maxShiftWidth, 0)
        )
      }
    val shiftRightJam = Module(
      new ShiftRightJam(x.getWidth, maxShiftWidth + 1)
    )
    shiftRightJam.io.in := x
    shiftRightJam.io.shamt := reducedShamt
    (shiftRightJam.io.out, shiftRightJam.io.sticky)
  }
}
