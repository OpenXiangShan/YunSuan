/**
  * Integer multiplier to perform:
  *   Two 12*12 UInt multiplications, OR one 24*24 UInt multiplication
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._

class IntMUL_12_24_dummy extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val uop_in = Input(new VUop)
    val a_in = Input(UInt(24.W))
    val b_in = Input(UInt(24.W))
    val is_16 = Input(Bool())
    val valid_out = Output(Bool())
    val uop_out = Output(new VUop)
    val res_out = Output(UInt(48.W))
  })

  val res_16_high, res_16_low = Wire(UInt(24.W))
  val res_32 = Wire(UInt(48.W))
  res_16_high := io.a_in(23, 12) * io.b_in(23, 12)
  res_16_low := io.a_in(11, 0) * io.b_in(11, 0)
  res_32 := io.a_in * io.b_in

  io.valid_out := RegNext(io.valid_in)
  io.uop_out := RegEnable(io.uop_in, io.valid_in)


  val res_out = Mux(!io.is_16, res_32, Cat(res_16_high, res_16_low))
  io.res_out := RegEnable(res_out, io.valid_in)
}