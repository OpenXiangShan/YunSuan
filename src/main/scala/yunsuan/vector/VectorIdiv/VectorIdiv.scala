package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._


class VectorIdiv extends Module {
  val Vectorwidth = 128
  val io = IO(new Bundle() {
    val sew = Input(UInt(2.W))
    /*
    b00 = I8
    b01 = I16
    b10 = I32
    b11 = I64
     */
    val sign = Input(Bool())
    val dividend_v = Input(UInt(Vectorwidth.W))
    val divisor_v = Input(UInt(Vectorwidth.W))
    val flush = Input(Bool())
    val d_zero = Output(UInt(16.W))

    val div_in_valid = Input(Bool())
    val div_in_ready = Output(Bool())
    val div_out_ready = Input(Bool())
    val div_out_valid = Output(Bool())
    val div_out_q_v = Output(UInt(Vectorwidth.W))
    val div_out_rem_v = Output(UInt(Vectorwidth.W))
  })

  val finish = Wire(Bool())
  val idle :: divide :: output :: Nil = Enum(3)
  val (oh_idle, oh_divide, oh_output) =
    (UIntToOH(idle,3),UIntToOH(divide,3),UIntToOH(output,3))

  // handshake
  val stateReg = RegInit((1<< idle.litValue.toInt).U(3.W))
  val stateNext = WireInit(stateReg)
  val in_handshake = io.div_in_valid & io.div_in_ready
  val out_handshake = io.div_out_valid & io.div_out_ready


  // fsm
  // part 1
  when (io.flush) {
    stateReg := oh_idle
  }.otherwise {
    stateReg := stateNext
  }
  // part 2
  switch(stateReg) {
    is((1<< idle.litValue.toInt).U(3.W)) {
      when (in_handshake) {
        stateNext := oh_divide
      }
    }
    is((1<< divide.litValue.toInt).U(3.W)) {
      when (finish) {
        stateNext := oh_output
      }.otherwise {
        stateNext := oh_divide
      }
    }
    is((1<< output.litValue.toInt).U(3.W)) {
      when (out_handshake) {
        stateNext := oh_idle
      }
    }
  }
  // part 3
  io.div_in_ready := stateReg(idle)
  io.div_out_valid := stateReg(output)

  val x_reg = RegEnable(io.dividend_v, in_handshake)
  val d_reg = RegEnable(io.divisor_v,in_handshake)
  val sign_reg = RegEnable(io.sign, in_handshake)
  val sew_reg = RegEnable(io.sew, in_handshake)
  val sew_hb = UIntToOH(sew_reg, 4)
  val Index_bound = Array(7, 15, 31, 63)

  /*
    I8  : 8*I8DivNr4 4*SRT16Divint(16) : sew = 00 2*SRT16Divint(32) : sew = 00 1*SRT16Divint(64) sew = 00
    I16 : 4*SRT16Divint(16) : sew = 01 4*SRT16Divint(16) : sew = 01 2*SRT16Divint(32) : sew = 01 1*SRT16Divint(64) sew = 01
    I32 : 2*SRT16Divint(32) : sew = 10 4*SRT16Divint(16) : sew = 10 2*SRT16Divint(32) : sew = 10 1*SRT16Divint(64) sew = 10
    I64 : 1*SRT16Divint(64) : sew = 11 4*SRT16Divint(16) : sew = 11 2*SRT16Divint(32) : sew = 11 1*SRT16Divint(64) sew = 11
   */
  // I8
  val divide_8_q_result = Wire(Vec(8,UInt(8.W)))
  val divide_8_rem_result = Wire(Vec(8,UInt(8.W)))
  val divide_8_finish = Wire(Vec(8,Bool()))
  val divide_8_d_zero = Wire(Vec(8,Bool()))
  for (i <-0 until 8) {
    val begin = i * 8
    val end = (i + 1) * 8 - 1
    val divide_8 = Module(new I8DivNr4().suggestName(s"8bit_divide_${i}"))
    divide_8.io.sign := sign_reg
    divide_8.io.flush := io.flush
    divide_8.io.div_in_valid := stateReg(divide)
    val ready = divide_8.io.div_ready // not use
    divide_8.io.dividend := x_reg(end, begin)
    divide_8.io.divisor := d_reg(end, begin)
    divide_8.io.div_out_ready := stateReg(output)
    divide_8_finish(i) := divide_8.io.div_out_valid
    divide_8_q_result(i) := divide_8.io.div_out_q
    divide_8_rem_result(i) := divide_8.io.div_out_rem
    divide_8_d_zero(i) := divide_8.io.d_zero
  }
  // I16
  val divide_16_q_result = Wire(Vec(4,UInt(16.W)))//additional field, storing both I8 and I16 results
  val divide_16_rem_result = Wire(Vec(4,UInt(16.W)))
  val divide_16_finish = Wire(Vec(4,Bool()))
  val divide_16_d_zero = Wire(Vec(4,Bool()))

  for (i <- 0 until 4) {
    val begin_I16 = i * 16
    val end_I16 = (i + 1) * 16 - 1
    val begin_I8 = 64 + i * 8
    val end_I8 = 64 + (i + 1) * 8 - 1
    val divide_16_dividend =
      Mux(sew_hb(0),x_reg(end_I8, begin_I8),
        x_reg(end_I16, begin_I16))
    val divide_16_divisor =
      Mux(sew_hb(0), d_reg(end_I8, begin_I8),
        d_reg(end_I16, begin_I16))
    val divide_16 = Module(new SRT16Divint(16).suggestName(s"16bit_divide_${i}"))
    divide_16.io.sign := sign_reg
    divide_16.io.flush := io.flush
    divide_16.io.sew := sew_reg
    divide_16.io.div_in_valid := stateReg(divide)
    val ready = divide_16.io.div_ready
    divide_16.io.dividend := divide_16_dividend
    divide_16.io.divisor := divide_16_divisor
    divide_16.io.div_out_ready := stateReg(output)
    divide_16_q_result(i) := divide_16.io.div_out_q
    divide_16_rem_result(i) := divide_16.io.div_out_rem
    divide_16_finish(i) := divide_16.io.div_out_valid
    divide_16_d_zero(i) := divide_16.io.d_zero

  }
  val divide_16_I8_q = Cat(divide_16_q_result(3)(Index_bound(0),0), divide_16_q_result(2)(Index_bound(0),0),divide_16_q_result(1)(Index_bound(0),0),divide_16_q_result(0)(Index_bound(0),0))
  val divide_16_I8_rem = Cat(divide_16_rem_result(3)(Index_bound(0),0), divide_16_rem_result(2)(Index_bound(0),0),divide_16_rem_result(1)(Index_bound(0),0),divide_16_rem_result(0)(Index_bound(0),0))
  val divide_16_I16_q = Cat(divide_16_q_result(3)(Index_bound(1),0), divide_16_q_result(2)(Index_bound(1),0),divide_16_q_result(1)(Index_bound(1),0),divide_16_q_result(0)(Index_bound(1),0))
  val divide_16_I16_rem = Cat(divide_16_rem_result(3)(Index_bound(1),0), divide_16_rem_result(2)(Index_bound(1),0),divide_16_rem_result(1)(Index_bound(1),0),divide_16_rem_result(0)(Index_bound(1),0))
  // I32
  val divide_32_q_result = Wire(Vec(2,UInt(32.W)))
  val divide_32_rem_result = Wire(Vec(2,UInt(32.W)))
  val divide_32_finish = Wire(Vec(2,Bool()))
  val divide_32_d_zero = Wire(Vec(2,Bool()))
  for (i <-0 until 2) {
    val begin_I8 = 64 + 32 + i * 8
    val end_I8 = 64 + 32 + (i + 1) * 8 - 1
    val begin_I16 = 64 + i * 16
    val end_I16 = 64 + (i + 1) * 16 - 1
    val begin_I32 = i * 32
    val end_I32 = (i + 1) * 32 - 1
    val divide_32_dividend =
      Mux(sew_hb(0),x_reg(end_I8, begin_I8),
        Mux(sew_hb(1),x_reg(end_I16, begin_I16),
          x_reg(end_I32, begin_I32)))
    val divide_32_divisor =
      Mux(sew_hb(0), d_reg(end_I8, begin_I8),
        Mux(sew_hb(1), d_reg(end_I16, begin_I16),
          d_reg(end_I32, begin_I32)))
    val divide_32 = Module(new SRT16Divint(32).suggestName(s"32bit_divide_${i}"))
    divide_32.io.sign := sign_reg
    divide_32.io.flush := io.flush
    divide_32.io.sew := sew_reg
    divide_32.io.div_in_valid := stateReg(divide)
    val ready = divide_32.io.div_ready
    divide_32.io.dividend := divide_32_dividend
    divide_32.io.divisor := divide_32_divisor
    divide_32.io.div_out_ready := stateReg(output)
    divide_32_q_result(i) := divide_32.io.div_out_q
    divide_32_rem_result(i) := divide_32.io.div_out_rem
    divide_32_finish(i) := divide_32.io.div_out_valid
    divide_32_d_zero(i) := divide_32.io.d_zero
  }
  val divide_32_I8_q = Cat(divide_32_q_result(1)(Index_bound(0),0),divide_32_q_result(0)(Index_bound(0),0))
  val divide_32_I8_rem = Cat(divide_32_rem_result(1)(Index_bound(0),0),divide_32_rem_result(0)(Index_bound(0),0))
  val divide_32_I16_q = Cat(divide_32_q_result(1)(Index_bound(1),0),divide_32_q_result(0)(Index_bound(1),0))
  val divide_32_I16_rem = Cat(divide_32_rem_result(1)(Index_bound(1),0),divide_32_rem_result(0)(Index_bound(1),0))
  val divide_32_I32_q = Cat(divide_32_q_result(1)(Index_bound(2),0),divide_32_q_result(0)(Index_bound(2),0))
  val divide_32_I32_rem = Cat(divide_32_rem_result(1)(Index_bound(2),0),divide_32_rem_result(0)(Index_bound(2),0))
  // I64
  val divide_64_q_result = Wire(Vec(2,UInt(64.W)))
  val divide_64_rem_result = Wire(Vec(2,UInt(64.W)))
  val divide_64_finish = Wire(Vec(2,Bool()))
  val divide_64_d_zero = Wire(Vec(2,Bool()))
  for (i <-0 until 2) {
    val begin_I8 = 64 + 32 + 16 + i * 8
    val end_I8 = 64 + 32 + 16 + (i + 1) * 8 - 1
    val begin_I16 = 64 + 32 + i * 16
    val end_I16 = 64 + 32 + (i + 1) * 16 - 1
    val begin_I32 = 64 + i * 32
    val end_I32 = 64 + (i + 1) * 32 - 1
    val begin_I64 = i * 64
    val end_I64 = (i + 1) * 64 -1
    val divide_64_dividend =
      Mux(sew_hb(0), x_reg(end_I8, begin_I8),
        Mux(sew_hb(1), x_reg(end_I16, begin_I16),
          Mux(sew_hb(2),x_reg(end_I32, begin_I32),
            x_reg(end_I64, begin_I64))))
    val divide_64_divisor =
      Mux(sew_hb(0), d_reg(end_I8, begin_I8),
        Mux(sew_hb(1), d_reg(end_I16, begin_I16),
          Mux(sew_hb(2), d_reg(end_I32, begin_I32),
            d_reg(end_I64, begin_I64))))
    val divide_64 = Module(new SRT16Divint(64).suggestName(s"64bit_divide_${i}"))
    divide_64.io.sign := sign_reg
    divide_64.io.flush := io.flush
    divide_64.io.sew := sew_reg
    divide_64.io.div_in_valid := stateReg(divide)
    val ready = divide_64.io.div_ready
    divide_64.io.dividend := divide_64_dividend
    divide_64.io.divisor := divide_64_divisor
    divide_64.io.div_out_ready := stateReg(output)
    divide_64_q_result(i) := divide_64.io.div_out_q
    divide_64_rem_result(i) := divide_64.io.div_out_rem
    divide_64_finish(i) := divide_64.io.div_out_valid
    divide_64_d_zero(i) := divide_64.io.d_zero
  }
  val divide_64_I8_q = Cat(divide_64_q_result(1)(Index_bound(0),0),divide_64_q_result(0)(Index_bound(0),0))
  val divide_64_I8_rem = Cat(divide_64_rem_result(1)(Index_bound(0),0),divide_64_rem_result(0)(Index_bound(0),0))
  val divide_64_I16_q = Cat(divide_64_q_result(1)(Index_bound(1),0),divide_64_q_result(0)(Index_bound(1),0))
  val divide_64_I16_rem = Cat(divide_64_rem_result(1)(Index_bound(1),0),divide_64_rem_result(0)(Index_bound(1),0))
  val divide_64_I32_q = Cat(divide_64_q_result(1)(Index_bound(2),0),divide_64_q_result(0)(Index_bound(2),0))
  val divide_64_I32_rem = Cat(divide_64_rem_result(1)(Index_bound(2),0),divide_64_rem_result(0)(Index_bound(2),0))
  val divide_64_I64_q = Cat(divide_64_q_result(1)(Index_bound(3),0),divide_64_q_result(0)(Index_bound(3),0))
  val divide_64_I64_rem = Cat(divide_64_rem_result(1)(Index_bound(3),0),divide_64_rem_result(0)(Index_bound(3),0))



  val div_out_q_result = Wire(UInt(Vectorwidth.W))
  val div_out_q_result_reg = RegEnable(div_out_q_result, stateReg(divide))
  val div_out_rem_result = Wire(UInt(Vectorwidth.W))
  val div_out_rem_result_reg = RegEnable(div_out_rem_result,stateReg(divide))
  val div_out_d_zero_result = Wire(UInt(16.W))
  val div_out_d_zero_result_reg = RegEnable(div_out_d_zero_result,stateReg(divide))

  finish := divide_8_finish.reduce(_ & _) & divide_16_finish.reduce(_ & _) & divide_32_finish.reduce(_ & _) & divide_64_finish.reduce(_ & _)
  div_out_d_zero_result :=
    Mux(sew_hb(0), Cat(divide_64_d_zero.asUInt, divide_32_d_zero.asUInt, divide_16_d_zero.asUInt, divide_8_d_zero.asUInt),
      Mux(sew_hb(1), Cat(0.U(8.W), divide_64_d_zero.asUInt, divide_32_d_zero.asUInt, divide_16_d_zero.asUInt),
        Mux(sew_hb(2), Cat(0.U(12.W),divide_64_d_zero.asUInt, divide_32_d_zero.asUInt),
          Cat(0.U(14.W),divide_64_d_zero.asUInt))))
  div_out_q_result :=
    Mux(sew_hb(0), Cat(divide_64_I8_q.asUInt,divide_32_I8_q.asUInt,divide_16_I8_q.asUInt, divide_8_q_result.asUInt),
      Mux(sew_hb(1),Cat(divide_64_I16_q.asUInt,divide_32_I16_q.asUInt,divide_16_I16_q.asUInt),
        Mux(sew_hb(2),Cat(divide_64_I32_q.asUInt,divide_32_I32_q.asUInt),
          divide_64_I64_q.asUInt)))
  div_out_rem_result :=
    Mux(sew_hb(0), Cat(divide_64_I8_rem.asUInt, divide_32_I8_rem.asUInt, divide_16_I8_rem.asUInt, divide_8_rem_result.asUInt),
      Mux(sew_hb(1), Cat(divide_64_I16_rem.asUInt, divide_32_I16_rem.asUInt, divide_16_I16_rem.asUInt),
        Mux(sew_hb(2), Cat(divide_64_I32_rem.asUInt, divide_32_I32_rem.asUInt),
          divide_64_I64_rem.asUInt)))


  // output
  io.div_out_q_v := div_out_q_result_reg
  io.div_out_rem_v := div_out_rem_result_reg
  io.d_zero := div_out_d_zero_result_reg


}
