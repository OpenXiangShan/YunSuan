package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._

class VectorIdiv extends VectorIdiv.Module {
  val in = IO(new VectorIdiv.In)
  val out = IO(new VectorIdiv.Out)

  val finish = Wire(Bool())
  val idle :: divide :: output :: Nil = Enum(3)
  val (oh_idle, oh_divide, oh_output) =
    (UIntToOH(idle, 3), UIntToOH(divide, 3), UIntToOH(output, 3))

  val sign  = in.ex0.bits.ctrl.sign
  val flush = in.ex0.bits.ctrl.flush
  val sel8  = in.ex0.bits.ctrl.sel8
  val sel16 = in.ex0.bits.ctrl.sel16
  val sel32 = in.ex0.bits.ctrl.sel32
  val sel64 = in.ex0.bits.ctrl.sel64
  val dividend_v = in.ex0.bits.data.dividend_v
  val divisor_v  = in.ex0.bits.data.divisor_v

  // handshake
  val stateReg = RegInit((1 << idle.litValue.toInt).U(3.W))
  val stateNext = WireInit(stateReg)
  val in_handshake = in.ex0.fire
  val out_handshake = out.ex0.fire

  // fsm
  // part 1
  when(flush) {
    stateReg := oh_idle
  }.otherwise {
    stateReg := stateNext
  }
  // part 2
  switch(stateReg) {
    is((1 << idle.litValue.toInt).U(3.W)) {
      when(in_handshake) {
        stateNext := oh_divide
      }
    }
    is((1 << divide.litValue.toInt).U(3.W)) {
      when(finish) {
        stateNext := oh_output
      }.otherwise {
        stateNext := oh_divide
      }
    }
    is((1 << output.litValue.toInt).U(3.W)) {
      when(out_handshake) {
        stateNext := oh_idle
      }
    }
  }
  // part 3
  in.ex0.ready := stateReg(idle)
  out.ex0.valid := stateReg(output)

  val x_reg = RegEnable(dividend_v, in_handshake)
  val d_reg = RegEnable(divisor_v, in_handshake)
  val sign_reg  = RegEnable(sign, in_handshake)
  val sel8_reg  = RegEnable(sel8, in_handshake)
  val sel16_reg = RegEnable(sel16, in_handshake)
  val sel32_reg = RegEnable(sel32, in_handshake)
  val sel64_reg = RegEnable(sel64, in_handshake)
  val Index_bound = Array(7, 15, 31, 63)

  /*
    I8  : 8*I8DivNr4 4*SRT16Divint(16) : sew = 00 2*SRT16Divint(32) : sew = 00 1*SRT16Divint(64) sew = 00
    I16 : 4*SRT16Divint(16) : sew = 01 4*SRT16Divint(16) : sew = 01 2*SRT16Divint(32) : sew = 01 1*SRT16Divint(64) sew = 01
    I32 : 2*SRT16Divint(32) : sew = 10 4*SRT16Divint(16) : sew = 10 2*SRT16Divint(32) : sew = 10 1*SRT16Divint(64) sew = 10
    I64 : 1*SRT16Divint(64) : sew = 11 4*SRT16Divint(16) : sew = 11 2*SRT16Divint(32) : sew = 11 1*SRT16Divint(64) sew = 11
   */
  // I8
  val divide_8_q_result   = Wire(Vec(i8Size, UInt(i8Width.W)))
  val divide_8_rem_result = Wire(Vec(i8Size, UInt(i8Width.W)))
  val divide_8_finish     = Wire(Vec(i8Size, Bool()))
  val divide_8_latency_valid = Wire(Vec(i8Size, Bool()))
  val divide_8_latency = Wire(Vec(i8Size, UInt(VectorIdiv.NonFixedLatencyWidth.W)))
  for (i <- 0 until i8Size) {
    val begin = i * i8Width
    val end = (i + 1) * i8Width - 1
    val divide_8 = Module(new I8DivNr4().suggestName(s"8bit_divide_${i}"))
    divide_8.io.sign          := sign_reg
    divide_8.io.flush         := flush
    divide_8.io.div_in_valid  := stateReg(divide)
    divide_8.io.dividend      := x_reg(end, begin)
    divide_8.io.divisor       := d_reg(end, begin)
    divide_8.io.div_out_ready := stateReg(output)

    divide_8_finish(i)        := divide_8.io.div_out_valid
    divide_8_q_result(i)      := divide_8.io.div_out_q
    divide_8_rem_result(i)    := divide_8.io.div_out_rem
    divide_8_latency_valid(i) := divide_8.io.div_latency.valid
    divide_8_latency(i)       := divide_8.io.div_latency.bits
  }

  // I16
  val divide_16_q_result   = Wire(Vec(i16Size, UInt(i16Width.W))) // additional field, storing both I8 and I16 results
  val divide_16_rem_result = Wire(Vec(i16Size, UInt(i16Width.W)))
  val divide_16_finish     = Wire(Vec(i16Size, Bool()))
  val divide_16_latency_valid = Wire(Vec(i16Size, Bool()))
  val divide_16_latency = Wire(Vec(i16Size, UInt(VectorIdiv.NonFixedLatencyWidth.W)))
  for (i <- 0 until i16Size) {
    val begin_I16 = i * i16Width
    val end_I16   = (i + 1) * i16Width - 1
    val begin_I8  = i64Width + i * i8Width
    val end_I8    = i64Width + (i + 1) * i8Width - 1
    val divide_16_dividend = Mux1H(Seq(
      sel8_reg  -> x_reg(end_I8, begin_I8),
      sel16_reg -> x_reg(end_I16, begin_I16),
    ))
    val divide_16_divisor = Mux1H(Seq(
      sel8_reg  -> d_reg(end_I8, begin_I8),
      sel16_reg -> d_reg(end_I16, begin_I16),
    ))
    val divide_16 = Module(new SRT16Divint(16).suggestName(s"16bit_divide_${i}"))
    divide_16.io.sign          := sign_reg
    divide_16.io.flush         := flush
    divide_16.io.sel8          := sel8_reg
    divide_16.io.sel16         := sel16_reg
    divide_16.io.sel32         := sel32_reg
    divide_16.io.sel64         := sel64_reg
    divide_16.io.div_in_valid  := stateReg(divide)
    divide_16.io.dividend      := divide_16_dividend
    divide_16.io.divisor       := divide_16_divisor
    divide_16.io.div_out_ready := stateReg(output)

    divide_16_q_result(i)      := divide_16.io.div_out_q
    divide_16_rem_result(i)    := divide_16.io.div_out_rem
    divide_16_finish(i)        := divide_16.io.div_out_valid
    divide_16_latency_valid(i) := divide_16.io.div_latency.valid
    divide_16_latency(i)       := divide_16.io.div_latency.bits
  }
  val divide_16_I8_q = Cat(divide_16_q_result(3)(Index_bound(0), 0), divide_16_q_result(2)(Index_bound(0), 0), divide_16_q_result(1)(Index_bound(0), 0), divide_16_q_result(0)(Index_bound(0), 0))
  val divide_16_I8_rem = Cat(divide_16_rem_result(3)(Index_bound(0), 0), divide_16_rem_result(2)(Index_bound(0), 0), divide_16_rem_result(1)(Index_bound(0), 0), divide_16_rem_result(0)(Index_bound(0), 0))
  val divide_16_I16_q = Cat(divide_16_q_result(3)(Index_bound(1), 0), divide_16_q_result(2)(Index_bound(1), 0), divide_16_q_result(1)(Index_bound(1), 0), divide_16_q_result(0)(Index_bound(1), 0))
  val divide_16_I16_rem = Cat(divide_16_rem_result(3)(Index_bound(1), 0), divide_16_rem_result(2)(Index_bound(1), 0), divide_16_rem_result(1)(Index_bound(1), 0), divide_16_rem_result(0)(Index_bound(1), 0))

  // I32
  val divide_32_q_result   = Wire(Vec(i32Size, UInt(i32Width.W)))
  val divide_32_rem_result = Wire(Vec(i32Size, UInt(i32Width.W)))
  val divide_32_finish     = Wire(Vec(i32Size, Bool()))
  val divide_32_latency_valid = Wire(Vec(i32Size, Bool()))
  val divide_32_latency = Wire(Vec(i32Size, UInt(VectorIdiv.NonFixedLatencyWidth.W)))
  for (i <- 0 until i32Size) {
    val begin_I8  = i64Width + i32Width + i * i8Width
    val end_I8    = i64Width + i32Width + (i + 1) * i8Width - 1
    val begin_I16 = i64Width + i * i16Width
    val end_I16   = i64Width + (i + 1) * i16Width - 1
    val begin_I32 = i * i32Width
    val end_I32   = (i + 1) * i32Width - 1
    val divide_32_dividend = Mux1H(Seq(
      sel8_reg  -> x_reg(end_I8, begin_I8),
      sel16_reg -> x_reg(end_I16, begin_I16),
      sel32_reg -> x_reg(end_I32, begin_I32),
    ))
    val divide_32_divisor = Mux1H(Seq(
      sel8_reg  -> d_reg(end_I8, begin_I8),
      sel16_reg -> d_reg(end_I16, begin_I16),
      sel32_reg -> d_reg(end_I32, begin_I32),
    ))
    val divide_32 = Module(new SRT16Divint(32).suggestName(s"32bit_divide_${i}"))
    divide_32.io.sign          := sign_reg
    divide_32.io.flush         := flush
    divide_32.io.sel8          := sel8_reg
    divide_32.io.sel16         := sel16_reg
    divide_32.io.sel32         := sel32_reg
    divide_32.io.sel64         := sel64_reg
    divide_32.io.div_in_valid  := stateReg(divide)
    divide_32.io.dividend      := divide_32_dividend
    divide_32.io.divisor       := divide_32_divisor
    divide_32.io.div_out_ready := stateReg(output)

    divide_32_q_result(i)      := divide_32.io.div_out_q
    divide_32_rem_result(i)    := divide_32.io.div_out_rem
    divide_32_finish(i)        := divide_32.io.div_out_valid
    divide_32_latency_valid(i) := divide_32.io.div_latency.valid
    divide_32_latency(i)       := divide_32.io.div_latency.bits
  }
  val divide_32_I8_q = Cat(divide_32_q_result(1)(Index_bound(0), 0), divide_32_q_result(0)(Index_bound(0), 0))
  val divide_32_I8_rem = Cat(divide_32_rem_result(1)(Index_bound(0), 0), divide_32_rem_result(0)(Index_bound(0), 0))
  val divide_32_I16_q = Cat(divide_32_q_result(1)(Index_bound(1), 0), divide_32_q_result(0)(Index_bound(1), 0))
  val divide_32_I16_rem = Cat(divide_32_rem_result(1)(Index_bound(1), 0), divide_32_rem_result(0)(Index_bound(1), 0))
  val divide_32_I32_q = Cat(divide_32_q_result(1)(Index_bound(2), 0), divide_32_q_result(0)(Index_bound(2), 0))
  val divide_32_I32_rem = Cat(divide_32_rem_result(1)(Index_bound(2), 0), divide_32_rem_result(0)(Index_bound(2), 0))

  // I64
  val divide_64_q_result   = Wire(Vec(i64Size, UInt(i64Width.W)))
  val divide_64_rem_result = Wire(Vec(i64Size, UInt(i64Width.W)))
  val divide_64_finish     = Wire(Vec(i64Size, Bool()))
  val divide_64_latency_valid = Wire(Vec(i64Size, Bool()))
  val divide_64_latency = Wire(Vec(i64Size, UInt(VectorIdiv.NonFixedLatencyWidth.W)))
  for (i <- 0 until i64Size) {
    val begin_I8  = i64Width + i32Width + i16Width + i * i8Width
    val end_I8    = i64Width + i32Width + i16Width + (i + 1) * i8Width - 1
    val begin_I16 = i64Width + i32Width + i * i16Width
    val end_I16   = i64Width + i32Width + (i + 1) * i16Width - 1
    val begin_I32 = i64Width + i * i32Width
    val end_I32   = i64Width + (i + 1) * i32Width - 1
    val begin_I64 = i * i64Width
    val end_I64   = (i + 1) * i64Width - 1
    val divide_64_dividend = Mux1H(Seq(
      sel8_reg  -> x_reg(end_I8, begin_I8),
      sel16_reg -> x_reg(end_I16, begin_I16),
      sel32_reg -> x_reg(end_I32, begin_I32),
      sel64_reg -> x_reg(end_I64, begin_I64),
    ))
    val divide_64_divisor = Mux1H(Seq(
      sel8_reg  -> d_reg(end_I8, begin_I8),
      sel16_reg -> d_reg(end_I16, begin_I16),
      sel32_reg -> d_reg(end_I32, begin_I32),
      sel64_reg -> d_reg(end_I64, begin_I64),
    ))
    val divide_64 = Module(new SRT16Divint(64).suggestName(s"64bit_divide_${i}"))
    divide_64.io.sign          := sign_reg
    divide_64.io.flush         := flush
    divide_64.io.sel8          := sel8_reg
    divide_64.io.sel16         := sel16_reg
    divide_64.io.sel32         := sel32_reg
    divide_64.io.sel64         := sel64_reg
    divide_64.io.div_in_valid  := stateReg(divide)
    divide_64.io.dividend      := divide_64_dividend
    divide_64.io.divisor       := divide_64_divisor
    divide_64.io.div_out_ready := stateReg(output)

    divide_64_q_result(i)      := divide_64.io.div_out_q
    divide_64_rem_result(i)    := divide_64.io.div_out_rem
    divide_64_finish(i)        := divide_64.io.div_out_valid
    divide_64_latency_valid(i) := divide_64.io.div_latency.valid
    divide_64_latency(i)       := divide_64.io.div_latency.bits
  }
  val divide_64_I8_q = Cat(divide_64_q_result(1)(Index_bound(0), 0), divide_64_q_result(0)(Index_bound(0), 0))
  val divide_64_I8_rem = Cat(divide_64_rem_result(1)(Index_bound(0), 0), divide_64_rem_result(0)(Index_bound(0), 0))
  val divide_64_I16_q = Cat(divide_64_q_result(1)(Index_bound(1), 0), divide_64_q_result(0)(Index_bound(1), 0))
  val divide_64_I16_rem = Cat(divide_64_rem_result(1)(Index_bound(1), 0), divide_64_rem_result(0)(Index_bound(1), 0))
  val divide_64_I32_q = Cat(divide_64_q_result(1)(Index_bound(2), 0), divide_64_q_result(0)(Index_bound(2), 0))
  val divide_64_I32_rem = Cat(divide_64_rem_result(1)(Index_bound(2), 0), divide_64_rem_result(0)(Index_bound(2), 0))
  val divide_64_I64_q = Cat(divide_64_q_result(1)(Index_bound(3), 0), divide_64_q_result(0)(Index_bound(3), 0))
  val divide_64_I64_rem = Cat(divide_64_rem_result(1)(Index_bound(3), 0), divide_64_rem_result(0)(Index_bound(3), 0))

  val div_out_q_result = Wire(UInt(vWidth.W))
  val div_out_q_result_reg = RegEnable(div_out_q_result, stateReg(divide))
  val div_out_rem_result = Wire(UInt(vWidth.W))
  val div_out_rem_result_reg = RegEnable(div_out_rem_result, stateReg(divide))

  finish := divide_8_finish.reduce(_ & _) & divide_16_finish.reduce(_ & _) & divide_32_finish.reduce(_ & _) & divide_64_finish.reduce(_ & _)

  val child_latency_valid =
    divide_8_latency_valid.asUInt.andR &
      divide_16_latency_valid.asUInt.andR &
      divide_32_latency_valid.asUInt.andR &
      divide_64_latency_valid.asUInt.andR
  val max_child_latency = maxUInt(
    divide_8_latency ++ divide_16_latency ++ divide_32_latency ++ divide_64_latency
  )
  val div_latency_result = max_child_latency + 2.U(VectorIdiv.NonFixedLatencyWidth.W)
  val div_elapsed = RegInit(0.U(VectorIdiv.NonFixedLatencyWidth.W))
  val div_latency_valid_reg = RegInit(false.B)
  val div_latency_reg = RegInit(0.U(VectorIdiv.NonFixedLatencyWidth.W))
  val div_latency_reported = RegInit(false.B)
  val div_latency_fire = stateReg(divide) && child_latency_valid && !div_latency_reported
  when(flush || in_handshake) {
    div_elapsed := 0.U
    div_latency_valid_reg := false.B
    div_latency_reported := false.B
  }.elsewhen(stateReg(output)) {
    div_elapsed := 0.U
  }.otherwise {
    when(stateReg(divide)) {
      div_elapsed := div_elapsed + 1.U
    }
    div_latency_valid_reg := div_latency_fire
  }
  when(div_latency_fire) {
    val elapsed_at_latency_valid = div_elapsed + 2.U(VectorIdiv.NonFixedLatencyWidth.W)
    val remaining_latency = Mux(
      div_latency_result > elapsed_at_latency_valid,
      div_latency_result - elapsed_at_latency_valid,
      0.U
    )
    div_latency_reg := remaining_latency(VectorIdiv.NonFixedLatencyWidth - 1, 0)
    div_latency_reported := true.B
  }

  div_out_q_result := Mux1H(Seq(
    sel8_reg  -> Cat(divide_64_I8_q.asUInt, divide_32_I8_q.asUInt, divide_16_I8_q.asUInt, divide_8_q_result.asUInt),
    sel16_reg -> Cat(divide_64_I16_q.asUInt, divide_32_I16_q.asUInt, divide_16_I16_q.asUInt),
    sel32_reg -> Cat(divide_64_I32_q.asUInt, divide_32_I32_q.asUInt),
    sel64_reg -> divide_64_I64_q.asUInt
  ))
  div_out_rem_result := Mux1H(Seq(
    sel8_reg  -> Cat(divide_64_I8_rem.asUInt, divide_32_I8_rem.asUInt, divide_16_I8_rem.asUInt, divide_8_rem_result.asUInt),
    sel16_reg -> Cat(divide_64_I16_rem.asUInt, divide_32_I16_rem.asUInt, divide_16_I16_rem.asUInt),
    sel32_reg -> Cat(divide_64_I32_rem.asUInt, divide_32_I32_rem.asUInt),
    sel64_reg -> divide_64_I64_rem.asUInt
  ))

  // output
  out.ex0.bits.q_v := div_out_q_result_reg
  out.ex0.bits.rem_v := div_out_rem_result_reg
  out.ex0.bits.div_latency.valid := div_latency_valid_reg
  out.ex0.bits.div_latency.bits := div_latency_reg

  private def maxUInt(seq: Seq[UInt]): UInt =
    seq.reduce((a, b) => Mux(a > b, a, b))
}

object VectorIdiv {
  val NonFixedLatencyWidth = 5

  class In extends Bundle {
    val ex0 = Flipped(DecoupledIO(new InEx0))
  }

  class Out extends Bundle {
    val ex0 = DecoupledIO(new OutEx0)
  }

  class InEx0 extends Bundle {
    val ctrl = new InCtrlEx0
    val data = new InDataEx0
  }

  class InCtrlEx0 extends Bundle {
    val sign   = Bool()
    val flush  = Bool()
    val sel8   = Bool()
    val sel16  = Bool()
    val sel32  = Bool()
    val sel64  = Bool()
  }

  class InDataEx0 extends Bundle {
    val dividend_v = UInt(vWidth.W)
    val divisor_v = UInt(vWidth.W)
  }

  class OutEx0 extends Bundle {
    val q_v = UInt(vWidth.W)
    val rem_v = UInt(vWidth.W)
    val div_latency = ValidIO(UInt(NonFixedLatencyWidth.W))
  }

  trait Config {
    val vWidth = 128
    val i8Size = 8
    val i8Width = 8
    val i16Size = 4
    val i16Width = 16
    val i32Size = 2
    val i32Width = 32
    val i64Size = 2
    val i64Width = 64
  }

  class Bundle extends chisel3.Bundle with Config
  class Module extends chisel3.Module with Config
}
