/**
  * FMA supporting bf/fp16 and fp32. Includs:
  *   (1) bf16 -> bf16   (2) fp16 -> fp16   (3) fp32 -> fp32
  *   (4) bf16 -> fp32   (5) fp16 -> fp32
  * Hardware reuse:
  *   Can handle two fp16*fp16 or one fp32*fp32
  * Note: 
  *   1) For widen instrn, input bf/fp16 should be the highest half of the 32-bit input !
  *   2) NaN is not supported
  *   3) Rounding mode only supports RNE
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._
import race.vpu.yunsuan.util._


class FMUL_16_32 extends Module {
  val (expWidth_bf16, sigWidth_bf16) = (8, 8)
  val (expWidth_fp16, sigWidth_fp16) = (5, 11)
  val (expWidth_fp32, sigWidth_fp32) = (8, 24)
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val uop_in = Input(new VUop)
    val is_bf16, is_fp16, is_fp32 = Input(Bool())
    val a_in = UInt(32.W)
    val b_in = UInt(32.W)
    val uop_out = Output(new VUop)
    val res_out = Output(UInt(32.W))
  })

  val (is_bf16, is_fp16, is_fp32) = (io.is_bf16, io.is_fp16, io.is_fp32)
  val is_16 = is_fp16 || is_bf16
  val res_is_32 = io.uop_in.ctrl.widen || is_fp32
  val res_is_bf16 = is_bf16 && !io.uop_in.ctrl.widen
  val res_is_fp16 = is_fp16 && !io.uop_in.ctrl.widen
  
  val exp_high_a, exp_low_a, exp_high_b, exp_low_b = Wire(UInt(8.W))
  exp_high_a := Mux(is_fp16, io.a_in(30, 30-5+1), io.a_in(30, 30-8+1))
  exp_low_a := Mux(is_fp16, io.a_in(14, 14-5+1), io.a_in(14, 14-8+1))
  exp_high_b := Mux(is_fp16, io.b_in(30, 30-5+1), io.b_in(30, 30-8+1))
  exp_low_b := Mux(is_fp16, io.b_in(14, 14-5+1), io.b_in(14, 14-8+1))
  val exp_in = Seq(exp_low_a, exp_low_b, exp_high_a, exp_high_b)

  val frac_high_a_16, frac_low_a_16, frac_high_b_16, frac_low_b_16 = Wire(UInt(10.W))
  frac_high_a_16 := Mux(is_fp16, io.a_in(16+11-2, 16), Cat(io.a_in(16+8-2, 16), 0.U(3.W)))
  frac_low_a_16 := Mux(is_fp16, io.a_in(0+11-2, 0), Cat(io.a_in(0+8-2, 0), 0.U(3.W)))
  frac_high_b_16 := Mux(is_fp16, io.b_in(16+11-2, 16), Cat(io.b_in(16+8-2, 16), 0.U(3.W)))
  frac_low_b_16 := Mux(is_fp16, io.b_in(0+11-2, 0), Cat(io.b_in(0+8-2, 0), 0.U(3.W)))
  val frac_a_32 = io.a_in(22, 0)
  val frac_b_32 = io.b_in(22, 0)
  val frac_in_16 = Seq(frac_low_a_16, frac_low_b_16, frac_high_a_16, frac_high_b_16)
  val frac_in_32 = Seq(frac_a_32, frac_b_32)

  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_is_0 = Wire(Vec(4, Bool()))
  val exp_is_all1s = Wire(Vec(4, Bool()))
  val frac_is_0_16 = Wire(Vec(4, Bool()))
  exp_is_0 zip exp_in foreach { case (is_0, exp) => is_0 := exp === 0.U }
  frac_is_0_16 zip frac_in_16 foreach { case (is_0, frac) => is_0 := frac === 0.U }
  exp_is_all1s zip exp_in foreach { case (is_all1s, exp) =>
    is_all1s := exp === Mux(is_fp16, "b00011111".U, "b1111_1111".U)
  }
  //----   a, b = 0, 1  ----
  val frac_is_0_32 = Wire(Vec(2, Bool()))
  frac_is_0_32 zip frac_in_32 foreach { case (is_0, frac) => is_0 := frac === 0.U}

  val is_subnorm_16 = exp_is_0 zip frac_is_0_16 map {case (is_0, is_0_frac) => is_0 && !is_0_frac}
  val is_subnorm_32 = exp_is_0.drop(2) zip frac_is_0_32 map {case (is_0, is_0_frac) => is_0 && !is_0_frac}
  val is_zero_16 = exp_is_0 zip frac_is_0_16 map {case (is_0, is_0_frac) => is_0 && is_0_frac}
  val is_zero_32 = exp_is_0.drop(2) zip frac_is_0_32 map {case (is_0, is_0_frac) => is_0 && is_0_frac}
  val is_inf_16 = exp_is_all1s zip frac_is_0_16 map {case (is_all1s, is_0_frac) => is_all1s && is_0_frac}
  val is_inf_32 = exp_is_all1s.drop(2) zip frac_is_0_32 map {case (is_all1s, is_0_frac) => is_all1s && is_0_frac}

  val is_subnorm = Mux(is_16, VecInit(is_subnorm_16), VecInit(Seq(false.B, false.B) ++ is_subnorm_32))

  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_adjust_subnorm = Wire(Vec(4, UInt(8.W)))
  for (i <- 0 until 4) {
    exp_adjust_subnorm(i) := Mux(is_subnorm(i), 1.U, exp_in(i))
  }
  //  x.xxxxxxx000   bf16 (1 + 7 + "000")
  //  x.xxxxxxxxxx   fp16 (1 + 10)
  val sig_adjust_subnorm_16 = Wire(Vec(4, UInt(11.W)))
  for (i <- 0 until 4) {
    sig_adjust_subnorm_16(i) := Mux(is_subnorm(i), 0.U(1.W), 1.U(1.W)) ## frac_in_16(i)
  }
  //  x.xxxxxxxxxxxxxxxxxxxxxxx   fp32 (1 + 23)
  val sig_adjust_subnorm_32 = Wire(Vec(2, UInt(24.W)))
  for (i <- 0 until 2) {
    sig_adjust_subnorm_32(i) := Mux(is_subnorm(i+2), 0.U(1.W), 1.U(1.W)) ## frac_in_32(i)
  }

  /**
    * Here we need a integer multiplier to perform:
    *   Two 12*12 UInt multiplications, OR one 24*24 UInt multiplication
    */
  val intMul_12_24 = Module(new IntMUL_12_24_dummy)
  intMul_12_24.io.a_in := Mux(!is_16, sig_adjust_subnorm_32(0),
                          Cat(sig_adjust_subnorm_16(2), false.B, sig_adjust_subnorm_16(0), false.B))
  intMul_12_24.io.b_in := Mux(!is_16, sig_adjust_subnorm_32(1),
                          Cat(sig_adjust_subnorm_16(3), false.B, sig_adjust_subnorm_16(1), false.B))
  intMul_12_24.io.valid_in := io.valid_in
  intMul_12_24.io.uop_in := io.uop_in
  intMul_12_24.io.is_16 := is_16
  val uop_S1 = intMul_12_24.io.uop_out
  val valid_S1 = intMul_12_24.io.valid_out
  val res_intMul_S1 = intMul_12_24.io.res_out

  /**
    * Result normalization
    */
  // Exp calculation
  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_adjsubn_sum_high = Wire(UInt(10.W)) // 8 + 2
  val exp_adjsubn_sum_low = Wire(UInt(10.W)) // 8 + 2
  exp_adjsubn_sum_high := exp_adjust_subnorm(2) +& exp_adjust_subnorm(3)
  exp_adjsubn_sum_low := exp_adjust_subnorm(0) +& exp_adjust_subnorm(1)
  val exp_res_adjsubn_high = Mux(io.uop_in.ctrl.widen && is_fp16, // fp16 -> fp32
                                 exp_adjsubn_sum_high + (127 - 15 - 15).U,
                                 exp_adjsubn_sum_high - Mux(is_fp16, 15.U, 127.U))
  val exp_res_adjsubn_low = exp_adjsubn_sum_low - Mux(is_fp16, 15.U, 127.U)

  val res_is_inf_high = Mux(is_fp16, 
          !exp_res_adjsubn_high(9) && (exp_res_adjsubn_high(8, 5) === "b0001".U || exp_res_adjsubn_high(4, 0) === "b11111".U),
          !exp_res_adjsubn_high(9) && (exp_res_adjsubn_high(8) || exp_res_adjsubn_high(7, 0) === "b1111_1111".U))
  val res_is_inf_low = Mux(is_fp16, 
          !exp_res_adjsubn_low(9) && (exp_res_adjsubn_low(8, 5) === "b0001".U || exp_res_adjsubn_low(4, 0) === "b11111".U),
          !exp_res_adjsubn_low(9) && (exp_res_adjsubn_low(8) || exp_res_adjsubn_low(7, 0) === "b1111_1111".U))

  //----------------------------------------
  //---- Below is S1 (pipeline 1) stage ----
  //----------------------------------------
  val res_is_32_S1 = RegEnable(res_is_32, io.valid_in)
  val res_is_bf16_S1 = RegEnable(res_is_bf16, io.valid_in)
  val res_is_fp16_S1 = RegEnable(res_is_fp16, io.valid_in)
  val is_zero_16_S1 = is_zero_16.map(RegEnable(_, io.valid_in))
  val is_zero_32_S1 = is_zero_32.map(RegEnable(_, io.valid_in))
  val is_inf_16_S1 = is_inf_16.map(RegEnable(_, io.valid_in))
  val is_inf_32_S1 = is_inf_32.map(RegEnable(_, io.valid_in))
  val exp_res_adjsubn_high_S1 = RegEnable(exp_res_adjsubn_high, io.valid_in)  // 10 bits
  val exp_res_adjsubn_low_S1 = RegEnable(exp_res_adjsubn_low, io.valid_in)    // 10 bits
  val res_is_inf_high_S1 = RegEnable(res_is_inf_high, io.valid_in)
  val res_is_inf_low_S1 = RegEnable(res_is_inf_low, io.valid_in)

  // Int MUL result format:
  // xx.xxxxxxxxxxxxxx00000000  bf16 (2 + 14 + "000000" + "00")
  // xx.xxxxxxxxxxxxxxxxxxxx00  fp16 (2 + 20 + "00")
  // xx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  fp32 (2 + 46)
  val (res_intMul_high24, res_intMul_low24) = (res_intMul_S1(47, 24), res_intMul_S1(23, 0))
  val res_intMul_48 = Mux(uop_S1.ctrl.widen, res_intMul_high24 ## 0.U(24.W), res_intMul_S1)
  
  val int_part_high = res_intMul_high24(23, 22)
  val int_part_low = res_intMul_low24(23, 22)

  // ---- Calculate shift direction and shift amount ----
  val (shift_right_high, shift_right_low) = (Wire(Bool()), Wire(Bool()))
  val (shift_amount_high, shift_amount_low) = (Wire(UInt(5.W)), Wire(UInt(4.W)))
  val (res_is_subnorm_high, res_is_subnorm_low) = (Wire(Bool()), Wire(Bool()))
  val (res_is_inf_high_case1, res_is_inf_low_case1) = (Wire(Bool()), Wire(Bool()))
  val exp_res_low_under_1 = 1.U - exp_res_adjsubn_low_S1
  val exp_res_low_over_1 = exp_res_adjsubn_low_S1 - 1.U
  val lzd_low_22 = LZD(res_intMul_low24(21, 0))
  val lzd_high_22 = LZD(res_intMul_high24(21, 0))
  val lzd_46 = LZD(res_intMul_48)
  val lzd_low = Mux(lzd_low_22(4), 15.U(4.W), lzd_low_22(3, 0))
  val lzd_high = Mux(lzd_high_22(4), 15.U(4.W), lzd_high_22(3, 0))
  val lzd_whole = Mux(lzd_46(5), 31.U(5.W), lzd_46(4, 0))

  when (int_part_low(1)) { // integer part >= 2
    when (exp_res_adjsubn_low_S1(9)) {
      res_is_subnorm_low := true.B
    }
    when (exp_res_adjsubn_low_S1 === Mux(res_is_fp16_S1, "b00_00011110".U, "b00_11111110".U)) {
      res_is_inf_low_case1 := true.B
    }
    shift_right_low := true.B
    shift_amount_low := Mux(!res_is_subnorm_low, 1.U, exp_res_low_under_1)
  }.elsewhen (int_part_low(0)) { // integer part = 1
    when (exp_res_adjsubn_low_S1(9) || exp_res_adjsubn_low_S1 === 0.U) {
      res_is_subnorm_low := true.B
    }
    res_is_inf_low_case1 := false.B
    shift_right_low := true.B
    shift_amount_low := Mux(!res_is_subnorm_low, 0.U, exp_res_low_under_1)
  }.otherwise { // Integer part == 0; At least one input is sub-normal
    when (!exp_res_low_under_1(9)) { // exp <= 1
      res_is_subnorm_low := true.B
      shift_right_low := true.B
      shift_amount_low := exp_res_low_under_1
    }.elsewhen(exp_res_low_over_1 <= lzd_low) { // exp > 1  &&  (exp-1) <= LZD of fraction
      res_is_subnorm_low := true.B
      shift_right_low := false.B
      shift_amount_low := exp_res_low_over_1
    }.otherwise {  // exp > 1  &&  (exp-1) > LZD of fraction
      res_is_subnorm_low := false.B
      shift_right_low := false.B
      shift_amount_low := lzd_low + 1.U
    }
    res_is_inf_low_case1 := true.B
    
  }


  // Inf, Zero, subnormal

}
