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
    val c_in = UInt(32.W)
    val uop_out = Output(new VUop)
    val res_out = Output(UInt(32.W))
  })

  val (is_bf16, is_fp16, is_fp32) = (io.is_bf16, io.is_fp16, io.is_fp32)
  val is_16 = is_fp16 || is_bf16
  val widen = io.uop_in.ctrl.widen
  val res_is_32 = widen || is_fp32
  val res_is_bf16 = is_bf16 && !widen
  val res_is_fp16 = is_fp16 && !widen
  
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
    * MUL result normalization (partly)
    */
  // Exp calculation
  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_adjsubn_sum_high = Wire(UInt(10.W)) // 8 + 2
  val exp_adjsubn_sum_low = Wire(UInt(10.W)) // 8 + 2
  exp_adjsubn_sum_high := exp_adjust_subnorm(2) +& exp_adjust_subnorm(3)
  exp_adjsubn_sum_low := exp_adjust_subnorm(0) +& exp_adjust_subnorm(1)
  val exp_res_adjsubn_high = Mux(widen && is_fp16, // fp16 -> fp32
                                 exp_adjsubn_sum_high + (127 - 15 - 15).U,
                                 exp_adjsubn_sum_high - Mux(is_fp16, 15.U, 127.U))
  val exp_res_adjsubn_low = exp_adjsubn_sum_low - Mux(is_fp16, 15.U, 127.U)

  val res_is_inf_high = Mux(is_fp16, 
          !exp_res_adjsubn_high(9) && (exp_res_adjsubn_high(8, 5) === "b0001".U || exp_res_adjsubn_high(4, 0) === "b11111".U),
          !exp_res_adjsubn_high(9) && (exp_res_adjsubn_high(8) || exp_res_adjsubn_high(7, 0) === "b1111_1111".U))
  val res_is_inf_low = Mux(is_fp16, 
          !exp_res_adjsubn_low(9) && (exp_res_adjsubn_low(8, 5) === "b0001".U || exp_res_adjsubn_low(4, 0) === "b11111".U),
          !exp_res_adjsubn_low(9) && (exp_res_adjsubn_low(8) || exp_res_adjsubn_low(7, 0) === "b1111_1111".U))
  val res_sign_high = io.a_in(31) ^ io.b_in(31)
  val res_sign_low = io.a_in(15) ^ io.b_in(15)

  //----------------------------------------
  //---- Below is S1 (pipeline 1) stage ----
  //----------------------------------------
  val input_is_16_S1 = RegEnable(is_16, io.valid_in)
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
  val res_sign_high_S1 = RegEnable(res_sign_high, io.valid_in)
  val res_sign_low_S1 = RegEnable(res_sign_low, io.valid_in)

  // Int MUL result format:
  // xx.xxxxxxxxxxxxxx00000000  bf16 (2 + 14 + "000000" + "00")
  // xx.xxxxxxxxxxxxxxxxxxxx00  fp16 (2 + 20 + "00")
  // xx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  fp32 (2 + 46)
  val (res_intMul_high24, res_intMul_low24) = (res_intMul_S1(47, 24), res_intMul_S1(23, 0))
  val res_intMul_48 = Mux(input_is_16_S1, res_intMul_high24 ## 0.U(24.W), res_intMul_S1)
  
  val int_part_high = res_intMul_high24(23, 22)
  val int_part_low = res_intMul_low24(23, 22)

  // ----------------------------------------------------
  // ---- Calculate shift direction and shift amount ----
  // ----------------------------------------------------
  val (shift_right_high, shift_right_low) = (Wire(Bool()), Wire(Bool()))
  val (shift_amount_high, shift_amount_low) = (Wire(UInt(5.W)), Wire(UInt(4.W)))
  val (res_is_subnorm_high, res_is_subnorm_low) = (Wire(Bool()), Wire(Bool()))
  val (res_is_inf_high_case1, res_is_inf_low_case1) = (Wire(Bool()), Wire(Bool()))
  val lzd_low = LZD(res_intMul_low24(21, 0)) // 5 bits
  val lzd_high = LZD(res_intMul_high24(21, 0)) // 5 bits
  val lzd_whole = LZD(res_intMul_48(45, 0)) // 6 bits
  val exp_res_low_under_1 = 1.U - exp_res_adjsubn_low_S1 // 10 bits
  val exp_res_low_over_1 = exp_res_adjsubn_low_S1 - 1.U  // 10 bits
  val exp_res_high_under_1 = 1.U - exp_res_adjsubn_high_S1 // 10 bits
  val exp_res_high_over_1 = exp_res_adjsubn_high_S1 - 1.U  // 10 bits
  // 如果指数小于1，并且被1减后的值大于15(31)，则判定结果为0。（此处15为方便计算而取的一个值）
  val res_is_zero_low_case1 = !exp_res_low_under_1(9) && exp_res_low_under_1(8, 4).orR // > 15
  val res_is_zero_high_case1 = !exp_res_high_under_1(9) && Mux(res_is_32_S1, exp_res_high_under_1(8, 5).orR, // > 31
                                                               exp_res_high_under_1(8, 4).orR) // > 15
  // Low part
  when (int_part_low(1)) { // integer part >= 2
    res_is_subnorm_low := exp_res_adjsubn_low_S1(9)
    res_is_inf_low_case1 := exp_res_adjsubn_low_S1 === Mux(res_is_fp16_S1, "b00_00011110".U, "b00_11111110".U)
    shift_right_low := true.B
    shift_amount_low := Mux(!res_is_subnorm_low, 1.U, exp_res_low_under_1)
  }.elsewhen (int_part_low(0)) { // integer part = 1
    res_is_subnorm_low := exp_res_adjsubn_low_S1(9) || exp_res_adjsubn_low_S1 === 0.U
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
    res_is_inf_low_case1 := false.B
  }

  // High part
  when (int_part_high(1)) { // integer part >= 2
    res_is_subnorm_high := exp_res_adjsubn_high_S1(9)
    res_is_inf_high_case1 := exp_res_adjsubn_high_S1 === Mux(res_is_fp16_S1, "b00_00011110".U, "b00_11111110".U)
    shift_right_high := true.B
    shift_amount_high := Mux(!res_is_subnorm_high, 1.U, exp_res_high_under_1)
  }.elsewhen (int_part_high(0)) { // integer part = 1
    res_is_subnorm_high := exp_res_adjsubn_high_S1(9) || exp_res_adjsubn_high_S1 === 0.U
    res_is_inf_high_case1 := false.B
    shift_right_high := true.B
    shift_amount_high := Mux(!res_is_subnorm_high, 0.U, exp_res_high_under_1)
  }.otherwise { // Integer part == 0; At least one input is sub-normal
    when (!exp_res_high_under_1(9)) { // exp <= 1
      res_is_subnorm_high := true.B
      shift_right_high := true.B
      shift_amount_high := exp_res_high_under_1
    }.elsewhen(exp_res_high_over_1 <= Mux(res_is_32_S1, lzd_whole, lzd_high)) { // exp > 1  &&  (exp-1) <= LZD of fraction
      res_is_subnorm_high := true.B
      shift_right_high := false.B
      shift_amount_high := exp_res_high_over_1
    }.otherwise {  // exp > 1  &&  (exp-1) > LZD of fraction
      res_is_subnorm_high := false.B
      shift_right_high := false.B
      shift_amount_high := Mux(res_is_32_S1, lzd_whole, lzd_high) + 1.U
    }
    res_is_inf_high_case1 := false.B
  }
  
  //---- Inf, zero, subnormal ----
  // TODO:  res_is_NaN
  // TODO:  inf * zero should be NaN, but here we set it to zero
  val res_is_zero_low_preS2 = is_zero_16_S1(0) || is_zero_16_S1(1) || res_is_zero_low_case1
  val res_is_zero_high_preS2 = Mux(input_is_16_S1, is_zero_16_S1(2) || is_zero_16_S1(3), is_zero_32_S1(0) || is_zero_32_S1(1)) ||
                               res_is_zero_high_case1
  val res_is_inf_low_preS2 = Mux(res_is_zero_low_preS2, false.B,
                                 is_inf_16_S1(0) || is_inf_16_S1(1) || res_is_inf_low_S1 || res_is_inf_low_case1)
  val res_is_inf_high_preS2 = Mux(res_is_zero_high_preS2, false.B,
                                  Mux(input_is_16_S1, is_inf_16_S1(2) || is_inf_16_S1(3), is_inf_32_S1(0) || is_inf_32_S1(1)) ||
                                  res_is_inf_high_S1 || res_is_inf_high_case1)
  
  //---- 对乘法的结果significand进行移位----
  //---- Shift the significand of the multiplication result
  val sig_res_shifted_low = shift(res_intMul_low24, shift_amount_low, shift_right_low)  // 24 bits
  val sig_res_shifted_whole = shift(res_intMul_48, shift_amount_high, shift_right_high)  // 48 bits

  //---- 乘法结果的exp会在此处更新 ----
  //---- the exponent of the multiplication result will be updated here
  val exp_resMul_shifted_low = (Cat(exp_res_adjsubn_low_S1, !shift_right_low) +
          Cat(Mux(shift_right_low, shift_amount_low, ~(shift_amount_low.pad(10))), !shift_right_low))(10, 1)
  val exp_resMul_shifted_high = (Cat(exp_res_adjsubn_high_S1, !shift_right_high) +
          Cat(Mux(shift_right_high, shift_amount_high, ~(shift_amount_high.pad(10))), !shift_right_high))(10, 1)
  // 参考下面注释掉的代码，理解上面的代码。上面代码的目的是为了减少一个减法器
  // val exp_resMul_shifted_low = Mux(shift_right_low, exp_res_adjsubn_low_S1 + shift_amount_low,
  //                                                exp_res_adjsubn_low_S1 - shift_amount_low) // 10 bits
  // val exp_resMul_shifted_high = Mux(shift_right_high, exp_res_adjsubn_high_S1 + shift_amount_high,
  //                                                exp_res_adjsubn_high_S1 - shift_amount_high) // 10 bits

  val exp_resMul_low_final = exp_resMul_shifted_low(7, 0)
  val exp_resMul_high_final = exp_resMul_shifted_high(7, 0)

  //-----------------------------------------
  //---- Below is S2 (pipeline 2) stage:
  //-----------------------------------------
  val valid_S2 = RegNext(valid_S1)
  val uop_S2 = RegEnable(uop_S1, valid_S1)
  val input_is_16_S2 = RegEnable(is_16, valid_S1)
  val res_is_32_S2 = RegEnable(res_is_32, valid_S1)
  val res_is_bf16_S2 = RegEnable(res_is_bf16, valid_S1)
  val res_is_fp16_S2 = RegEnable(res_is_fp16, valid_S1)
  val resMul_sign_high_S2 = RegEnable(res_sign_high_S1, valid_S1)
  val resMul_sign_low_S2 = RegEnable(res_sign_low_S1, valid_S1)
  val resMul_is_inf_low_S2 = RegEnable(res_is_inf_low_preS2, valid_S1)
  val resMul_is_inf_high_S2 = RegEnable(res_is_inf_high_preS2, valid_S1)
  val resMul_is_subnorm_low_S2 = RegEnable(res_is_subnorm_low, valid_S1)
  val resMul_is_subnorm_high_S2 = RegEnable(res_is_subnorm_high, valid_S1)

  val sig_resMul_low_S2 = RegEnable(sig_res_shifted_low, valid_S1)
  val sig_resMul_whole_S2 = RegEnable(sig_res_shifted_whole, valid_S1)
  val exp_resMul_low_S2 = RegEnable(exp_resMul_low_final, valid_S1)
  val exp_resMul_high_S2 = RegEnable(exp_resMul_high_final, valid_S1)
 
  //-------------------------------------------------------
  //---- Here is S2 (pipeline 2) stage of addend (c) ------
  //-------------------------------------------------------
  val c_in_S1 = RegEnable(io.c_in, io.valid_in)
  val c_in_S2 = RegEnable(c_in_S1, valid_S1)
  val widen_S2 = uop_S2.ctrl.widen
  val c_is_32 = !input_is_16_S2 || widen_S2
  val c_is_fp16 = RegEnable(RegEnable(is_fp16, valid_S1), valid_S2)
  val exp_high_c, exp_low_c = Wire(UInt(8.W))
  exp_high_c := Mux(c_is_fp16 && !widen_S2, c_in_S2(30, 30-5+1), c_in_S2(30, 30-8+1))
  exp_low_c := Mux(c_is_fp16 && !widen_S2, c_in_S2(14, 14-5+1), io.a_in(14, 14-8+1))
  val exp_in_c = Seq(exp_low_c, exp_high_c)

  val frac_high_c_16, frac_low_c_16 = Wire(UInt(10.W))
  frac_high_c_16 := Mux(c_is_fp16, c_in_S2(16+11-2, 16), Cat(c_in_S2(16+8-2, 16), 0.U(3.W)))
  frac_low_c_16 := Mux(c_is_fp16, c_in_S2(0+11-2, 0), Cat(c_in_S2(0+8-2, 0), 0.U(3.W)))
  val frac_c_32 = c_in_S2(22, 0)
  val frac_in_c_16 = Seq(frac_low_c_16, frac_high_c_16)

  val exp_is_0_c = Wire(Vec(2, Bool()))
  exp_is_0_c zip exp_in_c foreach {case (is_0, exp) => is_0 := exp === 0.U}

  val is_subnorm_zero_16_c = exp_is_0_c
  val is_subnorm_zero_32_c = exp_is_0_c(1)
  val is_subnorm_zero_c = Mux(!c_is_32, is_subnorm_zero_16_c, VecInit(false.B, is_subnorm_zero_32_c))

  val exp_adjust_subnorm_c = Wire(Vec(2, UInt(8.W))) //---- low, high = 0, 1 ----
  for (i <- 0 until 2) {
    exp_adjust_subnorm_c(i) := Mux(is_subnorm_zero_c(i), 1.U, exp_in_c(i))
  }
  // x.xxxxxxx000   bf16 (1 + 7 + "000")
  // x.xxxxxxxxxx   fp16 (1 + 10)
  val sig_adjust_subnorm_16_c = Wire(Vec(2, UInt(12.W)))
  for (i <- 0 until 2) { // c需要在整数部分多补一个0，和乘法输出保证一致
    sig_adjust_subnorm_16_c(i) := Mux(is_subnorm_zero_c(i), 0.U(2.W), 1.U(2.W)) ## frac_in_c_16(i)
  }
  // x.xxxxxxxxxxxxxxxxxxxxxxx   fp32 (1 + 23)
  val sig_adjust_subnorm_32_c = Wire(UInt(25.W)) // c需要在整数部分多补一个0，和乘法输出保证一致
  sig_adjust_subnorm_32_c := Mux(is_subnorm_zero_c(1), 0.U(2.W), 1.U(2.W)) ## frac_c_32

  //----------------------------
  //---- Addition (a*b + c) ----
  //----------------------------
  val exp_diff_low_c_minus_ab = exp_adjust_subnorm_c(0) -& exp_resMul_low_S2 // 9 bits
  val exp_diff_low_ab_minus_c = exp_resMul_low_S2 -& exp_adjust_subnorm_c(0) // 9 bits
  val exp_diff_high_c_minus_ab = exp_adjust_subnorm_c(1) -& exp_resMul_high_S2 // 9 bits
  val exp_diff_high_ab_minus_c = exp_resMul_high_S2 -& exp_adjust_subnorm_c(1) // 9 bits
  
  // ---- Shifting of low part ----
  val c_gte_ab_low = !exp_diff_low_c_minus_ab(8)
  val c_dominates_low = c_gte_ab_low && exp_diff_low_c_minus_ab(7, 5) =/= 0.U // > 31
  val ab_dominates_low = !exp_diff_low_ab_minus_c(8) && exp_diff_low_ab_minus_c(7, 5) =/= 0.U // > 31
  val shift_amount_ab_low, shift_amount_c_low = Wire(UInt(5.W))
  val shift_ab_low = Wire(Bool())
  when (c_dominates_low) {
    shift_amount_ab_low := 31.U
    shift_amount_c_low := 0.U
    shift_ab_low := true.B
  }.elsewhen (ab_dominates_low) {
    shift_amount_ab_low := 0.U
    shift_amount_c_low := 31.U
    shift_ab_low := false.B
  }.elsewhen (c_gte_ab_low) {
    shift_amount_ab_low := 0.U
    shift_amount_c_low := 0.U
    shift_ab_low := true.B
  }.otherwise {
    shift_amount_ab_low := 0.U
    shift_amount_c_low := 0.U
    shift_ab_low := false.B
  }
  // Select input of shift block, ab or c.
  val sig_adjust_subnorm_c_low_24 = sig_adjust_subnorm_16_c(0) ## 0.U(12.W) // 24 bits
  val shift_in_low = Mux(shift_ab_low, sig_resMul_low_S2, sig_adjust_subnorm_c_low_24) // 24 bits
  val shift_amount_in_low = Mux(shift_ab_low, shift_amount_ab_low, shift_amount_c_low) // 5 bits
  val shift_out_low = shift_right(shift_in_low, shift_amount_in_low) // 24 bits

  // ---- Shifting of high/whole part ----
  val c_gte_ab_high = !exp_diff_high_c_minus_ab(8)
  val c_dominates_high = c_gte_ab_high && exp_diff_high_c_minus_ab(7, 6) =/= 0.U // > 63
  val ab_dominates_high = !exp_diff_high_ab_minus_c(8) && exp_diff_high_ab_minus_c(7, 6) =/= 0.U // > 63
  val shift_amount_ab_high, shift_amount_c_high = Wire(UInt(6.W))
  val shift_ab_high = Wire(Bool())
  when (c_dominates_high) {
    shift_amount_ab_high := 63.U
    shift_amount_c_high := 0.U
    shift_ab_high := true.B
  }.elsewhen (ab_dominates_high) {
    shift_amount_ab_high := 0.U
    shift_amount_c_high := 63.U
    shift_ab_high := false.B
  }.elsewhen (c_gte_ab_high) {
    shift_amount_ab_high := 0.U
    shift_amount_c_high := 0.U
    shift_ab_high := true.B
  }.otherwise {
    shift_amount_ab_high := 0.U
    shift_amount_c_high := 0.U
    shift_ab_high := false.B
  }
  // Select input of shift block, ab or c.
  // val sig_adjust_subnorm_c_low_24 = sig_adjust_subnorm_16_c(0) ## 0.U(12.W) // 24 bits
  // val shift_in_low = Mux(shift_ab_low, sig_resMul_low_S2, sig_adjust_subnorm_c_low_24) // 24 bits
  // val shift_amount_in_low = Mux(shift_ab_low, shift_amount_ab_low, shift_amount_c_low) // 5 bits
  // val shift_out_low = shift_right(shift_in_low, shift_amount_in_low) // 24 bits


  // Addition:   24 24
  //           + 24 24
  sig_resMul_low_S2  //24  (2 + 22)
  sig_adjust_subnorm_c_low_24  //24  (2 + 22)
  shift_out_low  //24  (2 + 22)





  // 加法要减少位




  // !!!! After rounding, subnormal may become normal, when exp==1 and sig = 0.1111111111..
  

  def shift(data: UInt, shift_amount: UInt, shift_right: Bool): UInt = {
    // Reverse the data when shifting left
    val reversed_data = Mux(shift_right, data, Cat(data.asBools))
    val shifted_data = reversed_data >> shift_amount
    Mux(shift_right, shifted_data, Cat(shifted_data.asBools))
  }
  def shift_right(data: UInt, shift_amount: UInt): UInt = {
    data >> shift_amount
  }


  // 加法时，如果mul res是zero，则结果等于c_in
  // 加法时，如果mul res是inf，则结果等于inf

  // Inf, Zero, subnormal

}
