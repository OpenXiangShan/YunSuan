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
  *   4) wResMul32 is tunable parameter: larger wResMul32 means better precision and higher hardware cost
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._
import race.vpu.yunsuan.util._

class VFMA_16_32 extends Module {
  val wResMul32 = 48  // Bits to reserve for the significand of the a*b (range: 28 ~ 48)
  val wResMul16 = wResMul32 / 2  // Bits (FP/BF16) to reserve for the significand of the a*b
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val is_bf16, is_fp16, is_fp32 = Input(Bool())
    val is_widen = Input(Bool())
    val a_in = Input(UInt(32.W))
    val b_in = Input(UInt(32.W))
    val c_in = Input(UInt(32.W))
    val res_out = Output(UInt(32.W))
    val valid_out = Output(Bool())
    val valid_S1, valid_S2 = Output(Bool())
  })

  val (is_bf16, is_fp16, is_fp32) = (io.is_bf16, io.is_fp16, io.is_fp32)
  val is_16 = is_fp16 || is_bf16
  val widen = io.is_widen
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
  val intMul_12_24 = Module(new IntMUL_12_24)
  intMul_12_24.io.a_in := Mux(!is_16, sig_adjust_subnorm_32(0),
                          Cat(sig_adjust_subnorm_16(2), false.B, sig_adjust_subnorm_16(0), false.B))
  intMul_12_24.io.b_in := Mux(!is_16, sig_adjust_subnorm_32(1),
                          Cat(sig_adjust_subnorm_16(3), false.B, sig_adjust_subnorm_16(1), false.B))
  intMul_12_24.io.valid_in := io.valid_in
  intMul_12_24.io.is_16 := is_16
  val widen_S1 = RegEnable(widen, io.valid_in)
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

  val res_is_inf_high = Mux(is_fp16 && !widen, 
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

  // Int MUL result format (when wResMul32 = 48, wResMul16 = 24):
  // xx.xxxxxxxxxxxxxx00000000  bf16 (2 + 14 + "000000" + "00")
  // xx.xxxxxxxxxxxxxxxxxxxx00  fp16 (2 + 20 + "00")
  // xx.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  fp32 (2 + 46)
  val (res_intMul_high24, res_intMul_low24) = (res_intMul_S1(47, 47-wResMul16+1), res_intMul_S1(23, 23-wResMul16+1)) // wResMul16 bits
  val res_intMul_48 = Mux(input_is_16_S1,
          Mux(widen_S1, res_intMul_S1(47, 24), res_intMul_high24 ## 0.U((24-wResMul16).W)) ## 0.U((wResMul32-24).W),
          res_intMul_S1(47, 47-wResMul32+1))  // wResMul32 bits
  
  val int_part_high = res_intMul_high24(wResMul16-1, wResMul16-2)
  val int_part_low = res_intMul_low24(wResMul16-1, wResMul16-2)

  // ----------------------------------------------------
  // ---- Calculate shift direction and shift amount ----
  // ----------------------------------------------------
  val (shift_right_high, shift_right_low) = (Wire(Bool()), Wire(Bool()))
  // val (shift_amount_high, shift_amount_low) = (Wire(UInt(6.W)), Wire(UInt(5.W)))
  val shift_amount_high = if (wResMul32 < 32) Wire(UInt(5.W)) else Wire(UInt(6.W))
  val shift_amount_low = if (wResMul16 < 16) Wire(UInt(4.W)) else Wire(UInt(5.W))
  val (res_is_subnorm_high, res_is_subnorm_low) = (Wire(Bool()), Wire(Bool()))
  val (res_is_inf_high_case1, res_is_inf_low_case1) = (Wire(Bool()), Wire(Bool()))
  val lzd_low = LZD(res_intMul_low24(wResMul16-1-2, 0)) // 4 bits (wResMul16 < 16) or 5 bits (wResMul16 >= 16)
  val lzd_high = LZD(res_intMul_high24(wResMul16-1-2, 0)) // 4 bits (wResMul16 < 16) or 5 bits (wResMul16 >= 16)
  val lzd_whole = LZD(res_intMul_48(wResMul32-1-2, 0)) // 5 bits (wResMul32 < 32) or 6 bits (wResMul32 >= 32)
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
  val sig_resMul_shifted_low = shift(res_intMul_low24, shift_amount_low, shift_right_low)  // wResMul16 bits
  val sig_resMul_shifted_whole = shift(res_intMul_48, shift_amount_high, shift_right_high)  // wResMul32 bits

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
  val valid_S2 = RegNext(valid_S1, init = false.B)
  val input_is_16_S2 = RegEnable(is_16, valid_S1)
  val res_is_32_S2 = RegEnable(res_is_32, valid_S1)
  val res_is_bf16_S2 = RegEnable(res_is_bf16, valid_S1)
  val res_is_fp16_S2 = RegEnable(res_is_fp16, valid_S1)
  val resMul_sign_high_S2 = RegEnable(res_sign_high_S1, valid_S1)
  val resMul_sign_low_S2 = RegEnable(res_sign_low_S1, valid_S1)
  val resMul_is_zero_low_S2 = RegEnable(res_is_zero_low_preS2, valid_S1)
  val resMul_is_zero_high_S2 = RegEnable(res_is_zero_high_preS2, valid_S1)
  val resMul_is_inf_low_S2 = RegEnable(res_is_inf_low_preS2, valid_S1)
  val resMul_is_inf_high_S2 = RegEnable(res_is_inf_high_preS2, valid_S1)
  val resMul_is_subnorm_low_S2 = RegEnable(res_is_subnorm_low, valid_S1)
  val resMul_is_subnorm_high_S2 = RegEnable(res_is_subnorm_high, valid_S1)

  val sig_resMul_low_S2 = RegEnable(sig_resMul_shifted_low, valid_S1)
  val sig_resMul_whole_S2 = RegEnable(sig_resMul_shifted_whole, valid_S1)
  val exp_resMul_low_S2 = RegEnable(exp_resMul_low_final, valid_S1)
  val exp_resMul_high_S2 = RegEnable(exp_resMul_high_final, valid_S1)
 
  //-------------------------------------------------------
  //---- Here is S2 (pipeline 2) stage of addend (c) ------
  //-------------------------------------------------------
  val c_in_S1 = RegEnable(io.c_in, io.valid_in)
  val c_in_S2 = RegEnable(c_in_S1, valid_S1)
  val (sign_c_high, sign_c_low) = (c_in_S2(31), c_in_S2(15))
  val widen_S2 = RegEnable(widen, valid_S1)
  val c_is_32 = !input_is_16_S2 || widen_S2
  val c_is_fp16 = RegEnable(RegEnable(is_fp16, io.valid_in), valid_S1)
  val exp_high_c, exp_low_c = Wire(UInt(8.W))
  exp_high_c := Mux(c_is_fp16 && !widen_S2, c_in_S2(30, 30-5+1), c_in_S2(30, 30-8+1))
  exp_low_c := Mux(c_is_fp16 && !widen_S2, c_in_S2(14, 14-5+1), c_in_S2(14, 14-8+1))
  val exp_in_c = Seq(exp_low_c, exp_high_c)

  val frac_high_c_16, frac_low_c_16 = Wire(UInt(10.W))
  frac_high_c_16 := Mux(c_is_fp16, c_in_S2(16+11-2, 16), Cat(c_in_S2(16+8-2, 16), 0.U(3.W)))
  frac_low_c_16 := Mux(c_is_fp16, c_in_S2(0+11-2, 0), Cat(c_in_S2(0+8-2, 0), 0.U(3.W)))
  val frac_c_32 = c_in_S2(22, 0)
  val frac_c_16 = Seq(frac_low_c_16, frac_high_c_16)

  val exp_is_0_c = Wire(Vec(2, Bool()))
  exp_is_0_c zip exp_in_c foreach {case (is_0, exp) => is_0 := exp === 0.U}
  val frac_is_0_16_c = frac_c_16.map(_ === 0.U)
  val frac_is_0_32_c = frac_c_32 === 0.U
  val exp_is_all1s_c = exp_in_c.map(_ === Mux(is_fp16, "b00011111".U, "b1111_1111".U))

  val is_inf_16_c = exp_is_all1s_c zip frac_is_0_16_c map {case (is_all1s, is_0_frac) => is_all1s && is_0_frac}
  val is_inf_32_c = exp_is_all1s_c(1) && frac_is_0_32_c
  val is_inf_high_c = Mux(c_is_32, is_inf_32_c, is_inf_16_c(1))
  val is_inf_low_c = is_inf_16_c(0)

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
    sig_adjust_subnorm_16_c(i) := Mux(is_subnorm_zero_c(i), 0.U(2.W), 1.U(2.W)) ## frac_c_16(i)
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
  val exp_c_gte_ab_low = !exp_diff_low_c_minus_ab(8)
  val c_dominates_low = if (wResMul16 < 16) { exp_c_gte_ab_low && exp_diff_low_c_minus_ab(7, 4) =/= 0.U } else // > 15
                                            { exp_c_gte_ab_low && exp_diff_low_c_minus_ab(7, 5) =/= 0.U } // > 31
  val ab_dominates_low = if (wResMul16 < 16) { !exp_diff_low_ab_minus_c(8) && exp_diff_low_ab_minus_c(7, 4) =/= 0.U } else // > 15
                                             { !exp_diff_low_ab_minus_c(8) && exp_diff_low_ab_minus_c(7, 5) =/= 0.U } // > 31
  val shift_amount_ab_low, shift_amount_c_low = if (wResMul16 < 16) Wire(UInt(4.W)) else Wire(UInt(5.W))
  val shift_ab_low = Wire(Bool())
  when (c_dominates_low) {
    shift_amount_ab_low := { if (wResMul16 < 16) 15.U else 31.U }
    shift_amount_c_low := 0.U
    shift_ab_low := true.B
  }.elsewhen (ab_dominates_low) {
    shift_amount_ab_low := 0.U
    shift_amount_c_low := { if (wResMul16 < 16) 15.U else 31.U }
    shift_ab_low := false.B
  }.elsewhen (exp_c_gte_ab_low) {
    shift_amount_ab_low := exp_diff_low_c_minus_ab(4, 0)
    shift_amount_c_low := 0.U
    shift_ab_low := true.B
  }.otherwise {
    shift_amount_ab_low := 0.U
    shift_amount_c_low := exp_diff_low_ab_minus_c(4, 0)
    shift_ab_low := false.B
  }
  // Select input of shift block, ab or c.
  val sig_adjust_subnorm_c_low_24 = sig_adjust_subnorm_16_c(0) ## 0.U((wResMul16-12).W) // wResMul16 bits
  val shift_in_low = Mux(shift_ab_low, sig_resMul_low_S2, sig_adjust_subnorm_c_low_24) // wResMul16 bits
  val shift_amount_in_low = Mux(shift_ab_low, shift_amount_ab_low, shift_amount_c_low) // 4 bits or 5 bits
  val shift_out_low = shift_right(shift_in_low, shift_amount_in_low) // wResMul16 bits

  // ---- Shifting of high/whole part ----
  val exp_c_gte_ab_high = !exp_diff_high_c_minus_ab(8)
  val c_dominates_high = if (wResMul32 < 32) { exp_c_gte_ab_high && exp_diff_high_c_minus_ab(7, 5) =/= 0.U } else // > 31
                                             { exp_c_gte_ab_high && exp_diff_high_c_minus_ab(7, 6) =/= 0.U } // > 63
  val ab_dominates_high = if (wResMul32 < 32) { !exp_diff_high_ab_minus_c(8) && exp_diff_high_ab_minus_c(7, 5) =/= 0.U } else // > 31
                                              { !exp_diff_high_ab_minus_c(8) && exp_diff_high_ab_minus_c(7, 6) =/= 0.U } // > 63
  val shift_amount_ab_high, shift_amount_c_high = if (wResMul32 < 32) Wire(UInt(5.W)) else Wire(UInt(6.W))
  val shift_ab_high = Wire(Bool())
  when (c_dominates_high) {
    shift_amount_ab_high := { if (wResMul32 < 32) 31.U else 63.U }
    shift_amount_c_high := 0.U
    shift_ab_high := true.B
  }.elsewhen (ab_dominates_high) {
    shift_amount_ab_high := 0.U
    shift_amount_c_high := { if (wResMul32 < 32) 31.U else 63.U }
    shift_ab_high := false.B
  }.elsewhen (exp_c_gte_ab_high) {
    shift_amount_ab_high := exp_diff_high_c_minus_ab(5, 0)
    shift_amount_c_high := 0.U
    shift_ab_high := true.B
  }.otherwise {
    shift_amount_ab_high := 0.U
    shift_amount_c_high := exp_diff_high_ab_minus_c(5, 0)
    shift_ab_high := false.B
  }
  // Select input of shift block, ab or c.
  val sig_adjust_subnorm_c_whole_48 = Mux(c_is_32, sig_adjust_subnorm_32_c ## 0.U((wResMul32-25).W),
                                                   sig_adjust_subnorm_16_c(1) ## 0.U((wResMul32-12).W)) // wResMul32 bits
  val shift_in_whole = Mux(shift_ab_high, sig_resMul_whole_S2, sig_adjust_subnorm_c_whole_48) // wResMul32 bits
  val shift_amount_in_whole = Mux(shift_ab_high, shift_amount_ab_high, shift_amount_c_high) // 5 bits or 6 bits
  val shift_out_whole = shift_right(shift_in_whole, shift_amount_in_whole) // wResMul32 bits

  //---- Comparison of absolute value of ab and c ----
  // Low
  val sig_ab_gt_c_low = sig_resMul_low_S2 > sig_adjust_subnorm_c_low_24
  val exp_ab_gt_c_low = exp_diff_low_c_minus_ab(8)
  val abs_ab_gt_c_low = exp_ab_gt_c_low ||
                        exp_diff_low_ab_minus_c === 0.U && sig_ab_gt_c_low
  // Whole
  val sig_ab_gt_c_whole = sig_resMul_whole_S2 > sig_adjust_subnorm_c_whole_48
  val exp_ab_gt_c_whole = exp_diff_high_c_minus_ab(8)
  val abs_ab_gt_c_whole = exp_ab_gt_c_whole ||
                           exp_diff_high_ab_minus_c === 0.U && sig_ab_gt_c_whole

  // sig_resMul_low_S2:   wResMul16  (2 + wResMul16-2)
  // sig_adjust_subnorm_c_low_24: wResMul16  (2 + wResMul16-2)
  // shift_out_low: wResMul16
  
  // sig_resMul_whole_S2: wResMul32  (2 + wResMul32-2)
  // sig_adjust_subnorm_c_whole_48: wResMul32  (2 + wResMul32-2)
  // shift_out_whole: wResMul32
  
  //-----------------------------------------
  // Addition:   wResMul16 wResMul16
  //           + wResMul16 wResMul16
  //-----------------------------------------
  //---- Adder-48 is made of two adder-24s    (48 -> wResMul32, 24 -> wResMul16)
  //-----------------------------------------
  val adderIn_ab_low_24 = Mux(shift_ab_low, shift_out_low, sig_resMul_low_S2)
  val adderIn_c_low_24 = Mux(!shift_ab_low, shift_out_low, sig_adjust_subnorm_c_low_24)
  val adderIn_ab_whole_48 = Mux(shift_ab_high, shift_out_whole, sig_resMul_whole_S2)
  val adderIn_c_whole_48 = Mux(!shift_ab_high, shift_out_whole, sig_adjust_subnorm_c_whole_48)
  
  val ab_p_c_p_low = !resMul_sign_low_S2 && !sign_c_low // ab_low > 0 && c_low > 0
  val ab_n_c_n_low = resMul_sign_low_S2 && sign_c_low // ab_low < 0 && c_low < 0
  val ab_p_c_n_low = !resMul_sign_low_S2 && sign_c_low // ab_low > 0 && c_low < 0
  val ab_n_c_p_low = resMul_sign_low_S2 && !sign_c_low // ab_low < 0 && c_low > 0
  val ab_p_c_p_high = !resMul_sign_high_S2 && !sign_c_high // ab_high > 0 && c_high > 0
  val ab_n_c_n_high = resMul_sign_high_S2 && sign_c_high // ab_high < 0 && c_high < 0
  val ab_p_c_n_high = !resMul_sign_high_S2 && sign_c_high // ab_high > 0 && c_high < 0
  val ab_n_c_p_high = resMul_sign_high_S2 && !sign_c_high // ab_high < 0 && c_high > 0

  val ab_c_diffSign_low = ab_n_c_p_low || ab_p_c_n_low
  val ab_c_diffSign_high = ab_n_c_p_high || ab_p_c_n_high

  // 对于负数，要取反加一。本设计保证让绝对值较小的那个数取反加1，这样sig结果为正数，避免了结果为负时还需要再对结果取反加一。
  // 在ab与c符号相反的情况下，若(1) ab绝对值 > c绝对值，c取反加一，结果符号位与ab符号位相同；
  //                      否则(2) ab绝对值 <= c绝对值，ab取反加一，结果符号位与c符号位相同。
  val adderIn_ab_low_24_inv = Mux(ab_c_diffSign_low && !abs_ab_gt_c_low, ~adderIn_ab_low_24, adderIn_ab_low_24)
  val adderIn_c_low_24_inv = Mux(ab_c_diffSign_low && abs_ab_gt_c_low, ~adderIn_c_low_24, adderIn_c_low_24)
  val adderIn_ab_whole_48_inv = Mux(ab_c_diffSign_high && !abs_ab_gt_c_whole, ~adderIn_ab_whole_48, adderIn_ab_whole_48)
  val adderIn_c_whole_48_inv = Mux(ab_c_diffSign_high && abs_ab_gt_c_whole, ~adderIn_c_whole_48, adderIn_c_whole_48)
  
  val adderIn_ab_high_24_final = adderIn_ab_whole_48_inv(wResMul32-1, wResMul16)
  val adderIn_c_high_24_final = adderIn_c_whole_48_inv(wResMul32-1, wResMul16)
  val adderIn_ab_low_24_final = Mux(res_is_32_S2, adderIn_ab_whole_48_inv(wResMul16-1, 0), adderIn_ab_low_24_inv)
  val adderIn_c_low_24_final = Mux(res_is_32_S2, adderIn_c_whole_48_inv(wResMul16-1, 0), adderIn_c_low_24_inv)

  // Low 24-bit adder
  val adderIn_cin_low = Mux(res_is_32_S2, ab_c_diffSign_high, ab_c_diffSign_low)
  //                             cout                             cin for negative addend
  val adderOut_low_26_temp = Cat(false.B, adderIn_ab_low_24_final, adderIn_cin_low) +
                             Cat(false.B, adderIn_c_low_24_final, adderIn_cin_low)
  val adderOut_low_cout = adderOut_low_26_temp(wResMul16+1)
  val adderOut_low_24 = adderOut_low_26_temp(wResMul16, 1) // wResMul16 bits

  // High 24-bit adder                                      cin: low-cout (fp32) or negative addend (bf/fp16)
  val adderOut_high_25_temp = Cat(adderIn_ab_high_24_final, Mux(res_is_32_S2, adderOut_low_cout, ab_c_diffSign_high)) +
                              Cat(adderIn_c_high_24_final, Mux(res_is_32_S2, adderOut_low_cout, ab_c_diffSign_high))
  val adderOut_high_24 = adderOut_high_25_temp(wResMul16, 1) // wResMul16 bits

  val adderOut_sign_low = Mux(ab_c_diffSign_low, 
                              Mux(abs_ab_gt_c_low, resMul_sign_low_S2, sign_c_low),
                              ab_n_c_n_low)
  val adderOut_sign_high = Mux(ab_c_diffSign_high,
                              Mux(abs_ab_gt_c_whole, resMul_sign_high_S2, sign_c_high),
                              ab_n_c_n_high)
  // 下面处理a*b或者c为inf的情况下，sign的值。但是inf + -inf = nan的情况目前未考虑                           
  val adderOut_sign_high_preS3 = Mux(resMul_is_inf_high_S2, resMul_sign_high_S2,
                                 Mux(is_inf_high_c, sign_c_high, adderOut_sign_high))
  val adderOut_sign_low_preS3 = Mux(resMul_is_inf_low_S2, resMul_sign_low_S2,
                                 Mux(is_inf_low_c, sign_c_low, adderOut_sign_low))

  //--------------------------------------------------
  //---- Below is S3 (pipeline 3) stage:
  //       Adder result shifting and rounding
  //--------------------------------------------------
  val valid_S3 = RegNext(valid_S2, init = false.B)
  val c_in_S3 = RegEnable(c_in_S2, valid_S2)
  val input_is_16_S3 = RegEnable(input_is_16_S2, valid_S2)
  val res_is_32_S3 = RegEnable(res_is_32_S2, valid_S2)
  val res_is_bf16_S3 = RegEnable(res_is_bf16_S2, valid_S2)
  val res_is_fp16_S3 = RegEnable(res_is_fp16_S2, valid_S2)
  val is_inf_low_c_S3 = RegEnable(is_inf_low_c, valid_S2)
  val is_inf_high_c_S3 = RegEnable(is_inf_high_c, valid_S2)
  val resMul_is_zero_low_S3 = RegEnable(resMul_is_zero_low_S2, valid_S2)
  val resMul_is_zero_high_S3 = RegEnable(resMul_is_zero_high_S2, valid_S2)
  val resMul_is_inf_low_S3 = RegEnable(resMul_is_inf_low_S2, valid_S2)
  val resMul_is_inf_high_S3 = RegEnable(resMul_is_inf_high_S2, valid_S2)
  val resMul_is_subnorm_low_S3 = RegEnable(resMul_is_subnorm_low_S2, valid_S2)
  val resMul_is_subnorm_high_S3 = RegEnable(resMul_is_subnorm_high_S2, valid_S2)
  
  val adderOut_low_S3 = RegEnable(adderOut_low_24, valid_S2) // 24 bits
  val adderOut_high_S3 = RegEnable(adderOut_high_24, valid_S2) // 24 bits
  val adderOut_whole_S3 = Cat(adderOut_high_S3, adderOut_low_S3) // 48 bits
  val adderOut_sign_low_S3 = RegEnable(adderOut_sign_low_preS3, valid_S2)
  val adderOut_sign_high_S3 = RegEnable(adderOut_sign_high_preS3, valid_S2)

  val exp_resMul_low_S3 = RegEnable(exp_resMul_low_S2, valid_S2)  // 8 bits
  val exp_resMul_high_S3 = RegEnable(exp_resMul_high_S2, valid_S2) // 8 bits
  val exp_c_low_S3 = RegEnable(exp_adjust_subnorm_c(0), valid_S2) // 8 bits
  val exp_c_high_S3 = RegEnable(exp_adjust_subnorm_c(1), valid_S2) // 8 bits
  val exp_c_gte_ab_low_S3 = RegEnable(exp_c_gte_ab_low, valid_S2)
  val exp_c_gte_ab_high_S3 = RegEnable(exp_c_gte_ab_high, valid_S2)

  val exp_adderOut_low = Mux(exp_c_gte_ab_low_S3, exp_c_low_S3, exp_resMul_low_S3)
  val exp_adderOut_high = Mux(exp_c_gte_ab_high_S3, exp_c_high_S3, exp_resMul_high_S3)

  val adderOut_is_inf_low = resMul_is_inf_low_S3 || is_inf_low_c_S3 ||
                            exp_adderOut_low === Mux(res_is_fp16_S3, "b00011111".U, "b11111111".U)
  val adderOut_is_inf_high = resMul_is_inf_high_S3 || is_inf_high_c_S3 ||
                             exp_adderOut_high === Mux(res_is_fp16_S3, "b00011111".U, "b11111111".U)
  
  val adderOut_int_part_low = adderOut_low_S3(wResMul16-1, wResMul16-2)
  val adderOut_int_part_high = adderOut_high_S3(wResMul16-1, wResMul16-2)
  val adderOut_isZero_low, adderOut_isZero_high = Wire(Bool())

  // ---- Calculate shift amount of adder out----
  // ----  (shift direction is left) ----
  val adderOut_is_subnorm_high, adderOut_is_subnorm_low = Wire(Bool())
  val adderOut_is_inf_high_case1, adderOut_is_inf_low_case1 = Wire(Bool())
  val adderOut_shift_amount_high = if (wResMul32 < 32) Wire(UInt(5.W)) else Wire(UInt(6.W))
  val adderOut_shift_amount_low = if (wResMul16 < 16) Wire(UInt(4.W)) else Wire(UInt(5.W))
  val exp_adderOut_tobe_subtracted_high, exp_adderOut_tobe_subtracted_low = Wire(UInt(8.W))
  val exp_adderOut_high_over_1 = exp_adderOut_high - 1.U // 8 bits
  val exp_adderOut_low_over_1 = exp_adderOut_low - 1.U // 8 bits
  val lzd_adderOut_low = LZD(adderOut_low_S3(wResMul16-1-2, 0)) // 4 bits or 5 bits (wResMul16 >= 16)
  val lzd_adderOut_high = LZD(adderOut_high_S3(wResMul16-1-2, 0)) // 4 bits or 5 bits (wResMul16 >= 16)
  val lzd_adderOut_whole = LZD(adderOut_whole_S3(wResMul32-1-2, 0)) // 5 bits (wResMul32 < 32) or 6 bits (wResMul32 >= 32)
  // Low part
  when (adderOut_int_part_low(1)) { // integer part >= 2
    adderOut_is_subnorm_low := false.B
    adderOut_is_inf_low_case1 := exp_adderOut_low === Mux(res_is_fp16_S3, "b00011110".U, "b11111110".U)
    // 由于只进行左移，所以整数部分大于2时，不移动。整数部分为1时，左移一位。
    // 最终sig的小数点在最高位之后，即 23.22 21 20 --- 1 0
    adderOut_shift_amount_low := 0.U
    // 整数部分大于2时，exp需要加1，相当于减去-1
    exp_adderOut_tobe_subtracted_low := ~0.U(8.W)
    adderOut_isZero_low := false.B
  }.elsewhen (adderOut_int_part_low(0)) { // integer part = 1
    adderOut_is_subnorm_low := false.B
    adderOut_is_inf_low_case1 := false.B
    adderOut_shift_amount_low := 1.U
    exp_adderOut_tobe_subtracted_low := 0.U
    adderOut_isZero_low := false.B
  }.otherwise { // Integer part == 0;
    when(exp_adderOut_low_over_1 <= lzd_adderOut_low) { // exp > 1  &&  (exp-1) <= LZD of fraction
      adderOut_is_subnorm_low := true.B
      adderOut_shift_amount_low := exp_adderOut_low_over_1 + 1.U
      exp_adderOut_tobe_subtracted_low := exp_adderOut_low_over_1
      adderOut_isZero_low := false.B
    }.otherwise {  // exp > 1  &&  (exp-1) > LZD of fraction
      adderOut_is_subnorm_low := false.B
      adderOut_shift_amount_low := lzd_adderOut_low + 2.U
      exp_adderOut_tobe_subtracted_low := lzd_adderOut_low + 1.U
      adderOut_isZero_low := lzd_adderOut_low === (wResMul16-2).U
    }
    adderOut_is_inf_low_case1 := false.B
  }

  // High part
  val lzd_adderOut_high_or_whole = Mux(res_is_32_S3, lzd_adderOut_whole, lzd_adderOut_high)
  when (adderOut_int_part_high(1)) { // integer part >= 2
    adderOut_is_subnorm_high := false.B
    adderOut_is_inf_high_case1 := exp_adderOut_high === Mux(res_is_fp16_S3, "b00011110".U, "b11111110".U)
    // 由于只进行左移，所以整数部分大于2时，不移动。整数部分为1时，左移一位。
    // 最终sig的小数点在最高位之后，即 23.22 21 20 --- 1 0
    adderOut_shift_amount_high := 0.U
    // 整数部分大于2时，exp需要加1，相当于减去-1
    exp_adderOut_tobe_subtracted_high := ~0.U(8.W)
    adderOut_isZero_high := false.B
  }.elsewhen (adderOut_int_part_high(0)) { // integer part = 1
    adderOut_is_subnorm_high := false.B
    adderOut_is_inf_high_case1 := false.B
    adderOut_shift_amount_high := 1.U
    exp_adderOut_tobe_subtracted_high := 0.U
    adderOut_isZero_high := false.B
  }.otherwise { // Integer part == 0;
    when(exp_adderOut_high_over_1 <= lzd_adderOut_high_or_whole) { // exp > 1  &&  (exp-1) <= LZD of fraction
      adderOut_is_subnorm_high := true.B
      adderOut_shift_amount_high := exp_adderOut_high_over_1 + 1.U
      exp_adderOut_tobe_subtracted_high := exp_adderOut_high_over_1
      adderOut_isZero_high := false.B
    }.otherwise {  // exp > 1  &&  (exp-1) > LZD of fraction
      adderOut_is_subnorm_high := false.B
      adderOut_shift_amount_high := lzd_adderOut_high_or_whole + 2.U
      exp_adderOut_tobe_subtracted_high := lzd_adderOut_high_or_whole + 1.U
      adderOut_isZero_high := lzd_adderOut_high_or_whole === Mux(res_is_32_S3, (wResMul32-2).U, (wResMul16-2).U)
    }
    adderOut_is_inf_high_case1 := false.B
  }

  val sig_adderOut_shifted_low = shift_left(adderOut_low_S3, adderOut_shift_amount_low)(wResMul16-1, 0) // wResMul16 bits
  val sig_adderOut_shifted_whole = shift_left(adderOut_whole_S3, adderOut_shift_amount_high)(wResMul32-1, 0) // wResMul32 bits

  val exp_adderOut_shifted_low = exp_adderOut_low - exp_adderOut_tobe_subtracted_low
  val exp_adderOut_shifted_high = exp_adderOut_high - exp_adderOut_tobe_subtracted_high

  //---- Rouding (only RNE) of adder out ----
  //---- (1) Calculate LSB, Guard bit, Sticky bit, and significand
  // Low 16: before rounding: 23.22 21 20 --- 1 0
  // fp16
  val lsb_adderOut_low16_fp16 = sig_adderOut_shifted_low(wResMul16-1-10) // LSB
  val g_adderOut_low16_fp16 = sig_adderOut_shifted_low(wResMul16-1-10-1) // Guard bit
  val s_adderOut_low16_fp16 = sig_adderOut_shifted_low(wResMul16-1-10-2, 0).orR // Sticky bit
  val sigEff_adderOut_low16_fp16 = sig_adderOut_shifted_low(wResMul16-1, wResMul16-1-10) // 11 bits
  // bf16
  val lsb_adderOut_low16_bf16 = sig_adderOut_shifted_low(wResMul16-1-7) // LSB
  val g_adderOut_low16_bf16 = sig_adderOut_shifted_low(wResMul16-1-7-1) // Guard bit
  val s_adderOut_low16_bf16 = sig_adderOut_shifted_low(wResMul16-1-7-2, wResMul16-1-7-4).orR || s_adderOut_low16_fp16 // Sticky bit
  val sigEff_adderOut_low16_bf16 = sig_adderOut_shifted_low(wResMul16-1, wResMul16-1-7) // 8 bits
  // High 16: before rounding: 47.46 45 44 --- 25 24
  val sig_adderOut_shifted_high = sig_adderOut_shifted_whole(wResMul32-1, wResMul16) // wResMul32 bits
  // fp16
  val lsb_adderOut_high16_fp16 = sig_adderOut_shifted_high(wResMul16-1-10) // LSB
  val g_adderOut_high16_fp16 = sig_adderOut_shifted_high(wResMul16-1-10-1) // Guard bit
  val s_adderOut_high16_fp16 = sig_adderOut_shifted_high(wResMul16-1-10-2, 0).orR // Sticky bit
  val sigEff_adderOut_high16_fp16 = sig_adderOut_shifted_high(wResMul16-1, wResMul16-1-10) // 11 bits
  // bf16
  val lsb_adderOut_high16_bf16 = sig_adderOut_shifted_high(wResMul16-1-7) // LSB
  val g_adderOut_high16_bf16 = sig_adderOut_shifted_high(wResMul16-1-7-1) // Guard bit
  val s_adderOut_high16_bf16 = sig_adderOut_shifted_high(wResMul16-1-7-2, wResMul16-1-7-4).orR || s_adderOut_high16_fp16 // Sticky bit
  val sigEff_adderOut_high16_bf16 = sig_adderOut_shifted_high(wResMul16-1, wResMul16-1-7) // 8 bits
  // 32: before rounding: 47.46 45 44 --- 1 0
  val lsb_adderOut_whole32 = sig_adderOut_shifted_whole(wResMul32-1-23) // LSB
  val g_adderOut_whole32 = sig_adderOut_shifted_whole(wResMul32-1-23-1) // Guard bit
  val s_adderOut_whole32 = sig_adderOut_shifted_whole(wResMul32-1-23-2, 0).orR
  val sigEff_adderOut_whole32 = sig_adderOut_shifted_whole(wResMul32-1, wResMul32-1-23) // 24 bits

  //---- (2) Calculate final significand and exponent of result ----
  // Low fp16
  val rnd_cin_low_fp16 = Mux(!g_adderOut_low16_fp16, false.B,
                         Mux(s_adderOut_low16_fp16, true.B, lsb_adderOut_low16_fp16))
  val sigExt_resFinal_low_fp16 = sigEff_adderOut_low16_fp16 +& rnd_cin_low_fp16.asUInt // 12 bits
  val sig_resFinal_low_fp16 = Mux(sigExt_resFinal_low_fp16(11),
                                  sigExt_resFinal_low_fp16(11, 1), sigExt_resFinal_low_fp16(10, 0)) // 11 bits
  val exp_adjust_resFinal_low_fp16 = exp_adderOut_shifted_low + sigExt_resFinal_low_fp16(11).asUInt
  val isInf_resFinal_low_fp16 = sigExt_resFinal_low_fp16(11) && exp_adderOut_shifted_low === "b00011110".U
  val exp_resFinal_low_fp16 = Mux(exp_adjust_resFinal_low_fp16 === 1.U && !sig_resFinal_low_fp16(10), 0.U, exp_adjust_resFinal_low_fp16) // 8 bits
  val sign_resFinal_low_fp16 = adderOut_sign_low_S3
  // low bf16
  val rnd_cin_low_bf16 = Mux(!g_adderOut_low16_bf16, false.B,
                         Mux(s_adderOut_low16_bf16, true.B, lsb_adderOut_low16_bf16))
  val sigExt_resFinal_low_bf16 = sigEff_adderOut_low16_bf16 +& rnd_cin_low_bf16.asUInt //  9 bits
  val sig_resFinal_low_bf16 = Mux(sigExt_resFinal_low_bf16(8),
                                  sigExt_resFinal_low_bf16(8, 1), sigExt_resFinal_low_bf16(7, 0)) // 8 bits
  val exp_adjust_resFinal_low_bf16 = exp_adderOut_shifted_low + sigExt_resFinal_low_bf16(8).asUInt
  val isInf_resFinal_low_bf16 = sigExt_resFinal_low_bf16(8) && exp_adderOut_shifted_low === "b11111110".U
  val exp_resFinal_low_bf16 = Mux(exp_adjust_resFinal_low_bf16 === 1.U && !sig_resFinal_low_bf16(7), 0.U, exp_adjust_resFinal_low_bf16) // 8 bits
  val sign_resFinal_low_bf16 = adderOut_sign_low_S3
  // high fp16
  val rnd_cin_high_fp16 = Mux(!g_adderOut_high16_fp16, false.B,
                         Mux(s_adderOut_high16_fp16, true.B, lsb_adderOut_high16_fp16))
  val sigExt_resFinal_high_fp16 = sigEff_adderOut_high16_fp16 +& rnd_cin_high_fp16.asUInt // 12 bits
  val sig_resFinal_high_fp16 = Mux(sigExt_resFinal_high_fp16(11),
                                  sigExt_resFinal_high_fp16(11, 1), sigExt_resFinal_high_fp16(10, 0)) // 11 bits
  val exp_adjust_resFinal_high_fp16 = exp_adderOut_shifted_high + sigExt_resFinal_high_fp16(11).asUInt
  val isInf_resFinal_high_fp16 = sigExt_resFinal_high_fp16(11) && exp_adderOut_shifted_high === "b00011110".U
  val exp_resFinal_high_fp16 = Mux(exp_adjust_resFinal_high_fp16 === 1.U && !sig_resFinal_high_fp16(10), 0.U, exp_adjust_resFinal_high_fp16) // 8 bits
  val sign_resFinal_high_fp16 = adderOut_sign_high_S3
  // high bf16
  val rnd_cin_high_bf16 = Mux(!g_adderOut_high16_bf16, false.B,
                         Mux(s_adderOut_high16_bf16, true.B, lsb_adderOut_high16_bf16))
  val sigExt_resFinal_high_bf16 = sigEff_adderOut_high16_bf16 +& rnd_cin_high_bf16.asUInt // 9 bits
  val sig_resFinal_high_bf16 = Mux(sigExt_resFinal_high_bf16(8),
                                  sigExt_resFinal_high_bf16(8, 1), sigExt_resFinal_high_bf16(7, 0)) // 8 bits
  val exp_adjust_resFinal_high_bf16 = exp_adderOut_shifted_high + sigExt_resFinal_high_bf16(8).asUInt
  val isInf_resFinal_high_bf16 = sigExt_resFinal_high_bf16(8) && exp_adderOut_shifted_high === "b11111110".U
  val exp_resFinal_high_bf16 = Mux(exp_adjust_resFinal_high_bf16 === 1.U && !sig_resFinal_high_bf16(7), 0.U, exp_adjust_resFinal_high_bf16) // 8 bits
  val sign_resFinal_high_bf16 = adderOut_sign_high_S3
  // high 32
  val rnd_cin_whole32 = Mux(!g_adderOut_whole32, false.B,
                         Mux(s_adderOut_whole32, true.B, lsb_adderOut_whole32))
  val sigExt_resFinal_whole32 = sigEff_adderOut_whole32 +& rnd_cin_whole32.asUInt // 25 bits
  val sig_resFinal_whole32 = Mux(sigExt_resFinal_whole32(24),
                                  sigExt_resFinal_whole32(24, 1), sigExt_resFinal_whole32(23, 0)) // 24 bits
  val exp_adjust_resFinal_whole32 = exp_adderOut_shifted_high + sigExt_resFinal_whole32(24).asUInt
  val isInf_resFinal_whole32 = sigExt_resFinal_whole32(24) && exp_adderOut_shifted_high === "b11111110".U
  val exp_resFinal_whole32 = Mux(exp_adjust_resFinal_whole32 === 1.U && !sig_resFinal_whole32(23), 0.U, exp_adjust_resFinal_whole32) // 8 bits
  val sign_resFinal_whole32 = adderOut_sign_high_S3

  //-----------------------
  //---- Final result -----
  //-----------------------
  val resFinal_whole32_tmp = Cat(sign_resFinal_whole32, exp_resFinal_whole32, sig_resFinal_whole32(22, 0))
  val resFinal_whole32 = Mux(adderOut_isZero_high, Cat(resFinal_whole32_tmp(31), 0.U(31.W)), resFinal_whole32_tmp)
  val resFinal_fp16_high = Cat(sign_resFinal_high_fp16, exp_resFinal_high_fp16(4, 0), sig_resFinal_high_fp16(9, 0))
  val resFinal_fp16_low = Cat(sign_resFinal_low_fp16, exp_resFinal_low_fp16(4, 0), sig_resFinal_low_fp16(9, 0))
  val resFinal_bf16_high = Cat(sign_resFinal_high_bf16, exp_resFinal_high_bf16, sig_resFinal_high_bf16(6, 0))
  val resFinal_bf16_low = Cat(sign_resFinal_low_bf16, exp_resFinal_low_bf16, sig_resFinal_low_bf16(6, 0))
  require(resFinal_whole32_tmp.getWidth == 32 && resFinal_fp16_high.getWidth == 16 &&
          resFinal_fp16_low.getWidth == 16 && resFinal_bf16_high.getWidth == 16 && resFinal_bf16_low.getWidth == 16)
  val resFinal_high16_tmp = Mux(res_is_fp16_S3, resFinal_fp16_high, resFinal_bf16_high)
  val resFinal_high16 = Mux(adderOut_isZero_high, Cat(resFinal_high16_tmp(15), 0.U(15.W)), resFinal_high16_tmp)
  val resFinal_low16_tmp = Mux(res_is_fp16_S3, resFinal_fp16_low, resFinal_bf16_low)
  val resFinal_low16 = Mux(adderOut_isZero_low, Cat(resFinal_low16_tmp(15), 0.U(15.W)), resFinal_low16_tmp)

  //---- Inf ----
  val isInf_resFinal_low = adderOut_is_inf_low || adderOut_is_inf_low_case1 ||
            Mux(res_is_fp16_S3, isInf_resFinal_low_fp16, isInf_resFinal_low_bf16)
  val isInf_resFinal_high = adderOut_is_inf_high || adderOut_is_inf_high_case1 ||
            Mux(res_is_32_S3, isInf_resFinal_whole32,
                Mux(res_is_fp16_S3, isInf_resFinal_high_fp16, isInf_resFinal_high_bf16))

  val res_out_low16 = Mux(resMul_is_zero_low_S3, c_in_S3(15, 0),
                      Mux(isInf_resFinal_low, 
                      Mux(res_is_fp16_S3, sign_resFinal_low_fp16 ## "h7C00".U(15.W), sign_resFinal_low_bf16 ## "h7F80".U(15.W)),
                          resFinal_low16))
  val res_out_high16 = Mux(resMul_is_zero_high_S3, c_in_S3(31, 16),
                       Mux(isInf_resFinal_high,
                       Mux(res_is_fp16_S3, sign_resFinal_high_fp16 ## "h7C00".U(15.W), sign_resFinal_high_bf16 ## "h7F80".U(15.W)),
                           resFinal_high16))
  val res_out_whole32 = Mux(resMul_is_zero_high_S3, c_in_S3,
                        Mux(isInf_resFinal_high,
                            sign_resFinal_whole32 ## "h7F800000".U(31.W),
                            resFinal_whole32))
  
  io.res_out := Mux(res_is_32_S3, res_out_whole32, res_out_high16 ## res_out_low16)
  io.valid_out := valid_S3
  io.valid_S1 := valid_S1
  io.valid_S2 := valid_S2


  def shift(data: UInt, shift_amount: UInt, shift_right: Bool): UInt = {
    // Reverse the data when shifting left
    val reversed_data = Mux(shift_right, data, Cat(data.asBools))
    val shifted_data = reversed_data >> shift_amount
    Mux(shift_right, shifted_data, Cat(shifted_data.asBools))
  }
  def shift_right(data: UInt, shift_amount: UInt): UInt = {
    data >> shift_amount
  }
  def shift_left(data: UInt, shift_amount: UInt): UInt = {
    data << shift_amount
  }
}