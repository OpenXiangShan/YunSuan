package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.yunsuan.util._

/**
  * Fp adder (incomplete) without rounding
  *   Input: 
  *     (1) significand has 1 bit integer part (MSB)
  *     (2) exponent of submormal is already changed to 1, and the significand's integer part is 0 accordingly
  *     (3) significand is extended to SigWidth + ExtendedWidth
  *     (4) if ExtAreZeros is true, the extended part of a and b are all zeros (which is the case of normal add)
  *   Output:
  *     (1) output exponent/significand has same format as input exponent/significand
  *     (2) only support RNE rounding mode
  *     (3) output is 1 bit longer than input (result of addition)
  * 
  *   Note: for shifting operation, the shifted-out part is disgarded
  * Pipeline:  |      
  *       ---->|----->
  *        S0  |  S1  
  */
class FAdd_extSig(
    ExpWidth: Int, // fp16: 5   bf16: 8   fp32: 8
    SigWidth: Int, // fp16: 11  bf16: 8   fp32: 24
    ExtendedWidth: Int, // default: SigWidth + 2,
    ExtAreZeros: Boolean = false
) extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val is_fp16 = Input(Bool()) // Inf of 8-bit exp: fp16: 00011111, bf16: 11111111
    val a, b = Input(UInt((1 + ExpWidth + SigWidth + ExtendedWidth).W))
    val a_is_inf, b_is_inf = Input(Bool())
    val a_is_nan, b_is_nan = Input(Bool())
    val res = Output(UInt((1 + ExpWidth + SigWidth + ExtendedWidth + 1).W))
    val res_is_posInf, res_is_negInf, res_is_nan = Output(Bool())
    val valid_out = Output(Bool())
  })

  require(ExpWidth == 5 || ExpWidth == 8, "Typical ExpWidth of FAdd_extSig is 5 (fp16) or 8 (fp32/bf16)")
  require(SigWidth == 8 || SigWidth == 11 || SigWidth == 24, "Typical SigWidth of FAdd_extSig is 8 (fp16) or 11 (bf16) or 24 (fp32)")

  val sign_a = io.a.head(1).asBool
  val sign_b = io.b.head(1).asBool
  val exp_a = io.a.tail(1).head(ExpWidth)
  val exp_b = io.b.tail(1).head(ExpWidth)
  val sig_a = io.a.tail(1 + ExpWidth)
  val sig_b = io.b.tail(1 + ExpWidth)

  val exp_diff_a_minus_b = exp_a -& exp_b // ExpWidth + 1 bits
  val exp_diff_b_minus_a = exp_b -& exp_a // ExpWidth + 1 bits
  
  //---- Shifting ----
  val exp_a_gte_b = !exp_diff_a_minus_b(ExpWidth)
  // number of bits of shift amount
  val bShiftRight = log2Up(SigWidth + ExtendedWidth)
  val a_dominates = exp_a_gte_b && exp_diff_a_minus_b(ExpWidth, bShiftRight).orR
  val b_dominates = !exp_diff_b_minus_a(ExpWidth) && exp_diff_b_minus_a(ExpWidth, bShiftRight).orR
  val shift_amount_a, shift_amount_b = Wire(UInt(bShiftRight.W))
  when (a_dominates) {
    shift_amount_b := ~0.U(bShiftRight.W)
    shift_amount_a := 0.U
  }.elsewhen (b_dominates) {
    shift_amount_b := 0.U
    shift_amount_a := ~0.U(bShiftRight.W)
  }.elsewhen (exp_a_gte_b) {
    shift_amount_b := exp_diff_a_minus_b(bShiftRight - 1, 0)
    shift_amount_a := 0.U
  }.otherwise {
    shift_amount_b := 0.U
    shift_amount_a := exp_diff_b_minus_a(bShiftRight - 1, 0)
  }
  // Select input of shift block, a or b.
  val shiftRight_in = Mux(exp_a_gte_b, sig_b, sig_a)
  val shiftRight_amount = Mux(exp_a_gte_b, shift_amount_b, shift_amount_a)
  val shiftRight_out = shiftRight_in >> shiftRight_amount // SigWidth + ExtendedWidth bits

  //---- Comparison of absolute value of a and b ----
  // Divide the comparison into two parts: SigWidth part and ExtendedWidth part
  // Part 1 of comparison: exponent + SigWidth part
  val (sigPart_a, sigPart_b) = (sig_a.head(SigWidth), sig_b.head(SigWidth))
  val exp_a_eq_b = exp_diff_a_minus_b === 0.U
  val expSigPart_a_eq_b = exp_a_eq_b && sigPart_a === sigPart_b
  val sigPart_a_gt_b = sigPart_a > sigPart_b
  val exp_a_gt_b = exp_diff_b_minus_a(ExpWidth)
  val abs_expSigPart_a_gt_b = exp_a_gt_b || exp_a_eq_b && sigPart_a_gt_b
  // Part 2 of comparison: ExtendedWidth part
  val (extPart_a, extPart_b) = (sig_a.tail(SigWidth), sig_b.tail(SigWidth))
  val extPart_a_gt_b = extPart_a > extPart_b
  // Final result of comparison: part1 and part2
  val abs_a_gt_b = if (ExtAreZeros) {
    abs_expSigPart_a_gt_b
  } else {
    abs_expSigPart_a_gt_b || expSigPart_a_eq_b && extPart_a_gt_b
  }

  //---- Adder input ----
  val adderIn_a = Mux(exp_a_gte_b, sig_a, shiftRight_out)
  val adderIn_b = Mux(!exp_a_gte_b, sig_b, shiftRight_out)

  //---- Inf and NaN ----
  val res_is_nan_S0 = io.a_is_nan || io.b_is_nan || io.a_is_inf && io.b_is_inf && sign_a =/= sign_b
  val res_is_posInf_S0 = !res_is_nan_S0 && (io.a_is_inf && !sign_a || io.b_is_inf && !sign_b)
  val res_is_negInf_S0 = !res_is_nan_S0 && (io.a_is_inf && sign_a || io.b_is_inf && sign_b)

  //----------------------------------------------
  //  Below is the second stage: S1 (pipeline 1)
  //----------------------------------------------
  io.valid_out := RegNext(io.valid_in)
  val is_fp16_S1 = RegEnable(io.is_fp16, io.valid_in)
  val a_in_S1 = RegEnable(io.a, io.valid_in)
  val b_in_S1 = RegEnable(io.b, io.valid_in)
  val sign_a_S1 = RegEnable(sign_a, io.valid_in)
  val sign_b_S1 = RegEnable(sign_b, io.valid_in)
  val exp_a_S1 = RegEnable(exp_a, io.valid_in)
  val exp_b_S1 = RegEnable(exp_b, io.valid_in)
  val abs_a_gt_b_S1 = RegEnable(abs_a_gt_b, io.valid_in)
  val adderIn_a_S1 = 0.U(1.W) ## RegEnable(adderIn_a, io.valid_in)
  val adderIn_b_S1 = 0.U(1.W) ## RegEnable(adderIn_b, io.valid_in)
  val exp_a_gte_b_S1 = RegEnable(exp_a_gte_b, io.valid_in)

  val a_n_b_n = sign_a_S1 && sign_b_S1  // a < 0 && b < 0
  val a_p_b_n = !sign_a_S1 && sign_b_S1 // a > 0 && b < 0
  val a_n_b_p = sign_a_S1 && !sign_b_S1 // a < 0 && b > 0
  val a_b_diffSign = a_n_b_p || a_p_b_n

  // 对于负数，要取反加一。本设计保证让绝对值较小的那个数取反加1，这样sig结果为正数，避免了结果为负时还需要再对结果取反加一。
  // 在a与b符号相反的情况下，若(1) a绝对值 > b绝对值，b取反加一，结果符号位与a符号位相同；
  //                     否则(2) a绝对值 <= b绝对值，a取反加一，结果符号位与b符号位相同。
  val adderIn_a_inv = Mux(a_b_diffSign && !abs_a_gt_b_S1, ~adderIn_a_S1, adderIn_a_S1)
  val adderIn_b_inv = Mux(a_b_diffSign && abs_a_gt_b_S1, ~adderIn_b_S1, adderIn_b_S1)
  val adderIn_cin = a_b_diffSign
  
  // ---- Addition ----
  val adderOut_temp = Cat(adderIn_a_inv, adderIn_cin) + Cat(adderIn_b_inv, adderIn_cin)
  //  2 + (SigWidth - 1) + ExtendedWidth
  val adderOut = adderOut_temp(SigWidth + ExtendedWidth + 1, 1) // SigWidth + ExtendedWidth + 1 bits
  
  val sign_adderOut = Mux(a_b_diffSign, Mux(abs_a_gt_b_S1, sign_a_S1, sign_b_S1), a_n_b_n)
  val exp_adderOut = Mux(exp_a_gte_b_S1, exp_a_S1, exp_b_S1)
  val adderOut_int_part = adderOut.head(2)
  val adderOut_isZero = Wire(Bool())
  
  // ---- Calculate shift amount of adder out----
  // ----  (shift direction is left) ----
  val adderOut_is_subnorm, adderOut_is_inf = Wire(Bool())
  val bShiftLeft = log2Up(SigWidth + ExtendedWidth + 1)
  val shiftLeft_amount = Wire(UInt(bShiftLeft.W))
  val exp_adderOut_tobe_subtracted = Wire(UInt(ExpWidth.W))
  val exp_adderOut_over_1 = exp_adderOut - 1.U
  val lzd_adderOut = LZD(adderOut.tail(2))
  when (adderOut_int_part(1)) { // integer part >= 2
    adderOut_is_subnorm := false.B
    adderOut_is_inf := exp_adderOut === Mux(is_fp16_S1, "b00011110".U, ~1.U(ExpWidth.W))
    // 由于只进行左移，所以整数部分大于2时，不移动。整数部分为1时，左移一位。
    // 最终sig的小数点在最高位之后，即 x.x (SigWidth-1) ExtendedWidth
    shiftLeft_amount := 0.U
    // 整数部分大于2时，exp需要加1，相当于减去-1
    exp_adderOut_tobe_subtracted := ~0.U(ExpWidth.W)
    adderOut_isZero := false.B
  }.elsewhen (adderOut_int_part(0)) { // integer part = 1
    adderOut_is_subnorm := false.B
    adderOut_is_inf := false.B
    shiftLeft_amount := 1.U
    exp_adderOut_tobe_subtracted := 0.U
    adderOut_isZero := false.B
  }.otherwise { // Integer part == 0;
    when(exp_adderOut_over_1 <= lzd_adderOut) { // exp > 1  &&  (exp-1) <= LZD of fraction
      adderOut_is_subnorm := true.B
      shiftLeft_amount := exp_adderOut_over_1 + 1.U
      exp_adderOut_tobe_subtracted := exp_adderOut_over_1
      adderOut_isZero := false.B
    }.otherwise {  // exp > 1  &&  (exp-1) > LZD of fraction
      adderOut_is_subnorm := false.B
      shiftLeft_amount := lzd_adderOut + 2.U
      exp_adderOut_tobe_subtracted := lzd_adderOut + 1.U
      adderOut_isZero := lzd_adderOut === (SigWidth + ExtendedWidth - 1).U
    }
    adderOut_is_inf := false.B
  }

  val sig_adderOut_shifted = (adderOut << shiftLeft_amount)(SigWidth + ExtendedWidth, 0) // SigWidth + ExtendedWidth + 1 bits

  val exp_adderOut_shifted = exp_adderOut - exp_adderOut_tobe_subtracted

  //                                                           1 + ExpWidth + (SigWidth + ExtendedWidth + 1)
  io.res := Mux(adderOut_isZero, 0.U, Cat(sign_adderOut, exp_adderOut_shifted, sig_adderOut_shifted))

  io.res_is_nan := RegEnable(res_is_nan_S0, io.valid_in)
  io.res_is_posInf := RegEnable(res_is_posInf_S0, io.valid_in) || adderOut_is_inf && !sign_adderOut
  io.res_is_negInf := RegEnable(res_is_negInf_S0, io.valid_in) || adderOut_is_inf && sign_adderOut
}

// object VerilogFAdd_extSig extends App {
//   println("Generating the FAdd_extSig hardware")
//   emitVerilog(new FAdd_extSig(ExpWidth = 8, SigWidth = 24, ExtendedWidth = 24 + 2), Array("--target-dir", "build/verilog_fadd_extsig"))
// }