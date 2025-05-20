/**
  * FMA supporting bf/fp16 and fp32. Includs:
  *   (1) bf16 -> bf16   (2) fp16 -> fp16   (3) fp32 -> fp32
  *   (4) bf16 -> fp32   (5) fp16 -> fp32
  * Hardware reuse:
  *   Can handle two fp16*fp16 or one fp32*fp32
  * Note: 
  *   1) So far, bf16 is not supported
  *   2) NaN is not supported
  *   3) Rounding mode only supports RNE
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._

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
  
  val exp_high_a, exp_low_a, exp_high_b, exp_low_b = Wire(UInt(8.W))
  exp_high_a := Mux(is_fp16, io.a_in(30, 30-5+1), io.a_in(30, 30-8+1))
  exp_low_a := Mux(is_fp16, io.a_in(14, 14-5+1), io.a_in(14, 14-8+1))
  exp_high_b := Mux(is_fp16, io.b_in(30, 30-5+1), io.b_in(30, 30-8+1))
  exp_low_b := Mux(is_fp16, io.b_in(14, 14-5+1), io.b_in(14, 14-8+1))
  val exp_in = Seq(exp_low_a, exp_low_b, exp_high_a, exp_high_b)

  val frac_high_a_16, frac_low_a_16, frac_high_b_16, frac_low_b_16 = Wire(UInt(10.W))
  frac_high_a_16 := Mux(is_fp16, io.a_in(16+11-2, 16), io.a_in(16+8-2, 16))
  frac_low_a_16 := Mux(is_fp16, io.a_in(0+11-2, 0), io.a_in(0+8-2, 0))
  frac_high_b_16 := Mux(is_fp16, io.b_in(16+11-2, 16), io.b_in(16+8-2, 16))
  frac_low_b_16 := Mux(is_fp16, io.b_in(0+11-2, 0), io.b_in(0+8-2, 0))
  val frac_a_32 = io.a_in(22, 0)
  val frac_b_32 = io.b_in(22, 0)
  val frac_in_16 = Seq(frac_low_a_16, frac_low_b_16, frac_high_a_16, frac_high_b_16)
  val frac_in_32 = Seq(frac_a_32, frac_b_32)

  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_is_0 = Wire(Vec(4, Bool()))
  val exp_is_all1s = Wire(Vec(4, Bool()))
  val frac_is_0_16 = Wire(Vec(4, Bool()))
  exp_is_0 zip Seq(exp_low_a, exp_low_b, exp_high_a, exp_high_b) foreach { case (is_0, exp) =>
    is_0 := exp === 0.U
  }
  frac_is_0_16 zip Seq(frac_low_a_16, frac_low_b_16, frac_high_a_16, frac_high_b_16) foreach { case (is_0, frac) =>
    is_0 := frac === 0.U
  }
  exp_is_all1s zip Seq(exp_low_a, exp_low_b, exp_high_a, exp_high_b) foreach { case (is_all1s, exp) =>
    is_all1s := exp === Mux(is_fp16, "b00011111".U, "b1111_1111".U)
  }
  //----   a, b = 0, 1  ----
  val frac_is_0_32 = Wire(Vec(2, Bool()))
  frac_is_0_32 zip Seq(frac_a_32, frac_b_32) foreach { case (is_0, frac) => is_0 := frac === 0.U}

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
  val sig_adjust_subnorm_16 = Wire(Vec(4, UInt(11.W)))
  for (i <- 0 until 4) {
    sig_adjust_subnorm_16(i) := Mux(is_subnorm(i), 0.U(1.W), 1.U(1.W)) ## frac_in_16(i)
  }
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
                          Cat(sig_adjust_subnorm_16(2).pad(12), sig_adjust_subnorm_16(0).pad(12)))
  intMul_12_24.io.b_in := Mux(!is_16, sig_adjust_subnorm_32(1),
                          Cat(sig_adjust_subnorm_16(3).pad(12), sig_adjust_subnorm_16(1).pad(12)))
  intMul_12_24.io.valid_in := io.valid_in
  intMul_12_24.io.uop_in := io.uop_in
  intMul_12_24.io.is_16 := is_16
  val uop_S1 = intMul_12_24.io.uop_out
  val valid_S1 = intMul_12_24.io.valid_out
  val res_intMul = intMul_12_24.io.res_out























  val is_zero_16_S1 = is_zero_16.map(RegEnable(_, io.valid_in))
  val is_zero_32_S1 = is_zero_32.map(RegEnable(_, io.valid_in))
  val is_inf_16_S1 = is_inf_16.map(RegEnable(_, io.valid_in))
  val is_inf_32_S1 = is_inf_32.map(RegEnable(_, io.valid_in))

  // Inf, Zero, subnormal

}
