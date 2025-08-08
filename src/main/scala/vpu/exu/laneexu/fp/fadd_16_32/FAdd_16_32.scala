/**
  * FAdd supporting bf/fp16 and fp32. Includs:
  *   (1) bf16 -> bf16   (2) fp16 -> fp16   (3) fp32 -> fp32
  *   (4) bf16 -> fp32   (5) fp16 -> fp32
  * Hardware reuse:
  *   One fp19 adder and one fp32 adder
  * Scenario:
  *   AI, vector processing in LLM, etc.
  * Note: 
  *   1) For widen instrn, input bf/fp16 should be the highest half of the 32-bit input
  *   2) Rounding mode only supports RNE
  * Pipeline: |      |
  *      ---->|----->|----->
  *       S0  |  S1  |  S2
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._

class FAdd_16_32(
  ExtendedWidthFp19: Int = 10 + 1 + 2, // Tunable parameter: trade-off between area and precision
  ExtendedWidthFp32: Int = 23 + 1 + 2
) extends Module {
  val SigWidthFp19 = 10 + 1  // Fixed
  val SigWidthFp32 = 23 + 1  // Fixed
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val is_bf16, is_fp16, is_fp32 = Input(Bool())
    val is_widen = Input(Bool())
    val a_already_widen = Input(Bool()) // a already widened to fp32 (b not)
    val a, b = Input(UInt(32.W))  // a: vs2   b: vs1/rs1
    val res = Output(UInt(32.W))
    val valid_out = Output(Bool())
    val valid_S1 = Output(Bool())
  })

  val (is_bf16, is_fp16, is_fp32) = (io.is_bf16, io.is_fp16, io.is_fp32)
  val is_16 = is_fp16 || is_bf16
  val widen = io.is_widen
  val res_is_32 = widen || is_fp32
  val res_is_bf16 = is_bf16 && !widen
  val res_is_fp16 = is_fp16 && !widen
  val (sign_low_a, sign_low_b, sign_high_a, sign_high_b) = (io.a(15), io.b(15), io.a(31), io.b(31))

  val exp_high_a, exp_low_a, exp_high_b, exp_low_b = Wire(UInt(8.W))
  exp_high_a := Mux(is_fp16, io.a(30, 30-5+1), io.a(30, 30-8+1))
  exp_low_a := Mux(is_fp16, io.a(14, 14-5+1), io.a(14, 14-8+1))
  exp_high_b := Mux(is_fp16, io.b(30, 30-5+1), io.b(30, 30-8+1))
  exp_low_b := Mux(is_fp16, io.b(14, 14-5+1), io.b(14, 14-8+1))
  val exp_in = Seq(exp_low_a, exp_low_b, exp_high_a, exp_high_b)

  val frac_high_a_16, frac_low_a_16, frac_high_b_16, frac_low_b_16 = Wire(UInt(10.W))
  frac_high_a_16 := Mux(is_fp16, io.a(16+11-2, 16), Cat(io.a(16+8-2, 16), 0.U(3.W)))
  frac_low_a_16 := Mux(is_fp16, io.a(0+11-2, 0), Cat(io.a(0+8-2, 0), 0.U(3.W)))
  frac_high_b_16 := Mux(is_fp16, io.b(16+11-2, 16), Cat(io.b(16+8-2, 16), 0.U(3.W)))
  frac_low_b_16 := Mux(is_fp16, io.b(0+11-2, 0), Cat(io.b(0+8-2, 0), 0.U(3.W)))
  val frac_a_32 = io.a(22, 0)
  val frac_b_32 = io.b(22, 0)
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

  // val is_subnorm_16 = exp_is_0 zip frac_is_0_16 map {case (is_0, is_0_frac) => is_0 && !is_0_frac}
  // val is_subnorm_32 = exp_is_0.drop(2) zip frac_is_0_32 map {case (is_0, is_0_frac) => is_0 && !is_0_frac}
  val is_subnorm_16 = exp_is_0 // Subnormal and Zero (treat Zero as subnormal)
  val is_subnorm_32 = exp_is_0.drop(2) // Subnormal and Zero (treat Zero as subnormal)
  // val is_zero_16 = exp_is_0 zip frac_is_0_16 map {case (is_0, is_0_frac) => is_0 && is_0_frac}
  // val is_zero_32 = exp_is_0.drop(2) zip frac_is_0_32 map {case (is_0, is_0_frac) => is_0 && is_0_frac}
  val is_inf_16 = exp_is_all1s zip frac_is_0_16 map {case (is_all1s, is_0_frac) => is_all1s && is_0_frac}
  val is_inf_32 = exp_is_all1s.drop(2) zip frac_is_0_32 map {case (is_all1s, is_0_frac) => is_all1s && is_0_frac}
  val is_nan_16 = exp_is_all1s zip frac_is_0_16 map {case (is_all1s, is_0_frac) => is_all1s && !is_0_frac}
  val is_nan_32 = exp_is_all1s.drop(2) zip frac_is_0_32 map {case (is_all1s, is_0_frac) => is_all1s && !is_0_frac}

  val is_subnorm = Mux(is_16, is_subnorm_16, VecInit(Seq(false.B, false.B) ++ is_subnorm_32))

  //----   low_a, low_b, high_a, high_b = 0, 1, 2, 3   ----
  val exp_adjust_subnorm = Wire(Vec(4, UInt(8.W)))
  val is_fp16_widen = is_fp16 && widen
  for (i <- 0 until 2) {
      exp_adjust_subnorm(i) := Mux(is_subnorm(i), 1.U, exp_in(i))
  }
  exp_adjust_subnorm(2) := Mux(is_subnorm(2), 1.U, exp_in(2)) + Mux(is_fp16_widen && !io.a_already_widen, (127 - 15).U, 0.U)
  exp_adjust_subnorm(3) := Mux(is_subnorm(3), 1.U, exp_in(3)) + Mux(is_fp16_widen, (127 - 15).U, 0.U)
    
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

  //---- fp19 adder (low) + fp32 adder (low) ----
  val fadd_extSig_fp19 = Module(new FAdd_extSig(ExpWidth = 8, SigWidth = SigWidthFp19, ExtendedWidth = ExtendedWidthFp19, ExtAreZeros = true, UseShiftRightJam = true))
  fadd_extSig_fp19.io.valid_in := io.valid_in
  fadd_extSig_fp19.io.is_fp16 := is_fp16
  fadd_extSig_fp19.io.a.sign := sign_low_a
  fadd_extSig_fp19.io.a.exp := exp_adjust_subnorm(0)
  fadd_extSig_fp19.io.a.sig := sig_adjust_subnorm_16(0) ## 0.U(ExtendedWidthFp19.W)
  fadd_extSig_fp19.io.b.sign := sign_low_b
  fadd_extSig_fp19.io.b.exp := exp_adjust_subnorm(1)
  fadd_extSig_fp19.io.b.sig := sig_adjust_subnorm_16(1) ## 0.U(ExtendedWidthFp19.W)
  fadd_extSig_fp19.io.a_is_inf := is_inf_16(0)
  fadd_extSig_fp19.io.b_is_inf := is_inf_16(1)
  fadd_extSig_fp19.io.a_is_nan := is_nan_16(0)
  fadd_extSig_fp19.io.b_is_nan := is_nan_16(1)
  
  val fadd_extSig_fp32 = Module(new FAdd_extSig(ExpWidth = 8, SigWidth = SigWidthFp32, ExtendedWidth = ExtendedWidthFp32, ExtAreZeros = true, UseShiftRightJam = true))
  fadd_extSig_fp32.io.valid_in := io.valid_in
  fadd_extSig_fp32.io.is_fp16 := res_is_fp16
  val sig_adjust_subnorm_high_a = Mux(is_16 && !io.a_already_widen, sig_adjust_subnorm_16(2) ## 0.U(13.W), sig_adjust_subnorm_32(0))
  val sig_adjust_subnorm_high_b = Mux(is_16, sig_adjust_subnorm_16(3) ## 0.U(13.W), sig_adjust_subnorm_32(1))
  fadd_extSig_fp32.io.a.sign := sign_high_a
  fadd_extSig_fp32.io.a.exp := exp_adjust_subnorm(2)
  fadd_extSig_fp32.io.a.sig := sig_adjust_subnorm_high_a ## 0.U(ExtendedWidthFp32.W)
  fadd_extSig_fp32.io.b.sign := sign_high_b
  fadd_extSig_fp32.io.b.exp := exp_adjust_subnorm(3)
  fadd_extSig_fp32.io.b.sig := sig_adjust_subnorm_high_b ## 0.U(ExtendedWidthFp32.W)
  fadd_extSig_fp32.io.a_is_inf := Mux(is_16, is_inf_16(2), is_inf_32(0))
  fadd_extSig_fp32.io.b_is_inf := Mux(is_16, is_inf_16(3), is_inf_32(1))
  fadd_extSig_fp32.io.a_is_nan := Mux(is_16, is_nan_16(2), is_nan_32(0))
  fadd_extSig_fp32.io.b_is_nan := Mux(is_16, is_nan_16(3), is_nan_32(1))

  //-----------------------------------------
  //---- Second stage: S1 (pipeline 1)   ----
  //-----------------------------------------
  val valid_S1 = fadd_extSig_fp19.io.valid_out
  val res_extSig_fp19_S1 = fadd_extSig_fp19.io.res
  val res_extSig_fp32_S1 = fadd_extSig_fp32.io.res

  val res_is_posInf_low_S1 = fadd_extSig_fp19.io.res_is_posInf
  val res_is_negInf_low_S1 = fadd_extSig_fp19.io.res_is_negInf
  val res_is_nan_low_S1 = fadd_extSig_fp19.io.res_is_nan
  val res_is_posInf_high_S1 = fadd_extSig_fp32.io.res_is_posInf
  val res_is_negInf_high_S1 = fadd_extSig_fp32.io.res_is_negInf
  val res_is_nan_high_S1 = fadd_extSig_fp32.io.res_is_nan

  val res_is_32_S1 = RegEnable(res_is_32, io.valid_in)
  val res_is_bf16_S1 = RegEnable(res_is_bf16, io.valid_in)
  val res_is_fp16_S1 = RegEnable(res_is_fp16, io.valid_in)

  //-----------------------------------------
  //---- Third stage: S2 (pipeline 2)   ----
  //-----------------------------------------
  val valid_S2 = RegNext(valid_S1)
  val res_extSig_fp19_S2 = RegEnable(res_extSig_fp19_S1, valid_S1)
  val res_extSig_fp32_S2 = RegEnable(res_extSig_fp32_S1, valid_S1)
  val res_is_32_S2 = RegEnable(res_is_32_S1, valid_S1)
  val res_is_bf16_S2 = RegEnable(res_is_bf16_S1, valid_S1)
  val res_is_fp16_S2 = RegEnable(res_is_fp16_S1, valid_S1)

  val res_is_posInf_high_S2 = RegEnable(res_is_posInf_high_S1, valid_S1)
  val res_is_negInf_high_S2 = RegEnable(res_is_negInf_high_S1, valid_S1)
  val res_is_nan_high_S2 = RegEnable(res_is_nan_high_S1, valid_S1)
  val res_is_posInf_low_S2 = RegEnable(res_is_posInf_low_S1, valid_S1)
  val res_is_negInf_low_S2 = RegEnable(res_is_negInf_low_S1, valid_S1)
  val res_is_nan_low_S2 = RegEnable(res_is_nan_low_S1, valid_S1)

  val (sign_res_extSig_fp19, sign_res_extSig_fp32) = (res_extSig_fp19_S2.sign, res_extSig_fp32_S2.sign)
  val (exp_res_extSig_fp19, exp_res_extSig_fp32) = (res_extSig_fp19_S2.exp, res_extSig_fp32_S2.exp)
  val (sig_res_extSig_fp19, sig_res_extSig_fp32) = (res_extSig_fp19_S2.sig, res_extSig_fp32_S2.sig)

  //---- Rouding (only RNE) of adder out ----
  //---- (1) Calculate LSB, Guard bit, Sticky bit, and significand
  // Low fp16/bf16
  val lsb_adderOut_low_fp16 = sig_res_extSig_fp19(ExtendedWidthFp19 + 1)
  val g_adderOut_low_fp16 = sig_res_extSig_fp19(ExtendedWidthFp19)
  val s_adderOut_low_fp16 = sig_res_extSig_fp19(ExtendedWidthFp19 - 1, 0).orR
  val sig_adderOut_low_fp16 = sig_res_extSig_fp19.head(SigWidthFp19)

  val lsb_adderOut_low_bf16 = sig_res_extSig_fp19(ExtendedWidthFp19 + 1 + 3)
  val g_adderOut_low_bf16 = sig_res_extSig_fp19(ExtendedWidthFp19 + 3)
  val s_adderOut_low_bf16 = sig_res_extSig_fp19(ExtendedWidthFp19 + 3 - 1, ExtendedWidthFp19).orR ||
                             s_adderOut_low_fp16
  val sig_adderOut_low_bf16 = sig_res_extSig_fp19.head(SigWidthFp19 - 3)

  // High fp32/fp16/bf16
  val lsb_adderOut_high_fp32 = sig_res_extSig_fp32(ExtendedWidthFp32 + 1)
  val g_adderOut_high_fp32 = sig_res_extSig_fp32(ExtendedWidthFp32)
  val s_adderOut_high_fp32 = sig_res_extSig_fp32(ExtendedWidthFp32 - 1, 0).orR
  val sig_adderOut_high_fp32 = sig_res_extSig_fp32.head(SigWidthFp32)

  val lsb_adderOut_high_fp16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 14)
  val g_adderOut_high_fp16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 13)
  val s_adderOut_high_fp16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 13 - 1, ExtendedWidthFp32).orR ||
                            s_adderOut_high_fp32
  val sig_adderOut_high_fp16 = sig_res_extSig_fp32.head(SigWidthFp32 - 13)

  val lsb_adderOut_high_bf16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 14 + 3)
  val g_adderOut_high_bf16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 13 + 3)
  val s_adderOut_high_bf16 = sig_res_extSig_fp32(ExtendedWidthFp32 + 13 + 2, ExtendedWidthFp32 + 13).orR ||
                            s_adderOut_high_fp16
  val sig_adderOut_high_bf16 = sig_res_extSig_fp32.head(SigWidthFp32 - 13 - 3)
  
  //---- (2) Calculate final significand and exponent of result ----
  // Low fp16/bf16
  val rnd_cin_low_fp16 = Mux(!g_adderOut_low_fp16, false.B,
                          Mux(s_adderOut_low_fp16, true.B, lsb_adderOut_low_fp16))
  val rnd_cin_low_bf16 = Mux(!g_adderOut_low_bf16, false.B,
                          Mux(s_adderOut_low_bf16, true.B, lsb_adderOut_low_bf16))
  val sig_res_low_tmp = sig_adderOut_low_fp16 +&
            Mux(res_is_fp16_S2, rnd_cin_low_fp16.asUInt, rnd_cin_low_bf16.asUInt << 3) // SigWidthFp19 + 1 bits
  val sig_res_low = Mux(sig_res_low_tmp(SigWidthFp19),
                         sig_res_low_tmp(SigWidthFp19, 1), sig_res_low_tmp(SigWidthFp19 - 1, 0)) // SigWidthFp19 bits
  val exp_adjust_res_low = exp_res_extSig_fp19 + sig_res_low_tmp(SigWidthFp19).asUInt // 8 bits
  val isInf_res_low = sig_res_low_tmp(SigWidthFp19) &&
            Mux(!res_is_fp16_S2, exp_res_extSig_fp19 === "b11111110".U, exp_res_extSig_fp19 === "b00011110".U)
  val exp_res_low = Mux(exp_adjust_res_low === 1.U && !sig_res_low(SigWidthFp19 - 1), 0.U, exp_adjust_res_low) // 8 bits

  // High fp32/fp16/bf16
  val rnd_cin_high_fp32 = Mux(!g_adderOut_high_fp32, false.B,
                          Mux(s_adderOut_high_fp32, true.B, lsb_adderOut_high_fp32))
  val rnd_cin_high_fp16 = Mux(!g_adderOut_high_fp16, false.B,
                          Mux(s_adderOut_high_fp16, true.B, lsb_adderOut_high_fp16))
  val rnd_cin_high_bf16 = Mux(!g_adderOut_high_bf16, false.B,
                          Mux(s_adderOut_high_bf16, true.B, lsb_adderOut_high_bf16))
  val sig_res_high_tmp = sig_adderOut_high_fp32 +& Mux(res_is_32_S2, rnd_cin_high_fp32.asUInt,
            Mux(res_is_fp16_S2, rnd_cin_high_fp16.asUInt << 13, rnd_cin_high_bf16.asUInt << 16)) // SigWidthFp32 + 1 bits
  val sig_res_high = Mux(sig_res_high_tmp(SigWidthFp32),
                        sig_res_high_tmp(SigWidthFp32, 1), sig_res_high_tmp(SigWidthFp32 - 1, 0)) // SigWidthFp32 bits
  val exp_adjust_res_high = exp_res_extSig_fp32 + sig_res_high_tmp(SigWidthFp32).asUInt // 8 bits
  val isInf_res_high = sig_res_high_tmp(SigWidthFp32) &&
            Mux(!res_is_fp16_S2, exp_res_extSig_fp32 === "b11111110".U, exp_res_extSig_fp32 === "b00011110".U)
  val exp_res_high = Mux(exp_adjust_res_high === 1.U && !sig_res_high(SigWidthFp32 - 1), 0.U, exp_adjust_res_high) // 8 bits

  //-----------------------------------------
  //---- Final result -----
  //-----------------------------------------
  val resFinal_fp16_low_tmp = Cat(sign_res_extSig_fp19, exp_res_low(4, 0), sig_res_low(SigWidthFp19 - 2, 0))
  val resFinal_bf16_low_tmp = Cat(sign_res_extSig_fp19, exp_res_low, sig_res_low(SigWidthFp19 - 2, 3))
  val resFinal_32_high_tmp = Cat(sign_res_extSig_fp32, exp_res_high, sig_res_high(SigWidthFp32 - 2, 0))
  val resFinal_fp16_high_tmp = Cat(sign_res_extSig_fp32, exp_res_high(4, 0), sig_res_high(SigWidthFp32 - 2, 13))
  val resFinal_bf16_high_tmp = Cat(sign_res_extSig_fp32, exp_res_high, sig_res_high(SigWidthFp32 - 2, 16))

  // val resFinal_is_posInf_high = isInf_res_high || res_is_posInf_high_S2
  // val resFinal_is_negInf_high = isInf_res_high || res_is_negInf_high_S2
  // val resFinal_is_posInf_low = isInf_res_low || res_is_posInf_low_S2
  // val resFinal_is_negInf_low = isInf_res_low || res_is_negInf_low_S2

  val resFinal_32_high = MuxCase(resFinal_32_high_tmp, Seq(
          res_is_nan_high_S2 -> "h7FC00000".U,
          res_is_posInf_high_S2 -> "h7F800000".U,
          res_is_negInf_high_S2 -> "hFF800000".U,
          isInf_res_high -> sign_res_extSig_fp32 ## ~0.U(8.W) ## 0.U(23.W)
  ))
  val resFinal_bf16_high = MuxCase(resFinal_bf16_high_tmp, Seq(
          res_is_nan_high_S2 -> "h7FC0".U,
          res_is_posInf_high_S2 -> "h7F80".U,
          res_is_negInf_high_S2 -> "hFF80".U,
          isInf_res_high -> sign_res_extSig_fp32 ## ~0.U(8.W) ## 0.U(7.W)
  ))
  val resFinal_fp16_high = MuxCase(resFinal_fp16_high_tmp, Seq(
          res_is_nan_high_S2 -> "h7E00".U,
          res_is_posInf_high_S2 -> "h7C00".U,
          res_is_negInf_high_S2 -> "hFC00".U,
          isInf_res_high -> sign_res_extSig_fp32 ## ~0.U(5.W) ## 0.U(10.W)
  ))
  val resFinal_bf16_low = MuxCase(resFinal_bf16_low_tmp, Seq(
          res_is_nan_low_S2 -> "h7FC0".U,
          res_is_posInf_low_S2 -> "h7F80".U,
          res_is_negInf_low_S2 -> "hFF80".U,
          isInf_res_low -> sign_res_extSig_fp19 ## ~0.U(8.W) ## 0.U(7.W)
  ))
  val resFinal_fp16_low = MuxCase(resFinal_fp16_low_tmp, Seq(
          res_is_nan_low_S2 -> "h7E00".U,
          res_is_posInf_low_S2 -> "h7C00".U,
          res_is_negInf_low_S2 -> "hFC00".U,
          isInf_res_low -> sign_res_extSig_fp19 ## ~0.U(5.W) ## 0.U(10.W)
  ))

  // val resFinal_bf16_high = Mux(res_is_nan_high_S2, "h7FC0".U, Mux(resFinal_is_posInf_high, "h7F80".U, Mux(resFinal_is_negInf_high, "hFF80".U, resFinal_bf16_high_tmp)))
  // val resFinal_fp16_high = Mux(res_is_nan_high_S2, "h7E00".U, Mux(resFinal_is_posInf_high, "h7C00".U, Mux(resFinal_is_negInf_high, "hFC00".U, resFinal_fp16_high_tmp)))
  // val resFinal_bf16_low = Mux(res_is_nan_low_S2, "h7FC0".U, Mux(resFinal_is_posInf_low, "h7F80".U, Mux(resFinal_is_negInf_low, "hFF80".U, resFinal_bf16_low_tmp)))
  // val resFinal_fp16_low = Mux(res_is_nan_low_S2, "h7E00".U, Mux(resFinal_is_posInf_low, "h7C00".U, Mux(resFinal_is_negInf_low, "hFC00".U, resFinal_fp16_low_tmp)))

  io.res := Mux(res_is_32_S2, resFinal_32_high,
            Mux(res_is_fp16_S2, Cat(resFinal_fp16_high, resFinal_fp16_low),
                Cat(resFinal_bf16_high, resFinal_bf16_low)))
  io.valid_out := valid_S2
  io.valid_S1 := valid_S1
}

object VerilogFAdd_16_32 extends App {
  println("Generating the FAdd_16_32 hardware")
  emitVerilog(new FAdd_16_32, Array("--target-dir", "build/verilog_fadd_16_32"))
}