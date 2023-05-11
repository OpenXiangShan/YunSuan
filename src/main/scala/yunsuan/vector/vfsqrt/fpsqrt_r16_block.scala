package yunsuan.vector.vfsqrt

import chisel3._
import chisel3.util._
import yunsuan.vector.vfsqrt._

class fpsqrt_r16_block(
                        S0_CSA_SPECULATIVE: Int = 0,
                        S0_CSA_MERGED: Int = 1,
                        S1_QDS_SPECULATIVE: Int = 1,
                        S1_CSA_SPECULATIVE: Int = 1,
                        S1_CSA_MERGED: Int = 0,
                        RT_DIG_W: Int = 5
                      ) extends Module {

  val REM_W = if ((S0_CSA_SPECULATIVE == 0) & (S0_CSA_MERGED == 1)) 70 else 64
  val fp_fmt_i = IO(Input(UInt(3.W)))
  val f_r_s_i = IO(Input(UInt(REM_W.W)))
  val f_r_c_i = IO(Input(UInt(REM_W.W)))
  val rt_i = IO(Input(UInt(56.W)))
  val rt_m1_i = IO(Input(UInt(53.W)))
  val mask_i = IO(Input(UInt(13.W)))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_0_i = IO(Input(UInt(7.W)))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_1_i = IO(Input(UInt(7.W)))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_2_i = IO(Input(UInt(7.W)))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_3_i = IO(Input(UInt(7.W)))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_0_i = IO(Input(UInt(9.W)))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_1_i = IO(Input(UInt(9.W)))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_2_i = IO(Input(UInt(9.W)))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_3_i = IO(Input(UInt(9.W)))
  val m_neg_1_for_nxt_cycle_s0_qds_0_i = IO(Input(UInt(5.W)))
  val m_neg_1_for_nxt_cycle_s0_qds_1_i = IO(Input(UInt(5.W)))
  val m_neg_1_for_nxt_cycle_s0_qds_2_i = IO(Input(UInt(5.W)))
  val m_neg_1_for_nxt_cycle_s0_qds_3_i = IO(Input(UInt(5.W)))
  val m_neg_0_for_nxt_cycle_s0_qds_0_i = IO(Input(UInt(4.W)))
  val m_neg_0_for_nxt_cycle_s0_qds_1_i = IO(Input(UInt(4.W)))
  val m_neg_0_for_nxt_cycle_s0_qds_2_i = IO(Input(UInt(4.W)))
  val m_neg_0_for_nxt_cycle_s0_qds_3_i = IO(Input(UInt(4.W)))
  val m_pos_1_for_nxt_cycle_s0_qds_0_i = IO(Input(UInt(3.W)))
  val m_pos_1_for_nxt_cycle_s0_qds_1_i = IO(Input(UInt(3.W)))
  val m_pos_1_for_nxt_cycle_s0_qds_2_i = IO(Input(UInt(3.W)))
  val m_pos_1_for_nxt_cycle_s0_qds_3_i = IO(Input(UInt(3.W)))
  val m_pos_2_for_nxt_cycle_s0_qds_0_i = IO(Input(UInt(4.W)))
  val m_pos_2_for_nxt_cycle_s0_qds_1_i = IO(Input(UInt(4.W)))
  val m_pos_2_for_nxt_cycle_s0_qds_2_i = IO(Input(UInt(4.W)))
  val m_pos_2_for_nxt_cycle_s0_qds_3_i = IO(Input(UInt(4.W)))
  val nxt_rt_o = IO(Output(UInt(56.W)))
  val nxt_rt_m1_o = IO(Output(UInt(53.W)))
  val nxt_f_r_s_o = IO(Output(UInt(REM_W.W)))
  val nxt_f_r_c_o = IO(Output(UInt(REM_W.W)))
  val adder_7b_res_for_nxt_cycle_s0_qds_0_o = IO(Output(UInt(7.W)))
  val adder_7b_res_for_nxt_cycle_s0_qds_1_o = IO(Output(UInt(7.W)))
  val adder_7b_res_for_nxt_cycle_s0_qds_2_o = IO(Output(UInt(7.W)))
  val adder_7b_res_for_nxt_cycle_s0_qds_3_o = IO(Output(UInt(7.W)))
  val adder_9b_res_for_nxt_cycle_s1_qds_0_o = IO(Output(UInt(9.W)))
  val adder_9b_res_for_nxt_cycle_s1_qds_1_o = IO(Output(UInt(9.W)))
  val adder_9b_res_for_nxt_cycle_s1_qds_2_o = IO(Output(UInt(9.W)))
  val adder_9b_res_for_nxt_cycle_s1_qds_3_o = IO(Output(UInt(9.W)))
  val m_neg_1_to_nxt_cycle_0_o = IO(Output(UInt(7.W)))
  val m_neg_1_to_nxt_cycle_1_o = IO(Output(UInt(7.W)))
  val m_neg_1_to_nxt_cycle_2_o = IO(Output(UInt(7.W)))
  val m_neg_1_to_nxt_cycle_3_o = IO(Output(UInt(7.W)))
  val m_neg_0_to_nxt_cycle_0_o = IO(Output(UInt(7.W)))
  val m_neg_0_to_nxt_cycle_1_o = IO(Output(UInt(7.W)))
  val m_neg_0_to_nxt_cycle_2_o = IO(Output(UInt(7.W)))
  val m_neg_0_to_nxt_cycle_3_o = IO(Output(UInt(7.W)))
  val m_pos_1_to_nxt_cycle_0_o = IO(Output(UInt(7.W)))
  val m_pos_1_to_nxt_cycle_1_o = IO(Output(UInt(7.W)))
  val m_pos_1_to_nxt_cycle_2_o = IO(Output(UInt(7.W)))
  val m_pos_1_to_nxt_cycle_3_o = IO(Output(UInt(7.W)))
  val m_pos_2_to_nxt_cycle_0_o = IO(Output(UInt(7.W)))
  val m_pos_2_to_nxt_cycle_1_o = IO(Output(UInt(7.W)))
  val m_pos_2_to_nxt_cycle_2_o = IO(Output(UInt(7.W)))
  val m_pos_2_to_nxt_cycle_3_o = IO(Output(UInt(7.W)))

  val F64_REM_W = 2 + 54
  val F32_REM_W = 2 + 26
  val F16_REM_W = 2 + 14
  val MERGED_REM_W = (2 + F16_REM_W) * 3 + F16_REM_W
  val F64_FULL_RT_W = F64_REM_W - 1
  val F32_FULL_RT_W = F32_REM_W - 1
  val F16_FULL_RT_W = F16_REM_W - 1
  val S0_CSA_IS_MERGED = if ((S0_CSA_SPECULATIVE == 0) & (S0_CSA_MERGED == 1)) 1 else 0
  val S1_CSA_IS_MERGED = if ((S1_CSA_SPECULATIVE == 0) & (S1_CSA_MERGED == 1)) 1 else 0
  val rt_0 = Wire(UInt(F64_FULL_RT_W.W))
  val rt_m1_0 = Wire(UInt(F64_FULL_RT_W.W))
  val rt_for_csa_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val rt_m1_for_csa_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val rt_1 = Wire(UInt(F32_FULL_RT_W.W))
  val rt_m1_1 = Wire(UInt(F32_FULL_RT_W.W))
  val rt_for_csa_1 = Wire(Vec(2, UInt(F32_FULL_RT_W.W)))
  val rt_m1_for_csa_1 = Wire(Vec(2, UInt(F32_FULL_RT_W.W)))
  val rt_2 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_m1_2 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_3 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_m1_3 = Wire(UInt(F16_FULL_RT_W.W))
  val nxt_rt_spec_s0_0 = Wire(Vec(5, UInt(F64_FULL_RT_W.W)))
  val nxt_rt_spec_s0_1 = Wire(Vec(5, UInt(F32_FULL_RT_W.W)))
  val nxt_rt_spec_s0_2 = Wire(Vec(5, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_spec_s0_3 = Wire(Vec(5, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_spec_s1_0 = Wire(Vec(5, UInt(F64_FULL_RT_W.W)))
  val nxt_rt_spec_s1_1 = Wire(Vec(5, UInt(F32_FULL_RT_W.W)))
  val nxt_rt_spec_s1_2 = Wire(Vec(5, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_spec_s1_3 = Wire(Vec(5, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val nxt_rt_m1_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val nxt_rt_1 = Wire(Vec(2, UInt(F32_FULL_RT_W.W)))
  val nxt_rt_m1_1 = Wire(Vec(2, UInt(F32_FULL_RT_W.W)))
  val nxt_rt_2 = Wire(Vec(2, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_m1_2 = Wire(Vec(2, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_3 = Wire(Vec(2, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_m1_3 = Wire(Vec(2, UInt(F16_FULL_RT_W.W)))
  val nxt_rt_dig_0 = Wire(Vec(2, UInt(RT_DIG_W.W)))
  val nxt_rt_dig_1 = Wire(Vec(2, UInt(RT_DIG_W.W)))
  val nxt_rt_dig_2 = Wire(Vec(2, UInt(RT_DIG_W.W)))
  val nxt_rt_dig_3 = Wire(Vec(2, UInt(RT_DIG_W.W)))
  val mask_csa_ext = Wire(Vec(2, UInt(F64_REM_W.W)))
  val mask_csa_neg_2 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val mask_csa_neg_1 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val mask_csa_pos_1 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val mask_csa_pos_2 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val mask_rt_ext = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_neg_2 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_neg_1 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_neg_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_pos_1 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_pos_2 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_m1_neg_2 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_m1_neg_1 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_m1_neg_0 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_m1_pos_1 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val mask_rt_m1_pos_2 = Wire(Vec(2, UInt(F64_FULL_RT_W.W)))
  val f_r_s_for_csa_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val f_r_c_for_csa_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val f_r_s_for_csa_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val f_r_c_for_csa_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val f_r_s_for_csa_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val f_r_c_for_csa_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val f_r_s_for_csa_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val f_r_c_for_csa_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val nxt_f_r_s_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  nxt_f_r_s_0 := 0.U.asTypeOf(nxt_f_r_s_0)
  val nxt_f_r_c_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  nxt_f_r_c_0 := 0.U.asTypeOf(nxt_f_r_c_0)
  val nxt_f_r_c_pre_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  nxt_f_r_c_pre_0 := 0.U.asTypeOf(nxt_f_r_c_pre_0)
  val nxt_f_r_s_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  nxt_f_r_s_1 := 0.U.asTypeOf(nxt_f_r_s_1)
  val nxt_f_r_c_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  nxt_f_r_c_1 := 0.U.asTypeOf(nxt_f_r_c_1)
  val nxt_f_r_c_pre_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  nxt_f_r_c_pre_1 := 0.U.asTypeOf(nxt_f_r_c_pre_1)
  val nxt_f_r_s_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_s_2 := 0.U.asTypeOf(nxt_f_r_s_2)
  val nxt_f_r_c_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_c_2 := 0.U.asTypeOf(nxt_f_r_c_2)
  val nxt_f_r_c_pre_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_c_pre_2 := 0.U.asTypeOf(nxt_f_r_c_pre_2)
  val nxt_f_r_s_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_s_3 := 0.U.asTypeOf(nxt_f_r_s_3)
  val nxt_f_r_c_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_c_3 := 0.U.asTypeOf(nxt_f_r_c_3)
  val nxt_f_r_c_pre_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  nxt_f_r_c_pre_3 := 0.U.asTypeOf(nxt_f_r_c_pre_3)
  val nxt_f_r_s_merged = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  nxt_f_r_s_merged := 0.U.asTypeOf(nxt_f_r_s_merged)
  val nxt_f_r_c_merged = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  nxt_f_r_c_merged := 0.U.asTypeOf(nxt_f_r_c_merged)
  val nxt_f_r_c_merged_pre = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  nxt_f_r_c_merged_pre := 0.U.asTypeOf(nxt_f_r_c_merged_pre)
  val f_r_s_for_csa_merged = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  f_r_s_for_csa_merged := 0.U.asTypeOf(f_r_s_for_csa_merged)
  val f_r_c_for_csa_merged = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  f_r_c_for_csa_merged := 0.U.asTypeOf(f_r_c_for_csa_merged)
  val nxt_f_r_s_spec_s0_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_c_spec_s0_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_c_pre_spec_s0_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_s_spec_s1_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_c_spec_s1_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_c_pre_spec_s1_0 = Wire(Vec(5, UInt(F64_REM_W.W)))
  val nxt_f_r_s_spec_s0_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_c_spec_s0_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_c_pre_spec_s0_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_s_spec_s1_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_c_spec_s1_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_c_pre_spec_s1_1 = Wire(Vec(5, UInt(F32_REM_W.W)))
  val nxt_f_r_s_spec_s0_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_spec_s0_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_pre_spec_s0_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_s_spec_s1_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_spec_s1_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_pre_spec_s1_2 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_s_spec_s0_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_spec_s0_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_pre_spec_s0_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_s_spec_s1_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_spec_s1_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val nxt_f_r_c_pre_spec_s1_3 = Wire(Vec(5, UInt(F16_REM_W.W)))
  val sqrt_csa_val_neg_2_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val sqrt_csa_val_neg_2_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val sqrt_csa_val_neg_2_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_neg_2_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_neg_1_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val sqrt_csa_val_neg_1_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val sqrt_csa_val_neg_1_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_neg_1_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_pos_1_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val sqrt_csa_val_pos_1_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val sqrt_csa_val_pos_1_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_pos_1_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_pos_2_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val sqrt_csa_val_pos_2_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val sqrt_csa_val_pos_2_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_pos_2_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_0 = Wire(Vec(2, UInt(F64_REM_W.W)))
  val sqrt_csa_val_1 = Wire(Vec(2, UInt(F32_REM_W.W)))
  val sqrt_csa_val_2 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_3 = Wire(Vec(2, UInt(F16_REM_W.W)))
  val sqrt_csa_val_merged = Wire(Vec(2, UInt(MERGED_REM_W.W)))
  sqrt_csa_val_merged := 0.U.asTypeOf(sqrt_csa_val_merged)
  val a0_spec_s0_0 = Wire(Vec(5, Bool()))
  val a0_spec_s0_1 = Wire(Vec(5, Bool()))
  val a0_spec_s0_2 = Wire(Vec(5, Bool()))
  val a0_spec_s0_3 = Wire(Vec(5, Bool()))
  val a0_spec_s1_0 = Wire(Vec(5, Bool()))
  val a0_spec_s1_1 = Wire(Vec(5, Bool()))
  val a0_spec_s1_2 = Wire(Vec(5, Bool()))
  val a0_spec_s1_3 = Wire(Vec(5, Bool()))
  val a2_spec_s0_0 = Wire(Vec(5, Bool()))
  val a2_spec_s0_1 = Wire(Vec(5, Bool()))
  val a2_spec_s0_2 = Wire(Vec(5, Bool()))
  val a2_spec_s0_3 = Wire(Vec(5, Bool()))
  val a2_spec_s1_0 = Wire(Vec(5, Bool()))
  val a2_spec_s1_1 = Wire(Vec(5, Bool()))
  val a2_spec_s1_2 = Wire(Vec(5, Bool()))
  val a2_spec_s1_3 = Wire(Vec(5, Bool()))
  val a3_spec_s0_0 = Wire(Vec(5, Bool()))
  val a3_spec_s0_1 = Wire(Vec(5, Bool()))
  val a3_spec_s0_2 = Wire(Vec(5, Bool()))
  val a3_spec_s0_3 = Wire(Vec(5, Bool()))
  val a3_spec_s1_0 = Wire(Vec(5, Bool()))
  val a3_spec_s1_1 = Wire(Vec(5, Bool()))
  val a3_spec_s1_2 = Wire(Vec(5, Bool()))
  val a3_spec_s1_3 = Wire(Vec(5, Bool()))
  val a4_spec_s0_0 = Wire(Vec(5, Bool()))
  val a4_spec_s0_1 = Wire(Vec(5, Bool()))
  val a4_spec_s0_2 = Wire(Vec(5, Bool()))
  val a4_spec_s0_3 = Wire(Vec(5, Bool()))
  val a4_spec_s1_0 = Wire(Vec(5, Bool()))
  val a4_spec_s1_1 = Wire(Vec(5, Bool()))
  val a4_spec_s1_2 = Wire(Vec(5, Bool()))
  val a4_spec_s1_3 = Wire(Vec(5, Bool()))
  val m_neg_1_0 = Wire(Vec(2, UInt(7.W)))
  val m_neg_1_1 = Wire(Vec(2, UInt(7.W)))
  val m_neg_1_2 = Wire(Vec(2, UInt(7.W)))
  val m_neg_1_3 = Wire(Vec(2, UInt(7.W)))
  val m_neg_0_0 = Wire(Vec(2, UInt(7.W)))
  val m_neg_0_1 = Wire(Vec(2, UInt(7.W)))
  val m_neg_0_2 = Wire(Vec(2, UInt(7.W)))
  val m_neg_0_3 = Wire(Vec(2, UInt(7.W)))
  val m_pos_1_0 = Wire(Vec(2, UInt(7.W)))
  val m_pos_1_1 = Wire(Vec(2, UInt(7.W)))
  val m_pos_1_2 = Wire(Vec(2, UInt(7.W)))
  val m_pos_1_3 = Wire(Vec(2, UInt(7.W)))
  val m_pos_2_0 = Wire(Vec(2, UInt(7.W)))
  val m_pos_2_1 = Wire(Vec(2, UInt(7.W)))
  val m_pos_2_2 = Wire(Vec(2, UInt(7.W)))
  val m_pos_2_3 = Wire(Vec(2, UInt(7.W)))
  val m_neg_1_spec_s0_0 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s0_1 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s0_2 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s0_3 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s1_0 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s1_1 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s1_2 = Wire(Vec(5, UInt(7.W)))
  val m_neg_1_spec_s1_3 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s0_0 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s0_1 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s0_2 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s0_3 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s1_0 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s1_1 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s1_2 = Wire(Vec(5, UInt(7.W)))
  val m_neg_0_spec_s1_3 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s0_0 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s0_1 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s0_2 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s0_3 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s1_0 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s1_1 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s1_2 = Wire(Vec(5, UInt(7.W)))
  val m_pos_1_spec_s1_3 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s0_0 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s0_1 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s0_2 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s0_3 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s1_0 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s1_1 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s1_2 = Wire(Vec(5, UInt(7.W)))
  val m_pos_2_spec_s1_3 = Wire(Vec(5, UInt(7.W)))
  val adder_9b_for_nxt_cycle_s0_qds_spec_0 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_nxt_cycle_s0_qds_spec_1 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_nxt_cycle_s0_qds_spec_2 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_nxt_cycle_s0_qds_spec_3 = Wire(Vec(5, UInt(9.W)))
  val adder_10b_for_nxt_cycle_s1_qds_spec_0 = Wire(Vec(5, UInt(10.W)))
  val adder_10b_for_nxt_cycle_s1_qds_spec_1 = Wire(Vec(5, UInt(10.W)))
  val adder_10b_for_nxt_cycle_s1_qds_spec_2 = Wire(Vec(5, UInt(10.W)))
  val adder_10b_for_nxt_cycle_s1_qds_spec_3 = Wire(Vec(5, UInt(10.W)))
  val adder_9b_for_s1_qds_spec_0 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_s1_qds_spec_1 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_s1_qds_spec_2 = Wire(Vec(5, UInt(9.W)))
  val adder_9b_for_s1_qds_spec_3 = Wire(Vec(5, UInt(9.W)))
  val adder_7b_res_for_s1_qds_0 = Wire(UInt(7.W))
  val adder_7b_res_for_s1_qds_1 = Wire(UInt(7.W))
  val adder_7b_res_for_s1_qds_2 = Wire(UInt(7.W))
  val adder_7b_res_for_s1_qds_3 = Wire(UInt(7.W))
  mask_csa_ext(0) := Cat(
    "b0".U(2.W),
    "b0".U(3.W), mask_i(12),
    "b0".U(3.W), mask_i(11),
    "b0".U(3.W), mask_i(10),
    "b0".U(3.W), mask_i(9),
    "b0".U(3.W), mask_i(8),
    "b0".U(3.W), mask_i(7),
    "b0".U(3.W), mask_i(6),
    "b0".U(3.W), mask_i(5),
    "b0".U(3.W), mask_i(4),
    "b0".U(3.W), mask_i(3),
    "b0".U(3.W), mask_i(2),
    "b0".U(3.W), mask_i(1),
    "b0".U(3.W), mask_i(0),
    "b0".U(2.W)
  )
  mask_csa_neg_2(0) := (mask_csa_ext(0) << 2) | (mask_csa_ext(0) << 3)
  mask_csa_neg_1(0) := mask_csa_ext(0) | (mask_csa_ext(0) << 1) | (mask_csa_ext(0) << 2)
  mask_csa_pos_1(0) := mask_csa_ext(0)
  mask_csa_pos_2(0) := mask_csa_ext(0) << 2
  mask_rt_ext(0) := mask_csa_ext(0)(0 + F64_FULL_RT_W - 1, 0)
  mask_rt_neg_2(0) := mask_rt_ext(0) << 1
  mask_rt_neg_1(0) := mask_rt_ext(0) | (mask_rt_ext(0) << 1)
  mask_rt_neg_0(0) := 0.U
  mask_rt_pos_1(0) := mask_rt_ext(0)
  mask_rt_pos_2(0) := mask_rt_ext(0) << 1
  mask_rt_m1_neg_2(0) := mask_rt_ext(0)
  mask_rt_m1_neg_1(0) := mask_rt_ext(0) << 1
  mask_rt_m1_neg_0(0) := mask_rt_ext(0) | (mask_rt_ext(0) << 1)
  mask_rt_m1_pos_1(0) := 0.U
  mask_rt_m1_pos_2(0) := mask_rt_ext(0)
  mask_csa_ext(1) := mask_csa_ext(0) >> 2
  mask_csa_neg_2(1) := (mask_csa_ext(1) << 2) | (mask_csa_ext(1) << 3)
  mask_csa_neg_1(1) := mask_csa_ext(1) | (mask_csa_ext(1) << 1) | (mask_csa_ext(1) << 2)
  mask_csa_pos_1(1) := mask_csa_ext(1)
  mask_csa_pos_2(1) := mask_csa_ext(1) << 2
  mask_rt_ext(1) := mask_rt_ext(0) >> 2
  mask_rt_neg_2(1) := mask_rt_ext(1) << 1
  mask_rt_neg_1(1) := mask_rt_ext(1) | (mask_rt_ext(1) << 1)
  mask_rt_neg_0(1) := 0.U
  mask_rt_pos_1(1) := mask_rt_ext(1)
  mask_rt_pos_2(1) := mask_rt_ext(1) << 1
  mask_rt_m1_neg_2(1) := mask_rt_ext(1)
  mask_rt_m1_neg_1(1) := mask_rt_ext(1) << 1
  mask_rt_m1_neg_0(1) := mask_rt_ext(1) | (mask_rt_ext(1) << 1)
  mask_rt_m1_pos_1(1) := 0.U
  mask_rt_m1_pos_2(1) := mask_rt_ext(1)
  m_neg_1_0(0) := Cat("b0".U(2.W), m_neg_1_for_nxt_cycle_s0_qds_0_i)
  m_neg_0_0(0) := Cat("b0".U(3.W), m_neg_0_for_nxt_cycle_s0_qds_0_i)
  m_pos_1_0(0) := Cat("b1111".U(4.W), m_pos_1_for_nxt_cycle_s0_qds_0_i)
  m_pos_2_0(0) := Cat("b11".U(2.W), m_pos_2_for_nxt_cycle_s0_qds_0_i, "b0".U(1.W))
  val u_r4_qds_s0_0 = Module(new r4_qds())
  u_r4_qds_s0_0.rem_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_0_i
  u_r4_qds_s0_0.m_neg_1_i <> m_neg_1_0(0)
  u_r4_qds_s0_0.m_neg_0_i <> m_neg_0_0(0)
  u_r4_qds_s0_0.m_pos_1_i <> m_pos_1_0(0)
  u_r4_qds_s0_0.m_pos_2_i <> m_pos_2_0(0)
  u_r4_qds_s0_0.rt_dig_o <> nxt_rt_dig_0(0)

  m_neg_1_1(0) := Cat("b0".U(2.W), m_neg_1_for_nxt_cycle_s0_qds_1_i)
  m_neg_0_1(0) := Cat("b0".U(3.W), m_neg_0_for_nxt_cycle_s0_qds_1_i)
  m_pos_1_1(0) := Cat("b1111".U(4.W), m_pos_1_for_nxt_cycle_s0_qds_1_i)
  m_pos_2_1(0) := Cat("b11".U(2.W), m_pos_2_for_nxt_cycle_s0_qds_1_i, "b0".U(1.W))
  val u_r4_qds_s0_1 = Module(new r4_qds())
  u_r4_qds_s0_1.rem_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_1_i
  u_r4_qds_s0_1.m_neg_1_i <> m_neg_1_1(0)
  u_r4_qds_s0_1.m_neg_0_i <> m_neg_0_1(0)
  u_r4_qds_s0_1.m_pos_1_i <> m_pos_1_1(0)
  u_r4_qds_s0_1.m_pos_2_i <> m_pos_2_1(0)
  u_r4_qds_s0_1.rt_dig_o <> nxt_rt_dig_1(0)

  m_neg_1_2(0) := Cat("b0".U(2.W), m_neg_1_for_nxt_cycle_s0_qds_2_i)
  m_neg_0_2(0) := Cat("b0".U(3.W), m_neg_0_for_nxt_cycle_s0_qds_2_i)
  m_pos_1_2(0) := Cat("b1111".U(4.W), m_pos_1_for_nxt_cycle_s0_qds_2_i)
  m_pos_2_2(0) := Cat("b11".U(2.W), m_pos_2_for_nxt_cycle_s0_qds_2_i, "b0".U(1.W))
  val u_r4_qds_s0_2 = Module(new r4_qds())
  u_r4_qds_s0_2.rem_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_2_i
  u_r4_qds_s0_2.m_neg_1_i <> m_neg_1_2(0)
  u_r4_qds_s0_2.m_neg_0_i <> m_neg_0_2(0)
  u_r4_qds_s0_2.m_pos_1_i <> m_pos_1_2(0)
  u_r4_qds_s0_2.m_pos_2_i <> m_pos_2_2(0)
  u_r4_qds_s0_2.rt_dig_o <> nxt_rt_dig_2(0)

  m_neg_1_3(0) := Cat("b0".U(2.W), m_neg_1_for_nxt_cycle_s0_qds_3_i)
  m_neg_0_3(0) := Cat("b0".U(3.W), m_neg_0_for_nxt_cycle_s0_qds_3_i)
  m_pos_1_3(0) := Cat("b1111".U(4.W), m_pos_1_for_nxt_cycle_s0_qds_3_i)
  m_pos_2_3(0) := Cat("b11".U(2.W), m_pos_2_for_nxt_cycle_s0_qds_3_i, "b0".U(1.W))
  val u_r4_qds_s0_3 = Module(new r4_qds())
  u_r4_qds_s0_3.rem_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_3_i
  u_r4_qds_s0_3.m_neg_1_i <> m_neg_1_3(0)
  u_r4_qds_s0_3.m_neg_0_i <> m_neg_0_3(0)
  u_r4_qds_s0_3.m_pos_1_i <> m_pos_1_3(0)
  u_r4_qds_s0_3.m_pos_2_i <> m_pos_2_3(0)
  u_r4_qds_s0_3.rt_dig_o <> nxt_rt_dig_3(0)

  rt_0 := Cat(~rt_i(55), rt_i(55, 2))
  rt_m1_0 := Cat("b0".U(1.W), "b1".U(1.W), rt_m1_i(52, 0))
  rt_1 := Cat(~rt_i(27), rt_i(27, 2))
  rt_m1_1 := Cat("b0".U(1.W), "b1".U(1.W), rt_m1_i(26, 2))
  rt_2 := Cat(~rt_i(41), rt_i(41, 28))
  rt_m1_2 := Cat("b0".U(1.W), "b1".U(1.W), rt_m1_i(39, 27))
  rt_3 := Cat(~rt_i(13), rt_i(13, 0))
  rt_m1_3 := Cat("b0".U(1.W), "b1".U(1.W), rt_m1_i(13, 1))
  rt_for_csa_0(0) := Cat(
    rt_0(54, 40),
    Mux(fp_fmt_i(0), "b0".U(2.W), rt_0(39, 38)),
    rt_0(37, 28),
    Mux(fp_fmt_i(1), "b0".U(2.W), rt_0(27, 26)),
    rt_0(25, 0)
  )
  rt_m1_for_csa_0(0) := Cat(
    rt_m1_0(54, 40),
    Mux(fp_fmt_i(0), "b0".U(2.W), rt_m1_0(39, 38)),
    rt_m1_0(37, 28),
    Mux(fp_fmt_i(1), "b0".U(2.W), rt_m1_0(27, 26)),
    rt_m1_0(25, 0)
  )
  rt_for_csa_1(0) := Cat(
    rt_1(26, 12),
    Mux(fp_fmt_i(0), "b0".U(2.W), rt_1(11, 10)),
    rt_1(9, 0)
  )
  rt_m1_for_csa_1(0) := Cat(
    rt_m1_1(26, 12),
    Mux(fp_fmt_i(0), "b0".U(2.W), rt_m1_1(11, 10)),
    rt_m1_1(9, 0)
  )
  sqrt_csa_val_neg_2_0(0) := (Cat("b0".U(1.W), rt_m1_for_csa_0(0)) << 2) | mask_csa_neg_2(0)
  sqrt_csa_val_neg_1_0(0) := (Cat("b0".U(1.W), rt_m1_for_csa_0(0)) << 1) | mask_csa_neg_1(0)
  sqrt_csa_val_pos_1_0(0) := ~((Cat("b0".U(1.W), rt_for_csa_0(0)) << 1) | mask_csa_pos_1(0))
  sqrt_csa_val_pos_2_0(0) := ~((Cat("b0".U(1.W), rt_for_csa_0(0)) << 2) | mask_csa_pos_2(0))
  sqrt_csa_val_0(0) :=
    (Fill(F64_REM_W, nxt_rt_dig_0(0)(4)) & sqrt_csa_val_neg_2_0(0)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(0)(3)) & sqrt_csa_val_neg_1_0(0)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(0)(1)) & sqrt_csa_val_pos_1_0(0)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(0)(0)) & sqrt_csa_val_pos_2_0(0))
  f_r_s_for_csa_0(0) := Cat(
    f_r_s_i(61, 48),
    Mux(fp_fmt_i(0), "b00".U(2.W), f_r_s_i(47, 46)),
    f_r_s_i(45, 36),
    Mux(fp_fmt_i(1), "b00".U(2.W), f_r_s_i(35, 34)),
    f_r_s_i(33, 8),
    "b00".U(2.W)
  )
  f_r_c_for_csa_0(0) := Cat(
    f_r_c_i(61, 48),
    Mux(fp_fmt_i(0), "b00".U(2.W), f_r_c_i(47, 46)),
    f_r_c_i(45, 36),
    Mux(fp_fmt_i(1), "b00".U(2.W), f_r_c_i(35, 34)),
    f_r_c_i(33, 8),
    "b00".U(2.W)
  )
  nxt_f_r_s_spec_s0_0(4) :=
    f_r_s_for_csa_0(0) ^
      f_r_c_for_csa_0(0) ^
      sqrt_csa_val_neg_2_0(0)
  nxt_f_r_c_pre_spec_s0_0(4) := Cat(
    (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_0(0)((F64_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_0(4) := Cat(
    nxt_f_r_c_pre_spec_s0_0(4)(55, 41),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_0(4)(40)),
    nxt_f_r_c_pre_spec_s0_0(4)(39, 29),
    Mux(fp_fmt_i(1), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_0(4)(28)),
    nxt_f_r_c_pre_spec_s0_0(4)(27, 0)
  )
  nxt_f_r_s_spec_s0_0(3) :=
    f_r_s_for_csa_0(0) ^
      f_r_c_for_csa_0(0) ^
      sqrt_csa_val_neg_1_0(0)
  nxt_f_r_c_pre_spec_s0_0(3) := Cat(
    (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_0(0)((F64_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_0(3) := Cat(
    nxt_f_r_c_pre_spec_s0_0(3)(55, 41),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_0(3)(40)),
    nxt_f_r_c_pre_spec_s0_0(3)(39, 29),
    Mux(fp_fmt_i(1), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_0(3)(28)),
    nxt_f_r_c_pre_spec_s0_0(3)(27, 0)
  )
  nxt_f_r_s_spec_s0_0(2) := f_r_s_for_csa_0(0)
  nxt_f_r_c_pre_spec_s0_0(2) := f_r_c_for_csa_0(0)
  nxt_f_r_c_spec_s0_0(2) := nxt_f_r_c_pre_spec_s0_0(2)
  nxt_f_r_s_spec_s0_0(1) :=
    f_r_s_for_csa_0(0) ^
      f_r_c_for_csa_0(0) ^
      sqrt_csa_val_pos_1_0(0)
  nxt_f_r_c_pre_spec_s0_0(1) := Cat(
    (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_0(0)((F64_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_0(1) := Cat(
    nxt_f_r_c_pre_spec_s0_0(1)(55, 41),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_0(1)(40)),
    nxt_f_r_c_pre_spec_s0_0(1)(39, 29),
    Mux(fp_fmt_i(1), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_0(1)(28)),
    nxt_f_r_c_pre_spec_s0_0(1)(27, 0)
  )
  nxt_f_r_s_spec_s0_0(0) :=
    f_r_s_for_csa_0(0) ^
      f_r_c_for_csa_0(0) ^
      sqrt_csa_val_pos_2_0(0)
  nxt_f_r_c_pre_spec_s0_0(0) := Cat(
    (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_0(0)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_0(0)((F64_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_0(0) := Cat(
    nxt_f_r_c_pre_spec_s0_0(0)(55, 41),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_0(0)(40)),
    nxt_f_r_c_pre_spec_s0_0(0)(39, 29),
    Mux(fp_fmt_i(1), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_0(0)(28)),
    nxt_f_r_c_pre_spec_s0_0(0)(27, 0)
  )
  sqrt_csa_val_neg_2_1(0) := (Cat("b0".U(1.W), rt_m1_for_csa_1(0)) << 2) | mask_csa_neg_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1)
  sqrt_csa_val_neg_1_1(0) := (Cat("b0".U(1.W), rt_m1_for_csa_1(0)) << 1) | mask_csa_neg_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1)
  sqrt_csa_val_pos_1_1(0) := ~((Cat("b0".U(1.W), rt_for_csa_1(0)) << 1) | mask_csa_pos_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1))
  sqrt_csa_val_pos_2_1(0) := ~((Cat("b0".U(1.W), rt_for_csa_1(0)) << 2) | mask_csa_pos_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1))
  sqrt_csa_val_1(0) :=
    (Fill(F32_REM_W, nxt_rt_dig_1(0)(4)) & sqrt_csa_val_neg_2_1(0)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(0)(3)) & sqrt_csa_val_neg_1_1(0)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(0)(1)) & sqrt_csa_val_pos_1_1(0)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(0)(0)) & sqrt_csa_val_pos_2_1(0))
  f_r_s_for_csa_1(0) := Cat(
    f_r_s_i(29, 16),
    Mux(fp_fmt_i(0), "b00".U(2.W), f_r_s_i(15, 14)),
    f_r_s_i(13, 4),
    "b00".U(2.W)
  )
  f_r_c_for_csa_1(0) := Cat(
    f_r_c_i(29, 16),
    Mux(fp_fmt_i(0), "b00".U(2.W), f_r_c_i(15, 14)),
    f_r_c_i(13, 4),
    "b00".U(2.W)
  )
  nxt_f_r_s_spec_s0_1(4) :=
    f_r_s_for_csa_1(0) ^
      f_r_c_for_csa_1(0) ^
      sqrt_csa_val_neg_2_1(0)
  nxt_f_r_c_pre_spec_s0_1(4) := Cat(
    (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_1(0)((F32_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_1(4) := Cat(
    nxt_f_r_c_pre_spec_s0_1(4)(27, 13),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_1(4)(12)),
    nxt_f_r_c_pre_spec_s0_1(4)(11, 0)
  )
  nxt_f_r_s_spec_s0_1(3) :=
    f_r_s_for_csa_1(0) ^
      f_r_c_for_csa_1(0) ^
      sqrt_csa_val_neg_1_1(0)
  nxt_f_r_c_pre_spec_s0_1(3) := Cat(
    (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_1(0)((F32_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_1(3) := Cat(
    nxt_f_r_c_pre_spec_s0_1(3)(27, 13),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s0_1(3)(12)),
    nxt_f_r_c_pre_spec_s0_1(3)(11, 0)
  )
  nxt_f_r_s_spec_s0_1(2) := f_r_s_for_csa_1(0)
  nxt_f_r_c_pre_spec_s0_1(2) := f_r_c_for_csa_1(0)
  nxt_f_r_c_spec_s0_1(2) := nxt_f_r_c_pre_spec_s0_1(2)
  nxt_f_r_s_spec_s0_1(1) :=
    f_r_s_for_csa_1(0) ^
      f_r_c_for_csa_1(0) ^
      sqrt_csa_val_pos_1_1(0)
  nxt_f_r_c_pre_spec_s0_1(1) := Cat(
    (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_1(0)((F32_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_1(1) := Cat(
    nxt_f_r_c_pre_spec_s0_1(1)(27, 13),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_1(1)(12)),
    nxt_f_r_c_pre_spec_s0_1(1)(11, 0)
  )
  nxt_f_r_s_spec_s0_1(0) :=
    f_r_s_for_csa_1(0) ^
      f_r_c_for_csa_1(0) ^
      sqrt_csa_val_pos_2_1(0)
  nxt_f_r_c_pre_spec_s0_1(0) := Cat(
    (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_1(0)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_1(0)((F32_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_1(0) := Cat(
    nxt_f_r_c_pre_spec_s0_1(0)(27, 13),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s0_1(0)(12)),
    nxt_f_r_c_pre_spec_s0_1(0)(11, 0)
  )
  sqrt_csa_val_neg_2_2(0) := (Cat("b0".U(1.W), rt_m1_2) << 2) | mask_csa_neg_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_neg_1_2(0) := (Cat("b0".U(1.W), rt_m1_2) << 1) | mask_csa_neg_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_pos_1_2(0) := ~((Cat("b0".U(1.W), rt_2) << 1) | mask_csa_pos_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_pos_2_2(0) := ~((Cat("b0".U(1.W), rt_2) << 2) | mask_csa_pos_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_2(0) :=
    (Fill(F16_REM_W, nxt_rt_dig_2(0)(4)) & sqrt_csa_val_neg_2_2(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(0)(3)) & sqrt_csa_val_neg_1_2(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(0)(1)) & sqrt_csa_val_pos_1_2(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(0)(0)) & sqrt_csa_val_pos_2_2(0))
  f_r_s_for_csa_2(0) := Cat(f_r_s_i(45, 32), "b00".U(2.W))
  f_r_c_for_csa_2(0) := Cat(f_r_c_i(45, 32), "b00".U(2.W))
  nxt_f_r_s_spec_s0_2(4) :=
    f_r_s_for_csa_2(0) ^
      f_r_c_for_csa_2(0) ^
      sqrt_csa_val_neg_2_2(0)
  nxt_f_r_c_pre_spec_s0_2(4) := Cat(
    (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_2(0)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_2(4) := nxt_f_r_c_pre_spec_s0_2(4)
  nxt_f_r_s_spec_s0_2(3) :=
    f_r_s_for_csa_2(0) ^
      f_r_c_for_csa_2(0) ^
      sqrt_csa_val_neg_1_2(0)
  nxt_f_r_c_pre_spec_s0_2(3) := Cat(
    (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_2(0)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_2(3) := nxt_f_r_c_pre_spec_s0_2(3)
  nxt_f_r_s_spec_s0_2(2) := f_r_s_for_csa_2(0)
  nxt_f_r_c_pre_spec_s0_2(2) := f_r_c_for_csa_2(0)
  nxt_f_r_c_spec_s0_2(2) := nxt_f_r_c_pre_spec_s0_2(2)
  nxt_f_r_s_spec_s0_2(1) :=
    f_r_s_for_csa_2(0) ^
      f_r_c_for_csa_2(0) ^
      sqrt_csa_val_pos_1_2(0)
  nxt_f_r_c_pre_spec_s0_2(1) := Cat(
    (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_2(0)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_2(1) := nxt_f_r_c_pre_spec_s0_2(1)
  nxt_f_r_s_spec_s0_2(0) :=
    f_r_s_for_csa_2(0) ^
      f_r_c_for_csa_2(0) ^
      sqrt_csa_val_pos_2_2(0)
  nxt_f_r_c_pre_spec_s0_2(0) := Cat(
    (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_2(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_2(0)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_2(0) := nxt_f_r_c_pre_spec_s0_2(0)
  sqrt_csa_val_neg_2_3(0) := (Cat("b0".U(1.W), rt_m1_3) << 2) | mask_csa_neg_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_neg_1_3(0) := (Cat("b0".U(1.W), rt_m1_3) << 1) | mask_csa_neg_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_pos_1_3(0) := ~((Cat("b0".U(1.W), rt_3) << 1) | mask_csa_pos_1(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_pos_2_3(0) := ~((Cat("b0".U(1.W), rt_3) << 2) | mask_csa_pos_2(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_3(0) :=
    (Fill(F16_REM_W, nxt_rt_dig_3(0)(4)) & sqrt_csa_val_neg_2_3(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(0)(3)) & sqrt_csa_val_neg_1_3(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(0)(1)) & sqrt_csa_val_pos_1_3(0)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(0)(0)) & sqrt_csa_val_pos_2_3(0))
  f_r_s_for_csa_3(0) := Cat(f_r_s_i(13, 0), "b00".U(2.W))
  f_r_c_for_csa_3(0) := Cat(f_r_c_i(13, 0), "b00".U(2.W))
  nxt_f_r_s_spec_s0_3(4) :=
    f_r_s_for_csa_3(0) ^
      f_r_c_for_csa_3(0) ^
      sqrt_csa_val_neg_2_3(0)
  nxt_f_r_c_pre_spec_s0_3(4) := Cat(
    (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_3(0)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_3(4) := nxt_f_r_c_pre_spec_s0_3(4)
  nxt_f_r_s_spec_s0_3(3) :=
    f_r_s_for_csa_3(0) ^
      f_r_c_for_csa_3(0) ^
      sqrt_csa_val_neg_1_3(0)
  nxt_f_r_c_pre_spec_s0_3(3) := Cat(
    (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_3(0)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s0_3(3) := nxt_f_r_c_pre_spec_s0_3(3)
  nxt_f_r_s_spec_s0_3(2) := f_r_s_for_csa_3(0)
  nxt_f_r_c_pre_spec_s0_3(2) := f_r_c_for_csa_3(0)
  nxt_f_r_c_spec_s0_3(2) := nxt_f_r_c_pre_spec_s0_3(2)
  nxt_f_r_s_spec_s0_3(1) :=
    f_r_s_for_csa_3(0) ^
      f_r_c_for_csa_3(0) ^
      sqrt_csa_val_pos_1_3(0)
  nxt_f_r_c_pre_spec_s0_3(1) := Cat(
    (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_3(0)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_3(1) := nxt_f_r_c_pre_spec_s0_3(1)
  nxt_f_r_s_spec_s0_3(0) :=
    f_r_s_for_csa_3(0) ^
      f_r_c_for_csa_3(0) ^
      sqrt_csa_val_pos_2_3(0)
  nxt_f_r_c_pre_spec_s0_3(0) := Cat(
    (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_3(0)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_3(0)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s0_3(0) := nxt_f_r_c_pre_spec_s0_3(0)
  adder_9b_for_s1_qds_spec_0(4) := nr_f_r_9b_for_nxt_cycle_s1_qds_0_i + sqrt_csa_val_neg_2_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_0(3) := nr_f_r_9b_for_nxt_cycle_s1_qds_0_i + sqrt_csa_val_neg_1_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_0(2) := nr_f_r_9b_for_nxt_cycle_s1_qds_0_i
  adder_9b_for_s1_qds_spec_0(1) := nr_f_r_9b_for_nxt_cycle_s1_qds_0_i + sqrt_csa_val_pos_1_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_0(0) := nr_f_r_9b_for_nxt_cycle_s1_qds_0_i + sqrt_csa_val_pos_2_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_1(4) := nr_f_r_9b_for_nxt_cycle_s1_qds_1_i + sqrt_csa_val_neg_2_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_1(3) := nr_f_r_9b_for_nxt_cycle_s1_qds_1_i + sqrt_csa_val_neg_1_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_1(2) := nr_f_r_9b_for_nxt_cycle_s1_qds_1_i
  adder_9b_for_s1_qds_spec_1(1) := nr_f_r_9b_for_nxt_cycle_s1_qds_1_i + sqrt_csa_val_pos_1_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_1(0) := nr_f_r_9b_for_nxt_cycle_s1_qds_1_i + sqrt_csa_val_pos_2_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_2(4) := nr_f_r_9b_for_nxt_cycle_s1_qds_2_i + sqrt_csa_val_neg_2_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_2(3) := nr_f_r_9b_for_nxt_cycle_s1_qds_2_i + sqrt_csa_val_neg_1_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_2(2) := nr_f_r_9b_for_nxt_cycle_s1_qds_2_i
  adder_9b_for_s1_qds_spec_2(1) := nr_f_r_9b_for_nxt_cycle_s1_qds_2_i + sqrt_csa_val_pos_1_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_2(0) := nr_f_r_9b_for_nxt_cycle_s1_qds_2_i + sqrt_csa_val_pos_2_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_3(4) := nr_f_r_9b_for_nxt_cycle_s1_qds_3_i + sqrt_csa_val_neg_2_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_3(3) := nr_f_r_9b_for_nxt_cycle_s1_qds_3_i + sqrt_csa_val_neg_1_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_3(2) := nr_f_r_9b_for_nxt_cycle_s1_qds_3_i
  adder_9b_for_s1_qds_spec_3(1) := nr_f_r_9b_for_nxt_cycle_s1_qds_3_i + sqrt_csa_val_pos_1_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  adder_9b_for_s1_qds_spec_3(0) := nr_f_r_9b_for_nxt_cycle_s1_qds_3_i + sqrt_csa_val_pos_2_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
  nxt_rt_spec_s0_0(4) := rt_m1_0 | mask_rt_neg_2(0)
  nxt_rt_spec_s0_0(3) := rt_m1_0 | mask_rt_neg_1(0)
  nxt_rt_spec_s0_0(2) := rt_0
  nxt_rt_spec_s0_0(1) := rt_0 | mask_rt_pos_1(0)
  nxt_rt_spec_s0_0(0) := rt_0 | mask_rt_pos_2(0)
  a0_spec_s0_0(4) := nxt_rt_spec_s0_0(4)(F64_FULL_RT_W - 1)
  a2_spec_s0_0(4) := nxt_rt_spec_s0_0(4)(F64_FULL_RT_W - 3)
  a3_spec_s0_0(4) := nxt_rt_spec_s0_0(4)(F64_FULL_RT_W - 4)
  a4_spec_s0_0(4) := nxt_rt_spec_s0_0(4)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_2_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_2_0.a0_i <> a0_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.a2_i <> a2_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.a3_i <> a3_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.a4_i <> a4_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.m_neg_1_o <> m_neg_1_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.m_neg_0_o <> m_neg_0_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.m_pos_1_o <> m_pos_1_spec_s0_0(4)
  u_r4_qds_cg_spec_s0_neg_2_0.m_pos_2_o <> m_pos_2_spec_s0_0(4)

  a0_spec_s0_0(3) := nxt_rt_spec_s0_0(3)(F64_FULL_RT_W - 1)
  a2_spec_s0_0(3) := nxt_rt_spec_s0_0(3)(F64_FULL_RT_W - 3)
  a3_spec_s0_0(3) := nxt_rt_spec_s0_0(3)(F64_FULL_RT_W - 4)
  a4_spec_s0_0(3) := nxt_rt_spec_s0_0(3)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_1_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_1_0.a0_i <> a0_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.a2_i <> a2_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.a3_i <> a3_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.a4_i <> a4_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.m_neg_1_o <> m_neg_1_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.m_neg_0_o <> m_neg_0_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.m_pos_1_o <> m_pos_1_spec_s0_0(3)
  u_r4_qds_cg_spec_s0_neg_1_0.m_pos_2_o <> m_pos_2_spec_s0_0(3)

  a0_spec_s0_0(2) := nxt_rt_spec_s0_0(2)(F64_FULL_RT_W - 1)
  a2_spec_s0_0(2) := nxt_rt_spec_s0_0(2)(F64_FULL_RT_W - 3)
  a3_spec_s0_0(2) := nxt_rt_spec_s0_0(2)(F64_FULL_RT_W - 4)
  a4_spec_s0_0(2) := nxt_rt_spec_s0_0(2)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_0_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_0_0.a0_i <> a0_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.a2_i <> a2_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.a3_i <> a3_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.a4_i <> a4_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.m_neg_1_o <> m_neg_1_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.m_neg_0_o <> m_neg_0_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.m_pos_1_o <> m_pos_1_spec_s0_0(2)
  u_r4_qds_cg_spec_s0_neg_0_0.m_pos_2_o <> m_pos_2_spec_s0_0(2)

  a0_spec_s0_0(1) := nxt_rt_spec_s0_0(1)(F64_FULL_RT_W - 1)
  a2_spec_s0_0(1) := nxt_rt_spec_s0_0(1)(F64_FULL_RT_W - 3)
  a3_spec_s0_0(1) := nxt_rt_spec_s0_0(1)(F64_FULL_RT_W - 4)
  a4_spec_s0_0(1) := nxt_rt_spec_s0_0(1)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_1_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_1_0.a0_i <> a0_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.a2_i <> a2_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.a3_i <> a3_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.a4_i <> a4_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.m_neg_1_o <> m_neg_1_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.m_neg_0_o <> m_neg_0_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.m_pos_1_o <> m_pos_1_spec_s0_0(1)
  u_r4_qds_cg_spec_s0_pos_1_0.m_pos_2_o <> m_pos_2_spec_s0_0(1)

  a0_spec_s0_0(0) := nxt_rt_spec_s0_0(0)(F64_FULL_RT_W - 1)
  a2_spec_s0_0(0) := nxt_rt_spec_s0_0(0)(F64_FULL_RT_W - 3)
  a3_spec_s0_0(0) := nxt_rt_spec_s0_0(0)(F64_FULL_RT_W - 4)
  a4_spec_s0_0(0) := nxt_rt_spec_s0_0(0)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_2_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_2_0.a0_i <> a0_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.a2_i <> a2_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.a3_i <> a3_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.a4_i <> a4_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.m_neg_1_o <> m_neg_1_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.m_neg_0_o <> m_neg_0_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.m_pos_1_o <> m_pos_1_spec_s0_0(0)
  u_r4_qds_cg_spec_s0_pos_2_0.m_pos_2_o <> m_pos_2_spec_s0_0(0)

  nxt_rt_spec_s0_1(4) := rt_m1_1 | mask_rt_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s0_1(3) := rt_m1_1 | mask_rt_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s0_1(2) := rt_1
  nxt_rt_spec_s0_1(1) := rt_1 | mask_rt_pos_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s0_1(0) := rt_1 | mask_rt_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  a0_spec_s0_1(4) := nxt_rt_spec_s0_1(4)(F32_FULL_RT_W - 1)
  a2_spec_s0_1(4) := nxt_rt_spec_s0_1(4)(F32_FULL_RT_W - 3)
  a3_spec_s0_1(4) := nxt_rt_spec_s0_1(4)(F32_FULL_RT_W - 4)
  a4_spec_s0_1(4) := nxt_rt_spec_s0_1(4)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_2_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_2_1.a0_i <> a0_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.a2_i <> a2_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.a3_i <> a3_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.a4_i <> a4_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.m_neg_1_o <> m_neg_1_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.m_neg_0_o <> m_neg_0_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.m_pos_1_o <> m_pos_1_spec_s0_1(4)
  u_r4_qds_cg_spec_s0_neg_2_1.m_pos_2_o <> m_pos_2_spec_s0_1(4)

  a0_spec_s0_1(3) := nxt_rt_spec_s0_1(3)(F32_FULL_RT_W - 1)
  a2_spec_s0_1(3) := nxt_rt_spec_s0_1(3)(F32_FULL_RT_W - 3)
  a3_spec_s0_1(3) := nxt_rt_spec_s0_1(3)(F32_FULL_RT_W - 4)
  a4_spec_s0_1(3) := nxt_rt_spec_s0_1(3)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_1_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_1_1.a0_i <> a0_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.a2_i <> a2_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.a3_i <> a3_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.a4_i <> a4_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.m_neg_1_o <> m_neg_1_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.m_neg_0_o <> m_neg_0_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.m_pos_1_o <> m_pos_1_spec_s0_1(3)
  u_r4_qds_cg_spec_s0_neg_1_1.m_pos_2_o <> m_pos_2_spec_s0_1(3)

  a0_spec_s0_1(2) := nxt_rt_spec_s0_1(2)(F32_FULL_RT_W - 1)
  a2_spec_s0_1(2) := nxt_rt_spec_s0_1(2)(F32_FULL_RT_W - 3)
  a3_spec_s0_1(2) := nxt_rt_spec_s0_1(2)(F32_FULL_RT_W - 4)
  a4_spec_s0_1(2) := nxt_rt_spec_s0_1(2)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_0_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_0_1.a0_i <> a0_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.a2_i <> a2_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.a3_i <> a3_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.a4_i <> a4_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.m_neg_1_o <> m_neg_1_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.m_neg_0_o <> m_neg_0_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.m_pos_1_o <> m_pos_1_spec_s0_1(2)
  u_r4_qds_cg_spec_s0_neg_0_1.m_pos_2_o <> m_pos_2_spec_s0_1(2)

  a0_spec_s0_1(1) := nxt_rt_spec_s0_1(1)(F32_FULL_RT_W - 1)
  a2_spec_s0_1(1) := nxt_rt_spec_s0_1(1)(F32_FULL_RT_W - 3)
  a3_spec_s0_1(1) := nxt_rt_spec_s0_1(1)(F32_FULL_RT_W - 4)
  a4_spec_s0_1(1) := nxt_rt_spec_s0_1(1)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_1_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_1_1.a0_i <> a0_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.a2_i <> a2_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.a3_i <> a3_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.a4_i <> a4_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.m_neg_1_o <> m_neg_1_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.m_neg_0_o <> m_neg_0_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.m_pos_1_o <> m_pos_1_spec_s0_1(1)
  u_r4_qds_cg_spec_s0_pos_1_1.m_pos_2_o <> m_pos_2_spec_s0_1(1)

  a0_spec_s0_1(0) := nxt_rt_spec_s0_1(0)(F32_FULL_RT_W - 1)
  a2_spec_s0_1(0) := nxt_rt_spec_s0_1(0)(F32_FULL_RT_W - 3)
  a3_spec_s0_1(0) := nxt_rt_spec_s0_1(0)(F32_FULL_RT_W - 4)
  a4_spec_s0_1(0) := nxt_rt_spec_s0_1(0)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_2_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_2_1.a0_i <> a0_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.a2_i <> a2_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.a3_i <> a3_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.a4_i <> a4_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.m_neg_1_o <> m_neg_1_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.m_neg_0_o <> m_neg_0_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.m_pos_1_o <> m_pos_1_spec_s0_1(0)
  u_r4_qds_cg_spec_s0_pos_2_1.m_pos_2_o <> m_pos_2_spec_s0_1(0)

  nxt_rt_spec_s0_2(4) := rt_m1_2 | mask_rt_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_2(3) := rt_m1_2 | mask_rt_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_2(2) := rt_2
  nxt_rt_spec_s0_2(1) := rt_2 | mask_rt_pos_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_2(0) := rt_2 | mask_rt_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  a0_spec_s0_2(4) := nxt_rt_spec_s0_2(4)(F16_FULL_RT_W - 1)
  a2_spec_s0_2(4) := nxt_rt_spec_s0_2(4)(F16_FULL_RT_W - 3)
  a3_spec_s0_2(4) := nxt_rt_spec_s0_2(4)(F16_FULL_RT_W - 4)
  a4_spec_s0_2(4) := nxt_rt_spec_s0_2(4)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_2_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_2_2.a0_i <> a0_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.a2_i <> a2_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.a3_i <> a3_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.a4_i <> a4_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.m_neg_1_o <> m_neg_1_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.m_neg_0_o <> m_neg_0_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.m_pos_1_o <> m_pos_1_spec_s0_2(4)
  u_r4_qds_cg_spec_s0_neg_2_2.m_pos_2_o <> m_pos_2_spec_s0_2(4)

  a0_spec_s0_2(3) := nxt_rt_spec_s0_2(3)(F16_FULL_RT_W - 1)
  a2_spec_s0_2(3) := nxt_rt_spec_s0_2(3)(F16_FULL_RT_W - 3)
  a3_spec_s0_2(3) := nxt_rt_spec_s0_2(3)(F16_FULL_RT_W - 4)
  a4_spec_s0_2(3) := nxt_rt_spec_s0_2(3)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_1_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_1_2.a0_i <> a0_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.a2_i <> a2_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.a3_i <> a3_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.a4_i <> a4_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.m_neg_1_o <> m_neg_1_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.m_neg_0_o <> m_neg_0_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.m_pos_1_o <> m_pos_1_spec_s0_2(3)
  u_r4_qds_cg_spec_s0_neg_1_2.m_pos_2_o <> m_pos_2_spec_s0_2(3)

  a0_spec_s0_2(2) := nxt_rt_spec_s0_2(2)(F16_FULL_RT_W - 1)
  a2_spec_s0_2(2) := nxt_rt_spec_s0_2(2)(F16_FULL_RT_W - 3)
  a3_spec_s0_2(2) := nxt_rt_spec_s0_2(2)(F16_FULL_RT_W - 4)
  a4_spec_s0_2(2) := nxt_rt_spec_s0_2(2)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_0_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_0_2.a0_i <> a0_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.a2_i <> a2_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.a3_i <> a3_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.a4_i <> a4_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.m_neg_1_o <> m_neg_1_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.m_neg_0_o <> m_neg_0_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.m_pos_1_o <> m_pos_1_spec_s0_2(2)
  u_r4_qds_cg_spec_s0_neg_0_2.m_pos_2_o <> m_pos_2_spec_s0_2(2)

  a0_spec_s0_2(1) := nxt_rt_spec_s0_2(1)(F16_FULL_RT_W - 1)
  a2_spec_s0_2(1) := nxt_rt_spec_s0_2(1)(F16_FULL_RT_W - 3)
  a3_spec_s0_2(1) := nxt_rt_spec_s0_2(1)(F16_FULL_RT_W - 4)
  a4_spec_s0_2(1) := nxt_rt_spec_s0_2(1)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_1_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_1_2.a0_i <> a0_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.a2_i <> a2_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.a3_i <> a3_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.a4_i <> a4_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.m_neg_1_o <> m_neg_1_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.m_neg_0_o <> m_neg_0_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.m_pos_1_o <> m_pos_1_spec_s0_2(1)
  u_r4_qds_cg_spec_s0_pos_1_2.m_pos_2_o <> m_pos_2_spec_s0_2(1)

  a0_spec_s0_2(0) := nxt_rt_spec_s0_2(0)(F16_FULL_RT_W - 1)
  a2_spec_s0_2(0) := nxt_rt_spec_s0_2(0)(F16_FULL_RT_W - 3)
  a3_spec_s0_2(0) := nxt_rt_spec_s0_2(0)(F16_FULL_RT_W - 4)
  a4_spec_s0_2(0) := nxt_rt_spec_s0_2(0)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_2_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_2_2.a0_i <> a0_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.a2_i <> a2_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.a3_i <> a3_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.a4_i <> a4_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.m_neg_1_o <> m_neg_1_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.m_neg_0_o <> m_neg_0_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.m_pos_1_o <> m_pos_1_spec_s0_2(0)
  u_r4_qds_cg_spec_s0_pos_2_2.m_pos_2_o <> m_pos_2_spec_s0_2(0)

  nxt_rt_spec_s0_3(4) := rt_m1_3 | mask_rt_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_3(3) := rt_m1_3 | mask_rt_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_3(2) := rt_3
  nxt_rt_spec_s0_3(1) := rt_3 | mask_rt_pos_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s0_3(0) := rt_3 | mask_rt_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  a0_spec_s0_3(4) := nxt_rt_spec_s0_3(4)(F16_FULL_RT_W - 1)
  a2_spec_s0_3(4) := nxt_rt_spec_s0_3(4)(F16_FULL_RT_W - 3)
  a3_spec_s0_3(4) := nxt_rt_spec_s0_3(4)(F16_FULL_RT_W - 4)
  a4_spec_s0_3(4) := nxt_rt_spec_s0_3(4)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_2_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_2_3.a0_i <> a0_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.a2_i <> a2_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.a3_i <> a3_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.a4_i <> a4_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.m_neg_1_o <> m_neg_1_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.m_neg_0_o <> m_neg_0_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.m_pos_1_o <> m_pos_1_spec_s0_3(4)
  u_r4_qds_cg_spec_s0_neg_2_3.m_pos_2_o <> m_pos_2_spec_s0_3(4)

  a0_spec_s0_3(3) := nxt_rt_spec_s0_3(3)(F16_FULL_RT_W - 1)
  a2_spec_s0_3(3) := nxt_rt_spec_s0_3(3)(F16_FULL_RT_W - 3)
  a3_spec_s0_3(3) := nxt_rt_spec_s0_3(3)(F16_FULL_RT_W - 4)
  a4_spec_s0_3(3) := nxt_rt_spec_s0_3(3)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_1_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_1_3.a0_i <> a0_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.a2_i <> a2_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.a3_i <> a3_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.a4_i <> a4_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.m_neg_1_o <> m_neg_1_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.m_neg_0_o <> m_neg_0_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.m_pos_1_o <> m_pos_1_spec_s0_3(3)
  u_r4_qds_cg_spec_s0_neg_1_3.m_pos_2_o <> m_pos_2_spec_s0_3(3)

  a0_spec_s0_3(2) := nxt_rt_spec_s0_3(2)(F16_FULL_RT_W - 1)
  a2_spec_s0_3(2) := nxt_rt_spec_s0_3(2)(F16_FULL_RT_W - 3)
  a3_spec_s0_3(2) := nxt_rt_spec_s0_3(2)(F16_FULL_RT_W - 4)
  a4_spec_s0_3(2) := nxt_rt_spec_s0_3(2)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_neg_0_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_neg_0_3.a0_i <> a0_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.a2_i <> a2_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.a3_i <> a3_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.a4_i <> a4_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.m_neg_1_o <> m_neg_1_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.m_neg_0_o <> m_neg_0_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.m_pos_1_o <> m_pos_1_spec_s0_3(2)
  u_r4_qds_cg_spec_s0_neg_0_3.m_pos_2_o <> m_pos_2_spec_s0_3(2)

  a0_spec_s0_3(1) := nxt_rt_spec_s0_3(1)(F16_FULL_RT_W - 1)
  a2_spec_s0_3(1) := nxt_rt_spec_s0_3(1)(F16_FULL_RT_W - 3)
  a3_spec_s0_3(1) := nxt_rt_spec_s0_3(1)(F16_FULL_RT_W - 4)
  a4_spec_s0_3(1) := nxt_rt_spec_s0_3(1)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_1_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_1_3.a0_i <> a0_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.a2_i <> a2_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.a3_i <> a3_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.a4_i <> a4_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.m_neg_1_o <> m_neg_1_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.m_neg_0_o <> m_neg_0_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.m_pos_1_o <> m_pos_1_spec_s0_3(1)
  u_r4_qds_cg_spec_s0_pos_1_3.m_pos_2_o <> m_pos_2_spec_s0_3(1)

  a0_spec_s0_3(0) := nxt_rt_spec_s0_3(0)(F16_FULL_RT_W - 1)
  a2_spec_s0_3(0) := nxt_rt_spec_s0_3(0)(F16_FULL_RT_W - 3)
  a3_spec_s0_3(0) := nxt_rt_spec_s0_3(0)(F16_FULL_RT_W - 4)
  a4_spec_s0_3(0) := nxt_rt_spec_s0_3(0)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s0_pos_2_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s0_pos_2_3.a0_i <> a0_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.a2_i <> a2_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.a3_i <> a3_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.a4_i <> a4_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.m_neg_1_o <> m_neg_1_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.m_neg_0_o <> m_neg_0_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.m_pos_1_o <> m_pos_1_spec_s0_3(0)
  u_r4_qds_cg_spec_s0_pos_2_3.m_pos_2_o <> m_pos_2_spec_s0_3(0)

  if (S0_CSA_SPECULATIVE == 1) {
    nxt_f_r_s_0(0) :=
      (Fill(F64_REM_W, nxt_rt_dig_0(0)(4)) & nxt_f_r_s_spec_s0_0(4)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(3)) & nxt_f_r_s_spec_s0_0(3)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(2)) & nxt_f_r_s_spec_s0_0(2)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(1)) & nxt_f_r_s_spec_s0_0(1)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(0)) & nxt_f_r_s_spec_s0_0(0))
    nxt_f_r_s_1(0) :=
      (Fill(F32_REM_W, nxt_rt_dig_1(0)(4)) & nxt_f_r_s_spec_s0_1(4)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(3)) & nxt_f_r_s_spec_s0_1(3)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(2)) & nxt_f_r_s_spec_s0_1(2)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(1)) & nxt_f_r_s_spec_s0_1(1)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(0)) & nxt_f_r_s_spec_s0_1(0))
    nxt_f_r_s_2(0) :=
      (Fill(F16_REM_W, nxt_rt_dig_2(0)(4)) & nxt_f_r_s_spec_s0_2(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(3)) & nxt_f_r_s_spec_s0_2(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(2)) & nxt_f_r_s_spec_s0_2(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(1)) & nxt_f_r_s_spec_s0_2(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(0)) & nxt_f_r_s_spec_s0_2(0))
    nxt_f_r_s_3(0) :=
      (Fill(F16_REM_W, nxt_rt_dig_3(0)(4)) & nxt_f_r_s_spec_s0_3(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(3)) & nxt_f_r_s_spec_s0_3(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(2)) & nxt_f_r_s_spec_s0_3(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(1)) & nxt_f_r_s_spec_s0_3(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(0)) & nxt_f_r_s_spec_s0_3(0))
    nxt_f_r_c_0(0) :=
      (Fill(F64_REM_W, nxt_rt_dig_0(0)(4)) & nxt_f_r_c_spec_s0_0(4)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(3)) & nxt_f_r_c_spec_s0_0(3)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(2)) & nxt_f_r_c_spec_s0_0(2)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(1)) & nxt_f_r_c_spec_s0_0(1)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(0)(0)) & nxt_f_r_c_spec_s0_0(0))
    nxt_f_r_c_1(0) :=
      (Fill(F32_REM_W, nxt_rt_dig_1(0)(4)) & nxt_f_r_c_spec_s0_1(4)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(3)) & nxt_f_r_c_spec_s0_1(3)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(2)) & nxt_f_r_c_spec_s0_1(2)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(1)) & nxt_f_r_c_spec_s0_1(1)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(0)(0)) & nxt_f_r_c_spec_s0_1(0))
    nxt_f_r_c_2(0) :=
      (Fill(F16_REM_W, nxt_rt_dig_2(0)(4)) & nxt_f_r_c_spec_s0_2(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(3)) & nxt_f_r_c_spec_s0_2(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(2)) & nxt_f_r_c_spec_s0_2(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(1)) & nxt_f_r_c_spec_s0_2(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(0)(0)) & nxt_f_r_c_spec_s0_2(0))
    nxt_f_r_c_3(0) :=
      (Fill(F16_REM_W, nxt_rt_dig_3(0)(4)) & nxt_f_r_c_spec_s0_3(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(3)) & nxt_f_r_c_spec_s0_3(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(2)) & nxt_f_r_c_spec_s0_3(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(1)) & nxt_f_r_c_spec_s0_3(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(0)(0)) & nxt_f_r_c_spec_s0_3(0))
  }
  else if (S0_CSA_MERGED == 0) {
    nxt_f_r_s_0(0) :=
      f_r_s_for_csa_0(0) ^
        f_r_c_for_csa_0(0) ^
        sqrt_csa_val_0(0)
    nxt_f_r_c_pre_0(0) := Cat(
      (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_0(0)((F64_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_0(0)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_0(0)((F64_REM_W - 1) - 1, 0)),
      nxt_rt_dig_0(0)(0) | nxt_rt_dig_0(0)(1)
    )
    nxt_f_r_c_0(0) := Cat(
      nxt_f_r_c_pre_0(0)(55, 41),
      Mux(fp_fmt_i(0), (nxt_rt_dig_0(0)(0) | nxt_rt_dig_0(0)(1)), nxt_f_r_c_pre_0(0)(40)),
      nxt_f_r_c_pre_0(0)(39, 29),
      Mux(fp_fmt_i(1), (nxt_rt_dig_0(0)(0) | nxt_rt_dig_0(0)(1)), nxt_f_r_c_pre_0(0)(28)),
      nxt_f_r_c_pre_0(0)(27, 0)
    )
    nxt_f_r_s_1(0) :=
      f_r_s_for_csa_1(0) ^
        f_r_c_for_csa_1(0) ^
        sqrt_csa_val_1(0)
    nxt_f_r_c_pre_1(0) := Cat(
      (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_1(0)((F32_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_1(0)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_1(0)((F32_REM_W - 1) - 1, 0)),
      nxt_rt_dig_1(0)(0) | nxt_rt_dig_1(0)(1)
    )
    nxt_f_r_c_1(0) := Cat(
      nxt_f_r_c_pre_1(0)(27, 13),
      Mux(fp_fmt_i(0), (nxt_rt_dig_1(0)(0) | nxt_rt_dig_1(0)(1)), nxt_f_r_c_pre_1(0)(12)),
      nxt_f_r_c_pre_1(0)(11, 0)
    )
    nxt_f_r_s_2(0) :=
      f_r_s_for_csa_2(0) ^
        f_r_c_for_csa_2(0) ^
        sqrt_csa_val_2(0)
    nxt_f_r_c_pre_2(0) := Cat(
      (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_2(0)((F16_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_2(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_2(0)((F16_REM_W - 1) - 1, 0)),
      nxt_rt_dig_2(0)(0) | nxt_rt_dig_2(0)(1)
    )
    nxt_f_r_c_2(0) := nxt_f_r_c_pre_2(0)
    nxt_f_r_s_3(0) :=
      f_r_s_for_csa_3(0) ^
        f_r_c_for_csa_3(0) ^
        sqrt_csa_val_3(0)
    nxt_f_r_c_pre_3(0) := Cat(
      (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_3(0)((F16_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_3(0)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_3(0)((F16_REM_W - 1) - 1, 0)),
      nxt_rt_dig_3(0)(0) | nxt_rt_dig_3(0)(1)
    )
    nxt_f_r_c_3(0) := nxt_f_r_c_pre_3(0)
  }
  else {
    f_r_s_for_csa_merged(0) := Cat(f_r_s_i((REM_W - 2) - 1, 0), "b0".U(2.W))
    f_r_c_for_csa_merged(0) := Cat(f_r_c_i((REM_W - 2) - 1, 0), "b0".U(2.W))
    sqrt_csa_val_merged(0) :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        sqrt_csa_val_0(0)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1),
        "b00".U(2.W),
        sqrt_csa_val_2(0),
        "b00".U(2.W),
        sqrt_csa_val_1(0)(F32_REM_W - 1, F32_REM_W - 1 - F16_REM_W + 1),
        "b00".U(2.W),
        sqrt_csa_val_3(0)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(sqrt_csa_val_0(0)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1), "b0".U(6.W), "b0".U(2.W), sqrt_csa_val_1(0), "b0".U(6.W))) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(sqrt_csa_val_0(0), "b0".U(14.W)))
    nxt_f_r_s_merged(0) :=
      f_r_s_for_csa_merged(0) ^
        f_r_c_for_csa_merged(0) ^
        sqrt_csa_val_merged(0)
    nxt_f_r_c_merged_pre(0) := Cat(
      (f_r_s_for_csa_merged(0)((REM_W - 1) - 1, 0) & f_r_c_for_csa_merged(0)((REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_merged(0)((REM_W - 1) - 1, 0) & sqrt_csa_val_merged(0)((REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_merged(0)((REM_W - 1) - 1, 0) & sqrt_csa_val_merged(0)((REM_W - 1) - 1, 0)),
      "b0".U(1.W)
    )
    nxt_f_r_c_merged(0) :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        nxt_f_r_c_merged_pre(0)(69, 55),
        nxt_rt_dig_0(0)(1) | nxt_rt_dig_0(0)(0),
        "b0".U(2.W),
        nxt_f_r_c_merged_pre(0)(51, 37),
        nxt_rt_dig_2(0)(1) | nxt_rt_dig_2(0)(0),
        "b0".U(2.W),
        nxt_f_r_c_merged_pre(0)(33, 19),
        nxt_rt_dig_1(0)(1) | nxt_rt_dig_1(0)(0),
        "b0".U(2.W),
        nxt_f_r_c_merged_pre(0)(15, 1),
        nxt_rt_dig_3(0)(1) | nxt_rt_dig_3(0)(0)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(
          nxt_f_r_c_merged_pre(0)(69, 43),
          nxt_rt_dig_0(0)(1) | nxt_rt_dig_0(0)(0),
          "b0".U(6.W),
          "b0".U(2.W),
          nxt_f_r_c_merged_pre(0)(33, 7),
          nxt_rt_dig_1(0)(1) | nxt_rt_dig_1(0)(0),
          "b0".U(6.W)
        )) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(nxt_f_r_c_merged_pre(0)(69, 15), nxt_rt_dig_0(0)(1) | nxt_rt_dig_0(0)(0), "b0".U(14.W)))
  }

  adder_7b_res_for_s1_qds_0 :=
    (Fill(7, nxt_rt_dig_0(0)(4)) & adder_9b_for_s1_qds_spec_0(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(0)(3)) & adder_9b_for_s1_qds_spec_0(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(0)(2)) & adder_9b_for_s1_qds_spec_0(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(0)(1)) & adder_9b_for_s1_qds_spec_0(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(0)(0)) & adder_9b_for_s1_qds_spec_0(0)(8, 2))
  adder_7b_res_for_s1_qds_1 :=
    (Fill(7, nxt_rt_dig_1(0)(4)) & adder_9b_for_s1_qds_spec_1(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(0)(3)) & adder_9b_for_s1_qds_spec_1(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(0)(2)) & adder_9b_for_s1_qds_spec_1(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(0)(1)) & adder_9b_for_s1_qds_spec_1(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(0)(0)) & adder_9b_for_s1_qds_spec_1(0)(8, 2))
  adder_7b_res_for_s1_qds_2 :=
    (Fill(7, nxt_rt_dig_2(0)(4)) & adder_9b_for_s1_qds_spec_2(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(0)(3)) & adder_9b_for_s1_qds_spec_2(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(0)(2)) & adder_9b_for_s1_qds_spec_2(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(0)(1)) & adder_9b_for_s1_qds_spec_2(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(0)(0)) & adder_9b_for_s1_qds_spec_2(0)(8, 2))
  adder_7b_res_for_s1_qds_3 :=
    (Fill(7, nxt_rt_dig_3(0)(4)) & adder_9b_for_s1_qds_spec_3(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(0)(3)) & adder_9b_for_s1_qds_spec_3(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(0)(2)) & adder_9b_for_s1_qds_spec_3(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(0)(1)) & adder_9b_for_s1_qds_spec_3(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(0)(0)) & adder_9b_for_s1_qds_spec_3(0)(8, 2))
  m_neg_1_0(1) :=
    (Fill(7, nxt_rt_dig_0(0)(4)) & m_neg_1_spec_s0_0(4)) |
      (Fill(7, nxt_rt_dig_0(0)(3)) & m_neg_1_spec_s0_0(3)) |
      (Fill(7, nxt_rt_dig_0(0)(2)) & m_neg_1_spec_s0_0(2)) |
      (Fill(7, nxt_rt_dig_0(0)(1)) & m_neg_1_spec_s0_0(1)) |
      (Fill(7, nxt_rt_dig_0(0)(0)) & m_neg_1_spec_s0_0(0))
  m_neg_1_1(1) :=
    (Fill(7, nxt_rt_dig_1(0)(4)) & m_neg_1_spec_s0_1(4)) |
      (Fill(7, nxt_rt_dig_1(0)(3)) & m_neg_1_spec_s0_1(3)) |
      (Fill(7, nxt_rt_dig_1(0)(2)) & m_neg_1_spec_s0_1(2)) |
      (Fill(7, nxt_rt_dig_1(0)(1)) & m_neg_1_spec_s0_1(1)) |
      (Fill(7, nxt_rt_dig_1(0)(0)) & m_neg_1_spec_s0_1(0))
  m_neg_1_2(1) :=
    (Fill(7, nxt_rt_dig_2(0)(4)) & m_neg_1_spec_s0_2(4)) |
      (Fill(7, nxt_rt_dig_2(0)(3)) & m_neg_1_spec_s0_2(3)) |
      (Fill(7, nxt_rt_dig_2(0)(2)) & m_neg_1_spec_s0_2(2)) |
      (Fill(7, nxt_rt_dig_2(0)(1)) & m_neg_1_spec_s0_2(1)) |
      (Fill(7, nxt_rt_dig_2(0)(0)) & m_neg_1_spec_s0_2(0))
  m_neg_1_3(1) :=
    (Fill(7, nxt_rt_dig_3(0)(4)) & m_neg_1_spec_s0_3(4)) |
      (Fill(7, nxt_rt_dig_3(0)(3)) & m_neg_1_spec_s0_3(3)) |
      (Fill(7, nxt_rt_dig_3(0)(2)) & m_neg_1_spec_s0_3(2)) |
      (Fill(7, nxt_rt_dig_3(0)(1)) & m_neg_1_spec_s0_3(1)) |
      (Fill(7, nxt_rt_dig_3(0)(0)) & m_neg_1_spec_s0_3(0))
  m_neg_0_0(1) :=
    (Fill(7, nxt_rt_dig_0(0)(4)) & m_neg_0_spec_s0_0(4)) |
      (Fill(7, nxt_rt_dig_0(0)(3)) & m_neg_0_spec_s0_0(3)) |
      (Fill(7, nxt_rt_dig_0(0)(2)) & m_neg_0_spec_s0_0(2)) |
      (Fill(7, nxt_rt_dig_0(0)(1)) & m_neg_0_spec_s0_0(1)) |
      (Fill(7, nxt_rt_dig_0(0)(0)) & m_neg_0_spec_s0_0(0))
  m_neg_0_1(1) :=
    (Fill(7, nxt_rt_dig_1(0)(4)) & m_neg_0_spec_s0_1(4)) |
      (Fill(7, nxt_rt_dig_1(0)(3)) & m_neg_0_spec_s0_1(3)) |
      (Fill(7, nxt_rt_dig_1(0)(2)) & m_neg_0_spec_s0_1(2)) |
      (Fill(7, nxt_rt_dig_1(0)(1)) & m_neg_0_spec_s0_1(1)) |
      (Fill(7, nxt_rt_dig_1(0)(0)) & m_neg_0_spec_s0_1(0))
  m_neg_0_2(1) :=
    (Fill(7, nxt_rt_dig_2(0)(4)) & m_neg_0_spec_s0_2(4)) |
      (Fill(7, nxt_rt_dig_2(0)(3)) & m_neg_0_spec_s0_2(3)) |
      (Fill(7, nxt_rt_dig_2(0)(2)) & m_neg_0_spec_s0_2(2)) |
      (Fill(7, nxt_rt_dig_2(0)(1)) & m_neg_0_spec_s0_2(1)) |
      (Fill(7, nxt_rt_dig_2(0)(0)) & m_neg_0_spec_s0_2(0))
  m_neg_0_3(1) :=
    (Fill(7, nxt_rt_dig_3(0)(4)) & m_neg_0_spec_s0_3(4)) |
      (Fill(7, nxt_rt_dig_3(0)(3)) & m_neg_0_spec_s0_3(3)) |
      (Fill(7, nxt_rt_dig_3(0)(2)) & m_neg_0_spec_s0_3(2)) |
      (Fill(7, nxt_rt_dig_3(0)(1)) & m_neg_0_spec_s0_3(1)) |
      (Fill(7, nxt_rt_dig_3(0)(0)) & m_neg_0_spec_s0_3(0))
  m_pos_1_0(1) :=
    (Fill(7, nxt_rt_dig_0(0)(4)) & m_pos_1_spec_s0_0(4)) |
      (Fill(7, nxt_rt_dig_0(0)(3)) & m_pos_1_spec_s0_0(3)) |
      (Fill(7, nxt_rt_dig_0(0)(2)) & m_pos_1_spec_s0_0(2)) |
      (Fill(7, nxt_rt_dig_0(0)(1)) & m_pos_1_spec_s0_0(1)) |
      (Fill(7, nxt_rt_dig_0(0)(0)) & m_pos_1_spec_s0_0(0))
  m_pos_1_1(1) :=
    (Fill(7, nxt_rt_dig_1(0)(4)) & m_pos_1_spec_s0_1(4)) |
      (Fill(7, nxt_rt_dig_1(0)(3)) & m_pos_1_spec_s0_1(3)) |
      (Fill(7, nxt_rt_dig_1(0)(2)) & m_pos_1_spec_s0_1(2)) |
      (Fill(7, nxt_rt_dig_1(0)(1)) & m_pos_1_spec_s0_1(1)) |
      (Fill(7, nxt_rt_dig_1(0)(0)) & m_pos_1_spec_s0_1(0))
  m_pos_1_2(1) :=
    (Fill(7, nxt_rt_dig_2(0)(4)) & m_pos_1_spec_s0_2(4)) |
      (Fill(7, nxt_rt_dig_2(0)(3)) & m_pos_1_spec_s0_2(3)) |
      (Fill(7, nxt_rt_dig_2(0)(2)) & m_pos_1_spec_s0_2(2)) |
      (Fill(7, nxt_rt_dig_2(0)(1)) & m_pos_1_spec_s0_2(1)) |
      (Fill(7, nxt_rt_dig_2(0)(0)) & m_pos_1_spec_s0_2(0))
  m_pos_1_3(1) :=
    (Fill(7, nxt_rt_dig_3(0)(4)) & m_pos_1_spec_s0_3(4)) |
      (Fill(7, nxt_rt_dig_3(0)(3)) & m_pos_1_spec_s0_3(3)) |
      (Fill(7, nxt_rt_dig_3(0)(2)) & m_pos_1_spec_s0_3(2)) |
      (Fill(7, nxt_rt_dig_3(0)(1)) & m_pos_1_spec_s0_3(1)) |
      (Fill(7, nxt_rt_dig_3(0)(0)) & m_pos_1_spec_s0_3(0))
  m_pos_2_0(1) :=
    (Fill(7, nxt_rt_dig_0(0)(4)) & m_pos_2_spec_s0_0(4)) |
      (Fill(7, nxt_rt_dig_0(0)(3)) & m_pos_2_spec_s0_0(3)) |
      (Fill(7, nxt_rt_dig_0(0)(2)) & m_pos_2_spec_s0_0(2)) |
      (Fill(7, nxt_rt_dig_0(0)(1)) & m_pos_2_spec_s0_0(1)) |
      (Fill(7, nxt_rt_dig_0(0)(0)) & m_pos_2_spec_s0_0(0))
  m_pos_2_1(1) :=
    (Fill(7, nxt_rt_dig_1(0)(4)) & m_pos_2_spec_s0_1(4)) |
      (Fill(7, nxt_rt_dig_1(0)(3)) & m_pos_2_spec_s0_1(3)) |
      (Fill(7, nxt_rt_dig_1(0)(2)) & m_pos_2_spec_s0_1(2)) |
      (Fill(7, nxt_rt_dig_1(0)(1)) & m_pos_2_spec_s0_1(1)) |
      (Fill(7, nxt_rt_dig_1(0)(0)) & m_pos_2_spec_s0_1(0))
  m_pos_2_2(1) :=
    (Fill(7, nxt_rt_dig_2(0)(4)) & m_pos_2_spec_s0_2(4)) |
      (Fill(7, nxt_rt_dig_2(0)(3)) & m_pos_2_spec_s0_2(3)) |
      (Fill(7, nxt_rt_dig_2(0)(2)) & m_pos_2_spec_s0_2(2)) |
      (Fill(7, nxt_rt_dig_2(0)(1)) & m_pos_2_spec_s0_2(1)) |
      (Fill(7, nxt_rt_dig_2(0)(0)) & m_pos_2_spec_s0_2(0))
  m_pos_2_3(1) :=
    (Fill(7, nxt_rt_dig_3(0)(4)) & m_pos_2_spec_s0_3(4)) |
      (Fill(7, nxt_rt_dig_3(0)(3)) & m_pos_2_spec_s0_3(3)) |
      (Fill(7, nxt_rt_dig_3(0)(2)) & m_pos_2_spec_s0_3(2)) |
      (Fill(7, nxt_rt_dig_3(0)(1)) & m_pos_2_spec_s0_3(1)) |
      (Fill(7, nxt_rt_dig_3(0)(0)) & m_pos_2_spec_s0_3(0))
  nxt_rt_0(0) :=
    (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(4)) & nxt_rt_spec_s0_0(4)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(3)) & nxt_rt_spec_s0_0(3)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(2)) & nxt_rt_spec_s0_0(2)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(1)) & nxt_rt_spec_s0_0(1)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(0)) & nxt_rt_spec_s0_0(0))
  nxt_rt_m1_0(0) :=
    (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(4)) & (rt_m1_0 | mask_rt_m1_neg_2(0))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(3)) & (rt_m1_0 | mask_rt_m1_neg_1(0))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(2)) & (rt_m1_0 | mask_rt_m1_neg_0(0))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(1)) & rt_0) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(0)(0)) & (rt_0 | mask_rt_m1_pos_2(0)))
  nxt_rt_1(0) :=
    (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(4)) & nxt_rt_spec_s0_1(4)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(3)) & nxt_rt_spec_s0_1(3)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(2)) & nxt_rt_spec_s0_1(2)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(1)) & nxt_rt_spec_s0_1(1)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(0)) & nxt_rt_spec_s0_1(0))
  nxt_rt_m1_1(0) :=
    (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(4)) & (rt_m1_1 | mask_rt_m1_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(3)) & (rt_m1_1 | mask_rt_m1_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(2)) & (rt_m1_1 | mask_rt_m1_neg_0(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(1)) & rt_1) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(0)(0)) & (rt_1 | mask_rt_m1_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)))
  rt_for_csa_0(1) := Cat(
    nxt_rt_0(0)(54, 40),
    Mux(fp_fmt_i(0), "b0".U(2.W), nxt_rt_0(0)(39, 38)),
    nxt_rt_0(0)(37, 28),
    Mux(fp_fmt_i(1), "b0".U(2.W), nxt_rt_0(0)(27, 26)),
    nxt_rt_0(0)(25, 0)
  )
  rt_m1_for_csa_0(1) := Cat(
    nxt_rt_m1_0(0)(54, 40),
    Mux(fp_fmt_i(0), "b0".U(2.W), nxt_rt_m1_0(0)(39, 38)),
    nxt_rt_m1_0(0)(37, 28),
    Mux(fp_fmt_i(1), "b0".U(2.W), nxt_rt_m1_0(0)(27, 26)),
    nxt_rt_m1_0(0)(25, 0)
  )
  rt_for_csa_1(1) := Cat(
    nxt_rt_1(0)(26, 12),
    Mux(fp_fmt_i(0), "b0".U(2.W), nxt_rt_1(0)(11, 10)),
    nxt_rt_1(0)(9, 0)
  )
  rt_m1_for_csa_1(1) := Cat(
    nxt_rt_m1_1(0)(26, 12),
    Mux(fp_fmt_i(0), "b0".U(2.W), nxt_rt_m1_1(0)(11, 10)),
    nxt_rt_m1_1(0)(9, 0)
  )
  nxt_rt_2(0) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(4)) & nxt_rt_spec_s0_2(4)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(3)) & nxt_rt_spec_s0_2(3)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(2)) & nxt_rt_spec_s0_2(2)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(1)) & nxt_rt_spec_s0_2(1)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(0)) & nxt_rt_spec_s0_2(0))
  nxt_rt_m1_2(0) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(4)) & (rt_m1_2 | mask_rt_m1_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(3)) & (rt_m1_2 | mask_rt_m1_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(2)) & (rt_m1_2 | mask_rt_m1_neg_0(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(1)) & rt_2) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(0)(0)) & (rt_2 | mask_rt_m1_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)))
  nxt_rt_3(0) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(4)) & nxt_rt_spec_s0_3(4)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(3)) & nxt_rt_spec_s0_3(3)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(2)) & nxt_rt_spec_s0_3(2)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(1)) & nxt_rt_spec_s0_3(1)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(0)) & nxt_rt_spec_s0_3(0))
  nxt_rt_m1_3(0) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(4)) & (rt_m1_3 | mask_rt_m1_neg_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(3)) & (rt_m1_3 | mask_rt_m1_neg_1(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(2)) & (rt_m1_3 | mask_rt_m1_neg_0(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(1)) & rt_3) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(0)(0)) & (rt_3 | mask_rt_m1_pos_2(0)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)))
  sqrt_csa_val_neg_2_0(1) := (Cat("b0".U(1.W), rt_m1_for_csa_0(1)) << 2) | mask_csa_neg_2(1)
  sqrt_csa_val_neg_1_0(1) := (Cat("b0".U(1.W), rt_m1_for_csa_0(1)) << 1) | mask_csa_neg_1(1)
  sqrt_csa_val_pos_1_0(1) := ~((Cat("b0".U(1.W), rt_for_csa_0(1)) << 1) | mask_csa_pos_1(1))
  sqrt_csa_val_pos_2_0(1) := ~((Cat("b0".U(1.W), rt_for_csa_0(1)) << 2) | mask_csa_pos_2(1))
  sqrt_csa_val_0(1) :=
    (Fill(F64_REM_W, nxt_rt_dig_0(1)(4)) & sqrt_csa_val_neg_2_0(1)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(1)(3)) & sqrt_csa_val_neg_1_0(1)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(1)(1)) & sqrt_csa_val_pos_1_0(1)) |
      (Fill(F64_REM_W, nxt_rt_dig_0(1)(0)) & sqrt_csa_val_pos_2_0(1))
  if (S0_CSA_IS_MERGED == 1) {
    f_r_s_for_csa_0(1) := Cat(
      nxt_f_r_s_merged(0)(67, 54),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_s_merged(0)(53, 52)),
      nxt_f_r_s_merged(0)(51, 40),
      Mux(fp_fmt_i(1), "b00".U(2.W), nxt_f_r_s_merged(0)(39, 38)),
      nxt_f_r_s_merged(0)(37, 14),
      "b00".U(2.W)
    )
    f_r_c_for_csa_0(1) := Cat(
      nxt_f_r_c_merged(0)(67, 54),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_c_merged(0)(53, 52)),
      nxt_f_r_c_merged(0)(51, 40),
      Mux(fp_fmt_i(1), "b00".U(2.W), nxt_f_r_c_merged(0)(39, 38)),
      nxt_f_r_c_merged(0)(37, 14),
      "b00".U(2.W)
    )
  }
  else {
    f_r_s_for_csa_0(1) := Cat(
      nxt_f_r_s_0(0)(53, 40),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_s_0(0)(39, 38)),
      nxt_f_r_s_0(0)(37, 28),
      Mux(fp_fmt_i(1), "b00".U(2.W), nxt_f_r_s_0(0)(27, 26)),
      nxt_f_r_s_0(0)(25, 0),
      "b00".U(2.W)
    )
    f_r_c_for_csa_0(1) := Cat(
      nxt_f_r_c_0(0)(53, 40),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_c_0(0)(39, 38)),
      nxt_f_r_c_0(0)(37, 28),
      Mux(fp_fmt_i(1), "b00".U(2.W), nxt_f_r_c_0(0)(27, 26)),
      nxt_f_r_c_0(0)(25, 0),
      "b00".U(2.W)
    )
  }

  nxt_f_r_s_spec_s1_0(4) :=
    f_r_s_for_csa_0(1) ^
      f_r_c_for_csa_0(1) ^
      sqrt_csa_val_neg_2_0(1)
  nxt_f_r_c_pre_spec_s1_0(4) := Cat(
    (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_0(1)((F64_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_0(4) := Cat(
    nxt_f_r_c_pre_spec_s1_0(4)(55, 41),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_0(4)(40)),
    nxt_f_r_c_pre_spec_s1_0(4)(39, 29),
    Mux(fp_fmt_i(1), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_0(4)(28)),
    nxt_f_r_c_pre_spec_s1_0(4)(27, 0)
  )
  nxt_f_r_s_spec_s1_0(3) :=
    f_r_s_for_csa_0(1) ^
      f_r_c_for_csa_0(1) ^
      sqrt_csa_val_neg_1_0(1)
  nxt_f_r_c_pre_spec_s1_0(3) := Cat(
    (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_0(1)((F64_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_0(3) := Cat(
    nxt_f_r_c_pre_spec_s1_0(3)(55, 41),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_0(3)(40)),
    nxt_f_r_c_pre_spec_s1_0(3)(39, 29),
    Mux(fp_fmt_i(1), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_0(3)(28)),
    nxt_f_r_c_pre_spec_s1_0(3)(27, 0)
  )
  nxt_f_r_s_spec_s1_0(2) := f_r_s_for_csa_0(1)
  nxt_f_r_c_pre_spec_s1_0(2) := f_r_c_for_csa_0(1)
  nxt_f_r_c_spec_s1_0(2) := nxt_f_r_c_pre_spec_s1_0(2)
  nxt_f_r_s_spec_s1_0(1) :=
    f_r_s_for_csa_0(1) ^
      f_r_c_for_csa_0(1) ^
      sqrt_csa_val_pos_1_0(1)
  nxt_f_r_c_pre_spec_s1_0(1) := Cat(
    (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_0(1)((F64_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_0(1) := Cat(
    nxt_f_r_c_pre_spec_s1_0(1)(55, 41),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_0(1)(40)),
    nxt_f_r_c_pre_spec_s1_0(1)(39, 29),
    Mux(fp_fmt_i(1), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_0(1)(28)),
    nxt_f_r_c_pre_spec_s1_0(1)(27, 0)
  )
  nxt_f_r_s_spec_s1_0(0) :=
    f_r_s_for_csa_0(1) ^
      f_r_c_for_csa_0(1) ^
      sqrt_csa_val_pos_2_0(1)
  nxt_f_r_c_pre_spec_s1_0(0) := Cat(
    (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_0(1)((F64_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_0(1)((F64_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_0(0) := Cat(
    nxt_f_r_c_pre_spec_s1_0(0)(55, 41),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_0(0)(40)),
    nxt_f_r_c_pre_spec_s1_0(0)(39, 29),
    Mux(fp_fmt_i(1), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_0(0)(28)),
    nxt_f_r_c_pre_spec_s1_0(0)(27, 0)
  )
  sqrt_csa_val_neg_2_1(1) := (Cat("b0".U(1.W), rt_m1_for_csa_1(1)) << 2) | mask_csa_neg_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1)
  sqrt_csa_val_neg_1_1(1) := (Cat("b0".U(1.W), rt_m1_for_csa_1(1)) << 1) | mask_csa_neg_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1)
  sqrt_csa_val_pos_1_1(1) := ~((Cat("b0".U(1.W), rt_for_csa_1(1)) << 1) | mask_csa_pos_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1))
  sqrt_csa_val_pos_2_1(1) := ~((Cat("b0".U(1.W), rt_for_csa_1(1)) << 2) | mask_csa_pos_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1))
  sqrt_csa_val_1(1) :=
    (Fill(F32_REM_W, nxt_rt_dig_1(1)(4)) & sqrt_csa_val_neg_2_1(1)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(1)(3)) & sqrt_csa_val_neg_1_1(1)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(1)(1)) & sqrt_csa_val_pos_1_1(1)) |
      (Fill(F32_REM_W, nxt_rt_dig_1(1)(0)) & sqrt_csa_val_pos_2_1(1))
  if (S0_CSA_IS_MERGED == 1) {
    f_r_s_for_csa_1(1) := Cat(
      nxt_f_r_s_merged(0)(31, 18),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_s_merged(0)(17, 16)),
      nxt_f_r_s_merged(0)(15, 6),
      "b00".U(2.W)
    )
    f_r_c_for_csa_1(1) := Cat(
      nxt_f_r_c_merged(0)(31, 18),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_c_merged(0)(17, 16)),
      nxt_f_r_c_merged(0)(15, 6),
      "b00".U(2.W)
    )
  }
  else {
    f_r_s_for_csa_1(1) := Cat(
      nxt_f_r_s_1(0)(25, 12),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_s_1(0)(11, 10)),
      nxt_f_r_s_1(0)(9, 0),
      "b00".U(2.W)
    )
    f_r_c_for_csa_1(1) := Cat(
      nxt_f_r_c_1(0)(25, 12),
      Mux(fp_fmt_i(0), "b00".U(2.W), nxt_f_r_c_1(0)(11, 10)),
      nxt_f_r_c_1(0)(9, 0),
      "b00".U(2.W)
    )
  }

  nxt_f_r_s_spec_s1_1(4) :=
    f_r_s_for_csa_1(1) ^
      f_r_c_for_csa_1(1) ^
      sqrt_csa_val_neg_2_1(1)
  nxt_f_r_c_pre_spec_s1_1(4) := Cat(
    (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_1(1)((F32_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_1(4) := Cat(
    nxt_f_r_c_pre_spec_s1_1(4)(27, 13),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_1(4)(12)),
    nxt_f_r_c_pre_spec_s1_1(4)(11, 0)
  )
  nxt_f_r_s_spec_s1_1(3) :=
    f_r_s_for_csa_1(1) ^
      f_r_c_for_csa_1(1) ^
      sqrt_csa_val_neg_1_1(1)
  nxt_f_r_c_pre_spec_s1_1(3) := Cat(
    (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_1(1)((F32_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_1(3) := Cat(
    nxt_f_r_c_pre_spec_s1_1(3)(27, 13),
    Mux(fp_fmt_i(0), "b0".U(1.W), nxt_f_r_c_pre_spec_s1_1(3)(12)),
    nxt_f_r_c_pre_spec_s1_1(3)(11, 0)
  )
  nxt_f_r_s_spec_s1_1(2) := f_r_s_for_csa_1(1)
  nxt_f_r_c_pre_spec_s1_1(2) := f_r_c_for_csa_1(1)
  nxt_f_r_c_spec_s1_1(2) := nxt_f_r_c_pre_spec_s1_1(2)
  nxt_f_r_s_spec_s1_1(1) :=
    f_r_s_for_csa_1(1) ^
      f_r_c_for_csa_1(1) ^
      sqrt_csa_val_pos_1_1(1)
  nxt_f_r_c_pre_spec_s1_1(1) := Cat(
    (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_1(1)((F32_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_1(1) := Cat(
    nxt_f_r_c_pre_spec_s1_1(1)(27, 13),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_1(1)(12)),
    nxt_f_r_c_pre_spec_s1_1(1)(11, 0)
  )
  nxt_f_r_s_spec_s1_1(0) :=
    f_r_s_for_csa_1(1) ^
      f_r_c_for_csa_1(1) ^
      sqrt_csa_val_pos_2_1(1)
  nxt_f_r_c_pre_spec_s1_1(0) := Cat(
    (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_1(1)((F32_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_1(1)((F32_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_1(0) := Cat(
    nxt_f_r_c_pre_spec_s1_1(0)(27, 13),
    Mux(fp_fmt_i(0), "b1".U(1.W), nxt_f_r_c_pre_spec_s1_1(0)(12)),
    nxt_f_r_c_pre_spec_s1_1(0)(11, 0)
  )
  sqrt_csa_val_neg_2_2(1) := (Cat("b0".U(1.W), nxt_rt_m1_2(0)) << 2) | mask_csa_neg_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_neg_1_2(1) := (Cat("b0".U(1.W), nxt_rt_m1_2(0)) << 1) | mask_csa_neg_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_pos_1_2(1) := ~((Cat("b0".U(1.W), nxt_rt_2(0)) << 1) | mask_csa_pos_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_pos_2_2(1) := ~((Cat("b0".U(1.W), nxt_rt_2(0)) << 2) | mask_csa_pos_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_2(1) :=
    (Fill(F16_REM_W, nxt_rt_dig_2(1)(4)) & sqrt_csa_val_neg_2_2(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(1)(3)) & sqrt_csa_val_neg_1_2(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(1)(1)) & sqrt_csa_val_pos_1_2(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_2(1)(0)) & sqrt_csa_val_pos_2_2(1))
  if (S0_CSA_IS_MERGED == 1) {
    f_r_s_for_csa_2(1) := Cat(nxt_f_r_s_merged(0)(49, 36), "b00".U(2.W))
    f_r_c_for_csa_2(1) := Cat(nxt_f_r_c_merged(0)(49, 36), "b00".U(2.W))
  }
  else {
    f_r_s_for_csa_2(1) := Cat(nxt_f_r_s_2(0)(13, 0), "b00".U(2.W))
    f_r_c_for_csa_2(1) := Cat(nxt_f_r_c_2(0)(13, 0), "b00".U(2.W))
  }

  nxt_f_r_s_spec_s1_2(4) :=
    f_r_s_for_csa_2(1) ^
      f_r_c_for_csa_2(1) ^
      sqrt_csa_val_neg_2_2(1)
  nxt_f_r_c_pre_spec_s1_2(4) := Cat(
    (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_2(1)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_2(4) := nxt_f_r_c_pre_spec_s1_2(4)
  nxt_f_r_s_spec_s1_2(3) :=
    f_r_s_for_csa_2(1) ^
      f_r_c_for_csa_2(1) ^
      sqrt_csa_val_neg_1_2(1)
  nxt_f_r_c_pre_spec_s1_2(3) := Cat(
    (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_2(1)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_2(3) := nxt_f_r_c_pre_spec_s1_2(3)
  nxt_f_r_s_spec_s1_2(2) := f_r_s_for_csa_2(1)
  nxt_f_r_c_pre_spec_s1_2(2) := f_r_c_for_csa_2(1)
  nxt_f_r_c_spec_s1_2(2) := nxt_f_r_c_pre_spec_s1_2(2)
  nxt_f_r_s_spec_s1_2(1) :=
    f_r_s_for_csa_2(1) ^
      f_r_c_for_csa_2(1) ^
      sqrt_csa_val_pos_1_2(1)
  nxt_f_r_c_pre_spec_s1_2(1) := Cat(
    (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_2(1)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_2(1) := nxt_f_r_c_pre_spec_s1_2(1)
  nxt_f_r_s_spec_s1_2(0) :=
    f_r_s_for_csa_2(1) ^
      f_r_c_for_csa_2(1) ^
      sqrt_csa_val_pos_2_2(1)
  nxt_f_r_c_pre_spec_s1_2(0) := Cat(
    (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_2(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_2(1)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_2(0) := nxt_f_r_c_pre_spec_s1_2(0)
  sqrt_csa_val_neg_2_3(1) := (Cat("b0".U(1.W), nxt_rt_m1_3(0)) << 2) | mask_csa_neg_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_neg_1_3(1) := (Cat("b0".U(1.W), nxt_rt_m1_3(0)) << 1) | mask_csa_neg_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1)
  sqrt_csa_val_pos_1_3(1) := ~((Cat("b0".U(1.W), nxt_rt_3(0)) << 1) | mask_csa_pos_1(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_pos_2_3(1) := ~((Cat("b0".U(1.W), nxt_rt_3(0)) << 2) | mask_csa_pos_2(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1))
  sqrt_csa_val_3(1) :=
    (Fill(F16_REM_W, nxt_rt_dig_3(1)(4)) & sqrt_csa_val_neg_2_3(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(1)(3)) & sqrt_csa_val_neg_1_3(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(1)(1)) & sqrt_csa_val_pos_1_3(1)) |
      (Fill(F16_REM_W, nxt_rt_dig_3(1)(0)) & sqrt_csa_val_pos_2_3(1))
  if (S0_CSA_IS_MERGED == 1) {
    f_r_s_for_csa_3(1) := Cat(nxt_f_r_s_merged(0)(13, 0), "b00".U(2.W))
    f_r_c_for_csa_3(1) := Cat(nxt_f_r_c_merged(0)(13, 0), "b00".U(2.W))
  }
  else {
    f_r_s_for_csa_3(1) := Cat(nxt_f_r_s_3(0)(13, 0), "b00".U(2.W))
    f_r_c_for_csa_3(1) := Cat(nxt_f_r_c_3(0)(13, 0), "b00".U(2.W))
  }

  nxt_f_r_s_spec_s1_3(4) :=
    f_r_s_for_csa_3(1) ^
      f_r_c_for_csa_3(1) ^
      sqrt_csa_val_neg_2_3(1)
  nxt_f_r_c_pre_spec_s1_3(4) := Cat(
    (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_2_3(1)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_3(4) := nxt_f_r_c_pre_spec_s1_3(4)
  nxt_f_r_s_spec_s1_3(3) :=
    f_r_s_for_csa_3(1) ^
      f_r_c_for_csa_3(1) ^
      sqrt_csa_val_neg_1_3(1)
  nxt_f_r_c_pre_spec_s1_3(3) := Cat(
    (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_neg_1_3(1)((F16_REM_W - 1) - 1, 0)),
    "b0".U(1.W)
  )
  nxt_f_r_c_spec_s1_3(3) := nxt_f_r_c_pre_spec_s1_3(3)
  nxt_f_r_s_spec_s1_3(2) := f_r_s_for_csa_3(1)
  nxt_f_r_c_pre_spec_s1_3(2) := f_r_c_for_csa_3(1)
  nxt_f_r_c_spec_s1_3(2) := nxt_f_r_c_pre_spec_s1_3(2)
  nxt_f_r_s_spec_s1_3(1) :=
    f_r_s_for_csa_3(1) ^
      f_r_c_for_csa_3(1) ^
      sqrt_csa_val_pos_1_3(1)
  nxt_f_r_c_pre_spec_s1_3(1) := Cat(
    (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_1_3(1)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_3(1) := nxt_f_r_c_pre_spec_s1_3(1)
  nxt_f_r_s_spec_s1_3(0) :=
    f_r_s_for_csa_3(1) ^
      f_r_c_for_csa_3(1) ^
      sqrt_csa_val_pos_2_3(1)
  nxt_f_r_c_pre_spec_s1_3(0) := Cat(
    (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_3(1)((F16_REM_W - 1) - 1, 0)) |
      (f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_pos_2_3(1)((F16_REM_W - 1) - 1, 0)),
    "b1".U(1.W)
  )
  nxt_f_r_c_spec_s1_3(0) := nxt_f_r_c_pre_spec_s1_3(0)
  if (S0_CSA_IS_MERGED == 1) {
    adder_9b_for_nxt_cycle_s0_qds_spec_0(4) :=
      nxt_f_r_s_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(3) :=
      nxt_f_r_s_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(2) :=
      nxt_f_r_s_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2, 69 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(1) :=
      nxt_f_r_s_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(0) :=
      nxt_f_r_s_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2, 69 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(4) :=
      nxt_f_r_s_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(3) :=
      nxt_f_r_s_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(2) :=
      nxt_f_r_s_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(1) :=
      nxt_f_r_s_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(0) :=
      nxt_f_r_s_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(69 - 2 - 2, 69 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(4) :=
      nxt_f_r_s_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(3) :=
      nxt_f_r_s_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(2) :=
      nxt_f_r_s_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2, 33 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(1) :=
      nxt_f_r_s_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(0) :=
      nxt_f_r_s_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2, 33 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(4) :=
      nxt_f_r_s_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(3) :=
      nxt_f_r_s_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(2) :=
      nxt_f_r_s_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(1) :=
      nxt_f_r_s_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(0) :=
      nxt_f_r_s_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(33 - 2 - 2, 33 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(4) :=
      nxt_f_r_s_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(3) :=
      nxt_f_r_s_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(2) :=
      nxt_f_r_s_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2, 51 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(1) :=
      nxt_f_r_s_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(0) :=
      nxt_f_r_s_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2, 51 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(4) :=
      nxt_f_r_s_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(3) :=
      nxt_f_r_s_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(2) :=
      nxt_f_r_s_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(1) :=
      nxt_f_r_s_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(0) :=
      nxt_f_r_s_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(51 - 2 - 2, 51 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(4) :=
      nxt_f_r_s_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(3) :=
      nxt_f_r_s_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(2) :=
      nxt_f_r_s_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2, 15 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(1) :=
      nxt_f_r_s_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(0) :=
      nxt_f_r_s_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2, 15 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(4) :=
      nxt_f_r_s_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(3) :=
      nxt_f_r_s_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(2) :=
      nxt_f_r_s_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(1) :=
      nxt_f_r_s_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(0) :=
      nxt_f_r_s_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_merged(0)(15 - 2 - 2, 15 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
  }
  else {
    adder_9b_for_nxt_cycle_s0_qds_spec_0(4) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(3) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(2) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(1) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_0(0) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_0(1)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(4) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(3) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(2) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(1) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_0(0) :=
      nxt_f_r_s_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_0(0)(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_0(1)(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(4) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(3) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(2) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(1) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_1(0) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_1(1)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(4) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(3) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(2) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(1) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_1(0) :=
      nxt_f_r_s_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_1(0)(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_1(1)(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(4) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(3) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(2) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(1) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_2(0) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_2(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(4) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(3) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(2) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(1) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_2(0) :=
      nxt_f_r_s_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_2(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_2(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(4) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_2_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(3) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_neg_1_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(2) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(1) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_1_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_9b_for_nxt_cycle_s0_qds_spec_3(0) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 9 + 1) +&
        sqrt_csa_val_pos_2_3(1)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(4) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_2_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(3) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_neg_1_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(2) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(1) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_1_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
    adder_10b_for_nxt_cycle_s1_qds_spec_3(0) :=
      nxt_f_r_s_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        nxt_f_r_c_3(0)(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 10 + 1) +&
        sqrt_csa_val_pos_2_3(1)(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 10 + 1)
  }

  nxt_rt_spec_s1_0(4) := nxt_rt_m1_0(0) | mask_rt_neg_2(1)
  nxt_rt_spec_s1_0(3) := nxt_rt_m1_0(0) | mask_rt_neg_1(1)
  nxt_rt_spec_s1_0(2) := nxt_rt_0(0)
  nxt_rt_spec_s1_0(1) := nxt_rt_0(0) | mask_rt_pos_1(1)
  nxt_rt_spec_s1_0(0) := nxt_rt_0(0) | mask_rt_pos_2(1)
  a0_spec_s1_0(4) := nxt_rt_spec_s1_0(4)(F64_FULL_RT_W - 1)
  a2_spec_s1_0(4) := nxt_rt_spec_s1_0(4)(F64_FULL_RT_W - 3)
  a3_spec_s1_0(4) := nxt_rt_spec_s1_0(4)(F64_FULL_RT_W - 4)
  a4_spec_s1_0(4) := nxt_rt_spec_s1_0(4)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_2_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_2_0.a0_i <> a0_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.a2_i <> a2_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.a3_i <> a3_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.a4_i <> a4_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.m_neg_1_o <> m_neg_1_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.m_neg_0_o <> m_neg_0_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.m_pos_1_o <> m_pos_1_spec_s1_0(4)
  u_r4_qds_cg_spec_s1_neg_2_0.m_pos_2_o <> m_pos_2_spec_s1_0(4)

  a0_spec_s1_0(3) := nxt_rt_spec_s1_0(3)(F64_FULL_RT_W - 1)
  a2_spec_s1_0(3) := nxt_rt_spec_s1_0(3)(F64_FULL_RT_W - 3)
  a3_spec_s1_0(3) := nxt_rt_spec_s1_0(3)(F64_FULL_RT_W - 4)
  a4_spec_s1_0(3) := nxt_rt_spec_s1_0(3)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_1_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_1_0.a0_i <> a0_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.a2_i <> a2_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.a3_i <> a3_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.a4_i <> a4_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.m_neg_1_o <> m_neg_1_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.m_neg_0_o <> m_neg_0_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.m_pos_1_o <> m_pos_1_spec_s1_0(3)
  u_r4_qds_cg_spec_s1_neg_1_0.m_pos_2_o <> m_pos_2_spec_s1_0(3)

  a0_spec_s1_0(2) := nxt_rt_spec_s1_0(2)(F64_FULL_RT_W - 1)
  a2_spec_s1_0(2) := nxt_rt_spec_s1_0(2)(F64_FULL_RT_W - 3)
  a3_spec_s1_0(2) := nxt_rt_spec_s1_0(2)(F64_FULL_RT_W - 4)
  a4_spec_s1_0(2) := nxt_rt_spec_s1_0(2)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_0_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_0_0.a0_i <> a0_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.a2_i <> a2_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.a3_i <> a3_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.a4_i <> a4_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.m_neg_1_o <> m_neg_1_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.m_neg_0_o <> m_neg_0_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.m_pos_1_o <> m_pos_1_spec_s1_0(2)
  u_r4_qds_cg_spec_s1_neg_0_0.m_pos_2_o <> m_pos_2_spec_s1_0(2)

  a0_spec_s1_0(1) := nxt_rt_spec_s1_0(1)(F64_FULL_RT_W - 1)
  a2_spec_s1_0(1) := nxt_rt_spec_s1_0(1)(F64_FULL_RT_W - 3)
  a3_spec_s1_0(1) := nxt_rt_spec_s1_0(1)(F64_FULL_RT_W - 4)
  a4_spec_s1_0(1) := nxt_rt_spec_s1_0(1)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_1_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_1_0.a0_i <> a0_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.a2_i <> a2_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.a3_i <> a3_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.a4_i <> a4_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.m_neg_1_o <> m_neg_1_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.m_neg_0_o <> m_neg_0_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.m_pos_1_o <> m_pos_1_spec_s1_0(1)
  u_r4_qds_cg_spec_s1_pos_1_0.m_pos_2_o <> m_pos_2_spec_s1_0(1)

  a0_spec_s1_0(0) := nxt_rt_spec_s1_0(0)(F64_FULL_RT_W - 1)
  a2_spec_s1_0(0) := nxt_rt_spec_s1_0(0)(F64_FULL_RT_W - 3)
  a3_spec_s1_0(0) := nxt_rt_spec_s1_0(0)(F64_FULL_RT_W - 4)
  a4_spec_s1_0(0) := nxt_rt_spec_s1_0(0)(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_2_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_2_0.a0_i <> a0_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.a2_i <> a2_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.a3_i <> a3_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.a4_i <> a4_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.m_neg_1_o <> m_neg_1_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.m_neg_0_o <> m_neg_0_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.m_pos_1_o <> m_pos_1_spec_s1_0(0)
  u_r4_qds_cg_spec_s1_pos_2_0.m_pos_2_o <> m_pos_2_spec_s1_0(0)

  nxt_rt_spec_s1_1(4) := nxt_rt_m1_1(0) | mask_rt_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s1_1(3) := nxt_rt_m1_1(0) | mask_rt_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s1_1(2) := nxt_rt_1(0)
  nxt_rt_spec_s1_1(1) := nxt_rt_1(0) | mask_rt_pos_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  nxt_rt_spec_s1_1(0) := nxt_rt_1(0) | mask_rt_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)
  a0_spec_s1_1(4) := nxt_rt_spec_s1_1(4)(F32_FULL_RT_W - 1)
  a2_spec_s1_1(4) := nxt_rt_spec_s1_1(4)(F32_FULL_RT_W - 3)
  a3_spec_s1_1(4) := nxt_rt_spec_s1_1(4)(F32_FULL_RT_W - 4)
  a4_spec_s1_1(4) := nxt_rt_spec_s1_1(4)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_2_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_2_1.a0_i <> a0_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.a2_i <> a2_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.a3_i <> a3_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.a4_i <> a4_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.m_neg_1_o <> m_neg_1_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.m_neg_0_o <> m_neg_0_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.m_pos_1_o <> m_pos_1_spec_s1_1(4)
  u_r4_qds_cg_spec_s1_neg_2_1.m_pos_2_o <> m_pos_2_spec_s1_1(4)

  a0_spec_s1_1(3) := nxt_rt_spec_s1_1(3)(F32_FULL_RT_W - 1)
  a2_spec_s1_1(3) := nxt_rt_spec_s1_1(3)(F32_FULL_RT_W - 3)
  a3_spec_s1_1(3) := nxt_rt_spec_s1_1(3)(F32_FULL_RT_W - 4)
  a4_spec_s1_1(3) := nxt_rt_spec_s1_1(3)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_1_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_1_1.a0_i <> a0_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.a2_i <> a2_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.a3_i <> a3_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.a4_i <> a4_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.m_neg_1_o <> m_neg_1_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.m_neg_0_o <> m_neg_0_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.m_pos_1_o <> m_pos_1_spec_s1_1(3)
  u_r4_qds_cg_spec_s1_neg_1_1.m_pos_2_o <> m_pos_2_spec_s1_1(3)

  a0_spec_s1_1(2) := nxt_rt_spec_s1_1(2)(F32_FULL_RT_W - 1)
  a2_spec_s1_1(2) := nxt_rt_spec_s1_1(2)(F32_FULL_RT_W - 3)
  a3_spec_s1_1(2) := nxt_rt_spec_s1_1(2)(F32_FULL_RT_W - 4)
  a4_spec_s1_1(2) := nxt_rt_spec_s1_1(2)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_0_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_0_1.a0_i <> a0_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.a2_i <> a2_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.a3_i <> a3_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.a4_i <> a4_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.m_neg_1_o <> m_neg_1_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.m_neg_0_o <> m_neg_0_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.m_pos_1_o <> m_pos_1_spec_s1_1(2)
  u_r4_qds_cg_spec_s1_neg_0_1.m_pos_2_o <> m_pos_2_spec_s1_1(2)

  a0_spec_s1_1(1) := nxt_rt_spec_s1_1(1)(F32_FULL_RT_W - 1)
  a2_spec_s1_1(1) := nxt_rt_spec_s1_1(1)(F32_FULL_RT_W - 3)
  a3_spec_s1_1(1) := nxt_rt_spec_s1_1(1)(F32_FULL_RT_W - 4)
  a4_spec_s1_1(1) := nxt_rt_spec_s1_1(1)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_1_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_1_1.a0_i <> a0_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.a2_i <> a2_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.a3_i <> a3_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.a4_i <> a4_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.m_neg_1_o <> m_neg_1_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.m_neg_0_o <> m_neg_0_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.m_pos_1_o <> m_pos_1_spec_s1_1(1)
  u_r4_qds_cg_spec_s1_pos_1_1.m_pos_2_o <> m_pos_2_spec_s1_1(1)

  a0_spec_s1_1(0) := nxt_rt_spec_s1_1(0)(F32_FULL_RT_W - 1)
  a2_spec_s1_1(0) := nxt_rt_spec_s1_1(0)(F32_FULL_RT_W - 3)
  a3_spec_s1_1(0) := nxt_rt_spec_s1_1(0)(F32_FULL_RT_W - 4)
  a4_spec_s1_1(0) := nxt_rt_spec_s1_1(0)(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_2_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_2_1.a0_i <> a0_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.a2_i <> a2_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.a3_i <> a3_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.a4_i <> a4_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.m_neg_1_o <> m_neg_1_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.m_neg_0_o <> m_neg_0_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.m_pos_1_o <> m_pos_1_spec_s1_1(0)
  u_r4_qds_cg_spec_s1_pos_2_1.m_pos_2_o <> m_pos_2_spec_s1_1(0)

  nxt_rt_spec_s1_2(4) := nxt_rt_m1_2(0) | mask_rt_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_2(3) := nxt_rt_m1_2(0) | mask_rt_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_2(2) := nxt_rt_2(0)
  nxt_rt_spec_s1_2(1) := nxt_rt_2(0) | mask_rt_pos_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_2(0) := nxt_rt_2(0) | mask_rt_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  a0_spec_s1_2(4) := nxt_rt_spec_s1_2(4)(F16_FULL_RT_W - 1)
  a2_spec_s1_2(4) := nxt_rt_spec_s1_2(4)(F16_FULL_RT_W - 3)
  a3_spec_s1_2(4) := nxt_rt_spec_s1_2(4)(F16_FULL_RT_W - 4)
  a4_spec_s1_2(4) := nxt_rt_spec_s1_2(4)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_2_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_2_2.a0_i <> a0_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.a2_i <> a2_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.a3_i <> a3_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.a4_i <> a4_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.m_neg_1_o <> m_neg_1_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.m_neg_0_o <> m_neg_0_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.m_pos_1_o <> m_pos_1_spec_s1_2(4)
  u_r4_qds_cg_spec_s1_neg_2_2.m_pos_2_o <> m_pos_2_spec_s1_2(4)

  a0_spec_s1_2(3) := nxt_rt_spec_s1_2(3)(F16_FULL_RT_W - 1)
  a2_spec_s1_2(3) := nxt_rt_spec_s1_2(3)(F16_FULL_RT_W - 3)
  a3_spec_s1_2(3) := nxt_rt_spec_s1_2(3)(F16_FULL_RT_W - 4)
  a4_spec_s1_2(3) := nxt_rt_spec_s1_2(3)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_1_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_1_2.a0_i <> a0_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.a2_i <> a2_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.a3_i <> a3_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.a4_i <> a4_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.m_neg_1_o <> m_neg_1_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.m_neg_0_o <> m_neg_0_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.m_pos_1_o <> m_pos_1_spec_s1_2(3)
  u_r4_qds_cg_spec_s1_neg_1_2.m_pos_2_o <> m_pos_2_spec_s1_2(3)

  a0_spec_s1_2(2) := nxt_rt_spec_s1_2(2)(F16_FULL_RT_W - 1)
  a2_spec_s1_2(2) := nxt_rt_spec_s1_2(2)(F16_FULL_RT_W - 3)
  a3_spec_s1_2(2) := nxt_rt_spec_s1_2(2)(F16_FULL_RT_W - 4)
  a4_spec_s1_2(2) := nxt_rt_spec_s1_2(2)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_0_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_0_2.a0_i <> a0_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.a2_i <> a2_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.a3_i <> a3_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.a4_i <> a4_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.m_neg_1_o <> m_neg_1_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.m_neg_0_o <> m_neg_0_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.m_pos_1_o <> m_pos_1_spec_s1_2(2)
  u_r4_qds_cg_spec_s1_neg_0_2.m_pos_2_o <> m_pos_2_spec_s1_2(2)

  a0_spec_s1_2(1) := nxt_rt_spec_s1_2(1)(F16_FULL_RT_W - 1)
  a2_spec_s1_2(1) := nxt_rt_spec_s1_2(1)(F16_FULL_RT_W - 3)
  a3_spec_s1_2(1) := nxt_rt_spec_s1_2(1)(F16_FULL_RT_W - 4)
  a4_spec_s1_2(1) := nxt_rt_spec_s1_2(1)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_1_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_1_2.a0_i <> a0_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.a2_i <> a2_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.a3_i <> a3_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.a4_i <> a4_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.m_neg_1_o <> m_neg_1_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.m_neg_0_o <> m_neg_0_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.m_pos_1_o <> m_pos_1_spec_s1_2(1)
  u_r4_qds_cg_spec_s1_pos_1_2.m_pos_2_o <> m_pos_2_spec_s1_2(1)

  a0_spec_s1_2(0) := nxt_rt_spec_s1_2(0)(F16_FULL_RT_W - 1)
  a2_spec_s1_2(0) := nxt_rt_spec_s1_2(0)(F16_FULL_RT_W - 3)
  a3_spec_s1_2(0) := nxt_rt_spec_s1_2(0)(F16_FULL_RT_W - 4)
  a4_spec_s1_2(0) := nxt_rt_spec_s1_2(0)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_2_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_2_2.a0_i <> a0_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.a2_i <> a2_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.a3_i <> a3_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.a4_i <> a4_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.m_neg_1_o <> m_neg_1_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.m_neg_0_o <> m_neg_0_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.m_pos_1_o <> m_pos_1_spec_s1_2(0)
  u_r4_qds_cg_spec_s1_pos_2_2.m_pos_2_o <> m_pos_2_spec_s1_2(0)

  nxt_rt_spec_s1_3(4) := nxt_rt_m1_3(0) | mask_rt_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_3(3) := nxt_rt_m1_3(0) | mask_rt_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_3(2) := nxt_rt_3(0)
  nxt_rt_spec_s1_3(1) := nxt_rt_3(0) | mask_rt_pos_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  nxt_rt_spec_s1_3(0) := nxt_rt_3(0) | mask_rt_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)
  a0_spec_s1_3(4) := nxt_rt_spec_s1_3(4)(F16_FULL_RT_W - 1)
  a2_spec_s1_3(4) := nxt_rt_spec_s1_3(4)(F16_FULL_RT_W - 3)
  a3_spec_s1_3(4) := nxt_rt_spec_s1_3(4)(F16_FULL_RT_W - 4)
  a4_spec_s1_3(4) := nxt_rt_spec_s1_3(4)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_2_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_2_3.a0_i <> a0_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.a2_i <> a2_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.a3_i <> a3_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.a4_i <> a4_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.m_neg_1_o <> m_neg_1_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.m_neg_0_o <> m_neg_0_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.m_pos_1_o <> m_pos_1_spec_s1_3(4)
  u_r4_qds_cg_spec_s1_neg_2_3.m_pos_2_o <> m_pos_2_spec_s1_3(4)

  a0_spec_s1_3(3) := nxt_rt_spec_s1_3(3)(F16_FULL_RT_W - 1)
  a2_spec_s1_3(3) := nxt_rt_spec_s1_3(3)(F16_FULL_RT_W - 3)
  a3_spec_s1_3(3) := nxt_rt_spec_s1_3(3)(F16_FULL_RT_W - 4)
  a4_spec_s1_3(3) := nxt_rt_spec_s1_3(3)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_1_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_1_3.a0_i <> a0_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.a2_i <> a2_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.a3_i <> a3_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.a4_i <> a4_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.m_neg_1_o <> m_neg_1_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.m_neg_0_o <> m_neg_0_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.m_pos_1_o <> m_pos_1_spec_s1_3(3)
  u_r4_qds_cg_spec_s1_neg_1_3.m_pos_2_o <> m_pos_2_spec_s1_3(3)

  a0_spec_s1_3(2) := nxt_rt_spec_s1_3(2)(F16_FULL_RT_W - 1)
  a2_spec_s1_3(2) := nxt_rt_spec_s1_3(2)(F16_FULL_RT_W - 3)
  a3_spec_s1_3(2) := nxt_rt_spec_s1_3(2)(F16_FULL_RT_W - 4)
  a4_spec_s1_3(2) := nxt_rt_spec_s1_3(2)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_neg_0_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_neg_0_3.a0_i <> a0_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.a2_i <> a2_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.a3_i <> a3_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.a4_i <> a4_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.m_neg_1_o <> m_neg_1_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.m_neg_0_o <> m_neg_0_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.m_pos_1_o <> m_pos_1_spec_s1_3(2)
  u_r4_qds_cg_spec_s1_neg_0_3.m_pos_2_o <> m_pos_2_spec_s1_3(2)

  a0_spec_s1_3(1) := nxt_rt_spec_s1_3(1)(F16_FULL_RT_W - 1)
  a2_spec_s1_3(1) := nxt_rt_spec_s1_3(1)(F16_FULL_RT_W - 3)
  a3_spec_s1_3(1) := nxt_rt_spec_s1_3(1)(F16_FULL_RT_W - 4)
  a4_spec_s1_3(1) := nxt_rt_spec_s1_3(1)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_1_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_1_3.a0_i <> a0_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.a2_i <> a2_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.a3_i <> a3_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.a4_i <> a4_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.m_neg_1_o <> m_neg_1_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.m_neg_0_o <> m_neg_0_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.m_pos_1_o <> m_pos_1_spec_s1_3(1)
  u_r4_qds_cg_spec_s1_pos_1_3.m_pos_2_o <> m_pos_2_spec_s1_3(1)

  a0_spec_s1_3(0) := nxt_rt_spec_s1_3(0)(F16_FULL_RT_W - 1)
  a2_spec_s1_3(0) := nxt_rt_spec_s1_3(0)(F16_FULL_RT_W - 3)
  a3_spec_s1_3(0) := nxt_rt_spec_s1_3(0)(F16_FULL_RT_W - 4)
  a4_spec_s1_3(0) := nxt_rt_spec_s1_3(0)(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_spec_s1_pos_2_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_spec_s1_pos_2_3.a0_i <> a0_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.a2_i <> a2_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.a3_i <> a3_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.a4_i <> a4_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.m_neg_1_o <> m_neg_1_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.m_neg_0_o <> m_neg_0_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.m_pos_1_o <> m_pos_1_spec_s1_3(0)
  u_r4_qds_cg_spec_s1_pos_2_3.m_pos_2_o <> m_pos_2_spec_s1_3(0)

  if (S1_QDS_SPECULATIVE == 1) {

    val u_r4_qds_s1_0 = Module(new r4_qds_spec())
    u_r4_qds_s1_0.rem_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_0_i
    u_r4_qds_s1_0.sqrt_csa_val_neg_2_msbs_i <> sqrt_csa_val_neg_2_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_0.sqrt_csa_val_neg_1_msbs_i <> sqrt_csa_val_neg_1_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_0.sqrt_csa_val_pos_1_msbs_i <> sqrt_csa_val_pos_1_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_0.sqrt_csa_val_pos_2_msbs_i <> sqrt_csa_val_pos_2_0(0)(F64_REM_W - 1, F64_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_0.m_neg_1_neg_2_i <> m_neg_1_spec_s0_0(4)
    u_r4_qds_s1_0.m_neg_0_neg_2_i <> m_neg_0_spec_s0_0(4)
    u_r4_qds_s1_0.m_pos_1_neg_2_i <> m_pos_1_spec_s0_0(4)
    u_r4_qds_s1_0.m_pos_2_neg_2_i <> m_pos_2_spec_s0_0(4)
    u_r4_qds_s1_0.m_neg_1_neg_1_i <> m_neg_1_spec_s0_0(3)
    u_r4_qds_s1_0.m_neg_0_neg_1_i <> m_neg_0_spec_s0_0(3)
    u_r4_qds_s1_0.m_pos_1_neg_1_i <> m_pos_1_spec_s0_0(3)
    u_r4_qds_s1_0.m_pos_2_neg_1_i <> m_pos_2_spec_s0_0(3)
    u_r4_qds_s1_0.m_neg_1_neg_0_i <> m_neg_1_spec_s0_0(2)
    u_r4_qds_s1_0.m_neg_0_neg_0_i <> m_neg_0_spec_s0_0(2)
    u_r4_qds_s1_0.m_pos_1_neg_0_i <> m_pos_1_spec_s0_0(2)
    u_r4_qds_s1_0.m_pos_2_neg_0_i <> m_pos_2_spec_s0_0(2)
    u_r4_qds_s1_0.m_neg_1_pos_1_i <> m_neg_1_spec_s0_0(1)
    u_r4_qds_s1_0.m_neg_0_pos_1_i <> m_neg_0_spec_s0_0(1)
    u_r4_qds_s1_0.m_pos_1_pos_1_i <> m_pos_1_spec_s0_0(1)
    u_r4_qds_s1_0.m_pos_2_pos_1_i <> m_pos_2_spec_s0_0(1)
    u_r4_qds_s1_0.m_neg_1_pos_2_i <> m_neg_1_spec_s0_0(0)
    u_r4_qds_s1_0.m_neg_0_pos_2_i <> m_neg_0_spec_s0_0(0)
    u_r4_qds_s1_0.m_pos_1_pos_2_i <> m_pos_1_spec_s0_0(0)
    u_r4_qds_s1_0.m_pos_2_pos_2_i <> m_pos_2_spec_s0_0(0)
    u_r4_qds_s1_0.prev_rt_dig_i <> nxt_rt_dig_0(0)
    u_r4_qds_s1_0.rt_dig_o <> nxt_rt_dig_0(1)

    val u_r4_qds_s1_1 = Module(new r4_qds_spec())
    u_r4_qds_s1_1.rem_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_1_i
    u_r4_qds_s1_1.sqrt_csa_val_neg_2_msbs_i <> sqrt_csa_val_neg_2_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_1.sqrt_csa_val_neg_1_msbs_i <> sqrt_csa_val_neg_1_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_1.sqrt_csa_val_pos_1_msbs_i <> sqrt_csa_val_pos_1_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_1.sqrt_csa_val_pos_2_msbs_i <> sqrt_csa_val_pos_2_1(0)(F32_REM_W - 1, F32_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_1.m_neg_1_neg_2_i <> m_neg_1_spec_s0_1(4)
    u_r4_qds_s1_1.m_neg_0_neg_2_i <> m_neg_0_spec_s0_1(4)
    u_r4_qds_s1_1.m_pos_1_neg_2_i <> m_pos_1_spec_s0_1(4)
    u_r4_qds_s1_1.m_pos_2_neg_2_i <> m_pos_2_spec_s0_1(4)
    u_r4_qds_s1_1.m_neg_1_neg_1_i <> m_neg_1_spec_s0_1(3)
    u_r4_qds_s1_1.m_neg_0_neg_1_i <> m_neg_0_spec_s0_1(3)
    u_r4_qds_s1_1.m_pos_1_neg_1_i <> m_pos_1_spec_s0_1(3)
    u_r4_qds_s1_1.m_pos_2_neg_1_i <> m_pos_2_spec_s0_1(3)
    u_r4_qds_s1_1.m_neg_1_neg_0_i <> m_neg_1_spec_s0_1(2)
    u_r4_qds_s1_1.m_neg_0_neg_0_i <> m_neg_0_spec_s0_1(2)
    u_r4_qds_s1_1.m_pos_1_neg_0_i <> m_pos_1_spec_s0_1(2)
    u_r4_qds_s1_1.m_pos_2_neg_0_i <> m_pos_2_spec_s0_1(2)
    u_r4_qds_s1_1.m_neg_1_pos_1_i <> m_neg_1_spec_s0_1(1)
    u_r4_qds_s1_1.m_neg_0_pos_1_i <> m_neg_0_spec_s0_1(1)
    u_r4_qds_s1_1.m_pos_1_pos_1_i <> m_pos_1_spec_s0_1(1)
    u_r4_qds_s1_1.m_pos_2_pos_1_i <> m_pos_2_spec_s0_1(1)
    u_r4_qds_s1_1.m_neg_1_pos_2_i <> m_neg_1_spec_s0_1(0)
    u_r4_qds_s1_1.m_neg_0_pos_2_i <> m_neg_0_spec_s0_1(0)
    u_r4_qds_s1_1.m_pos_1_pos_2_i <> m_pos_1_spec_s0_1(0)
    u_r4_qds_s1_1.m_pos_2_pos_2_i <> m_pos_2_spec_s0_1(0)
    u_r4_qds_s1_1.prev_rt_dig_i <> nxt_rt_dig_1(0)
    u_r4_qds_s1_1.rt_dig_o <> nxt_rt_dig_1(1)

    val u_r4_qds_s1_2 = Module(new r4_qds_spec())
    u_r4_qds_s1_2.rem_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_2_i
    u_r4_qds_s1_2.sqrt_csa_val_neg_2_msbs_i <> sqrt_csa_val_neg_2_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_2.sqrt_csa_val_neg_1_msbs_i <> sqrt_csa_val_neg_1_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_2.sqrt_csa_val_pos_1_msbs_i <> sqrt_csa_val_pos_1_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_2.sqrt_csa_val_pos_2_msbs_i <> sqrt_csa_val_pos_2_2(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_2.m_neg_1_neg_2_i <> m_neg_1_spec_s0_2(4)
    u_r4_qds_s1_2.m_neg_0_neg_2_i <> m_neg_0_spec_s0_2(4)
    u_r4_qds_s1_2.m_pos_1_neg_2_i <> m_pos_1_spec_s0_2(4)
    u_r4_qds_s1_2.m_pos_2_neg_2_i <> m_pos_2_spec_s0_2(4)
    u_r4_qds_s1_2.m_neg_1_neg_1_i <> m_neg_1_spec_s0_2(3)
    u_r4_qds_s1_2.m_neg_0_neg_1_i <> m_neg_0_spec_s0_2(3)
    u_r4_qds_s1_2.m_pos_1_neg_1_i <> m_pos_1_spec_s0_2(3)
    u_r4_qds_s1_2.m_pos_2_neg_1_i <> m_pos_2_spec_s0_2(3)
    u_r4_qds_s1_2.m_neg_1_neg_0_i <> m_neg_1_spec_s0_2(2)
    u_r4_qds_s1_2.m_neg_0_neg_0_i <> m_neg_0_spec_s0_2(2)
    u_r4_qds_s1_2.m_pos_1_neg_0_i <> m_pos_1_spec_s0_2(2)
    u_r4_qds_s1_2.m_pos_2_neg_0_i <> m_pos_2_spec_s0_2(2)
    u_r4_qds_s1_2.m_neg_1_pos_1_i <> m_neg_1_spec_s0_2(1)
    u_r4_qds_s1_2.m_neg_0_pos_1_i <> m_neg_0_spec_s0_2(1)
    u_r4_qds_s1_2.m_pos_1_pos_1_i <> m_pos_1_spec_s0_2(1)
    u_r4_qds_s1_2.m_pos_2_pos_1_i <> m_pos_2_spec_s0_2(1)
    u_r4_qds_s1_2.m_neg_1_pos_2_i <> m_neg_1_spec_s0_2(0)
    u_r4_qds_s1_2.m_neg_0_pos_2_i <> m_neg_0_spec_s0_2(0)
    u_r4_qds_s1_2.m_pos_1_pos_2_i <> m_pos_1_spec_s0_2(0)
    u_r4_qds_s1_2.m_pos_2_pos_2_i <> m_pos_2_spec_s0_2(0)
    u_r4_qds_s1_2.prev_rt_dig_i <> nxt_rt_dig_2(0)
    u_r4_qds_s1_2.rt_dig_o <> nxt_rt_dig_2(1)

    val u_r4_qds_s1_3 = Module(new r4_qds_spec())
    u_r4_qds_s1_3.rem_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_3_i
    u_r4_qds_s1_3.sqrt_csa_val_neg_2_msbs_i <> sqrt_csa_val_neg_2_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_3.sqrt_csa_val_neg_1_msbs_i <> sqrt_csa_val_neg_1_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_3.sqrt_csa_val_pos_1_msbs_i <> sqrt_csa_val_pos_1_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_3.sqrt_csa_val_pos_2_msbs_i <> sqrt_csa_val_pos_2_3(0)(F16_REM_W - 1, F16_REM_W - 1 - 9 + 1)
    u_r4_qds_s1_3.m_neg_1_neg_2_i <> m_neg_1_spec_s0_3(4)
    u_r4_qds_s1_3.m_neg_0_neg_2_i <> m_neg_0_spec_s0_3(4)
    u_r4_qds_s1_3.m_pos_1_neg_2_i <> m_pos_1_spec_s0_3(4)
    u_r4_qds_s1_3.m_pos_2_neg_2_i <> m_pos_2_spec_s0_3(4)
    u_r4_qds_s1_3.m_neg_1_neg_1_i <> m_neg_1_spec_s0_3(3)
    u_r4_qds_s1_3.m_neg_0_neg_1_i <> m_neg_0_spec_s0_3(3)
    u_r4_qds_s1_3.m_pos_1_neg_1_i <> m_pos_1_spec_s0_3(3)
    u_r4_qds_s1_3.m_pos_2_neg_1_i <> m_pos_2_spec_s0_3(3)
    u_r4_qds_s1_3.m_neg_1_neg_0_i <> m_neg_1_spec_s0_3(2)
    u_r4_qds_s1_3.m_neg_0_neg_0_i <> m_neg_0_spec_s0_3(2)
    u_r4_qds_s1_3.m_pos_1_neg_0_i <> m_pos_1_spec_s0_3(2)
    u_r4_qds_s1_3.m_pos_2_neg_0_i <> m_pos_2_spec_s0_3(2)
    u_r4_qds_s1_3.m_neg_1_pos_1_i <> m_neg_1_spec_s0_3(1)
    u_r4_qds_s1_3.m_neg_0_pos_1_i <> m_neg_0_spec_s0_3(1)
    u_r4_qds_s1_3.m_pos_1_pos_1_i <> m_pos_1_spec_s0_3(1)
    u_r4_qds_s1_3.m_pos_2_pos_1_i <> m_pos_2_spec_s0_3(1)
    u_r4_qds_s1_3.m_neg_1_pos_2_i <> m_neg_1_spec_s0_3(0)
    u_r4_qds_s1_3.m_neg_0_pos_2_i <> m_neg_0_spec_s0_3(0)
    u_r4_qds_s1_3.m_pos_1_pos_2_i <> m_pos_1_spec_s0_3(0)
    u_r4_qds_s1_3.m_pos_2_pos_2_i <> m_pos_2_spec_s0_3(0)
    u_r4_qds_s1_3.prev_rt_dig_i <> nxt_rt_dig_3(0)
    u_r4_qds_s1_3.rt_dig_o <> nxt_rt_dig_3(1)

  }
  else {

    val u_r4_qds_s1_0 = Module(new r4_qds())
    u_r4_qds_s1_0.rem_i <> adder_7b_res_for_s1_qds_0
    u_r4_qds_s1_0.m_neg_1_i <> m_neg_1_0(1)
    u_r4_qds_s1_0.m_neg_0_i <> m_neg_0_0(1)
    u_r4_qds_s1_0.m_pos_1_i <> m_pos_1_0(1)
    u_r4_qds_s1_0.m_pos_2_i <> m_pos_2_0(1)
    u_r4_qds_s1_0.rt_dig_o <> nxt_rt_dig_0(1)

    val u_r4_qds_s1_1 = Module(new r4_qds())
    u_r4_qds_s1_1.rem_i <> adder_7b_res_for_s1_qds_1
    u_r4_qds_s1_1.m_neg_1_i <> m_neg_1_1(1)
    u_r4_qds_s1_1.m_neg_0_i <> m_neg_0_1(1)
    u_r4_qds_s1_1.m_pos_1_i <> m_pos_1_1(1)
    u_r4_qds_s1_1.m_pos_2_i <> m_pos_2_1(1)
    u_r4_qds_s1_1.rt_dig_o <> nxt_rt_dig_1(1)

    val u_r4_qds_s1_2 = Module(new r4_qds())
    u_r4_qds_s1_2.rem_i <> adder_7b_res_for_s1_qds_2
    u_r4_qds_s1_2.m_neg_1_i <> m_neg_1_2(1)
    u_r4_qds_s1_2.m_neg_0_i <> m_neg_0_2(1)
    u_r4_qds_s1_2.m_pos_1_i <> m_pos_1_2(1)
    u_r4_qds_s1_2.m_pos_2_i <> m_pos_2_2(1)
    u_r4_qds_s1_2.rt_dig_o <> nxt_rt_dig_2(1)

    val u_r4_qds_s1_3 = Module(new r4_qds())
    u_r4_qds_s1_3.rem_i <> adder_7b_res_for_s1_qds_3
    u_r4_qds_s1_3.m_neg_1_i <> m_neg_1_3(1)
    u_r4_qds_s1_3.m_neg_0_i <> m_neg_0_3(1)
    u_r4_qds_s1_3.m_pos_1_i <> m_pos_1_3(1)
    u_r4_qds_s1_3.m_pos_2_i <> m_pos_2_3(1)
    u_r4_qds_s1_3.rt_dig_o <> nxt_rt_dig_3(1)

  }

  adder_7b_res_for_nxt_cycle_s0_qds_0_o :=
    (Fill(7, nxt_rt_dig_0(1)(4)) & adder_9b_for_nxt_cycle_s0_qds_spec_0(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(1)(3)) & adder_9b_for_nxt_cycle_s0_qds_spec_0(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(1)(2)) & adder_9b_for_nxt_cycle_s0_qds_spec_0(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(1)(1)) & adder_9b_for_nxt_cycle_s0_qds_spec_0(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_0(1)(0)) & adder_9b_for_nxt_cycle_s0_qds_spec_0(0)(8, 2))
  adder_7b_res_for_nxt_cycle_s0_qds_1_o :=
    (Fill(7, nxt_rt_dig_1(1)(4)) & adder_9b_for_nxt_cycle_s0_qds_spec_1(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(1)(3)) & adder_9b_for_nxt_cycle_s0_qds_spec_1(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(1)(2)) & adder_9b_for_nxt_cycle_s0_qds_spec_1(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(1)(1)) & adder_9b_for_nxt_cycle_s0_qds_spec_1(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_1(1)(0)) & adder_9b_for_nxt_cycle_s0_qds_spec_1(0)(8, 2))
  adder_7b_res_for_nxt_cycle_s0_qds_2_o :=
    (Fill(7, nxt_rt_dig_2(1)(4)) & adder_9b_for_nxt_cycle_s0_qds_spec_2(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(1)(3)) & adder_9b_for_nxt_cycle_s0_qds_spec_2(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(1)(2)) & adder_9b_for_nxt_cycle_s0_qds_spec_2(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(1)(1)) & adder_9b_for_nxt_cycle_s0_qds_spec_2(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_2(1)(0)) & adder_9b_for_nxt_cycle_s0_qds_spec_2(0)(8, 2))
  adder_7b_res_for_nxt_cycle_s0_qds_3_o :=
    (Fill(7, nxt_rt_dig_3(1)(4)) & adder_9b_for_nxt_cycle_s0_qds_spec_3(4)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(1)(3)) & adder_9b_for_nxt_cycle_s0_qds_spec_3(3)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(1)(2)) & adder_9b_for_nxt_cycle_s0_qds_spec_3(2)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(1)(1)) & adder_9b_for_nxt_cycle_s0_qds_spec_3(1)(8, 2)) |
      (Fill(7, nxt_rt_dig_3(1)(0)) & adder_9b_for_nxt_cycle_s0_qds_spec_3(0)(8, 2))
  adder_9b_res_for_nxt_cycle_s1_qds_0_o :=
    (Fill(9, nxt_rt_dig_0(1)(4)) & adder_10b_for_nxt_cycle_s1_qds_spec_0(4)(9, 1)) |
      (Fill(9, nxt_rt_dig_0(1)(3)) & adder_10b_for_nxt_cycle_s1_qds_spec_0(3)(9, 1)) |
      (Fill(9, nxt_rt_dig_0(1)(2)) & adder_10b_for_nxt_cycle_s1_qds_spec_0(2)(9, 1)) |
      (Fill(9, nxt_rt_dig_0(1)(1)) & adder_10b_for_nxt_cycle_s1_qds_spec_0(1)(9, 1)) |
      (Fill(9, nxt_rt_dig_0(1)(0)) & adder_10b_for_nxt_cycle_s1_qds_spec_0(0)(9, 1))
  adder_9b_res_for_nxt_cycle_s1_qds_1_o :=
    (Fill(9, nxt_rt_dig_1(1)(4)) & adder_10b_for_nxt_cycle_s1_qds_spec_1(4)(9, 1)) |
      (Fill(9, nxt_rt_dig_1(1)(3)) & adder_10b_for_nxt_cycle_s1_qds_spec_1(3)(9, 1)) |
      (Fill(9, nxt_rt_dig_1(1)(2)) & adder_10b_for_nxt_cycle_s1_qds_spec_1(2)(9, 1)) |
      (Fill(9, nxt_rt_dig_1(1)(1)) & adder_10b_for_nxt_cycle_s1_qds_spec_1(1)(9, 1)) |
      (Fill(9, nxt_rt_dig_1(1)(0)) & adder_10b_for_nxt_cycle_s1_qds_spec_1(0)(9, 1))
  adder_9b_res_for_nxt_cycle_s1_qds_2_o :=
    (Fill(9, nxt_rt_dig_2(1)(4)) & adder_10b_for_nxt_cycle_s1_qds_spec_2(4)(9, 1)) |
      (Fill(9, nxt_rt_dig_2(1)(3)) & adder_10b_for_nxt_cycle_s1_qds_spec_2(3)(9, 1)) |
      (Fill(9, nxt_rt_dig_2(1)(2)) & adder_10b_for_nxt_cycle_s1_qds_spec_2(2)(9, 1)) |
      (Fill(9, nxt_rt_dig_2(1)(1)) & adder_10b_for_nxt_cycle_s1_qds_spec_2(1)(9, 1)) |
      (Fill(9, nxt_rt_dig_2(1)(0)) & adder_10b_for_nxt_cycle_s1_qds_spec_2(0)(9, 1))
  adder_9b_res_for_nxt_cycle_s1_qds_3_o :=
    (Fill(9, nxt_rt_dig_3(1)(4)) & adder_10b_for_nxt_cycle_s1_qds_spec_3(4)(9, 1)) |
      (Fill(9, nxt_rt_dig_3(1)(3)) & adder_10b_for_nxt_cycle_s1_qds_spec_3(3)(9, 1)) |
      (Fill(9, nxt_rt_dig_3(1)(2)) & adder_10b_for_nxt_cycle_s1_qds_spec_3(2)(9, 1)) |
      (Fill(9, nxt_rt_dig_3(1)(1)) & adder_10b_for_nxt_cycle_s1_qds_spec_3(1)(9, 1)) |
      (Fill(9, nxt_rt_dig_3(1)(0)) & adder_10b_for_nxt_cycle_s1_qds_spec_3(0)(9, 1))
  m_neg_1_to_nxt_cycle_0_o :=
    (Fill(7, nxt_rt_dig_0(1)(4)) & m_neg_1_spec_s1_0(4)) |
      (Fill(7, nxt_rt_dig_0(1)(3)) & m_neg_1_spec_s1_0(3)) |
      (Fill(7, nxt_rt_dig_0(1)(2)) & m_neg_1_spec_s1_0(2)) |
      (Fill(7, nxt_rt_dig_0(1)(1)) & m_neg_1_spec_s1_0(1)) |
      (Fill(7, nxt_rt_dig_0(1)(0)) & m_neg_1_spec_s1_0(0))
  m_neg_1_to_nxt_cycle_1_o :=
    (Fill(7, nxt_rt_dig_1(1)(4)) & m_neg_1_spec_s1_1(4)) |
      (Fill(7, nxt_rt_dig_1(1)(3)) & m_neg_1_spec_s1_1(3)) |
      (Fill(7, nxt_rt_dig_1(1)(2)) & m_neg_1_spec_s1_1(2)) |
      (Fill(7, nxt_rt_dig_1(1)(1)) & m_neg_1_spec_s1_1(1)) |
      (Fill(7, nxt_rt_dig_1(1)(0)) & m_neg_1_spec_s1_1(0))
  m_neg_1_to_nxt_cycle_2_o :=
    (Fill(7, nxt_rt_dig_2(1)(4)) & m_neg_1_spec_s1_2(4)) |
      (Fill(7, nxt_rt_dig_2(1)(3)) & m_neg_1_spec_s1_2(3)) |
      (Fill(7, nxt_rt_dig_2(1)(2)) & m_neg_1_spec_s1_2(2)) |
      (Fill(7, nxt_rt_dig_2(1)(1)) & m_neg_1_spec_s1_2(1)) |
      (Fill(7, nxt_rt_dig_2(1)(0)) & m_neg_1_spec_s1_2(0))
  m_neg_1_to_nxt_cycle_3_o :=
    (Fill(7, nxt_rt_dig_3(1)(4)) & m_neg_1_spec_s1_3(4)) |
      (Fill(7, nxt_rt_dig_3(1)(3)) & m_neg_1_spec_s1_3(3)) |
      (Fill(7, nxt_rt_dig_3(1)(2)) & m_neg_1_spec_s1_3(2)) |
      (Fill(7, nxt_rt_dig_3(1)(1)) & m_neg_1_spec_s1_3(1)) |
      (Fill(7, nxt_rt_dig_3(1)(0)) & m_neg_1_spec_s1_3(0))
  m_neg_0_to_nxt_cycle_0_o :=
    (Fill(7, nxt_rt_dig_0(1)(4)) & m_neg_0_spec_s1_0(4)) |
      (Fill(7, nxt_rt_dig_0(1)(3)) & m_neg_0_spec_s1_0(3)) |
      (Fill(7, nxt_rt_dig_0(1)(2)) & m_neg_0_spec_s1_0(2)) |
      (Fill(7, nxt_rt_dig_0(1)(1)) & m_neg_0_spec_s1_0(1)) |
      (Fill(7, nxt_rt_dig_0(1)(0)) & m_neg_0_spec_s1_0(0))
  m_neg_0_to_nxt_cycle_1_o :=
    (Fill(7, nxt_rt_dig_1(1)(4)) & m_neg_0_spec_s1_1(4)) |
      (Fill(7, nxt_rt_dig_1(1)(3)) & m_neg_0_spec_s1_1(3)) |
      (Fill(7, nxt_rt_dig_1(1)(2)) & m_neg_0_spec_s1_1(2)) |
      (Fill(7, nxt_rt_dig_1(1)(1)) & m_neg_0_spec_s1_1(1)) |
      (Fill(7, nxt_rt_dig_1(1)(0)) & m_neg_0_spec_s1_1(0))
  m_neg_0_to_nxt_cycle_2_o :=
    (Fill(7, nxt_rt_dig_2(1)(4)) & m_neg_0_spec_s1_2(4)) |
      (Fill(7, nxt_rt_dig_2(1)(3)) & m_neg_0_spec_s1_2(3)) |
      (Fill(7, nxt_rt_dig_2(1)(2)) & m_neg_0_spec_s1_2(2)) |
      (Fill(7, nxt_rt_dig_2(1)(1)) & m_neg_0_spec_s1_2(1)) |
      (Fill(7, nxt_rt_dig_2(1)(0)) & m_neg_0_spec_s1_2(0))
  m_neg_0_to_nxt_cycle_3_o :=
    (Fill(7, nxt_rt_dig_3(1)(4)) & m_neg_0_spec_s1_3(4)) |
      (Fill(7, nxt_rt_dig_3(1)(3)) & m_neg_0_spec_s1_3(3)) |
      (Fill(7, nxt_rt_dig_3(1)(2)) & m_neg_0_spec_s1_3(2)) |
      (Fill(7, nxt_rt_dig_3(1)(1)) & m_neg_0_spec_s1_3(1)) |
      (Fill(7, nxt_rt_dig_3(1)(0)) & m_neg_0_spec_s1_3(0))
  m_pos_1_to_nxt_cycle_0_o :=
    (Fill(7, nxt_rt_dig_0(1)(4)) & m_pos_1_spec_s1_0(4)) |
      (Fill(7, nxt_rt_dig_0(1)(3)) & m_pos_1_spec_s1_0(3)) |
      (Fill(7, nxt_rt_dig_0(1)(2)) & m_pos_1_spec_s1_0(2)) |
      (Fill(7, nxt_rt_dig_0(1)(1)) & m_pos_1_spec_s1_0(1)) |
      (Fill(7, nxt_rt_dig_0(1)(0)) & m_pos_1_spec_s1_0(0))
  m_pos_1_to_nxt_cycle_1_o :=
    (Fill(7, nxt_rt_dig_1(1)(4)) & m_pos_1_spec_s1_1(4)) |
      (Fill(7, nxt_rt_dig_1(1)(3)) & m_pos_1_spec_s1_1(3)) |
      (Fill(7, nxt_rt_dig_1(1)(2)) & m_pos_1_spec_s1_1(2)) |
      (Fill(7, nxt_rt_dig_1(1)(1)) & m_pos_1_spec_s1_1(1)) |
      (Fill(7, nxt_rt_dig_1(1)(0)) & m_pos_1_spec_s1_1(0))
  m_pos_1_to_nxt_cycle_2_o :=
    (Fill(7, nxt_rt_dig_2(1)(4)) & m_pos_1_spec_s1_2(4)) |
      (Fill(7, nxt_rt_dig_2(1)(3)) & m_pos_1_spec_s1_2(3)) |
      (Fill(7, nxt_rt_dig_2(1)(2)) & m_pos_1_spec_s1_2(2)) |
      (Fill(7, nxt_rt_dig_2(1)(1)) & m_pos_1_spec_s1_2(1)) |
      (Fill(7, nxt_rt_dig_2(1)(0)) & m_pos_1_spec_s1_2(0))
  m_pos_1_to_nxt_cycle_3_o :=
    (Fill(7, nxt_rt_dig_3(1)(4)) & m_pos_1_spec_s1_3(4)) |
      (Fill(7, nxt_rt_dig_3(1)(3)) & m_pos_1_spec_s1_3(3)) |
      (Fill(7, nxt_rt_dig_3(1)(2)) & m_pos_1_spec_s1_3(2)) |
      (Fill(7, nxt_rt_dig_3(1)(1)) & m_pos_1_spec_s1_3(1)) |
      (Fill(7, nxt_rt_dig_3(1)(0)) & m_pos_1_spec_s1_3(0))
  m_pos_2_to_nxt_cycle_0_o :=
    (Fill(7, nxt_rt_dig_0(1)(4)) & m_pos_2_spec_s1_0(4)) |
      (Fill(7, nxt_rt_dig_0(1)(3)) & m_pos_2_spec_s1_0(3)) |
      (Fill(7, nxt_rt_dig_0(1)(2)) & m_pos_2_spec_s1_0(2)) |
      (Fill(7, nxt_rt_dig_0(1)(1)) & m_pos_2_spec_s1_0(1)) |
      (Fill(7, nxt_rt_dig_0(1)(0)) & m_pos_2_spec_s1_0(0))
  m_pos_2_to_nxt_cycle_1_o :=
    (Fill(7, nxt_rt_dig_1(1)(4)) & m_pos_2_spec_s1_1(4)) |
      (Fill(7, nxt_rt_dig_1(1)(3)) & m_pos_2_spec_s1_1(3)) |
      (Fill(7, nxt_rt_dig_1(1)(2)) & m_pos_2_spec_s1_1(2)) |
      (Fill(7, nxt_rt_dig_1(1)(1)) & m_pos_2_spec_s1_1(1)) |
      (Fill(7, nxt_rt_dig_1(1)(0)) & m_pos_2_spec_s1_1(0))
  m_pos_2_to_nxt_cycle_2_o :=
    (Fill(7, nxt_rt_dig_2(1)(4)) & m_pos_2_spec_s1_2(4)) |
      (Fill(7, nxt_rt_dig_2(1)(3)) & m_pos_2_spec_s1_2(3)) |
      (Fill(7, nxt_rt_dig_2(1)(2)) & m_pos_2_spec_s1_2(2)) |
      (Fill(7, nxt_rt_dig_2(1)(1)) & m_pos_2_spec_s1_2(1)) |
      (Fill(7, nxt_rt_dig_2(1)(0)) & m_pos_2_spec_s1_2(0))
  m_pos_2_to_nxt_cycle_3_o :=
    (Fill(7, nxt_rt_dig_3(1)(4)) & m_pos_2_spec_s1_3(4)) |
      (Fill(7, nxt_rt_dig_3(1)(3)) & m_pos_2_spec_s1_3(3)) |
      (Fill(7, nxt_rt_dig_3(1)(2)) & m_pos_2_spec_s1_3(2)) |
      (Fill(7, nxt_rt_dig_3(1)(1)) & m_pos_2_spec_s1_3(1)) |
      (Fill(7, nxt_rt_dig_3(1)(0)) & m_pos_2_spec_s1_3(0))
  nxt_rt_0(1) :=
    (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(4)) & nxt_rt_spec_s1_0(4)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(3)) & nxt_rt_spec_s1_0(3)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(2)) & nxt_rt_spec_s1_0(2)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(1)) & nxt_rt_spec_s1_0(1)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(0)) & nxt_rt_spec_s1_0(0))
  nxt_rt_m1_0(1) :=
    (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(4)) & (nxt_rt_m1_0(0) | mask_rt_m1_neg_2(1))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(3)) & (nxt_rt_m1_0(0) | mask_rt_m1_neg_1(1))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(2)) & (nxt_rt_m1_0(0) | mask_rt_m1_neg_0(1))) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(1)) & nxt_rt_0(0)) |
      (Fill(F64_FULL_RT_W, nxt_rt_dig_0(1)(0)) & (nxt_rt_0(0) | mask_rt_m1_pos_2(1)))
  nxt_rt_1(1) :=
    (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(4)) & nxt_rt_spec_s1_1(4)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(3)) & nxt_rt_spec_s1_1(3)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(2)) & nxt_rt_spec_s1_1(2)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(1)) & nxt_rt_spec_s1_1(1)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(0)) & nxt_rt_spec_s1_1(0))
  nxt_rt_m1_1(1) :=
    (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(4)) & (nxt_rt_m1_1(0) | mask_rt_m1_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(3)) & (nxt_rt_m1_1(0) | mask_rt_m1_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(2)) & (nxt_rt_m1_1(0) | mask_rt_m1_neg_0(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1))) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(1)) & nxt_rt_1(0)) |
      (Fill(F32_FULL_RT_W, nxt_rt_dig_1(1)(0)) & (nxt_rt_1(0) | mask_rt_m1_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F32_FULL_RT_W + 1)))
  nxt_rt_2(1) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(4)) & nxt_rt_spec_s1_2(4)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(3)) & nxt_rt_spec_s1_2(3)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(2)) & nxt_rt_spec_s1_2(2)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(1)) & nxt_rt_spec_s1_2(1)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(0)) & nxt_rt_spec_s1_2(0))
  nxt_rt_m1_2(1) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(4)) & (nxt_rt_m1_2(0) | mask_rt_m1_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(3)) & (nxt_rt_m1_2(0) | mask_rt_m1_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(2)) & (nxt_rt_m1_2(0) | mask_rt_m1_neg_0(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(1)) & nxt_rt_2(0)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_2(1)(0)) & (nxt_rt_2(0) | mask_rt_m1_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)))
  nxt_rt_3(1) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(4)) & nxt_rt_spec_s1_3(4)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(3)) & nxt_rt_spec_s1_3(3)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(2)) & nxt_rt_spec_s1_3(2)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(1)) & nxt_rt_spec_s1_3(1)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(0)) & nxt_rt_spec_s1_3(0))
  nxt_rt_m1_3(1) :=
    (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(4)) & (nxt_rt_m1_3(0) | mask_rt_m1_neg_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(3)) & (nxt_rt_m1_3(0) | mask_rt_m1_neg_1(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(2)) & (nxt_rt_m1_3(0) | mask_rt_m1_neg_0(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1))) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(1)) & nxt_rt_3(0)) |
      (Fill(F16_FULL_RT_W, nxt_rt_dig_3(1)(0)) & (nxt_rt_3(0) | mask_rt_m1_pos_2(1)(F64_FULL_RT_W - 1, F64_FULL_RT_W - 1 - F16_FULL_RT_W + 1)))
  nxt_rt_o :=
    (Fill(56, fp_fmt_i(0)) & Cat(
      nxt_rt_0(1)(53, 40),
      nxt_rt_2(1)(13, 0),
      nxt_rt_1(1)(25, 12),
      nxt_rt_3(1)(13, 0)
    )) |
      (Fill(56, fp_fmt_i(1)) & Cat(nxt_rt_0(1)(53, 28), "b0".U(2.W), nxt_rt_1(1)(25, 0), "b0".U(2.W))) |
      (Fill(56, fp_fmt_i(2)) & Cat(nxt_rt_0(1)(53, 0), "b0".U(2.W)))
  nxt_rt_m1_o :=
    (Fill(53, fp_fmt_i(0)) & Cat(
      nxt_rt_m1_0(1)(52, 40),
      nxt_rt_m1_2(1)(12, 0),
      nxt_rt_m1_1(1)(24, 12),
      nxt_rt_m1_3(1)(12, 0),
      "b0".U(1.W)
    )) |
      (Fill(53, fp_fmt_i(1)) & Cat(nxt_rt_m1_0(1)(52, 28), "b0".U(1.W), nxt_rt_m1_1(1)(24, 0), "b0".U(2.W))) |
      (Fill(53, fp_fmt_i(2)) & Cat(nxt_rt_m1_0(1)(52, 0)))
  if (S1_CSA_SPECULATIVE == 1) {
    nxt_f_r_s_0(1) :=
      (Fill(F64_REM_W, nxt_rt_dig_0(1)(4)) & nxt_f_r_s_spec_s1_0(4)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(3)) & nxt_f_r_s_spec_s1_0(3)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(2)) & nxt_f_r_s_spec_s1_0(2)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(1)) & nxt_f_r_s_spec_s1_0(1)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(0)) & nxt_f_r_s_spec_s1_0(0))
    nxt_f_r_s_1(1) :=
      (Fill(F32_REM_W, nxt_rt_dig_1(1)(4)) & nxt_f_r_s_spec_s1_1(4)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(3)) & nxt_f_r_s_spec_s1_1(3)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(2)) & nxt_f_r_s_spec_s1_1(2)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(1)) & nxt_f_r_s_spec_s1_1(1)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(0)) & nxt_f_r_s_spec_s1_1(0))
    nxt_f_r_s_2(1) :=
      (Fill(F16_REM_W, nxt_rt_dig_2(1)(4)) & nxt_f_r_s_spec_s1_2(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(3)) & nxt_f_r_s_spec_s1_2(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(2)) & nxt_f_r_s_spec_s1_2(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(1)) & nxt_f_r_s_spec_s1_2(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(0)) & nxt_f_r_s_spec_s1_2(0))
    nxt_f_r_s_3(1) :=
      (Fill(F16_REM_W, nxt_rt_dig_3(1)(4)) & nxt_f_r_s_spec_s1_3(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(3)) & nxt_f_r_s_spec_s1_3(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(2)) & nxt_f_r_s_spec_s1_3(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(1)) & nxt_f_r_s_spec_s1_3(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(0)) & nxt_f_r_s_spec_s1_3(0))
    nxt_f_r_c_0(1) :=
      (Fill(F64_REM_W, nxt_rt_dig_0(1)(4)) & nxt_f_r_c_spec_s1_0(4)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(3)) & nxt_f_r_c_spec_s1_0(3)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(2)) & nxt_f_r_c_spec_s1_0(2)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(1)) & nxt_f_r_c_spec_s1_0(1)) |
        (Fill(F64_REM_W, nxt_rt_dig_0(1)(0)) & nxt_f_r_c_spec_s1_0(0))
    nxt_f_r_c_1(1) :=
      (Fill(F32_REM_W, nxt_rt_dig_1(1)(4)) & nxt_f_r_c_spec_s1_1(4)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(3)) & nxt_f_r_c_spec_s1_1(3)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(2)) & nxt_f_r_c_spec_s1_1(2)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(1)) & nxt_f_r_c_spec_s1_1(1)) |
        (Fill(F32_REM_W, nxt_rt_dig_1(1)(0)) & nxt_f_r_c_spec_s1_1(0))
    nxt_f_r_c_2(1) :=
      (Fill(F16_REM_W, nxt_rt_dig_2(1)(4)) & nxt_f_r_c_spec_s1_2(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(3)) & nxt_f_r_c_spec_s1_2(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(2)) & nxt_f_r_c_spec_s1_2(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(1)) & nxt_f_r_c_spec_s1_2(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_2(1)(0)) & nxt_f_r_c_spec_s1_2(0))
    nxt_f_r_c_3(1) :=
      (Fill(F16_REM_W, nxt_rt_dig_3(1)(4)) & nxt_f_r_c_spec_s1_3(4)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(3)) & nxt_f_r_c_spec_s1_3(3)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(2)) & nxt_f_r_c_spec_s1_3(2)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(1)) & nxt_f_r_c_spec_s1_3(1)) |
        (Fill(F16_REM_W, nxt_rt_dig_3(1)(0)) & nxt_f_r_c_spec_s1_3(0))
  }
  else if (S1_CSA_MERGED == 0) {
    nxt_f_r_s_0(1) :=
      f_r_s_for_csa_0(1) ^
        f_r_c_for_csa_0(1) ^
        sqrt_csa_val_0(1)
    nxt_f_r_c_pre_0(1) := Cat(
      (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_0(1)((F64_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_0(1)((F64_REM_W - 1) - 1, 0) & sqrt_csa_val_0(1)((F64_REM_W - 1) - 1, 0)),
      nxt_rt_dig_0(1)(0) | nxt_rt_dig_0(1)(1)
    )
    nxt_f_r_c_0(1) := Cat(
      nxt_f_r_c_pre_0(1)(55, 41),
      Mux(fp_fmt_i(0), (nxt_rt_dig_0(1)(0) | nxt_rt_dig_0(1)(1)), nxt_f_r_c_pre_0(1)(40)),
      nxt_f_r_c_pre_0(1)(39, 29),
      Mux(fp_fmt_i(1), (nxt_rt_dig_0(1)(0) | nxt_rt_dig_0(1)(1)), nxt_f_r_c_pre_0(1)(28)),
      nxt_f_r_c_pre_0(1)(27, 0)
    )
    nxt_f_r_s_1(1) :=
      f_r_s_for_csa_1(1) ^
        f_r_c_for_csa_1(1) ^
        sqrt_csa_val_1(1)
    nxt_f_r_c_pre_1(1) := Cat(
      (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_1(1)((F32_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_1(1)((F32_REM_W - 1) - 1, 0) & sqrt_csa_val_1(1)((F32_REM_W - 1) - 1, 0)),
      nxt_rt_dig_1(1)(0) | nxt_rt_dig_1(1)(1)
    )
    nxt_f_r_c_1(1) := Cat(
      nxt_f_r_c_pre_1(1)(27, 13),
      Mux(fp_fmt_i(0), (nxt_rt_dig_1(1)(0) | nxt_rt_dig_1(1)(1)), nxt_f_r_c_pre_1(1)(12)),
      nxt_f_r_c_pre_1(1)(11, 0)
    )
    nxt_f_r_s_2(1) :=
      f_r_s_for_csa_2(1) ^
        f_r_c_for_csa_2(1) ^
        sqrt_csa_val_2(1)
    nxt_f_r_c_pre_2(1) := Cat(
      (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_2(1)((F16_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_2(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_2(1)((F16_REM_W - 1) - 1, 0)),
      nxt_rt_dig_2(1)(0) | nxt_rt_dig_2(1)(1)
    )
    nxt_f_r_c_2(1) := nxt_f_r_c_pre_2(1)
    nxt_f_r_s_3(1) :=
      f_r_s_for_csa_3(1) ^
        f_r_c_for_csa_3(1) ^
        sqrt_csa_val_3(1)
    nxt_f_r_c_pre_3(1) := Cat(
      (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0)) |
        (f_r_s_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_3(1)((F16_REM_W - 1) - 1, 0)) |
        (f_r_c_for_csa_3(1)((F16_REM_W - 1) - 1, 0) & sqrt_csa_val_3(1)((F16_REM_W - 1) - 1, 0)),
      nxt_rt_dig_3(1)(0) | nxt_rt_dig_3(1)(1)
    )
    nxt_f_r_c_3(1) := nxt_f_r_c_pre_3(1)
  }
  if (S0_CSA_IS_MERGED == 1) {
    nxt_f_r_s_o :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        nxt_f_r_s_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1),
        "b0".U(2.W),
        nxt_f_r_s_2(1),
        "b0".U(2.W),
        nxt_f_r_s_1(1)(F32_REM_W - 1, F32_REM_W - 1 - F16_REM_W + 1),
        "b0".U(2.W),
        nxt_f_r_s_3(1)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(nxt_f_r_s_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1), "b0".U(6.W), "b0".U(2.W), nxt_f_r_s_1(1), "b0".U(6.W))) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(nxt_f_r_s_0(1), "b0".U(14.W)))
    nxt_f_r_c_o :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        nxt_f_r_c_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1),
        "b0".U(2.W),
        nxt_f_r_c_2(1),
        "b0".U(2.W),
        nxt_f_r_c_1(1)(F32_REM_W - 1, F32_REM_W - 1 - F16_REM_W + 1),
        "b0".U(2.W),
        nxt_f_r_c_3(1)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(nxt_f_r_c_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1), "b0".U(6.W), "b0".U(2.W), nxt_f_r_c_1(1), "b0".U(6.W))) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(nxt_f_r_c_0(1), "b0".U(14.W)))
  }
  else {
    nxt_f_r_s_o :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        nxt_f_r_s_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1),
        nxt_f_r_s_2(1),
        nxt_f_r_s_1(1)(F32_REM_W - 1, F32_REM_W - 1 - F16_REM_W + 1),
        nxt_f_r_s_3(1)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(nxt_f_r_s_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1), "b0".U(4.W), nxt_f_r_s_1(1), "b0".U(4.W))) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(nxt_f_r_s_0(1), "b0".U(8.W)))
    nxt_f_r_c_o :=
      (Fill(REM_W, fp_fmt_i(0)) & Cat(
        nxt_f_r_c_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F16_REM_W + 1),
        nxt_f_r_c_2(1),
        nxt_f_r_c_1(1)(F32_REM_W - 1, F32_REM_W - 1 - F16_REM_W + 1),
        nxt_f_r_c_3(1)
      )) |
        (Fill(REM_W, fp_fmt_i(1)) & Cat(nxt_f_r_c_0(1)(F64_REM_W - 1, F64_REM_W - 1 - F32_REM_W + 1), "b0".U(4.W), nxt_f_r_c_1(1), "b0".U(4.W))) |
        (Fill(REM_W, fp_fmt_i(2)) & Cat(nxt_f_r_c_0(1), "b0".U(8.W)))
  }

}