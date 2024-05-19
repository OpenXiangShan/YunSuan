package yunsuan.fpulite.fqrt

import chisel3._
import chisel3.util._
import yunsuan.vector.vfsqrt._

class fpsqrt_r16(
                  S0_CSA_SPECULATIVE: Int = 1,
                  S0_CSA_MERGED: Int = 1,
                  S1_QDS_SPECULATIVE: Int = 1,
                  S1_CSA_SPECULATIVE: Int = 1,
                  S1_CSA_MERGED: Int = 0
                ) extends Module {

  val start_valid_i = IO(Input(Bool()))
  val start_ready_o = IO(Output(Bool()))
  val flush_i = IO(Input(Bool()))
  val fp_format_i = IO(Input(UInt(2.W)))
  val op_i = IO(Input(UInt(64.W)))
  val rm_i = IO(Input(UInt(3.W)))
  val vector_mode_i = IO(Input(Bool()))
  val finish_valid_o = IO(Output(Bool()))
  val finish_ready_i = IO(Input(Bool()))
  val fpsqrt_res_o = IO(Output(UInt(64.W)))
  val fflags_o = IO(Output(UInt(5.W)))
  val fp_aIsFpCanonicalNAN = IO(Input(Bool()))

  val F64_REM_W = 2 + 54
  val F32_REM_W = 2 + 26
  val F16_REM_W = 2 + 14
  val F64_FULL_RT_W = F64_REM_W - 1
  val F32_FULL_RT_W = F32_REM_W - 1
  val F16_FULL_RT_W = F16_REM_W - 1
  val S0_CSA_IS_MERGED = if ((S0_CSA_SPECULATIVE == 0) & (S0_CSA_MERGED == 1)) 1 else 0
  val S1_CSA_IS_MERGED = if ((S1_CSA_SPECULATIVE == 0) & (S1_CSA_MERGED == 1)) 1 else 0
  val REM_W = if (S0_CSA_IS_MERGED == 1) ((2 + F16_REM_W) * 3 + F16_REM_W) else (4 * F16_REM_W)
  val F64_FRAC_W = 52 + 1
  val F32_FRAC_W = 23 + 1
  val F16_FRAC_W = 10 + 1
  val F64_EXP_W = 11
  val F32_EXP_W = 8
  val F16_EXP_W = 5
  val F64_ITER_NUM = 13
  val F32_ITER_NUM = 6
  val F16_ITER_NUM = 3
  val F64_RT_W = 4 * F64_ITER_NUM + 2
  val F32_RT_W = 4 * F32_ITER_NUM + 2
  val F16_RT_W = 4 * F16_ITER_NUM + 2
  val ITER_NUM_W = 4
  val FSM_W = 4
  val FSM_PRE_0 = (1 << 0).U
  val FSM_PRE_1 = (1 << 1).U
  val FSM_ITER = (1 << 2).U
  val FSM_POST_0 = (1 << 3).U
  val FSM_PRE_0_BIT = 0
  val FSM_PRE_1_BIT = 1
  val FSM_ITER_BIT = 2
  val FSM_POST_0_BIT = 3
  val RT_DIG_W = 5
  val RT_DIG_NEG_2_BIT = 4
  val RT_DIG_NEG_1_BIT = 3
  val RT_DIG_NEG_0_BIT = 2
  val RT_DIG_POS_1_BIT = 1
  val RT_DIG_POS_2_BIT = 0
  val RT_DIG_NEG_2 = (1 << 4)
  val RT_DIG_NEG_1 = (1 << 3)
  val RT_DIG_NEG_0 = (1 << 2)
  val RT_DIG_POS_1 = (1 << 1)
  val RT_DIG_POS_2 = (1 << 0)
  val SQRT_2_WITH_ROUND_BIT = "b1_01101010000010011110011001100111111100111011110011001".U((F64_FRAC_W + 1).W)
  val RM_RNE = "b000".U(3.W)
  val RM_RTZ = "b001".U(3.W)
  val RM_RDN = "b010".U(3.W)
  val RM_RUP = "b011".U(3.W)
  val RM_RMM = "b100".U(3.W)
  val start_handshaked = Wire(Bool())
  val fsm_d = Wire(UInt(FSM_W.W))
  val fsm_q = RegInit(UInt(FSM_W.W), FSM_PRE_0)
  val iter_num_en = Wire(Bool())
  val iter_num_d = Wire(UInt(4.W))
  val iter_num_q = Reg(UInt(4.W))
  val final_iter = Wire(Bool())
  val fp_fmt_d = Wire(UInt(3.W))
  val fp_fmt_q = Reg(UInt(3.W))
  val rm_d = Wire(UInt(3.W))
  val rm_q = Reg(UInt(3.W))
  val v_mode_d = Wire(Bool())
  val v_mode_q = Reg(Bool())
  val out_sign_0_d = Wire(Bool())
  val out_sign_0_q = Reg(Bool())
  val out_sign_1_d = Wire(Bool())
  val out_sign_1_q = Reg(Bool())
  val out_sign_2_d = Wire(Bool())
  val out_sign_2_q = Reg(Bool())
  val out_sign_3_d = Wire(Bool())
  val out_sign_3_q = Reg(Bool())
  val out_exp_0_d = Wire(UInt(F64_EXP_W.W))
  val out_exp_0_q = Reg(UInt(F64_EXP_W.W))
  val out_exp_1_d = Wire(UInt(F32_EXP_W.W))
  val out_exp_1_q = Reg(UInt(F32_EXP_W.W))
  val out_exp_2_d = Wire(UInt(F16_EXP_W.W))
  val out_exp_2_q = Reg(UInt(F16_EXP_W.W))
  val out_exp_3_d = Wire(UInt(F16_EXP_W.W))
  val out_exp_3_q = Reg(UInt(F16_EXP_W.W))
  val out_exp_pre_0 = Wire(UInt(((F64_EXP_W + 1)).W))
  val out_exp_pre_1 = Wire(UInt(((F32_EXP_W + 1)).W))
  val out_exp_pre_2 = Wire(UInt(((F16_EXP_W + 1)).W))
  val out_exp_pre_3 = Wire(UInt(((F16_EXP_W + 1)).W))
  val op_sign_0 = Wire(Bool())
  val op_sign_1 = Wire(Bool())
  val op_sign_2 = Wire(Bool())
  val op_sign_3 = Wire(Bool())
  val op_exp_0 = Wire(UInt(F64_EXP_W.W))
  val op_exp_1 = Wire(UInt(F32_EXP_W.W))
  val op_exp_2 = Wire(UInt(F16_EXP_W.W))
  val op_exp_3 = Wire(UInt(F16_EXP_W.W))
  val op_exp_is_zero_0 = Wire(Bool())
  val op_exp_is_zero_1 = Wire(Bool())
  val op_exp_is_zero_2 = Wire(Bool())
  val op_exp_is_zero_3 = Wire(Bool())
  val op_exp_is_max_0 = Wire(Bool())
  val op_exp_is_max_1 = Wire(Bool())
  val op_exp_is_max_2 = Wire(Bool())
  val op_exp_is_max_3 = Wire(Bool())
  val op_is_zero_0 = Wire(Bool())
  val op_is_zero_1 = Wire(Bool())
  val op_is_zero_2 = Wire(Bool())
  val op_is_zero_3 = Wire(Bool())
  val op_is_inf_0 = Wire(Bool())
  val op_is_inf_1 = Wire(Bool())
  val op_is_inf_2 = Wire(Bool())
  val op_is_inf_3 = Wire(Bool())
  val op_is_qnan_0 = Wire(Bool())
  val op_is_qnan_1 = Wire(Bool())
  val op_is_qnan_2 = Wire(Bool())
  val op_is_qnan_3 = Wire(Bool())
  val op_is_snan_0 = Wire(Bool())
  val op_is_snan_1 = Wire(Bool())
  val op_is_snan_2 = Wire(Bool())
  val op_is_snan_3 = Wire(Bool())
  val op_is_nan_0 = Wire(Bool())
  val op_is_nan_1 = Wire(Bool())
  val op_is_nan_2 = Wire(Bool())
  val op_is_nan_3 = Wire(Bool())
  val res_is_nan_0_d = Wire(Bool())
  val res_is_nan_0_q = Reg(Bool())
  val res_is_nan_1_d = Wire(Bool())
  val res_is_nan_1_q = Reg(Bool())
  val res_is_nan_2_d = Wire(Bool())
  val res_is_nan_2_q = Reg(Bool())
  val res_is_nan_3_d = Wire(Bool())
  val res_is_nan_3_q = Reg(Bool())
  val res_is_inf_0_d = Wire(Bool())
  val res_is_inf_0_q = Reg(Bool())
  val res_is_inf_1_d = Wire(Bool())
  val res_is_inf_1_q = Reg(Bool())
  val res_is_inf_2_d = Wire(Bool())
  val res_is_inf_2_q = Reg(Bool())
  val res_is_inf_3_d = Wire(Bool())
  val res_is_inf_3_q = Reg(Bool())
  val res_is_exact_zero_0_d = Wire(Bool())
  val res_is_exact_zero_0_q = Reg(Bool())
  val res_is_exact_zero_1_d = Wire(Bool())
  val res_is_exact_zero_1_q = Reg(Bool())
  val res_is_exact_zero_2_d = Wire(Bool())
  val res_is_exact_zero_2_q = Reg(Bool())
  val res_is_exact_zero_3_d = Wire(Bool())
  val res_is_exact_zero_3_q = Reg(Bool())
  val op_invalid_0_d = Wire(Bool())
  val op_invalid_0_q = Reg(Bool())
  val op_invalid_1_d = Wire(Bool())
  val op_invalid_1_q = Reg(Bool())
  val op_invalid_2_d = Wire(Bool())
  val op_invalid_2_q = Reg(Bool())
  val op_invalid_3_d = Wire(Bool())
  val op_invalid_3_q = Reg(Bool())
  val res_is_sqrt_2_d = Wire(Bool())
  val res_is_sqrt_2_q = Reg(Bool())
  val early_finish = Wire(Bool())
  val need_2_cycles_init = Wire(Bool())
  val op_l_shift_num_pre_0 = Wire(UInt(log2Ceil(F64_FRAC_W).W))
  val op_l_shift_num_pre_1 = Wire(UInt(log2Ceil(F32_FRAC_W).W))
  val op_l_shift_num_pre_2 = Wire(UInt(log2Ceil(F16_FRAC_W).W))
  val op_l_shift_num_pre_3 = Wire(UInt(log2Ceil(F16_FRAC_W).W))
  val op_l_shift_num_0 = Wire(UInt(log2Ceil(F64_FRAC_W).W))
  val op_l_shift_num_1 = Wire(UInt(log2Ceil(F32_FRAC_W).W))
  val op_l_shift_num_2 = Wire(UInt(log2Ceil(F16_FRAC_W).W))
  val op_l_shift_num_3 = Wire(UInt(log2Ceil(F16_FRAC_W).W))
  val op_frac_pre_shifted_0 = Wire(UInt(F64_FRAC_W.W))
  val op_frac_pre_shifted_1 = Wire(UInt(F32_FRAC_W.W))
  val op_frac_pre_shifted_2 = Wire(UInt(F16_FRAC_W.W))
  val op_frac_pre_shifted_3 = Wire(UInt(F16_FRAC_W.W))
  val op_frac_l_shifted_s5_to_s2 = Wire(UInt(((F64_FRAC_W - 1)).W))
  val op_frac_l_shifted_1 = Wire(UInt(((F32_FRAC_W - 1)).W))
  val op_frac_l_shifted_2 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val op_frac_l_shifted_3 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val op_frac_l_shifted_0 = Wire(UInt(((F64_FRAC_W - 1)).W))
  val op_frac_is_zero_0 = Wire(Bool())
  val op_frac_is_zero_1 = Wire(Bool())
  val op_frac_is_zero_2 = Wire(Bool())
  val op_frac_is_zero_3 = Wire(Bool())
  val rt_1st_0 = Wire(Vec(3, Bool()))
  val rt_1st_1 = Wire(Vec(3, Bool()))
  val rt_1st_2 = Wire(Vec(3, Bool()))
  val rt_1st_3 = Wire(Vec(3, Bool()))
  val rt_en = Wire(Bool())
  val rt_d = Wire(UInt(56.W))
  val rt_q = Reg(UInt(56.W))
  val rt_m1_en = Wire(Bool())
  val rt_m1_d = Wire(UInt(53.W))
  val rt_m1_q = Reg(UInt(53.W))
  val rt_iter_init = Wire(UInt(56.W))
  val rt_m1_iter_init = Wire(UInt(53.W))
  val nxt_rt = Wire(UInt(56.W))
  val nxt_rt_m1 = Wire(UInt(53.W))
  val rt_iter_init_0 = Wire(UInt(F64_FULL_RT_W.W))
  val rt_iter_init_1 = Wire(UInt(F32_FULL_RT_W.W))
  val rt_iter_init_2 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_iter_init_3 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_m1_iter_init_0 = Wire(UInt(F64_FULL_RT_W.W))
  val rt_m1_iter_init_1 = Wire(UInt(F32_FULL_RT_W.W))
  val rt_m1_iter_init_2 = Wire(UInt(F16_FULL_RT_W.W))
  val rt_m1_iter_init_3 = Wire(UInt(F16_FULL_RT_W.W))
  val exp_is_odd_pre_0_0 = Wire(Bool())
  val exp_is_odd_pre_0_1 = Wire(Bool())
  val exp_is_odd_pre_0_2 = Wire(Bool())
  val exp_is_odd_pre_0_3 = Wire(Bool())
  val current_exp_is_odd_0 = Wire(Bool())
  val current_exp_is_odd_1 = Wire(Bool())
  val current_exp_is_odd_2 = Wire(Bool())
  val current_exp_is_odd_3 = Wire(Bool())
  val current_frac_0 = Wire(UInt(((F64_FRAC_W - 1)).W))
  val current_frac_1 = Wire(UInt(((F32_FRAC_W - 1)).W))
  val current_frac_2 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val current_frac_3 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val mask_en = Wire(Bool())
  val mask_d = Wire(UInt(13.W))
  val mask_q = Reg(UInt(13.W))
  val f_r_s_iter_init_pre_0 = Wire(UInt(F64_REM_W.W))
  val f_r_s_iter_init_pre_1 = Wire(UInt(F32_REM_W.W))
  val f_r_s_iter_init_pre_2 = Wire(UInt(F16_REM_W.W))
  val f_r_s_iter_init_pre_3 = Wire(UInt(F16_REM_W.W))
  val f_r_s_iter_init_0 = Wire(UInt(F64_REM_W.W))
  val f_r_s_iter_init_1 = Wire(UInt(F32_REM_W.W))
  val f_r_s_iter_init_2 = Wire(UInt(F16_REM_W.W))
  val f_r_s_iter_init_3 = Wire(UInt(F16_REM_W.W))
  val f_r_c_iter_init_0 = Wire(UInt(F64_REM_W.W))
  val f_r_c_iter_init_1 = Wire(UInt(F32_REM_W.W))
  val f_r_c_iter_init_2 = Wire(UInt(F16_REM_W.W))
  val f_r_c_iter_init_3 = Wire(UInt(F16_REM_W.W))
  val f_r_s_en = Wire(Bool())
  val f_r_s_d = Wire(UInt(REM_W.W))
  val f_r_s_q = Reg(UInt(REM_W.W))
  val f_r_c_en = Wire(Bool())
  val f_r_c_d = Wire(UInt(REM_W.W))
  val f_r_c_q = Reg(UInt(REM_W.W))

  val f_r_s_iter_init_69_54 = Wire(UInt(16.W))
  val f_r_s_iter_init_53_52 = Wire(UInt(2.W))
  val f_r_s_iter_init_51_36 = Wire(UInt(16.W))
  val f_r_s_iter_init_35_34 = Wire(UInt(2.W))
  val f_r_s_iter_init_33_18 = Wire(UInt(16.W))
  val f_r_s_iter_init_17_16 = Wire(UInt(2.W))
  val f_r_s_iter_init_15_0 = Wire(UInt(16.W))
  f_r_s_iter_init_69_54 := 0.U
  f_r_s_iter_init_53_52 := 0.U
  f_r_s_iter_init_51_36 := 0.U
  f_r_s_iter_init_35_34 := 0.U
  f_r_s_iter_init_33_18 := 0.U
  f_r_s_iter_init_17_16 := 0.U
  f_r_s_iter_init_15_0 := 0.U
  val f_r_s_iter_init = Wire(UInt(REM_W.W))
  f_r_s_iter_init := Cat(f_r_s_iter_init_69_54, f_r_s_iter_init_53_52, f_r_s_iter_init_51_36, f_r_s_iter_init_35_34, f_r_s_iter_init_33_18, f_r_s_iter_init_17_16, f_r_s_iter_init_15_0)

  val f_r_c_iter_init_69_54 = Wire(UInt(16.W))
  val f_r_c_iter_init_53_52 = Wire(UInt(2.W))
  val f_r_c_iter_init_51_36 = Wire(UInt(16.W))
  val f_r_c_iter_init_35_34 = Wire(UInt(2.W))
  val f_r_c_iter_init_33_18 = Wire(UInt(16.W))
  val f_r_c_iter_init_17_16 = Wire(UInt(2.W))
  val f_r_c_iter_init_15_0 = Wire(UInt(16.W))
  f_r_c_iter_init_69_54 := 0.U
  f_r_c_iter_init_53_52 := 0.U
  f_r_c_iter_init_51_36 := 0.U
  f_r_c_iter_init_35_34 := 0.U
  f_r_c_iter_init_33_18 := 0.U
  f_r_c_iter_init_17_16 := 0.U
  f_r_c_iter_init_15_0 := 0.U
  val f_r_c_iter_init = Wire(UInt(REM_W.W))
  f_r_c_iter_init := Cat(f_r_c_iter_init_69_54, f_r_c_iter_init_53_52, f_r_c_iter_init_51_36, f_r_c_iter_init_35_34, f_r_c_iter_init_33_18, f_r_c_iter_init_17_16, f_r_c_iter_init_15_0)
  val nxt_f_r_s = Wire(UInt(REM_W.W))
  val nxt_f_r_c = Wire(UInt(REM_W.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_0_en = Wire(Bool())
  val nr_f_r_7b_for_nxt_cycle_s0_qds_0_d = Wire(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_0_q = Reg(UInt(7.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_0_en = Wire(Bool())
  val nr_f_r_9b_for_nxt_cycle_s1_qds_0_d = Wire(UInt(9.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_0_q = Reg(UInt(9.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_1_en = Wire(Bool())
  val nr_f_r_7b_for_nxt_cycle_s0_qds_1_d = Wire(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_1_q = Reg(UInt(7.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_1_en = Wire(Bool())
  val nr_f_r_9b_for_nxt_cycle_s1_qds_1_d = Wire(UInt(9.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_1_q = Reg(UInt(9.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_2_en = Wire(Bool())
  val nr_f_r_7b_for_nxt_cycle_s0_qds_2_d = Wire(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_2_q = Reg(UInt(7.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_2_en = Wire(Bool())
  val nr_f_r_9b_for_nxt_cycle_s1_qds_2_d = Wire(UInt(9.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_2_q = Reg(UInt(9.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_3_en = Wire(Bool())
  val nr_f_r_7b_for_nxt_cycle_s0_qds_3_d = Wire(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s0_qds_3_q = Reg(UInt(7.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_3_en = Wire(Bool())
  val nr_f_r_9b_for_nxt_cycle_s1_qds_3_d = Wire(UInt(9.W))
  val nr_f_r_9b_for_nxt_cycle_s1_qds_3_q = Reg(UInt(9.W))
  val adder_8b_iter_init_0 = Wire(UInt(8.W))
  val adder_8b_iter_init_1 = Wire(UInt(8.W))
  val adder_8b_iter_init_2 = Wire(UInt(8.W))
  val adder_8b_iter_init_3 = Wire(UInt(8.W))
  val adder_9b_iter_init_0 = Wire(UInt(9.W))
  val adder_9b_iter_init_1 = Wire(UInt(9.W))
  val adder_9b_iter_init_2 = Wire(UInt(9.W))
  val adder_9b_iter_init_3 = Wire(UInt(9.W))
  val a0_iter_init_0 = Wire(Bool())
  val a0_iter_init_1 = Wire(Bool())
  val a0_iter_init_2 = Wire(Bool())
  val a0_iter_init_3 = Wire(Bool())
  val a2_iter_init_0 = Wire(Bool())
  val a2_iter_init_1 = Wire(Bool())
  val a2_iter_init_2 = Wire(Bool())
  val a2_iter_init_3 = Wire(Bool())
  val a3_iter_init_0 = Wire(Bool())
  val a3_iter_init_1 = Wire(Bool())
  val a3_iter_init_2 = Wire(Bool())
  val a3_iter_init_3 = Wire(Bool())
  val a4_iter_init_0 = Wire(Bool())
  val a4_iter_init_1 = Wire(Bool())
  val a4_iter_init_2 = Wire(Bool())
  val a4_iter_init_3 = Wire(Bool())
  val m_neg_1_iter_init_0 = Wire(UInt(7.W))
  val m_neg_1_iter_init_1 = Wire(UInt(7.W))
  val m_neg_1_iter_init_2 = Wire(UInt(7.W))
  val m_neg_1_iter_init_3 = Wire(UInt(7.W))
  val m_neg_0_iter_init_0 = Wire(UInt(7.W))
  val m_neg_0_iter_init_1 = Wire(UInt(7.W))
  val m_neg_0_iter_init_2 = Wire(UInt(7.W))
  val m_neg_0_iter_init_3 = Wire(UInt(7.W))
  val m_pos_1_iter_init_0 = Wire(UInt(7.W))
  val m_pos_1_iter_init_1 = Wire(UInt(7.W))
  val m_pos_1_iter_init_2 = Wire(UInt(7.W))
  val m_pos_1_iter_init_3 = Wire(UInt(7.W))
  val m_pos_2_iter_init_0 = Wire(UInt(7.W))
  val m_pos_2_iter_init_1 = Wire(UInt(7.W))
  val m_pos_2_iter_init_2 = Wire(UInt(7.W))
  val m_pos_2_iter_init_3 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s0_qds_0 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s0_qds_1 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s0_qds_2 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s0_qds_3 = Wire(UInt(7.W))
  val adder_9b_res_for_nxt_cycle_s1_qds_0 = Wire(UInt(9.W))
  val adder_9b_res_for_nxt_cycle_s1_qds_1 = Wire(UInt(9.W))
  val adder_9b_res_for_nxt_cycle_s1_qds_2 = Wire(UInt(9.W))
  val adder_9b_res_for_nxt_cycle_s1_qds_3 = Wire(UInt(9.W))
  val m_neg_1_for_nxt_cycle_s0_qds_0_en = Wire(Bool())
  val m_neg_1_for_nxt_cycle_s0_qds_0_d = Wire(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_0_q = Reg(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_1_en = Wire(Bool())
  val m_neg_1_for_nxt_cycle_s0_qds_1_d = Wire(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_1_q = Reg(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_2_en = Wire(Bool())
  val m_neg_1_for_nxt_cycle_s0_qds_2_d = Wire(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_2_q = Reg(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_3_en = Wire(Bool())
  val m_neg_1_for_nxt_cycle_s0_qds_3_d = Wire(UInt(5.W))
  val m_neg_1_for_nxt_cycle_s0_qds_3_q = Reg(UInt(5.W))
  val m_neg_0_for_nxt_cycle_s0_qds_0_en = Wire(Bool())
  val m_neg_0_for_nxt_cycle_s0_qds_0_d = Wire(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_0_q = Reg(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_1_en = Wire(Bool())
  val m_neg_0_for_nxt_cycle_s0_qds_1_d = Wire(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_1_q = Reg(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_2_en = Wire(Bool())
  val m_neg_0_for_nxt_cycle_s0_qds_2_d = Wire(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_2_q = Reg(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_3_en = Wire(Bool())
  val m_neg_0_for_nxt_cycle_s0_qds_3_d = Wire(UInt(4.W))
  val m_neg_0_for_nxt_cycle_s0_qds_3_q = Reg(UInt(4.W))
  val m_pos_1_for_nxt_cycle_s0_qds_0_en = Wire(Bool())
  val m_pos_1_for_nxt_cycle_s0_qds_0_d = Wire(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_0_q = Reg(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_1_en = Wire(Bool())
  val m_pos_1_for_nxt_cycle_s0_qds_1_d = Wire(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_1_q = Reg(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_2_en = Wire(Bool())
  val m_pos_1_for_nxt_cycle_s0_qds_2_d = Wire(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_2_q = Reg(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_3_en = Wire(Bool())
  val m_pos_1_for_nxt_cycle_s0_qds_3_d = Wire(UInt(3.W))
  val m_pos_1_for_nxt_cycle_s0_qds_3_q = Reg(UInt(3.W))
  val m_pos_2_for_nxt_cycle_s0_qds_0_en = Wire(Bool())
  val m_pos_2_for_nxt_cycle_s0_qds_0_d = Wire(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_0_q = Reg(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_1_en = Wire(Bool())
  val m_pos_2_for_nxt_cycle_s0_qds_1_d = Wire(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_1_q = Reg(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_2_en = Wire(Bool())
  val m_pos_2_for_nxt_cycle_s0_qds_2_d = Wire(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_2_q = Reg(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_3_en = Wire(Bool())
  val m_pos_2_for_nxt_cycle_s0_qds_3_d = Wire(UInt(4.W))
  val m_pos_2_for_nxt_cycle_s0_qds_3_q = Reg(UInt(4.W))
  val m_neg_1_to_nxt_cycle_0 = Wire(UInt(7.W))
  val m_neg_1_to_nxt_cycle_1 = Wire(UInt(7.W))
  val m_neg_1_to_nxt_cycle_2 = Wire(UInt(7.W))
  val m_neg_1_to_nxt_cycle_3 = Wire(UInt(7.W))
  val m_neg_0_to_nxt_cycle_0 = Wire(UInt(7.W))
  val m_neg_0_to_nxt_cycle_1 = Wire(UInt(7.W))
  val m_neg_0_to_nxt_cycle_2 = Wire(UInt(7.W))
  val m_neg_0_to_nxt_cycle_3 = Wire(UInt(7.W))
  val m_pos_1_to_nxt_cycle_0 = Wire(UInt(7.W))
  val m_pos_1_to_nxt_cycle_1 = Wire(UInt(7.W))
  val m_pos_1_to_nxt_cycle_2 = Wire(UInt(7.W))
  val m_pos_1_to_nxt_cycle_3 = Wire(UInt(7.W))
  val m_pos_2_to_nxt_cycle_0 = Wire(UInt(7.W))
  val m_pos_2_to_nxt_cycle_1 = Wire(UInt(7.W))
  val m_pos_2_to_nxt_cycle_2 = Wire(UInt(7.W))
  val m_pos_2_to_nxt_cycle_3 = Wire(UInt(7.W))
  val nr_f_r_merged = Wire(UInt(REM_W.W))
  val nr_f_r = Wire(UInt(((((F16_REM_W + 1) * 3) + F16_REM_W)).W))
  val nr_f_r_adder_in = Wire(Vec(2, UInt(((((F16_REM_W + 1) * 3) + F16_REM_W)).W)))
  val f_r_xor = Wire(UInt(((REM_W - 2)).W))
  val f_r_or = Wire(UInt(((REM_W - 2)).W))
  val rem_is_not_zero_0 = Wire(Bool())
  val rem_is_not_zero_1 = Wire(Bool())
  val rem_is_not_zero_2 = Wire(Bool())
  val rem_is_not_zero_3 = Wire(Bool())
  val select_rt_m1_0 = Wire(Bool())
  val select_rt_m1_1 = Wire(Bool())
  val select_rt_m1_2 = Wire(Bool())
  val select_rt_m1_3 = Wire(Bool())
  val f64_res_is_sqrt_2 = Wire(Bool())
  val f32_res_is_sqrt_2 = Wire(Bool())
  val f16_res_is_sqrt_2 = Wire(Bool())
  val rt_for_inc = Wire(UInt(56.W))
  val rt_pre_inc = Wire(UInt(((F64_FRAC_W - 1)).W))
  val rt_inc_lane = Wire(UInt(((F64_FRAC_W - 1)).W))
  val rt_m1_pre_inc_0 = Wire(UInt(((F64_FRAC_W - 1)).W))
  val rt_m1_pre_inc_1 = Wire(UInt(((F32_FRAC_W - 1)).W))
  val rt_m1_pre_inc_2 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val rt_m1_pre_inc_3 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val rt_inc_res = Wire(UInt(F64_FRAC_W.W))
  val rt_m1_inc_res_0 = Wire(UInt(F64_FRAC_W.W))
  val rt_m1_inc_res_1 = Wire(UInt(F32_FRAC_W.W))
  val rt_m1_inc_res_2 = Wire(UInt(F16_FRAC_W.W))
  val rt_m1_inc_res_3 = Wire(UInt(F16_FRAC_W.W))
  val guard_bit_rt_0 = Wire(Bool())
  val guard_bit_rt_1 = Wire(Bool())
  val guard_bit_rt_2 = Wire(Bool())
  val guard_bit_rt_3 = Wire(Bool())
  val round_bit_rt_0 = Wire(Bool())
  val round_bit_rt_1 = Wire(Bool())
  val round_bit_rt_2 = Wire(Bool())
  val round_bit_rt_3 = Wire(Bool())
  val sticky_bit_rt_0 = Wire(Bool())
  val sticky_bit_rt_1 = Wire(Bool())
  val sticky_bit_rt_2 = Wire(Bool())
  val sticky_bit_rt_3 = Wire(Bool())
  val rt_need_rup_0 = Wire(Bool())
  val rt_need_rup_1 = Wire(Bool())
  val rt_need_rup_2 = Wire(Bool())
  val rt_need_rup_3 = Wire(Bool())
  val inexact_rt_0 = Wire(Bool())
  val inexact_rt_1 = Wire(Bool())
  val inexact_rt_2 = Wire(Bool())
  val inexact_rt_3 = Wire(Bool())
  val inexact_0 = Wire(Bool())
  val inexact_1 = Wire(Bool())
  val inexact_2 = Wire(Bool())
  val inexact_3 = Wire(Bool())
  val guard_bit_rt_m1_0 = Wire(Bool())
  val guard_bit_rt_m1_1 = Wire(Bool())
  val guard_bit_rt_m1_2 = Wire(Bool())
  val guard_bit_rt_m1_3 = Wire(Bool())
  val round_bit_rt_m1_0 = Wire(Bool())
  val round_bit_rt_m1_1 = Wire(Bool())
  val round_bit_rt_m1_2 = Wire(Bool())
  val round_bit_rt_m1_3 = Wire(Bool())
  val rt_m1_need_rup_0 = Wire(Bool())
  val rt_m1_need_rup_1 = Wire(Bool())
  val rt_m1_need_rup_2 = Wire(Bool())
  val rt_m1_need_rup_3 = Wire(Bool())
  val rt_rounded_0 = Wire(UInt(F64_FRAC_W.W))
  val rt_rounded_1 = Wire(UInt(F32_FRAC_W.W))
  val rt_rounded_2 = Wire(UInt(F16_FRAC_W.W))
  val rt_rounded_3 = Wire(UInt(F16_FRAC_W.W))
  val rt_m1_rounded_0 = Wire(UInt(F64_FRAC_W.W))
  val rt_m1_rounded_1 = Wire(UInt(F32_FRAC_W.W))
  val rt_m1_rounded_2 = Wire(UInt(F16_FRAC_W.W))
  val rt_m1_rounded_3 = Wire(UInt(F16_FRAC_W.W))
  val carry_after_round_0 = Wire(Bool())
  val carry_after_round_1 = Wire(Bool())
  val carry_after_round_2 = Wire(Bool())
  val carry_after_round_3 = Wire(Bool())
  val frac_rounded_0 = Wire(UInt(F64_FRAC_W.W))
  val frac_rounded_1 = Wire(UInt(F32_FRAC_W.W))
  val frac_rounded_2 = Wire(UInt(F16_FRAC_W.W))
  val frac_rounded_3 = Wire(UInt(F16_FRAC_W.W))
  val exp_rounded_0 = Wire(UInt(F64_EXP_W.W))
  val exp_rounded_1 = Wire(UInt(F32_EXP_W.W))
  val exp_rounded_2 = Wire(UInt(F16_EXP_W.W))
  val exp_rounded_3 = Wire(UInt(F16_EXP_W.W))
  val f16_res_0 = Wire(UInt(((F16_EXP_W + F16_FRAC_W)).W))
  val f16_res_1 = Wire(UInt(((F16_EXP_W + F16_FRAC_W)).W))
  val f16_res_2 = Wire(UInt(((F16_EXP_W + F16_FRAC_W)).W))
  val f16_res_3 = Wire(UInt(((F16_EXP_W + F16_FRAC_W)).W))
  val f32_res_0 = Wire(UInt(((F32_EXP_W + F32_FRAC_W)).W))
  val f32_res_1 = Wire(UInt(((F32_EXP_W + F32_FRAC_W)).W))
  val f64_res_0 = Wire(UInt(((F64_EXP_W + F64_FRAC_W)).W))
  val f16_exp_res_0 = Wire(UInt(F16_EXP_W.W))
  val f16_exp_res_1 = Wire(UInt(F16_EXP_W.W))
  val f16_exp_res_2 = Wire(UInt(F16_EXP_W.W))
  val f16_exp_res_3 = Wire(UInt(F16_EXP_W.W))
  val f32_exp_res_0 = Wire(UInt(F32_EXP_W.W))
  val f32_exp_res_1 = Wire(UInt(F32_EXP_W.W))
  val f64_exp_res_0 = Wire(UInt(F64_EXP_W.W))
  val f16_frac_res_0 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val f16_frac_res_1 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val f16_frac_res_2 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val f16_frac_res_3 = Wire(UInt(((F16_FRAC_W - 1)).W))
  val f32_frac_res_0 = Wire(UInt(((F32_FRAC_W - 1)).W))
  val f32_frac_res_1 = Wire(UInt(((F32_FRAC_W - 1)).W))
  val f64_frac_res_0 = Wire(UInt(((F64_FRAC_W - 1)).W))
  val fflags_invalid_operation_0 = Wire(Bool())
  val fflags_invalid_operation_1 = Wire(Bool())
  val fflags_invalid_operation_2 = Wire(Bool())
  val fflags_invalid_operation_3 = Wire(Bool())
  val f16_fflags_invalid_operation = Wire(Bool())
  val f32_fflags_invalid_operation = Wire(Bool())
  val f64_fflags_invalid_operation = Wire(Bool())
  val fflags_div_by_zero_0 = Wire(Bool())
  val fflags_div_by_zero_1 = Wire(Bool())
  val fflags_div_by_zero_2 = Wire(Bool())
  val fflags_div_by_zero_3 = Wire(Bool())
  val f16_fflags_div_by_zero = Wire(Bool())
  val f32_fflags_div_by_zero = Wire(Bool())
  val f64_fflags_div_by_zero = Wire(Bool())
  val fflags_overflow_0 = Wire(Bool())
  val fflags_overflow_1 = Wire(Bool())
  val fflags_overflow_2 = Wire(Bool())
  val fflags_overflow_3 = Wire(Bool())
  val f16_fflags_overflow = Wire(Bool())
  val f32_fflags_overflow = Wire(Bool())
  val f64_fflags_overflow = Wire(Bool())
  val fflags_underflow_0 = Wire(Bool())
  val fflags_underflow_1 = Wire(Bool())
  val fflags_underflow_2 = Wire(Bool())
  val fflags_underflow_3 = Wire(Bool())
  val f16_fflags_underflow = Wire(Bool())
  val f32_fflags_underflow = Wire(Bool())
  val f64_fflags_underflow = Wire(Bool())
  val fflags_inexact_0 = Wire(Bool())
  val fflags_inexact_1 = Wire(Bool())
  val fflags_inexact_2 = Wire(Bool())
  val fflags_inexact_3 = Wire(Bool())
  val f16_fflags_inexact = Wire(Bool())
  val f32_fflags_inexact = Wire(Bool())
  val f64_fflags_inexact = Wire(Bool())
  start_ready_o := fsm_q(FSM_PRE_0_BIT)
  start_handshaked := start_valid_i & start_ready_o
  finish_valid_o := fsm_q(FSM_POST_0_BIT)
  op_sign_0 := op_i(63)
  op_sign_1 := op_i(31)
  op_sign_2 := op_i(47)
  op_sign_3 := op_i(15)
  op_exp_0 :=
    (Fill(F64_EXP_W, fp_format_i === 0.U(2.W)) & Cat(Fill(F64_EXP_W - F16_EXP_W, "b0".U(1.W)), op_i(62, 62 - F16_EXP_W + 1))) |
      (Fill(F64_EXP_W, fp_format_i === 1.U(2.W)) & Cat(Fill(F64_EXP_W - F32_EXP_W, "b0".U(1.W)), op_i(62, 62 - F32_EXP_W + 1))) |
      (Fill(F64_EXP_W, fp_format_i === 2.U(2.W)) & Cat(op_i(62, 62 - F64_EXP_W + 1)))
  op_exp_1 :=
    (Fill(F32_EXP_W, fp_format_i === 0.U(2.W)) & Cat(Fill(F32_EXP_W - F16_EXP_W, "b0".U(1.W)), op_i(30, 30 - F16_EXP_W + 1))) |
      (Fill(F32_EXP_W, fp_format_i === 1.U(2.W)) & Cat(op_i(30, 30 - F32_EXP_W + 1)))
  op_exp_2 := op_i(46, 46 - F16_EXP_W + 1)
  op_exp_3 := op_i(14, 14 - F16_EXP_W + 1)
  op_exp_is_zero_0 := (op_exp_0 === 0.U)
  op_exp_is_zero_1 := (op_exp_1 === 0.U)
  op_exp_is_zero_2 := (op_exp_2 === 0.U)
  op_exp_is_zero_3 := (op_exp_3 === 0.U)
  op_exp_is_max_0 := (op_exp_0 === (Mux((fp_format_i === 0.U(2.W)), 31.U(11.W), Mux((fp_format_i === 1.U(2.W)), 255.U(11.W), 2047.U(11.W)))))
  op_exp_is_max_1 := (op_exp_1 === (Mux((fp_format_i === 0.U(2.W)), 31.U(8.W), 255.U(8.W))))
  op_exp_is_max_2 := (op_exp_2 === 31.U(5.W))
  op_exp_is_max_3 := (op_exp_3 === 31.U(5.W))
  op_is_zero_0 := !fp_aIsFpCanonicalNAN & op_exp_is_zero_0 & op_frac_is_zero_0
  op_is_zero_1 := !fp_aIsFpCanonicalNAN & op_exp_is_zero_1 & op_frac_is_zero_1
  op_is_zero_2 := !fp_aIsFpCanonicalNAN & op_exp_is_zero_2 & op_frac_is_zero_2
  op_is_zero_3 := !fp_aIsFpCanonicalNAN & op_exp_is_zero_3 & op_frac_is_zero_3
  op_is_inf_0 := !fp_aIsFpCanonicalNAN & op_exp_is_max_0 & op_frac_is_zero_0
  op_is_inf_1 := !fp_aIsFpCanonicalNAN & op_exp_is_max_1 & op_frac_is_zero_1
  op_is_inf_2 := !fp_aIsFpCanonicalNAN & op_exp_is_max_2 & op_frac_is_zero_2
  op_is_inf_3 := !fp_aIsFpCanonicalNAN & op_exp_is_max_3 & op_frac_is_zero_3
  op_is_qnan_0 := fp_aIsFpCanonicalNAN | op_exp_is_max_0 & (Mux((fp_format_i === 0.U(2.W)), op_i(57), Mux((fp_format_i === 1.U(2.W)), op_i(54), op_i(51))))
  op_is_qnan_1 := fp_aIsFpCanonicalNAN | op_exp_is_max_1 & (Mux((fp_format_i === 0.U(2.W)), op_i(25), op_i(22)))
  op_is_qnan_2 := fp_aIsFpCanonicalNAN | op_exp_is_max_2 & op_i(41)
  op_is_qnan_3 := fp_aIsFpCanonicalNAN | op_exp_is_max_3 & op_i(9)
  op_is_snan_0 := !fp_aIsFpCanonicalNAN & op_exp_is_max_0 & ~op_frac_is_zero_0 & (Mux((fp_format_i === 0.U(2.W)), ~op_i(57), Mux((fp_format_i === 1.U(2.W)), ~op_i(54), ~op_i(51))))
  op_is_snan_1 := !fp_aIsFpCanonicalNAN & op_exp_is_max_1 & ~op_frac_is_zero_1 & (Mux((fp_format_i === 0.U(2.W)), ~op_i(25), ~op_i(22)))
  op_is_snan_2 := !fp_aIsFpCanonicalNAN & op_exp_is_max_2 & ~op_frac_is_zero_2 & ~op_i(41)
  op_is_snan_3 := !fp_aIsFpCanonicalNAN & op_exp_is_max_3 & ~op_frac_is_zero_3 & ~op_i(9)
  op_is_nan_0 := (op_is_qnan_0 | op_is_snan_0)
  op_is_nan_1 := (op_is_qnan_1 | op_is_snan_1)
  op_is_nan_2 := (op_is_qnan_2 | op_is_snan_2)
  op_is_nan_3 := (op_is_qnan_3 | op_is_snan_3)
  op_invalid_0_d := (op_sign_0 & ~op_is_zero_0 & ~op_is_qnan_0) | op_is_snan_0
  op_invalid_1_d := (op_sign_1 & ~op_is_zero_1 & ~op_is_qnan_1) | op_is_snan_1
  op_invalid_2_d := (op_sign_2 & ~op_is_zero_2 & ~op_is_qnan_2) | op_is_snan_2
  op_invalid_3_d := (op_sign_3 & ~op_is_zero_3 & ~op_is_qnan_3) | op_is_snan_3
  res_is_nan_0_d := op_is_nan_0 | op_invalid_0_d
  res_is_nan_1_d := op_is_nan_1 | op_invalid_1_d
  res_is_nan_2_d := op_is_nan_2 | op_invalid_2_d
  res_is_nan_3_d := op_is_nan_3 | op_invalid_3_d
  res_is_inf_0_d := op_is_inf_0
  res_is_inf_1_d := op_is_inf_1
  res_is_inf_2_d := op_is_inf_2
  res_is_inf_3_d := op_is_inf_3
  res_is_exact_zero_0_d := op_is_zero_0
  res_is_exact_zero_1_d := op_is_zero_1
  res_is_exact_zero_2_d := op_is_zero_2
  res_is_exact_zero_3_d := op_is_zero_3
  out_sign_0_d := Mux(res_is_nan_0_d, "b0".U(1.W), op_sign_0)
  out_sign_1_d := Mux(res_is_nan_1_d, "b0".U(1.W), op_sign_1)
  out_sign_2_d := Mux(res_is_nan_2_d, "b0".U(1.W), op_sign_2)
  out_sign_3_d := Mux(res_is_nan_3_d, "b0".U(1.W), op_sign_3)
  out_exp_0_d := out_exp_pre_0(F64_EXP_W, 1)
  out_exp_1_d := out_exp_pre_1(F32_EXP_W, 1)
  out_exp_2_d := out_exp_pre_2(F16_EXP_W, 1)
  out_exp_3_d := out_exp_pre_3(F16_EXP_W, 1)
  fp_fmt_d := Cat(fp_format_i === 2.U(2.W), fp_format_i === 1.U(2.W), fp_format_i === 0.U(2.W))
  rm_d := rm_i
  v_mode_d := vector_mode_i
  res_is_sqrt_2_d := ~vector_mode_i & (
    (Fill(1, fp_format_i === 0.U(2.W)) & op_frac_is_zero_3 & ~op_exp_3(0)) |
      (Fill(1, fp_format_i === 1.U(2.W)) & op_frac_is_zero_1 & ~op_exp_1(0)) |
      (Fill(1, fp_format_i === 2.U(2.W)) & op_frac_is_zero_0 & ~op_exp_0(0))
    )
  early_finish := Mux(vector_mode_i, false.B,
    Mux(fp_format_i === 2.U(2.W), res_is_nan_0_d | res_is_inf_0_d | res_is_exact_zero_0_d | res_is_sqrt_2_d,
      Mux(fp_format_i === 0.U(2.W), res_is_nan_3_d | res_is_inf_3_d | res_is_exact_zero_3_d | res_is_sqrt_2_d,
        res_is_nan_1_d | res_is_inf_1_d | res_is_exact_zero_1_d | res_is_sqrt_2_d
      )
    )
  )
  need_2_cycles_init :=
    Mux((fp_format_i === 2.U(2.W)), op_exp_is_zero_0,
      (vector_mode_i | (Mux((fp_format_i === 0.U(2.W)), op_exp_is_zero_3, op_exp_is_zero_1))))
  op_frac_pre_shifted_0 :=
    (Fill(F64_FRAC_W, fp_format_i === 0.U(2.W)) & Cat("b0".U(1.W), op_i(48 + F16_FRAC_W - 1 - 1, 48), Fill(F64_FRAC_W - F16_FRAC_W, "b0".U(1.W)))) |
      (Fill(F64_FRAC_W, fp_format_i === 1.U(2.W)) & Cat("b0".U(1.W), op_i(32 + F32_FRAC_W - 1 - 1, 32), Fill(F64_FRAC_W - F32_FRAC_W, "b0".U(1.W)))) |
      (Fill(F64_FRAC_W, fp_format_i === 2.U(2.W)) & Cat("b0".U(1.W), op_i(0 + F64_FRAC_W - 1 - 1, 0)))
  val u_lzc_0 = Module(new lzc(
    WIDTH = F64_FRAC_W,
    MODE = 1.U))
  u_lzc_0.in_i <> op_frac_pre_shifted_0
  u_lzc_0.cnt_o <> op_l_shift_num_pre_0
  u_lzc_0.empty_o <> op_frac_is_zero_0

  op_l_shift_num_0 := Fill(log2Ceil(F64_FRAC_W), op_exp_is_zero_0) & op_l_shift_num_pre_0
  op_frac_l_shifted_s5_to_s2 := op_frac_pre_shifted_0(0 + F64_FRAC_W - 1 - 1, 0) << Cat(op_l_shift_num_0(5, 2), "b0".U(2.W))
  op_frac_l_shifted_0 := rt_m1_q(0 + F64_FRAC_W - 1 - 1, 0) << iter_num_q(1, 0)
  op_frac_pre_shifted_1 :=
    (Fill(F32_FRAC_W, fp_format_i === 0.U(2.W)) & Cat("b0".U(1.W), op_i(16 + F16_FRAC_W - 1 - 1, 16), Fill(F32_FRAC_W - F16_FRAC_W, "b0".U(1.W)))) |
      (Fill(F32_FRAC_W, fp_format_i === 1.U(2.W)) & Cat("b0".U(1.W), op_i(0 + F32_FRAC_W - 1 - 1, 0)))
  val u_lzc_1 = Module(new lzc(
    WIDTH = F32_FRAC_W,
    MODE = 1.U))
  u_lzc_1.in_i <> op_frac_pre_shifted_1
  u_lzc_1.cnt_o <> op_l_shift_num_pre_1
  u_lzc_1.empty_o <> op_frac_is_zero_1

  op_l_shift_num_1 := Fill(log2Ceil(F32_FRAC_W), op_exp_is_zero_1) & op_l_shift_num_pre_1
  op_frac_l_shifted_1 := op_frac_pre_shifted_1(0 + F32_FRAC_W - 1 - 1, 0) << op_l_shift_num_1
  op_frac_pre_shifted_2 := Cat("b0".U(1.W), op_i(32 + F16_FRAC_W - 1 - 1, 32))
  val u_lzc_2 = Module(new lzc(
    WIDTH = F16_FRAC_W,
    MODE = 1.U))
  u_lzc_2.in_i <> op_frac_pre_shifted_2
  u_lzc_2.cnt_o <> op_l_shift_num_pre_2
  u_lzc_2.empty_o <> op_frac_is_zero_2

  op_l_shift_num_2 := Fill(log2Ceil(F16_FRAC_W), op_exp_is_zero_2) & op_l_shift_num_pre_2
  op_frac_l_shifted_2 := op_frac_pre_shifted_2(0 + F16_FRAC_W - 1 - 1, 0) << op_l_shift_num_2
  op_frac_pre_shifted_3 := Cat("b0".U(1.W), op_i(0 + F16_FRAC_W - 1 - 1, 0))
  val u_lzc_3 = Module(new lzc(
    WIDTH = F16_FRAC_W,
    MODE = 1.U))
  u_lzc_3.in_i <> op_frac_pre_shifted_3
  u_lzc_3.cnt_o <> op_l_shift_num_pre_3
  u_lzc_3.empty_o <> op_frac_is_zero_3

  op_l_shift_num_3 := Fill(log2Ceil(F16_FRAC_W), op_exp_is_zero_3) & op_l_shift_num_pre_3
  op_frac_l_shifted_3 := op_frac_pre_shifted_3(0 + F16_FRAC_W - 1 - 1, 0) << op_l_shift_num_3
  out_exp_pre_0 := Cat("b0".U(1.W), op_exp_0(10, 1), op_exp_0(0) | op_exp_is_zero_0) + Cat(
    "b0".U(2.W),
    (Fill(6, fp_format_i === 0.U(2.W)) & "b0".U(6.W)) |
      (Fill(6, fp_format_i === 1.U(2.W)) & Cat("b0".U(3.W), "b11".U(2.W), ~op_l_shift_num_0(4))) |
      (Fill(6, fp_format_i === 2.U(2.W)) & Cat("b1111".U(4.W), ~op_l_shift_num_0(5, 4))),
    ~op_l_shift_num_0(3, 0)
  )
  out_exp_pre_1 := Cat("b0".U(1.W), op_exp_1(7, 1), op_exp_1(0) | op_exp_is_zero_1) + Cat(
    "b0".U(2.W),
    (Fill(3, fp_format_i === 0.U(2.W)) & "b0".U(3.W)) |
      (Fill(3, fp_format_i === 1.U(2.W)) & Cat("b11".U(2.W), ~op_l_shift_num_1(4))),
    ~op_l_shift_num_1(3, 0)
  )
  out_exp_pre_2 := Cat("b0".U(1.W), op_exp_2(4, 1), op_exp_2(0) | op_exp_is_zero_2) + Cat("b0".U(2.W), ~op_l_shift_num_2(3, 0))
  out_exp_pre_3 := Cat("b0".U(1.W), op_exp_3(4, 1), op_exp_3(0) | op_exp_is_zero_3) + Cat("b0".U(2.W), ~op_l_shift_num_3(3, 0))
  exp_is_odd_pre_0_0 := Mux(op_exp_is_zero_0, op_l_shift_num_0(0), ~op_exp_0(0))
  current_exp_is_odd_0 := Mux(fsm_q(FSM_PRE_0_BIT), exp_is_odd_pre_0_0, mask_q(0))
  current_frac_0 := Mux(fsm_q(FSM_PRE_0_BIT), op_frac_pre_shifted_0(0 + F64_FRAC_W - 1 - 1, 0), op_frac_l_shifted_0(0 + F64_FRAC_W - 1 - 1, 0))
  rt_1st_0(0) := (Cat(current_exp_is_odd_0, current_frac_0(F64_FRAC_W - 2)) === "b00".U(2.W))
  rt_1st_0(1) := (Cat(current_exp_is_odd_0, current_frac_0(F64_FRAC_W - 2)) === "b01".U(2.W)) | (Cat(current_exp_is_odd_0, current_frac_0(F64_FRAC_W - 2)) === "b10".U(2.W))
  rt_1st_0(2) := (Cat(current_exp_is_odd_0, current_frac_0(F64_FRAC_W - 2)) === "b11".U(2.W))
  rt_iter_init_0 :=
    (Fill(F64_FULL_RT_W, rt_1st_0(0)) & Cat("b010".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F64_FULL_RT_W, rt_1st_0(1)) & Cat("b011".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F64_FULL_RT_W, rt_1st_0(2)) & Cat("b100".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W))))
  rt_m1_iter_init_0 :=
    (Fill(F64_FULL_RT_W, rt_1st_0(0)) & Cat("b001".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F64_FULL_RT_W, rt_1st_0(1)) & Cat("b010".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F64_FULL_RT_W, rt_1st_0(2)) & Cat("b011".U(3.W), Fill(F64_FULL_RT_W - 3, "b0".U(1.W))))
  f_r_s_iter_init_pre_0 := Cat("b11".U(2.W), Mux(current_exp_is_odd_0, Cat("b1".U(1.W), current_frac_0, "b0".U(1.W)), Cat("b0".U(1.W), "b1".U(1.W), current_frac_0)))
  f_r_s_iter_init_0 := Cat(f_r_s_iter_init_pre_0((F64_REM_W - 1) - 2, 0), "b0".U(2.W))
  f_r_c_iter_init_0 :=
    (Fill(F64_REM_W, rt_1st_0(0)) & Cat("b11".U(2.W), Fill(F64_REM_W - 2, "b0".U(1.W)))) |
      (Fill(F64_REM_W, rt_1st_0(1)) & Cat("b0111".U(4.W), Fill(F64_REM_W - 4, "b0".U(1.W)))) |
      (Fill(F64_REM_W, rt_1st_0(2)) & Fill(F64_REM_W, "b0".U(1.W)))
  exp_is_odd_pre_0_1 := Mux(op_exp_is_zero_1, op_l_shift_num_1(0), ~op_exp_1(0))
  current_exp_is_odd_1 := Mux(fsm_q(FSM_PRE_0_BIT), exp_is_odd_pre_0_1, mask_q(1))
  current_frac_1 := Mux(fsm_q(FSM_PRE_0_BIT), op_frac_pre_shifted_1(0 + F32_FRAC_W - 1 - 1, 0), rt_q(0 + F32_FRAC_W - 1 - 1, 0))
  rt_1st_1(0) := (Cat(current_exp_is_odd_1, current_frac_1(F32_FRAC_W - 2)) === "b00".U(2.W))
  rt_1st_1(1) := (Cat(current_exp_is_odd_1, current_frac_1(F32_FRAC_W - 2)) === "b01".U(2.W)) | (Cat(current_exp_is_odd_1, current_frac_1(F32_FRAC_W - 2)) === "b10".U(2.W))
  rt_1st_1(2) := (Cat(current_exp_is_odd_1, current_frac_1(F32_FRAC_W - 2)) === "b11".U(2.W))
  rt_iter_init_1 :=
    (Fill(F32_FULL_RT_W, rt_1st_1(0)) & Cat("b010".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F32_FULL_RT_W, rt_1st_1(1)) & Cat("b011".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F32_FULL_RT_W, rt_1st_1(2)) & Cat("b100".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W))))
  rt_m1_iter_init_1 :=
    (Fill(F32_FULL_RT_W, rt_1st_1(0)) & Cat("b001".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F32_FULL_RT_W, rt_1st_1(1)) & Cat("b010".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F32_FULL_RT_W, rt_1st_1(2)) & Cat("b011".U(3.W), Fill(F32_FULL_RT_W - 3, "b0".U(1.W))))
  f_r_s_iter_init_pre_1 := Cat("b11".U(2.W), Mux(current_exp_is_odd_1, Cat("b1".U(1.W), current_frac_1, "b0".U(2.W)), Cat("b0".U(1.W), "b1".U(1.W), current_frac_1, "b0".U(1.W))))
  f_r_s_iter_init_1 := Cat(f_r_s_iter_init_pre_1((F32_REM_W - 1) - 2, 0), "b0".U(2.W))
  f_r_c_iter_init_1 :=
    (Fill(F32_REM_W, rt_1st_1(0)) & Cat("b11".U(2.W), Fill(F32_REM_W - 2, "b0".U(1.W)))) |
      (Fill(F32_REM_W, rt_1st_1(1)) & Cat("b0111".U(4.W), Fill(F32_REM_W - 4, "b0".U(1.W)))) |
      (Fill(F32_REM_W, rt_1st_1(2)) & Fill(F32_REM_W, "b0".U(1.W)))
  exp_is_odd_pre_0_2 := Mux(op_exp_is_zero_2, op_l_shift_num_2(0), ~op_exp_2(0))
  current_exp_is_odd_2 := Mux(fsm_q(FSM_PRE_0_BIT), exp_is_odd_pre_0_2, mask_q(2))
  current_frac_2 := Mux(fsm_q(FSM_PRE_0_BIT), op_frac_pre_shifted_2(0 + F16_FRAC_W - 1 - 1, 0), rt_q(23 + F16_FRAC_W - 1 - 1, 23))
  rt_1st_2(0) := (Cat(current_exp_is_odd_2, current_frac_2(F16_FRAC_W - 2)) === "b00".U(2.W))
  rt_1st_2(1) := (Cat(current_exp_is_odd_2, current_frac_2(F16_FRAC_W - 2)) === "b01".U(2.W)) | (Cat(current_exp_is_odd_2, current_frac_2(F16_FRAC_W - 2)) === "b10".U(2.W))
  rt_1st_2(2) := (Cat(current_exp_is_odd_2, current_frac_2(F16_FRAC_W - 2)) === "b11".U(2.W))
  rt_iter_init_2 :=
    (Fill(F16_FULL_RT_W, rt_1st_2(0)) & Cat("b010".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_2(1)) & Cat("b011".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_2(2)) & Cat("b100".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W))))
  rt_m1_iter_init_2 :=
    (Fill(F16_FULL_RT_W, rt_1st_2(0)) & Cat("b001".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_2(1)) & Cat("b010".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_2(2)) & Cat("b011".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W))))
  f_r_s_iter_init_pre_2 := Cat("b11".U(2.W), Mux(current_exp_is_odd_2, Cat("b1".U(1.W), current_frac_2, "b0".U(3.W)), Cat("b0".U(1.W), "b1".U(1.W), current_frac_2, "b0".U(2.W))))
  f_r_s_iter_init_2 := Cat(f_r_s_iter_init_pre_2((F16_REM_W - 1) - 2, 0), "b0".U(2.W))
  f_r_c_iter_init_2 :=
    (Fill(F16_REM_W, rt_1st_2(0)) & Cat("b11".U(2.W), Fill(F16_REM_W - 2, "b0".U(1.W)))) |
      (Fill(F16_REM_W, rt_1st_2(1)) & Cat("b0111".U(4.W), Fill(F16_REM_W - 4, "b0".U(1.W)))) |
      (Fill(F16_REM_W, rt_1st_2(2)) & Fill(F16_REM_W, "b0".U(1.W)))
  exp_is_odd_pre_0_3 := Mux(op_exp_is_zero_3, op_l_shift_num_3(0), ~op_exp_3(0))
  current_exp_is_odd_3 := Mux(fsm_q(FSM_PRE_0_BIT), exp_is_odd_pre_0_3, mask_q(3))
  current_frac_3 := Mux(fsm_q(FSM_PRE_0_BIT), op_frac_pre_shifted_3(0 + F16_FRAC_W - 1 - 1, 0), rt_q(33 + F16_FRAC_W - 1 - 1, 33))
  rt_1st_3(0) := (Cat(current_exp_is_odd_3, current_frac_3(F16_FRAC_W - 2)) === "b00".U(2.W))
  rt_1st_3(1) := (Cat(current_exp_is_odd_3, current_frac_3(F16_FRAC_W - 2)) === "b01".U(2.W)) | (Cat(current_exp_is_odd_3, current_frac_3(F16_FRAC_W - 2)) === "b10".U(2.W))
  rt_1st_3(2) := (Cat(current_exp_is_odd_3, current_frac_3(F16_FRAC_W - 2)) === "b11".U(2.W))
  rt_iter_init_3 :=
    (Fill(F16_FULL_RT_W, rt_1st_3(0)) & Cat("b010".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_3(1)) & Cat("b011".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_3(2)) & Cat("b100".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W))))
  rt_m1_iter_init_3 :=
    (Fill(F16_FULL_RT_W, rt_1st_3(0)) & Cat("b001".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_3(1)) & Cat("b010".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W)))) |
      (Fill(F16_FULL_RT_W, rt_1st_3(2)) & Cat("b011".U(3.W), Fill(F16_FULL_RT_W - 3, "b0".U(1.W))))
  f_r_s_iter_init_pre_3 := Cat("b11".U(2.W), Mux(current_exp_is_odd_3, Cat("b1".U(1.W), current_frac_3, "b0".U(3.W)), Cat("b0".U(1.W), "b1".U(1.W), current_frac_3, "b0".U(2.W))))
  f_r_s_iter_init_3 := Cat(f_r_s_iter_init_pre_3((F16_REM_W - 1) - 2, 0), "b0".U(2.W))
  f_r_c_iter_init_3 :=
    (Fill(F16_REM_W, rt_1st_3(0)) & Cat("b11".U(2.W), Fill(F16_REM_W - 2, "b0".U(1.W)))) |
      (Fill(F16_REM_W, rt_1st_3(1)) & Cat("b0111".U(4.W), Fill(F16_REM_W - 4, "b0".U(1.W)))) |
      (Fill(F16_REM_W, rt_1st_3(2)) & Fill(F16_REM_W, "b0".U(1.W)))
  rt_iter_init := Cat(
    rt_iter_init_0(F64_FULL_RT_W - 2, F64_FULL_RT_W - 2 - 2 + 1),
    "b0".U(12.W),
    Fill(2, Mux(fsm_q(FSM_PRE_0_BIT), (fp_format_i === 0.U(2.W)), fp_fmt_q(0))) & rt_iter_init_2(F16_FULL_RT_W - 2, F16_FULL_RT_W - 2 - 2 + 1),
    "b0".U(12.W),
    Fill(2, Mux(fsm_q(FSM_PRE_0_BIT), ((fp_format_i === 0.U(2.W)) | (fp_format_i === 1.U(2.W))), (fp_fmt_q(0) | fp_fmt_q(1)))) & rt_iter_init_1(F32_FULL_RT_W - 2, F32_FULL_RT_W - 2 - 2 + 1),
    "b0".U(12.W),
    Fill(2, Mux(fsm_q(FSM_PRE_0_BIT), (fp_format_i === 0.U(2.W)), fp_fmt_q(0))) & rt_iter_init_3(F16_FULL_RT_W - 2, F16_FULL_RT_W - 2 - 2 + 1),
    "b0".U(12.W)
  )
  rt_d := Mux(
    fsm_q(FSM_PRE_0_BIT), (Mux(need_2_cycles_init, Cat(
      rt_q(55, 43),
      op_frac_l_shifted_3(0 + F16_FRAC_W - 1 - 1, 0),
      op_frac_l_shifted_2(0 + F16_FRAC_W - 1 - 1, 0),
      op_frac_l_shifted_1(0 + F32_FRAC_W - 1 - 1, 0)
    ), rt_iter_init)), Mux(
      fsm_q(FSM_PRE_1_BIT), rt_iter_init,
      nxt_rt))
  rt_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  rt_m1_iter_init := Cat(
    rt_m1_iter_init_0(F64_FULL_RT_W - 3),
    "b0".U(12.W),
    Fill(1, Mux(fsm_q(FSM_PRE_0_BIT), (fp_format_i === 0.U(2.W)), fp_fmt_q(0))) & rt_m1_iter_init_2(F16_FULL_RT_W - 3),
    "b0".U(12.W),
    Fill(1, Mux(fsm_q(FSM_PRE_0_BIT), ((fp_format_i === 0.U(2.W)) | (fp_format_i === 1.U(2.W))), (fp_fmt_q(0) | fp_fmt_q(1)))) & rt_m1_iter_init_1(F32_FULL_RT_W - 3),
    "b0".U(12.W),
    Fill(1, Mux(fsm_q(FSM_PRE_0_BIT), (fp_format_i === 0.U(2.W)), fp_fmt_q(0))) & rt_m1_iter_init_3(F16_FULL_RT_W - 3),
    "b0".U(12.W),
    "b0".U(1.W)
  )
  rt_m1_d := Mux(
    fsm_q(FSM_PRE_0_BIT), (Mux(need_2_cycles_init, Cat(rt_m1_q(52), op_frac_l_shifted_s5_to_s2), rt_m1_iter_init)), Mux(
      fsm_q(FSM_PRE_1_BIT), rt_m1_iter_init,
      nxt_rt_m1))
  rt_m1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  mask_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  mask_d := Mux(
    fsm_q(FSM_PRE_0_BIT), (Mux(need_2_cycles_init, Cat(
      mask_q(12, 4),
      exp_is_odd_pre_0_3,
      exp_is_odd_pre_0_2,
      exp_is_odd_pre_0_1,
      exp_is_odd_pre_0_0
    ), Cat("b1".U(1.W), "b0".U(12.W)))), Mux(
      fsm_q(FSM_PRE_1_BIT), Cat("b1".U(1.W), "b0".U(12.W)),
      (mask_q >> 1)))
  if (S0_CSA_IS_MERGED == 1) {
    f_r_s_iter_init_69_54 := f_r_s_iter_init_0(55, 40)
    f_r_s_iter_init_53_52 := f_r_s_iter_init_0(39, 38)
    f_r_s_iter_init_51_36 := Mux((fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0)), f_r_s_iter_init_2(15, 0), f_r_s_iter_init_0(37, 22))
    f_r_s_iter_init_35_34 := f_r_s_iter_init_0(21, 20)
    f_r_s_iter_init_33_18 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_s_iter_init_0(19, 4)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_s_iter_init_1(27, 12))
    f_r_s_iter_init_17_16 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_s_iter_init_0(3, 2)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_s_iter_init_1(11, 10))
    f_r_s_iter_init_15_0 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 0.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0))) & f_r_s_iter_init_3(15, 0)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 1.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(1))) & Cat(f_r_s_iter_init_1(9, 0), "b0".U(6.W))) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & Cat(f_r_s_iter_init_0(1, 0), "b0".U(14.W)))
    f_r_c_iter_init_69_54 := f_r_c_iter_init_0(55, 40)
    f_r_c_iter_init_53_52 := f_r_c_iter_init_0(39, 38)
    f_r_c_iter_init_51_36 := Mux((fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0)), f_r_c_iter_init_2(15, 0), f_r_c_iter_init_0(37, 22))
    f_r_c_iter_init_35_34 := f_r_c_iter_init_0(21, 20)
    f_r_c_iter_init_33_18 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_c_iter_init_0(19, 4)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_c_iter_init_1(27, 12))
    f_r_c_iter_init_17_16 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_c_iter_init_0(3, 2)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_c_iter_init_1(11, 10))
    f_r_c_iter_init_15_0 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 0.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0))) & f_r_c_iter_init_3(15, 0)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 1.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(1))) & Cat(f_r_c_iter_init_1(9, 0), "b0".U(6.W))) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & Cat(f_r_c_iter_init_0(1, 0), "b0".U(14.W)))
  }
  else {
    val f_r_s_iter_init_63_48 = f_r_s_iter_init_0(55, 40)
    val f_r_s_iter_init_47_32 = Mux((fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0)), f_r_s_iter_init_2(15, 0), f_r_s_iter_init_0(39, 24))
    val f_r_s_iter_init_31_16 =
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_s_iter_init_0(23, 8)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_s_iter_init_1(27, 12))
    f_r_s_iter_init_15_0 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 0.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0))) & f_r_s_iter_init_3(15, 0)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 1.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(1))) & Cat(f_r_s_iter_init_1(11, 0), "b0".U(4.W))) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & Cat(f_r_s_iter_init_0(7, 0), "b0".U(8.W)))
    val f_r_c_iter_init_63_48 = f_r_c_iter_init_0(55, 40)
    val f_r_c_iter_init_47_32 = Mux((fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0)), f_r_c_iter_init_2(15, 0), f_r_c_iter_init_0(39, 24))
    val f_r_c_iter_init_31_16 =
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & f_r_c_iter_init_0(23, 8)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i =/= 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & ~fp_fmt_q(2))) & f_r_c_iter_init_1(27, 12))
    f_r_c_iter_init_15_0 :=
      (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 0.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(0))) & f_r_c_iter_init_3(15, 0)) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 1.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(1))) & Cat(f_r_c_iter_init_1(11, 0), "b0".U(4.W))) |
        (Fill(16, (fsm_q(FSM_PRE_0_BIT) & (fp_format_i === 2.U(2.W))) | (fsm_q(FSM_PRE_1_BIT) & fp_fmt_q(2))) & Cat(f_r_c_iter_init_0(7, 0), "b0".U(8.W)))
    f_r_c_iter_init := Cat(f_r_c_iter_init_63_48, f_r_c_iter_init_47_32, f_r_c_iter_init_31_16, f_r_c_iter_init_15_0)
    f_r_s_iter_init := Cat(f_r_s_iter_init_63_48, f_r_s_iter_init_47_32, f_r_s_iter_init_31_16, f_r_s_iter_init_15_0)
  }

  f_r_s_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  f_r_s_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), f_r_s_iter_init, nxt_f_r_s)
  f_r_c_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  f_r_c_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), f_r_c_iter_init, nxt_f_r_c)
  iter_num_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | (fsm_q(FSM_ITER_BIT) & ~final_iter)
  iter_num_d := Mux(
    fsm_q(FSM_PRE_0_BIT), (Mux(need_2_cycles_init, Cat(iter_num_q(3, 2), op_l_shift_num_0(1, 0)), Cat(
      (Fill(4, fp_format_i === 0.U(2.W)) & 2.U(4.W)) |
        (Fill(4, fp_format_i === 1.U(2.W)) & 5.U(4.W)) |
        (Fill(4, fp_format_i === 2.U(2.W)) & 12.U(4.W))
    ))), Mux(
      fsm_q(FSM_PRE_1_BIT), (
        (Fill(4, fp_fmt_q(0)) & 2.U(4.W)) |
          (Fill(4, fp_fmt_q(1)) & 5.U(4.W)) |
          (Fill(4, fp_fmt_q(2)) & 12.U(4.W))
        ),
      (iter_num_q - 1.U(4.W))))
  final_iter := (iter_num_q === 0.U(4.W))
  adder_8b_iter_init_0 := Cat(f_r_s_iter_init_0(F64_REM_W - 1, F64_REM_W - 1 - 4 + 1) + f_r_c_iter_init_0(F64_REM_W - 1, F64_REM_W - 1 - 4 + 1), f_r_s_iter_init_0(F64_REM_W - 1 - 4, F64_REM_W - 1 - 4 - 4 + 1))
  nr_f_r_7b_for_nxt_cycle_s0_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_7b_for_nxt_cycle_s0_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_8b_iter_init_0(7, 1), adder_7b_res_for_nxt_cycle_s0_qds_0)
  adder_8b_iter_init_1 := Cat(f_r_s_iter_init_1(F32_REM_W - 1, F32_REM_W - 1 - 4 + 1) + f_r_c_iter_init_1(F32_REM_W - 1, F32_REM_W - 1 - 4 + 1), f_r_s_iter_init_1(F32_REM_W - 1 - 4, F32_REM_W - 1 - 4 - 4 + 1))
  nr_f_r_7b_for_nxt_cycle_s0_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_7b_for_nxt_cycle_s0_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_8b_iter_init_1(7, 1), adder_7b_res_for_nxt_cycle_s0_qds_1)
  adder_8b_iter_init_2 := Cat(f_r_s_iter_init_2(F16_REM_W - 1, F16_REM_W - 1 - 4 + 1) + f_r_c_iter_init_2(F16_REM_W - 1, F16_REM_W - 1 - 4 + 1), f_r_s_iter_init_2(F16_REM_W - 1 - 4, F16_REM_W - 1 - 4 - 4 + 1))
  nr_f_r_7b_for_nxt_cycle_s0_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_7b_for_nxt_cycle_s0_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_8b_iter_init_2(7, 1), adder_7b_res_for_nxt_cycle_s0_qds_2)
  adder_8b_iter_init_3 := Cat(f_r_s_iter_init_3(F16_REM_W - 1, F16_REM_W - 1 - 4 + 1) + f_r_c_iter_init_3(F16_REM_W - 1, F16_REM_W - 1 - 4 + 1), f_r_s_iter_init_3(F16_REM_W - 1 - 4, F16_REM_W - 1 - 4 - 4 + 1))
  nr_f_r_7b_for_nxt_cycle_s0_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_7b_for_nxt_cycle_s0_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_8b_iter_init_3(7, 1), adder_7b_res_for_nxt_cycle_s0_qds_3)
  adder_9b_iter_init_0 := Cat(f_r_s_iter_init_0(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 2 + 1) + f_r_c_iter_init_0(F64_REM_W - 1 - 2, F64_REM_W - 1 - 2 - 2 + 1), f_r_s_iter_init_0(F64_REM_W - 1 - 2 - 2, F64_REM_W - 1 - 2 - 2 - 7 + 1))
  nr_f_r_9b_for_nxt_cycle_s1_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_9b_for_nxt_cycle_s1_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_9b_iter_init_0, adder_9b_res_for_nxt_cycle_s1_qds_0)
  adder_9b_iter_init_1 := Cat(f_r_s_iter_init_1(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 2 + 1) + f_r_c_iter_init_1(F32_REM_W - 1 - 2, F32_REM_W - 1 - 2 - 2 + 1), f_r_s_iter_init_1(F32_REM_W - 1 - 2 - 2, F32_REM_W - 1 - 2 - 2 - 7 + 1))
  nr_f_r_9b_for_nxt_cycle_s1_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_9b_for_nxt_cycle_s1_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_9b_iter_init_1, adder_9b_res_for_nxt_cycle_s1_qds_1)
  adder_9b_iter_init_2 := Cat(f_r_s_iter_init_2(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 2 + 1) + f_r_c_iter_init_2(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 2 + 1), f_r_s_iter_init_2(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 7 + 1))
  nr_f_r_9b_for_nxt_cycle_s1_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_9b_for_nxt_cycle_s1_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_9b_iter_init_2, adder_9b_res_for_nxt_cycle_s1_qds_2)
  adder_9b_iter_init_3 := Cat(f_r_s_iter_init_3(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 2 + 1) + f_r_c_iter_init_3(F16_REM_W - 1 - 2, F16_REM_W - 1 - 2 - 2 + 1), f_r_s_iter_init_3(F16_REM_W - 1 - 2 - 2, F16_REM_W - 1 - 2 - 2 - 7 + 1))
  nr_f_r_9b_for_nxt_cycle_s1_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  nr_f_r_9b_for_nxt_cycle_s1_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), adder_9b_iter_init_3, adder_9b_res_for_nxt_cycle_s1_qds_3)
  a0_iter_init_0 := rt_iter_init_0(F64_FULL_RT_W - 1)
  a2_iter_init_0 := rt_iter_init_0(F64_FULL_RT_W - 3)
  a3_iter_init_0 := rt_iter_init_0(F64_FULL_RT_W - 4)
  a4_iter_init_0 := rt_iter_init_0(F64_FULL_RT_W - 5)
  val u_r4_qds_cg_iter_init_0 = Module(new r4_qds_cg())
  u_r4_qds_cg_iter_init_0.a0_i <> a0_iter_init_0
  u_r4_qds_cg_iter_init_0.a2_i <> a2_iter_init_0
  u_r4_qds_cg_iter_init_0.a3_i <> a3_iter_init_0
  u_r4_qds_cg_iter_init_0.a4_i <> a4_iter_init_0
  u_r4_qds_cg_iter_init_0.m_neg_1_o <> m_neg_1_iter_init_0
  u_r4_qds_cg_iter_init_0.m_neg_0_o <> m_neg_0_iter_init_0
  u_r4_qds_cg_iter_init_0.m_pos_1_o <> m_pos_1_iter_init_0
  u_r4_qds_cg_iter_init_0.m_pos_2_o <> m_pos_2_iter_init_0

  a0_iter_init_1 := rt_iter_init_1(F32_FULL_RT_W - 1)
  a2_iter_init_1 := rt_iter_init_1(F32_FULL_RT_W - 3)
  a3_iter_init_1 := rt_iter_init_1(F32_FULL_RT_W - 4)
  a4_iter_init_1 := rt_iter_init_1(F32_FULL_RT_W - 5)
  val u_r4_qds_cg_iter_init_1 = Module(new r4_qds_cg())
  u_r4_qds_cg_iter_init_1.a0_i <> a0_iter_init_1
  u_r4_qds_cg_iter_init_1.a2_i <> a2_iter_init_1
  u_r4_qds_cg_iter_init_1.a3_i <> a3_iter_init_1
  u_r4_qds_cg_iter_init_1.a4_i <> a4_iter_init_1
  u_r4_qds_cg_iter_init_1.m_neg_1_o <> m_neg_1_iter_init_1
  u_r4_qds_cg_iter_init_1.m_neg_0_o <> m_neg_0_iter_init_1
  u_r4_qds_cg_iter_init_1.m_pos_1_o <> m_pos_1_iter_init_1
  u_r4_qds_cg_iter_init_1.m_pos_2_o <> m_pos_2_iter_init_1

  a0_iter_init_2 := rt_iter_init_2(F16_FULL_RT_W - 1)
  a2_iter_init_2 := rt_iter_init_2(F16_FULL_RT_W - 3)
  a3_iter_init_2 := rt_iter_init_2(F16_FULL_RT_W - 4)
  a4_iter_init_2 := rt_iter_init_2(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_iter_init_2 = Module(new r4_qds_cg())
  u_r4_qds_cg_iter_init_2.a0_i <> a0_iter_init_2
  u_r4_qds_cg_iter_init_2.a2_i <> a2_iter_init_2
  u_r4_qds_cg_iter_init_2.a3_i <> a3_iter_init_2
  u_r4_qds_cg_iter_init_2.a4_i <> a4_iter_init_2
  u_r4_qds_cg_iter_init_2.m_neg_1_o <> m_neg_1_iter_init_2
  u_r4_qds_cg_iter_init_2.m_neg_0_o <> m_neg_0_iter_init_2
  u_r4_qds_cg_iter_init_2.m_pos_1_o <> m_pos_1_iter_init_2
  u_r4_qds_cg_iter_init_2.m_pos_2_o <> m_pos_2_iter_init_2

  a0_iter_init_3 := rt_iter_init_3(F16_FULL_RT_W - 1)
  a2_iter_init_3 := rt_iter_init_3(F16_FULL_RT_W - 3)
  a3_iter_init_3 := rt_iter_init_3(F16_FULL_RT_W - 4)
  a4_iter_init_3 := rt_iter_init_3(F16_FULL_RT_W - 5)
  val u_r4_qds_cg_iter_init_3 = Module(new r4_qds_cg())
  u_r4_qds_cg_iter_init_3.a0_i <> a0_iter_init_3
  u_r4_qds_cg_iter_init_3.a2_i <> a2_iter_init_3
  u_r4_qds_cg_iter_init_3.a3_i <> a3_iter_init_3
  u_r4_qds_cg_iter_init_3.a4_i <> a4_iter_init_3
  u_r4_qds_cg_iter_init_3.m_neg_1_o <> m_neg_1_iter_init_3
  u_r4_qds_cg_iter_init_3.m_neg_0_o <> m_neg_0_iter_init_3
  u_r4_qds_cg_iter_init_3.m_pos_1_o <> m_pos_1_iter_init_3
  u_r4_qds_cg_iter_init_3.m_pos_2_o <> m_pos_2_iter_init_3

  m_neg_1_for_nxt_cycle_s0_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_1_for_nxt_cycle_s0_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_1_iter_init_0(4, 0), m_neg_1_to_nxt_cycle_0(4, 0))
  m_neg_1_for_nxt_cycle_s0_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_1_for_nxt_cycle_s0_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_1_iter_init_1(4, 0), m_neg_1_to_nxt_cycle_1(4, 0))
  m_neg_1_for_nxt_cycle_s0_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_1_for_nxt_cycle_s0_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_1_iter_init_2(4, 0), m_neg_1_to_nxt_cycle_2(4, 0))
  m_neg_1_for_nxt_cycle_s0_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_1_for_nxt_cycle_s0_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_1_iter_init_3(4, 0), m_neg_1_to_nxt_cycle_3(4, 0))
  m_neg_0_for_nxt_cycle_s0_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_0_for_nxt_cycle_s0_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_0_iter_init_0(3, 0), m_neg_0_to_nxt_cycle_0(3, 0))
  m_neg_0_for_nxt_cycle_s0_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_0_for_nxt_cycle_s0_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_0_iter_init_1(3, 0), m_neg_0_to_nxt_cycle_1(3, 0))
  m_neg_0_for_nxt_cycle_s0_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_0_for_nxt_cycle_s0_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_0_iter_init_2(3, 0), m_neg_0_to_nxt_cycle_2(3, 0))
  m_neg_0_for_nxt_cycle_s0_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_neg_0_for_nxt_cycle_s0_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_neg_0_iter_init_3(3, 0), m_neg_0_to_nxt_cycle_3(3, 0))
  m_pos_1_for_nxt_cycle_s0_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_1_for_nxt_cycle_s0_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_1_iter_init_0(2, 0), m_pos_1_to_nxt_cycle_0(2, 0))
  m_pos_1_for_nxt_cycle_s0_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_1_for_nxt_cycle_s0_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_1_iter_init_1(2, 0), m_pos_1_to_nxt_cycle_1(2, 0))
  m_pos_1_for_nxt_cycle_s0_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_1_for_nxt_cycle_s0_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_1_iter_init_2(2, 0), m_pos_1_to_nxt_cycle_2(2, 0))
  m_pos_1_for_nxt_cycle_s0_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_1_for_nxt_cycle_s0_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_1_iter_init_3(2, 0), m_pos_1_to_nxt_cycle_3(2, 0))
  m_pos_2_for_nxt_cycle_s0_qds_0_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_2_for_nxt_cycle_s0_qds_0_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_2_iter_init_0(4, 1), m_pos_2_to_nxt_cycle_0(4, 1))
  m_pos_2_for_nxt_cycle_s0_qds_1_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_2_for_nxt_cycle_s0_qds_1_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_2_iter_init_1(4, 1), m_pos_2_to_nxt_cycle_1(4, 1))
  m_pos_2_for_nxt_cycle_s0_qds_2_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_2_for_nxt_cycle_s0_qds_2_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_2_iter_init_2(4, 1), m_pos_2_to_nxt_cycle_2(4, 1))
  m_pos_2_for_nxt_cycle_s0_qds_3_en := start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_ITER_BIT)
  m_pos_2_for_nxt_cycle_s0_qds_3_d := Mux((fsm_q(FSM_PRE_0_BIT) | fsm_q(FSM_PRE_1_BIT)), m_pos_2_iter_init_3(4, 1), m_pos_2_to_nxt_cycle_3(4, 1))
  val u_fpsqrt_r16_block = Module(new fpsqrt_r16_block(
    S0_CSA_SPECULATIVE = S0_CSA_SPECULATIVE,
    S0_CSA_MERGED = S0_CSA_MERGED,
    S1_QDS_SPECULATIVE = S1_QDS_SPECULATIVE,
    S1_CSA_SPECULATIVE = S1_CSA_SPECULATIVE,
    S1_CSA_MERGED = S1_CSA_MERGED,
    RT_DIG_W = RT_DIG_W))
  u_fpsqrt_r16_block.fp_fmt_i <> fp_fmt_q
  u_fpsqrt_r16_block.f_r_s_i <> f_r_s_q
  u_fpsqrt_r16_block.f_r_c_i <> f_r_c_q
  u_fpsqrt_r16_block.rt_i <> rt_q
  u_fpsqrt_r16_block.rt_m1_i <> rt_m1_q
  u_fpsqrt_r16_block.mask_i <> mask_q
  u_fpsqrt_r16_block.nr_f_r_7b_for_nxt_cycle_s0_qds_0_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_0_q
  u_fpsqrt_r16_block.nr_f_r_7b_for_nxt_cycle_s0_qds_1_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_1_q
  u_fpsqrt_r16_block.nr_f_r_7b_for_nxt_cycle_s0_qds_2_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_2_q
  u_fpsqrt_r16_block.nr_f_r_7b_for_nxt_cycle_s0_qds_3_i <> nr_f_r_7b_for_nxt_cycle_s0_qds_3_q
  u_fpsqrt_r16_block.nr_f_r_9b_for_nxt_cycle_s1_qds_0_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_0_q
  u_fpsqrt_r16_block.nr_f_r_9b_for_nxt_cycle_s1_qds_1_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_1_q
  u_fpsqrt_r16_block.nr_f_r_9b_for_nxt_cycle_s1_qds_2_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_2_q
  u_fpsqrt_r16_block.nr_f_r_9b_for_nxt_cycle_s1_qds_3_i <> nr_f_r_9b_for_nxt_cycle_s1_qds_3_q
  u_fpsqrt_r16_block.m_neg_1_for_nxt_cycle_s0_qds_0_i <> m_neg_1_for_nxt_cycle_s0_qds_0_q
  u_fpsqrt_r16_block.m_neg_1_for_nxt_cycle_s0_qds_1_i <> m_neg_1_for_nxt_cycle_s0_qds_1_q
  u_fpsqrt_r16_block.m_neg_1_for_nxt_cycle_s0_qds_2_i <> m_neg_1_for_nxt_cycle_s0_qds_2_q
  u_fpsqrt_r16_block.m_neg_1_for_nxt_cycle_s0_qds_3_i <> m_neg_1_for_nxt_cycle_s0_qds_3_q
  u_fpsqrt_r16_block.m_neg_0_for_nxt_cycle_s0_qds_0_i <> m_neg_0_for_nxt_cycle_s0_qds_0_q
  u_fpsqrt_r16_block.m_neg_0_for_nxt_cycle_s0_qds_1_i <> m_neg_0_for_nxt_cycle_s0_qds_1_q
  u_fpsqrt_r16_block.m_neg_0_for_nxt_cycle_s0_qds_2_i <> m_neg_0_for_nxt_cycle_s0_qds_2_q
  u_fpsqrt_r16_block.m_neg_0_for_nxt_cycle_s0_qds_3_i <> m_neg_0_for_nxt_cycle_s0_qds_3_q
  u_fpsqrt_r16_block.m_pos_1_for_nxt_cycle_s0_qds_0_i <> m_pos_1_for_nxt_cycle_s0_qds_0_q
  u_fpsqrt_r16_block.m_pos_1_for_nxt_cycle_s0_qds_1_i <> m_pos_1_for_nxt_cycle_s0_qds_1_q
  u_fpsqrt_r16_block.m_pos_1_for_nxt_cycle_s0_qds_2_i <> m_pos_1_for_nxt_cycle_s0_qds_2_q
  u_fpsqrt_r16_block.m_pos_1_for_nxt_cycle_s0_qds_3_i <> m_pos_1_for_nxt_cycle_s0_qds_3_q
  u_fpsqrt_r16_block.m_pos_2_for_nxt_cycle_s0_qds_0_i <> m_pos_2_for_nxt_cycle_s0_qds_0_q
  u_fpsqrt_r16_block.m_pos_2_for_nxt_cycle_s0_qds_1_i <> m_pos_2_for_nxt_cycle_s0_qds_1_q
  u_fpsqrt_r16_block.m_pos_2_for_nxt_cycle_s0_qds_2_i <> m_pos_2_for_nxt_cycle_s0_qds_2_q
  u_fpsqrt_r16_block.m_pos_2_for_nxt_cycle_s0_qds_3_i <> m_pos_2_for_nxt_cycle_s0_qds_3_q
  u_fpsqrt_r16_block.nxt_rt_o <> nxt_rt
  u_fpsqrt_r16_block.nxt_rt_m1_o <> nxt_rt_m1
  u_fpsqrt_r16_block.nxt_f_r_s_o <> nxt_f_r_s
  u_fpsqrt_r16_block.nxt_f_r_c_o <> nxt_f_r_c
  u_fpsqrt_r16_block.adder_7b_res_for_nxt_cycle_s0_qds_0_o <> adder_7b_res_for_nxt_cycle_s0_qds_0
  u_fpsqrt_r16_block.adder_7b_res_for_nxt_cycle_s0_qds_1_o <> adder_7b_res_for_nxt_cycle_s0_qds_1
  u_fpsqrt_r16_block.adder_7b_res_for_nxt_cycle_s0_qds_2_o <> adder_7b_res_for_nxt_cycle_s0_qds_2
  u_fpsqrt_r16_block.adder_7b_res_for_nxt_cycle_s0_qds_3_o <> adder_7b_res_for_nxt_cycle_s0_qds_3
  u_fpsqrt_r16_block.adder_9b_res_for_nxt_cycle_s1_qds_0_o <> adder_9b_res_for_nxt_cycle_s1_qds_0
  u_fpsqrt_r16_block.adder_9b_res_for_nxt_cycle_s1_qds_1_o <> adder_9b_res_for_nxt_cycle_s1_qds_1
  u_fpsqrt_r16_block.adder_9b_res_for_nxt_cycle_s1_qds_2_o <> adder_9b_res_for_nxt_cycle_s1_qds_2
  u_fpsqrt_r16_block.adder_9b_res_for_nxt_cycle_s1_qds_3_o <> adder_9b_res_for_nxt_cycle_s1_qds_3
  u_fpsqrt_r16_block.m_neg_1_to_nxt_cycle_0_o <> m_neg_1_to_nxt_cycle_0
  u_fpsqrt_r16_block.m_neg_1_to_nxt_cycle_1_o <> m_neg_1_to_nxt_cycle_1
  u_fpsqrt_r16_block.m_neg_1_to_nxt_cycle_2_o <> m_neg_1_to_nxt_cycle_2
  u_fpsqrt_r16_block.m_neg_1_to_nxt_cycle_3_o <> m_neg_1_to_nxt_cycle_3
  u_fpsqrt_r16_block.m_neg_0_to_nxt_cycle_0_o <> m_neg_0_to_nxt_cycle_0
  u_fpsqrt_r16_block.m_neg_0_to_nxt_cycle_1_o <> m_neg_0_to_nxt_cycle_1
  u_fpsqrt_r16_block.m_neg_0_to_nxt_cycle_2_o <> m_neg_0_to_nxt_cycle_2
  u_fpsqrt_r16_block.m_neg_0_to_nxt_cycle_3_o <> m_neg_0_to_nxt_cycle_3
  u_fpsqrt_r16_block.m_pos_1_to_nxt_cycle_0_o <> m_pos_1_to_nxt_cycle_0
  u_fpsqrt_r16_block.m_pos_1_to_nxt_cycle_1_o <> m_pos_1_to_nxt_cycle_1
  u_fpsqrt_r16_block.m_pos_1_to_nxt_cycle_2_o <> m_pos_1_to_nxt_cycle_2
  u_fpsqrt_r16_block.m_pos_1_to_nxt_cycle_3_o <> m_pos_1_to_nxt_cycle_3
  u_fpsqrt_r16_block.m_pos_2_to_nxt_cycle_0_o <> m_pos_2_to_nxt_cycle_0
  u_fpsqrt_r16_block.m_pos_2_to_nxt_cycle_1_o <> m_pos_2_to_nxt_cycle_1
  u_fpsqrt_r16_block.m_pos_2_to_nxt_cycle_2_o <> m_pos_2_to_nxt_cycle_2
  u_fpsqrt_r16_block.m_pos_2_to_nxt_cycle_3_o <> m_pos_2_to_nxt_cycle_3

  nr_f_r_adder_in(0) := Cat(
    f_r_s_q(63, 48),
    ~fp_fmt_q(0),
    f_r_s_q(47, 32),
    fp_fmt_q(2),
    f_r_s_q(31, 16),
    ~fp_fmt_q(0),
    f_r_s_q(15, 0)
  )
  nr_f_r_adder_in(1) := Cat(
    f_r_c_q(63, 48),
    "b0".U(1.W),
    f_r_c_q(47, 32),
    "b0".U(1.W),
    f_r_c_q(31, 16),
    "b0".U(1.W),
    f_r_c_q(15, 0)
  )
  nr_f_r := nr_f_r_adder_in(0) + nr_f_r_adder_in(1)
  nr_f_r_merged := f_r_s_q + f_r_c_q
  f_r_xor := f_r_s_q((REM_W - 1) - 1, 1) ^ f_r_c_q((REM_W - 1) - 1, 1)
  f_r_or := f_r_s_q((REM_W - 1) - 2, 0) | f_r_c_q((REM_W - 1) - 2, 0)
  if (S0_CSA_IS_MERGED == 1) {
    rem_is_not_zero_0 := nr_f_r_merged(69) | (
      (f_r_xor(67, 54) =/= f_r_or(67, 54)) |
        (!fp_fmt_q(0) & (f_r_xor(53, 42) =/= f_r_or(53, 42))) |
        (fp_fmt_q(2) & (f_r_xor(41, 14) =/= f_r_or(41, 14)))
      )
    rem_is_not_zero_1 := nr_f_r_merged(33) | (
      (f_r_xor(31, 18) =/= f_r_or(31, 18)) |
        (fp_fmt_q(1) & (f_r_xor(17, 6) =/= f_r_or(17, 6)))
      )
    rem_is_not_zero_2 := nr_f_r_merged(51) | (f_r_xor(49, 36) =/= f_r_or(49, 36))
    rem_is_not_zero_3 := nr_f_r_merged(15) | (f_r_xor(13, 0) =/= f_r_or(13, 0))
    select_rt_m1_0 := nr_f_r_merged(69) & ~res_is_sqrt_2_q
    select_rt_m1_1 := nr_f_r_merged(33) & ~res_is_sqrt_2_q
    select_rt_m1_2 := nr_f_r_merged(51)
    select_rt_m1_3 := nr_f_r_merged(15) & ~res_is_sqrt_2_q
  }
  else {
    rem_is_not_zero_0 := nr_f_r(66) | (
      (f_r_xor(61, 48) =/= f_r_or(61, 48)) |
        (!fp_fmt_q(0) & (f_r_xor(47, 36) =/= f_r_or(47, 36))) |
        (fp_fmt_q(2) & (f_r_xor(35, 8) =/= f_r_or(35, 8)))
      )
    rem_is_not_zero_1 := nr_f_r(32) | (
      (f_r_xor(29, 16) =/= f_r_or(29, 16)) |
        (fp_fmt_q(1) & (f_r_xor(15, 4) =/= f_r_or(15, 4)))
      )
    rem_is_not_zero_2 := nr_f_r(49) | (f_r_xor(45, 32) =/= f_r_or(45, 32))
    rem_is_not_zero_3 := nr_f_r(15) | (f_r_xor(13, 0) =/= f_r_or(13, 0))
    select_rt_m1_0 := nr_f_r(66) & ~res_is_sqrt_2_q
    select_rt_m1_1 := nr_f_r(32) & ~res_is_sqrt_2_q
    select_rt_m1_2 := nr_f_r(49)
    select_rt_m1_3 := nr_f_r(15) & ~res_is_sqrt_2_q
  }

  f64_res_is_sqrt_2 := res_is_sqrt_2_q & fp_fmt_q(2)
  f32_res_is_sqrt_2 := res_is_sqrt_2_q & fp_fmt_q(1)
  f16_res_is_sqrt_2 := res_is_sqrt_2_q & fp_fmt_q(0)
  rt_for_inc := Mux(res_is_sqrt_2_q, (
    (Fill(56, f64_res_is_sqrt_2) & Cat(SQRT_2_WITH_ROUND_BIT, rt_q(1, 0))) |
      (Fill(56, f32_res_is_sqrt_2) & Cat(rt_q(55, 28), SQRT_2_WITH_ROUND_BIT(53, 53 - 25 + 1), rt_q(2, 0))) |
      (Fill(56, f16_res_is_sqrt_2) & Cat(rt_q(55, 14), SQRT_2_WITH_ROUND_BIT(53, 53 - 12 + 1), rt_q(1, 0)))
    ), rt_q)
  rt_pre_inc := Cat(
    rt_for_inc(54, 42), Mux(
      fp_fmt_q(0), "b0".U(1.W), rt_for_inc(41)),
    rt_for_inc(40, 28),
    Mux((fp_fmt_q(0) | fp_fmt_q(1)), "b0".U(1.W), rt_for_inc(27)),
    rt_for_inc(26, 14), Mux(
      fp_fmt_q(0), "b0".U(1.W), rt_for_inc(13)),
    rt_for_inc(12, 3)
  )
  rt_inc_lane :=
    (Fill(52, fp_fmt_q(0)) & Cat(
      "b0".U(9.W), "b1".U(1.W),
      "b0".U(4.W),
      "b0".U(9.W), "b1".U(1.W),
      "b0".U(4.W),
      "b0".U(9.W), "b1".U(1.W),
      "b0".U(4.W),
      "b0".U(9.W), "b1".U(1.W)
    )) |
      (Fill(52, fp_fmt_q(1)) & Cat(
        "b0".U(22.W), "b1".U(1.W),
        "b0".U(5.W),
        "b0".U(22.W), "b1".U(1.W),
        "b0".U(1.W)
      )) |
      (Fill(52, fp_fmt_q(2)) & Cat("b0".U(51.W), "b1".U(1.W)))
  rt_inc_res := Cat("b0".U(1.W), rt_pre_inc) + Cat("b0".U(1.W), rt_inc_lane)
  rt_m1_pre_inc_0 := rt_m1_q(52, 1)
  rt_m1_pre_inc_1 := rt_m1_q(26, 4)
  rt_m1_pre_inc_2 := rt_m1_q(39, 30)
  rt_m1_pre_inc_3 := rt_m1_q(13, 4)
  guard_bit_rt_0 :=
    (Fill(1, fp_fmt_q(0)) & rt_q(45)) |
      (Fill(1, fp_fmt_q(1)) & rt_q(32)) |
      (Fill(1, fp_fmt_q(2)) & rt_q(3))
  guard_bit_rt_1 :=
    (Fill(1, fp_fmt_q(0)) & rt_q(17)) |
      (Fill(1, fp_fmt_q(1)) & rt_q(4))
  guard_bit_rt_2 := rt_q(31)
  guard_bit_rt_3 := rt_q(3)
  guard_bit_rt_m1_0 :=
    (Fill(1, fp_fmt_q(0)) & rt_m1_q(43)) |
      (Fill(1, fp_fmt_q(1)) & rt_m1_q(30)) |
      (Fill(1, fp_fmt_q(2)) & rt_m1_q(1))
  guard_bit_rt_m1_1 :=
    (Fill(1, fp_fmt_q(0)) & rt_m1_q(17)) |
      (Fill(1, fp_fmt_q(1)) & rt_m1_q(4))
  guard_bit_rt_m1_2 := rt_m1_q(30)
  guard_bit_rt_m1_3 := rt_m1_q(4)
  rt_m1_inc_res_0 := Mux((guard_bit_rt_0 === guard_bit_rt_m1_0), rt_inc_res, Cat("b0".U(1.W), rt_pre_inc))
  rt_m1_inc_res_1 := Mux((guard_bit_rt_1 === guard_bit_rt_m1_1), rt_inc_res(24, 1), Cat("b0".U(1.W), rt_pre_inc(23, 1)))
  rt_m1_inc_res_2 := Mux((guard_bit_rt_2 === guard_bit_rt_m1_2), rt_inc_res(38, 28), Cat("b0".U(1.W), rt_pre_inc(37, 28)))
  rt_m1_inc_res_3 := Mux((guard_bit_rt_3 === guard_bit_rt_m1_3), rt_inc_res(10, 0), Cat("b0".U(1.W), rt_pre_inc(9, 0)))
  round_bit_rt_0 :=
    (Fill(1, fp_fmt_q(0)) & rt_for_inc(44)) |
      (Fill(1, fp_fmt_q(1)) & rt_for_inc(31)) |
      (Fill(1, fp_fmt_q(2)) & rt_for_inc(2))
  round_bit_rt_1 :=
    (Fill(1, fp_fmt_q(0)) & rt_for_inc(16)) |
      (Fill(1, fp_fmt_q(1)) & rt_for_inc(3))
  round_bit_rt_2 := rt_for_inc(30)
  round_bit_rt_3 := rt_for_inc(2)
  sticky_bit_rt_0 := rem_is_not_zero_0
  sticky_bit_rt_1 := rem_is_not_zero_1
  sticky_bit_rt_2 := rem_is_not_zero_2
  sticky_bit_rt_3 := rem_is_not_zero_3
  inexact_rt_0 := round_bit_rt_0 | sticky_bit_rt_0
  inexact_rt_1 := round_bit_rt_1 | sticky_bit_rt_1
  inexact_rt_2 := round_bit_rt_2 | sticky_bit_rt_2
  inexact_rt_3 := round_bit_rt_3 | sticky_bit_rt_3
  inexact_0 := inexact_rt_0 | select_rt_m1_0
  inexact_1 := inexact_rt_1 | select_rt_m1_1
  inexact_2 := inexact_rt_2 | select_rt_m1_2
  inexact_3 := inexact_rt_3 | select_rt_m1_3
  rt_need_rup_0 :=
    (Cat(rm_q === RM_RNE) & round_bit_rt_0) |
      (Cat(rm_q === RM_RUP) & (round_bit_rt_0 | sticky_bit_rt_0)) |
      (Cat(rm_q === RM_RMM) & round_bit_rt_0)
  rt_need_rup_1 :=
    (Cat(rm_q === RM_RNE) & round_bit_rt_1) |
      (Cat(rm_q === RM_RUP) & (round_bit_rt_1 | sticky_bit_rt_1)) |
      (Cat(rm_q === RM_RMM) & round_bit_rt_1)
  rt_need_rup_2 :=
    (Cat(rm_q === RM_RNE) & round_bit_rt_2) |
      (Cat(rm_q === RM_RUP) & (round_bit_rt_2 | sticky_bit_rt_2)) |
      (Cat(rm_q === RM_RMM) & round_bit_rt_2)
  rt_need_rup_3 :=
    (Cat(rm_q === RM_RNE) & round_bit_rt_3) |
      (Cat(rm_q === RM_RUP) & (round_bit_rt_3 | sticky_bit_rt_3)) |
      (Cat(rm_q === RM_RMM) & round_bit_rt_3)
  round_bit_rt_m1_0 :=
    (Fill(1, fp_fmt_q(0)) & rt_m1_q(42)) |
      (Fill(1, fp_fmt_q(1)) & rt_m1_q(29)) |
      (Fill(1, fp_fmt_q(2)) & rt_m1_q(0))
  round_bit_rt_m1_1 :=
    (Fill(1, fp_fmt_q(0)) & rt_m1_q(16)) |
      (Fill(1, fp_fmt_q(1)) & rt_m1_q(3))
  round_bit_rt_m1_2 := rt_m1_q(29)
  round_bit_rt_m1_3 := rt_m1_q(3)
  rt_m1_need_rup_0 := (rm_q === RM_RUP) | (((rm_q === RM_RNE) | (rm_q === RM_RMM)) & round_bit_rt_m1_0)
  rt_m1_need_rup_1 := (rm_q === RM_RUP) | (((rm_q === RM_RNE) | (rm_q === RM_RMM)) & round_bit_rt_m1_1)
  rt_m1_need_rup_2 := (rm_q === RM_RUP) | (((rm_q === RM_RNE) | (rm_q === RM_RMM)) & round_bit_rt_m1_2)
  rt_m1_need_rup_3 := (rm_q === RM_RUP) | (((rm_q === RM_RNE) | (rm_q === RM_RMM)) & round_bit_rt_m1_3)
  rt_rounded_0 := Mux(rt_need_rup_0, rt_inc_res, Cat("b0".U(1.W), rt_pre_inc))
  rt_rounded_1 := Mux(rt_need_rup_1, rt_inc_res(24, 1), Cat("b0".U(1.W), rt_pre_inc(23, 1)))
  rt_rounded_2 := Mux(rt_need_rup_2, rt_inc_res(38, 28), Cat("b0".U(1.W), rt_pre_inc(37, 28)))
  rt_rounded_3 := Mux(rt_need_rup_3, rt_inc_res(10, 0), Cat("b0".U(1.W), rt_pre_inc(9, 0)))
  rt_m1_rounded_0 := Mux(rt_m1_need_rup_0, rt_m1_inc_res_0, Cat("b0".U(1.W), rt_m1_pre_inc_0))
  rt_m1_rounded_1 := Mux(rt_m1_need_rup_1, rt_m1_inc_res_1, Cat("b0".U(1.W), rt_m1_pre_inc_1))
  rt_m1_rounded_2 := Mux(rt_m1_need_rup_2, rt_m1_inc_res_2, Cat("b0".U(1.W), rt_m1_pre_inc_2))
  rt_m1_rounded_3 := Mux(rt_m1_need_rup_3, rt_m1_inc_res_3, Cat("b0".U(1.W), rt_m1_pre_inc_3))
  frac_rounded_0 := Mux(select_rt_m1_0, rt_m1_rounded_0, rt_rounded_0)
  frac_rounded_1 := Mux(select_rt_m1_1, rt_m1_rounded_1, rt_rounded_1)
  frac_rounded_2 := Mux(select_rt_m1_2, rt_m1_rounded_2, rt_rounded_2)
  frac_rounded_3 := Mux(select_rt_m1_3, rt_m1_rounded_3, rt_rounded_3)
  carry_after_round_0 := frac_rounded_0(52)
  carry_after_round_1 := frac_rounded_1(23)
  carry_after_round_2 := frac_rounded_2(10)
  carry_after_round_3 := frac_rounded_3(10)
  exp_rounded_0 := Mux(carry_after_round_0, (out_exp_0_q + 1.U(11.W)), out_exp_0_q)
  exp_rounded_1 := Mux(carry_after_round_1, (out_exp_1_q + 1.U(8.W)), out_exp_1_q)
  exp_rounded_2 := Mux(carry_after_round_2, (out_exp_2_q + 1.U(5.W)), out_exp_2_q)
  exp_rounded_3 := Mux(carry_after_round_3, (out_exp_3_q + 1.U(5.W)), out_exp_3_q)
  f16_exp_res_0 :=
    Mux((res_is_nan_0_q | res_is_inf_0_q), Fill(5, "b1".U(1.W)), Mux(
      res_is_exact_zero_0_q, "b0".U(5.W),
      exp_rounded_0(4, 0)))
  f16_exp_res_1 :=
    Mux((res_is_nan_1_q | res_is_inf_1_q), Fill(5, "b1".U(1.W)), Mux(
      res_is_exact_zero_1_q, "b0".U(5.W),
      exp_rounded_1(4, 0)))
  f16_exp_res_2 :=
    Mux((res_is_nan_2_q | res_is_inf_2_q), Fill(5, "b1".U(1.W)), Mux(
      res_is_exact_zero_2_q, "b0".U(5.W),
      exp_rounded_2))
  f16_exp_res_3 :=
    Mux((res_is_nan_3_q | res_is_inf_3_q), Fill(5, "b1".U(1.W)), Mux(
      res_is_exact_zero_3_q, "b0".U(5.W),
      exp_rounded_3))
  f32_exp_res_0 :=
    Mux((res_is_nan_0_q | res_is_inf_0_q), Fill(8, "b1".U(1.W)), Mux(
      res_is_exact_zero_0_q, "b0".U(8.W),
      exp_rounded_0(7, 0)))
  f32_exp_res_1 :=
    Mux((res_is_nan_1_q | res_is_inf_1_q), Fill(8, "b1".U(1.W)), Mux(
      res_is_exact_zero_1_q, "b0".U(8.W),
      exp_rounded_1))
  f64_exp_res_0 :=
    Mux((res_is_nan_0_q | res_is_inf_0_q), Fill(11, "b1".U(1.W)), Mux(
      res_is_exact_zero_0_q, "b0".U(11.W),
      exp_rounded_0))
  f16_frac_res_0 := Mux(
    res_is_nan_0_q, Cat("b1".U(1.W), "b0".U(9.W)),
    Mux((res_is_inf_0_q | res_is_exact_zero_0_q), "b0".U(10.W),
      frac_rounded_0(51, 51 - 10 + 1)))
  f16_frac_res_1 := Mux(
    res_is_nan_1_q, Cat("b1".U(1.W), "b0".U(9.W)),
    Mux((res_is_inf_1_q | res_is_exact_zero_1_q), "b0".U(10.W),
      frac_rounded_1(22, 22 - 10 + 1)))
  f16_frac_res_2 := Mux(
    res_is_nan_2_q, Cat("b1".U(1.W), "b0".U(9.W)),
    Mux((res_is_inf_2_q | res_is_exact_zero_2_q), "b0".U(10.W),
      frac_rounded_2(0 + 10 - 1, 0)))
  f16_frac_res_3 := Mux(
    res_is_nan_3_q, Cat("b1".U(1.W), "b0".U(9.W)),
    Mux((res_is_inf_3_q | res_is_exact_zero_3_q), "b0".U(10.W),
      frac_rounded_3(0 + 10 - 1, 0)))
  f32_frac_res_0 := Mux(
    res_is_nan_0_q, Cat("b1".U(1.W), "b0".U(22.W)),
    Mux((res_is_inf_0_q | res_is_exact_zero_0_q), "b0".U(23.W),
      frac_rounded_0(51, 51 - 23 + 1)))
  f32_frac_res_1 := Mux(
    res_is_nan_1_q, Cat("b1".U(1.W), "b0".U(22.W)),
    Mux((res_is_inf_1_q | res_is_exact_zero_1_q), "b0".U(23.W),
      frac_rounded_1(0 + 23 - 1, 0)))
  f64_frac_res_0 := Mux(
    res_is_nan_0_q, Cat("b1".U(1.W), "b0".U(51.W)),
    Mux((res_is_inf_0_q | res_is_exact_zero_0_q), "b0".U(52.W),
      frac_rounded_0(0 + 52 - 1, 0)))
  f16_res_0 := Cat(out_sign_0_q, f16_exp_res_0, f16_frac_res_0)
  f16_res_1 := Cat(out_sign_1_q, f16_exp_res_1, f16_frac_res_1)
  f16_res_2 := Cat(out_sign_2_q, f16_exp_res_2, f16_frac_res_2)
  f16_res_3 := Cat(out_sign_3_q, f16_exp_res_3, f16_frac_res_3)
  f32_res_0 := Cat(out_sign_0_q, f32_exp_res_0, f32_frac_res_0)
  f32_res_1 := Cat(out_sign_1_q, f32_exp_res_1, f32_frac_res_1)
  f64_res_0 := Cat(out_sign_0_q, f64_exp_res_0, f64_frac_res_0)
  val fpsqrt_res_vector = (Fill(64, fp_fmt_q(0)) & Cat(f16_res_0, f16_res_2, f16_res_1, f16_res_3)) |
    (Fill(64, fp_fmt_q(1)) & Cat(f32_res_0, f32_res_1)) |
    (Fill(64, fp_fmt_q(2)) & f64_res_0)
  val fpsqrt_res_scalar = Mux1H(
    Seq(
      fp_fmt_q(0) -> Cat(0.U(48.W),f16_res_3),
      fp_fmt_q(1) -> Cat(0.U(32.W),f32_res_1),
      fp_fmt_q(2) -> f64_res_0
    )
  )
  fpsqrt_res_o := Mux(v_mode_q, fpsqrt_res_vector, fpsqrt_res_scalar)
  //    (Fill(64, fp_fmt_q(0)) & Cat(f16_res_0, f16_res_2, f16_res_1, f16_res_3)) |
  //      (Fill(64, fp_fmt_q(1)) & Cat(f32_res_0, f32_res_1)) |
  //      (Fill(64, fp_fmt_q(2)) & f64_res_0)
  fflags_invalid_operation_0 := op_invalid_0_q
  fflags_div_by_zero_0 := 0.U
  fflags_overflow_0 := 0.U
  fflags_underflow_0 := 0.U
  fflags_inexact_0 := inexact_0 & ~res_is_nan_0_q & ~res_is_exact_zero_0_q
  fflags_invalid_operation_1 := op_invalid_1_q
  fflags_div_by_zero_1 := 0.U
  fflags_overflow_1 := 0.U
  fflags_underflow_1 := 0.U
  fflags_inexact_1 := inexact_1 & ~res_is_nan_1_q & ~res_is_exact_zero_1_q
  fflags_invalid_operation_2 := op_invalid_2_q
  fflags_div_by_zero_2 := 0.U
  fflags_overflow_2 := 0.U
  fflags_underflow_2 := 0.U
  fflags_inexact_2 := inexact_2 & ~res_is_nan_2_q & ~res_is_exact_zero_2_q
  fflags_invalid_operation_3 := op_invalid_3_q
  fflags_div_by_zero_3 := 0.U
  fflags_overflow_3 := 0.U
  fflags_underflow_3 := 0.U
  fflags_inexact_3 := inexact_3 & ~res_is_nan_3_q & ~res_is_exact_zero_3_q
  f16_fflags_invalid_operation := fflags_invalid_operation_3 | (Mux(
    v_mode_q, (
      fflags_invalid_operation_0 |
        fflags_invalid_operation_1 |
        fflags_invalid_operation_2
      ), "b0".U(1.W)
  ))
  f32_fflags_invalid_operation := fflags_invalid_operation_1 | (Mux(v_mode_q, fflags_invalid_operation_0, "b0".U(1.W)))
  f64_fflags_invalid_operation := fflags_invalid_operation_0
  f16_fflags_div_by_zero := fflags_div_by_zero_3 | (Mux(
    v_mode_q, (
      fflags_div_by_zero_0 |
        fflags_div_by_zero_1 |
        fflags_div_by_zero_2
      ), "b0".U(1.W)
  ))
  f32_fflags_div_by_zero := fflags_div_by_zero_1 | (Mux(v_mode_q, fflags_div_by_zero_0, "b0".U(1.W)))
  f64_fflags_div_by_zero := fflags_div_by_zero_0
  f16_fflags_overflow := fflags_overflow_3 | (Mux(
    v_mode_q, (
      fflags_overflow_0 |
        fflags_overflow_1 |
        fflags_overflow_2
      ), "b0".U(1.W)
  ))
  f32_fflags_overflow := fflags_overflow_1 | (Mux(v_mode_q, fflags_overflow_0, "b0".U(1.W)))
  f64_fflags_overflow := fflags_overflow_0
  f16_fflags_underflow := fflags_underflow_3 | (Mux(
    v_mode_q, (
      fflags_underflow_0 |
        fflags_underflow_1 |
        fflags_underflow_2
      ), "b0".U(1.W)
  ))
  f32_fflags_underflow := fflags_underflow_1 | (Mux(v_mode_q, fflags_underflow_0, "b0".U(1.W)))
  f64_fflags_underflow := fflags_underflow_0
  f16_fflags_inexact := fflags_inexact_3 | (Mux(
    v_mode_q, (
      fflags_inexact_0 |
        fflags_inexact_1 |
        fflags_inexact_2
      ), "b0".U(1.W)
  ))
  f32_fflags_inexact := fflags_inexact_1 | (Mux(v_mode_q, fflags_inexact_0, "b0".U(1.W)))
  f64_fflags_inexact := fflags_inexact_0
  val fflags_3 = Cat(fflags_invalid_operation_3,0.U,0.U,0.U,fflags_inexact_3)
  val fflags_2 = Cat(fflags_invalid_operation_2,0.U,0.U,0.U,fflags_inexact_2)
  val fflags_1 = Cat(fflags_invalid_operation_1,0.U,0.U,0.U,fflags_inexact_1)
  val fflags_0 = Cat(fflags_invalid_operation_0,0.U,0.U,0.U,fflags_inexact_0)
  val scalar_fflags = Mux1H(
    Seq(
      fp_fmt_q(0) -> fflags_3,
      fp_fmt_q(1) -> fflags_1,
      fp_fmt_q(2) -> fflags_0
    )
  )
  fflags_o := scalar_fflags
  //    Cat(
  //    (Fill(1, fp_fmt_q(0)) & f16_fflags_invalid_operation) |
  //      (Fill(1, fp_fmt_q(1)) & f32_fflags_invalid_operation) |
  //      (Fill(1, fp_fmt_q(2)) & f64_fflags_invalid_operation),
  //    (Fill(1, fp_fmt_q(0)) & f16_fflags_div_by_zero) |
  //      (Fill(1, fp_fmt_q(1)) & f32_fflags_div_by_zero) |
  //      (Fill(1, fp_fmt_q(2)) & f64_fflags_div_by_zero),
  //    (Fill(1, fp_fmt_q(0)) & f16_fflags_overflow) |
  //      (Fill(1, fp_fmt_q(1)) & f32_fflags_overflow) |
  //      (Fill(1, fp_fmt_q(2)) & f64_fflags_overflow),
  //    (Fill(1, fp_fmt_q(0)) & f16_fflags_underflow) |
  //      (Fill(1, fp_fmt_q(1)) & f32_fflags_underflow) |
  //      (Fill(1, fp_fmt_q(2)) & f64_fflags_underflow),
  //    (Fill(1, fp_fmt_q(0)) & f16_fflags_inexact) |
  //      (Fill(1, fp_fmt_q(1)) & f32_fflags_inexact) |
  //      (Fill(1, fp_fmt_q(2)) & f64_fflags_inexact)
  //  )
  when(flush_i) {
    fsm_d := FSM_PRE_0
  }.elsewhen(fsm_q === FSM_PRE_0) {
    fsm_d := Mux(start_valid_i, (Mux(early_finish, FSM_POST_0, (Mux(need_2_cycles_init, FSM_PRE_1, FSM_ITER)))), FSM_PRE_0)
  }.elsewhen(fsm_q === FSM_PRE_1) {
    fsm_d := FSM_ITER
  }.elsewhen(fsm_q === FSM_ITER) {
    fsm_d := Mux(final_iter, FSM_POST_0, FSM_ITER)
  }.elsewhen(fsm_q === FSM_POST_0) {
    fsm_d := Mux(finish_ready_i, FSM_PRE_0, FSM_POST_0)
  }.otherwise {
    fsm_d := FSM_PRE_0
  }
  fsm_q := fsm_d

  when(start_handshaked) {
    fp_fmt_q := fp_fmt_d
    rm_q := rm_d
    v_mode_q := v_mode_d
    res_is_sqrt_2_q := res_is_sqrt_2_d
    op_invalid_0_q := op_invalid_0_d
    op_invalid_1_q := op_invalid_1_d
    op_invalid_2_q := op_invalid_2_d
    op_invalid_3_q := op_invalid_3_d
    res_is_nan_0_q := res_is_nan_0_d
    res_is_nan_1_q := res_is_nan_1_d
    res_is_nan_2_q := res_is_nan_2_d
    res_is_nan_3_q := res_is_nan_3_d
    res_is_inf_0_q := res_is_inf_0_d
    res_is_inf_1_q := res_is_inf_1_d
    res_is_inf_2_q := res_is_inf_2_d
    res_is_inf_3_q := res_is_inf_3_d
    res_is_exact_zero_0_q := res_is_exact_zero_0_d
    res_is_exact_zero_1_q := res_is_exact_zero_1_d
    res_is_exact_zero_2_q := res_is_exact_zero_2_d
    res_is_exact_zero_3_q := res_is_exact_zero_3_d
    out_sign_0_q := out_sign_0_d
    out_sign_1_q := out_sign_1_d
    out_sign_2_q := out_sign_2_d
    out_sign_3_q := out_sign_3_d
    out_exp_0_q := out_exp_0_d
    out_exp_1_q := out_exp_1_d
    out_exp_2_q := out_exp_2_d
    out_exp_3_q := out_exp_3_d
  }
  when(rt_en) {
    rt_q := rt_d
  }
  when(rt_m1_en) {
    rt_m1_q := rt_m1_d
  }
  when(mask_en) {
    mask_q := mask_d
  }
  when(f_r_s_en) {
    f_r_s_q := f_r_s_d
  }
  when(f_r_c_en) {
    f_r_c_q := f_r_c_d
  }
  when(iter_num_en) {
    iter_num_q := iter_num_d
  }
  when(nr_f_r_7b_for_nxt_cycle_s0_qds_0_en) {
    nr_f_r_7b_for_nxt_cycle_s0_qds_0_q := nr_f_r_7b_for_nxt_cycle_s0_qds_0_d
  }
  when(nr_f_r_7b_for_nxt_cycle_s0_qds_1_en) {
    nr_f_r_7b_for_nxt_cycle_s0_qds_1_q := nr_f_r_7b_for_nxt_cycle_s0_qds_1_d
  }
  when(nr_f_r_7b_for_nxt_cycle_s0_qds_2_en) {
    nr_f_r_7b_for_nxt_cycle_s0_qds_2_q := nr_f_r_7b_for_nxt_cycle_s0_qds_2_d
  }
  when(nr_f_r_7b_for_nxt_cycle_s0_qds_3_en) {
    nr_f_r_7b_for_nxt_cycle_s0_qds_3_q := nr_f_r_7b_for_nxt_cycle_s0_qds_3_d
  }
  when(nr_f_r_9b_for_nxt_cycle_s1_qds_0_en) {
    nr_f_r_9b_for_nxt_cycle_s1_qds_0_q := nr_f_r_9b_for_nxt_cycle_s1_qds_0_d
  }
  when(nr_f_r_9b_for_nxt_cycle_s1_qds_1_en) {
    nr_f_r_9b_for_nxt_cycle_s1_qds_1_q := nr_f_r_9b_for_nxt_cycle_s1_qds_1_d
  }
  when(nr_f_r_9b_for_nxt_cycle_s1_qds_2_en) {
    nr_f_r_9b_for_nxt_cycle_s1_qds_2_q := nr_f_r_9b_for_nxt_cycle_s1_qds_2_d
  }
  when(nr_f_r_9b_for_nxt_cycle_s1_qds_3_en) {
    nr_f_r_9b_for_nxt_cycle_s1_qds_3_q := nr_f_r_9b_for_nxt_cycle_s1_qds_3_d
  }
  when(m_neg_1_for_nxt_cycle_s0_qds_0_en) {
    m_neg_1_for_nxt_cycle_s0_qds_0_q := m_neg_1_for_nxt_cycle_s0_qds_0_d
  }
  when(m_neg_1_for_nxt_cycle_s0_qds_1_en) {
    m_neg_1_for_nxt_cycle_s0_qds_1_q := m_neg_1_for_nxt_cycle_s0_qds_1_d
  }
  when(m_neg_1_for_nxt_cycle_s0_qds_2_en) {
    m_neg_1_for_nxt_cycle_s0_qds_2_q := m_neg_1_for_nxt_cycle_s0_qds_2_d
  }
  when(m_neg_1_for_nxt_cycle_s0_qds_3_en) {
    m_neg_1_for_nxt_cycle_s0_qds_3_q := m_neg_1_for_nxt_cycle_s0_qds_3_d
  }
  when(m_neg_0_for_nxt_cycle_s0_qds_0_en) {
    m_neg_0_for_nxt_cycle_s0_qds_0_q := m_neg_0_for_nxt_cycle_s0_qds_0_d
  }
  when(m_neg_0_for_nxt_cycle_s0_qds_1_en) {
    m_neg_0_for_nxt_cycle_s0_qds_1_q := m_neg_0_for_nxt_cycle_s0_qds_1_d
  }
  when(m_neg_0_for_nxt_cycle_s0_qds_2_en) {
    m_neg_0_for_nxt_cycle_s0_qds_2_q := m_neg_0_for_nxt_cycle_s0_qds_2_d
  }
  when(m_neg_0_for_nxt_cycle_s0_qds_3_en) {
    m_neg_0_for_nxt_cycle_s0_qds_3_q := m_neg_0_for_nxt_cycle_s0_qds_3_d
  }
  when(m_pos_1_for_nxt_cycle_s0_qds_0_en) {
    m_pos_1_for_nxt_cycle_s0_qds_0_q := m_pos_1_for_nxt_cycle_s0_qds_0_d
  }
  when(m_pos_1_for_nxt_cycle_s0_qds_1_en) {
    m_pos_1_for_nxt_cycle_s0_qds_1_q := m_pos_1_for_nxt_cycle_s0_qds_1_d
  }
  when(m_pos_1_for_nxt_cycle_s0_qds_2_en) {
    m_pos_1_for_nxt_cycle_s0_qds_2_q := m_pos_1_for_nxt_cycle_s0_qds_2_d
  }
  when(m_pos_1_for_nxt_cycle_s0_qds_3_en) {
    m_pos_1_for_nxt_cycle_s0_qds_3_q := m_pos_1_for_nxt_cycle_s0_qds_3_d
  }
  when(m_pos_2_for_nxt_cycle_s0_qds_0_en) {
    m_pos_2_for_nxt_cycle_s0_qds_0_q := m_pos_2_for_nxt_cycle_s0_qds_0_d
  }
  when(m_pos_2_for_nxt_cycle_s0_qds_1_en) {
    m_pos_2_for_nxt_cycle_s0_qds_1_q := m_pos_2_for_nxt_cycle_s0_qds_1_d
  }
  when(m_pos_2_for_nxt_cycle_s0_qds_2_en) {
    m_pos_2_for_nxt_cycle_s0_qds_2_q := m_pos_2_for_nxt_cycle_s0_qds_2_d
  }
  when(m_pos_2_for_nxt_cycle_s0_qds_3_en) {
    m_pos_2_for_nxt_cycle_s0_qds_3_q := m_pos_2_for_nxt_cycle_s0_qds_3_d
  }

}