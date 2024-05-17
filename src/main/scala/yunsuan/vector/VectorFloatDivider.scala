package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.vector.vfsqrt.fpsqrt_vector_r16

class VectorFloatDivider() extends Module {
  val QDS_ARCH: Int = 2
  val S1_SPECULATIVE_QDS: Int = 1
  val io = IO(new Bundle {
    val start_valid_i = Input(Bool())
    val start_ready_o = Output(Bool())
    val flush_i = Input(Bool())
    val fp_format_i = Input(UInt(2.W)) // b01->fp16,b10->fp32,b11->fp64
    val opa_i = Input(UInt(64.W)) // vs2
    val opb_i = Input(UInt(64.W)) // vs1
    val frs2_i = Input(UInt(64.W)) // f[rs2]
    val frs1_i = Input(UInt(64.W)) // f[rs1]
    val is_frs2_i = Input(Bool()) // if true, f[rs2] / vs1
    val is_frs1_i = Input(Bool()) // if true, vs2 / f[rs1]
    val is_sqrt_i = Input(Bool()) // must false, not support sqrt now
    val rm_i = Input(UInt(3.W))
    val is_vec_i = Input(Bool())
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())

    val finish_valid_o = Output(Bool())
    val finish_ready_i = Input(Bool())
    val fpdiv_res_o = Output(UInt(64.W))
    val fflags_o = Output(UInt(20.W))
  })
  val is_sqrt_i = io.is_sqrt_i
  val u_vector_float_sqrt_r16 = Module(new fpsqrt_vector_r16())
  u_vector_float_sqrt_r16.start_valid_i := is_sqrt_i & io.start_valid_i
  u_vector_float_sqrt_r16.flush_i := io.flush_i
  u_vector_float_sqrt_r16.fp_format_i := io.fp_format_i - 1.U
  u_vector_float_sqrt_r16.op_i := io.opa_i
  u_vector_float_sqrt_r16.rm_i := io.rm_i
  u_vector_float_sqrt_r16.vector_mode_i := io.is_vec_i
  u_vector_float_sqrt_r16.finish_ready_i := io.finish_ready_i
  u_vector_float_sqrt_r16.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  val u_vector_float_divider_r64 = Module(new VectorFloatDividerR64())
  u_vector_float_divider_r64.io.start_valid_i := !is_sqrt_i & io.start_valid_i
  u_vector_float_divider_r64.io.flush_i := io.flush_i
  u_vector_float_divider_r64.io.fp_format_i := io.fp_format_i
  u_vector_float_divider_r64.io.opa_i := io.opa_i
  u_vector_float_divider_r64.io.opb_i := io.opb_i
  u_vector_float_divider_r64.io.frs2_i := io.frs2_i
  u_vector_float_divider_r64.io.frs1_i := io.frs1_i
  u_vector_float_divider_r64.io.is_frs2_i := io.is_frs2_i
  u_vector_float_divider_r64.io.is_frs1_i := io.is_frs1_i
  u_vector_float_divider_r64.io.rm_i := io.rm_i
  u_vector_float_divider_r64.io.is_vec_i := io.is_vec_i
  u_vector_float_divider_r64.io.finish_ready_i := io.finish_ready_i
  u_vector_float_divider_r64.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  u_vector_float_divider_r64.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN

  io.start_ready_o := u_vector_float_divider_r64.io.start_ready_o & u_vector_float_sqrt_r16.start_ready_o
  io.finish_valid_o := u_vector_float_divider_r64.io.finish_valid_o | u_vector_float_sqrt_r16.finish_valid_o
  io.fpdiv_res_o := Mux1H(
    Seq(
      u_vector_float_divider_r64.io.finish_valid_o -> u_vector_float_divider_r64.io.fpdiv_res_o,
      u_vector_float_sqrt_r16.finish_valid_o -> u_vector_float_sqrt_r16.fpsqrt_res_o
    )
  )
  io.fflags_o := Mux1H(
    Seq(
      u_vector_float_divider_r64.io.finish_valid_o -> u_vector_float_divider_r64.io.fflags_o,
      u_vector_float_sqrt_r16.finish_valid_o -> u_vector_float_sqrt_r16.fflags_o
    )
  )
}

class VectorFloatDividerR64() extends Module {
  val QDS_ARCH: Int = 2
  val S1_SPECULATIVE_QDS: Int = 1
  val io = IO(new Bundle {
    val start_valid_i = Input(Bool())
    val start_ready_o = Output(Bool())
    val flush_i = Input(Bool())
    val fp_format_i = Input(UInt(2.W)) // b01->fp16,b10->fp32,b11->fp64
    val opa_i = Input(UInt(64.W)) // vs2
    val opb_i = Input(UInt(64.W)) // vs1
    val frs2_i = Input(UInt(64.W)) // f[rs2]
    val frs1_i = Input(UInt(64.W)) // f[rs1]
    val is_frs2_i = Input(Bool()) // if true, f[rs2] / vs1
    val is_frs1_i = Input(Bool()) // if true, vs2 / f[rs1]
    val rm_i = Input(UInt(3.W))
    val is_vec_i = Input(Bool())
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())

    val finish_valid_o = Output(Bool())
    val finish_ready_i = Input(Bool())
    val fpdiv_res_o = Output(UInt(64.W))
    val fflags_o = Output(UInt(20.W))

  })

  val fp_format_is_fp16 = io.fp_format_i === 1.U(2.W)
  val fp_format_is_fp32 = io.fp_format_i === 2.U(2.W)
  val fp_format_is_fp64 = io.fp_format_i === 3.U(2.W)
  val frs2 = Mux1H(
    Seq(
      fp_format_is_fp16,
      fp_format_is_fp32,
      fp_format_is_fp64
    ),
    Seq(
      Fill(4,io.frs2_i(15,0)),
      Fill(2,io.frs2_i(31,0)),
      Fill(1,io.frs2_i(63,0))
    )
  )
  val frs1 = Mux1H(
    Seq(
      fp_format_is_fp16,
      fp_format_is_fp32,
      fp_format_is_fp64
    ),
    Seq(
      Fill(4,io.frs1_i(15,0)),
      Fill(2,io.frs1_i(31,0)),
      Fill(1,io.frs1_i(63,0))
    )
  )
  val opa = Mux(io.is_frs2_i, frs2, io.opa_i)
  val opb = Mux(io.is_frs1_i, frs1, io.opb_i)
  val REM_W_f64_0 = 3 + 53 + 3 + 1
  val REM_W_f32_1 = 3 + 24 + 3 + 1
  val REM_W_f16_2 = 3 + 11 + 3 + 1
  val REM_W_f16_3 = 3 + 11 + 3 + 1

  val QUO_DIG_W = 5
  val QUO_DIG_NEG_2_BIT = 4
  val QUO_DIG_NEG_1_BIT = 3
  val QUO_DIG_NEG_0_BIT = 2
  val QUO_DIG_POS_1_BIT = 1
  val QUO_DIG_POS_2_BIT = 0

  val FP64_FRAC_W = 52 + 1
  val FP32_FRAC_W = 23 + 1
  val FP16_FRAC_W = 10 + 1

  val FP64_EXP_W = 11
  val FP32_EXP_W = 8
  val FP16_EXP_W = 5

  val FSM_W = 6
  val FSM_PRE_0 = (1 << 0).U(FSM_W.W)
  val FSM_PRE_1 = (1 << 1).U(FSM_W.W)
  val FSM_PRE_2 = (1 << 2).U(FSM_W.W)
  val FSM_ITER = (1 << 3).U(FSM_W.W)
  val FSM_POST_0 = (1 << 4).U(FSM_W.W)
  val FSM_POST_1 = (1 << 5).U(FSM_W.W)
  val FSM_PRE_0_BIT = 0
  val FSM_PRE_1_BIT = 1
  val FSM_PRE_2_BIT = 2
  val FSM_ITER_BIT = 3
  val FSM_POST_0_BIT = 4
  val FSM_POST_1_BIT = 5

  val R_SHIFT_NUM_LIMIT_f64_0 = 54.U(6.W)
  val R_SHIFT_NUM_LIMIT_f32_1 = 25.U(5.W)
  val R_SHIFT_NUM_LIMIT_f16_2 = 13.U(4.W)
  val R_SHIFT_NUM_LIMIT_f16_3 = 13.U(4.W)
  // TODO: use outside's rm coding
  val RM_RNE = "b000".U(3.W)
  val RM_RTZ = "b001".U(3.W)
  val RM_RDN = "b010".U(3.W)
  val RM_RUP = "b011".U(3.W)
  val RM_RMM = "b100".U(3.W)

  val fsm_d = Wire(UInt(FSM_W.W))
  val fsm_q = RegInit(FSM_PRE_0)
  val early_finish = Wire(Bool())

  val init_cycles = Wire(UInt(3.W))
  val opb_is_power_of_2_f64_0 = Wire(Bool())
  val opb_is_power_of_2_f32_1 = Wire(Bool())
  val opb_is_power_of_2_f16_2 = Wire(Bool())
  val opb_is_power_of_2_f16_3 = Wire(Bool())

  val opb_is_power_of_2_q = Reg(UInt(4.W))
  val res_is_denormal_f64_0 = Wire(Bool())
  val res_is_denormal_f32_1 = Wire(Bool())
  val res_is_denormal_f16_2 = Wire(Bool())
  val res_is_denormal_f16_3 = Wire(Bool())
  val final_iter = Wire(Bool())
  val is_vec_q = Reg(Bool())
  fsm_q := fsm_d

  when(io.flush_i) {
    fsm_d := FSM_PRE_0
  }.elsewhen(fsm_q === FSM_PRE_0) {
    fsm_d := Mux(
      io.start_valid_i,
      Mux(
        !io.is_vec_i & early_finish,
        FSM_POST_1,
        Mux(
          !io.is_vec_i & opb_is_power_of_2_f64_0,
          FSM_POST_0,
          Mux(!io.is_vec_i & init_cycles(1), FSM_PRE_2, FSM_PRE_1)
        )
      ),
      FSM_PRE_0
    )
  }.elsewhen(fsm_q === FSM_PRE_1) {
    fsm_d := FSM_PRE_2
  }.elsewhen(fsm_q === FSM_PRE_2) {
    fsm_d := FSM_ITER
  }.elsewhen(fsm_q === FSM_ITER) {
    fsm_d := Mux(final_iter, FSM_POST_0, FSM_ITER)
  }.elsewhen(fsm_q === FSM_POST_0) {
    fsm_d := Mux(is_vec_q | res_is_denormal_f64_0, FSM_POST_1, Mux(!is_vec_q & io.finish_ready_i, FSM_PRE_0, FSM_POST_0))
  }.elsewhen(fsm_q === FSM_POST_1) {
    fsm_d := Mux(io.finish_ready_i, FSM_PRE_0, FSM_POST_1)
  }.otherwise {
    fsm_d := FSM_PRE_0
  }
  io.start_ready_o := fsm_q(FSM_PRE_0_BIT)
  val start_handshaked = io.start_valid_i & io.start_ready_o
  io.finish_valid_o := fsm_q(FSM_POST_1_BIT) | (fsm_q(FSM_POST_0_BIT) & !is_vec_q & ~res_is_denormal_f64_0)

  val opa_sign_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opa(63),
      opa(31),
      opa(15)
    )
  )
  val opa_sign_f32_1 = Mux(fp_format_is_fp32, opa(63), opa(31))
  val opa_sign_f16_2 = opa(47)
  val opa_sign_f16_3 = opa(63)
  val opb_sign_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opb(63),
      opb(31),
      opb(15)
    )
  )
  val opb_sign_f32_1 = Mux(fp_format_is_fp32, opb(63), opb(31))
  val opb_sign_f16_2 = opb(47)
  val opb_sign_f16_3 = opb(63)
  val opa_exp_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opa(62, 52),
      Cat(0.U(3.W), opa(30, 23)),
      Cat(0.U(6.W), opa(14, 10))
    )
  )
  val opa_exp_f32_1 = Mux(fp_format_is_fp32, opa.head(32)(30, 23), Cat(0.U(3.W), opa(30, 26)))
  val opa_exp_f16_2 = opa(46, 42)
  val opa_exp_f16_3 = opa(62, 58)
  val opb_exp_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opb(62, 52),
      Cat(0.U(3.W), opb(30, 23)),
      Cat(0.U(6.W), opb(14, 10))
    )
  )
  val opb_exp_f32_1 = Mux(fp_format_is_fp32, opb.head(32)(30, 23), Cat(0.U(3.W), opb(30, 26)))
  val opb_exp_f16_2 = opb(46, 42)
  val opb_exp_f16_3 = opb(62, 58)
  val opa_exp_is_zero_f64_0 = opa_exp_f64_0 === 0.U(11.W)
  val opa_exp_is_zero_f32_1 = opa_exp_f32_1 === 0.U(8.W)
  val opa_exp_is_zero_f16_2 = opa_exp_f16_2 === 0.U(5.W)
  val opa_exp_is_zero_f16_3 = opa_exp_f16_3 === 0.U(5.W)
  val opb_exp_is_zero_f64_0 = opb_exp_f64_0 === 0.U(11.W)
  val opb_exp_is_zero_f32_1 = opb_exp_f32_1 === 0.U(8.W)
  val opb_exp_is_zero_f16_2 = opb_exp_f16_2 === 0.U(5.W)
  val opb_exp_is_zero_f16_3 = opb_exp_f16_3 === 0.U(5.W)

  init_cycles := Cat(
    io.is_vec_i | opa_exp_is_zero_f64_0 | opb_exp_is_zero_f64_0,
    !io.is_vec_i & !(opa_exp_is_zero_f64_0 | opb_exp_is_zero_f64_0),
    0.U(1.W)
  )

  val opa_exp_biased_f64_0 = opa_exp_f64_0 | opa_exp_is_zero_f64_0
  val opa_exp_biased_f32_1 = opa_exp_f32_1 | opa_exp_is_zero_f32_1
  val opa_exp_biased_f16_2 = opa_exp_f16_2 | opa_exp_is_zero_f16_2
  val opa_exp_biased_f16_3 = opa_exp_f16_3 | opa_exp_is_zero_f16_3
  val opb_exp_biased_f64_0 = opb_exp_f64_0 | opb_exp_is_zero_f64_0
  val opb_exp_biased_f32_1 = opb_exp_f32_1 | opb_exp_is_zero_f32_1
  val opb_exp_biased_f16_2 = opb_exp_f16_2 | opb_exp_is_zero_f16_2
  val opb_exp_biased_f16_3 = opb_exp_f16_3 | opb_exp_is_zero_f16_3
  val opa_exp_is_max_f64_0 = opa_exp_f64_0 === Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      2047.U(11.W),
      255.U(11.W),
      31.U(11.W)
    )
  )
  val opa_exp_is_max_f32_1 = opa_exp_f32_1 === Mux(fp_format_is_fp32, 255.U(8.W), 31.U(8.W))
  val opa_exp_is_max_f16_2 = opa_exp_f16_2 === 31.U(5.W)
  val opa_exp_is_max_f16_3 = opa_exp_f16_3 === 31.U(5.W)
  val opb_exp_is_max_f64_0 = opb_exp_f64_0 === Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      2047.U(11.W),
      255.U(11.W),
      31.U(11.W)
    )
  )
  val opb_exp_is_max_f32_1 = opb_exp_f32_1 === Mux(fp_format_is_fp32, 255.U(8.W), 31.U(8.W))
  val opb_exp_is_max_f16_2 = opb_exp_f16_2 === 31.U(5.W)
  val opb_exp_is_max_f16_3 = opb_exp_f16_3 === 31.U(5.W)
  val opa_frac_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opa(51, 0),
      Cat(opa(22, 0), 0.U(29.W)),
      Cat(opa(9, 0), 0.U(42.W))
    )
  )
  val opa_frac_f32_1 = Mux(fp_format_is_fp32, opa(54, 32), Cat(opa(25, 16), 0.U(13.W)))
  val opa_frac_f16_2 = opa(41, 32)
  val opa_frac_f16_3 = opa(57, 48)
  val opa_frac_is_zero_f64_0 = opa_frac_f64_0 === 0.U
  val opa_frac_is_zero_f32_1 = opa_frac_f32_1 === 0.U
  val opa_frac_is_zero_f16_2 = opa_frac_f16_2 === 0.U
  val opa_frac_is_zero_f16_3 = opa_frac_f16_3 === 0.U
  val opb_frac_f64_0 = Mux1H(
    Seq(
      fp_format_is_fp64,
      fp_format_is_fp32,
      fp_format_is_fp16
    ),
    Seq(
      opb(51, 0),
      Cat(opb(22, 0), 0.U(29.W)),
      Cat(opb(9, 0), 0.U(42.W))
    )
  )
  val opb_frac_f32_1 = Mux(fp_format_is_fp32, opb(54, 32), Cat(opb(25, 16), 0.U(13.W)))
  val opb_frac_f16_2 = opb(41, 32)
  val opb_frac_f16_3 = opb(57, 48)
  val opb_frac_is_zero_f64_0 = opb_frac_f64_0 === 0.U
  val opb_frac_is_zero_f32_1 = opb_frac_f32_1 === 0.U
  val opb_frac_is_zero_f16_2 = opb_frac_f16_2 === 0.U
  val opb_frac_is_zero_f16_3 = opb_frac_f16_3 === 0.U
  val opa_is_zero_f64_0 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_zero_f64_0 & opa_frac_is_zero_f64_0
  val opa_is_zero_f32_1 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_zero_f32_1 & opa_frac_is_zero_f32_1
  val opa_is_zero_f16_2 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_zero_f16_2 & opa_frac_is_zero_f16_2
  val opa_is_zero_f16_3 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_zero_f16_3 & opa_frac_is_zero_f16_3
  val opb_is_zero_f64_0 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_zero_f64_0 & opb_frac_is_zero_f64_0
  val opb_is_zero_f32_1 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_zero_f32_1 & opb_frac_is_zero_f32_1
  val opb_is_zero_f16_2 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_zero_f16_2 & opb_frac_is_zero_f16_2
  val opb_is_zero_f16_3 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_zero_f16_3 & opb_frac_is_zero_f16_3
  val opa_is_inf_f64_0 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f64_0 & opa_frac_is_zero_f64_0
  val opa_is_inf_f32_1 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f32_1 & opa_frac_is_zero_f32_1
  val opa_is_inf_f16_2 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f16_2 & opa_frac_is_zero_f16_2
  val opa_is_inf_f16_3 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f16_3 & opa_frac_is_zero_f16_3
  val opb_is_inf_f64_0 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f64_0 & opb_frac_is_zero_f64_0
  val opb_is_inf_f32_1 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f32_1 & opb_frac_is_zero_f32_1
  val opb_is_inf_f16_2 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f16_2 & opb_frac_is_zero_f16_2
  val opb_is_inf_f16_3 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f16_3 & opb_frac_is_zero_f16_3
  val opa_is_qnan_f64_0 = io.fp_aIsFpCanonicalNAN | opa_exp_is_max_f64_0 & opa_frac_f64_0.head(1).asBool
  val opa_is_qnan_f32_1 = io.fp_aIsFpCanonicalNAN | opa_exp_is_max_f32_1 & opa_frac_f32_1.head(1).asBool
  val opa_is_qnan_f16_2 = io.fp_aIsFpCanonicalNAN | opa_exp_is_max_f16_2 & opa_frac_f16_2.head(1).asBool
  val opa_is_qnan_f16_3 = io.fp_aIsFpCanonicalNAN | opa_exp_is_max_f16_3 & opa_frac_f16_3.head(1).asBool
  val opb_is_qnan_f64_0 = io.fp_bIsFpCanonicalNAN | opb_exp_is_max_f64_0 & opb_frac_f64_0.head(1).asBool
  val opb_is_qnan_f32_1 = io.fp_bIsFpCanonicalNAN | opb_exp_is_max_f32_1 & opb_frac_f32_1.head(1).asBool
  val opb_is_qnan_f16_2 = io.fp_bIsFpCanonicalNAN | opb_exp_is_max_f16_2 & opb_frac_f16_2.head(1).asBool
  val opb_is_qnan_f16_3 = io.fp_bIsFpCanonicalNAN | opb_exp_is_max_f16_3 & opb_frac_f16_3.head(1).asBool
  val opa_is_snan_f64_0 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f64_0 & !opa_frac_f64_0.head(1).asBool & !opa_frac_is_zero_f64_0
  val opa_is_snan_f32_1 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f32_1 & !opa_frac_f32_1.head(1).asBool & !opa_frac_is_zero_f32_1
  val opa_is_snan_f16_2 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f16_2 & !opa_frac_f16_2.head(1).asBool & !opa_frac_is_zero_f16_2
  val opa_is_snan_f16_3 = !io.fp_aIsFpCanonicalNAN & opa_exp_is_max_f16_3 & !opa_frac_f16_3.head(1).asBool & !opa_frac_is_zero_f16_3
  val opb_is_snan_f64_0 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f64_0 & !opb_frac_f64_0.head(1).asBool & !opb_frac_is_zero_f64_0
  val opb_is_snan_f32_1 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f32_1 & !opb_frac_f32_1.head(1).asBool & !opb_frac_is_zero_f32_1
  val opb_is_snan_f16_2 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f16_2 & !opb_frac_f16_2.head(1).asBool & !opb_frac_is_zero_f16_2
  val opb_is_snan_f16_3 = !io.fp_bIsFpCanonicalNAN & opb_exp_is_max_f16_3 & !opb_frac_f16_3.head(1).asBool & !opb_frac_is_zero_f16_3
  val opa_is_nan_f64_0 =  opa_is_qnan_f64_0 | opa_is_snan_f64_0
  val opa_is_nan_f32_1 =  opa_is_qnan_f32_1 | opa_is_snan_f32_1
  val opa_is_nan_f16_2 =  opa_is_qnan_f16_2 | opa_is_snan_f16_2
  val opa_is_nan_f16_3 =  opa_is_qnan_f16_3 | opa_is_snan_f16_3
  val opb_is_nan_f64_0 =  opb_is_qnan_f64_0 | opb_is_snan_f64_0
  val opb_is_nan_f32_1 =  opb_is_qnan_f32_1 | opb_is_snan_f32_1
  val opb_is_nan_f16_2 =  opb_is_qnan_f16_2 | opb_is_snan_f16_2
  val opb_is_nan_f16_3 =  opb_is_qnan_f16_3 | opb_is_snan_f16_3
  val op_invalid_f64_0 = (opa_is_inf_f64_0 & opb_is_inf_f64_0) | (opa_is_zero_f64_0 & opb_is_zero_f64_0) | opa_is_snan_f64_0 | opb_is_snan_f64_0
  val op_invalid_f32_1 = (opa_is_inf_f32_1 & opb_is_inf_f32_1) | (opa_is_zero_f32_1 & opb_is_zero_f32_1) | opa_is_snan_f32_1 | opb_is_snan_f32_1
  val op_invalid_f16_2 = (opa_is_inf_f16_2 & opb_is_inf_f16_2) | (opa_is_zero_f16_2 & opb_is_zero_f16_2) | opa_is_snan_f16_2 | opb_is_snan_f16_2
  val op_invalid_f16_3 = (opa_is_inf_f16_3 & opb_is_inf_f16_3) | (opa_is_zero_f16_3 & opb_is_zero_f16_3) | opa_is_snan_f16_3 | opb_is_snan_f16_3
  val res_is_nan_f64_0 = opa_is_nan_f64_0 | opb_is_nan_f64_0 | op_invalid_f64_0
  val res_is_nan_f32_1 = opa_is_nan_f32_1 | opb_is_nan_f32_1 | op_invalid_f32_1
  val res_is_nan_f16_2 = opa_is_nan_f16_2 | opb_is_nan_f16_2 | op_invalid_f16_2
  val res_is_nan_f16_3 = opa_is_nan_f16_3 | opb_is_nan_f16_3 | op_invalid_f16_3
  val res_is_inf_f64_0 = opa_is_inf_f64_0 | opb_is_zero_f64_0
  val res_is_inf_f32_1 = opa_is_inf_f32_1 | opb_is_zero_f32_1
  val res_is_inf_f16_2 = opa_is_inf_f16_2 | opb_is_zero_f16_2
  val res_is_inf_f16_3 = opa_is_inf_f16_3 | opb_is_zero_f16_3
  val res_is_exact_zero_f64_0 = opa_is_zero_f64_0 | opb_is_inf_f64_0
  val res_is_exact_zero_f32_1 = opa_is_zero_f32_1 | opb_is_inf_f32_1
  val res_is_exact_zero_f16_2 = opa_is_zero_f16_2 | opb_is_inf_f16_2
  val res_is_exact_zero_f16_3 = opa_is_zero_f16_3 | opb_is_inf_f16_3
  opb_is_power_of_2_f64_0 := opb_frac_is_zero_f64_0
  opb_is_power_of_2_f32_1 := opb_frac_is_zero_f32_1
  opb_is_power_of_2_f16_2 := opb_frac_is_zero_f16_2
  opb_is_power_of_2_f16_3 := opb_frac_is_zero_f16_3

  val divided_by_zero_f64_0 = !res_is_nan_f64_0 & !opa_is_inf_f64_0 & opb_is_zero_f64_0
  val divided_by_zero_f32_1 = !res_is_nan_f32_1 & !opa_is_inf_f32_1 & opb_is_zero_f32_1
  val divided_by_zero_f16_2 = !res_is_nan_f16_2 & !opa_is_inf_f16_2 & opb_is_zero_f16_2
  val divided_by_zero_f16_3 = !res_is_nan_f16_3 & !opa_is_inf_f16_3 & opb_is_zero_f16_3

  val opa_exp_plus_biased_f64_0 = Cat(0.U(1.W), opa_exp_biased_f64_0(10, 0)) + Mux(fp_format_is_fp16, 15.U(12.W), Mux(fp_format_is_fp32, 127.U(12.W), 1023.U(12.W)))
  val opa_exp_plus_biased_f32_1 = Cat(0.U(1.W), opa_exp_biased_f32_1(7, 0)) + Mux(fp_format_is_fp16, 15.U(7.W), 127.U(7.W))
  val opa_exp_plus_biased_f16_2 = Cat(0.U(1.W), opa_exp_biased_f16_2(4, 0)) + 15.U(5.W)
  val opa_exp_plus_biased_f16_3 = Cat(0.U(1.W), opa_exp_biased_f16_3(4, 0)) + 15.U(5.W)


  val opa_l_shift_num_f64_0 = Wire(UInt(FP64_FRAC_W.U.getWidth.W))
  val opa_l_shift_num_f32_1 = Wire(UInt(FP32_FRAC_W.U.getWidth.W))
  val opa_l_shift_num_f16_2 = Wire(UInt(FP16_FRAC_W.U.getWidth.W))
  val opa_l_shift_num_f16_3 = Wire(UInt(FP16_FRAC_W.U.getWidth.W))
  val opb_l_shift_num_f64_0 = Wire(UInt(FP64_FRAC_W.U.getWidth.W))
  val opb_l_shift_num_f32_1 = Wire(UInt(FP32_FRAC_W.U.getWidth.W))
  val opb_l_shift_num_f16_2 = Wire(UInt(FP16_FRAC_W.U.getWidth.W))
  val opb_l_shift_num_f16_3 = Wire(UInt(FP16_FRAC_W.U.getWidth.W))

  val op_exp_diff_f64_0 =
    Cat(0.U(1.W), opa_exp_plus_biased_f64_0) -
      Cat(0.U(7.W), opa_l_shift_num_f64_0) -
      Cat(0.U(2.W), opb_exp_biased_f64_0) +
      Cat(0.U(7.W), opb_l_shift_num_f64_0)

  val op_exp_diff_f32_1 =
    Cat(0.U(1.W), opa_exp_plus_biased_f32_1) -
      Cat(0.U(5.W), opa_l_shift_num_f32_1) -
      Cat(0.U(2.W), opb_exp_biased_f32_1) +
      Cat(0.U(5.W), opb_l_shift_num_f32_1)

  val op_exp_diff_f16_2 =
    Cat(0.U(1.W), opa_exp_plus_biased_f16_2) -
      Cat(0.U(3.W), opa_l_shift_num_f16_2) -
      Cat(0.U(2.W), opb_exp_biased_f16_2) +
      Cat(0.U(3.W), opb_l_shift_num_f16_2)
  val op_exp_diff_f16_3 =
    Cat(0.U(1.W), opa_exp_plus_biased_f16_3) -
      Cat(0.U(3.W), opa_l_shift_num_f16_3) -
      Cat(0.U(2.W), opb_exp_biased_f16_3) +
      Cat(0.U(3.W), opb_l_shift_num_f16_3)

  val out_sign_d = Cat(
    Mux(res_is_nan_f16_3, 0.U(1.W), opa_sign_f16_3 ^ opb_sign_f16_3),
    Mux(res_is_nan_f16_2, 0.U(1.W), opa_sign_f16_2 ^ opb_sign_f16_2),
    Mux(res_is_nan_f32_1, 0.U(1.W), opa_sign_f32_1 ^ opb_sign_f32_1),
    Mux(res_is_nan_f64_0, 0.U(1.W), opa_sign_f64_0 ^ opb_sign_f64_0)
  )
  val fp_format_onehot_d = Cat(fp_format_is_fp64, fp_format_is_fp32, fp_format_is_fp16)
  val is_vec_d = io.is_vec_i
  val rm_d = io.rm_i
  val res_is_nan_d = Cat(res_is_nan_f16_3, res_is_nan_f16_2, res_is_nan_f32_1, res_is_nan_f64_0)
  val res_is_inf_d = Cat(res_is_inf_f16_3, res_is_inf_f16_2, res_is_inf_f32_1, res_is_inf_f64_0)
  val res_is_exact_zero_d = Cat(
    res_is_exact_zero_f16_3,
    res_is_exact_zero_f16_2,
    res_is_exact_zero_f32_1,
    res_is_exact_zero_f64_0
  )
  val opb_is_power_of_2_d = Cat(
    opb_is_power_of_2_f16_3,
    opb_is_power_of_2_f16_2,
    opb_is_power_of_2_f32_1,
    opb_is_power_of_2_f64_0
  )
  val op_invalid_div_d = Cat(
    op_invalid_f16_3,
    op_invalid_f16_2,
    op_invalid_f32_1,
    op_invalid_f64_0
  )
  val divided_by_zero_d = Cat(
    divided_by_zero_f16_3,
    divided_by_zero_f16_2,
    divided_by_zero_f32_1,
    divided_by_zero_f64_0
  )
  val fp_format_onehot_q = Reg(UInt(3.W))
  val fp_format_q_is_fp64 = fp_format_onehot_q(2)
  val fp_format_q_is_fp32 = fp_format_onehot_q(1)
  val fp_format_q_is_fp16 = fp_format_onehot_q(0)
  val rm_q = Reg(UInt(3.W))
  val out_sign_q = Reg(UInt(4.W))
  val res_is_nan_q = Reg(UInt(4.W))
  val res_is_inf_q = Reg(UInt(4.W))
  val res_is_exact_zero_q = Reg(UInt(4.W))
  val op_invalid_div_q = Reg(UInt(4.W))
  val divided_by_zero_q = Reg(UInt(4.W))
  val overflow_d = Wire(UInt(4.W))
  val overflow_q = Reg(UInt(4.W))
  overflow_q := overflow_d
  when(start_handshaked) {
    out_sign_q := out_sign_d
    fp_format_onehot_q := fp_format_onehot_d
    rm_q := rm_d
    res_is_nan_q := res_is_nan_d
    res_is_inf_q := res_is_inf_d
    res_is_exact_zero_q := res_is_exact_zero_d
    opb_is_power_of_2_q := opb_is_power_of_2_d
    op_invalid_div_q := op_invalid_div_d
    divided_by_zero_q := divided_by_zero_d
    is_vec_q := is_vec_d
  }
  early_finish := res_is_nan_f64_0 | res_is_inf_f64_0 | res_is_exact_zero_f64_0

  val out_exp_diff_q_f64_0 = Reg(UInt(13.W))
  val out_exp_diff_q_f32_1 = Reg(UInt(10.W))
  val out_exp_diff_q_f16_2 = Reg(UInt(7.W))
  val out_exp_diff_q_f16_3 = Reg(UInt(7.W))
  res_is_denormal_f64_0 := (out_exp_diff_q_f64_0(11, 0) === 0.U) | out_exp_diff_q_f64_0(12)
  res_is_denormal_f32_1 := (out_exp_diff_q_f32_1(8, 0) === 0.U) | out_exp_diff_q_f32_1(9)
  res_is_denormal_f16_2 := (out_exp_diff_q_f16_2(5, 0) === 0.U) | out_exp_diff_q_f16_2(6)
  res_is_denormal_f16_3 := (out_exp_diff_q_f16_3(5, 0) === 0.U) | out_exp_diff_q_f16_3(6)


  val out_exp_diff_en = start_handshaked | fsm_q(FSM_PRE_2_BIT)
  val iter_num_q = Reg(UInt(4.W))


  val out_exp_diff_d_f64_0 = Mux(
    fsm_q(FSM_PRE_0_BIT),
    op_exp_diff_f64_0,
    out_exp_diff_q_f64_0 - iter_num_q(0)
  )
  val out_exp_diff_d_f32_1 = Mux(
    fsm_q(FSM_PRE_0_BIT),
    op_exp_diff_f32_1,
    out_exp_diff_q_f32_1 - iter_num_q(1)
  )
  val out_exp_diff_d_f16_2 = Mux(
    fsm_q(FSM_PRE_0_BIT),
    op_exp_diff_f16_2,
    out_exp_diff_q_f16_2 - iter_num_q(2)
  )
  val out_exp_diff_d_f16_3 = Mux(
    fsm_q(FSM_PRE_0_BIT),
    op_exp_diff_f16_3,
    out_exp_diff_q_f16_3 - iter_num_q(3)
  )
  when(out_exp_diff_en) {
    out_exp_diff_q_f64_0 := out_exp_diff_d_f64_0
    out_exp_diff_q_f32_1 := out_exp_diff_d_f32_1
    out_exp_diff_q_f16_2 := out_exp_diff_d_f16_2
    out_exp_diff_q_f16_3 := out_exp_diff_d_f16_3
  }
  val opa_frac_pre_shifted_f64_0 = Cat(0.U(1.W), opa_frac_f64_0)
  val opb_frac_pre_shifted_f64_0 = Cat(0.U(1.W), opb_frac_f64_0)
  val U_Left_Shift_opa_f64_0 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP64_FRAC_W, priorityShiftValueW = FP64_FRAC_W))
  U_Left_Shift_opa_f64_0.io.src := opa_frac_pre_shifted_f64_0
  U_Left_Shift_opa_f64_0.io.priority_shiftValue := opa_frac_pre_shifted_f64_0
  opa_l_shift_num_f64_0 := Fill(6, opa_exp_is_zero_f64_0) & U_Left_Shift_opa_f64_0.io.lzd_result

  val opa_frac_l_shifted_f64_0 = Cat(1.U(1.W), Mux(opa_exp_is_zero_f64_0, U_Left_Shift_opa_f64_0.io.lshift_result.tail(1), opa_frac_f64_0))
  val U_Left_Shift_opb_f64_0 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP64_FRAC_W, priorityShiftValueW = FP64_FRAC_W))
  U_Left_Shift_opb_f64_0.io.src := opb_frac_pre_shifted_f64_0
  U_Left_Shift_opb_f64_0.io.priority_shiftValue := opb_frac_pre_shifted_f64_0
  opb_l_shift_num_f64_0 := Fill(6, opb_exp_is_zero_f64_0) & U_Left_Shift_opb_f64_0.io.lzd_result
  val opb_frac_l_shifted_f64_0 = Cat(1.U(1.W), Mux(opb_exp_is_zero_f64_0, U_Left_Shift_opb_f64_0.io.lshift_result.tail(1), opb_frac_f64_0))

  val opa_frac_pre_shifted_f32_1 = Cat(0.U(1.W), opa_frac_f32_1)
  val opb_frac_pre_shifted_f32_1 = Cat(0.U(1.W), opb_frac_f32_1)
  val U_Left_Shift_opa_f32_1 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP32_FRAC_W, priorityShiftValueW = FP32_FRAC_W))
  U_Left_Shift_opa_f32_1.io.src := opa_frac_pre_shifted_f32_1
  U_Left_Shift_opa_f32_1.io.priority_shiftValue := opa_frac_pre_shifted_f32_1
  opa_l_shift_num_f32_1 := Fill(5, opa_exp_is_zero_f32_1) & U_Left_Shift_opa_f32_1.io.lzd_result
  val opa_frac_l_shifted_f32_1 = Cat(1.U(1.W), Mux(opa_exp_is_zero_f32_1, U_Left_Shift_opa_f32_1.io.lshift_result.tail(1), opa_frac_f32_1))
  val U_Left_Shift_opb_f32_1 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP32_FRAC_W, priorityShiftValueW = FP32_FRAC_W))
  U_Left_Shift_opb_f32_1.io.src := opb_frac_pre_shifted_f32_1
  U_Left_Shift_opb_f32_1.io.priority_shiftValue := opb_frac_pre_shifted_f32_1
  opb_l_shift_num_f32_1 := Fill(5, opb_exp_is_zero_f32_1) & U_Left_Shift_opb_f32_1.io.lzd_result
  val opb_frac_l_shifted_f32_1 = Cat(1.U(1.W), Mux(opb_exp_is_zero_f32_1, U_Left_Shift_opb_f32_1.io.lshift_result.tail(1), opb_frac_f32_1))

  val opa_frac_pre_shifted_f16_2 = Cat(0.U(1.W), opa_frac_f16_2)
  val opb_frac_pre_shifted_f16_2 = Cat(0.U(1.W), opb_frac_f16_2)
  val U_Left_Shift_opa_f16_2 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP16_FRAC_W, priorityShiftValueW = FP16_FRAC_W))
  U_Left_Shift_opa_f16_2.io.src := opa_frac_pre_shifted_f16_2
  U_Left_Shift_opa_f16_2.io.priority_shiftValue := opa_frac_pre_shifted_f16_2
  opa_l_shift_num_f16_2 := Fill(5, opa_exp_is_zero_f16_2) & U_Left_Shift_opa_f16_2.io.lzd_result
  val opa_frac_l_shifted_f16_2 = Cat(1.U(1.W), Mux(opa_exp_is_zero_f16_2, U_Left_Shift_opa_f16_2.io.lshift_result.tail(1), opa_frac_f16_2))
  val U_Left_Shift_opb_f16_2 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP16_FRAC_W, priorityShiftValueW = FP16_FRAC_W))
  U_Left_Shift_opb_f16_2.io.src := opb_frac_pre_shifted_f16_2
  U_Left_Shift_opb_f16_2.io.priority_shiftValue := opb_frac_pre_shifted_f16_2
  opb_l_shift_num_f16_2 := Fill(5, opb_exp_is_zero_f16_2) & U_Left_Shift_opb_f16_2.io.lzd_result
  val opb_frac_l_shifted_f16_2 = Cat(1.U(1.W), Mux(opb_exp_is_zero_f16_2, U_Left_Shift_opb_f16_2.io.lshift_result.tail(1), opb_frac_f16_2))

  val opa_frac_pre_shifted_f16_3 = Cat(0.U(1.W), opa_frac_f16_3)
  val opb_frac_pre_shifted_f16_3 = Cat(0.U(1.W), opb_frac_f16_3)
  val U_Left_Shift_opa_f16_3 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP16_FRAC_W, priorityShiftValueW = FP16_FRAC_W))
  U_Left_Shift_opa_f16_3.io.src := opa_frac_pre_shifted_f16_3
  U_Left_Shift_opa_f16_3.io.priority_shiftValue := opa_frac_pre_shifted_f16_3
  opa_l_shift_num_f16_3 := Fill(5, opa_exp_is_zero_f16_3) & U_Left_Shift_opa_f16_3.io.lzd_result
  val opa_frac_l_shifted_f16_3 = Cat(1.U(1.W), Mux(opa_exp_is_zero_f16_3, U_Left_Shift_opa_f16_3.io.lshift_result.tail(1), opa_frac_f16_3))
  val U_Left_Shift_opb_f16_3 = Module(new ShiftLeftPriorityWithLZDResult(srcW = FP16_FRAC_W, priorityShiftValueW = FP16_FRAC_W))
  U_Left_Shift_opb_f16_3.io.src := opb_frac_pre_shifted_f16_3
  U_Left_Shift_opb_f16_3.io.priority_shiftValue := opb_frac_pre_shifted_f16_3
  opb_l_shift_num_f16_3 := Fill(5, opb_exp_is_zero_f16_3) & U_Left_Shift_opb_f16_3.io.lzd_result
  val opb_frac_l_shifted_f16_3 = Cat(1.U(1.W), Mux(opb_exp_is_zero_f16_3, U_Left_Shift_opb_f16_3.io.lshift_result.tail(1), opb_frac_f16_3))

  val quo_iter_q_f64_0 = Reg(UInt((FP64_FRAC_W + 1).W))
  val quo_iter_q_f32_1 = Reg(UInt((FP32_FRAC_W + 1).W))
  val quo_iter_q_f16_2 = Reg(UInt((FP16_FRAC_W + 1).W))
  val quo_iter_q_f16_3 = Reg(UInt((FP16_FRAC_W + 1).W))
  val quo_m1_iter_q_f64_0 = Reg(UInt((FP64_FRAC_W + 1).W))
  val quo_m1_iter_q_f32_1 = Reg(UInt((FP32_FRAC_W + 1).W))
  val quo_m1_iter_q_f16_2 = Reg(UInt((FP16_FRAC_W + 1).W))
  val quo_m1_iter_q_f16_3 = Reg(UInt((FP16_FRAC_W + 1).W))
  val frac_divisor_q_60bit = Reg(UInt(60.W))
  val frac_divisor_q_f64_0 = Mux1H(
    Seq(
      fp_format_q_is_fp64 | !is_vec_q,
      fp_format_q_is_fp32 & is_vec_q,
      fp_format_q_is_fp16 & is_vec_q
    ),
    Seq(
      frac_divisor_q_60bit.head(57),
      Cat(frac_divisor_q_60bit(29, 2), 0.U(29.W)),
      Cat(frac_divisor_q_60bit(14, 0), 0.U(42.W))
    )
  )
  val frac_divisor_q_f32_1 = Mux(fp_format_q_is_fp32 & is_vec_q, frac_divisor_q_60bit.head(28), Cat(frac_divisor_q_60bit(29, 15), 0.U(13.W)))
  val frac_divisor_q_f16_2 = frac_divisor_q_60bit(44, 30)
  val frac_divisor_q_f16_3 = frac_divisor_q_60bit(59, 45)


  val a_frac_lt_b_frac_f64_0 = opa_frac_f64_0 < opb_frac_f64_0
  val a_frac_lt_b_frac_f32_1 = opa_frac_f32_1 < opb_frac_f32_1
  val a_frac_lt_b_frac_f16_2 = opa_frac_f16_2 < opb_frac_f16_2
  val a_frac_lt_b_frac_f16_3 = opa_frac_f16_3 < opb_frac_f16_3

  val a_frac_lt_b_frac_l_shifted_f64_0 = quo_iter_q_f64_0(51, 0) < quo_m1_iter_q_f64_0(51, 0)
  val a_frac_lt_b_frac_l_shifted_f32_1 = quo_iter_q_f32_1(22, 0) < quo_m1_iter_q_f32_1(22, 0)
  val a_frac_lt_b_frac_l_shifted_f16_2 = quo_iter_q_f16_2(9, 0) < quo_m1_iter_q_f16_2(9, 0)
  val a_frac_lt_b_frac_l_shifted_f16_3 = quo_iter_q_f16_3(9, 0) < quo_m1_iter_q_f16_3(9, 0)


  val prescaled_a_frac_f64_0 = Cat(1.U(1.W), Mux(fsm_q(FSM_PRE_0_BIT), opa_frac_f64_0, quo_iter_q_f64_0(51, 0)))
  val prescaled_a_frac_f32_1 = Cat(1.U(1.W), quo_iter_q_f32_1(22, 0))
  val prescaled_a_frac_f16_2 = Cat(1.U(1.W), quo_iter_q_f16_2(9, 0))
  val prescaled_a_frac_f16_3 = Cat(1.U(1.W), quo_iter_q_f16_3(9, 0))
  val prescaled_b_frac_f64_0 = Cat(1.U(1.W), Mux(fsm_q(FSM_PRE_0_BIT), opb_frac_f64_0, quo_m1_iter_q_f64_0(51, 0)))
  val prescaled_b_frac_f32_1 = Cat(1.U(1.W), quo_m1_iter_q_f32_1(22, 0))
  val prescaled_b_frac_f16_2 = Cat(1.U(1.W), quo_m1_iter_q_f16_2(9, 0))
  val prescaled_b_frac_f16_3 = Cat(1.U(1.W), quo_m1_iter_q_f16_3(9, 0))
  val scale_factor_idx_f64_0 = prescaled_b_frac_f64_0(51, 49)
  val scale_factor_idx_f32_1 = prescaled_b_frac_f32_1(22, 20)
  val scale_factor_idx_f16_2 = prescaled_b_frac_f16_2(9, 7)
  val scale_factor_idx_f16_3 = prescaled_b_frac_f16_3(9, 7)

  val scale_adder_opa_in_f64_0 = Wire(Vec(3, UInt(57.W)))
  val scale_adder_opa_in_f32_1 = Wire(Vec(3, UInt(28.W)))
  val scale_adder_opa_in_f16_2 = Wire(Vec(3, UInt(15.W)))
  val scale_adder_opa_in_f16_3 = Wire(Vec(3, UInt(15.W)))
  scale_adder_opa_in_f64_0(0) := Cat(0.U(1.W), prescaled_a_frac_f64_0, 0.U(3.W))
  scale_adder_opa_in_f32_1(0) := Cat(0.U(1.W), prescaled_a_frac_f32_1, 0.U(3.W))
  scale_adder_opa_in_f16_2(0) := Cat(0.U(1.W), prescaled_a_frac_f16_2, 0.U(3.W))
  scale_adder_opa_in_f16_3(0) := Cat(0.U(1.W), prescaled_a_frac_f16_3, 0.U(3.W))
  scale_adder_opa_in_f64_0(1) := Mux1H(
    Seq(
      scale_factor_idx_f64_0 === 0.U(3.W),
      scale_factor_idx_f64_0 === 1.U(3.W),
      scale_factor_idx_f64_0 === 2.U(3.W),
      scale_factor_idx_f64_0 === 3.U(3.W),
      scale_factor_idx_f64_0 === 4.U(3.W),
      scale_factor_idx_f64_0 === 5.U(3.W),
      scale_factor_idx_f64_0 === 6.U(3.W),
      scale_factor_idx_f64_0 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f64_0, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f64_0, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_a_frac_f64_0, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f64_0, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f64_0, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_a_frac_f64_0, 0.U(1.W)),
      0.U(57.W),
      0.U(57.W),
    )
  )
  scale_adder_opa_in_f32_1(1) := Mux1H(
    Seq(
      scale_factor_idx_f32_1 === 0.U(3.W),
      scale_factor_idx_f32_1 === 1.U(3.W),
      scale_factor_idx_f32_1 === 2.U(3.W),
      scale_factor_idx_f32_1 === 3.U(3.W),
      scale_factor_idx_f32_1 === 4.U(3.W),
      scale_factor_idx_f32_1 === 5.U(3.W),
      scale_factor_idx_f32_1 === 6.U(3.W),
      scale_factor_idx_f32_1 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f32_1, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f32_1, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_a_frac_f32_1, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f32_1, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f32_1, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_a_frac_f32_1, 0.U(1.W)),
      0.U(28.W),
      0.U(28.W),
    )
  )
  scale_adder_opa_in_f16_2(1) := Mux1H(
    Seq(
      scale_factor_idx_f16_2 === 0.U(3.W),
      scale_factor_idx_f16_2 === 1.U(3.W),
      scale_factor_idx_f16_2 === 2.U(3.W),
      scale_factor_idx_f16_2 === 3.U(3.W),
      scale_factor_idx_f16_2 === 4.U(3.W),
      scale_factor_idx_f16_2 === 5.U(3.W),
      scale_factor_idx_f16_2 === 6.U(3.W),
      scale_factor_idx_f16_2 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f16_2, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_2, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_2, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_2, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_2, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_2, 0.U(1.W)),
      0.U(15.W),
      0.U(15.W),
    )
  )
  scale_adder_opa_in_f16_3(1) := Mux1H(
    Seq(
      scale_factor_idx_f16_3 === 0.U(3.W),
      scale_factor_idx_f16_3 === 1.U(3.W),
      scale_factor_idx_f16_3 === 2.U(3.W),
      scale_factor_idx_f16_3 === 3.U(3.W),
      scale_factor_idx_f16_3 === 4.U(3.W),
      scale_factor_idx_f16_3 === 5.U(3.W),
      scale_factor_idx_f16_3 === 6.U(3.W),
      scale_factor_idx_f16_3 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f16_3, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_3, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_3, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_3, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_3, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_a_frac_f16_3, 0.U(1.W)),
      0.U(15.W),
      0.U(15.W),
    )
  )
  scale_adder_opa_in_f64_0(2) := Mux1H(
    Seq(
      scale_factor_idx_f64_0 === 0.U(3.W),
      scale_factor_idx_f64_0 === 1.U(3.W),
      scale_factor_idx_f64_0 === 2.U(3.W),
      scale_factor_idx_f64_0 === 3.U(3.W),
      scale_factor_idx_f64_0 === 4.U(3.W),
      scale_factor_idx_f64_0 === 5.U(3.W),
      scale_factor_idx_f64_0 === 6.U(3.W),
      scale_factor_idx_f64_0 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f64_0, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f64_0, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_a_frac_f64_0),
      0.U(57.W),
      Cat(0.U(4.W), prescaled_a_frac_f64_0),
      0.U(57.W),
      Cat(0.U(4.W), prescaled_a_frac_f64_0),
      Cat(0.U(4.W), prescaled_a_frac_f64_0),
    )
  )
  scale_adder_opa_in_f32_1(2) := Mux1H(
    Seq(
      scale_factor_idx_f32_1 === 0.U(3.W),
      scale_factor_idx_f32_1 === 1.U(3.W),
      scale_factor_idx_f32_1 === 2.U(3.W),
      scale_factor_idx_f32_1 === 3.U(3.W),
      scale_factor_idx_f32_1 === 4.U(3.W),
      scale_factor_idx_f32_1 === 5.U(3.W),
      scale_factor_idx_f32_1 === 6.U(3.W),
      scale_factor_idx_f32_1 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f32_1, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f32_1, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_a_frac_f32_1),
      0.U(28.W),
      Cat(0.U(4.W), prescaled_a_frac_f32_1),
      0.U(28.W),
      Cat(0.U(4.W), prescaled_a_frac_f32_1),
      Cat(0.U(4.W), prescaled_a_frac_f32_1),
    )
  )
  scale_adder_opa_in_f16_2(2) := Mux1H(
    Seq(
      scale_factor_idx_f16_2 === 0.U(3.W),
      scale_factor_idx_f16_2 === 1.U(3.W),
      scale_factor_idx_f16_2 === 2.U(3.W),
      scale_factor_idx_f16_2 === 3.U(3.W),
      scale_factor_idx_f16_2 === 4.U(3.W),
      scale_factor_idx_f16_2 === 5.U(3.W),
      scale_factor_idx_f16_2 === 6.U(3.W),
      scale_factor_idx_f16_2 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f16_2, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_2, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_a_frac_f16_2),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_a_frac_f16_2),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_a_frac_f16_2),
      Cat(0.U(4.W), prescaled_a_frac_f16_2),
    )
  )
  scale_adder_opa_in_f16_3(2) := Mux1H(
    Seq(
      scale_factor_idx_f16_3 === 0.U(3.W),
      scale_factor_idx_f16_3 === 1.U(3.W),
      scale_factor_idx_f16_3 === 2.U(3.W),
      scale_factor_idx_f16_3 === 3.U(3.W),
      scale_factor_idx_f16_3 === 4.U(3.W),
      scale_factor_idx_f16_3 === 5.U(3.W),
      scale_factor_idx_f16_3 === 6.U(3.W),
      scale_factor_idx_f16_3 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_a_frac_f16_3, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_a_frac_f16_3, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_a_frac_f16_3),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_a_frac_f16_3),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_a_frac_f16_3),
      Cat(0.U(4.W), prescaled_a_frac_f16_3),
    )
  )
  val scale_adder_opa_f64_0 = scale_adder_opa_in_f64_0(0) + scale_adder_opa_in_f64_0(1) + scale_adder_opa_in_f64_0(2)
  val scale_adder_opa_f32_1 = scale_adder_opa_in_f32_1(0) + scale_adder_opa_in_f32_1(1) + scale_adder_opa_in_f32_1(2)
  val scale_adder_opa_f16_2 = scale_adder_opa_in_f16_2(0) + scale_adder_opa_in_f16_2(1) + scale_adder_opa_in_f16_2(2)
  val scale_adder_opa_f16_3 = scale_adder_opa_in_f16_3(0) + scale_adder_opa_in_f16_3(1) + scale_adder_opa_in_f16_3(2)

  val scale_adder_opb_in_f64_0 = Wire(Vec(3, UInt(57.W)))
  val scale_adder_opb_in_f32_1 = Wire(Vec(3, UInt(28.W)))
  val scale_adder_opb_in_f16_2 = Wire(Vec(3, UInt(15.W)))
  val scale_adder_opb_in_f16_3 = Wire(Vec(3, UInt(15.W)))
  scale_adder_opb_in_f64_0(0) := Cat(0.U(1.W), prescaled_b_frac_f64_0, 0.U(3.W))
  scale_adder_opb_in_f32_1(0) := Cat(0.U(1.W), prescaled_b_frac_f32_1, 0.U(3.W))
  scale_adder_opb_in_f16_2(0) := Cat(0.U(1.W), prescaled_b_frac_f16_2, 0.U(3.W))
  scale_adder_opb_in_f16_3(0) := Cat(0.U(1.W), prescaled_b_frac_f16_3, 0.U(3.W))
  scale_adder_opb_in_f64_0(1) := Mux1H(
    Seq(
      scale_factor_idx_f64_0 === 0.U(3.W),
      scale_factor_idx_f64_0 === 1.U(3.W),
      scale_factor_idx_f64_0 === 2.U(3.W),
      scale_factor_idx_f64_0 === 3.U(3.W),
      scale_factor_idx_f64_0 === 4.U(3.W),
      scale_factor_idx_f64_0 === 5.U(3.W),
      scale_factor_idx_f64_0 === 6.U(3.W),
      scale_factor_idx_f64_0 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f64_0, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f64_0, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_b_frac_f64_0, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f64_0, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f64_0, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_b_frac_f64_0, 0.U(1.W)),
      0.U(57.W),
      0.U(57.W),
    )
  )
  scale_adder_opb_in_f32_1(1) := Mux1H(
    Seq(
      scale_factor_idx_f32_1 === 0.U(3.W),
      scale_factor_idx_f32_1 === 1.U(3.W),
      scale_factor_idx_f32_1 === 2.U(3.W),
      scale_factor_idx_f32_1 === 3.U(3.W),
      scale_factor_idx_f32_1 === 4.U(3.W),
      scale_factor_idx_f32_1 === 5.U(3.W),
      scale_factor_idx_f32_1 === 6.U(3.W),
      scale_factor_idx_f32_1 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f32_1, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f32_1, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_b_frac_f32_1, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f32_1, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f32_1, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_b_frac_f32_1, 0.U(1.W)),
      0.U(28.W),
      0.U(28.W),
    )
  )
  scale_adder_opb_in_f16_2(1) := Mux1H(
    Seq(
      scale_factor_idx_f16_2 === 0.U(3.W),
      scale_factor_idx_f16_2 === 1.U(3.W),
      scale_factor_idx_f16_2 === 2.U(3.W),
      scale_factor_idx_f16_2 === 3.U(3.W),
      scale_factor_idx_f16_2 === 4.U(3.W),
      scale_factor_idx_f16_2 === 5.U(3.W),
      scale_factor_idx_f16_2 === 6.U(3.W),
      scale_factor_idx_f16_2 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f16_2, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_2, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_2, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_2, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_2, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_2, 0.U(1.W)),
      0.U(15.W),
      0.U(15.W),
    )
  )
  scale_adder_opb_in_f16_3(1) := Mux1H(
    Seq(
      scale_factor_idx_f16_3 === 0.U(3.W),
      scale_factor_idx_f16_3 === 1.U(3.W),
      scale_factor_idx_f16_3 === 2.U(3.W),
      scale_factor_idx_f16_3 === 3.U(3.W),
      scale_factor_idx_f16_3 === 4.U(3.W),
      scale_factor_idx_f16_3 === 5.U(3.W),
      scale_factor_idx_f16_3 === 6.U(3.W),
      scale_factor_idx_f16_3 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f16_3, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_3, 0.U(1.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_3, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_3, 0.U(2.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_3, 0.U(1.W)),
      Cat(0.U(3.W), prescaled_b_frac_f16_3, 0.U(1.W)),
      0.U(15.W),
      0.U(15.W),
    )
  )
  scale_adder_opb_in_f64_0(2) := Mux1H(
    Seq(
      scale_factor_idx_f64_0 === 0.U(3.W),
      scale_factor_idx_f64_0 === 1.U(3.W),
      scale_factor_idx_f64_0 === 2.U(3.W),
      scale_factor_idx_f64_0 === 3.U(3.W),
      scale_factor_idx_f64_0 === 4.U(3.W),
      scale_factor_idx_f64_0 === 5.U(3.W),
      scale_factor_idx_f64_0 === 6.U(3.W),
      scale_factor_idx_f64_0 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f64_0, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f64_0, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_b_frac_f64_0),
      0.U(57.W),
      Cat(0.U(4.W), prescaled_b_frac_f64_0),
      0.U(57.W),
      Cat(0.U(4.W), prescaled_b_frac_f64_0),
      Cat(0.U(4.W), prescaled_b_frac_f64_0),
    )
  )
  scale_adder_opb_in_f32_1(2) := Mux1H(
    Seq(
      scale_factor_idx_f32_1 === 0.U(3.W),
      scale_factor_idx_f32_1 === 1.U(3.W),
      scale_factor_idx_f32_1 === 2.U(3.W),
      scale_factor_idx_f32_1 === 3.U(3.W),
      scale_factor_idx_f32_1 === 4.U(3.W),
      scale_factor_idx_f32_1 === 5.U(3.W),
      scale_factor_idx_f32_1 === 6.U(3.W),
      scale_factor_idx_f32_1 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f32_1, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f32_1, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_b_frac_f32_1),
      0.U(28.W),
      Cat(0.U(4.W), prescaled_b_frac_f32_1),
      0.U(28.W),
      Cat(0.U(4.W), prescaled_b_frac_f32_1),
      Cat(0.U(4.W), prescaled_b_frac_f32_1),
    )
  )
  scale_adder_opb_in_f16_2(2) := Mux1H(
    Seq(
      scale_factor_idx_f16_2 === 0.U(3.W),
      scale_factor_idx_f16_2 === 1.U(3.W),
      scale_factor_idx_f16_2 === 2.U(3.W),
      scale_factor_idx_f16_2 === 3.U(3.W),
      scale_factor_idx_f16_2 === 4.U(3.W),
      scale_factor_idx_f16_2 === 5.U(3.W),
      scale_factor_idx_f16_2 === 6.U(3.W),
      scale_factor_idx_f16_2 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f16_2, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_2, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_b_frac_f16_2),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_b_frac_f16_2),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_b_frac_f16_2),
      Cat(0.U(4.W), prescaled_b_frac_f16_2),
    )
  )
  scale_adder_opb_in_f16_3(2) := Mux1H(
    Seq(
      scale_factor_idx_f16_3 === 0.U(3.W),
      scale_factor_idx_f16_3 === 1.U(3.W),
      scale_factor_idx_f16_3 === 2.U(3.W),
      scale_factor_idx_f16_3 === 3.U(3.W),
      scale_factor_idx_f16_3 === 4.U(3.W),
      scale_factor_idx_f16_3 === 5.U(3.W),
      scale_factor_idx_f16_3 === 6.U(3.W),
      scale_factor_idx_f16_3 === 7.U(3.W)
    ),
    Seq(
      Cat(0.U(2.W), prescaled_b_frac_f16_3, 0.U(2.W)),
      Cat(0.U(2.W), prescaled_b_frac_f16_3, 0.U(2.W)),
      Cat(0.U(4.W), prescaled_b_frac_f16_3),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_b_frac_f16_3),
      0.U(15.W),
      Cat(0.U(4.W), prescaled_b_frac_f16_3),
      Cat(0.U(4.W), prescaled_b_frac_f16_3),
    )
  )
  val scale_adder_opb_f64_0 = scale_adder_opb_in_f64_0(0) + scale_adder_opb_in_f64_0(1) + scale_adder_opb_in_f64_0(2)
  val scale_adder_opb_f32_1 = scale_adder_opb_in_f32_1(0) + scale_adder_opb_in_f32_1(1) + scale_adder_opb_in_f32_1(2)
  val scale_adder_opb_f16_2 = scale_adder_opb_in_f16_2(0) + scale_adder_opb_in_f16_2(1) + scale_adder_opb_in_f16_2(2)
  val scale_adder_opb_f16_3 = scale_adder_opb_in_f16_3(0) + scale_adder_opb_in_f16_3(1) + scale_adder_opb_in_f16_3(2)


  val a_frac_lt_b_frac_real_f64_0 = iter_num_q(0)
  val a_frac_lt_b_frac_real_f32_1 = iter_num_q(1)
  val a_frac_lt_b_frac_real_f16_2 = iter_num_q(2)
  val a_frac_lt_b_frac_real_f16_3 = iter_num_q(3)


  val scaled_a_frac_f64_0 = Cat(quo_m1_iter_q_f64_0(2, 0), quo_iter_q_f64_0(53, 0))
  val scaled_a_frac_f32_1 = Cat(quo_m1_iter_q_f32_1(2, 0), quo_iter_q_f32_1(24, 0))
  val scaled_a_frac_f16_2 = Cat(quo_m1_iter_q_f16_2(2, 0), quo_iter_q_f16_2(11, 0))
  val scaled_a_frac_f16_3 = Cat(quo_m1_iter_q_f16_3(2, 0), quo_iter_q_f16_3(11, 0))
  val scaled_b_frac_f64_0 = frac_divisor_q_f64_0
  val scaled_b_frac_f32_1 = frac_divisor_q_f32_1
  val scaled_b_frac_f16_2 = frac_divisor_q_f16_2
  val scaled_b_frac_f16_3 = frac_divisor_q_f16_3

  val f_r_s_iter_init_pre_f64_0 = Cat(0.U(3.W), Mux(a_frac_lt_b_frac_real_f64_0, Cat(scaled_a_frac_f64_0, 0.U(1.W)), Cat(0.U(1.W), scaled_a_frac_f64_0)))
  val f_r_s_iter_init_pre_f32_1 = Cat(0.U(3.W), Mux(a_frac_lt_b_frac_real_f32_1, Cat(scaled_a_frac_f32_1, 0.U(1.W)), Cat(0.U(1.W), scaled_a_frac_f32_1)))
  val f_r_s_iter_init_pre_f16_2 = Cat(0.U(3.W), Mux(a_frac_lt_b_frac_real_f16_2, Cat(scaled_a_frac_f16_2, 0.U(1.W)), Cat(0.U(1.W), scaled_a_frac_f16_2)))
  val f_r_s_iter_init_pre_f16_3 = Cat(0.U(3.W), Mux(a_frac_lt_b_frac_real_f16_3, Cat(scaled_a_frac_f16_3, 0.U(1.W)), Cat(0.U(1.W), scaled_a_frac_f16_3)))
  val f_r_s_for_integer_quo_f64_0 = f_r_s_iter_init_pre_f64_0((REM_W_f64_0 + 1) - 1 - 2 - 1, (REM_W_f64_0 + 1) - 1 - 2 - 1 - 4)
  val f_r_s_for_integer_quo_f32_1 = f_r_s_iter_init_pre_f32_1((REM_W_f32_1 + 1) - 1 - 2 - 1, (REM_W_f32_1 + 1) - 1 - 2 - 1 - 4)
  val f_r_s_for_integer_quo_f16_2 = f_r_s_iter_init_pre_f16_2((REM_W_f16_2 + 1) - 1 - 2 - 1, (REM_W_f16_2 + 1) - 1 - 2 - 1 - 4)
  val f_r_s_for_integer_quo_f16_3 = f_r_s_iter_init_pre_f16_3((REM_W_f16_3 + 1) - 1 - 2 - 1, (REM_W_f16_3 + 1) - 1 - 2 - 1 - 4)
  val integer_quo_is_pos_2_f64_0 = f_r_s_for_integer_quo_f64_0(4) | (f_r_s_for_integer_quo_f64_0(3, 2) === 3.U(2.W))
  val integer_quo_is_pos_2_f32_1 = f_r_s_for_integer_quo_f32_1(4) | (f_r_s_for_integer_quo_f32_1(3, 2) === 3.U(2.W))
  val integer_quo_is_pos_2_f16_2 = f_r_s_for_integer_quo_f16_2(4) | (f_r_s_for_integer_quo_f16_2(3, 2) === 3.U(2.W))
  val integer_quo_is_pos_2_f16_3 = f_r_s_for_integer_quo_f16_3(4) | (f_r_s_for_integer_quo_f16_3(3, 2) === 3.U(2.W))
  val scaled_b_frac_ext_f64_0 = Cat(0.U(2.W), scaled_b_frac_f64_0, 0.U(1.W))
  val scaled_b_frac_ext_f32_1 = Cat(0.U(2.W), scaled_b_frac_f32_1, 0.U(1.W))
  val scaled_b_frac_ext_f16_2 = Cat(0.U(2.W), scaled_b_frac_f16_2, 0.U(1.W))
  val scaled_b_frac_ext_f16_3 = Cat(0.U(2.W), scaled_b_frac_f16_3, 0.U(1.W))
  val scaled_b_frac_ext_mul_neg_1_f64_0 = ~scaled_b_frac_ext_f64_0
  val scaled_b_frac_ext_mul_neg_1_f32_1 = ~scaled_b_frac_ext_f32_1
  val scaled_b_frac_ext_mul_neg_1_f16_2 = ~scaled_b_frac_ext_f16_2
  val scaled_b_frac_ext_mul_neg_1_f16_3 = ~scaled_b_frac_ext_f16_3
  val scaled_b_frac_ext_mul_neg_2_f64_0 = ~Cat(scaled_b_frac_ext_f64_0((REM_W_f64_0 - 1) - 1, 0), 0.U(1.W))
  val scaled_b_frac_ext_mul_neg_2_f32_1 = ~Cat(scaled_b_frac_ext_f32_1((REM_W_f32_1 - 1) - 1, 0), 0.U(1.W))
  val scaled_b_frac_ext_mul_neg_2_f16_2 = ~Cat(scaled_b_frac_ext_f16_2((REM_W_f16_2 - 1) - 1, 0), 0.U(1.W))
  val scaled_b_frac_ext_mul_neg_2_f16_3 = ~Cat(scaled_b_frac_ext_f16_3((REM_W_f16_3 - 1) - 1, 0), 0.U(1.W))

  val f_r_s_iter_init_f64_0 = Cat(f_r_s_iter_init_pre_f64_0((REM_W_f64_0 + 1) - 1 - 2, 0), 0.U(1.W)) |
    Cat(0.U(17.W), fp_format_q_is_fp16 & is_vec_q, 0.U(12.W), fp_format_q_is_fp32 & is_vec_q, 0.U(28.W), fp_format_q_is_fp64 | !is_vec_q)
  val f_r_s_iter_init_f32_1 = Cat(f_r_s_iter_init_pre_f32_1((REM_W_f32_1 + 1) - 1 - 2, 0), 0.U(1.W)) |
    Cat(0.U(17.W), fp_format_q_is_fp16 & is_vec_q, 0.U(12.W), fp_format_q_is_fp32 & is_vec_q)
  val f_r_s_iter_init_f16_2 = Cat(f_r_s_iter_init_pre_f16_2((REM_W_f16_2 + 1) - 1 - 2, 0), 1.U(1.W))
  val f_r_s_iter_init_f16_3 = Cat(f_r_s_iter_init_pre_f16_3((REM_W_f16_3 + 1) - 1 - 2, 0), 1.U(1.W))
  val f_r_c_iter_init_f64_0 = Mux(integer_quo_is_pos_2_f64_0, scaled_b_frac_ext_mul_neg_2_f64_0, scaled_b_frac_ext_mul_neg_1_f64_0)
  val f_r_c_iter_init_f32_1 = Mux(integer_quo_is_pos_2_f32_1, scaled_b_frac_ext_mul_neg_2_f32_1, scaled_b_frac_ext_mul_neg_1_f32_1)
  val f_r_c_iter_init_f16_2 = Mux(integer_quo_is_pos_2_f16_2, scaled_b_frac_ext_mul_neg_2_f16_2, scaled_b_frac_ext_mul_neg_1_f16_2)
  val f_r_c_iter_init_f16_3 = Mux(integer_quo_is_pos_2_f16_3, scaled_b_frac_ext_mul_neg_2_f16_3, scaled_b_frac_ext_mul_neg_1_f16_3)
  val nxt_f_r_s_70bit = Wire(Vec(3, UInt((4 * REM_W_f16_2).W)))
  val nxt_f_r_c_70bit = Wire(Vec(3, UInt((4 * REM_W_f16_2).W)))
  val nxt_f_r_s_f64_0 = Wire(Vec(3, UInt(REM_W_f64_0.W)))
  val nxt_f_r_s_f32_1 = Wire(Vec(3, UInt(REM_W_f32_1.W)))
  val nxt_f_r_s_f16_2 = Wire(Vec(3, UInt(REM_W_f16_2.W)))
  val nxt_f_r_s_f16_3 = Wire(Vec(3, UInt(REM_W_f16_3.W)))
  val nxt_f_r_c_f64_0 = Wire(Vec(3, UInt(REM_W_f64_0.W)))
  val nxt_f_r_c_f32_1 = Wire(Vec(3, UInt(REM_W_f32_1.W)))
  val nxt_f_r_c_f16_2 = Wire(Vec(3, UInt(REM_W_f16_2.W)))
  val nxt_f_r_c_f16_3 = Wire(Vec(3, UInt(REM_W_f16_3.W)))
  val f_r_s_d_72bit = Mux(
    fsm_q(FSM_PRE_2_BIT),
    Mux1H(
      Seq(
        fp_format_q_is_fp64 | !is_vec_q,
        fp_format_q_is_fp32 & is_vec_q,
        fp_format_q_is_fp16 & is_vec_q
      ),
      Seq(
        Cat(f_r_s_iter_init_f64_0, 0.U(12.W)),
        Cat(f_r_s_iter_init_f32_1, 0.U(5.W), f_r_s_iter_init_f64_0.head(31), 0.U(5.W)),
        Cat(f_r_s_iter_init_f16_3, f_r_s_iter_init_f16_2, f_r_s_iter_init_f32_1.head(18), f_r_s_iter_init_f64_0.head(18))
      )
    ),
    nxt_f_r_s_70bit(2)
  )
  val f_r_c_d_72bit = Mux(
    fsm_q(FSM_PRE_2_BIT),
    Mux1H(
      Seq(
        fp_format_q_is_fp64 | !is_vec_q,
        fp_format_q_is_fp32 & is_vec_q,
        fp_format_q_is_fp16 & is_vec_q
      ),
      Seq(
        Cat(f_r_c_iter_init_f64_0, 0.U(12.W)),
        Cat(f_r_c_iter_init_f32_1, 0.U(5.W), f_r_c_iter_init_f64_0.head(31), 0.U(5.W)),
        Cat(f_r_c_iter_init_f16_3, f_r_c_iter_init_f16_2, f_r_c_iter_init_f32_1.head(18), f_r_c_iter_init_f64_0.head(18))
      )
    ),
    nxt_f_r_c_70bit(2)
  )
  val f_r_s_d_f64_0 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_s_iter_init_f64_0, nxt_f_r_s_f64_0(2))
  val f_r_s_d_f32_1 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_s_iter_init_f32_1, nxt_f_r_s_f32_1(2))
  val f_r_s_d_f16_2 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_s_iter_init_f16_2, nxt_f_r_s_f16_2(2))
  val f_r_s_d_f16_3 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_s_iter_init_f16_3, nxt_f_r_s_f16_3(2))

  val f_r_s_en = start_handshaked | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_ITER_BIT)
  val f_r_c_d_f64_0 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_c_iter_init_f64_0, nxt_f_r_c_f64_0(2))
  val f_r_c_d_f32_1 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_c_iter_init_f32_1, nxt_f_r_c_f32_1(2))
  val f_r_c_d_f16_2 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_c_iter_init_f16_2, nxt_f_r_c_f16_2(2))
  val f_r_c_d_f16_3 = Mux(fsm_q(FSM_PRE_2_BIT), f_r_c_iter_init_f16_3, nxt_f_r_c_f16_3(2))
  val f_r_c_en = start_handshaked | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_ITER_BIT)
  val adder_6b_iter_init_f64_0 = f_r_s_iter_init_f64_0.tail(2).head(6) + f_r_c_iter_init_f64_0.tail(2).head(6)
  val adder_6b_iter_init_f32_1 = f_r_s_iter_init_f32_1.tail(2).head(6) + f_r_c_iter_init_f32_1.tail(2).head(6)
  val adder_6b_iter_init_f16_2 = f_r_s_iter_init_f16_2.tail(2).head(6) + f_r_c_iter_init_f16_2.tail(2).head(6)
  val adder_6b_iter_init_f16_3 = f_r_s_iter_init_f16_3.tail(2).head(6) + f_r_c_iter_init_f16_3.tail(2).head(6)
  val adder_7b_iter_init_f64_0 = f_r_s_iter_init_f64_0.tail(4).head(7) + f_r_c_iter_init_f64_0.tail(4).head(7)
  val adder_7b_iter_init_f32_1 = f_r_s_iter_init_f32_1.tail(4).head(7) + f_r_c_iter_init_f32_1.tail(4).head(7)
  val adder_7b_iter_init_f16_2 = f_r_s_iter_init_f16_2.tail(4).head(7) + f_r_c_iter_init_f16_2.tail(4).head(7)
  val adder_7b_iter_init_f16_3 = f_r_s_iter_init_f16_3.tail(4).head(7) + f_r_c_iter_init_f16_3.tail(4).head(7)

  val nr_f_r_6b_for_nxt_cycle_s0_qds_en = start_handshaked | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_ITER_BIT)
  val adder_6b_res_for_nxt_cycle_s0_qds_f64_0 = Wire(UInt(6.W))
  val adder_6b_res_for_nxt_cycle_s0_qds_f32_1 = Wire(UInt(6.W))
  val adder_6b_res_for_nxt_cycle_s0_qds_f16_2 = Wire(UInt(6.W))
  val adder_6b_res_for_nxt_cycle_s0_qds_f16_3 = Wire(UInt(6.W))
  val adder_7b_res_for_nxt_cycle_s1_qds_f64_0 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s1_qds_f32_1 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s1_qds_f16_2 = Wire(UInt(7.W))
  val adder_7b_res_for_nxt_cycle_s1_qds_f16_3 = Wire(UInt(7.W))

  val nr_f_r_6b_for_nxt_cycle_s0_qds_d_f64_0 = Mux(fsm_q(FSM_PRE_2_BIT), adder_6b_iter_init_f64_0, adder_6b_res_for_nxt_cycle_s0_qds_f64_0)
  val nr_f_r_6b_for_nxt_cycle_s0_qds_d_f32_1 = Mux(fsm_q(FSM_PRE_2_BIT), adder_6b_iter_init_f32_1, adder_6b_res_for_nxt_cycle_s0_qds_f32_1)
  val nr_f_r_6b_for_nxt_cycle_s0_qds_d_f16_2 = Mux(fsm_q(FSM_PRE_2_BIT), adder_6b_iter_init_f16_2, adder_6b_res_for_nxt_cycle_s0_qds_f16_2)
  val nr_f_r_6b_for_nxt_cycle_s0_qds_d_f16_3 = Mux(fsm_q(FSM_PRE_2_BIT), adder_6b_iter_init_f16_3, adder_6b_res_for_nxt_cycle_s0_qds_f16_3)
  val nr_f_r_7b_for_nxt_cycle_s1_qds_en = start_handshaked | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_ITER_BIT)

  val nr_f_r_7b_for_nxt_cycle_s1_qds_d_f64_0 = Mux(fsm_q(FSM_PRE_2_BIT), adder_7b_iter_init_f64_0, adder_7b_res_for_nxt_cycle_s1_qds_f64_0)
  val nr_f_r_7b_for_nxt_cycle_s1_qds_d_f32_1 = Mux(fsm_q(FSM_PRE_2_BIT), adder_7b_iter_init_f32_1, adder_7b_res_for_nxt_cycle_s1_qds_f32_1)
  val nr_f_r_7b_for_nxt_cycle_s1_qds_d_f16_2 = Mux(fsm_q(FSM_PRE_2_BIT), adder_7b_iter_init_f16_2, adder_7b_res_for_nxt_cycle_s1_qds_f16_2)
  val nr_f_r_7b_for_nxt_cycle_s1_qds_d_f16_3 = Mux(fsm_q(FSM_PRE_2_BIT), adder_7b_iter_init_f16_3, adder_7b_res_for_nxt_cycle_s1_qds_f16_3)
  val nr_f_r_6b_for_nxt_cycle_s0_qds_q_f64_0 = Reg(UInt(6.W))
  val nr_f_r_6b_for_nxt_cycle_s0_qds_q_f32_1 = Reg(UInt(6.W))
  val nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_2 = Reg(UInt(6.W))
  val nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_3 = Reg(UInt(6.W))
  val nr_f_r_7b_for_nxt_cycle_s1_qds_q_f64_0 = Reg(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s1_qds_q_f32_1 = Reg(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_2 = Reg(UInt(7.W))
  val nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_3 = Reg(UInt(7.W))
  val f_r_s_q_72bit = Reg(UInt((4 * REM_W_f16_2).W))
  val f_r_c_q_72bit = Reg(UInt((4 * REM_W_f16_2).W))
  val f_r_s_q_f64_0 = Reg(UInt(REM_W_f64_0.W))
  val f_r_s_q_f32_1 = Reg(UInt(REM_W_f32_1.W))
  val f_r_s_q_f16_2 = Reg(UInt(REM_W_f16_2.W))
  val f_r_s_q_f16_3 = Reg(UInt(REM_W_f16_3.W))
  val f_r_c_q_f64_0 = Reg(UInt(REM_W_f64_0.W))
  val f_r_c_q_f32_1 = Reg(UInt(REM_W_f32_1.W))
  val f_r_c_q_f16_2 = Reg(UInt(REM_W_f16_2.W))
  val f_r_c_q_f16_3 = Reg(UInt(REM_W_f16_3.W))
  when(nr_f_r_6b_for_nxt_cycle_s0_qds_en) {
    nr_f_r_6b_for_nxt_cycle_s0_qds_q_f64_0 := nr_f_r_6b_for_nxt_cycle_s0_qds_d_f64_0
    nr_f_r_6b_for_nxt_cycle_s0_qds_q_f32_1 := nr_f_r_6b_for_nxt_cycle_s0_qds_d_f32_1
    nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_2 := nr_f_r_6b_for_nxt_cycle_s0_qds_d_f16_2
    nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_3 := nr_f_r_6b_for_nxt_cycle_s0_qds_d_f16_3
  }
  when(nr_f_r_7b_for_nxt_cycle_s1_qds_en) {
    nr_f_r_7b_for_nxt_cycle_s1_qds_q_f64_0 := nr_f_r_7b_for_nxt_cycle_s1_qds_d_f64_0
    nr_f_r_7b_for_nxt_cycle_s1_qds_q_f32_1 := nr_f_r_7b_for_nxt_cycle_s1_qds_d_f32_1
    nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_2 := nr_f_r_7b_for_nxt_cycle_s1_qds_d_f16_2
    nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_3 := nr_f_r_7b_for_nxt_cycle_s1_qds_d_f16_3
  }
  when(f_r_s_en) {
    f_r_s_q_72bit := f_r_s_d_72bit
    f_r_s_q_f64_0 := f_r_s_d_f64_0
    f_r_s_q_f32_1 := f_r_s_d_f32_1
    f_r_s_q_f16_2 := f_r_s_d_f16_2
    f_r_s_q_f16_3 := f_r_s_d_f16_3
  }
  when(f_r_c_en) {
    f_r_c_q_72bit := f_r_c_d_72bit
    f_r_c_q_f64_0 := f_r_c_d_f64_0
    f_r_c_q_f32_1 := f_r_c_d_f32_1
    f_r_c_q_f16_2 := f_r_c_d_f16_2
    f_r_c_q_f16_3 := f_r_c_d_f16_3
  }


  val nxt_frac_divisor_pre_0_f64_0 = Mux(init_cycles(1), scale_adder_opb_f64_0, frac_divisor_q_f64_0)
  val nxt_frac_divisor_pre_0_f32_1 = frac_divisor_q_f32_1
  val nxt_frac_divisor_pre_0_f16_2 = frac_divisor_q_f16_2
  val nxt_frac_divisor_pre_0_f16_3 = frac_divisor_q_f16_3
  val nxt_frac_divisor_pre_1_f64_0 = scale_adder_opb_f64_0
  val nxt_frac_divisor_pre_1_f32_1 = scale_adder_opb_f32_1
  val nxt_frac_divisor_pre_1_f16_2 = scale_adder_opb_f16_2
  val nxt_frac_divisor_pre_1_f16_3 = scale_adder_opb_f16_3

  val nxt_frac_divisor_pre_2_f64_0 = frac_divisor_q_f64_0
  val nxt_frac_divisor_pre_2_f32_1 = frac_divisor_q_f32_1
  val nxt_frac_divisor_pre_2_f16_2 = frac_divisor_q_f16_2
  val nxt_frac_divisor_pre_2_f16_3 = frac_divisor_q_f16_3
  val sticky_without_rem_f64_0 = Wire(UInt((FP64_FRAC_W + 1).W))
  val sticky_without_rem_f32_1 = Wire(UInt((FP32_FRAC_W + 1).W))

  val sticky_without_rem_f16_2 = Wire(UInt((FP16_FRAC_W + 2).W))
  val sticky_without_rem_f16_3 = Wire(UInt((FP16_FRAC_W + 2).W))

  val frac_divisor_d_f64_0 =
    (Fill(57, fsm_q(FSM_PRE_0_BIT)) & nxt_frac_divisor_pre_0_f64_0) |
      (Fill(57, fsm_q(FSM_PRE_1_BIT)) & nxt_frac_divisor_pre_1_f64_0) |
      (Fill(57, fsm_q(FSM_PRE_2_BIT)) & nxt_frac_divisor_pre_2_f64_0) |
      (Fill(57, fsm_q(FSM_POST_0_BIT)) & Cat(0.U(3.W), sticky_without_rem_f64_0))
  val frac_divisor_d_f32_1 =
    (Fill(28, fsm_q(FSM_PRE_0_BIT)) & nxt_frac_divisor_pre_0_f32_1) |
      (Fill(28, fsm_q(FSM_PRE_1_BIT)) & nxt_frac_divisor_pre_1_f32_1) |
      (Fill(28, fsm_q(FSM_PRE_2_BIT)) & nxt_frac_divisor_pre_2_f32_1) |
      (Fill(28, fsm_q(FSM_POST_0_BIT)) & Cat(0.U(3.W), sticky_without_rem_f32_1))
  val frac_divisor_d_f16_2 =
    (Fill(15, fsm_q(FSM_PRE_0_BIT)) & nxt_frac_divisor_pre_0_f16_2) |
      (Fill(15, fsm_q(FSM_PRE_1_BIT)) & nxt_frac_divisor_pre_1_f16_2) |
      (Fill(15, fsm_q(FSM_PRE_2_BIT)) & nxt_frac_divisor_pre_2_f16_2) |
      (Fill(15, fsm_q(FSM_POST_0_BIT)) & Cat(0.U(2.W), sticky_without_rem_f16_2))
  val frac_divisor_d_f16_3 =
    (Fill(15, fsm_q(FSM_PRE_0_BIT)) & nxt_frac_divisor_pre_0_f16_3) |
      (Fill(15, fsm_q(FSM_PRE_1_BIT)) & nxt_frac_divisor_pre_1_f16_3) |
      (Fill(15, fsm_q(FSM_PRE_2_BIT)) & nxt_frac_divisor_pre_2_f16_3) |
      (Fill(15, fsm_q(FSM_POST_0_BIT)) & Cat(0.U(2.W), sticky_without_rem_f16_3))
  val frac_divisor_en_post_0 = fsm_q(FSM_POST_0_BIT) & (
    ((!is_vec_q | fp_format_q_is_fp64) & res_is_denormal_f64_0) | ((is_vec_q & fp_format_q_is_fp32) & res_is_denormal_f32_1)
    )
  val frac_divisor_en =
    start_handshaked | fsm_q(FSM_PRE_1_BIT) | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_POST_0_BIT)
  when(frac_divisor_en) {


    val frac_divisor_q_f64_0_temp = Mux(fsm_q(FSM_POST_0_BIT) & !res_is_denormal_f64_0, frac_divisor_q_f64_0, frac_divisor_d_f64_0)
    val frac_divisor_q_f32_1_temp = Mux(fsm_q(FSM_POST_0_BIT) & !res_is_denormal_f32_1, frac_divisor_q_f32_1, frac_divisor_d_f32_1)
    val frac_divisor_q_f16_2_temp = Mux(fsm_q(FSM_POST_0_BIT) & !res_is_denormal_f16_2, frac_divisor_q_f16_2, frac_divisor_d_f16_2)
    val frac_divisor_q_f16_3_temp = Mux(fsm_q(FSM_POST_0_BIT) & !res_is_denormal_f16_3, frac_divisor_q_f16_3, frac_divisor_d_f16_3)

    frac_divisor_q_60bit := Mux(
      (start_handshaked & (fp_format_is_fp64 | !is_vec_d)) | (!start_handshaked & (fp_format_q_is_fp64 | !is_vec_q)),
      Cat(frac_divisor_q_f64_0_temp, 0.U(3.W)),
      Mux(
        (start_handshaked & (fp_format_is_fp32 & is_vec_d)) | (!start_handshaked & (fp_format_q_is_fp32 & is_vec_q)),
        Cat(frac_divisor_q_f32_1_temp, 0.U(2.W), frac_divisor_q_f64_0_temp.head(28), 0.U(2.W)),
        Cat(frac_divisor_q_f16_3_temp(14, 0), frac_divisor_q_f16_2_temp(14, 0),
          frac_divisor_q_f32_1_temp.head(15),
          frac_divisor_q_f64_0_temp.head(15))
      )
    )
  }


  val prev_quo_dig_en = start_handshaked | fsm_q(FSM_PRE_2_BIT) | fsm_q(FSM_ITER_BIT)
  val nxt_quo_dig_f64_0 = Wire(Vec(3, UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_f32_1 = Wire(Vec(3, UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_f16_2 = Wire(Vec(3, UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_f16_3 = Wire(Vec(3, UInt(QUO_DIG_W.W)))
  val prev_quo_dig_d_f64_0 = Mux(fsm_q(FSM_PRE_2_BIT), Cat(0.U(3.W), ~integer_quo_is_pos_2_f64_0, integer_quo_is_pos_2_f64_0), nxt_quo_dig_f64_0(2))
  val prev_quo_dig_d_f32_1 = Mux(fsm_q(FSM_PRE_2_BIT), Cat(0.U(3.W), ~integer_quo_is_pos_2_f32_1, integer_quo_is_pos_2_f32_1), nxt_quo_dig_f32_1(2))
  val prev_quo_dig_d_f16_2 = Mux(fsm_q(FSM_PRE_2_BIT), Cat(0.U(3.W), ~integer_quo_is_pos_2_f16_2, integer_quo_is_pos_2_f16_2), nxt_quo_dig_f16_2(2))
  val prev_quo_dig_d_f16_3 = Mux(fsm_q(FSM_PRE_2_BIT), Cat(0.U(3.W), ~integer_quo_is_pos_2_f16_3, integer_quo_is_pos_2_f16_3), nxt_quo_dig_f16_3(2))
  val prev_quo_dig_q_f64_0 = Reg(UInt(QUO_DIG_W.W))
  val prev_quo_dig_q_f32_1 = Reg(UInt(QUO_DIG_W.W))
  val prev_quo_dig_q_f16_2 = Reg(UInt(QUO_DIG_W.W))
  val prev_quo_dig_q_f16_3 = Reg(UInt(QUO_DIG_W.W))
  when(prev_quo_dig_en) {
    prev_quo_dig_q_f64_0 := prev_quo_dig_d_f64_0
    prev_quo_dig_q_f32_1 := prev_quo_dig_d_f32_1
    prev_quo_dig_q_f16_2 := prev_quo_dig_d_f16_2
    prev_quo_dig_q_f16_3 := prev_quo_dig_d_f16_3
  }

  val nxt_quo_iter_f64_0 = Wire(Vec(3, UInt(56.W)))
  val nxt_quo_iter_f32_1 = Wire(Vec(3, UInt(26.W)))
  val nxt_quo_iter_f16_2 = Wire(Vec(3, UInt(14.W)))
  val nxt_quo_iter_f16_3 = Wire(Vec(3, UInt(14.W)))
  val nxt_quo_m1_iter_f64_0 = Wire(Vec(3, UInt(56.W)))
  val nxt_quo_m1_iter_f32_1 = Wire(Vec(3, UInt(26.W)))
  val nxt_quo_m1_iter_f16_2 = Wire(Vec(3, UInt(14.W)))
  val nxt_quo_m1_iter_f16_3 = Wire(Vec(3, UInt(14.W)))
  nxt_quo_iter_f64_0(0) :=
    (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f64_0((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f64_0((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_0_BIT)) & Cat(quo_iter_q_f64_0((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f64_0((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f64_0((56 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f32_1(0) :=
    (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f32_1((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f32_1((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_0_BIT)) & Cat(quo_iter_q_f32_1((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f32_1((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f32_1((26 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_2(0) :=
    (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f16_2((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f16_2((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_0_BIT)) & Cat(quo_iter_q_f16_2((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f16_2((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f16_2((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_3(0) :=
    (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f16_3((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f16_3((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_0_BIT)) & Cat(quo_iter_q_f16_3((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f16_3((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f16_3((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_m1_iter_f64_0(0) :=
    (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f64_0((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f64_0((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_0_BIT)) & Cat(quo_m1_iter_q_f64_0((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f64_0((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, prev_quo_dig_q_f64_0(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f64_0((56 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f32_1(0) :=
    (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f32_1((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f32_1((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_0_BIT)) & Cat(quo_m1_iter_q_f32_1((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f32_1((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, prev_quo_dig_q_f32_1(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f32_1((26 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_2(0) :=
    (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f16_2((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f16_2((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_0_BIT)) & Cat(quo_m1_iter_q_f16_2((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f16_2((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_2(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f16_2((14 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_3(0) :=
    (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_POS_2_BIT)) & Cat(quo_iter_q_f16_3((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_POS_1_BIT)) & Cat(quo_iter_q_f16_3((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_0_BIT)) & Cat(quo_m1_iter_q_f16_3((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_1_BIT)) & Cat(quo_m1_iter_q_f16_3((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, prev_quo_dig_q_f16_3(QUO_DIG_NEG_2_BIT)) & Cat(quo_m1_iter_q_f16_3((14 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_iter_f64_0(1) :=
    (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f64_0(0)((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f64_0(0)((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f64_0(0)((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f64_0(0)((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f64_0(0)((56 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f32_1(1) :=
    (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f32_1(0)((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f32_1(0)((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f32_1(0)((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f32_1(0)((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f32_1(0)((26 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_2(1) :=
    (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_2(0)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_2(0)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f16_2(0)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_2(0)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_2(0)((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_3(1) :=
    (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_3(0)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_3(0)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f16_3(0)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_3(0)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_3(0)((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_m1_iter_f64_0(1) :=
    (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f64_0(0)((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f64_0(0)((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f64_0(0)((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f64_0(0)((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f64_0(0)((56 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f32_1(1) :=
    (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f32_1(0)((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f32_1(0)((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f32_1(0)((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f32_1(0)((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f32_1(0)((26 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_2(1) :=
    (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_2(0)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_2(0)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f16_2(0)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_2(0)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_2(0)((14 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_3(1) :=
    (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_3(0)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_3(0)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f16_3(0)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_3(0)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(0)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_3(0)((14 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_iter_f64_0(2) :=
    (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f64_0(1)((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f64_0(1)((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f64_0(1)((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f64_0(1)((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f64_0(1)((56 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f32_1(2) :=
    (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f32_1(1)((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f32_1(1)((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f32_1(1)((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f32_1(1)((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f32_1(1)((26 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_2(2) :=
    (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_2(1)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_2(1)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f16_2(1)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_2(1)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_2(1)((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_iter_f16_3(2) :=
    (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_3(1)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_3(1)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_iter_f16_3(1)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_3(1)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_3(1)((14 - 1) - 2, 0), "b10".U(2.W)))
  nxt_quo_m1_iter_f64_0(2) :=
    (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f64_0(1)((56 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f64_0(1)((56 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f64_0(1)((56 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f64_0(1)((56 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(56, nxt_quo_dig_f64_0(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f64_0(1)((56 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f32_1(2) :=
    (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f32_1(1)((26 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f32_1(1)((26 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f32_1(1)((26 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f32_1(1)((26 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(26, nxt_quo_dig_f32_1(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f32_1(1)((26 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_2(2) :=
    (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_2(1)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_2(1)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f16_2(1)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_2(1)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_2(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_2(1)((14 - 1) - 2, 0), "b01".U(2.W)))
  nxt_quo_m1_iter_f16_3(2) :=
    (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_POS_2_BIT)) & Cat(nxt_quo_iter_f16_3(1)((14 - 1) - 2, 0), "b01".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_POS_1_BIT)) & Cat(nxt_quo_iter_f16_3(1)((14 - 1) - 2, 0), "b00".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_0_BIT)) & Cat(nxt_quo_m1_iter_f16_3(1)((14 - 1) - 2, 0), "b11".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_1_BIT)) & Cat(nxt_quo_m1_iter_f16_3(1)((14 - 1) - 2, 0), "b10".U(2.W))) |
      (Fill(14, nxt_quo_dig_f16_3(1)(QUO_DIG_NEG_2_BIT)) & Cat(nxt_quo_m1_iter_f16_3(1)((14 - 1) - 2, 0), "b01".U(2.W)))

  val nxt_quo_iter_pre_0_f64_0 = Wire(UInt((56 - 2).W))

  val nxt_quo_iter_pre_0_f32_1 = Wire(UInt((26 - 2 + 1).W))
  val nxt_quo_iter_pre_0_f16_2 = Wire(UInt((14 - 2).W))
  val nxt_quo_iter_pre_0_f16_3 = Wire(UInt((14 - 2).W))

  nxt_quo_iter_pre_0_f64_0 := Mux(
    init_cycles(1),
    Mux(
      opb_is_power_of_2_f64_0,
      Mux(
        fp_format_is_fp64,
        Cat(1.U(2.W), opa(51, 0)),
        Mux(
          fp_format_is_fp32,
          Cat(1.U(31.W), opa(22, 0)),
          Cat(1.U(44.W), opa(9, 0))
        )
      ),
      scale_adder_opa_f64_0(53, 0)
    ),
    Mux(
      opb_is_power_of_2_f64_0,
      Mux(
        fp_format_is_fp64,
        Cat(1.U(2.W), opa_frac_l_shifted_f64_0(51, 0)),
        Mux(
          fp_format_is_fp32,
          Cat(1.U(31.W), opa_frac_l_shifted_f64_0(51, 29)),
          Cat(1.U(44.W), opa_frac_l_shifted_f64_0(51, 42))
        )
      ),
      Cat(0.U(1.W), opa_frac_l_shifted_f64_0),
    )
  )
  nxt_quo_iter_pre_0_f32_1 := Mux(
    opb_is_power_of_2_f32_1 & !(opa_exp_is_zero_f32_1 | opb_exp_is_zero_f32_1),
    Mux(fp_format_is_fp32, Cat(1.U(2.W), opa.head(32)(22, 0)), Cat(1.U(15.W), opa(25, 16))),

    Mux(
      opb_is_power_of_2_f32_1 & (opa_exp_is_zero_f32_1 | opb_exp_is_zero_f32_1) & fp_format_is_fp16 & is_vec_d,
      Cat(1.U(15.W), opa_frac_l_shifted_f32_1.tail(1).head(10)),
      Cat(0.U(1.W), opa_frac_l_shifted_f32_1),
    )

  )
  nxt_quo_iter_pre_0_f16_2 := Mux(opb_is_power_of_2_f16_2 & !(opa_exp_is_zero_f16_2 | opb_exp_is_zero_f16_2), Cat(1.U(2.W), opa(41, 32)), Cat(0.U(1.W), opa_frac_l_shifted_f16_2))
  nxt_quo_iter_pre_0_f16_3 := Mux(opb_is_power_of_2_f16_3 & !(opa_exp_is_zero_f16_3 | opb_exp_is_zero_f16_3), Cat(1.U(2.W), opa(57, 48)), Cat(0.U(1.W), opa_frac_l_shifted_f16_3))
  val nxt_quo_iter_pre_1_f64_0 = scale_adder_opa_f64_0(53, 0)
  val nxt_quo_iter_pre_1_f32_1 = scale_adder_opa_f32_1(24, 0)
  val nxt_quo_iter_pre_1_f16_2 = scale_adder_opa_f16_2(11, 0)
  val nxt_quo_iter_pre_1_f16_3 = scale_adder_opa_f16_3(11, 0)
  val nxt_quo_iter_pre_2_f64_0 = 0.U(54.W)
  val nxt_quo_iter_pre_2_f32_1 = 0.U(25.W)
  val nxt_quo_iter_pre_2_f16_2 = 0.U(12.W)
  val nxt_quo_iter_pre_2_f16_3 = 0.U(12.W)


  val nxt_quo_m1_iter_pre_0_f64_0 = Mux(
    init_cycles(1),
    Cat(0.U(51.W), scale_adder_opa_f64_0(56, 54)),
    Cat(0.U(1.W), opb_frac_l_shifted_f64_0)
  )
  val nxt_quo_m1_iter_pre_0_f32_1 = Cat(0.U(1.W), opb_frac_l_shifted_f32_1)
  val nxt_quo_m1_iter_pre_0_f16_2 = Cat(0.U(1.W), opb_frac_l_shifted_f16_2)
  val nxt_quo_m1_iter_pre_0_f16_3 = Cat(0.U(1.W), opb_frac_l_shifted_f16_3)
  val nxt_quo_m1_iter_pre_1_f64_0 = Cat(0.U(51.W), scale_adder_opa_f64_0(56, 54))
  val nxt_quo_m1_iter_pre_1_f32_1 = Cat(0.U(22.W), scale_adder_opa_f32_1(27, 25))
  val nxt_quo_m1_iter_pre_1_f16_2 = Cat(0.U(9.W), scale_adder_opa_f16_2(14, 12))
  val nxt_quo_m1_iter_pre_1_f16_3 = Cat(0.U(9.W), scale_adder_opa_f16_3(14, 12))
  val rem_is_not_zero_f64_0 = Wire(Bool())
  val rem_is_not_zero_f32_1 = Wire(Bool())
  val rem_is_not_zero_f16_2 = Wire(Bool())
  val rem_is_not_zero_f16_3 = Wire(Bool())
  val correct_quo_r_shifted_f64_0 = Wire(UInt((FP64_FRAC_W + 1).W))
  val correct_quo_r_shifted_f32_1 = Wire(UInt((FP32_FRAC_W + 1).W))
  val correct_quo_r_shifted_f16_2 = Wire(UInt((FP16_FRAC_W + 1).W))
  val correct_quo_r_shifted_f16_3 = Wire(UInt((FP16_FRAC_W + 1).W))

  val out_frac_post_0_f64_0 = Wire(UInt(52.W))
  val out_frac_post_0_f32_0 = Wire(UInt(23.W))
  val out_frac_post_0_f32_1 = Wire(UInt(23.W))
  val out_frac_post_0_f16_0 = Wire(UInt(10.W))
  val out_frac_post_0_f16_1 = Wire(UInt(10.W))
  val out_frac_post_0_f16_2 = Wire(UInt(10.W))
  val out_frac_post_0_f16_3 = Wire(UInt(10.W))
  val nxt_quo_m1_iter_post_0_f64_0 = Mux(
    res_is_denormal_f64_0,
    Cat(rem_is_not_zero_f64_0, correct_quo_r_shifted_f64_0(52, 0)),
    Mux(
      is_vec_q & !fp_format_q_is_fp64,
      Mux(
        fp_format_q_is_fp32,
        Cat(0.U(29.W), out_frac_post_0_f32_0),
        Cat(0.U(42.W), out_frac_post_0_f16_0)
      ),
      out_frac_post_0_f64_0
    )
  )
  val nxt_quo_m1_iter_post_0_f32_1 = Mux(
    res_is_denormal_f32_1,
    Cat(rem_is_not_zero_f32_1, correct_quo_r_shifted_f32_1(23, 0)),
    Mux(is_vec_q & fp_format_q_is_fp32, out_frac_post_0_f32_1, Cat(0.U(13.W), out_frac_post_0_f16_1))
  )
  val nxt_quo_m1_iter_post_0_f16_2 = Mux(
    res_is_denormal_f16_2,
    Cat(rem_is_not_zero_f16_2, correct_quo_r_shifted_f16_2(10, 0)),
    out_frac_post_0_f16_2
  )
  val nxt_quo_m1_iter_post_0_f16_3 = Mux(
    res_is_denormal_f16_3,
    Cat(rem_is_not_zero_f16_3, correct_quo_r_shifted_f16_3(10, 0)),
    out_frac_post_0_f16_3
  )


  val quo_iter_d_f64_0 =
    (Fill(54, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_iter_pre_0_f64_0) |
      (Fill(54, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_iter_pre_1_f64_0) |
      (Fill(54, fsm_q(FSM_PRE_2_BIT)) & nxt_quo_iter_pre_2_f64_0) |
      (Fill(54, fsm_q(FSM_ITER_BIT)) & nxt_quo_iter_f64_0(2)(53, 0))
  val quo_iter_d_f32_1 =
    (Fill(25, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_iter_pre_0_f32_1) |
      (Fill(25, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_iter_pre_1_f32_1) |
      (Fill(25, fsm_q(FSM_PRE_2_BIT)) & nxt_quo_iter_pre_2_f32_1) |
      (Fill(25, fsm_q(FSM_ITER_BIT)) & nxt_quo_iter_f32_1(2)(24, 0))
  val quo_iter_d_f16_2 =
    (Fill(12, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_iter_pre_0_f16_2) |
      (Fill(12, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_iter_pre_1_f16_2) |
      (Fill(12, fsm_q(FSM_PRE_2_BIT)) & nxt_quo_iter_pre_2_f16_2) |
      (Fill(12, fsm_q(FSM_ITER_BIT)) & nxt_quo_iter_f16_2(2)(11, 0))
  val quo_iter_d_f16_3 =
    (Fill(12, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_iter_pre_0_f16_3) |
      (Fill(12, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_iter_pre_1_f16_3) |
      (Fill(12, fsm_q(FSM_PRE_2_BIT)) & nxt_quo_iter_pre_2_f16_3) |
      (Fill(12, fsm_q(FSM_ITER_BIT)) & nxt_quo_iter_f16_3(2)(11, 0))

  val quo_m1_iter_d_f64_0 =
    (Fill(54, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_m1_iter_pre_0_f64_0) |
      (Fill(54, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_m1_iter_pre_1_f64_0) |
      (Fill(54, fsm_q(FSM_ITER_BIT)) & nxt_quo_m1_iter_f64_0(2)(53, 0)) |
      (Fill(54, fsm_q(FSM_POST_0_BIT)) & nxt_quo_m1_iter_post_0_f64_0)
  val quo_m1_iter_d_f32_1 =
    (Fill(25, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_m1_iter_pre_0_f32_1) |
      (Fill(25, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_m1_iter_pre_1_f32_1) |
      (Fill(25, fsm_q(FSM_ITER_BIT)) & nxt_quo_m1_iter_f32_1(2)(24, 0)) |
      (Fill(25, fsm_q(FSM_POST_0_BIT)) & nxt_quo_m1_iter_post_0_f32_1)
  val quo_m1_iter_d_f16_2 =
    (Fill(12, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_m1_iter_pre_0_f16_2) |
      (Fill(12, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_m1_iter_pre_1_f16_2) |
      (Fill(12, fsm_q(FSM_ITER_BIT)) & nxt_quo_m1_iter_f16_2(2)(11, 0)) |
      (Fill(12, fsm_q(FSM_POST_0_BIT)) & nxt_quo_m1_iter_post_0_f16_2)
  val quo_m1_iter_d_f16_3 =
    (Fill(12, fsm_q(FSM_PRE_0_BIT)) & nxt_quo_m1_iter_pre_0_f16_3) |
      (Fill(12, fsm_q(FSM_PRE_1_BIT)) & nxt_quo_m1_iter_pre_1_f16_3) |
      (Fill(12, fsm_q(FSM_ITER_BIT)) & nxt_quo_m1_iter_f16_3(2)(11, 0)) |
      (Fill(12, fsm_q(FSM_POST_0_BIT)) & nxt_quo_m1_iter_post_0_f16_3)

  val quo_iter_en =
    start_handshaked |
      fsm_q(FSM_PRE_1_BIT) |
      fsm_q(FSM_PRE_2_BIT) |
      fsm_q(FSM_ITER_BIT)

  val quo_m1_iter_en =
    start_handshaked |
      fsm_q(FSM_PRE_1_BIT) |
      fsm_q(FSM_ITER_BIT) |
      (fsm_q(FSM_POST_0_BIT) & (!is_vec_q & res_is_denormal_f64_0 | is_vec_q))
  when(quo_iter_en) {
    quo_iter_q_f64_0 := Mux(start_handshaked, quo_iter_d_f64_0, Mux(!opb_is_power_of_2_q(0), quo_iter_d_f64_0, quo_iter_q_f64_0))
    quo_iter_q_f32_1 := Mux(start_handshaked, quo_iter_d_f32_1, Mux(!opb_is_power_of_2_q(1), quo_iter_d_f32_1, quo_iter_q_f32_1))
    quo_iter_q_f16_2 := Mux(start_handshaked, quo_iter_d_f16_2, Mux(!opb_is_power_of_2_q(2), quo_iter_d_f16_2, quo_iter_q_f16_2))
    quo_iter_q_f16_3 := Mux(start_handshaked, quo_iter_d_f16_3, Mux(!opb_is_power_of_2_q(3), quo_iter_d_f16_3, quo_iter_q_f16_3))
  }
  when(quo_m1_iter_en) {
    quo_m1_iter_q_f64_0 := quo_m1_iter_d_f64_0
    quo_m1_iter_q_f32_1 := quo_m1_iter_d_f32_1
    quo_m1_iter_q_f16_2 := quo_m1_iter_d_f16_2
    quo_m1_iter_q_f16_3 := quo_m1_iter_d_f16_3
  }
  final_iter := iter_num_q === 0.U(4.W)
  val iter_num_needed = Mux(fp_format_q_is_fp16, 1.U(4.W), Mux(fp_format_q_is_fp32, 3.U(4.W), 8.U(4.W)))


  val iter_num_en =
    start_handshaked |
      fsm_q(FSM_PRE_1_BIT) |
      fsm_q(FSM_PRE_2_BIT) |
      fsm_q(FSM_ITER_BIT)


  val iter_num_d =
    Mux(!is_vec_d & fsm_q(FSM_PRE_0_BIT), Cat(0.U(3.W), a_frac_lt_b_frac_f64_0),
      Mux(fsm_q(FSM_PRE_1_BIT), Cat(a_frac_lt_b_frac_l_shifted_f16_3, a_frac_lt_b_frac_l_shifted_f16_2, a_frac_lt_b_frac_l_shifted_f32_1, a_frac_lt_b_frac_l_shifted_f64_0),
        Mux(fsm_q(FSM_PRE_2_BIT), iter_num_needed,
          iter_num_q - 1.U(4.W)
        )
      )
    )
  when(iter_num_en) {
    iter_num_q := iter_num_d
  }

  val u_r64_block_vector = Module(new fpdiv_r64_block_vector())
  u_r64_block_vector.io.fp_format_onehot := Cat(fp_format_q_is_fp64, fp_format_q_is_fp32, fp_format_q_is_fp16)
  u_r64_block_vector.io.is_vec := is_vec_q
  u_r64_block_vector.io.f_r_s_i := f_r_s_q_72bit
  u_r64_block_vector.io.f_r_c_i := f_r_c_q_72bit
  u_r64_block_vector.io.divisor_i := frac_divisor_q_60bit

  val nr_f_r_6b_temp = Wire(Vec(4, UInt(6.W)))
  nr_f_r_6b_temp(3) := nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_3
  nr_f_r_6b_temp(2) := nr_f_r_6b_for_nxt_cycle_s0_qds_q_f16_2
  nr_f_r_6b_temp(1) := nr_f_r_6b_for_nxt_cycle_s0_qds_q_f32_1
  nr_f_r_6b_temp(0) := nr_f_r_6b_for_nxt_cycle_s0_qds_q_f64_0
  u_r64_block_vector.io.nr_f_r_6b_for_nxt_cycle_s0_qds_i := nr_f_r_6b_temp
  val nr_f_r_7b_temp = Wire(Vec(4, UInt(7.W)))
  nr_f_r_7b_temp(3) := nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_3
  nr_f_r_7b_temp(2) := nr_f_r_7b_for_nxt_cycle_s1_qds_q_f16_2
  nr_f_r_7b_temp(1) := nr_f_r_7b_for_nxt_cycle_s1_qds_q_f32_1
  nr_f_r_7b_temp(0) := nr_f_r_7b_for_nxt_cycle_s1_qds_q_f64_0
  u_r64_block_vector.io.nr_f_r_7b_for_nxt_cycle_s1_qds_i := nr_f_r_7b_temp
  nxt_quo_dig_f64_0 := u_r64_block_vector.io.nxt_quo_dig_o_0
  nxt_quo_dig_f32_1 := u_r64_block_vector.io.nxt_quo_dig_o_1
  nxt_quo_dig_f16_2 := u_r64_block_vector.io.nxt_quo_dig_o_2
  nxt_quo_dig_f16_3 := u_r64_block_vector.io.nxt_quo_dig_o_3

  nxt_f_r_s_70bit := u_r64_block_vector.io.nxt_f_r_s_o
  nxt_f_r_c_70bit := u_r64_block_vector.io.nxt_f_r_c_o
  nxt_f_r_s_f64_0 := Mux(
    fp_format_q_is_fp64 | !is_vec_q,
    Cat(
      u_r64_block_vector.io.nxt_f_r_s_o(2).head(REM_W_f64_0),
      u_r64_block_vector.io.nxt_f_r_s_o(1).head(REM_W_f64_0),
      u_r64_block_vector.io.nxt_f_r_s_o(0).head(REM_W_f64_0)
    ),
    Mux(
      fp_format_q_is_fp32 & is_vec_q,
      Cat(
        u_r64_block_vector.io.nxt_f_r_s_o(2).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W),
        u_r64_block_vector.io.nxt_f_r_s_o(1).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W),
        u_r64_block_vector.io.nxt_f_r_s_o(0).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W)
      ),
      Cat(
        u_r64_block_vector.io.nxt_f_r_s_o(2).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W),
        u_r64_block_vector.io.nxt_f_r_s_o(1).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W),
        u_r64_block_vector.io.nxt_f_r_s_o(0).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W)
      )
    )
  ).asTypeOf(nxt_f_r_s_f64_0)
  nxt_f_r_s_f32_1 := Mux(
    fp_format_q_is_fp32 & is_vec_q,
    Cat(
      u_r64_block_vector.io.nxt_f_r_s_o(2).head(REM_W_f32_1),
      u_r64_block_vector.io.nxt_f_r_s_o(1).head(REM_W_f32_1),
      u_r64_block_vector.io.nxt_f_r_s_o(0).head(REM_W_f32_1)
    ),
    Cat(
      u_r64_block_vector.io.nxt_f_r_s_o(2).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W),
      u_r64_block_vector.io.nxt_f_r_s_o(1).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W),
      u_r64_block_vector.io.nxt_f_r_s_o(0).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W)
    )
  ).asTypeOf(nxt_f_r_s_f32_1)
  nxt_f_r_s_f16_2 := Cat(
    u_r64_block_vector.io.nxt_f_r_s_o(2).tail(REM_W_f16_2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_s_o(1).tail(REM_W_f16_2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_s_o(0).tail(REM_W_f16_2).head(REM_W_f16_2)
  ).asTypeOf(nxt_f_r_s_f16_2)
  nxt_f_r_s_f16_3 := Cat(
    u_r64_block_vector.io.nxt_f_r_s_o(2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_s_o(1).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_s_o(0).head(REM_W_f16_2)
  ).asTypeOf(nxt_f_r_s_f16_3)
  nxt_f_r_c_f64_0 := Mux(
    fp_format_q_is_fp64 | !is_vec_q,
    Cat(
      u_r64_block_vector.io.nxt_f_r_c_o(2).head(REM_W_f64_0),
      u_r64_block_vector.io.nxt_f_r_c_o(1).head(REM_W_f64_0),
      u_r64_block_vector.io.nxt_f_r_c_o(0).head(REM_W_f64_0)
    ),
    Mux(
      fp_format_q_is_fp32 & is_vec_q,
      Cat(
        u_r64_block_vector.io.nxt_f_r_c_o(2).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W),
        u_r64_block_vector.io.nxt_f_r_c_o(1).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W),
        u_r64_block_vector.io.nxt_f_r_c_o(0).tail(2 * REM_W_f16_2).head(REM_W_f32_1), 0.U((REM_W_f64_0 - REM_W_f32_1).W)
      ),
      Cat(
        u_r64_block_vector.io.nxt_f_r_c_o(2).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W),
        u_r64_block_vector.io.nxt_f_r_c_o(1).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W),
        u_r64_block_vector.io.nxt_f_r_c_o(0).tail(3 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f64_0 - REM_W_f16_2).W)
      )
    )
  ).asTypeOf(nxt_f_r_c_f64_0)
  nxt_f_r_c_f32_1 := Mux(
    fp_format_q_is_fp32 & is_vec_q,
    Cat(
      u_r64_block_vector.io.nxt_f_r_c_o(2).head(REM_W_f32_1),
      u_r64_block_vector.io.nxt_f_r_c_o(1).head(REM_W_f32_1),
      u_r64_block_vector.io.nxt_f_r_c_o(0).head(REM_W_f32_1)
    ),
    Cat(
      u_r64_block_vector.io.nxt_f_r_c_o(2).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W),
      u_r64_block_vector.io.nxt_f_r_c_o(1).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W),
      u_r64_block_vector.io.nxt_f_r_c_o(0).tail(2 * REM_W_f16_2).head(REM_W_f16_2), 0.U((REM_W_f32_1 - REM_W_f16_2).W)
    )
  ).asTypeOf(nxt_f_r_c_f32_1)
  nxt_f_r_c_f16_2 := Cat(
    u_r64_block_vector.io.nxt_f_r_c_o(2).tail(REM_W_f16_2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_c_o(1).tail(REM_W_f16_2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_c_o(0).tail(REM_W_f16_2).head(REM_W_f16_2)
  ).asTypeOf(nxt_f_r_c_f16_2)
  nxt_f_r_c_f16_3 := Cat(
    u_r64_block_vector.io.nxt_f_r_c_o(2).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_c_o(1).head(REM_W_f16_2),
    u_r64_block_vector.io.nxt_f_r_c_o(0).head(REM_W_f16_2)
  ).asTypeOf(nxt_f_r_c_f16_3)
  adder_6b_res_for_nxt_cycle_s0_qds_f64_0 := u_r64_block_vector.io.adder_6b_res_for_nxt_cycle_s0_qds_o(0)
  adder_6b_res_for_nxt_cycle_s0_qds_f32_1 := u_r64_block_vector.io.adder_6b_res_for_nxt_cycle_s0_qds_o(1)
  adder_6b_res_for_nxt_cycle_s0_qds_f16_2 := u_r64_block_vector.io.adder_6b_res_for_nxt_cycle_s0_qds_o(2)
  adder_6b_res_for_nxt_cycle_s0_qds_f16_3 := u_r64_block_vector.io.adder_6b_res_for_nxt_cycle_s0_qds_o(3)
  adder_7b_res_for_nxt_cycle_s1_qds_f64_0 := u_r64_block_vector.io.adder_7b_res_for_nxt_cycle_s1_qds_o(0)
  adder_7b_res_for_nxt_cycle_s1_qds_f32_1 := u_r64_block_vector.io.adder_7b_res_for_nxt_cycle_s1_qds_o(1)
  adder_7b_res_for_nxt_cycle_s1_qds_f16_2 := u_r64_block_vector.io.adder_7b_res_for_nxt_cycle_s1_qds_o(2)
  adder_7b_res_for_nxt_cycle_s1_qds_f16_3 := u_r64_block_vector.io.adder_7b_res_for_nxt_cycle_s1_qds_o(3)

  val nr_f_r_s_f64_0 = Mux1H(
    Seq(
      fp_format_q_is_fp64 | !is_vec_q,
      fp_format_q_is_fp32 & is_vec_q,
      fp_format_q_is_fp16 & is_vec_q
    ),
    Seq(
      f_r_s_q_72bit(71, 12),
      Cat(f_r_s_q_72bit(35, 5), 0.U(29.W)),
      Cat(f_r_s_q_72bit(17, 0), 0.U(42.W))
    )
  )
  val nr_f_r_c_f64_0 = Mux1H(
    Seq(
      fp_format_q_is_fp64 | !is_vec_q,
      fp_format_q_is_fp32 & is_vec_q,
      fp_format_q_is_fp16 & is_vec_q
    ),
    Seq(
      f_r_c_q_72bit(71, 12),
      Cat(f_r_c_q_72bit(35, 5), 0.U(29.W)),
      Cat(f_r_c_q_72bit(17, 0), 0.U(42.W))
    )
  )
  val nr_f_r_s_f32_1 = Mux(fp_format_q_is_fp32 & is_vec_q, f_r_s_q_72bit(71, 41), Cat(f_r_s_q_72bit(35, 18), 0.U(13.W)))
  val nr_f_r_c_f32_1 = Mux(fp_format_q_is_fp32 & is_vec_q, f_r_c_q_72bit(71, 41), Cat(f_r_c_q_72bit(35, 18), 0.U(13.W)))
  val nr_f_r_f64_0 = nr_f_r_s_f64_0 + nr_f_r_c_f64_0
  val nr_f_r_f32_1 = nr_f_r_s_f32_1 + nr_f_r_c_f32_1
  val nr_f_r_f16_2 = f_r_s_q_72bit(53, 36) + f_r_c_q_72bit(53, 36)
  val nr_f_r_f16_3 = f_r_s_q_72bit(71, 54) + f_r_c_q_72bit(71, 54)
  val f_r_xor_f64_0 = nr_f_r_s_f64_0((REM_W_f64_0 - 1) - 1, 1) ^ nr_f_r_c_f64_0((REM_W_f64_0 - 1) - 1, 1)
  val f_r_xor_f32_1 = nr_f_r_s_f32_1((REM_W_f32_1 - 1) - 1, 1) ^ nr_f_r_c_f32_1((REM_W_f32_1 - 1) - 1, 1)
  val f_r_xor_f16_2 = f_r_s_q_72bit(53, 36)((REM_W_f16_2 - 1) - 1, 1) ^ f_r_c_q_72bit(53, 36)((REM_W_f16_2 - 1) - 1, 1)
  val f_r_xor_f16_3 = f_r_s_q_72bit(71, 54)((REM_W_f16_3 - 1) - 1, 1) ^ f_r_c_q_72bit(71, 54)((REM_W_f16_3 - 1) - 1, 1)
  val f_r_or_f64_0 = nr_f_r_s_f64_0((REM_W_f64_0 - 1) - 2, 0) | nr_f_r_c_f64_0((REM_W_f64_0 - 1) - 2, 0)
  val f_r_or_f32_1 = nr_f_r_s_f32_1((REM_W_f32_1 - 1) - 2, 0) | nr_f_r_c_f32_1((REM_W_f32_1 - 1) - 2, 0)
  val f_r_or_f16_2 = f_r_s_q_72bit(53, 36)((REM_W_f16_2 - 1) - 2, 0) | f_r_c_q_72bit(53, 36)((REM_W_f16_2 - 1) - 2, 0)
  val f_r_or_f16_3 = f_r_s_q_72bit(71, 54)((REM_W_f16_3 - 1) - 2, 0) | f_r_c_q_72bit(71, 54)((REM_W_f16_3 - 1) - 2, 0)
  rem_is_not_zero_f64_0 := !opb_is_power_of_2_q(0) & (nr_f_r_f64_0(REM_W_f64_0 - 1) | (f_r_xor_f64_0 =/= f_r_or_f64_0))
  rem_is_not_zero_f32_1 := !opb_is_power_of_2_q(1) & (nr_f_r_f32_1(REM_W_f32_1 - 1) | (f_r_xor_f32_1 =/= f_r_or_f32_1))
  rem_is_not_zero_f16_2 := !opb_is_power_of_2_q(2) & (nr_f_r_f16_2(REM_W_f16_2 - 1) | (f_r_xor_f16_2 =/= f_r_or_f16_2))
  rem_is_not_zero_f16_3 := !opb_is_power_of_2_q(3) & (nr_f_r_f16_3(REM_W_f16_3 - 1) | (f_r_xor_f16_3 =/= f_r_or_f16_3))

  val quo_pre_shift_f64_0 =
    Mux(opb_is_power_of_2_q(0), Cat(quo_iter_q_f64_0(52, 0), 0.U(1.W)),
      Cat(nxt_quo_iter_f64_0(0)(54, 26), Mux(fp_format_q_is_fp32, nxt_quo_iter_f64_0(0)(24, 0), nxt_quo_iter_f64_0(0)(25, 1)))
    )
  val quo_pre_shift_f32_1 = Mux(
    opb_is_power_of_2_q(1),
    Cat(quo_iter_q_f32_1(23, 0), 0.U(1.W)),
    Mux(fp_format_q_is_fp32 & is_vec_q, nxt_quo_iter_f32_1(0)(24, 0), nxt_quo_iter_f32_1(0)(25, 1))

  )
  val quo_pre_shift_f16_2 = Mux(opb_is_power_of_2_q(2), Cat(0.U(1.W), quo_iter_q_f16_2(10, 0), 0.U(1.W)), nxt_quo_iter_f16_2(0)(13, 1))
  val quo_pre_shift_f16_3 = Mux(opb_is_power_of_2_q(3), Cat(0.U(1.W), quo_iter_q_f16_3(10, 0), 0.U(1.W)), nxt_quo_iter_f16_3(0)(13, 1))
  val quo_m1_pre_shift_f64_0 = Cat(nxt_quo_m1_iter_f64_0(0)(54, 26), Mux(fp_format_q_is_fp32, nxt_quo_m1_iter_f64_0(0)(24, 0), nxt_quo_m1_iter_f64_0(0)(25, 1)))
  val quo_m1_pre_shift_f32_1 = Mux(fp_format_q_is_fp32 & is_vec_q, nxt_quo_m1_iter_f32_1(0)(24, 0), nxt_quo_m1_iter_f32_1(0)(25, 1))
  val quo_m1_pre_shift_f16_2 = nxt_quo_m1_iter_f16_2(0)(13, 1)
  val quo_m1_pre_shift_f16_3 = nxt_quo_m1_iter_f16_3(0)(13, 1)

  val r_shift_num_pre_f64_0 = 1.U(13.W) - out_exp_diff_q_f64_0
  val r_shift_num_pre_f32_1 = 1.U(10.W) - out_exp_diff_q_f32_1
  val r_shift_num_pre_f16_2 = 1.U(7.W) - out_exp_diff_q_f16_2
  val r_shift_num_pre_f16_3 = 1.U(7.W) - out_exp_diff_q_f16_3
  val r_shift_num_pre_minus_limit_f64_0 = 1.U(13.W) - out_exp_diff_q_f64_0 -
    Mux1H(
      Seq(
        fp_format_q_is_fp64 | !is_vec_q,
        fp_format_q_is_fp32 & is_vec_q,
        fp_format_q_is_fp16 & is_vec_q
      ),
      Seq(
        R_SHIFT_NUM_LIMIT_f64_0,
        R_SHIFT_NUM_LIMIT_f32_1,
        R_SHIFT_NUM_LIMIT_f16_2
      )
    )
  val r_shift_num_pre_minus_limit_f32_1 = 1.U(13.W) - out_exp_diff_q_f32_1 -
    Mux(fp_format_q_is_fp32 & is_vec_q, R_SHIFT_NUM_LIMIT_f32_1, R_SHIFT_NUM_LIMIT_f16_2)
  val r_shift_num_pre_minus_limit_f16_2 = 1.U(13.W) - out_exp_diff_q_f16_2 - R_SHIFT_NUM_LIMIT_f16_2
  val r_shift_num_pre_minus_limit_f16_3 = 1.U(13.W) - out_exp_diff_q_f16_3 - R_SHIFT_NUM_LIMIT_f16_3
  val r_shift_num_f64_0 = Mux(r_shift_num_pre_f64_0(12), 0.U(6.W), Mux(!r_shift_num_pre_minus_limit_f64_0(12),
    Mux1H(
      Seq(
        fp_format_q_is_fp64 | !is_vec_q,
        fp_format_q_is_fp32 & is_vec_q,
        fp_format_q_is_fp16 & is_vec_q
      ),
      Seq(
        R_SHIFT_NUM_LIMIT_f64_0,
        R_SHIFT_NUM_LIMIT_f32_1,
        R_SHIFT_NUM_LIMIT_f16_2
      )
    ),
    r_shift_num_pre_f64_0(5, 0)))
  val r_shift_num_f32_1 = Mux(r_shift_num_pre_f32_1(9), 0.U(5.W), Mux(!r_shift_num_pre_minus_limit_f32_1(9),
    Mux(fp_format_q_is_fp32 & is_vec_q, R_SHIFT_NUM_LIMIT_f32_1, R_SHIFT_NUM_LIMIT_f16_2),
    r_shift_num_pre_f32_1(4, 0)))
  val r_shift_num_f16_2 = Mux(r_shift_num_pre_f16_2(6), 0.U(4.W), Mux(!r_shift_num_pre_minus_limit_f16_2(6), R_SHIFT_NUM_LIMIT_f16_2, r_shift_num_pre_f16_2(3, 0)))
  val r_shift_num_f16_3 = Mux(r_shift_num_pre_f16_3(6), 0.U(4.W), Mux(!r_shift_num_pre_minus_limit_f16_3(6), R_SHIFT_NUM_LIMIT_f16_3, r_shift_num_pre_f16_3(3, 0)))
  val quo_r_shifted_f64_0 = Cat(quo_pre_shift_f64_0, 0.U(54.W)) >> r_shift_num_f64_0
  val quo_r_shifted_f32_1 = Cat(quo_pre_shift_f32_1, 0.U(25.W)) >> r_shift_num_f32_1
  val quo_r_shifted_f16_2 = Cat(quo_pre_shift_f16_2, 0.U(13.W)) >> r_shift_num_f16_2
  val quo_r_shifted_f16_3 = Cat(quo_pre_shift_f16_3, 0.U(13.W)) >> r_shift_num_f16_3
  val quo_m1_r_shifted_f64_0 = Cat(quo_m1_pre_shift_f64_0, 0.U(54.W)) >> r_shift_num_f64_0
  val quo_m1_r_shifted_f32_1 = Cat(quo_m1_pre_shift_f32_1, 0.U(25.W)) >> r_shift_num_f32_1
  val quo_m1_r_shifted_f16_2 = Cat(quo_m1_pre_shift_f16_2, 0.U(13.W)) >> r_shift_num_f16_2
  val quo_m1_r_shifted_f16_3 = Cat(quo_m1_pre_shift_f16_3, 0.U(13.W)) >> r_shift_num_f16_3
  val select_quo_m1_f64_0 = nr_f_r_f64_0(REM_W_f64_0 - 1) & !opb_is_power_of_2_q(0)

  val select_quo_m1_f32_1 = nr_f_r_f32_1(REM_W_f32_1 - 1) & !opb_is_power_of_2_q(1)
  val select_quo_m1_f16_2 = nr_f_r_f16_2(REM_W_f16_2 - 1) & !opb_is_power_of_2_q(2)
  val select_quo_m1_f16_3 = nr_f_r_f16_3(REM_W_f16_3 - 1) & !opb_is_power_of_2_q(3)
  correct_quo_r_shifted_f64_0 := Mux(select_quo_m1_f64_0, quo_m1_r_shifted_f64_0(107, 54), quo_r_shifted_f64_0(107, 54))
  correct_quo_r_shifted_f32_1 := Mux(select_quo_m1_f32_1, quo_m1_r_shifted_f32_1(49, 25), quo_r_shifted_f32_1(49, 25))
  correct_quo_r_shifted_f16_2 := Mux(select_quo_m1_f16_2, quo_m1_r_shifted_f16_2(25, 13), quo_r_shifted_f16_2(25, 13))
  correct_quo_r_shifted_f16_3 := Mux(select_quo_m1_f16_3, quo_m1_r_shifted_f16_3(25, 13), quo_r_shifted_f16_3(25, 13))
  sticky_without_rem_f64_0 := Mux(select_quo_m1_f64_0, quo_m1_r_shifted_f64_0(53, 0), quo_r_shifted_f64_0(53, 0))
  sticky_without_rem_f32_1 := Mux(select_quo_m1_f32_1, quo_m1_r_shifted_f32_1(24, 0), quo_r_shifted_f32_1(24, 0))
  sticky_without_rem_f16_2 := Mux(select_quo_m1_f16_2, quo_m1_r_shifted_f16_2(12, 0), quo_r_shifted_f16_2(12, 0))
  sticky_without_rem_f16_3 := Mux(select_quo_m1_f16_3, quo_m1_r_shifted_f16_3(12, 0), quo_r_shifted_f16_3(12, 0))

  val quo_pre_inc_f64_0 = Mux(
    fsm_q(FSM_POST_0_BIT),
    Cat(
      quo_pre_shift_f64_0(52, 25),
      !fp_format_q_is_fp32 & quo_pre_shift_f64_0(24),
      quo_pre_shift_f64_0(23, 12),
      !fp_format_q_is_fp16 & quo_pre_shift_f64_0(11),
      quo_pre_shift_f64_0(10, 1)
    ),
    Cat(
      quo_m1_iter_q_f64_0(52, 25),
      !fp_format_q_is_fp32 & quo_m1_iter_q_f64_0(24),
      quo_m1_iter_q_f64_0(23, 12),
      !fp_format_q_is_fp16 & quo_m1_iter_q_f64_0(11),
      quo_m1_iter_q_f64_0(10, 1)
    )
  )
  val quo_pre_inc_f32_1 = Mux(
    fsm_q(FSM_POST_0_BIT),
    Cat(
      quo_pre_shift_f32_1(23, 12),
      !fp_format_q_is_fp16 & quo_pre_shift_f32_1(11),
      quo_pre_shift_f32_1(10, 1)
    ),
    Cat(
      quo_m1_iter_q_f32_1(23, 12),
      !fp_format_q_is_fp16 & quo_m1_iter_q_f32_1(11),
      quo_m1_iter_q_f32_1(10, 1)
    )
  )
  val quo_pre_inc_f16_2 = Mux(
    fsm_q(FSM_POST_0_BIT),
    quo_pre_shift_f16_2(10, 1),
    quo_m1_iter_q_f16_2(10, 1)
  )
  val quo_pre_inc_f16_3 = Mux(
    fsm_q(FSM_POST_0_BIT),
    quo_pre_shift_f16_3(10, 1),
    quo_m1_iter_q_f16_3(10, 1)
  )
  val quo_m1_pre_inc_f64_0 = Cat(
    quo_m1_pre_shift_f64_0(52, 25),
    !fp_format_q_is_fp32 & quo_m1_pre_shift_f64_0(24),
    quo_m1_pre_shift_f64_0(23, 12),
    !fp_format_q_is_fp16 & quo_m1_pre_shift_f64_0(11),
    quo_m1_pre_shift_f64_0(10, 1)
  )
  val quo_m1_pre_inc_f32_1 = Cat(
    quo_m1_pre_shift_f32_1(23, 12),
    !fp_format_q_is_fp16 & quo_m1_pre_shift_f32_1(11),
    quo_m1_pre_shift_f32_1(10, 1)
  )
  val quo_m1_pre_inc_f16_2 = quo_m1_pre_shift_f16_2(10, 1)
  val quo_m1_pre_inc_f16_3 = quo_m1_pre_shift_f16_3(10, 1)
  val quo_inc_res_f64_0 = Cat(0.U(1.W), quo_pre_inc_f64_0(51, 0)) + Cat(0.U(52.W), 1.U(1.W))
  val quo_inc_res_f32_1 = Cat(0.U(1.W), quo_pre_inc_f32_1(22, 0)) + Cat(0.U(23.W), 1.U(1.W))
  val quo_inc_res_f16_2 = Cat(0.U(1.W), quo_pre_inc_f16_2(9, 0)) + Cat(0.U(10.W), 1.U(1.W))
  val quo_inc_res_f16_3 = Cat(0.U(1.W), quo_pre_inc_f16_3(9, 0)) + Cat(0.U(10.W), 1.U(1.W))
  val quo_m1_inc_res_f64_0 = Mux(quo_pre_inc_f64_0(0) === quo_m1_pre_inc_f64_0(0), quo_inc_res_f64_0(51, 0), quo_pre_inc_f64_0(51, 0))
  val quo_m1_inc_res_f32_1 = Mux(quo_pre_inc_f32_1(0) === quo_m1_pre_inc_f32_1(0), quo_inc_res_f32_1(22, 0), quo_pre_inc_f32_1(22, 0))
  val quo_m1_inc_res_f16_2 = Mux(quo_pre_inc_f16_2(0) === quo_m1_pre_inc_f16_2(0), quo_inc_res_f16_2(9, 0), quo_pre_inc_f16_2(9, 0))
  val quo_m1_inc_res_f16_3 = Mux(quo_pre_inc_f16_3(0) === quo_m1_pre_inc_f16_3(0), quo_inc_res_f16_3(9, 0), quo_pre_inc_f16_3(9, 0))

  val guard_bit_quo_f64_0 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f64_0, quo_pre_shift_f64_0(1), quo_m1_iter_q_f64_0(1))
  val guard_bit_quo_f32_1 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f32_1, quo_pre_shift_f32_1(1), quo_m1_iter_q_f32_1(1))
  val guard_bit_quo_f16_2 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_2, quo_pre_shift_f16_2(1), quo_m1_iter_q_f16_2(1))
  val guard_bit_quo_f16_3 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_3, quo_pre_shift_f16_3(1), quo_m1_iter_q_f16_3(1))
  val round_bit_quo_f64_0 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f64_0, quo_pre_shift_f64_0(0), quo_m1_iter_q_f64_0(0))
  val round_bit_quo_f32_1 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f32_1, quo_pre_shift_f32_1(0), quo_m1_iter_q_f32_1(0))
  val round_bit_quo_f16_2 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_2, quo_pre_shift_f16_2(0), quo_m1_iter_q_f16_2(0))
  val round_bit_quo_f16_3 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_3, quo_pre_shift_f16_3(0), quo_m1_iter_q_f16_3(0))
  val sticky_bit_quo_f64_0 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f64_0, rem_is_not_zero_f64_0, quo_m1_iter_q_f64_0(53) | (frac_divisor_q_f64_0(56, 0) =/= 0.U(57.W)))
  val sticky_bit_quo_f32_1 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f32_1, rem_is_not_zero_f32_1, quo_m1_iter_q_f32_1(24) | (frac_divisor_q_f32_1(24, 0) =/= 0.U(25.W)))

  val sticky_bit_quo_f16_2 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_2, rem_is_not_zero_f16_2, quo_m1_iter_q_f16_2(11) | (frac_divisor_q_f16_2(14, 0) =/= 0.U(15.W)))
  val sticky_bit_quo_f16_3 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_3, rem_is_not_zero_f16_3, quo_m1_iter_q_f16_3(11) | (frac_divisor_q_f16_3(14, 0) =/= 0.U(15.W)))
  val quo_need_rup_f64_0 =
    ((rm_q === RM_RNE) & ((round_bit_quo_f64_0 & sticky_bit_quo_f64_0) | (guard_bit_quo_f64_0 & round_bit_quo_f64_0))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_f64_0 | sticky_bit_quo_f64_0) & out_sign_q(0))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_f64_0 | sticky_bit_quo_f64_0) & ~out_sign_q(0))) |
      ((rm_q === RM_RMM) & round_bit_quo_f64_0)
  val quo_need_rup_f32_1 =
    ((rm_q === RM_RNE) & ((round_bit_quo_f32_1 & sticky_bit_quo_f32_1) | (guard_bit_quo_f32_1 & round_bit_quo_f32_1))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_f32_1 | sticky_bit_quo_f32_1) & out_sign_q(1))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_f32_1 | sticky_bit_quo_f32_1) & ~out_sign_q(1))) |
      ((rm_q === RM_RMM) & round_bit_quo_f32_1)
  val quo_need_rup_f16_2 =
    ((rm_q === RM_RNE) & ((round_bit_quo_f16_2 & sticky_bit_quo_f16_2) | (guard_bit_quo_f16_2 & round_bit_quo_f16_2))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_f16_2 | sticky_bit_quo_f16_2) & out_sign_q(2))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_f16_2 | sticky_bit_quo_f16_2) & ~out_sign_q(2))) |
      ((rm_q === RM_RMM) & round_bit_quo_f16_2)
  val quo_need_rup_f16_3 =
    ((rm_q === RM_RNE) & ((round_bit_quo_f16_3 & sticky_bit_quo_f16_3) | (guard_bit_quo_f16_3 & round_bit_quo_f16_3))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_f16_3 | sticky_bit_quo_f16_3) & out_sign_q(3))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_f16_3 | sticky_bit_quo_f16_3) & ~out_sign_q(3))) |
      ((rm_q === RM_RMM) & round_bit_quo_f16_3)
  val inexact_quo_f64_0 = round_bit_quo_f64_0 | sticky_bit_quo_f64_0
  val inexact_quo_f32_1 = round_bit_quo_f32_1 | sticky_bit_quo_f32_1
  val inexact_quo_f16_2 = round_bit_quo_f16_2 | sticky_bit_quo_f16_2
  val inexact_quo_f16_3 = round_bit_quo_f16_3 | sticky_bit_quo_f16_3
  val guard_bit_quo_m1_f64_0 = quo_m1_pre_shift_f64_0(1)
  val guard_bit_quo_m1_f32_1 = quo_m1_pre_shift_f32_1(1)
  val guard_bit_quo_m1_f16_2 = quo_m1_pre_shift_f16_2(1)
  val guard_bit_quo_m1_f16_3 = quo_m1_pre_shift_f16_3(1)
  val round_bit_quo_m1_f64_0 = quo_m1_pre_shift_f64_0(0)
  val round_bit_quo_m1_f32_1 = quo_m1_pre_shift_f32_1(0)
  val round_bit_quo_m1_f16_2 = quo_m1_pre_shift_f16_2(0)
  val round_bit_quo_m1_f16_3 = quo_m1_pre_shift_f16_3(0)
  val sticky_bit_quo_m1_f64_0 = true.B
  val sticky_bit_quo_m1_f32_1 = true.B
  val sticky_bit_quo_m1_f16_2 = true.B
  val sticky_bit_quo_m1_f16_3 = true.B

  val guard_correct_f64_0 = correct_quo_r_shifted_f64_0(0)
  val guard_correct_f32_1 = correct_quo_r_shifted_f32_1(0)
  val guard_correct_f16_2 = correct_quo_r_shifted_f16_2(0)
  val guard_correct_f16_3 = correct_quo_r_shifted_f16_3(0)
  val round_correct_f64_0 = sticky_without_rem_f64_0.head(1).asBool
  val round_correct_f32_1 = sticky_without_rem_f32_1.head(1).asBool
  val round_correct_f16_2 = sticky_without_rem_f16_2.head(1).asBool
  val round_correct_f16_3 = sticky_without_rem_f16_3.head(1).asBool
  val sticky_correct_f64_0 = Mux(select_quo_m1_f64_0, true.B, (sticky_without_rem_f64_0.tail(1) =/= 0.U) | rem_is_not_zero_f64_0)
  val sticky_correct_f32_1 = Mux(select_quo_m1_f32_1, true.B, (sticky_without_rem_f32_1.tail(1) =/= 0.U) | rem_is_not_zero_f32_1)
  val sticky_correct_f16_2 = Mux(select_quo_m1_f16_2, true.B, (sticky_without_rem_f16_2.tail(1) =/= 0.U) | rem_is_not_zero_f16_2)
  val sticky_correct_f16_3 = Mux(select_quo_m1_f16_3, true.B, (sticky_without_rem_f16_3.tail(1) =/= 0.U) | rem_is_not_zero_f16_3)
  val test_uf_need_rup_correct_f64_0 =
    ((rm_q === RM_RNE) & ((round_correct_f64_0 & sticky_correct_f64_0) | (guard_correct_f64_0 & round_correct_f64_0))) |
      ((rm_q === RM_RDN) & ((round_correct_f64_0 | sticky_correct_f64_0) & out_sign_q(0))) |
      ((rm_q === RM_RUP) & ((round_correct_f64_0 | sticky_correct_f64_0) & ~out_sign_q(0))) |
      ((rm_q === RM_RMM) & round_correct_f64_0)
  val test_uf_need_rup_correct_f32_1 =
    ((rm_q === RM_RNE) & ((round_correct_f32_1 & sticky_correct_f32_1) | (guard_correct_f32_1 & round_correct_f32_1))) |
      ((rm_q === RM_RDN) & ((round_correct_f32_1 | sticky_correct_f32_1) & out_sign_q(1))) |
      ((rm_q === RM_RUP) & ((round_correct_f32_1 | sticky_correct_f32_1) & ~out_sign_q(1))) |
      ((rm_q === RM_RMM) & round_correct_f32_1)
  val test_uf_need_rup_correct_f16_2 =
    ((rm_q === RM_RNE) & ((round_correct_f16_2 & sticky_correct_f16_2) | (guard_correct_f16_2 & round_correct_f16_2))) |
      ((rm_q === RM_RDN) & ((round_correct_f16_2 | sticky_correct_f16_2) & out_sign_q(2))) |
      ((rm_q === RM_RUP) & ((round_correct_f16_2 | sticky_correct_f16_2) & ~out_sign_q(2))) |
      ((rm_q === RM_RMM) & round_correct_f16_2)
  val test_uf_need_rup_correct_f16_3 =
    ((rm_q === RM_RNE) & ((round_correct_f16_3 & sticky_correct_f16_3) | (guard_correct_f16_3 & round_correct_f16_3))) |
      ((rm_q === RM_RDN) & ((round_correct_f16_3 | sticky_correct_f16_3) & out_sign_q(3))) |
      ((rm_q === RM_RUP) & ((round_correct_f16_3 | sticky_correct_f16_3) & ~out_sign_q(3))) |
      ((rm_q === RM_RMM) & round_correct_f16_3)

  val quo_m1_need_rup_f64_0 =
    ((rm_q === RM_RNE) & ((round_bit_quo_m1_f64_0 & sticky_bit_quo_m1_f64_0) | (guard_bit_quo_m1_f64_0 & round_bit_quo_m1_f64_0))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_m1_f64_0 | sticky_bit_quo_m1_f64_0) & out_sign_q(0))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_m1_f64_0 | sticky_bit_quo_m1_f64_0) & ~out_sign_q(0))) |
      ((rm_q === RM_RMM) & round_bit_quo_m1_f64_0)
  val quo_m1_need_rup_f32_1 =
    ((rm_q === RM_RNE) & ((round_bit_quo_m1_f32_1 & sticky_bit_quo_m1_f32_1) | (guard_bit_quo_m1_f32_1 & round_bit_quo_m1_f32_1))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_m1_f32_1 | sticky_bit_quo_m1_f32_1) & out_sign_q(1))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_m1_f32_1 | sticky_bit_quo_m1_f32_1) & ~out_sign_q(1))) |
      ((rm_q === RM_RMM) & round_bit_quo_m1_f32_1)
  val quo_m1_need_rup_f16_2 =
    ((rm_q === RM_RNE) & ((round_bit_quo_m1_f16_2 & sticky_bit_quo_m1_f16_2) | (guard_bit_quo_m1_f16_2 & round_bit_quo_m1_f16_2))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_m1_f16_2 | sticky_bit_quo_m1_f16_2) & out_sign_q(2))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_m1_f16_2 | sticky_bit_quo_m1_f16_2) & ~out_sign_q(2))) |
      ((rm_q === RM_RMM) & round_bit_quo_m1_f16_2)
  val quo_m1_need_rup_f16_3 =
    ((rm_q === RM_RNE) & ((round_bit_quo_m1_f16_3 & sticky_bit_quo_m1_f16_3) | (guard_bit_quo_m1_f16_3 & round_bit_quo_m1_f16_3))) |
      ((rm_q === RM_RDN) & ((round_bit_quo_m1_f16_3 | sticky_bit_quo_m1_f16_3) & out_sign_q(3))) |
      ((rm_q === RM_RUP) & ((round_bit_quo_m1_f16_3 | sticky_bit_quo_m1_f16_3) & ~out_sign_q(3))) |
      ((rm_q === RM_RMM) & round_bit_quo_m1_f16_3)
  val inexact_quo_m1_f64_0 = true.B
  val inexact_quo_m1_f32_1 = true.B
  val inexact_quo_m1_f16_2 = true.B
  val inexact_quo_m1_f16_3 = true.B
  val quo_rounded_f64_0 = Mux(quo_need_rup_f64_0, quo_inc_res_f64_0(52, 0), Cat(0.U(1.W), quo_pre_inc_f64_0))
  val quo_rounded_f32_1 = Mux(quo_need_rup_f32_1, quo_inc_res_f32_1(23, 0), Cat(0.U(1.W), quo_pre_inc_f32_1))
  val quo_rounded_f16_2 = Mux(quo_need_rup_f16_2, quo_inc_res_f16_2(10, 0), Cat(0.U(1.W), quo_pre_inc_f16_2))
  val quo_rounded_f16_3 = Mux(quo_need_rup_f16_3, quo_inc_res_f16_3(10, 0), Cat(0.U(1.W), quo_pre_inc_f16_3))
  val quo_m1_rounded_f64_0 = Mux(quo_m1_need_rup_f64_0, quo_m1_inc_res_f64_0, quo_m1_pre_inc_f64_0)
  val quo_m1_rounded_f32_1 = Mux(quo_m1_need_rup_f32_1, quo_m1_inc_res_f32_1, quo_m1_pre_inc_f32_1)
  val quo_m1_rounded_f16_2 = Mux(quo_m1_need_rup_f16_2, quo_m1_inc_res_f16_2, quo_m1_pre_inc_f16_2)
  val quo_m1_rounded_f16_3 = Mux(quo_m1_need_rup_f16_3, quo_m1_inc_res_f16_3, quo_m1_pre_inc_f16_3)
  val inexact_f64_0 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f64_0, select_quo_m1_f64_0 | inexact_quo_f64_0, inexact_quo_f64_0)
  val inexact_f32_1 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f32_1, select_quo_m1_f32_1 | inexact_quo_f32_1, inexact_quo_f32_1)
  val inexact_f16_2 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_2, select_quo_m1_f16_2 | inexact_quo_f16_2, inexact_quo_f16_2)
  val inexact_f16_3 = Mux(fsm_q(FSM_POST_0_BIT) | !res_is_denormal_f16_3, select_quo_m1_f16_3 | inexact_quo_f16_3, inexact_quo_f16_3)
  val frac_rounded_post_0_f64_0 = Mux(select_quo_m1_f64_0, quo_m1_rounded_f64_0(51, 0), quo_rounded_f64_0(51, 0))
  val frac_rounded_post_0_f32_1 = Mux(select_quo_m1_f32_1, quo_m1_rounded_f32_1(22, 0), quo_rounded_f32_1(22, 0))
  val frac_rounded_post_0_f16_2 = Mux(select_quo_m1_f16_2, quo_m1_rounded_f16_2(9, 0), quo_rounded_f16_2(9, 0))
  val frac_rounded_post_0_f16_3 = Mux(select_quo_m1_f16_3, quo_m1_rounded_f16_3(9, 0), quo_rounded_f16_3(9, 0))
  val carry_after_round_f64_0 = Mux(fp_format_q_is_fp16, quo_rounded_f64_0(10), Mux(fp_format_q_is_fp32, quo_rounded_f64_0(23), quo_rounded_f64_0(52)))
  val carry_after_round_f32_1 = Mux(fp_format_q_is_fp16, quo_rounded_f32_1(10), quo_rounded_f32_1(23))
  val carry_after_round_f16_2 = quo_rounded_f16_2(10)
  val carry_after_round_f16_3 = quo_rounded_f16_3(10)
  val overflow_f64_0 = out_exp_diff_q_f64_0(11, 0) >= Mux(fp_format_q_is_fp16, 31.U(12.W), Mux(fp_format_q_is_fp32, 255.U(12.W), 2047.U(12.W)))
  val overflow_f32_1 = out_exp_diff_q_f32_1(9, 0) >= Mux(fp_format_q_is_fp16, 31.U(10.W), 255.U(10.W))
  val overflow_f16_2 = out_exp_diff_q_f16_2(6, 0) >= 31.U(7.W)
  val overflow_f16_3 = out_exp_diff_q_f16_3(6, 0) >= 31.U(7.W)
  val overflow_to_inf_f64_0 =
    (rm_q === RM_RNE) |
      (rm_q === RM_RMM) |
      ((rm_q === RM_RUP) & ~out_sign_q(0)) |
      ((rm_q === RM_RDN) & out_sign_q(0))
  val overflow_to_inf_f32_1 =
    (rm_q === RM_RNE) |
      (rm_q === RM_RMM) |
      ((rm_q === RM_RUP) & ~out_sign_q(1)) |
      ((rm_q === RM_RDN) & out_sign_q(1))
  val overflow_to_inf_f16_2 =
    (rm_q === RM_RNE) |
      (rm_q === RM_RMM) |
      ((rm_q === RM_RUP) & ~out_sign_q(2)) |
      ((rm_q === RM_RDN) & out_sign_q(2))
  val overflow_to_inf_f16_3 =
    (rm_q === RM_RNE) |
      (rm_q === RM_RMM) |
      ((rm_q === RM_RUP) & ~out_sign_q(3)) |
      ((rm_q === RM_RDN) & out_sign_q(3))
  val out_exp_post_0_f16_0 =
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, Fill(5, 1.U(1.W)),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Cat(Fill(4, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f64_0(4, 0)
      )
    )
  val out_exp_post_0_f16_1 =
    Mux(overflow_f32_1 & overflow_to_inf_f32_1, Fill(5, 1.U(1.W)),
      Mux(overflow_f32_1 & ~overflow_to_inf_f32_1, Cat(Fill(4, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f32_1(4, 0)
      )
    )
  val out_exp_post_0_f16_2 =
    Mux(overflow_f16_2 & overflow_to_inf_f16_2, Fill(5, 1.U(1.W)),
      Mux(overflow_f16_2 & ~overflow_to_inf_f16_2, Cat(Fill(4, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f16_2(4, 0)
      )
    )
  val out_exp_post_0_f16_3 =
    Mux(overflow_f16_3 & overflow_to_inf_f16_3, Fill(5, 1.U(1.W)),
      Mux(overflow_f16_3 & ~overflow_to_inf_f16_3, Cat(Fill(4, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f16_3(4, 0)
      )
    )
  val out_exp_post_0_f32_0 =
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, Fill(8, 1.U(1.W)),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Cat(Fill(7, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f64_0(7, 0)
      )
    )
  val out_exp_post_0_f32_1 =
    Mux(overflow_f32_1 & overflow_to_inf_f32_1, Fill(8, 1.U(1.W)),
      Mux(overflow_f32_1 & ~overflow_to_inf_f32_1, Cat(Fill(7, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f32_1(7, 0)
      )
    )
  val out_exp_post_0_f64_0 =
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, Fill(11, 1.U(1.W)),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Cat(Fill(10, 1.U(1.W)), 0.U(1.W)),
        out_exp_diff_q_f64_0(10, 0)
      )
    )
  out_frac_post_0_f16_0 :=
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, 0.U(10.W),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Fill(10, 1.U(1.W)),
        frac_rounded_post_0_f64_0(9, 0)
      )
    )
  out_frac_post_0_f16_1 :=
    Mux(overflow_f32_1 & overflow_to_inf_f32_1, 0.U(10.W),
      Mux(overflow_f32_1 & ~overflow_to_inf_f32_1, Fill(10, 1.U(1.W)),
        frac_rounded_post_0_f32_1(9, 0)
      )
    )
  out_frac_post_0_f16_2 :=
    Mux(overflow_f16_2 & overflow_to_inf_f16_2, 0.U(10.W),
      Mux(overflow_f16_2 & ~overflow_to_inf_f16_2, Fill(10, 1.U(1.W)),
        frac_rounded_post_0_f16_2(9, 0)
      )
    )
  out_frac_post_0_f16_3 :=
    Mux(overflow_f16_3 & overflow_to_inf_f16_3, 0.U(10.W),
      Mux(overflow_f16_3 & ~overflow_to_inf_f16_3, Fill(10, 1.U(1.W)),
        frac_rounded_post_0_f16_3(9, 0)
      )
    )
  out_frac_post_0_f32_0 :=
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, 0.U(23.W),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Fill(23, 1.U(1.W)),
        frac_rounded_post_0_f64_0(22, 0)
      )
    )
  out_frac_post_0_f32_1 :=
    Mux(overflow_f32_1 & overflow_to_inf_f32_1, 0.U(23.W),
      Mux(overflow_f32_1 & ~overflow_to_inf_f32_1, Fill(23, 1.U(1.W)),
        frac_rounded_post_0_f32_1(22, 0)
      )
    )
  out_frac_post_0_f64_0 :=
    Mux(overflow_f64_0 & overflow_to_inf_f64_0, 0.U(52.W),
      Mux(overflow_f64_0 & ~overflow_to_inf_f64_0, Fill(52, 1.U(1.W)),
        frac_rounded_post_0_f64_0(51, 0)
      )
    )
  val res_post_0_f16_0 = Cat(out_sign_q(0), out_exp_post_0_f16_0, out_frac_post_0_f16_0)
  val res_post_0_f16_1 = Cat(out_sign_q(1), out_exp_post_0_f16_1, out_frac_post_0_f16_1)
  val res_post_0_f16_2 = Cat(out_sign_q(2), out_exp_post_0_f16_2, out_frac_post_0_f16_2)
  val res_post_0_f16_3 = Cat(out_sign_q(3), out_exp_post_0_f16_3, out_frac_post_0_f16_3)
  val res_post_0_f32_0 = Cat(out_sign_q(0), out_exp_post_0_f32_0, out_frac_post_0_f32_0)
  val res_post_0_f32_1 = Cat(out_sign_q(1), out_exp_post_0_f32_1, out_frac_post_0_f32_1)
  val res_post_0_f64_0 = Cat(out_sign_q(0), out_exp_post_0_f64_0, out_frac_post_0_f64_0)
  val fpdiv_res_post_0 = Cat(
    res_post_0_f64_0(63, 32),
    Mux(fp_format_q_is_fp32, res_post_0_f32_0(31, 16), res_post_0_f64_0(31, 16)),
    Mux(fp_format_q_is_fp16, res_post_0_f16_0(15, 0), Mux(fp_format_q_is_fp32, res_post_0_f32_0(15, 0), res_post_0_f64_0(15, 0)))
  )
  val out_exp_post_1_f16_0 =
    Mux(
      res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
      Mux(res_is_nan_q(0) | res_is_inf_q(0), Fill(5, 1.U(1.W)),
        Mux(res_is_exact_zero_q(0), 0.U(5.W),
          Cat(0.U(4.W), carry_after_round_f64_0)
        )
      ),
      out_exp_post_0_f16_0
    )
  val out_exp_post_1_f16_1 =
    Mux(
      res_is_denormal_f32_1 | res_is_nan_q(1) | res_is_inf_q(1) | res_is_exact_zero_q(1),
      Mux(res_is_nan_q(1) | res_is_inf_q(1), Fill(5, 1.U(1.W)),
        Mux(res_is_exact_zero_q(1), 0.U(5.W),
          Cat(0.U(4.W), carry_after_round_f32_1)
        )
      ),
      out_exp_post_0_f16_1
    )
  val out_exp_post_1_f16_2 =
    Mux(
      res_is_denormal_f16_2 | res_is_nan_q(2) | res_is_inf_q(2) | res_is_exact_zero_q(2),
      Mux(res_is_nan_q(2) | res_is_inf_q(2), Fill(5, 1.U(1.W)),
        Mux(res_is_exact_zero_q(2), 0.U(5.W),
          Cat(0.U(4.W), carry_after_round_f16_2)
        )
      ),
      out_exp_post_0_f16_2
    )
  val out_exp_post_1_f16_3 =
    Mux(
      res_is_denormal_f16_3 | res_is_nan_q(3) | res_is_inf_q(3) | res_is_exact_zero_q(3),
      Mux(res_is_nan_q(3) | res_is_inf_q(3), Fill(5, 1.U(1.W)),
        Mux(res_is_exact_zero_q(3), 0.U(5.W),
          Cat(0.U(4.W), carry_after_round_f16_3)
        )
      ),
      out_exp_post_0_f16_3
    )
  val out_exp_post_1_f32_0 =
    Mux(
      res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
      Mux(res_is_nan_q(0) | res_is_inf_q(0), Fill(8, 1.U(1.W)),
        Mux(res_is_exact_zero_q(0), 0.U(8.W),
          Cat(0.U(7.W), carry_after_round_f64_0)
        )
      ),
      out_exp_post_0_f32_0
    )
  val out_exp_post_1_f32_1 =
    Mux(
      res_is_denormal_f32_1 | res_is_nan_q(1) | res_is_inf_q(1) | res_is_exact_zero_q(1),
      Mux(res_is_nan_q(1) | res_is_inf_q(1), Fill(8, 1.U(1.W)),
        Mux(res_is_exact_zero_q(1), 0.U(8.W),
          Cat(0.U(7.W), carry_after_round_f32_1)
        )
      ),
      out_exp_post_0_f32_1
    )
  val out_exp_post_1_f64_0 =
    Mux(
      res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
      Mux(res_is_nan_q(0) | res_is_inf_q(0), Fill(11, 1.U(1.W)),
        Mux(res_is_exact_zero_q(0), 0.U(11.W),
          Cat(0.U(10.W), carry_after_round_f64_0)
        )
      ),
      out_exp_post_0_f64_0
    )

  val out_frac_post_1_f16_0 = Mux(
    res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
    Mux(res_is_nan_q(0), Cat(1.U(1.W), 0.U(9.W)),
      Mux(res_is_inf_q(0) | res_is_exact_zero_q(0), 0.U(10.W),
        quo_rounded_f64_0(9, 0)
      )
    ),
    quo_m1_iter_q_f64_0(9, 0)
  )
  val out_frac_post_1_f16_1 = Mux(
    res_is_denormal_f32_1 | res_is_nan_q(1) | res_is_inf_q(1) | res_is_exact_zero_q(1),
    Mux(res_is_nan_q(1), Cat(1.U(1.W), 0.U(9.W)),
      Mux(res_is_inf_q(1) | res_is_exact_zero_q(1), 0.U(10.W),
        quo_rounded_f32_1(9, 0)
      )
    ),
    quo_m1_iter_q_f32_1(9, 0)
  )
  val out_frac_post_1_f16_2 = Mux(
    res_is_denormal_f16_2 | res_is_nan_q(2) | res_is_inf_q(2) | res_is_exact_zero_q(2),
    Mux(res_is_nan_q(2), Cat(1.U(1.W), 0.U(9.W)),
      Mux(res_is_inf_q(2) | res_is_exact_zero_q(2), 0.U(10.W),
        quo_rounded_f16_2(9, 0)
      )
    ),
    quo_m1_iter_q_f16_2(9, 0)
  )
  val out_frac_post_1_f16_3 = Mux(
    res_is_denormal_f16_3 | res_is_nan_q(3) | res_is_inf_q(3) | res_is_exact_zero_q(3),
    Mux(res_is_nan_q(3), Cat(1.U(1.W), 0.U(9.W)),
      Mux(res_is_inf_q(3) | res_is_exact_zero_q(3), 0.U(10.W),
        quo_rounded_f16_3(9, 0)
      )
    ),
    quo_m1_iter_q_f16_3(9, 0)
  )
  val out_frac_post_1_f32_0 = Mux(
    res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
    Mux(res_is_nan_q(0), Cat(1.U(1.W), 0.U(22.W)),
      Mux(res_is_inf_q(0) | res_is_exact_zero_q(0), 0.U(23.W),
        quo_rounded_f64_0(22, 0)
      )
    ),
    quo_m1_iter_q_f64_0(22, 0)
  )
  val out_frac_post_1_f32_1 = Mux(
    res_is_denormal_f32_1 | res_is_nan_q(1) | res_is_inf_q(1) | res_is_exact_zero_q(1),
    Mux(res_is_nan_q(1), Cat(1.U(1.W), 0.U(22.W)),
      Mux(res_is_inf_q(1) | res_is_exact_zero_q(1), 0.U(23.W),
        quo_rounded_f32_1(22, 0)
      )
    ),
    quo_m1_iter_q_f32_1(22, 0)
  )
  val out_frac_post_1_f64_0 = Mux(
    res_is_denormal_f64_0 | res_is_nan_q(0) | res_is_inf_q(0) | res_is_exact_zero_q(0),
    Mux(res_is_nan_q(0), Cat(1.U(1.W), 0.U(51.W)),
      Mux(res_is_inf_q(0) | res_is_exact_zero_q(0), 0.U(52.W),
        quo_rounded_f64_0(51, 0)
      )
    ),
    quo_m1_iter_q_f64_0(51, 0)
  )

  val res_post_1_f16_0 = Cat(out_sign_q(0), out_exp_post_1_f16_0, out_frac_post_1_f16_0)
  val res_post_1_f16_1 = Cat(out_sign_q(1), out_exp_post_1_f16_1, out_frac_post_1_f16_1)
  val res_post_1_f16_2 = Cat(out_sign_q(2), out_exp_post_1_f16_2, out_frac_post_1_f16_2)
  val res_post_1_f16_3 = Cat(out_sign_q(3), out_exp_post_1_f16_3, out_frac_post_1_f16_3)
  val res_post_1_f32_0 = Cat(out_sign_q(0), out_exp_post_1_f32_0, out_frac_post_1_f32_0)
  val res_post_1_f32_1 = Cat(out_sign_q(1), out_exp_post_1_f32_1, out_frac_post_1_f32_1)
  val res_post_1_f64_0 = Cat(out_sign_q(0), out_exp_post_1_f64_0, out_frac_post_1_f64_0)
  val fpdiv_res_post_1 = Cat(
    res_post_1_f64_0(63, 32),
    Mux(fp_format_q_is_fp32, res_post_1_f32_0(31, 16), res_post_1_f64_0(31, 16)),
    Mux(fp_format_q_is_fp16, res_post_1_f16_0(15, 0), Mux(fp_format_q_is_fp32, res_post_1_f32_0(15, 0), res_post_1_f64_0(15, 0)))
  )
  val fpdiv_res_post_0_vec = Mux1H(
    Seq(
      fp_format_q_is_fp64,
      fp_format_q_is_fp32,
      fp_format_q_is_fp16
    ),
    Seq(
      res_post_0_f64_0,
      Cat(res_post_0_f32_1, res_post_0_f32_0),
      Cat(res_post_0_f16_3, res_post_0_f16_2, res_post_0_f16_1, res_post_0_f16_0),
    )
  )
  val fpdiv_res_post_1_vec = Mux1H(
    Seq(
      fp_format_q_is_fp64,
      fp_format_q_is_fp32,
      fp_format_q_is_fp16
    ),
    Seq(
      res_post_1_f64_0,
      Cat(res_post_1_f32_1, res_post_1_f32_0),
      Cat(res_post_1_f16_3, res_post_1_f16_2, res_post_1_f16_1, res_post_1_f16_0),
    )
  )
  io.fpdiv_res_o := Mux(
    fsm_q(FSM_POST_0_BIT),
    Mux(is_vec_q & !fp_format_q_is_fp64, fpdiv_res_post_0_vec, fpdiv_res_post_0),
    Mux(is_vec_q & !fp_format_q_is_fp64, fpdiv_res_post_1_vec, fpdiv_res_post_1)
  )
  val fflags_NV_vec = WireInit(false.B)
  val fflags_DZ_vec = WireInit(false.B)
  val fflags_OF_vec = WireInit(false.B)
  val fflags_UF_vec = WireInit(false.B)
  val fflags_NX_vec = WireInit(false.B)
  val fflags_invalid_operation = op_invalid_div_q(0)
  val fflags_div_by_zero = divided_by_zero_q(0)
  val fflags_overflow = (fsm_q(FSM_POST_0_BIT) & overflow_f64_0) |
    (fsm_q(FSM_POST_1_BIT) & !res_is_denormal_f64_0 & overflow_f64_0 & !res_is_exact_zero_q(0) & !res_is_inf_q(0) & !res_is_nan_q(0))
  val fflags_underflow = res_is_denormal_f64_0 & fsm_q(FSM_POST_1_BIT) &
    (!carry_after_round_f64_0 | !(guard_correct_f64_0 & test_uf_need_rup_correct_f64_0)) &
    inexact_f64_0 & !res_is_exact_zero_q(0) & !res_is_inf_q(0) & !res_is_nan_q(0)
  val fflags_inexact = (fflags_overflow | inexact_f64_0) & !res_is_inf_q(0) & !res_is_nan_q(0) & !res_is_exact_zero_q(0)
  fflags_NV_vec := Mux1H(
    Seq(
      fp_format_q_is_fp16,
      fp_format_q_is_fp32,
      fp_format_q_is_fp64
    ),
    Seq(
      op_invalid_div_q.orR,
      op_invalid_div_q(1) | op_invalid_div_q(0),
      op_invalid_div_q(0)
    )
  )
  fflags_DZ_vec := Mux1H(
    Seq(
      fp_format_q_is_fp16,
      fp_format_q_is_fp32,
      fp_format_q_is_fp64
    ),
    Seq(
      divided_by_zero_q.orR,
      divided_by_zero_q(1) | divided_by_zero_q(0),
      divided_by_zero_q(0)
    )
  )
  overflow_d := Cat(
    overflow_f16_3,
    overflow_f16_2,
    overflow_f32_1,
    overflow_f64_0
  )
  val fflags_overflow_1 = (fsm_q(FSM_POST_0_BIT) & overflow_f32_1) |
    (fsm_q(FSM_POST_1_BIT) & !res_is_denormal_f32_1 & overflow_f32_1 & !res_is_exact_zero_q(1) & !res_is_inf_q(1) & !res_is_nan_q(1))
  val fflags_overflow_2 = (fsm_q(FSM_POST_0_BIT) & overflow_f16_2) |
    (fsm_q(FSM_POST_1_BIT) & !res_is_denormal_f16_2 & overflow_f16_2 & !res_is_exact_zero_q(2) & !res_is_inf_q(2) & !res_is_nan_q(2))
  val fflags_overflow_3 = (fsm_q(FSM_POST_0_BIT) & overflow_f16_3) |
    (fsm_q(FSM_POST_1_BIT) & !res_is_denormal_f16_3 & overflow_f16_3 & !res_is_exact_zero_q(3) & !res_is_inf_q(3) & !res_is_nan_q(3))
  fflags_OF_vec := Mux1H(
    Seq(
      fp_format_q_is_fp16,
      fp_format_q_is_fp32,
      fp_format_q_is_fp64
    ),
    Seq(
      fflags_overflow_3 | fflags_overflow_2 | fflags_overflow_1 | fflags_overflow,
      fflags_overflow_1 | fflags_overflow,
      fflags_overflow
    )
  )
  val fflags_underflow_1 = res_is_denormal_f32_1 & fsm_q(FSM_POST_1_BIT) &
    (!carry_after_round_f32_1 | !(guard_correct_f32_1 & test_uf_need_rup_correct_f32_1)) &
    inexact_f32_1 & !res_is_exact_zero_q(1) & !res_is_inf_q(1) & !res_is_nan_q(1)
  val fflags_underflow_2 = res_is_denormal_f16_2 & fsm_q(FSM_POST_1_BIT) &
    (!carry_after_round_f16_2 | !(guard_correct_f16_2 & test_uf_need_rup_correct_f16_2)) &
    inexact_f16_2 & !res_is_exact_zero_q(2) & !res_is_inf_q(2) & !res_is_nan_q(2)
  val fflags_underflow_3 = res_is_denormal_f16_3 & fsm_q(FSM_POST_1_BIT) &
    (!carry_after_round_f16_3 | !(guard_correct_f16_3 & test_uf_need_rup_correct_f16_3)) &
    inexact_f16_3 & !res_is_exact_zero_q(3) & !res_is_inf_q(3) & !res_is_nan_q(3)
  fflags_UF_vec := Mux1H(
    Seq(
      fp_format_q_is_fp16,
      fp_format_q_is_fp32,
      fp_format_q_is_fp64
    ),
    Seq(
      fflags_underflow_3 | fflags_underflow_2 | fflags_underflow_1 | fflags_underflow,
      fflags_underflow_1 | fflags_underflow,
      fflags_underflow
    )
  )
  val fflags_inexact_1 = (fflags_overflow_1 | inexact_f32_1) & !res_is_inf_q(1) & !res_is_nan_q(1) & !res_is_exact_zero_q(1)
  val fflags_inexact_2 = (fflags_overflow_2 | inexact_f16_2) & !res_is_inf_q(2) & !res_is_nan_q(2) & !res_is_exact_zero_q(2)
  val fflags_inexact_3 = (fflags_overflow_3 | inexact_f16_3) & !res_is_inf_q(3) & !res_is_nan_q(3) & !res_is_exact_zero_q(3)
  fflags_NX_vec := Mux1H(
    Seq(
      fp_format_q_is_fp16,
      fp_format_q_is_fp32,
      fp_format_q_is_fp64
    ),
    Seq(
      fflags_inexact_3 | fflags_inexact_2 | fflags_inexact_1 | fflags_inexact,
      fflags_inexact_1 | fflags_inexact,
      fflags_inexact
    )
  )
  val fflags_scalar = Cat(fflags_invalid_operation,fflags_div_by_zero,fflags_overflow,fflags_underflow,fflags_inexact)
  val fflags_vec_1 = Cat(op_invalid_div_q(1),divided_by_zero_q(1),fflags_overflow_1,fflags_underflow_1,fflags_inexact_1)
  val fflags_vec_2 = Cat(op_invalid_div_q(2),divided_by_zero_q(2),fflags_overflow_2,fflags_underflow_2,fflags_inexact_2)
  val fflags_vec_3 = Cat(op_invalid_div_q(3),divided_by_zero_q(3),fflags_overflow_3,fflags_underflow_3,fflags_inexact_3)
  io.fflags_o := Cat(
    Mux(is_vec_q & fp_format_q_is_fp16, fflags_vec_3, 0.U(5.W)),
    Mux(is_vec_q & fp_format_q_is_fp16, fflags_vec_2, 0.U(5.W)),
    Mux(is_vec_q & (fp_format_q_is_fp32 | fp_format_q_is_fp16), fflags_vec_1, 0.U(5.W)),
    fflags_scalar
  )
//  io.fflags_o := Cat(
//    Mux(is_vec_q & !fp_format_q_is_fp64, fflags_NV_vec, fflags_invalid_operation),
//    Mux(is_vec_q & !fp_format_q_is_fp64, fflags_DZ_vec, fflags_div_by_zero),
//    Mux(is_vec_q & !fp_format_q_is_fp64, fflags_OF_vec, fflags_overflow),
//    Mux(is_vec_q & !fp_format_q_is_fp64, fflags_UF_vec, fflags_underflow),
//    Mux(is_vec_q & !fp_format_q_is_fp64, fflags_NX_vec, fflags_inexact)
//  )

}

class fpdiv_r64_block_vector(
                              val S1_SPECULATIVE_QDS:Int = 1,
                              val S2_SPECULATIVE_QDS:Boolean = true,
                              val QDS_ARCH:Int = 2,
                              val QUO_DIG_W:Int =5
                            ) extends Module {

  val remainderVectorWidth = (3+11+3+1)*4
  val divisorVectorWidth = (11+4)*4
  val io = IO(new Bundle {
    val fp_format_onehot = Input(UInt(3.W))
    val is_vec  = Input(Bool())
    val f_r_s_i = Input(UInt(remainderVectorWidth.W))
    val f_r_c_i = Input(UInt(remainderVectorWidth.W))
    val divisor_i = Input(UInt(divisorVectorWidth.W))
    val nr_f_r_6b_for_nxt_cycle_s0_qds_i = Input(Vec(4,UInt(6.W)))
    val nr_f_r_7b_for_nxt_cycle_s1_qds_i = Input(Vec(4,UInt(7.W)))

    val nxt_quo_dig_o_0 = Output(Vec(3,UInt(QUO_DIG_W.W)))
    val nxt_quo_dig_o_1 = Output(Vec(3,UInt(QUO_DIG_W.W)))
    val nxt_quo_dig_o_2 = Output(Vec(3,UInt(QUO_DIG_W.W)))
    val nxt_quo_dig_o_3 = Output(Vec(3,UInt(QUO_DIG_W.W)))
    val nxt_f_r_s_o = Output(Vec(3,UInt(remainderVectorWidth.W)))
    val nxt_f_r_c_o = Output(Vec(3,UInt(remainderVectorWidth.W)))
    val adder_6b_res_for_nxt_cycle_s0_qds_o = Output(Vec(4,UInt(6.W)))
    val adder_7b_res_for_nxt_cycle_s1_qds_o = Output(Vec(4,UInt(7.W)))

























  })
  val is_fp64 = io.fp_format_onehot(2)
  val is_fp32 = io.fp_format_onehot(1)
  val is_fp16 = io.fp_format_onehot(0)
  val is_vec = io.is_vec
  val divisorWidthF64 = 57
  val divisorWidthF32 = 28
  val divisorWidthF16 = 15
  val remainderWidthF64 = 60
  val remainderWidthF32 = 31
  val remainderWidthF16 = 18






  val divisor_f64_0 = io.divisor_i.head(divisorWidthF64)
  val divisor_ext_f64_0 = Cat(0.U(2.W), divisor_f64_0, 0.U(1.W))
  val divisor_mul_neg_2_f64_0 = (~Cat(divisor_ext_f64_0((remainderWidthF64-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f64_0 = (~divisor_ext_f64_0).asUInt
  val divisor_mul_pos_1_f64_0 = divisor_ext_f64_0
  val divisor_mul_pos_2_f64_0 = Cat(divisor_ext_f64_0((remainderWidthF64-1)-1,0), 0.U(1.W))

  val divisor_f32_0 = io.divisor_i.tail(2*divisorWidthF16).head(divisorWidthF32)
  val divisor_ext_f32_0 = Cat(0.U(2.W), divisor_f32_0, 0.U(1.W))
  val divisor_mul_neg_2_f32_0 = (~Cat(divisor_ext_f32_0((remainderWidthF32-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f32_0 = (~divisor_ext_f32_0).asUInt
  val divisor_mul_pos_1_f32_0 = divisor_ext_f32_0
  val divisor_mul_pos_2_f32_0 = Cat(divisor_ext_f32_0((remainderWidthF32-1)-1,0), 0.U(1.W))

  val divisor_f32_1 = io.divisor_i.head(divisorWidthF32)
  val divisor_ext_f32_1 = Cat(0.U(2.W), divisor_f32_1, 0.U(1.W))
  val divisor_mul_neg_2_f32_1 = (~Cat(divisor_ext_f32_1((remainderWidthF32-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f32_1 = (~divisor_ext_f32_1).asUInt
  val divisor_mul_pos_1_f32_1 = divisor_ext_f32_1
  val divisor_mul_pos_2_f32_1 = Cat(divisor_ext_f32_1((remainderWidthF32-1)-1,0), 0.U(1.W))

  val divisor_f16_0 = io.divisor_i(divisorWidthF16-1,0)
  val divisor_ext_f16_0 = Cat(0.U(2.W), divisor_f16_0, 0.U(1.W))
  val divisor_mul_neg_2_f16_0 = (~Cat(divisor_ext_f16_0((remainderWidthF16-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f16_0 = (~divisor_ext_f16_0).asUInt
  val divisor_mul_pos_1_f16_0 = divisor_ext_f16_0
  val divisor_mul_pos_2_f16_0 = Cat(divisor_ext_f16_0((remainderWidthF16-1)-1,0), 0.U(1.W))

  val divisor_f16_1 = io.divisor_i(2*divisorWidthF16-1,divisorWidthF16)
  val divisor_ext_f16_1 = Cat(0.U(2.W), divisor_f16_1, 0.U(1.W))
  val divisor_mul_neg_2_f16_1 = (~Cat(divisor_ext_f16_1((remainderWidthF16-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f16_1 = (~divisor_ext_f16_1).asUInt
  val divisor_mul_pos_1_f16_1 = divisor_ext_f16_1
  val divisor_mul_pos_2_f16_1 = Cat(divisor_ext_f16_1((remainderWidthF16-1)-1,0), 0.U(1.W))

  val divisor_f16_2 = io.divisor_i(3*divisorWidthF16-1,2*divisorWidthF16)
  val divisor_ext_f16_2 = Cat(0.U(2.W), divisor_f16_2, 0.U(1.W))
  val divisor_mul_neg_2_f16_2 = (~Cat(divisor_ext_f16_2((remainderWidthF16-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f16_2 = (~divisor_ext_f16_2).asUInt
  val divisor_mul_pos_1_f16_2 = divisor_ext_f16_2
  val divisor_mul_pos_2_f16_2 = Cat(divisor_ext_f16_2((remainderWidthF16-1)-1,0), 0.U(1.W))

  val divisor_f16_3 = io.divisor_i(4*divisorWidthF16-1,3*divisorWidthF16)
  val divisor_ext_f16_3 = Cat(0.U(2.W), divisor_f16_3, 0.U(1.W))
  val divisor_mul_neg_2_f16_3 = (~Cat(divisor_ext_f16_3((remainderWidthF16-1)-1,0), 0.U(1.W))).asUInt
  val divisor_mul_neg_1_f16_3 = (~divisor_ext_f16_3).asUInt
  val divisor_mul_pos_1_f16_3 = divisor_ext_f16_3
  val divisor_mul_pos_2_f16_3 = Cat(divisor_ext_f16_3((remainderWidthF16-1)-1,0), 0.U(1.W))

  val divisor_mul_neg_2 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(divisor_mul_neg_2_f64_0,0.U(12.W)),
      Cat(divisor_mul_neg_2_f32_1,0.U(5.W),divisor_mul_neg_2_f32_0,0.U(5.W)),
      Cat(divisor_mul_neg_2_f16_3,divisor_mul_neg_2_f16_2,divisor_mul_neg_2_f16_1,divisor_mul_neg_2_f16_0)
    )
  )
  val divisor_mul_neg_1 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(divisor_mul_neg_1_f64_0,0.U(12.W)),
      Cat(divisor_mul_neg_1_f32_1,0.U(5.W),divisor_mul_neg_1_f32_0,0.U(5.W)),
      Cat(divisor_mul_neg_1_f16_3,divisor_mul_neg_1_f16_2,divisor_mul_neg_1_f16_1,divisor_mul_neg_1_f16_0)
    )
  )
  val divisor_mul_pos_1 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(divisor_mul_pos_1_f64_0,0.U(12.W)),
      Cat(divisor_mul_pos_1_f32_1,0.U(5.W),divisor_mul_pos_1_f32_0,0.U(5.W)),
      Cat(divisor_mul_pos_1_f16_3,divisor_mul_pos_1_f16_2,divisor_mul_pos_1_f16_1,divisor_mul_pos_1_f16_0)
    )
  )
  val divisor_mul_pos_2 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(divisor_mul_pos_2_f64_0,0.U(12.W)),
      Cat(divisor_mul_pos_2_f32_1,0.U(5.W),divisor_mul_pos_2_f32_0,0.U(5.W)),
      Cat(divisor_mul_pos_2_f16_3,divisor_mul_pos_2_f16_2,divisor_mul_pos_2_f16_1,divisor_mul_pos_2_f16_0)
    )
  )

  val nxt_f_r_s_spec_s0 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val nxt_f_r_s_spec_s1 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val nxt_f_r_s_spec_s2 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val nxt_f_r_c_spec_s0 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val nxt_f_r_c_spec_s1 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val nxt_f_r_c_spec_s2 = Wire(Vec(QUO_DIG_W,UInt(remainderVectorWidth.W)))
  val mask_zero = Cat(
    Fill(17,1.U(1.W)),!(is_fp16 & is_vec),
    Fill(17,1.U(1.W)),!(is_fp16 & is_vec),
    Fill(17,1.U(1.W)),!(is_fp16 & is_vec),
    Fill(17,1.U(1.W)),!(is_fp16 & is_vec)
  )
  val mask_one = Cat(
    Fill(17,0.U(1.W)),is_fp16 & io.is_vec,
    Fill(12,0.U(1.W)),is_fp32 & io.is_vec,Fill(4,0.U(1.W)),is_fp16 & io.is_vec,
    Fill(17,0.U(1.W)),is_fp16 & io.is_vec,
    Fill(5,0.U(1.W)),is_fp64 | !io.is_vec,Fill(6,0.U(1.W)),is_fp32 & io.is_vec,Fill(4,0.U(1.W)),is_fp16 & io.is_vec
  )


































































  val U_CSA_0_4 = Module(new CSA3to2(width = 60+1+31*2+2+18*4+3))
  val U_CSA_0_in_a = Cat(
    io.f_r_s_i.head(60).tail(2),0.U(3.W),
    io.f_r_s_i.head(31).tail(2),0.U(3.W),io.f_r_s_i(35,5).tail(2),0.U(3.W),
    io.f_r_s_i.head(18).tail(2),0.U(3.W),io.f_r_s_i(53,36).tail(2),0.U(3.W),io.f_r_s_i(35,18).tail(2),0.U(3.W),io.f_r_s_i(17,0).tail(2),0.U(2.W)
  )
  val U_CSA_0_in_b = Cat(
    io.f_r_c_i.head(60).tail(2),0.U(3.W),
    io.f_r_c_i.head(31).tail(2),0.U(3.W),io.f_r_c_i(35,5).tail(2),0.U(3.W),
    io.f_r_c_i.head(18).tail(2),0.U(3.W),io.f_r_c_i(53,36).tail(2),0.U(3.W),io.f_r_c_i(35,18).tail(2),0.U(3.W),io.f_r_c_i(17,0).tail(2),0.U(2.W)
  )
  U_CSA_0_4.io.in_a := U_CSA_0_in_a
  U_CSA_0_4.io.in_b := U_CSA_0_in_b
  U_CSA_0_4.io.in_c := Cat(
    divisor_mul_pos_2_f64_0,0.U(1.W),
    divisor_mul_pos_2_f32_1,0.U(1.W),divisor_mul_pos_2_f32_0,0.U(1.W),
    divisor_mul_pos_2_f16_3,0.U(1.W),divisor_mul_pos_2_f16_2,0.U(1.W),divisor_mul_pos_2_f16_1,0.U(1.W),divisor_mul_pos_2_f16_0
  )
  val U_CSA_0_3 = Module(new CSA3to2(200))
  U_CSA_0_3.io.in_a := U_CSA_0_in_a
  U_CSA_0_3.io.in_b := U_CSA_0_in_b
  U_CSA_0_3.io.in_c := Cat(
    divisor_mul_pos_1_f64_0,0.U(1.W),
    divisor_mul_pos_1_f32_1,0.U(1.W),divisor_mul_pos_1_f32_0,0.U(1.W),
    divisor_mul_pos_1_f16_3,0.U(1.W),divisor_mul_pos_1_f16_2,0.U(1.W),divisor_mul_pos_1_f16_1,0.U(1.W),divisor_mul_pos_1_f16_0
  )
  val U_CSA_0_1 = Module(new CSA3to2(200))
  U_CSA_0_1.io.in_a := U_CSA_0_in_a
  U_CSA_0_1.io.in_b := U_CSA_0_in_b
  U_CSA_0_1.io.in_c := Cat(
    divisor_mul_neg_1_f64_0,0.U(1.W),
    divisor_mul_neg_1_f32_1,0.U(1.W),divisor_mul_neg_1_f32_0,0.U(1.W),
    divisor_mul_neg_1_f16_3,0.U(1.W),divisor_mul_neg_1_f16_2,0.U(1.W),divisor_mul_neg_1_f16_1,0.U(1.W),divisor_mul_neg_1_f16_0
  )
  val U_CSA_0_0 = Module(new CSA3to2(200))
  U_CSA_0_0.io.in_a := U_CSA_0_in_a
  U_CSA_0_0.io.in_b := U_CSA_0_in_b
  U_CSA_0_0.io.in_c := Cat(
    divisor_mul_neg_2_f64_0,0.U(1.W),
    divisor_mul_neg_2_f32_1,0.U(1.W),divisor_mul_neg_2_f32_0,0.U(1.W),
    divisor_mul_neg_2_f16_3,0.U(1.W),divisor_mul_neg_2_f16_2,0.U(1.W),divisor_mul_neg_2_f16_1,0.U(1.W),divisor_mul_neg_2_f16_0
  )

  nxt_f_r_s_spec_s0(4) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_4.io.out_sum.head(60),0.U(12.W)),
      Cat(U_CSA_0_4.io.out_sum.tail(61).head(31),0.U(5.W),U_CSA_0_4.io.out_sum.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_4.io.out_sum.tail(125).head(18),        U_CSA_0_4.io.out_sum.tail(144).head(18),
        U_CSA_0_4.io.out_sum.tail(163).head(18),        U_CSA_0_4.io.out_sum.tail(182).head(18))
    )
  )
  nxt_f_r_s_spec_s0(3) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_3.io.out_sum.head(60),0.U(12.W)),
      Cat(U_CSA_0_3.io.out_sum.tail(61).head(31),0.U(5.W),U_CSA_0_3.io.out_sum.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_3.io.out_sum.tail(125).head(18),        U_CSA_0_3.io.out_sum.tail(144).head(18),
        U_CSA_0_3.io.out_sum.tail(163).head(18),        U_CSA_0_3.io.out_sum.tail(182).head(18))
    )
  )
  nxt_f_r_s_spec_s0(2) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(io.f_r_s_i.head(remainderWidthF64)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(io.f_r_s_i.head(remainderWidthF32)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(io.f_r_s_i.tail(2*remainderWidthF16).head(remainderWidthF32)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W))
      ),
      Cat(
        Cat(io.f_r_s_i((4*remainderWidthF16-1)-2,3*remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_s_i((3*remainderWidthF16-1)-2,2*remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_s_i((2*remainderWidthF16-1)-2,remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_s_i((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )
  nxt_f_r_s_spec_s0(1) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_1.io.out_sum.head(60),0.U(12.W)),
      Cat(U_CSA_0_1.io.out_sum.tail(61).head(31),0.U(5.W),U_CSA_0_1.io.out_sum.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_1.io.out_sum.tail(125).head(18),        U_CSA_0_1.io.out_sum.tail(144).head(18),
        U_CSA_0_1.io.out_sum.tail(163).head(18),        U_CSA_0_1.io.out_sum.tail(182).head(18))
    )
  )
  nxt_f_r_s_spec_s0(0) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_0.io.out_sum.head(60),0.U(12.W)),
      Cat(U_CSA_0_0.io.out_sum.tail(61).head(31),0.U(5.W),U_CSA_0_0.io.out_sum.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_0.io.out_sum.tail(125).head(18),        U_CSA_0_0.io.out_sum.tail(144).head(18),
        U_CSA_0_0.io.out_sum.tail(163).head(18),        U_CSA_0_0.io.out_sum.tail(182).head(18))
    )
  )
  nxt_f_r_c_spec_s0(4) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_4.io.out_car.head(60),0.U(12.W)),
      Cat(U_CSA_0_4.io.out_car.tail(61).head(31),0.U(5.W),U_CSA_0_4.io.out_car.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_4.io.out_car.tail(125).head(18),        U_CSA_0_4.io.out_car.tail(144).head(18),
        U_CSA_0_4.io.out_car.tail(163).head(18),        U_CSA_0_4.io.out_car.tail(182).head(18))
    )
  )
  nxt_f_r_c_spec_s0(3) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_3.io.out_car.head(60),0.U(12.W)),
      Cat(U_CSA_0_3.io.out_car.tail(61).head(31),0.U(5.W),U_CSA_0_3.io.out_car.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_3.io.out_car.tail(125).head(18),        U_CSA_0_3.io.out_car.tail(144).head(18),
        U_CSA_0_3.io.out_car.tail(163).head(18),        U_CSA_0_3.io.out_car.tail(182).head(18))
    )
  )
  nxt_f_r_c_spec_s0(2) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(io.f_r_c_i.head(remainderWidthF64)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(io.f_r_c_i.head(remainderWidthF32)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(io.f_r_c_i.tail(2*remainderWidthF16).head(remainderWidthF32)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W))
      ),
      Cat(
        Cat(io.f_r_c_i((4*remainderWidthF16-1)-2,3*remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_c_i((3*remainderWidthF16-1)-2,2*remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_c_i((2*remainderWidthF16-1)-2,remainderWidthF16),0.U(2.W)),
        Cat(io.f_r_c_i((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )
  nxt_f_r_c_spec_s0(1) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_1.io.out_car.head(60),0.U(12.W)),
      Cat(U_CSA_0_1.io.out_car.tail(61).head(31),0.U(5.W),U_CSA_0_1.io.out_car.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_1.io.out_car.tail(125).head(18),        U_CSA_0_1.io.out_car.tail(144).head(18),
        U_CSA_0_1.io.out_car.tail(163).head(18),        U_CSA_0_1.io.out_car.tail(182).head(18))
    )
  ) | mask_one
  nxt_f_r_c_spec_s0(0) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      Cat(U_CSA_0_0.io.out_car.head(60),0.U(12.W)),
      Cat(U_CSA_0_0.io.out_car.tail(61).head(31),0.U(5.W),U_CSA_0_0.io.out_car.tail(93).head(31),0.U(5.W)),
      Cat(U_CSA_0_0.io.out_car.tail(125).head(18),        U_CSA_0_0.io.out_car.tail(144).head(18),
        U_CSA_0_0.io.out_car.tail(163).head(18),        U_CSA_0_0.io.out_car.tail(182).head(18))
    )
  ) | mask_one

  val adder_7b_for_s2_qds_in_s0_spec_f64_0 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_s2_qds_in_s0_spec_f32_1 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_s2_qds_in_s0_spec_f16_2 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_s2_qds_in_s0_spec_f16_3 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_res_for_s2_qds_in_s0 = Wire(Vec(4,UInt(7.W)))
  val adder_7b_for_nxt_cycle_s0_qds_spec_f64_0 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_nxt_cycle_s0_qds_spec_f32_1 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_nxt_cycle_s0_qds_spec_f16_2 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_7b_for_nxt_cycle_s0_qds_spec_f16_3 = Wire(Vec(QUO_DIG_W,UInt(7.W)))
  val adder_8b_for_nxt_cycle_s1_qds_spec_f64_0 = Wire(Vec(QUO_DIG_W,UInt(8.W)))
  val adder_8b_for_nxt_cycle_s1_qds_spec_f32_1 = Wire(Vec(QUO_DIG_W,UInt(8.W)))
  val adder_8b_for_nxt_cycle_s1_qds_spec_f16_2 = Wire(Vec(QUO_DIG_W,UInt(8.W)))
  val adder_8b_for_nxt_cycle_s1_qds_spec_f16_3 = Wire(Vec(QUO_DIG_W,UInt(8.W)))

  val adder_7b_for_s2_qds_in_s0_spec_3_4 = nxt_f_r_s_spec_s0(4).tail(4).head(7) + nxt_f_r_c_spec_s0(4).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_3_3 = nxt_f_r_s_spec_s0(3).tail(4).head(7) + nxt_f_r_c_spec_s0(3).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_3_2 = nxt_f_r_s_spec_s0(2).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_3_1 = nxt_f_r_s_spec_s0(1).tail(4).head(7) + nxt_f_r_c_spec_s0(1).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_3_0 = nxt_f_r_s_spec_s0(0).tail(4).head(7) + nxt_f_r_c_spec_s0(0).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_2_4 = nxt_f_r_s_spec_s0(4).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(4).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_2_3 = nxt_f_r_s_spec_s0(3).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(3).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_2_2 = nxt_f_r_s_spec_s0(2).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_2_1 = nxt_f_r_s_spec_s0(1).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(1).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_2_0 = nxt_f_r_s_spec_s0(0).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(0).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_1_4 = nxt_f_r_s_spec_s0(4).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(4).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_1_3 = nxt_f_r_s_spec_s0(3).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(3).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_1_2 = nxt_f_r_s_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_1_1 = nxt_f_r_s_spec_s0(1).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(1).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_1_0 = nxt_f_r_s_spec_s0(0).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(0).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_0_4 = nxt_f_r_s_spec_s0(4).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(4).tail(3*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_0_3 = nxt_f_r_s_spec_s0(3).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(3).tail(3*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_0_2 = nxt_f_r_s_spec_s0(2).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(3*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_0_1 = nxt_f_r_s_spec_s0(1).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(1).tail(3*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_0_0 = nxt_f_r_s_spec_s0(0).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(0).tail(3*remainderWidthF16).tail(4).head(7)

  val adder_7b_for_s2_qds_in_s0_spec_f64_0_4 = U_CSA_0_4.io.out_sum.tail(4).head(7) + U_CSA_0_4.io.out_car.tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f64_0_3 = U_CSA_0_3.io.out_sum.tail(4).head(7) + U_CSA_0_3.io.out_car.tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f64_0_2 = nxt_f_r_s_spec_s0(2).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f64_0_1 = U_CSA_0_1.io.out_sum.tail(4).head(7) + U_CSA_0_1.io.out_car.tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f64_0_0 = U_CSA_0_0.io.out_sum.tail(4).head(7) + U_CSA_0_0.io.out_car.tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_0_4 = U_CSA_0_4.io.out_sum.tail(93).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(93).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_0_3 = U_CSA_0_3.io.out_sum.tail(93).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(93).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_0_2 = nxt_f_r_s_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_0_1 = U_CSA_0_1.io.out_sum.tail(93).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(93).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_0_0 = U_CSA_0_0.io.out_sum.tail(93).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(93).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_1_4 = U_CSA_0_4.io.out_sum.tail(61).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(61).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_1_3 = U_CSA_0_3.io.out_sum.tail(61).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(61).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_1_2 = nxt_f_r_s_spec_s0(2).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_1_1 = U_CSA_0_1.io.out_sum.tail(61).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(61).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f32_1_0 = U_CSA_0_0.io.out_sum.tail(61).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(61).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_0_4 = U_CSA_0_4.io.out_sum.tail(182).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(182).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_0_3 = U_CSA_0_3.io.out_sum.tail(182).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(182).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_0_2 = nxt_f_r_s_spec_s0(2).tail(3*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(3*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_0_1 = U_CSA_0_1.io.out_sum.tail(182).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(182).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_0_0 = U_CSA_0_0.io.out_sum.tail(182).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(182).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_1_4 = U_CSA_0_4.io.out_sum.tail(163).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(163).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_1_3 = U_CSA_0_3.io.out_sum.tail(163).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(163).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_1_2 = nxt_f_r_s_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(2*remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_1_1 = U_CSA_0_1.io.out_sum.tail(163).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(163).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_1_0 = U_CSA_0_0.io.out_sum.tail(163).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(163).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_2_4 = U_CSA_0_4.io.out_sum.tail(144).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(144).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_2_3 = U_CSA_0_3.io.out_sum.tail(144).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(144).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_2_2 = nxt_f_r_s_spec_s0(2).tail(remainderWidthF16).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(remainderWidthF16).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_2_1 = U_CSA_0_1.io.out_sum.tail(144).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(144).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_2_0 = U_CSA_0_0.io.out_sum.tail(144).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(144).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_3_4 = U_CSA_0_4.io.out_sum.tail(125).tail(4).head(7) + U_CSA_0_4.io.out_car.tail(125).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_3_3 = U_CSA_0_3.io.out_sum.tail(125).tail(4).head(7) + U_CSA_0_3.io.out_car.tail(125).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_3_2 = nxt_f_r_s_spec_s0(2).tail(4).head(7) + nxt_f_r_c_spec_s0(2).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_3_1 = U_CSA_0_1.io.out_sum.tail(125).tail(4).head(7) + U_CSA_0_1.io.out_car.tail(125).tail(4).head(7)
  val adder_7b_for_s2_qds_in_s0_spec_f16_3_0 = U_CSA_0_0.io.out_sum.tail(125).tail(4).head(7) + U_CSA_0_0.io.out_car.tail(125).tail(4).head(7)


  adder_7b_for_s2_qds_in_s0_spec_f64_0(4) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0_4,
      adder_7b_for_s2_qds_in_s0_spec_f32_0_4,
      adder_7b_for_s2_qds_in_s0_spec_f16_0_4
    )
  )
  adder_7b_for_s2_qds_in_s0_spec_f64_0(3) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0_3,
      adder_7b_for_s2_qds_in_s0_spec_f32_0_3,
      adder_7b_for_s2_qds_in_s0_spec_f16_0_3
    )
  )
  adder_7b_for_s2_qds_in_s0_spec_f64_0(2) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0_2,
      adder_7b_for_s2_qds_in_s0_spec_f32_0_2,
      adder_7b_for_s2_qds_in_s0_spec_f16_0_2
    )
  )
  adder_7b_for_s2_qds_in_s0_spec_f64_0(1) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0_1,
      adder_7b_for_s2_qds_in_s0_spec_f32_0_1,
      adder_7b_for_s2_qds_in_s0_spec_f16_0_1
    )
  )
  adder_7b_for_s2_qds_in_s0_spec_f64_0(0) := Mux1H(
    Seq(
      is_fp64 | !is_vec,
      is_fp32 &  is_vec,
      is_fp16 &  is_vec
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0_0,
      adder_7b_for_s2_qds_in_s0_spec_f32_0_0,
      adder_7b_for_s2_qds_in_s0_spec_f16_0_0
    )
  )

  adder_7b_for_s2_qds_in_s0_spec_f32_1(4) := Mux(is_fp32 & is_vec, adder_7b_for_s2_qds_in_s0_spec_f32_1_4,adder_7b_for_s2_qds_in_s0_spec_f16_1_4)
  adder_7b_for_s2_qds_in_s0_spec_f32_1(3) := Mux(is_fp32 & is_vec, adder_7b_for_s2_qds_in_s0_spec_f32_1_3,adder_7b_for_s2_qds_in_s0_spec_f16_1_3)
  adder_7b_for_s2_qds_in_s0_spec_f32_1(2) := Mux(is_fp32 & is_vec, adder_7b_for_s2_qds_in_s0_spec_f32_1_2,adder_7b_for_s2_qds_in_s0_spec_f16_1_2)
  adder_7b_for_s2_qds_in_s0_spec_f32_1(1) := Mux(is_fp32 & is_vec, adder_7b_for_s2_qds_in_s0_spec_f32_1_1,adder_7b_for_s2_qds_in_s0_spec_f16_1_1)
  adder_7b_for_s2_qds_in_s0_spec_f32_1(0) := Mux(is_fp32 & is_vec, adder_7b_for_s2_qds_in_s0_spec_f32_1_0,adder_7b_for_s2_qds_in_s0_spec_f16_1_0)

  adder_7b_for_s2_qds_in_s0_spec_f16_2(4) := adder_7b_for_s2_qds_in_s0_spec_f16_2_4
  adder_7b_for_s2_qds_in_s0_spec_f16_2(3) := adder_7b_for_s2_qds_in_s0_spec_f16_2_3
  adder_7b_for_s2_qds_in_s0_spec_f16_2(2) := adder_7b_for_s2_qds_in_s0_spec_f16_2_2
  adder_7b_for_s2_qds_in_s0_spec_f16_2(1) := adder_7b_for_s2_qds_in_s0_spec_f16_2_1
  adder_7b_for_s2_qds_in_s0_spec_f16_2(0) := adder_7b_for_s2_qds_in_s0_spec_f16_2_0

  adder_7b_for_s2_qds_in_s0_spec_f16_3(4) := adder_7b_for_s2_qds_in_s0_spec_f16_3_4
  adder_7b_for_s2_qds_in_s0_spec_f16_3(3) := adder_7b_for_s2_qds_in_s0_spec_f16_3_3
  adder_7b_for_s2_qds_in_s0_spec_f16_3(2) := adder_7b_for_s2_qds_in_s0_spec_f16_3_2
  adder_7b_for_s2_qds_in_s0_spec_f16_3(1) := adder_7b_for_s2_qds_in_s0_spec_f16_3_1
  adder_7b_for_s2_qds_in_s0_spec_f16_3(0) := adder_7b_for_s2_qds_in_s0_spec_f16_3_0




  val nxt_quo_dig_0 = Wire(Vec(3,UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_1 = Wire(Vec(3,UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_2 = Wire(Vec(3,UInt(QUO_DIG_W.W)))
  val nxt_quo_dig_3 = Wire(Vec(3,UInt(QUO_DIG_W.W)))
  val u_r4_qds_s0_0 = Module(new r4_qds_v2(QDS_ARCH = QDS_ARCH))
  u_r4_qds_s0_0.io.rem_i := io.nr_f_r_6b_for_nxt_cycle_s0_qds_i(0)
  nxt_quo_dig_0(0) := u_r4_qds_s0_0.io.quo_dig_o
  val u_r4_qds_s0_1 = Module(new r4_qds_v2(QDS_ARCH = QDS_ARCH))
  u_r4_qds_s0_1.io.rem_i := io.nr_f_r_6b_for_nxt_cycle_s0_qds_i(1)
  nxt_quo_dig_1(0) := u_r4_qds_s0_1.io.quo_dig_o
  val u_r4_qds_s0_2 = Module(new r4_qds_v2(QDS_ARCH = QDS_ARCH))
  u_r4_qds_s0_2.io.rem_i := io.nr_f_r_6b_for_nxt_cycle_s0_qds_i(2)
  nxt_quo_dig_2(0) := u_r4_qds_s0_2.io.quo_dig_o
  val u_r4_qds_s0_3 = Module(new r4_qds_v2(QDS_ARCH = QDS_ARCH))
  u_r4_qds_s0_3.io.rem_i := io.nr_f_r_6b_for_nxt_cycle_s0_qds_i(3)
  nxt_quo_dig_3(0) := u_r4_qds_s0_3.io.quo_dig_o


  val nxt_f_r_s_s0_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(0)(4),
      nxt_quo_dig_0(0)(3),
      nxt_quo_dig_0(0)(2),
      nxt_quo_dig_0(0)(1),
      nxt_quo_dig_0(0)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s0(4),
      nxt_f_r_s_spec_s0(3),
      nxt_f_r_s_spec_s0(2),
      nxt_f_r_s_spec_s0(1),
      nxt_f_r_s_spec_s0(0)
    )
  )
  val nxt_f_r_c_s0_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(0)(4),
      nxt_quo_dig_0(0)(3),
      nxt_quo_dig_0(0)(2),
      nxt_quo_dig_0(0)(1),
      nxt_quo_dig_0(0)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s0(4),
      nxt_f_r_c_spec_s0(3),
      nxt_f_r_c_spec_s0(2),
      nxt_f_r_c_spec_s0(1),
      nxt_f_r_c_spec_s0(0)
    )
  )
  val nxt_f_r_s_f64_0 = Wire(Vec(3,UInt(remainderWidthF64.W)))
  val nxt_f_r_c_f64_0 = Wire(Vec(3,UInt(remainderWidthF64.W)))
  nxt_f_r_s_f64_0(0) := nxt_f_r_s_s0_dig0.head(remainderWidthF64)
  nxt_f_r_c_f64_0(0) := nxt_f_r_c_s0_dig0.head(remainderWidthF64)
  val nxt_f_r_s_f32_0 = Wire(Vec(3,UInt(remainderWidthF32.W)))
  val nxt_f_r_c_f32_0 = Wire(Vec(3,UInt(remainderWidthF32.W)))
  nxt_f_r_s_f32_0(0) := nxt_f_r_s_s0_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  nxt_f_r_c_f32_0(0) := nxt_f_r_c_s0_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  val nxt_f_r_s_f16_0 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  val nxt_f_r_c_f16_0 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  nxt_f_r_s_f16_0(0) := nxt_f_r_s_s0_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_0(0) := nxt_f_r_c_s0_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s0_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(0)(4),
      nxt_quo_dig_1(0)(3),
      nxt_quo_dig_1(0)(2),
      nxt_quo_dig_1(0)(1),
      nxt_quo_dig_1(0)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s0(4),
      nxt_f_r_s_spec_s0(3),
      nxt_f_r_s_spec_s0(2),
      nxt_f_r_s_spec_s0(1),
      nxt_f_r_s_spec_s0(0)
    )
  )
  val nxt_f_r_c_s0_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(0)(4),
      nxt_quo_dig_1(0)(3),
      nxt_quo_dig_1(0)(2),
      nxt_quo_dig_1(0)(1),
      nxt_quo_dig_1(0)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s0(4),
      nxt_f_r_c_spec_s0(3),
      nxt_f_r_c_spec_s0(2),
      nxt_f_r_c_spec_s0(1),
      nxt_f_r_c_spec_s0(0)
    )
  )
  val nxt_f_r_s_f32_1 = Wire(Vec(3,UInt(remainderWidthF32.W)))
  val nxt_f_r_c_f32_1 = Wire(Vec(3,UInt(remainderWidthF32.W)))
  nxt_f_r_s_f32_1(0) := nxt_f_r_s_s0_dig1.head(remainderWidthF32)
  nxt_f_r_c_f32_1(0) := nxt_f_r_c_s0_dig1.head(remainderWidthF32)
  val nxt_f_r_s_f16_1 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  val nxt_f_r_c_f16_1 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  nxt_f_r_s_f16_1(0) := nxt_f_r_s_s0_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_1(0) := nxt_f_r_c_s0_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s0_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(0)(4),
      nxt_quo_dig_2(0)(3),
      nxt_quo_dig_2(0)(2),
      nxt_quo_dig_2(0)(1),
      nxt_quo_dig_2(0)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s0(4).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(3).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(2).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s0_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(0)(4),
      nxt_quo_dig_2(0)(3),
      nxt_quo_dig_2(0)(2),
      nxt_quo_dig_2(0)(1),
      nxt_quo_dig_2(0)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s0(4).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s0(3).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s0(2).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      nxt_f_r_c_spec_s0(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_c_spec_s0(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  val nxt_f_r_s_f16_2 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  val nxt_f_r_c_f16_2 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  nxt_f_r_s_f16_2(0) := nxt_f_r_s_s0_dig2
  nxt_f_r_c_f16_2(0) := nxt_f_r_c_s0_dig2
  val nxt_f_r_s_s0_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(0)(4),
      nxt_quo_dig_3(0)(3),
      nxt_quo_dig_3(0)(2),
      nxt_quo_dig_3(0)(1),
      nxt_quo_dig_3(0)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s0(4).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(3).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(2).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(1).head(remainderWidthF16),
      nxt_f_r_s_spec_s0(0).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s0_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(0)(4),
      nxt_quo_dig_3(0)(3),
      nxt_quo_dig_3(0)(2),
      nxt_quo_dig_3(0)(1),
      nxt_quo_dig_3(0)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s0(4).head(remainderWidthF16-1),0.U),
      Cat(nxt_f_r_c_spec_s0(3).head(remainderWidthF16-1),0.U),
      Cat(nxt_f_r_c_spec_s0(2).head(remainderWidthF16-1),0.U),
      nxt_f_r_c_spec_s0(1).head(remainderWidthF16),
      nxt_f_r_c_spec_s0(0).head(remainderWidthF16)
    )
  )
  val nxt_f_r_s_f16_3 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  val nxt_f_r_c_f16_3 = Wire(Vec(3,UInt(remainderWidthF16.W)))
  nxt_f_r_s_f16_3(0) := nxt_f_r_s_s0_dig3
  nxt_f_r_c_f16_3(0) := nxt_f_r_c_s0_dig3

  val nxt_f_r_s = Wire(Vec(3,UInt(remainderVectorWidth.W)))
  val nxt_f_r_c = Wire(Vec(3,UInt(remainderVectorWidth.W)))
  nxt_f_r_s(0) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_s_f64_0(0),0.U(12.W)),
      Cat(nxt_f_r_s_f32_1(0),0.U(5.W),nxt_f_r_s_f32_0(0),0.U(5.W)),
      Cat(nxt_f_r_s_f16_3(0),nxt_f_r_s_f16_2(0),nxt_f_r_s_f16_1(0),nxt_f_r_s_f16_0(0))
    )
  )
  nxt_f_r_c(0) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_c_f64_0(0),0.U(12.W)),
      Cat(nxt_f_r_c_f32_1(0),0.U(5.W),nxt_f_r_c_f32_0(0),0.U(5.W)),
      Cat(nxt_f_r_c_f16_3(0),nxt_f_r_c_f16_2(0),nxt_f_r_c_f16_1(0),nxt_f_r_c_f16_0(0))
    )
  )
  adder_7b_res_for_s2_qds_in_s0(3) := Mux1H(
    Seq(
      nxt_quo_dig_3(0)(4),
      nxt_quo_dig_3(0)(3),
      nxt_quo_dig_3(0)(2),
      nxt_quo_dig_3(0)(1),
      nxt_quo_dig_3(0)(0)
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f16_3(4),
      adder_7b_for_s2_qds_in_s0_spec_f16_3(3),
      adder_7b_for_s2_qds_in_s0_spec_f16_3(2),
      adder_7b_for_s2_qds_in_s0_spec_f16_3(1),
      adder_7b_for_s2_qds_in_s0_spec_f16_3(0)
    )
  )
  adder_7b_res_for_s2_qds_in_s0(2) := Mux1H(
    Seq(
      nxt_quo_dig_2(0)(4),
      nxt_quo_dig_2(0)(3),
      nxt_quo_dig_2(0)(2),
      nxt_quo_dig_2(0)(1),
      nxt_quo_dig_2(0)(0)
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f16_2(4),
      adder_7b_for_s2_qds_in_s0_spec_f16_2(3),
      adder_7b_for_s2_qds_in_s0_spec_f16_2(2),
      adder_7b_for_s2_qds_in_s0_spec_f16_2(1),
      adder_7b_for_s2_qds_in_s0_spec_f16_2(0)
    )
  )
  adder_7b_res_for_s2_qds_in_s0(1) := Mux1H(
    Seq(
      nxt_quo_dig_1(0)(4),
      nxt_quo_dig_1(0)(3),
      nxt_quo_dig_1(0)(2),
      nxt_quo_dig_1(0)(1),
      nxt_quo_dig_1(0)(0)
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f32_1(4),
      adder_7b_for_s2_qds_in_s0_spec_f32_1(3),
      adder_7b_for_s2_qds_in_s0_spec_f32_1(2),
      adder_7b_for_s2_qds_in_s0_spec_f32_1(1),
      adder_7b_for_s2_qds_in_s0_spec_f32_1(0)
    )
  )
  adder_7b_res_for_s2_qds_in_s0(0) := Mux1H(
    Seq(
      nxt_quo_dig_0(0)(4),
      nxt_quo_dig_0(0)(3),
      nxt_quo_dig_0(0)(2),
      nxt_quo_dig_0(0)(1),
      nxt_quo_dig_0(0)(0)
    ),
    Seq(
      adder_7b_for_s2_qds_in_s0_spec_f64_0(4),
      adder_7b_for_s2_qds_in_s0_spec_f64_0(3),
      adder_7b_for_s2_qds_in_s0_spec_f64_0(2),
      adder_7b_for_s2_qds_in_s0_spec_f64_0(1),
      adder_7b_for_s2_qds_in_s0_spec_f64_0(0)
    )
  )

  val U_CSA_1_in_a = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_s_f64_0(0)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(nxt_f_r_s_f32_1(0)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(nxt_f_r_s_f32_0(0)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
      ),
      Cat(
        Cat(nxt_f_r_s_f16_3(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_2(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_1(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_0(0)((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )
  val U_CSA_1_in_b = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_c_f64_0(0)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(nxt_f_r_c_f32_1(0)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(nxt_f_r_c_f32_0(0)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
      ),
      Cat(
        Cat(nxt_f_r_c_f16_3(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_2(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_1(0)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_0(0)((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )
  val U_CSA_1_4 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_1_4.io.in_a := U_CSA_1_in_a
  U_CSA_1_4.io.in_b := U_CSA_1_in_b
  U_CSA_1_4.io.in_c := divisor_mul_pos_2
  val U_CSA_1_3 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_1_3.io.in_a := U_CSA_1_in_a
  U_CSA_1_3.io.in_b := U_CSA_1_in_b
  U_CSA_1_3.io.in_c := divisor_mul_pos_1
  val U_CSA_1_1 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_1_1.io.in_a := U_CSA_1_in_a
  U_CSA_1_1.io.in_b := U_CSA_1_in_b
  U_CSA_1_1.io.in_c := divisor_mul_neg_1
  val U_CSA_1_0 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_1_0.io.in_a := U_CSA_1_in_a
  U_CSA_1_0.io.in_b := U_CSA_1_in_b
  U_CSA_1_0.io.in_c := divisor_mul_neg_2
  nxt_f_r_s_spec_s1(4) := U_CSA_1_4.io.out_sum
  nxt_f_r_s_spec_s1(3) := U_CSA_1_3.io.out_sum
  nxt_f_r_s_spec_s1(2) := U_CSA_1_in_a
  nxt_f_r_s_spec_s1(1) := U_CSA_1_1.io.out_sum
  nxt_f_r_s_spec_s1(0) := U_CSA_1_0.io.out_sum
  nxt_f_r_c_spec_s1(4) := U_CSA_1_4.io.out_car & mask_zero
  nxt_f_r_c_spec_s1(3) := U_CSA_1_3.io.out_car & mask_zero
  nxt_f_r_c_spec_s1(2) := U_CSA_1_in_b
  nxt_f_r_c_spec_s1(1) := U_CSA_1_1.io.out_car | mask_one
  nxt_f_r_c_spec_s1(0) := U_CSA_1_0.io.out_car | mask_one

  val divisor_mul_pos_2_qds_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_2_f64_0.tail(2).head(7),
      divisor_mul_pos_2_f32_0.tail(2).head(7),
      divisor_mul_pos_2_f16_0.tail(2).head(7)
    )
  )
  val divisor_mul_pos_1_qds_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_1_f64_0.tail(2).head(7),
      divisor_mul_pos_1_f32_0.tail(2).head(7),
      divisor_mul_pos_1_f16_0.tail(2).head(7)
    )
  )
  val divisor_mul_neg_1_qds_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_1_f64_0.tail(2).head(7),
      divisor_mul_neg_1_f32_0.tail(2).head(7),
      divisor_mul_neg_1_f16_0.tail(2).head(7)
    )
  )
  val divisor_mul_neg_2_qds_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_2_f64_0.tail(2).head(7),
      divisor_mul_neg_2_f32_0.tail(2).head(7),
      divisor_mul_neg_2_f16_0.tail(2).head(7)
    )
  )
  val divisor_mul_pos_2_qds_1 = Mux(is_fp32 & io.is_vec,divisor_mul_pos_2_f32_1.tail(2).head(7),divisor_mul_pos_2_f16_1.tail(2).head(7))
  val divisor_mul_pos_1_qds_1 = Mux(is_fp32 & io.is_vec,divisor_mul_pos_1_f32_1.tail(2).head(7),divisor_mul_pos_1_f16_1.tail(2).head(7))
  val divisor_mul_neg_1_qds_1 = Mux(is_fp32 & io.is_vec,divisor_mul_neg_1_f32_1.tail(2).head(7),divisor_mul_neg_1_f16_1.tail(2).head(7))
  val divisor_mul_neg_2_qds_1 = Mux(is_fp32 & io.is_vec,divisor_mul_neg_2_f32_1.tail(2).head(7),divisor_mul_neg_2_f16_1.tail(2).head(7))
  if (S1_SPECULATIVE_QDS == 1) {

    val u_r4_qds_s1_0 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_0.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(0)
    u_r4_qds_s1_0.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_0.tail(2).head(7)
    u_r4_qds_s1_0.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_0.tail(2).head(7)
    u_r4_qds_s1_0.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_0.tail(2).head(7)
    u_r4_qds_s1_0.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_0.tail(2).head(7)
    u_r4_qds_s1_0.io.prev_quo_dig_i := nxt_quo_dig_0(0)

    val u_r4_qds_s1_1 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_1.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(1)
    u_r4_qds_s1_1.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_1.tail(2).head(7)
    u_r4_qds_s1_1.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_1.tail(2).head(7)
    u_r4_qds_s1_1.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_1.tail(2).head(7)
    u_r4_qds_s1_1.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_1.tail(2).head(7)
    u_r4_qds_s1_1.io.prev_quo_dig_i := nxt_quo_dig_1(0)

    val u_r4_qds_s1_2 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_2.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(2)
    u_r4_qds_s1_2.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_2.tail(2).head(7)
    u_r4_qds_s1_2.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_2.tail(2).head(7)
    u_r4_qds_s1_2.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_2.tail(2).head(7)
    u_r4_qds_s1_2.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_2.tail(2).head(7)
    u_r4_qds_s1_2.io.prev_quo_dig_i := nxt_quo_dig_2(0)

    val u_r4_qds_s1_3 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_3.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(3)
    u_r4_qds_s1_3.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_3.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_3.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_3.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_3.io.prev_quo_dig_i := nxt_quo_dig_3(0)

    val u_r4_qds_s1_4 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_4.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(0)
    u_r4_qds_s1_4.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_1.tail(2).head(7)
    u_r4_qds_s1_4.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_1.tail(2).head(7)
    u_r4_qds_s1_4.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_1.tail(2).head(7)
    u_r4_qds_s1_4.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_1.tail(2).head(7)
    u_r4_qds_s1_4.io.prev_quo_dig_i := nxt_quo_dig_0(0)

    val u_r4_qds_s1_5 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_5.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(1)
    u_r4_qds_s1_5.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_5.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_5.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_5.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_5.io.prev_quo_dig_i := nxt_quo_dig_1(0)

    val u_r4_qds_s1_6 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s1_6.io.rem_i := io.nr_f_r_7b_for_nxt_cycle_s1_qds_i(0)
    u_r4_qds_s1_6.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_6.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_6.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_3.tail(2).head(7)
    u_r4_qds_s1_6.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_3.tail(2).head(7)
    u_r4_qds_s1_6.io.prev_quo_dig_i := nxt_quo_dig_0(0)

    nxt_quo_dig_0(1) := Mux1H(
      Seq(
        is_fp64 | !is_vec,
        is_fp32 &  is_vec,
        is_fp16 &  is_vec
      ),
      Seq(
        u_r4_qds_s1_6.io.quo_dig_o,
        u_r4_qds_s1_4.io.quo_dig_o,
        u_r4_qds_s1_0.io.quo_dig_o
      )
    )
    nxt_quo_dig_1(1) := Mux(is_fp32 & io.is_vec,u_r4_qds_s1_5.io.quo_dig_o,u_r4_qds_s1_1.io.quo_dig_o)
    nxt_quo_dig_2(1) := u_r4_qds_s1_2.io.quo_dig_o
    nxt_quo_dig_3(1) := u_r4_qds_s1_3.io.quo_dig_o
  }


  val nxt_f_r_s_s1_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(1)(4),
      nxt_quo_dig_0(1)(3),
      nxt_quo_dig_0(1)(2),
      nxt_quo_dig_0(1)(1),
      nxt_quo_dig_0(1)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s1(4),
      nxt_f_r_s_spec_s1(3),
      nxt_f_r_s_spec_s1(2),
      nxt_f_r_s_spec_s1(1),
      nxt_f_r_s_spec_s1(0)
    )
  )
  val nxt_f_r_c_s1_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(1)(4),
      nxt_quo_dig_0(1)(3),
      nxt_quo_dig_0(1)(2),
      nxt_quo_dig_0(1)(1),
      nxt_quo_dig_0(1)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s1(4),
      nxt_f_r_c_spec_s1(3),
      nxt_f_r_c_spec_s1(2),
      nxt_f_r_c_spec_s1(1),
      nxt_f_r_c_spec_s1(0)
    )
  )
  nxt_f_r_s_f64_0(1) := nxt_f_r_s_s1_dig0.head(remainderWidthF64)
  nxt_f_r_c_f64_0(1) := nxt_f_r_c_s1_dig0.head(remainderWidthF64)
  nxt_f_r_s_f32_0(1) := nxt_f_r_s_s1_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  nxt_f_r_c_f32_0(1) := nxt_f_r_c_s1_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  nxt_f_r_s_f16_0(1) := nxt_f_r_s_s1_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_0(1) := nxt_f_r_c_s1_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s1_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(1)(4),
      nxt_quo_dig_1(1)(3),
      nxt_quo_dig_1(1)(2),
      nxt_quo_dig_1(1)(1),
      nxt_quo_dig_1(1)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s1(4),
      nxt_f_r_s_spec_s1(3),
      nxt_f_r_s_spec_s1(2),
      nxt_f_r_s_spec_s1(1),
      nxt_f_r_s_spec_s1(0)
    )
  )
  val nxt_f_r_c_s1_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(1)(4),
      nxt_quo_dig_1(1)(3),
      nxt_quo_dig_1(1)(2),
      nxt_quo_dig_1(1)(1),
      nxt_quo_dig_1(1)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s1(4),
      nxt_f_r_c_spec_s1(3),
      nxt_f_r_c_spec_s1(2),
      nxt_f_r_c_spec_s1(1),
      nxt_f_r_c_spec_s1(0)
    )
  )
  nxt_f_r_s_f32_1(1) := nxt_f_r_s_s1_dig1.head(remainderWidthF32)
  nxt_f_r_c_f32_1(1) := nxt_f_r_c_s1_dig1.head(remainderWidthF32)
  nxt_f_r_s_f16_1(1) := nxt_f_r_s_s1_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_1(1) := nxt_f_r_c_s1_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s1_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(1)(4),
      nxt_quo_dig_2(1)(3),
      nxt_quo_dig_2(1)(2),
      nxt_quo_dig_2(1)(1),
      nxt_quo_dig_2(1)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s1(4).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(3).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(2).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s1_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(1)(4),
      nxt_quo_dig_2(1)(3),
      nxt_quo_dig_2(1)(2),
      nxt_quo_dig_2(1)(1),
      nxt_quo_dig_2(1)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s1(4).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s1(3).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s1(2).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      nxt_f_r_c_spec_s1(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_c_spec_s1(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  nxt_f_r_s_f16_2(1) := nxt_f_r_s_s1_dig2
  nxt_f_r_c_f16_2(1) := nxt_f_r_c_s1_dig2
  val nxt_f_r_s_s1_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(1)(4),
      nxt_quo_dig_3(1)(3),
      nxt_quo_dig_3(1)(2),
      nxt_quo_dig_3(1)(1),
      nxt_quo_dig_3(1)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s1(4).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(3).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(2).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(1).head(remainderWidthF16),
      nxt_f_r_s_spec_s1(0).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s1_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(1)(4),
      nxt_quo_dig_3(1)(3),
      nxt_quo_dig_3(1)(2),
      nxt_quo_dig_3(1)(1),
      nxt_quo_dig_3(1)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s1(4).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s1(3).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s1(2).head(remainderWidthF16-1),0.U(1.W)),
      nxt_f_r_c_spec_s1(1).head(remainderWidthF16),
      nxt_f_r_c_spec_s1(0).head(remainderWidthF16)
    )
  )
  nxt_f_r_s_f16_3(1) := nxt_f_r_s_s1_dig3
  nxt_f_r_c_f16_3(1) := nxt_f_r_c_s1_dig3

  nxt_f_r_s(1) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_s_f64_0(1),0.U(12.W)),
      Cat(nxt_f_r_s_f32_1(1),0.U(5.W),nxt_f_r_s_f32_0(1),0.U(5.W)),
      Cat(nxt_f_r_s_f16_3(1),nxt_f_r_s_f16_2(1),nxt_f_r_s_f16_1(1),nxt_f_r_s_f16_0(1))
    )
  )
  nxt_f_r_c(1) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_c_f64_0(1),0.U(12.W)),
      Cat(nxt_f_r_c_f32_1(1),0.U(5.W),nxt_f_r_c_f32_0(1),0.U(5.W)),
      Cat(nxt_f_r_c_f16_3(1),nxt_f_r_c_f16_2(1),nxt_f_r_c_f16_1(1),nxt_f_r_c_f16_0(1))
    )
  )

  val U_CSA_2_in_a = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_s_f64_0(1)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(nxt_f_r_s_f32_1(1)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(nxt_f_r_s_f32_0(1)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
      ),
      Cat(
        Cat(nxt_f_r_s_f16_3(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_2(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_1(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_s_f16_0(1)((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )
  val U_CSA_2_in_b = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_c_f64_0(1)((remainderWidthF64-1)-2,0),0.U(2.W),0.U(12.W)),
      Cat(
        Cat(nxt_f_r_c_f32_1(1)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
        Cat(nxt_f_r_c_f32_0(1)((remainderWidthF32-1)-2,0),0.U(2.W),0.U(5.W)),
      ),
      Cat(
        Cat(nxt_f_r_c_f16_3(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_2(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_1(1)((remainderWidthF16-1)-2,0),0.U(2.W)),
        Cat(nxt_f_r_c_f16_0(1)((remainderWidthF16-1)-2,0),0.U(2.W))
      )
    )
  )

  val U_CSA_2_4 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_2_4.io.in_a := U_CSA_2_in_a
  U_CSA_2_4.io.in_b := U_CSA_2_in_b
  U_CSA_2_4.io.in_c := divisor_mul_pos_2
  val U_CSA_2_3 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_2_3.io.in_a := U_CSA_2_in_a
  U_CSA_2_3.io.in_b := U_CSA_2_in_b
  U_CSA_2_3.io.in_c := divisor_mul_pos_1
  val U_CSA_2_1 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_2_1.io.in_a := U_CSA_2_in_a
  U_CSA_2_1.io.in_b := U_CSA_2_in_b
  U_CSA_2_1.io.in_c := divisor_mul_neg_1
  val U_CSA_2_0 = Module(new CSA3to2(remainderVectorWidth))
  U_CSA_2_0.io.in_a := U_CSA_2_in_a
  U_CSA_2_0.io.in_b := U_CSA_2_in_b
  U_CSA_2_0.io.in_c := divisor_mul_neg_2
  nxt_f_r_s_spec_s2(4) := U_CSA_2_4.io.out_sum
  nxt_f_r_s_spec_s2(3) := U_CSA_2_3.io.out_sum
  nxt_f_r_s_spec_s2(2) := U_CSA_2_in_a
  nxt_f_r_s_spec_s2(1) := U_CSA_2_1.io.out_sum
  nxt_f_r_s_spec_s2(0) := U_CSA_2_0.io.out_sum
  nxt_f_r_c_spec_s2(4) := U_CSA_2_4.io.out_car & mask_zero
  nxt_f_r_c_spec_s2(3) := U_CSA_2_3.io.out_car & mask_zero
  nxt_f_r_c_spec_s2(2) := U_CSA_2_in_b
  nxt_f_r_c_spec_s2(1) := U_CSA_2_1.io.out_car | mask_one
  nxt_f_r_c_spec_s2(0) := U_CSA_2_0.io.out_car | mask_one
  val adder_7b_divisor_mul_pos_2_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_2.tail(2).head(7),
      divisor_mul_pos_2_f32_0.tail(2).head(7),
      divisor_mul_pos_2_f16_0.tail(2).head(7)
    )
  )
  val adder_7b_divisor_mul_pos_1_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_1.tail(2).head(7),
      divisor_mul_pos_1_f32_0.tail(2).head(7),
      divisor_mul_pos_1_f16_0.tail(2).head(7)
    )
  )
  val adder_7b_divisor_mul_neg_1_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_1.tail(2).head(7),
      divisor_mul_neg_1_f32_0.tail(2).head(7),
      divisor_mul_neg_1_f16_0.tail(2).head(7)
    )
  )
  val adder_7b_divisor_mul_neg_2_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_2.tail(2).head(7),
      divisor_mul_neg_2_f32_0.tail(2).head(7),
      divisor_mul_neg_2_f16_0.tail(2).head(7)
    )
  )
  val adder_8b_divisor_mul_pos_2_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_2.tail(4).head(8),
      divisor_mul_pos_2_f32_0.tail(4).head(8),
      divisor_mul_pos_2_f16_0.tail(4).head(8)
    )
  )
  val adder_8b_divisor_mul_pos_1_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_pos_1.tail(4).head(8),
      divisor_mul_pos_1_f32_0.tail(4).head(8),
      divisor_mul_pos_1_f16_0.tail(4).head(8)
    )
  )
  val adder_8b_divisor_mul_neg_1_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_1.tail(4).head(8),
      divisor_mul_neg_1_f32_0.tail(4).head(8),
      divisor_mul_neg_1_f16_0.tail(4).head(8)
    )
  )
  val adder_8b_divisor_mul_neg_2_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      divisor_mul_neg_2.tail(4).head(8),
      divisor_mul_neg_2_f32_0.tail(4).head(8),
      divisor_mul_neg_2_f16_0.tail(4).head(8)
    )
  )
  val adder_7b_nxt_f_r_s_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      nxt_f_r_s_f64_0(1).tail(4).head(7),
      nxt_f_r_s_f32_0(1).tail(4).head(7),
      nxt_f_r_s_f16_0(1).tail(4).head(7)
    )
  )
  val adder_7b_nxt_f_r_c_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      nxt_f_r_c_f64_0(1).tail(4).head(7),
      nxt_f_r_c_f32_0(1).tail(4).head(7),
      nxt_f_r_c_f16_0(1).tail(4).head(7)
    )
  )
  val adder_8b_nxt_f_r_s_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      nxt_f_r_s_f64_0(1).tail(6).head(8),
      nxt_f_r_s_f32_0(1).tail(6).head(8),
      nxt_f_r_s_f16_0(1).tail(6).head(8)
    )
  )
  val adder_8b_nxt_f_r_c_f64_0 = Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      nxt_f_r_c_f64_0(1).tail(6).head(8),
      nxt_f_r_c_f32_0(1).tail(6).head(8),
      nxt_f_r_c_f16_0(1).tail(6).head(8)
    )
  )
  adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(4) := adder_7b_nxt_f_r_s_f64_0 + adder_7b_nxt_f_r_c_f64_0 + adder_7b_divisor_mul_pos_2_f64_0
  adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(3) := adder_7b_nxt_f_r_s_f64_0 + adder_7b_nxt_f_r_c_f64_0 + adder_7b_divisor_mul_pos_1_f64_0
  adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(2) := adder_7b_nxt_f_r_s_f64_0 + adder_7b_nxt_f_r_c_f64_0
  adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(1) := adder_7b_nxt_f_r_s_f64_0 + adder_7b_nxt_f_r_c_f64_0 + adder_7b_divisor_mul_neg_1_f64_0
  adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(0) := adder_7b_nxt_f_r_s_f64_0 + adder_7b_nxt_f_r_c_f64_0 + adder_7b_divisor_mul_neg_2_f64_0
  adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(4) := adder_8b_nxt_f_r_s_f64_0 + adder_8b_nxt_f_r_c_f64_0 + adder_8b_divisor_mul_pos_2_f64_0
  adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(3) := adder_8b_nxt_f_r_s_f64_0 + adder_8b_nxt_f_r_c_f64_0 + adder_8b_divisor_mul_pos_1_f64_0
  adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(2) := adder_8b_nxt_f_r_s_f64_0 + adder_8b_nxt_f_r_c_f64_0
  adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(1) := adder_8b_nxt_f_r_s_f64_0 + adder_8b_nxt_f_r_c_f64_0 + adder_8b_divisor_mul_neg_1_f64_0
  adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(0) := adder_8b_nxt_f_r_s_f64_0 + adder_8b_nxt_f_r_c_f64_0 + adder_8b_divisor_mul_neg_2_f64_0

  val adder_7b_divisor_mul_pos_2_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_pos_2_f32_1.tail(2).head(7),divisor_mul_pos_2_f16_1.tail(2).head(7))
  val adder_7b_divisor_mul_pos_1_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_pos_1_f32_1.tail(2).head(7),divisor_mul_pos_1_f16_1.tail(2).head(7))
  val adder_7b_divisor_mul_neg_1_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_neg_1_f32_1.tail(2).head(7),divisor_mul_neg_1_f16_1.tail(2).head(7))
  val adder_7b_divisor_mul_neg_2_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_neg_2_f32_1.tail(2).head(7),divisor_mul_neg_2_f16_1.tail(2).head(7))
  val adder_8b_divisor_mul_pos_2_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_pos_2_f32_1.tail(4).head(8),divisor_mul_pos_2_f16_1.tail(4).head(8))
  val adder_8b_divisor_mul_pos_1_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_pos_1_f32_1.tail(4).head(8),divisor_mul_pos_1_f16_1.tail(4).head(8))
  val adder_8b_divisor_mul_neg_1_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_neg_1_f32_1.tail(4).head(8),divisor_mul_neg_1_f16_1.tail(4).head(8))
  val adder_8b_divisor_mul_neg_2_f32_1 = Mux(is_fp32 & is_vec,divisor_mul_neg_2_f32_1.tail(4).head(8),divisor_mul_neg_2_f16_1.tail(4).head(8))
  val adder_7b_nxt_f_r_s_f32_1 = Mux(is_fp32 & is_vec,nxt_f_r_s_f32_1(1).tail(4).head(7),nxt_f_r_s_f16_1(1).tail(4).head(7))
  val adder_7b_nxt_f_r_c_f32_1 = Mux(is_fp32 & is_vec,nxt_f_r_c_f32_1(1).tail(4).head(7),nxt_f_r_c_f16_1(1).tail(4).head(7))
  val adder_8b_nxt_f_r_s_f32_1 = Mux(is_fp32 & is_vec,nxt_f_r_s_f32_1(1).tail(6).head(8),nxt_f_r_s_f16_1(1).tail(6).head(8))
  val adder_8b_nxt_f_r_c_f32_1 = Mux(is_fp32 & is_vec,nxt_f_r_c_f32_1(1).tail(6).head(8),nxt_f_r_c_f16_1(1).tail(6).head(8))
  adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(4) := adder_7b_nxt_f_r_s_f32_1 + adder_7b_nxt_f_r_c_f32_1 + adder_7b_divisor_mul_pos_2_f32_1
  adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(3) := adder_7b_nxt_f_r_s_f32_1 + adder_7b_nxt_f_r_c_f32_1 + adder_7b_divisor_mul_pos_1_f32_1
  adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(2) := adder_7b_nxt_f_r_s_f32_1 + adder_7b_nxt_f_r_c_f32_1
  adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(1) := adder_7b_nxt_f_r_s_f32_1 + adder_7b_nxt_f_r_c_f32_1 + adder_7b_divisor_mul_neg_1_f32_1
  adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(0) := adder_7b_nxt_f_r_s_f32_1 + adder_7b_nxt_f_r_c_f32_1 + adder_7b_divisor_mul_neg_2_f32_1
  adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(4) := adder_8b_nxt_f_r_s_f32_1 + adder_8b_nxt_f_r_c_f32_1 + adder_8b_divisor_mul_pos_2_f32_1
  adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(3) := adder_8b_nxt_f_r_s_f32_1 + adder_8b_nxt_f_r_c_f32_1 + adder_8b_divisor_mul_pos_1_f32_1
  adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(2) := adder_8b_nxt_f_r_s_f32_1 + adder_8b_nxt_f_r_c_f32_1
  adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(1) := adder_8b_nxt_f_r_s_f32_1 + adder_8b_nxt_f_r_c_f32_1 + adder_8b_divisor_mul_neg_1_f32_1
  adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(0) := adder_8b_nxt_f_r_s_f32_1 + adder_8b_nxt_f_r_c_f32_1 + adder_8b_divisor_mul_neg_2_f32_1

  adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(4) := nxt_f_r_s_f16_2(1).tail(4).head(7) + nxt_f_r_c_f16_2(1).tail(4).head(7) + divisor_mul_pos_2_f16_2.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(3) := nxt_f_r_s_f16_2(1).tail(4).head(7) + nxt_f_r_c_f16_2(1).tail(4).head(7) + divisor_mul_pos_1_f16_2.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(2) := nxt_f_r_s_f16_2(1).tail(4).head(7) + nxt_f_r_c_f16_2(1).tail(4).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(1) := nxt_f_r_s_f16_2(1).tail(4).head(7) + nxt_f_r_c_f16_2(1).tail(4).head(7) + divisor_mul_neg_1_f16_2.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(0) := nxt_f_r_s_f16_2(1).tail(4).head(7) + nxt_f_r_c_f16_2(1).tail(4).head(7) + divisor_mul_neg_2_f16_2.tail(2).head(7)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(4) := nxt_f_r_s_f16_2(1).tail(6).head(8) + nxt_f_r_c_f16_2(1).tail(6).head(8) + divisor_mul_pos_2_f16_2.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(3) := nxt_f_r_s_f16_2(1).tail(6).head(8) + nxt_f_r_c_f16_2(1).tail(6).head(8) + divisor_mul_pos_1_f16_2.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(2) := nxt_f_r_s_f16_2(1).tail(6).head(8) + nxt_f_r_c_f16_2(1).tail(6).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(1) := nxt_f_r_s_f16_2(1).tail(6).head(8) + nxt_f_r_c_f16_2(1).tail(6).head(8) + divisor_mul_neg_1_f16_2.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(0) := nxt_f_r_s_f16_2(1).tail(6).head(8) + nxt_f_r_c_f16_2(1).tail(6).head(8) + divisor_mul_neg_2_f16_2.tail(4).head(8)

  adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(4) := nxt_f_r_s_f16_3(1).tail(4).head(7) + nxt_f_r_c_f16_3(1).tail(4).head(7) + divisor_mul_pos_2_f16_3.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(3) := nxt_f_r_s_f16_3(1).tail(4).head(7) + nxt_f_r_c_f16_3(1).tail(4).head(7) + divisor_mul_pos_1_f16_3.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(2) := nxt_f_r_s_f16_3(1).tail(4).head(7) + nxt_f_r_c_f16_3(1).tail(4).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(1) := nxt_f_r_s_f16_3(1).tail(4).head(7) + nxt_f_r_c_f16_3(1).tail(4).head(7) + divisor_mul_neg_1_f16_3.tail(2).head(7)
  adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(0) := nxt_f_r_s_f16_3(1).tail(4).head(7) + nxt_f_r_c_f16_3(1).tail(4).head(7) + divisor_mul_neg_2_f16_3.tail(2).head(7)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(4) := nxt_f_r_s_f16_3(1).tail(6).head(8) + nxt_f_r_c_f16_3(1).tail(6).head(8) + divisor_mul_pos_2_f16_3.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(3) := nxt_f_r_s_f16_3(1).tail(6).head(8) + nxt_f_r_c_f16_3(1).tail(6).head(8) + divisor_mul_pos_1_f16_3.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(2) := nxt_f_r_s_f16_3(1).tail(6).head(8) + nxt_f_r_c_f16_3(1).tail(6).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(1) := nxt_f_r_s_f16_3(1).tail(6).head(8) + nxt_f_r_c_f16_3(1).tail(6).head(8) + divisor_mul_neg_1_f16_3.tail(4).head(8)
  adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(0) := nxt_f_r_s_f16_3(1).tail(6).head(8) + nxt_f_r_c_f16_3(1).tail(6).head(8) + divisor_mul_neg_2_f16_3.tail(4).head(8)

  if (S2_SPECULATIVE_QDS) {

    val u_r4_qds_s2_0 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s2_0.io.rem_i := adder_7b_res_for_s2_qds_in_s0(0)
    u_r4_qds_s2_0.io.divisor_mul_pos_2_i := divisor_mul_pos_2_qds_0
    u_r4_qds_s2_0.io.divisor_mul_pos_1_i := divisor_mul_pos_1_qds_0
    u_r4_qds_s2_0.io.divisor_mul_neg_1_i := divisor_mul_neg_1_qds_0
    u_r4_qds_s2_0.io.divisor_mul_neg_2_i := divisor_mul_neg_2_qds_0
    u_r4_qds_s2_0.io.prev_quo_dig_i := nxt_quo_dig_0(1)
    nxt_quo_dig_0(2) := u_r4_qds_s2_0.io.quo_dig_o

    val u_r4_qds_s2_1 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s2_1.io.rem_i := adder_7b_res_for_s2_qds_in_s0(1)
    u_r4_qds_s2_1.io.divisor_mul_pos_2_i := divisor_mul_pos_2_qds_1
    u_r4_qds_s2_1.io.divisor_mul_pos_1_i := divisor_mul_pos_1_qds_1
    u_r4_qds_s2_1.io.divisor_mul_neg_1_i := divisor_mul_neg_1_qds_1
    u_r4_qds_s2_1.io.divisor_mul_neg_2_i := divisor_mul_neg_2_qds_1
    u_r4_qds_s2_1.io.prev_quo_dig_i := nxt_quo_dig_1(1)
    nxt_quo_dig_1(2) := u_r4_qds_s2_1.io.quo_dig_o

    val u_r4_qds_s2_2 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s2_2.io.rem_i := adder_7b_res_for_s2_qds_in_s0(2)
    u_r4_qds_s2_2.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_2.tail(2).head(7)
    u_r4_qds_s2_2.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_2.tail(2).head(7)
    u_r4_qds_s2_2.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_2.tail(2).head(7)
    u_r4_qds_s2_2.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_2.tail(2).head(7)
    u_r4_qds_s2_2.io.prev_quo_dig_i := nxt_quo_dig_2(1)
    nxt_quo_dig_2(2) := u_r4_qds_s2_2.io.quo_dig_o

    val u_r4_qds_s2_3 = Module(new r4_qds_v2_spec(QDS_ARCH = QDS_ARCH))
    u_r4_qds_s2_3.io.rem_i := adder_7b_res_for_s2_qds_in_s0(3)
    u_r4_qds_s2_3.io.divisor_mul_pos_2_i := divisor_mul_pos_2_f16_3.tail(2).head(7)
    u_r4_qds_s2_3.io.divisor_mul_pos_1_i := divisor_mul_pos_1_f16_3.tail(2).head(7)
    u_r4_qds_s2_3.io.divisor_mul_neg_1_i := divisor_mul_neg_1_f16_3.tail(2).head(7)
    u_r4_qds_s2_3.io.divisor_mul_neg_2_i := divisor_mul_neg_2_f16_3.tail(2).head(7)
    u_r4_qds_s2_3.io.prev_quo_dig_i := nxt_quo_dig_3(1)
    nxt_quo_dig_3(2) := u_r4_qds_s2_3.io.quo_dig_o
  }



  val nxt_f_r_s_s2_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(2)(4),
      nxt_quo_dig_0(2)(3),
      nxt_quo_dig_0(2)(2),
      nxt_quo_dig_0(2)(1),
      nxt_quo_dig_0(2)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s2(4),
      nxt_f_r_s_spec_s2(3),
      nxt_f_r_s_spec_s2(2),
      nxt_f_r_s_spec_s2(1),
      nxt_f_r_s_spec_s2(0)
    )
  )
  val nxt_f_r_c_s2_dig0 = Mux1H(
    Seq(
      nxt_quo_dig_0(2)(4),
      nxt_quo_dig_0(2)(3),
      nxt_quo_dig_0(2)(2),
      nxt_quo_dig_0(2)(1),
      nxt_quo_dig_0(2)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s2(4),
      nxt_f_r_c_spec_s2(3),
      nxt_f_r_c_spec_s2(2),
      nxt_f_r_c_spec_s2(1),
      nxt_f_r_c_spec_s2(0)
    )
  )
  nxt_f_r_s_f64_0(2) := nxt_f_r_s_s2_dig0.head(remainderWidthF64)
  nxt_f_r_c_f64_0(2) := nxt_f_r_c_s2_dig0.head(remainderWidthF64)
  nxt_f_r_s_f32_0(2) := nxt_f_r_s_s2_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  nxt_f_r_c_f32_0(2) := nxt_f_r_c_s2_dig0.tail(2*remainderWidthF16).head(remainderWidthF32)
  nxt_f_r_s_f16_0(2) := nxt_f_r_s_s2_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_0(2) := nxt_f_r_c_s2_dig0.tail(3*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s2_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(2)(4),
      nxt_quo_dig_1(2)(3),
      nxt_quo_dig_1(2)(2),
      nxt_quo_dig_1(2)(1),
      nxt_quo_dig_1(2)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s2(4),
      nxt_f_r_s_spec_s2(3),
      nxt_f_r_s_spec_s2(2),
      nxt_f_r_s_spec_s2(1),
      nxt_f_r_s_spec_s2(0)
    )
  )
  val nxt_f_r_c_s2_dig1 = Mux1H(
    Seq(
      nxt_quo_dig_1(2)(4),
      nxt_quo_dig_1(2)(3),
      nxt_quo_dig_1(2)(2),
      nxt_quo_dig_1(2)(1),
      nxt_quo_dig_1(2)(0)
    ),
    Seq(
      nxt_f_r_c_spec_s2(4),
      nxt_f_r_c_spec_s2(3),
      nxt_f_r_c_spec_s2(2),
      nxt_f_r_c_spec_s2(1),
      nxt_f_r_c_spec_s2(0)
    )
  )
  nxt_f_r_s_f32_1(2) := nxt_f_r_s_s2_dig1.head(remainderWidthF32)
  nxt_f_r_c_f32_1(2) := nxt_f_r_c_s2_dig1.head(remainderWidthF32)
  nxt_f_r_s_f16_1(2) := nxt_f_r_s_s2_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  nxt_f_r_c_f16_1(2) := nxt_f_r_c_s2_dig1.tail(2*remainderWidthF16).head(remainderWidthF16)
  val nxt_f_r_s_s2_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(2)(4),
      nxt_quo_dig_2(2)(3),
      nxt_quo_dig_2(2)(2),
      nxt_quo_dig_2(2)(1),
      nxt_quo_dig_2(2)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s2(4).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(3).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(2).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s2_dig2 = Mux1H(
    Seq(
      nxt_quo_dig_2(2)(4),
      nxt_quo_dig_2(2)(3),
      nxt_quo_dig_2(2)(2),
      nxt_quo_dig_2(2)(1),
      nxt_quo_dig_2(2)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s2(4).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s2(3).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s2(2).tail(remainderWidthF16).head(remainderWidthF16-1),0.U(1.W)),
      nxt_f_r_c_spec_s2(1).tail(remainderWidthF16).head(remainderWidthF16),
      nxt_f_r_c_spec_s2(0).tail(remainderWidthF16).head(remainderWidthF16)
    )
  )
  nxt_f_r_s_f16_2(2) := nxt_f_r_s_s2_dig2
  nxt_f_r_c_f16_2(2) := nxt_f_r_c_s2_dig2
  val nxt_f_r_s_s2_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(2)(4),
      nxt_quo_dig_3(2)(3),
      nxt_quo_dig_3(2)(2),
      nxt_quo_dig_3(2)(1),
      nxt_quo_dig_3(2)(0)
    ),
    Seq(
      nxt_f_r_s_spec_s2(4).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(3).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(2).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(1).head(remainderWidthF16),
      nxt_f_r_s_spec_s2(0).head(remainderWidthF16)
    )
  )
  val nxt_f_r_c_s2_dig3 = Mux1H(
    Seq(
      nxt_quo_dig_3(2)(4),
      nxt_quo_dig_3(2)(3),
      nxt_quo_dig_3(2)(2),
      nxt_quo_dig_3(2)(1),
      nxt_quo_dig_3(2)(0)
    ),
    Seq(
      Cat(nxt_f_r_c_spec_s2(4).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s2(3).head(remainderWidthF16-1),0.U(1.W)),
      Cat(nxt_f_r_c_spec_s2(2).head(remainderWidthF16-1),0.U(1.W)),
      nxt_f_r_c_spec_s2(1).head(remainderWidthF16),
      nxt_f_r_c_spec_s2(0).head(remainderWidthF16)
    )
  )
  nxt_f_r_s_f16_3(2) := nxt_f_r_s_s2_dig3
  nxt_f_r_c_f16_3(2) := nxt_f_r_c_s2_dig3

  nxt_f_r_s(2) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_s_f64_0(2),0.U(12.W)),
      Cat(nxt_f_r_s_f32_1(2),0.U(5.W),nxt_f_r_s_f32_0(2),0.U(5.W)),
      Cat(nxt_f_r_s_f16_3(2),nxt_f_r_s_f16_2(2),nxt_f_r_s_f16_1(2),nxt_f_r_s_f16_0(2))
    )
  )
  nxt_f_r_c(2) := Mux1H(
    Seq(
      is_fp64 | !io.is_vec,
      is_fp32 &  io.is_vec,
      is_fp16 &  io.is_vec
    ),
    Seq(
      Cat(nxt_f_r_c_f64_0(2),0.U(12.W)),
      Cat(nxt_f_r_c_f32_1(2),0.U(5.W),nxt_f_r_c_f32_0(2),0.U(5.W)),
      Cat(nxt_f_r_c_f16_3(2),nxt_f_r_c_f16_2(2),nxt_f_r_c_f16_1(2),nxt_f_r_c_f16_0(2))
    )
  )

  io.adder_6b_res_for_nxt_cycle_s0_qds_o(0) := Mux1H(
    Seq(
      nxt_quo_dig_0(2)(4),
      nxt_quo_dig_0(2)(3),
      nxt_quo_dig_0(2)(2),
      nxt_quo_dig_0(2)(1),
      nxt_quo_dig_0(2)(0)
    ),
    Seq(
      adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(4)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(3)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(2)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(1)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f64_0(0)(6,1)
    )
  )
  io.adder_6b_res_for_nxt_cycle_s0_qds_o(1) := Mux1H(
    Seq(
      nxt_quo_dig_1(2)(4),
      nxt_quo_dig_1(2)(3),
      nxt_quo_dig_1(2)(2),
      nxt_quo_dig_1(2)(1),
      nxt_quo_dig_1(2)(0)
    ),
    Seq(
      adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(4)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(3)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(2)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(1)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f32_1(0)(6,1)
    )
  )
  io.adder_6b_res_for_nxt_cycle_s0_qds_o(2) := Mux1H(
    Seq(
      nxt_quo_dig_2(2)(4),
      nxt_quo_dig_2(2)(3),
      nxt_quo_dig_2(2)(2),
      nxt_quo_dig_2(2)(1),
      nxt_quo_dig_2(2)(0)
    ),
    Seq(
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(4)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(3)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(2)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(1)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_2(0)(6,1)
    )
  )
  io.adder_6b_res_for_nxt_cycle_s0_qds_o(3) := Mux1H(
    Seq(
      nxt_quo_dig_3(2)(4),
      nxt_quo_dig_3(2)(3),
      nxt_quo_dig_3(2)(2),
      nxt_quo_dig_3(2)(1),
      nxt_quo_dig_3(2)(0)
    ),
    Seq(
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(4)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(3)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(2)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(1)(6,1),
      adder_7b_for_nxt_cycle_s0_qds_spec_f16_3(0)(6,1)
    )
  )
  io.adder_7b_res_for_nxt_cycle_s1_qds_o(0) := Mux1H(
    Seq(
      nxt_quo_dig_0(2)(4),
      nxt_quo_dig_0(2)(3),
      nxt_quo_dig_0(2)(2),
      nxt_quo_dig_0(2)(1),
      nxt_quo_dig_0(2)(0)
    ),
    Seq(
      adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(4)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(3)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(2)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(1)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f64_0(0)(7,1)
    )
  )
  io.adder_7b_res_for_nxt_cycle_s1_qds_o(1) := Mux1H(
    Seq(
      nxt_quo_dig_1(2)(4),
      nxt_quo_dig_1(2)(3),
      nxt_quo_dig_1(2)(2),
      nxt_quo_dig_1(2)(1),
      nxt_quo_dig_1(2)(0)
    ),
    Seq(
      adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(4)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(3)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(2)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(1)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f32_1(0)(7,1)
    )
  )
  io.adder_7b_res_for_nxt_cycle_s1_qds_o(2) := Mux1H(
    Seq(
      nxt_quo_dig_2(2)(4),
      nxt_quo_dig_2(2)(3),
      nxt_quo_dig_2(2)(2),
      nxt_quo_dig_2(2)(1),
      nxt_quo_dig_2(2)(0)
    ),
    Seq(
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(4)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(3)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(2)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(1)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_2(0)(7,1)
    )
  )
  io.adder_7b_res_for_nxt_cycle_s1_qds_o(3) := Mux1H(
    Seq(
      nxt_quo_dig_3(2)(4),
      nxt_quo_dig_3(2)(3),
      nxt_quo_dig_3(2)(2),
      nxt_quo_dig_3(2)(1),
      nxt_quo_dig_3(2)(0)
    ),
    Seq(
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(4)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(3)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(2)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(1)(7,1),
      adder_8b_for_nxt_cycle_s1_qds_spec_f16_3(0)(7,1)
    )
  )

  io.nxt_quo_dig_o_0(0) := nxt_quo_dig_0(0)
  io.nxt_quo_dig_o_0(1) := nxt_quo_dig_0(1)
  io.nxt_quo_dig_o_0(2) := nxt_quo_dig_0(2)
  io.nxt_quo_dig_o_1(0) := nxt_quo_dig_1(0)
  io.nxt_quo_dig_o_1(1) := nxt_quo_dig_1(1)
  io.nxt_quo_dig_o_1(2) := nxt_quo_dig_1(2)
  io.nxt_quo_dig_o_2(0) := nxt_quo_dig_2(0)
  io.nxt_quo_dig_o_2(1) := nxt_quo_dig_2(1)
  io.nxt_quo_dig_o_2(2) := nxt_quo_dig_2(2)
  io.nxt_quo_dig_o_3(0) := nxt_quo_dig_3(0)
  io.nxt_quo_dig_o_3(1) := nxt_quo_dig_3(1)
  io.nxt_quo_dig_o_3(2) := nxt_quo_dig_3(2)

  io.nxt_f_r_s_o(0) := nxt_f_r_s(0)
  io.nxt_f_r_s_o(1) := nxt_f_r_s(1)
  io.nxt_f_r_s_o(2) := nxt_f_r_s(2)

  io.nxt_f_r_c_o(0) := nxt_f_r_c(0)
  io.nxt_f_r_c_o(1) := nxt_f_r_c(1)
  io.nxt_f_r_c_o(2) := nxt_f_r_c(2)


























}

private[vector] class r4_qds_v2(
                 val QDS_ARCH:Int = 2
               ) extends Module{
  val io = IO(new Bundle {
    val rem_i = Input(UInt(6.W))
    val quo_dig_o = Output(UInt(5.W))
  })

  val M_POS_2 = 12.S(6.W)
  val M_POS_1 = 3.S(6.W)

  val M_NEG_0 = -4.S(6.W)
  val M_NEG_1 = -13.S(6.W)

  val M_POS_2_NEGATED = -12.S(6.W)
  val M_POS_1_NEGATED = -3.S(6.W)
  val M_NEG_0_NEGATED = 4.S(6.W)
  val M_NEG_1_NEGATED = 13.S(6.W)

  val rem_i_signed = io.rem_i.asSInt
  if (QDS_ARCH == 0) {
    io.quo_dig_o := Cat(
      rem_i_signed <= -14.S(6.W),
      rem_i_signed >= -13.S(6.W) & rem_i_signed <= -5.S(6.W),
      rem_i_signed >= -4.S(6.W) & rem_i_signed <= 2.S(6.W),
      rem_i_signed >= 3.S(6.W) & rem_i_signed <= 11.S(6.W),
      rem_i_signed >= 12.S(6.W)
    )
  }
  else if (QDS_ARCH == 1) {
    val rem_ge_m_pos_2 = rem_i_signed >= M_POS_2
    val rem_ge_m_pos_1 = rem_i_signed >= M_POS_1
    val rem_ge_m_neg_0 = rem_i_signed >= M_NEG_0
    val rem_ge_m_neg_1 = rem_i_signed >= M_NEG_1
    io.quo_dig_o := Cat(
      ~rem_ge_m_neg_1,
      rem_ge_m_neg_1 & ~rem_ge_m_neg_0,
      rem_ge_m_neg_0 & ~rem_ge_m_pos_1,
      rem_ge_m_pos_1 & ~rem_ge_m_pos_2,
      rem_ge_m_pos_2
    )
  }
  else {
    val qds_sign = Cat(
      (rem_i_signed + M_POS_2_NEGATED).head(1),
      (rem_i_signed + M_POS_1_NEGATED).head(1),
      (rem_i_signed + M_NEG_0_NEGATED).head(1),
      (rem_i_signed + M_NEG_1_NEGATED).head(1)
    )
    io.quo_dig_o := Cat(
      qds_sign(1,0) === "b11".U(2.W),
      qds_sign(1,0) === "b10".U(2.W),
      qds_sign(2,1) === "b10".U(2.W),
      qds_sign(3,2) === "b10".U(2.W),
      qds_sign(3,2) === "b00".U(2.W)
    )
  }
}

private[vector] class r4_qds_v2_spec(
                      val QDS_ARCH:Int = 2
                    ) extends Module{
  val io = IO(new Bundle {
    val rem_i = Input(UInt(7.W))
    val divisor_mul_pos_2_i = Input(UInt(7.W))
    val divisor_mul_pos_1_i = Input(UInt(7.W))
    val divisor_mul_neg_1_i = Input(UInt(7.W))
    val divisor_mul_neg_2_i = Input(UInt(7.W))
    val prev_quo_dig_i = Input(UInt(5.W))
    val quo_dig_o = Output(UInt(5.W))
  })

  val M_POS_2 = 12.S(6.W)
  val M_POS_1 = 3.S(6.W)

  val M_NEG_0 = -4.S(6.W)
  val M_NEG_1 = -13.S(6.W)

  val M_POS_2_NEGATED = -12.S(6.W)
  val M_POS_1_NEGATED = -3.S(6.W)
  val M_NEG_0_NEGATED = 4.S(6.W)
  val M_NEG_1_NEGATED = 13.S(6.W)


  val rem_i_signed = io.rem_i.asSInt
  if (QDS_ARCH == 0) {
    val adder_7b_spec = Wire(Vec(5,UInt(7.W)))
    adder_7b_spec(4) := io.rem_i + io.divisor_mul_pos_2_i
    adder_7b_spec(3) := io.rem_i + io.divisor_mul_pos_1_i
    adder_7b_spec(2) := io.rem_i
    adder_7b_spec(1) := io.rem_i + io.divisor_mul_neg_1_i
    adder_7b_spec(0) := io.rem_i + io.divisor_mul_neg_2_i
    val quo_dig_spec = Wire(Vec(5,UInt(5.W)))
    for (i <- 0 until 5){
      quo_dig_spec(i) := Cat(
        adder_7b_spec(i).head(6).asSInt <= -14.S(6.W),
        adder_7b_spec(i).head(6).asSInt >= -13.S(6.W) & adder_7b_spec(i).head(6).asSInt <= -5.S(6.W),
        adder_7b_spec(i).head(6).asSInt >= -4.S(6.W) & adder_7b_spec(i).head(6).asSInt <= 2.S(6.W),
        adder_7b_spec(i).head(6).asSInt >= 3.S(6.W) & adder_7b_spec(i).head(6).asSInt <= 11.S(6.W),
        adder_7b_spec(i).head(6).asSInt >= 12.S(6.W)
      )
    }
    io.quo_dig_o :=
      (Fill(5,io.prev_quo_dig_i(4)) & quo_dig_spec(4)) |
        (Fill(5,io.prev_quo_dig_i(3)) & quo_dig_spec(3)) |
        (Fill(5,io.prev_quo_dig_i(2)) & quo_dig_spec(2)) |
        (Fill(5,io.prev_quo_dig_i(1)) & quo_dig_spec(1)) |
        (Fill(5,io.prev_quo_dig_i(0)) & quo_dig_spec(0))
  }
  else if (QDS_ARCH == 1) {
    val adder_7b_spec = Wire(Vec(5,UInt(7.W)))
    adder_7b_spec(4) := io.rem_i + io.divisor_mul_pos_2_i
    adder_7b_spec(3) := io.rem_i + io.divisor_mul_pos_1_i
    adder_7b_spec(2) := io.rem_i
    adder_7b_spec(1) := io.rem_i + io.divisor_mul_neg_1_i
    adder_7b_spec(0) := io.rem_i + io.divisor_mul_neg_2_i
    val rem_ge_m_pos_2_spec = Wire(Vec(5,UInt(1.W)))
    val rem_ge_m_pos_1_spec = Wire(Vec(5,UInt(1.W)))
    val rem_ge_m_neg_0_spec = Wire(Vec(5,UInt(1.W)))
    val rem_ge_m_neg_1_spec = Wire(Vec(5,UInt(1.W)))
    for (i <- 0 until 5) {
      rem_ge_m_pos_2_spec(i) := adder_7b_spec(i).head(6).asSInt >= M_POS_2
      rem_ge_m_pos_1_spec(i) := adder_7b_spec(i).head(6).asSInt >= M_POS_1
      rem_ge_m_neg_0_spec(i) := adder_7b_spec(i).head(6).asSInt >= M_NEG_0
      rem_ge_m_neg_1_spec(i) := adder_7b_spec(i).head(6).asSInt >= M_NEG_1
    }
    val rem_ge_m_pos_2 =
      (io.prev_quo_dig_i(4) & rem_ge_m_pos_2_spec(4)) |
        (io.prev_quo_dig_i(3) & rem_ge_m_pos_2_spec(3)) |
        (io.prev_quo_dig_i(2) & rem_ge_m_pos_2_spec(2)) |
        (io.prev_quo_dig_i(1) & rem_ge_m_pos_2_spec(1)) |
        (io.prev_quo_dig_i(0) & rem_ge_m_pos_2_spec(0))
    val rem_ge_m_pos_1 =
      (io.prev_quo_dig_i(4) & rem_ge_m_pos_1_spec(4)) |
        (io.prev_quo_dig_i(3) & rem_ge_m_pos_1_spec(3)) |
        (io.prev_quo_dig_i(2) & rem_ge_m_pos_1_spec(2)) |
        (io.prev_quo_dig_i(1) & rem_ge_m_pos_1_spec(1)) |
        (io.prev_quo_dig_i(0) & rem_ge_m_pos_1_spec(0))
    val rem_ge_m_neg_0 =
      (io.prev_quo_dig_i(4) & rem_ge_m_neg_0_spec(4)) |
        (io.prev_quo_dig_i(3) & rem_ge_m_neg_0_spec(3)) |
        (io.prev_quo_dig_i(2) & rem_ge_m_neg_0_spec(2)) |
        (io.prev_quo_dig_i(1) & rem_ge_m_neg_0_spec(1)) |
        (io.prev_quo_dig_i(0) & rem_ge_m_neg_0_spec(0))
    val rem_ge_m_neg_1 =
      (io.prev_quo_dig_i(4) & rem_ge_m_neg_1_spec(4)) |
        (io.prev_quo_dig_i(3) & rem_ge_m_neg_1_spec(3)) |
        (io.prev_quo_dig_i(2) & rem_ge_m_neg_1_spec(2)) |
        (io.prev_quo_dig_i(1) & rem_ge_m_neg_1_spec(1)) |
        (io.prev_quo_dig_i(0) & rem_ge_m_neg_1_spec(0))
    io.quo_dig_o := Cat(
      ~rem_ge_m_neg_1,
      rem_ge_m_neg_1 & (~rem_ge_m_neg_0).asUInt,
      rem_ge_m_neg_0 & (~rem_ge_m_pos_1).asUInt,
      rem_ge_m_pos_1 & (~rem_ge_m_pos_2).asUInt,
      rem_ge_m_pos_2
    )
  }
  else {
    val qds_sign_spec = Wire(Vec(5,UInt(4.W)))
    qds_sign_spec(4) := Cat(
      (io.rem_i + io.divisor_mul_pos_2_i + Cat(M_POS_2_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_2_i + Cat(M_POS_1_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_2_i + Cat(M_NEG_0_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_2_i + Cat(M_NEG_1_NEGATED,0.U(1.W))).head(1)
    )
    qds_sign_spec(3) := Cat(
      (io.rem_i + io.divisor_mul_pos_1_i + Cat(M_POS_2_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_1_i + Cat(M_POS_1_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_1_i + Cat(M_NEG_0_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_pos_1_i + Cat(M_NEG_1_NEGATED,0.U(1.W))).head(1)
    )
    qds_sign_spec(2) := Cat(
      (io.rem_i + Cat(M_POS_2_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + Cat(M_POS_1_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + Cat(M_NEG_0_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + Cat(M_NEG_1_NEGATED,0.U(1.W))).head(1)
    )
    qds_sign_spec(1) := Cat(
      (io.rem_i + io.divisor_mul_neg_1_i + Cat(M_POS_2_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_1_i + Cat(M_POS_1_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_1_i + Cat(M_NEG_0_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_1_i + Cat(M_NEG_1_NEGATED,0.U(1.W))).head(1)
    )
    qds_sign_spec(0) := Cat(
      (io.rem_i + io.divisor_mul_neg_2_i + Cat(M_POS_2_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_2_i + Cat(M_POS_1_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_2_i + Cat(M_NEG_0_NEGATED,0.U(1.W))).head(1),
      (io.rem_i + io.divisor_mul_neg_2_i + Cat(M_NEG_1_NEGATED,0.U(1.W))).head(1)
    )
    val qds_sign =
      (Fill(4,io.prev_quo_dig_i(4)) & qds_sign_spec(4)) |
        (Fill(4,io.prev_quo_dig_i(3)) & qds_sign_spec(3)) |
        (Fill(4,io.prev_quo_dig_i(2)) & qds_sign_spec(2)) |
        (Fill(4,io.prev_quo_dig_i(1)) & qds_sign_spec(1)) |
        (Fill(4,io.prev_quo_dig_i(0)) & qds_sign_spec(0))
    io.quo_dig_o := Cat(
      qds_sign(1,0) === "b11".U,
      qds_sign(1,0) === "b10".U,
      qds_sign(2,1) === "b10".U,
      qds_sign(3,2) === "b10".U,
      qds_sign(3,2) === "b00".U,
    )
  }
}

