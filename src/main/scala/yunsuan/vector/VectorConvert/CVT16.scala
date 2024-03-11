package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.util._


class CVT16(width: Int = 16) extends CVT(width){
  /** cycle0                                                              | cycle1                  |   cycle2
   * fp2int   in(16) raw_in(17)  left,right    ShiftRightJam(37)          | RoundingUnit(11)  adder |
   * int2fp   in(16) in_abs(16)  lzc in_shift  exp_raw                    | RoundingUnit(10)  adder |  -> result & fflags
   * vfr      in(16)             lzc exp_nor   sig_nor  clz out_exp adder | Table                   |
   *
   */
  // control path
  val fire = io.fire
  val fireReg = GatedValidRegNext(io.fire)
  val is_sew_8 = io.sew === "b00".U
  val is_sew_16 = io.sew === "b01".U
  val is_single = io.opType.tail(3).head(2) === "b00".U
  val is_widen = io.opType.tail(3).head(2) === "b01".U
  val is_narrow = io.opType.tail(3).head(2) === "b10".U
  val is_single_reg0 = RegEnable(is_single, false.B, fire)
  val is_widen_reg0 = RegEnable(is_widen, false.B, fire)
  val is_narrow_reg0 = RegEnable(is_narrow, false.B, fire)

  val is_vfr = io.opType(5).asBool && is_sew_16
  val is_fp2int = io.opType.head(2) === "b10".U && (is_sew_16 && is_single || is_sew_8 && is_narrow)
  val is_int2fp = io.opType.head(2) === "b01".U && (is_sew_8 && is_widen || is_sew_16 && is_single)
  val is_vfr_reg0 = RegEnable(is_vfr, false.B, fire)
  val is_fp2int_reg0 = RegEnable(is_fp2int, false.B, fire)
  val is_int2fp_reg0 = RegEnable(is_int2fp, false.B, fire)

  val is_vfrsqrt7 = is_vfr && !io.opType(0).asBool
  val is_vfrec7 = is_vfr && io.opType(0).asBool
  val is_vfrsqrt7_reg0 = RegEnable(is_vfrsqrt7, false.B, fire)
  val is_vfrec7_reg0 = RegEnable(is_vfrec7, false.B, fire)

  val is_signed_int = io.opType(0)
  val is_signed_int_reg0 = RegEnable(is_signed_int, false.B, fire)

  val result = Wire(UInt(16.W))
  val NV, DZ, OF, UF, NX = WireInit(false.B)
  val fflags = WireInit(Cat(NV, DZ, OF, UF, NX))

  val result0 = Wire(UInt(16.W))
  val result0_reg1 = RegEnable(result0, 0.U(16.W), fireReg)
  val fflags0 = WireInit(Cat(NV, DZ, OF, UF, NX))
  val fflags0_reg1 = RegEnable(fflags0, fireReg)

  val round_in = Wire(UInt(11.W))
  val round_roundIn = Wire(Bool())
  val round_stickyIn = Wire(Bool())
  val round_signIn = Wire(Bool())
  val round_in_reg0 = RegEnable(round_in, 0.U(11.W), fire)
  val round_roundIn_reg0 = RegEnable(round_roundIn, false.B, fire)
  val round_stickyIn_reg0 = RegEnable(round_stickyIn, false.B, fire)
  val round_signIn_reg0 = RegEnable(round_signIn, false.B, fire)
  val rm_reg0 = RegEnable(io.rm, fire)

  val is_normal = Wire(Bool())
  val is_inf = Wire(Bool())
  val is_nan = Wire(Bool())
  val is_neginf = Wire(Bool())
  val is_neginf_negzero = Wire(Bool())
  val is_negzero = Wire(Bool())
  val is_poszero = Wire(Bool())
  val is_snan = Wire(Bool())
  val is_neg2_bplus1_b = Wire(Bool())
  val is_neg2_b_bminus1 = Wire(Bool())
  val is_neg2_negbminus1_negzero = Wire(Bool())
  val is_pos2_poszero_negbminus1 = Wire(Bool())
  val is_pos2_bminus1_b = Wire(Bool())
  val is_pos2_b_bplus1 = Wire(Bool())

  val is_normal_reg0 = RegEnable(is_normal, false.B, fire)
  val is_inf_reg0 = RegEnable(is_inf, false.B, fire)
  val is_nan_reg0 = RegEnable(is_nan, false.B, fire)
  val is_neginf_reg0 = RegEnable(is_neginf, false.B, fire)
  val is_neginf_negzero_reg0 = RegEnable(is_neginf_negzero, false.B, fire)
  val is_negzero_reg0 = RegEnable(is_negzero, false.B, fire)
  val is_poszero_reg0 = RegEnable(is_poszero, false.B, fire)
  val is_snan_reg0 = RegEnable(is_snan, false.B, fire)
  val is_neg2_bplus1_b_reg0 = RegEnable(is_neg2_bplus1_b, false.B, fire)
  val is_neg2_b_bminus1_reg0 = RegEnable(is_neg2_b_bminus1, false.B, fire)
  val is_neg2_negbminus1_negzero_reg0 = RegEnable(is_neg2_negbminus1_negzero, false.B, fire)
  val is_pos2_poszero_negbminus1_reg0 = RegEnable(is_pos2_poszero_negbminus1, false.B, fire)
  val is_pos2_bminus1_b_reg0 = RegEnable(is_pos2_bminus1_b, false.B, fire)
  val is_pos2_b_bplus1_reg0 = RegEnable(is_pos2_b_bplus1, false.B, fire)

  // cycle 0
  // in
  val in_sext = Wire(UInt(17.W))
  in_sext := Mux1H(
    Seq(is_int2fp && !is_signed_int && is_widen,
      is_int2fp && is_signed_int && is_widen,
      is_int2fp && !is_signed_int && is_single,
      is_int2fp && is_signed_int && is_single),
    Seq(Cat(Fill(9, 0.U), io.src(7,0)),
      Cat(Fill(9, io.src(7)), io.src(7,0)),
      Cat(0.U, io.src),
      Cat(io.src(15), io.src))
  )
  val in_orR = Wire(Bool())
  val in_orR_reg0 = RegEnable(in_orR, false.B, fire)
  in_orR := in_sext.orR

  val fp_in = VectorFloat.fromUInt(io.src, f16.expWidth, f16.precision)
  val fp2int_in = Wire(UInt(17.W))
  fp2int_in := RawVectorFloat.fromVFP(fp_in, Some(fp_in.decode.expNotZero)).asUInt

  val in = Wire(UInt(17.W))
  val in_reg0 = RegEnable(in, 0.U(17.W), fire)
  in := Mux1H(
    Seq(is_fp2int,
      is_int2fp,
      is_vfr),
    Seq(fp2int_in,
      in_sext,
      Cat(0.U, io.src))
  )

  val sign = Wire(Bool())
  val sign_reg0 = RegEnable(sign, false.B, fire)
  sign := Mux1H(
    Seq(is_fp2int || is_int2fp,
      is_vfr),
    Seq(in.head(1).asBool,
      in.tail(1).head(1).asBool)
  )


  val exp = Mux1H(
    Seq(is_fp2int,
      is_vfr),
    Seq(in.tail(1).head(f16.expWidth),
      in.tail(2).head(f16.expWidth))
  )
  val sig = Mux1H(
    Seq(is_fp2int,
      is_vfr),
    Seq(in.tail(6),
      in.tail(7))
  )

  // fp2int
  // left
  val max_int_exp = Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow),
    Seq(30.U,
      22.U)
  )

  val exp_of = Wire(Bool())
  val exp_of_reg0 = RegEnable(exp_of, false.B, fire)
  exp_of := is_fp2int && (exp > max_int_exp)
  
  val lpath_shamt = Mux(is_fp2int && is_single, exp - 25.U, 0.U)
  val lpath_sig_shifted = Wire(UInt(16.W))
  val lpath_sig_shifted_reg0 = RegEnable(lpath_sig_shifted, 0.U(16.W), fire)
  lpath_sig_shifted := Mux(is_fp2int && is_single, (sig << lpath_shamt(4, 0))(15, 0), 0.U)

  val lpath_iv = Wire(Bool())
  val lpath_iv_reg0 = RegEnable(lpath_iv, false.B, fire)
  lpath_iv := is_fp2int && is_single && !is_signed_int && sign


  val lpath_of = Wire(Bool())
  val lpath_of_reg0 = RegEnable(lpath_of, fire)
  lpath_of := is_fp2int && is_single && is_signed_int && (exp === max_int_exp) && (!sign || (sign && in.tail(7).orR))


  // right
  val rpath_shamt = Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow),
    Seq(25.U - exp,
    22.U - exp)
  )

  val rpath_sig_shifted0 = Wire(UInt(12.W))
  val rpath_sig_shifted_reg0 = RegEnable(rpath_sig_shifted0, 0.U(12.W), fire)
  val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(Cat(sig, 0.U), rpath_shamt)
  rpath_sig_shifted0 := rpath_sig_shifted


  // int2fp
  val in_abs = Mux1H(
    Seq(is_int2fp && sign,
      is_int2fp && !sign),
    Seq((~in).asUInt + 1.U,
      in)
  )

  val int2fp_clz = Mux(is_int2fp, CLZ(in_abs(15,0)), 0.U)

  val in_shift = Mux(is_int2fp, (in_abs << int2fp_clz)(14, 0), 0.U)

  val exp_raw = Wire(UInt(5.W))
  val exp_raw_reg0 = RegEnable(exp_raw, 0.U(5.W), fire)
  exp_raw := Mux(is_int2fp, 30.U - int2fp_clz, 0.U)



  // share RoundingUnit
  round_in := Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow,
      is_int2fp && is_widen,
      is_int2fp && is_single),
    Seq(rpath_sig_shifted.head(f16.precision),
      rpath_sig_shifted.head(8),
      in_shift.head(8),
      in_shift.head(10))
  )
  round_roundIn := Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow,
      is_int2fp && is_widen,
      is_int2fp && is_single),
    Seq(rpath_sig_shifted.tail(f16.precision),
      rpath_sig_shifted.tail(8).head(1),
      in_shift.tail(8).head(1),
      in_shift.tail(10).head(1))
  )
  round_stickyIn := Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow,
      is_int2fp && is_widen,
      is_int2fp && is_single),
    Seq(rpath_sticky,
      rpath_sticky || rpath_sig_shifted.tail(9).orR,
      in_shift.tail(9).orR,
      in_shift.tail(f16.precision).orR)
  )
  round_signIn := sign

  val sel_lpath = Wire(Bool())
  val sel_lpath_reg0 = RegEnable(sel_lpath, fire)
  sel_lpath := exp >= 25.U


  val max_int = Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow),
    Seq(Cat(!is_signed_int, ~0.U(15.W)),
      Cat(!is_signed_int,   ~0.U(7.W)))
  )
  val min_int = Mux1H(
    Seq(is_fp2int && is_single,
      is_fp2int && is_narrow),
    Seq(Cat(is_signed_int, 0.U(15.W)),
      Cat(is_signed_int,   0.U(7.W)))
  )
  val max_min_int = Wire(UInt(16.W))
  val max_min_int_reg0 = RegEnable(max_min_int, 0.U(16.W), fire)
  max_min_int := Mux(exp.andR && sig.tail(1).orR || !sign, max_int, min_int)


  // vfr
  is_normal := exp.orR & !exp.andR
  val is_subnormal = !exp.orR
  is_inf := exp.andR & !sig.tail(1).orR
  is_nan := exp.andR & sig.tail(1).orR
  is_neginf := sign & is_inf
  is_neginf_negzero := sign & (is_normal | is_subnormal & sig.tail(1).orR)
  is_negzero := sign & is_subnormal & !sig.tail(1).orR
  is_poszero := !sign & is_subnormal & !sig.tail(1).orR
  val is_poszero_posinf = !sign & (is_normal | is_subnormal & sig.tail(1).orR)
  is_snan := !sig.tail(1).head(1).asBool & is_nan
  is_neg2_bplus1_b := sign & (exp === 30.U)
  is_neg2_b_bminus1 := sign & (exp === 29.U)
  is_neg2_negbminus1_negzero := sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).orR
  is_pos2_poszero_negbminus1 := !sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).orR
  is_pos2_bminus1_b := !sign & (exp === 29.U)
  is_pos2_b_bplus1 := !sign & (exp === 30.U)

  val zero_minus_lzc = Mux(is_vfr, 0.U - CLZ(sig.tail(1)), 0.U)

  val exp_normalized = Wire(UInt(5.W))
  val exp_normalized_reg0 = RegEnable(exp_normalized, fire)
  exp_normalized := Mux1H(
    Seq(is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal),
      is_vfrsqrt7 && is_poszero_posinf && is_subnormal || (is_vfrec7 && is_subnormal)),
    Seq(exp,
      Cat(Fill(f16.expWidth - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc))
  )


  val sig_normalized = Wire(UInt(11.W))
  val sig_normalized_reg0 = RegEnable(sig_normalized, fire)
  sig_normalized := Mux1H(
    Seq(is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal),
      is_vfrsqrt7 && is_poszero_posinf && is_subnormal || (is_vfrec7 && is_subnormal)),
    Seq(Cat(0.U, sig.tail(1)),
      (sig.tail(1) << 1.U).asUInt)
  )


  val clz_sig = Wire(UInt(4.W))
  val clz_sig_reg0 = RegEnable(clz_sig, fire)
  clz_sig := CLZ(sig_normalized)


  val out_exp_normalized = Mux(is_vfrec7, 29.U - exp_normalized, 0.U)

  val out_exp_zero_negone = Wire(Bool())
  val out_exp_zero_negone_reg0 = RegEnable(out_exp_zero_negone, fire)
  out_exp_zero_negone := is_vfrec7 && !out_exp_normalized.orR || out_exp_normalized.andR


  val out_exp = Wire(UInt(5.W))
  val out_exp_reg0 = RegEnable(out_exp, 0.U(5.W), fire)
  out_exp := Mux1H(
    Seq(is_vfrsqrt7 && is_normal,
      is_vfrsqrt7 && is_subnormal,
      is_vfrec7 && out_exp_zero_negone,
      is_vfrec7 && !out_exp_zero_negone
    ),
    Seq((44.U - exp) >> 1,
      (44.U + CLZ(sig.tail(1))) >> 1,
      0.U,
      out_exp_normalized)
  )


  val out_sign = Wire(Bool())
  val out_sign_reg0 = RegEnable(out_sign, fire)
  out_sign := is_vfrsqrt7 && is_poszero_posinf && sign


  // cycle1
  val rounder = Module(new RoundingUnit(f16.precision))
  rounder.io.in := round_in_reg0
  rounder.io.roundIn := round_roundIn_reg0
  rounder.io.stickyIn := round_stickyIn_reg0
  rounder.io.signIn := round_signIn_reg0
  rounder.io.rm := rm_reg0

  val out_reg0 = Mux(rounder.io.r_up, rounder.io.in + 1.U, rounder.io.in)
  val cout_reg0 = rounder.io.r_up && Mux1H(
    Seq(is_fp2int_reg0 && is_narrow_reg0,
      is_fp2int_reg0 && is_single_reg0,
      is_int2fp_reg0),
    Seq(rounder.io.in.tail(4).andR,
      rounder.io.in.andR,
      rounder.io.in.tail(1).andR)
  )
  val exp_reg0 = Mux1H(
    Seq(is_int2fp_reg0 && in_orR_reg0,
      is_int2fp_reg0 && !in_orR_reg0),
    Seq(exp_raw_reg0 + cout_reg0,
      0.U)
  )
  val sig_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && is_single_reg0,
      is_fp2int_reg0 && is_narrow_reg0,
      is_int2fp_reg0),
    Seq(Cat(0.U(4.W), cout_reg0, out_reg0),
      Cat(0.U(5.W), out_reg0),
      out_reg0)
  )
  val rpath_ix_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && is_single_reg0,
      is_fp2int_reg0 && is_narrow_reg0),
    Seq(rounder.io.inexact,
      rounder.io.inexact || rpath_sig_shifted_reg0.tail(8).orR)
  )
  val rpath_iv_reg0 = is_fp2int_reg0 && !is_signed_int_reg0 && in_reg0.head(1).asBool && sig_reg0.orR

  val rpath_pos_of_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && !in_reg0.head(1).asBool && is_signed_int_reg0,
      is_fp2int_reg0 && !in_reg0.head(1).asBool && !is_signed_int_reg0),
    Seq((in_reg0.tail(1).head(5) === 22.U) || ((in_reg0.tail(1).head(5) === 21.U) && cout_reg0),
      (in_reg0.tail(1).head(5) === 22.U) && cout_reg0)
  )
  val rpath_neg_of_reg0 = is_fp2int_reg0 && in_reg0.head(1).asBool && (in_reg0.tail(1).head(5) === 22.U) && (rounder.io.in.tail(4).orR || rounder.io.r_up)

  val rpath_of_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && is_narrow_reg0,
      is_fp2int_reg0 && is_single_reg0),
    Seq(rpath_pos_of_reg0 || rpath_neg_of_reg0,
      cout_reg0)
  )


  val vfrsqrt7Table = Module(new Rsqrt7Table)
  val vfrec7Table = Module(new Rec7Table)

  val sig_in7 = Mux1H(
    Seq(is_vfr_reg0 && is_vfrsqrt7_reg0,
      is_vfr_reg0 && is_vfrec7_reg0),
    Seq(Cat(exp_normalized_reg0(0), (sig_normalized_reg0 << Mux(is_normal_reg0, 0.U, clz_sig_reg0))(9, 4)),
      (sig_normalized_reg0 << Mux(is_normal_reg0, 0.U, clz_sig_reg0))(9, 3))
  )
  vfrsqrt7Table.src := sig_in7
  vfrec7Table.src := sig_in7

  val sig_out7_reg0 = Wire(UInt(7.W))
  sig_out7_reg0 := Mux1H(
    Seq(is_vfr_reg0 && is_vfrsqrt7_reg0,
      is_vfr_reg0 && is_vfrec7_reg0),
    Seq(vfrsqrt7Table.out,
      vfrec7Table.out)
  )

  val out_sig_reg0 = Mux1H(
    Seq(is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0),
      is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      is_vfrec7_reg0 && out_exp_zero_negone_reg0 && !(is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0 || is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      is_vfrec7_reg0 && !out_exp_zero_negone_reg0),
    Seq(Cat(0.U, 1.U, sig_out7_reg0, 0.U),
      Cat(1.U, sig_out7_reg0, 0.U(2.W)),
      Cat(1.U, sig_out7_reg0, 0.U(2.W)) >> 1,
      Cat(sig_out7_reg0, 0.U(3.W)))
  )

  val fp_result = Wire(UInt(16.W))
  fp_result := Mux1H(
    Seq(is_vfrsqrt7_reg0,
      is_vfrec7_reg0),
    Seq(Cat(out_sign_reg0, out_exp_reg0, sig_out7_reg0, 0.U(3.W)),
      Cat(sign_reg0, out_exp_reg0, out_sig_reg0))
  )

  val result_nan = Cat(0.U(1.W), Fill(6, 1.U), 0.U(9.W))
  val result_inf = Cat(Fill(5, 1.U), 0.U(10.W))
  val result_greatest_fin = Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U))


  val of_reg0 = Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0),
    Seq(exp_of_reg0 || sel_lpath_reg0 && lpath_of_reg0 || !sel_lpath_reg0 && rpath_of_reg0,
      exp_reg0 === 31.U)
  )
  val iv_reg0 = is_fp2int_reg0 && (of_reg0 || sel_lpath_reg0 && lpath_iv_reg0 || !sel_lpath_reg0 && rpath_iv_reg0)

  val ix_reg0 = Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0),
    Seq(!iv_reg0 && !sel_lpath_reg0 && rpath_ix_reg0,
      rounder.io.inexact)
  )

  val int_abs_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && sel_lpath_reg0,
      is_fp2int_reg0 && !sel_lpath_reg0),
    Seq(lpath_sig_shifted_reg0,
      sig_reg0)
  )
  val int_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && is_narrow_reg0,
      is_fp2int_reg0 && is_single_reg0),
    Seq(Mux(in_reg0.head(1).asBool && is_signed_int_reg0, -int_abs_reg0.tail(8), int_abs_reg0.tail(8)),
      Mux(in_reg0.head(1).asBool && is_signed_int_reg0, -int_abs_reg0, int_abs_reg0))
  )

  result0 := Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0,
      is_vfrsqrt7_reg0 && (is_nan_reg0 || is_neginf_negzero_reg0) || is_vfrec7_reg0 && is_nan_reg0,
      is_vfrsqrt7_reg0 && is_inf_reg0,
      is_vfrsqrt7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
      is_vfrsqrt7_reg0 && !(is_nan_reg0 || is_neginf_negzero_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0) ||
        is_vfrec7_reg0 && !(is_nan_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0 || is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0),
      is_vfrec7_reg0 && is_inf_reg0,
      is_vfrec7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
      is_vfrec7_reg0 && is_neg2_negbminus1_negzero_reg0,
      is_vfrec7_reg0 && is_pos2_poszero_negbminus1_reg0
    ),
    Seq(Mux(iv_reg0, max_min_int_reg0, int_reg0),
      Cat(is_signed_int_reg0 && sign_reg0, exp_reg0, Mux(is_widen_reg0, Cat(sig_reg0(7,0), 0.U(2.W)), sig_reg0(9,0))),
      result_nan,
      Mux(is_neginf_reg0, result_nan, 0.U),
      Mux(is_negzero_reg0, Cat(1.U, result_inf), Cat(0.U, result_inf)),
      fp_result,
      Mux(is_neginf_reg0, Cat(1.U, 0.U(15.W)), 0.U),
      Mux(is_negzero_reg0, Cat(Fill(6, 1.U), 0.U(10.W)), Cat(0.U(1.W), Fill(5, 1.U), 0.U(10.W))),
      Mux(rm_reg0 === RUP || rm_reg0 === RTZ, Cat(1.U, result_greatest_fin), Cat(1.U, result_inf)),
      Mux(rm_reg0 === RDN || rm_reg0 === RTZ, Cat(0.U, result_greatest_fin), Cat(0.U, result_inf)))
  )

  fflags0 := Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0,
      is_vfrsqrt7_reg0 && (is_nan_reg0 || is_neginf_negzero_reg0),
      is_vfrsqrt7_reg0 && is_inf_reg0,
      is_vfrsqrt7_reg0 && (is_negzero_reg0 || is_poszero_reg0) || is_vfrec7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
      is_vfrsqrt7_reg0 && !(is_nan_reg0 || is_neginf_negzero_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0) ||
        is_vfrec7_reg0 && !(is_nan_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0 || is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0),
      is_vfrec7_reg0 && is_nan_reg0,
      is_vfrec7_reg0 && is_inf_reg0,
      is_vfrec7_reg0 && (is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0)),
    Seq(Cat(iv_reg0, false.B, false.B, false.B, ix_reg0),
      Cat(false.B, false.B, of_reg0, false.B, ix_reg0),
      Mux(is_snan_reg0 || is_neginf_negzero_reg0, "b10000".U, "b00000".U),
      Mux(is_neginf_reg0, "b10000".U, "b00000".U),
      "b01000".U,
      0.U,
      Mux(is_snan_reg0, "b10000".U, "b00000".U),
      Mux(is_neginf_reg0, Cat(1.U, 0.U(15.W)), 0.U),
      "b00101".U)
  )

  // cycle2
  result := result0_reg1
  fflags := fflags0_reg1

  io.result := result
  io.fflags := fflags
}
