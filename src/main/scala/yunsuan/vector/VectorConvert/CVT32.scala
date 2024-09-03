package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.vector.VectorConvert.util._
import yunsuan.vector.VectorConvert.RoundingModle._
import yunsuan.util._


class CVT32(width: Int = 32) extends CVT(width){
  /** cycle0                                 |                              cycle1                  |   cycle2
   * fp2int   in(32) raw_in(33)  left,right  |  ShiftRightJam(25)           RoundingUnit(11)  adder |
   * int2fp   in(32) in_abs(32)  lzc         | in_shift  exp_raw            RoundingUnit(10)  adder |  -> result & fflags
   * vfr      in(32)             lzc         | exp_nor   sig_nor  clz out_exp adder  Table          |
   * fp2fp
   */
  // control path
  val fire = io.fire
  val fireReg = GatedValidRegNext(io.fire)
  val is_sew_8  = io.sew === "b00".U
  val is_sew_16 = io.sew === "b01".U
  val is_sew_32 = io.sew === "b10".U

  val is_single = io.opType.tail(3).head(2) === "b00".U
  val is_widen = io.opType.tail(3).head(2) === "b01".U
  val is_narrow = io.opType.tail(3).head(2) === "b10".U
  val is_single_reg0 = RegEnable(is_single, fire)
  val is_widen_reg0 = RegEnable(is_widen, fire)
  val is_narrow_reg0 = RegEnable(is_narrow, fire)


  val is_vfr = io.opType(5).asBool && (is_sew_16 || is_sew_32)
  val is_fp2int = io.opType.head(2) === "b10".U && (is_sew_32 && is_single || is_sew_16 && is_narrow || is_sew_16 && is_widen || is_sew_16 && is_single || is_sew_8 && is_narrow)
  val is_int2fp = io.opType.head(2) === "b01".U && (is_sew_32 && is_single || is_sew_16 && is_narrow || is_sew_16 && is_widen || is_sew_16 && is_single || is_sew_8 && is_widen)
  val is_fp2fp = io.opType.head(3) === "b110".U && (is_sew_16 && (is_narrow || is_widen))
  val is_vfr_reg0 = RegEnable(is_vfr, false.B, fire)
  val is_fp2int_reg0 = RegEnable(is_fp2int, false.B, fire)
  val is_int2fp_reg0 = RegEnable(is_int2fp, false.B, fire)
  val is_fp2fp_reg0 = RegEnable(is_fp2fp, false.B, fire)

  val is_vfrsqrt7 = is_vfr && !io.opType(0).asBool
  val is_vfrec7 = is_vfr && io.opType(0).asBool
  val is_vfrsqrt7_reg0 = RegEnable(is_vfrsqrt7, false.B, fire)
  val is_vfrec7_reg0 = RegEnable(is_vfrec7, false.B, fire)

  val is_signed_int = io.opType(0)
  val is_signed_int_reg0 = RegEnable(is_signed_int, false.B, fire)

  val result = Wire(UInt(32.W))
  val NV, DZ, OF, UF, NX = WireInit(false.B)
  val fflags = WireInit(Cat(NV, DZ, OF, UF, NX))

  val result0 = Wire(UInt(32.W))
  val result0_reg1 = RegEnable(result0, 0.U(32.W), fireReg)
  val fflags0 = WireInit(Cat(NV, DZ, OF, UF, NX))
  val fflags0_reg1 = RegEnable(fflags0, fireReg)

  val round_in_reg0 = Wire(UInt(24.W))
  val round_roundIn_reg0 = Wire(Bool())
  val round_stickyIn_reg0 = Wire(Bool())
  val round_signIn_reg0 = Wire(Bool())
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

  // fp2fp
  val nor_in = Wire(UInt(10.W))
  val nor_roundBit = Wire(Bool())
  val nor_stickyBit = Wire(Bool())
  val nor_signBit = Wire(Bool())
  val nor_in_reg0 = RegEnable(nor_in, fire)
  val nor_roundBit_reg0 = RegEnable(nor_roundBit, fire)
  val nor_stickyBit_reg0 = RegEnable(nor_stickyBit, fire)
  val nor_signBit_reg0 = RegEnable(nor_signBit, fire)
  val subnor_in = Wire(UInt(10.W))
  val subnor_roundBit = Wire(Bool())
  val subnor_stickyBit = Wire(Bool())
  val subnor_signBit = Wire(Bool())
  val subnor_in_reg0 = RegEnable(subnor_in, fire)
  val subnor_roundBit_reg0 = RegEnable(subnor_roundBit, fire)
  val subnor_stickyBit_reg0 = RegEnable(subnor_stickyBit, fire)
  val subnor_signBit_reg0 = RegEnable(subnor_signBit, fire)


  // cycle0

  val in_is_fp16 = Wire(Bool())
  val in_is_fp32 = Wire(Bool())
  val fp32toint32 = Wire(Bool())
  val fp32toint16 = Wire(Bool())
  val fp16toint32 = Wire(Bool())
  val fp16toint16 = Wire(Bool())
  val fp16toint8 = Wire(Bool())

  val in_is_fp16_reg0 = RegEnable(in_is_fp16, fire)
  val in_is_fp32_reg0 = RegEnable(in_is_fp32, fire)
  val fp32toint32_reg0 = RegEnable(fp32toint32, fire)
  val fp32toint16_reg0 = RegEnable(fp32toint16, fire)
  val fp16toint32_reg0 = RegEnable(fp16toint32, fire)
  val fp16toint16_reg0 = RegEnable(fp16toint16, fire)
  val fp16toint8_reg0 = RegEnable(fp16toint8, fire)

  val out_is_fp16 = Wire(Bool())
  val out_is_fp32 = Wire(Bool())
  val int32tofp32 = Wire(Bool())
  val int16tofp32 = Wire(Bool())
  val int32tofp16 = Wire(Bool())
  val int16tofp16 = Wire(Bool())
  val int8tofp16 = Wire(Bool())
  val out_is_fp16_reg0 = RegEnable(out_is_fp16, fire)
  val out_is_fp32_reg0 = RegEnable(out_is_fp32, fire)
  val int32tofp32_reg0 = RegEnable(int32tofp32, fire)
  val int16tofp32_reg0 = RegEnable(int16tofp32, fire)
  val int32tofp16_reg0 = RegEnable(int32tofp16, fire)
  val int16tofp16_reg0 = RegEnable(int16tofp16, fire)
  val int8tofp16_reg0 = RegEnable(int8tofp16, fire)

  in_is_fp16 := Mux1H(
    Seq(is_fp2int,
      is_vfr
    ),
    Seq(is_sew_16 && is_widen || is_sew_16 && is_single || is_sew_8 && is_narrow,
      is_sew_16
    )
  )

  in_is_fp32 := Mux1H(
    Seq(is_fp2int,
      is_vfr),
    Seq(is_sew_32 && is_single || is_sew_16 && is_narrow,
      is_sew_32)
  )
  fp32toint32 := is_fp2int && in_is_fp32 && is_single
  fp32toint16 := is_fp2int && in_is_fp32 && is_narrow
  fp16toint32 := is_fp2int && in_is_fp16 && is_widen
  fp16toint16 := is_fp2int && in_is_fp16 && is_single
  fp16toint8  := is_fp2int && in_is_fp16 && is_narrow

  out_is_fp32 := Mux1H(
    Seq(is_int2fp,
      is_vfr,
      is_fp2fp),
    Seq(is_sew_32 && is_single || is_sew_16 && is_widen,
      is_sew_32,
      is_widen)
  )
  out_is_fp16 := Mux1H(
    Seq(is_int2fp,
      is_vfr,
      is_fp2fp),
    Seq(is_sew_16 && is_narrow || is_sew_16 && is_single || is_sew_8 && is_widen,
      is_sew_16,
      is_narrow)
  )
  int32tofp32 := is_int2fp && out_is_fp32 && is_single
  int16tofp32 := is_int2fp && out_is_fp32 && is_widen
  int32tofp16 := is_int2fp && out_is_fp16 && is_narrow
  int16tofp16 := is_int2fp && out_is_fp16 && is_single
  int8tofp16  := is_int2fp && out_is_fp16 && is_widen

  // in
  val src = Mux1H(
    Seq(in_is_fp32 || (is_vfr && is_sew_32) || (is_fp2fp && is_narrow),
      in_is_fp16 || (is_vfr && is_sew_16) || (is_fp2fp && is_widen)),
    Seq(io.src,
      Cat(io.src.tail(16).head(1), 0.U(3.W), io.src.tail(17).head(f16.expWidth), io.src.tail(f16.expWidth+17), 0.U(13.W)))
  )
  val fp_in = VectorFloat.fromUInt(src, f32.expWidth, f32.precision)
  val exp_not_zero = Mux1H(
    Seq(fp32toint32 || fp32toint16,
      fp16toint32 || fp16toint16 || fp16toint8),
    Seq(fp_in.decode.expNotZero,
      src.tail(4).head(f16.expWidth).orR)
  )
  val fp2int_in = Wire(UInt(33.W))
  fp2int_in := RawVectorFloat.fromVFP(fp_in, Some(exp_not_zero)).asUInt

  val in_sext = Wire(UInt(33.W))
  in_sext := Mux1H(
    Seq(!is_signed_int && (int32tofp32 || int32tofp16),
      is_signed_int && (int32tofp32 || int32tofp16),
      !is_signed_int && (int16tofp32 || int16tofp16),
      is_signed_int && (int16tofp32 || int16tofp16),
      !is_signed_int && int8tofp16,
      is_signed_int && int8tofp16
    ),
    Seq(Cat(0.U, io.src),
      Cat(io.src(31), io.src),
      Cat(0.U(17.W), io.src(15,0)),
      Cat(Fill(17, io.src(15)), io.src(15,0)),
      Cat(0.U(25.W), io.src(7,0)),
      Cat(Fill(25, io.src(7)), io.src(7,0))
    )
  )
  val in_orR = Wire(Bool())
  val in_orR_reg0 = RegEnable(in_orR, false.B, fire)
  in_orR := in_sext.orR

  val in = Wire(UInt(33.W))
  val in_reg0 = RegEnable(in, 0.U(33.W), fire)
  in := Mux1H(
    Seq(is_fp2int,
      is_int2fp,
      is_vfr),
    Seq(fp2int_in,
      in_sext,
      Cat(0.U, src))
  )
  val sign = Wire(Bool())
  val sign_reg0 = RegEnable(sign, false.B, fire)
  sign := Mux1H(
    Seq(is_fp2int || is_int2fp,
      is_vfr,
      is_fp2fp && is_widen),
    Seq(in.head(1).asBool,
      in.tail(1).head(1).asBool,
      fp_in.sign)
  )

  val exp = Mux1H(
    Seq(is_fp2int,
      is_vfr),
    Seq(in.tail(1).head(f32.expWidth),
      in.tail(2).head(f32.expWidth))
  )
  val sig = Mux1H(
    Seq(is_fp2int,
      is_vfr),
    Seq(in.tail(9),
      in.tail(10))
  )

  // fp2int
  // left
  val max_int_exp = Mux1H(
    Seq(fp32toint32,
      fp32toint16,
      fp16toint32,
      fp16toint16,
      fp16toint8,
    ),
    Seq(VectorFloat.expBias(f32.expWidth).U +& 31.U,
      VectorFloat.expBias(f32.expWidth).U +& 15.U,
      VectorFloat.expBias(f16.expWidth).U +& 31.U,
      VectorFloat.expBias(f16.expWidth).U +& 15.U,
      VectorFloat.expBias(f16.expWidth).U +& 7.U
    )
  )

  val lpath_shamt = exp - Mux1H(
    Seq(fp32toint32,
      fp16toint16 || fp16toint32
    ),
    Seq((VectorFloat.expBias(f32.expWidth) + f32.fracWidth).U,
      (VectorFloat.expBias(f16.expWidth) + f16.fracWidth).U)
  )
  val lpath_max_shamt = Mux1H(
    Seq(fp32toint32,
      fp16toint16,
      fp16toint32),
    Seq((31 - f32.fracWidth).U,
      (15 - f16.fracWidth).U,
      (31 - f16.fracWidth).U)
  )
  val lpath_sig_shifted = Wire(UInt(32.W))
  val lpath_sig_shifted_reg0 = RegEnable(lpath_sig_shifted, fire)
  lpath_sig_shifted := Mux1H(
    Seq(fp32toint32,
      fp16toint16 || fp16toint32),
    Seq((sig << lpath_shamt(lpath_max_shamt.getWidth - 1, 0))(31, 0),
      (sig << lpath_shamt(lpath_max_shamt.getWidth - 1, 0))(31, 13))
  )

  val lpath_iv = Wire(Bool())
  val lpath_iv_reg0 = RegEnable(lpath_iv, false.B, fire)

  lpath_iv := (fp32toint32 || fp16toint16 || fp16toint32) && !is_signed_int && sign

  val lpath_of = Wire(Bool())
  val lpath_of_reg0 = RegEnable(lpath_of, fire)
  lpath_of := (fp32toint32 || fp16toint16 || fp16toint32) && is_signed_int && (exp === max_int_exp) && (!sign || (sign && in.tail(10).orR))

  // right
  val rpath_shamt = Mux1H(
    Seq(fp32toint32,
      fp32toint16,
      fp16toint16 || fp16toint32,
      fp16toint8),
    Seq((VectorFloat.expBias(f32.expWidth) + f32.fracWidth).U,
      (VectorFloat.expBias(f32.expWidth) + 15).U,
      (VectorFloat.expBias(f16.expWidth) + f16.fracWidth).U,
      (VectorFloat.expBias(f16.expWidth) + 8 - 1).U)
  ) - exp

  // cycle1
  val sig_cat0 = Cat(sig, 0.U)
  val sig_cat0_reg = RegEnable(sig_cat0, fire)
  val rpath_shamt_reg0 = RegEnable(rpath_shamt, fire)
  val rpath_sig_shifted_reg0 = Wire(UInt(25.W))
  val rpath_sticky_reg0 = Wire(Bool())
  val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(sig_cat0_reg, rpath_shamt_reg0)
  rpath_sig_shifted_reg0 := rpath_sig_shifted
  rpath_sticky_reg0 := rpath_sticky

  // int2fp
  val in_abs = Mux1H(
    Seq(is_int2fp && sign,
      is_int2fp && !sign),
    Seq((~in).asUInt + 1.U,
      in)
  )
  val in_abs_reg0 = RegEnable(in_abs, fire)

  val exp_of = Wire(Bool())
  val exp_of_reg0 = RegEnable(exp_of, false.B, fire)
  exp_of := Mux1H(
    Seq(is_fp2int && in_is_fp32,
      is_fp2int && in_is_fp16,
      int32tofp16),
    Seq(exp > max_int_exp,
      exp > max_int_exp || src.tail(4).head(f16.expWidth).andR,
      in_abs.tail(1).head(16).orR)
  )

  val int2fp_clz = Mux1H(
    Seq(out_is_fp32 || exp_of,
      int32tofp16 && !in_abs.tail(1).head(16).orR || int16tofp16 || int8tofp16,
    ),
    Seq(CLZ(in_abs(31, 0)),
      CLZ(in_abs(15, 0)))
  )
  val int2fp_clz_reg0 = RegEnable(int2fp_clz, fire)

  val in_shift_reg0 = Mux1H(
    Seq(out_is_fp32_reg0 || exp_of_reg0,
      int32tofp16_reg0 && !in_abs_reg0.tail(1).head(16).orR || int16tofp16_reg0 || int8tofp16_reg0
    ),
    Seq((in_abs_reg0.tail(1) << int2fp_clz_reg0)(30, 0),
      Cat((in_abs_reg0.tail(1) << int2fp_clz_reg0)(14, 0), 0.U(16.W)))
  )

  val exp_raw_reg0 = Wire(UInt(8.W))
  exp_raw_reg0 := Mux1H(
    Seq(is_int2fp_reg0 && out_is_fp32_reg0,
      is_int2fp_reg0 && out_is_fp16_reg0),
    Seq(VectorFloat.expBias(f32.expWidth).asUInt +& 31.U - int2fp_clz_reg0,
      VectorFloat.expBias(f16.expWidth).asUInt +& 15.U - int2fp_clz_reg0)
  )

  // share RoundingUnit
  round_in_reg0 := Mux1H(
    Seq(fp32toint32_reg0,
      fp32toint16_reg0,
      fp16toint16_reg0 || fp16toint32_reg0,
      fp16toint8_reg0,
      int16tofp32_reg0,
      int32tofp32_reg0,
      int8tofp16_reg0,
      int16tofp16_reg0 || int32tofp16_reg0),
    Seq(rpath_sig_shifted_reg0.head(f32.precision),
      rpath_sig_shifted_reg0.head(16),
      rpath_sig_shifted_reg0.head(f16.precision),
      rpath_sig_shifted_reg0.head(8),
      in_shift_reg0.head(16),
      in_shift_reg0.head(23),
      in_shift_reg0.head(8),
      in_shift_reg0.head(10))
  )
  round_roundIn_reg0 := Mux1H(
    Seq(fp32toint32_reg0,
      fp32toint16_reg0,
      fp16toint16_reg0 || fp16toint32_reg0,
      fp16toint8_reg0,
      int16tofp32_reg0,
      int32tofp32_reg0,
      int8tofp16_reg0,
      int16tofp16_reg0 || int32tofp16_reg0),
    Seq(rpath_sig_shifted_reg0.tail(f32.precision).head(1),
      rpath_sig_shifted_reg0.tail(16).head(1),
      rpath_sig_shifted_reg0.tail(f16.precision).head(1),
      rpath_sig_shifted_reg0.tail(8).head(1),
      in_shift_reg0.tail(16).head(1),
      in_shift_reg0.tail(23).head(1),
      in_shift_reg0.tail(8).head(1),
      in_shift_reg0.tail(10).head(1)
    )
  )
  round_stickyIn_reg0 := Mux1H(
    Seq(fp32toint32_reg0,
      fp32toint16_reg0,
      fp16toint16_reg0 || fp16toint32_reg0,
      fp16toint8_reg0,
      int16tofp32_reg0,
      int32tofp32_reg0,
      int8tofp16_reg0,
      int16tofp16_reg0 || int32tofp16_reg0),
    Seq(rpath_sticky_reg0,
      rpath_sticky_reg0 || rpath_sig_shifted_reg0.tail(17).orR,
      rpath_sticky_reg0 || rpath_sig_shifted_reg0.tail(12).orR,
      rpath_sticky_reg0 || rpath_sig_shifted_reg0.tail(9).orR,
      in_shift_reg0.tail(16).orR,
      in_shift_reg0.tail(f32.precision).orR,
      in_shift_reg0.tail(8).orR,
      in_shift_reg0.tail(f16.precision).orR
    )
  )
  round_signIn_reg0 := (is_fp2int_reg0 || is_int2fp_reg0) && sign_reg0

  val sel_lpath = Wire(Bool())
  val sel_lpath_reg0 = RegEnable(sel_lpath, fire)
  sel_lpath := exp >= Mux1H(
    Seq(is_fp2int && in_is_fp32,
      is_fp2int && in_is_fp16),
    Seq((VectorFloat.expBias(f32.expWidth) + f32.fracWidth).U,
      (VectorFloat.expBias(f16.expWidth) + f16.fracWidth).U)
  )

  val max_int = Mux1H(
    Seq(fp32toint32 || fp16toint32,
      fp32toint16 || fp16toint16,
      fp16toint8),
    Seq(Cat(!is_signed_int, ~0.U(31.W)),
      Cat(!is_signed_int, ~0.U(15.W)),
      Cat(!is_signed_int, ~0.U(7.W)))
  )
  val min_int = Mux1H(
    Seq(fp32toint32 || fp16toint32,
      fp32toint16 || fp16toint16,
      fp16toint8),
    Seq(Cat(is_signed_int, 0.U(31.W)),
      Cat(is_signed_int, 0.U(15.W)),
      Cat(is_signed_int, 0.U(7.W)))
  )
  val max_min_int = Wire(UInt(32.W))
  val max_min_int_reg0 = RegEnable(max_min_int, 0.U(32.W), fire)

  val sign_or_nan = Mux1H(
    Seq(fp32toint32 || fp32toint16,
      fp16toint32 || fp16toint16 || fp16toint8),
    Seq(fp_in.decode.isNaN,
      src.tail(4).head(f16.expWidth).andR && src.tail(9).orR)) | !sign

  max_min_int := Mux(sign_or_nan, max_int, min_int)

  // vfr
  is_normal := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(exp.tail(3).orR & !exp.tail(3).andR,
      exp.orR & !exp.andR)
  )

  val is_subnormal = Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!exp.tail(3).orR,
      !exp.orR)
  )
  is_inf := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(exp.tail(3).andR & !sig.tail(1).head(10).orR,
      exp.andR & !sig.tail(1).orR)
  )
  is_nan := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(exp.tail(3).andR & sig.tail(1).head(10).orR,
      exp.andR & sig.tail(1).orR)
  )
  is_neginf := is_vfr && sign & is_inf

  is_neginf_negzero := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(sign & (is_normal | is_subnormal & sig.tail(1).head(10).orR),
      sign & (is_normal | is_subnormal & sig.tail(1).orR))
  )

  is_negzero := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(sign & is_subnormal & !sig.tail(1).head(10).orR,
      sign & is_subnormal & !sig.tail(1).orR)
  )

  is_poszero := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!sign & is_subnormal & !sig.tail(1).head(10).orR,
      !sign & is_subnormal & !sig.tail(1).orR)
  )

  val is_poszero_posinf = Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!sign & (is_normal | is_subnormal & sig.tail(1).head(10).orR),
      !sign & (is_normal | is_subnormal & sig.tail(1).orR))
  )
  is_snan := Mux1H(
    Seq(is_vfr,
      is_fp2fp && is_widen),
    Seq(!sig.tail(1).head(1).asBool && is_nan,
       fp_in.exp.tail(3).andR && fp_in.sig.head(10).orR && !fp_in.sig.head(1).asBool
    )
  )

  is_neg2_bplus1_b := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(sign & (exp.tail(3) === 30.U),
      sign & (exp === 254.U))
  )
  is_neg2_b_bminus1 := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(sign & (exp.tail(3) === 29.U),
      sign & (exp === 253.U))
  )
  is_neg2_negbminus1_negzero := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).head(10).orR,
      sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).orR)
  )
  is_pos2_poszero_negbminus1 := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).head(10).orR,
      !sign & (sig.tail(1).head(2) === "b00".U) & is_subnormal & sig.tail(1).orR)
  )
  is_pos2_bminus1_b := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!sign & (exp.tail(3) === 29.U),
      !sign & (exp === 253.U))
  )
  is_pos2_b_bplus1 := Mux1H(
    Seq(is_vfr && in_is_fp16,
      is_vfr && in_is_fp32),
    Seq(!sign & (exp.tail(3) === 30.U),
      !sign & (exp === 254.U))
  )

  val zero_minus_lzc_f16 = Mux(is_vfr && in_is_fp16, 0.U - CLZ(sig.tail(1).head(10)), 0.U)

  val zero_minus_lzc_f32 = Mux(is_vfr && in_is_fp32, 0.U - CLZ(sig.tail(1)), 0.U)

  val exp_normalized_5 = Wire(UInt(5.W))
  val exp_normalized_8 = Wire(UInt(8.W))
  val exp_normalized_5_reg0 = RegEnable(exp_normalized_5, fire)
  val exp_normalized_8_reg0 = RegEnable(exp_normalized_8, fire)
  exp_normalized_5 := Mux1H(
    Seq(in_is_fp16 && (is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal)),
       in_is_fp16 && (is_vfrsqrt7 && is_poszero_posinf && !is_normal || (is_vfrec7 && !is_normal))),
    Seq(exp(4,0),
      Cat(Fill(f16.expWidth - zero_minus_lzc_f16.getWidth, zero_minus_lzc_f16.head(1)), zero_minus_lzc_f16))
  )
  exp_normalized_8 := Mux1H(
    Seq(in_is_fp32 && is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal),
      in_is_fp32 && is_vfrsqrt7 && is_poszero_posinf && !is_normal || (is_vfrec7 && !is_normal)),
    Seq(exp,
      Cat(Fill(f32.expWidth - zero_minus_lzc_f32.getWidth, zero_minus_lzc_f32.head(1)), zero_minus_lzc_f32))
  )

  val sig_normalized_11 = Wire(UInt(11.W))
  val sig_normalized_11_reg0 = RegEnable(sig_normalized_11, fire)
  val sig_normalized_24 = Wire(UInt(24.W))
  val sig_normalized_24_reg0 = RegEnable(sig_normalized_24, fire)

  sig_normalized_11 := Mux1H(
    Seq(in_is_fp16 && (is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal)),
      in_is_fp16 && (is_vfrsqrt7 && is_poszero_posinf && !is_normal || (is_vfrec7 && !is_normal))),
    Seq(Cat(0.U, sig.tail(1).head(10)),
      (sig.tail(1).head(10) << 1.U).asUInt)
  )
  sig_normalized_24 := Mux1H(
    Seq(in_is_fp32 && (is_vfrsqrt7 && is_poszero_posinf && is_normal || (is_vfrec7 && is_normal)),
      in_is_fp32 && (is_vfrsqrt7 && is_poszero_posinf && !is_normal || (is_vfrec7 && !is_normal))),
    Seq(Cat(0.U, sig.tail(1)),
      (sig.tail(1) << 1.U).asUInt)
  )
  val clz_fp16 = Wire(UInt(4.W))
  val clz_fp32 = Wire(UInt(8.W))
  val clz_fp16_reg0 = RegEnable(clz_fp16, fire)
  val clz_fp32_reg0 = RegEnable(clz_fp32, fire)

  clz_fp16 := Mux(in_is_fp16 && is_vfr, CLZ(sig_normalized_11), 0.U)
  clz_fp32 := Mux(in_is_fp32 && is_vfr, CLZ(sig_normalized_24), 0.U)

  val out_exp_normalized_fp16 = Mux(is_vfrec7 && in_is_fp16, 29.U - exp_normalized_5, 0.U)

  val out_exp_normalized_fp32 = Mux(is_vfrec7 && in_is_fp32, 253.U - exp_normalized_8, 0.U)

  val out_exp_zero_negone = Wire(Bool())
  val out_exp_zero_negone_reg0 = RegEnable(out_exp_zero_negone, fire)
  out_exp_zero_negone := Mux1H(
    Seq(is_vfrec7 && in_is_fp16,
      is_vfrec7 && in_is_fp32),
    Seq(!out_exp_normalized_fp16.orR || out_exp_normalized_fp16.andR,
      !out_exp_normalized_fp32.orR || out_exp_normalized_fp32.andR)
  )

  val out_exp_fp16 = Wire(UInt(5.W))
  val out_exp_fp16_reg0 = RegEnable(out_exp_fp16, 0.U(5.W), fire)
  out_exp_fp16 := Mux1H(
    Seq(in_is_fp16 && is_vfrsqrt7 && is_normal,
      in_is_fp16 && is_vfrsqrt7 && !is_normal,
      in_is_fp16 && is_vfrec7 && out_exp_zero_negone,
      in_is_fp16 && is_vfrec7 && !out_exp_zero_negone),
    Seq((44.U - exp.tail(3)) >> 1,
      (44.U + CLZ(sig.tail(1).head(10))) >> 1,
      0.U,
      out_exp_normalized_fp16)
  )
  val out_exp_fp32 = Wire(UInt(8.W))
  val out_exp_fp32_reg0 = RegEnable(out_exp_fp32, 0.U(8.W), fire)
  out_exp_fp32 := Mux1H(
    Seq(in_is_fp32 && is_vfrsqrt7 && is_normal,
      in_is_fp32 && is_vfrsqrt7 && !is_normal,
      in_is_fp32 && is_vfrec7 && out_exp_zero_negone,
      in_is_fp32 && is_vfrec7 && !out_exp_zero_negone
    ),
    Seq((380.U - exp) >> 1,
      (380.U + CLZ(sig.tail(1))) >> 1,
      0.U,
      out_exp_normalized_fp32)
  )

  val out_sign = Wire(Bool())
  val out_sign_reg0 = RegEnable(out_sign, fire)
  out_sign := is_vfrsqrt7 && is_poszero_posinf && sign


  // fp2fp narrow
  val exp_delta = VectorFloat.expBias(f32.expWidth) - VectorFloat.expBias(f16.expWidth)

  val down_exp_s = fp_in.exp.zext - exp_delta.S
  val down_exp = Wire(UInt(9.W))
  val down_exp_reg0 = RegEnable(down_exp, fire)
  down_exp := down_exp_s.asUInt

  // normal
  nor_in := fp_in.sig.head(f16.precision - 1)
  nor_roundBit := fp_in.sig.tail(f16.precision - 1).head(1).asBool
  nor_stickyBit := fp_in.sig.tail(f16.precision).orR
  nor_signBit := fp_in.sign

  // subnormal
  val shamt = (exp_delta + 1).U(f32.expWidth.W) - fp_in.exp
  val (subnor_sig, shift_sticky) = ShiftRightJam(Cat(fp_in.decode.expNotZero, fp_in.sig.head(f16.precision)), shamt)
  subnor_in := subnor_sig.tail(1).head(f16.precision - 1)
  subnor_roundBit := subnor_sig(0)
  subnor_stickyBit := shift_sticky | nor_stickyBit
  subnor_signBit := fp_in.sign

  val may_be_subnor = Wire(Bool())
  val may_be_subnor_reg0 = RegEnable(may_be_subnor, fire)
  may_be_subnor := down_exp_s < 1.S

  val rmin = is_fp2fp && is_narrow &&  (io.rm === RTZ || (io.rm === RDN & !fp_in.sign) || (io.rm === RUP && fp_in.sign))

  val nor_of_exp = Wire(UInt(5.W))
  val nor_of_exp_reg0 = RegEnable(nor_of_exp, fire)
  nor_of_exp := Mux(rmin | io.opType(0).asBool, // ROD
    VectorFloat.maxNormExp(f16.expWidth).U(f16.expWidth.W),
    (VectorFloat.maxNormExp(f16.expWidth) + 1).U(f16.expWidth.W))

  val nor_of_sig = Wire(UInt(10.W))
  val nor_of_sig_reg0 = RegEnable(nor_of_sig, fire)
  nor_of_sig := Mux(rmin | io.opType(0).asBool, ~0.U((f16.precision - 1).W), 0.U((f16.precision - 1).W))

  val special_case = Wire(Bool())
  val special_case_reg0 = RegEnable(special_case, fire)
  special_case := is_fp2fp && is_narrow && fp_in.decode.expIsOnes

  val fp2fp_iv = Wire(Bool())
  val fp2fp_iv_reg0 = RegEnable(fp2fp_iv, fire)
  val fp2fp_dz = Wire(Bool())
  val fp2fp_dz_reg0 = RegEnable(fp2fp_dz, fire)
  fp2fp_iv := is_fp2fp && is_narrow && fp_in.decode.isSNaN
  fp2fp_dz := is_fp2fp && is_narrow && false.B

  val in_isnan_neg = Wire(Bool())
  val in_isnan_neg_reg0 = RegEnable(in_isnan_neg, fire)
  val in_sigNotZero = Wire(Bool())
  val in_sigNotZero_reg0 = RegEnable(in_sigNotZero, fire)
  in_isnan_neg := is_fp2fp && is_narrow && !fp_in.decode.isNaN && fp_in.sign
  in_sigNotZero := is_fp2fp && is_narrow && fp_in.decode.sigNotZero

  // fp2fp widen
  val nor_exp = Wire(UInt(8.W))
  val nor_exp_reg0 = RegEnable(nor_exp, fire)
  nor_exp := Mux(is_fp2fp && is_widen, exp_delta.U(f32.expWidth.W) + fp_in.exp, 0.U)

  val nor_sig = Wire(UInt(10.W))
  val nor_sig_reg0 = RegEnable(nor_sig, fire)
  nor_sig := Mux(is_fp2fp && is_widen, fp_in.sig.head(10), 0.U)

  val subnor_exp = Wire(UInt(8.W))
  val subnor_exp_reg0 = RegEnable(subnor_exp, fire)
  val subnor_sig_w = Wire(UInt(10.W))
  val subnor_sig_w_reg0 = RegEnable(subnor_sig_w, fire)
  val subnor_shamt = CLZ(fp_in.sig.head(10))
  subnor_exp := exp_delta.U(f32.expWidth.W) - subnor_shamt
  subnor_sig_w := Cat((fp_in.sig.head(10) << subnor_shamt)(f16.precision - 3, 0), 0.U(1.W))


  val isNaN = Wire(Bool())
  val expIsOnes = Wire(Bool())
  val expIsZero = Wire(Bool())
  val isZero = Wire(Bool())
  val isSubnormal = Wire(Bool())
  val sigNotZero = Wire(Bool())
  val isNaN_reg0 = RegEnable(isNaN, fire)
  val expIsOnes_reg0 = RegEnable(expIsOnes, fire)
  val expIsZero_reg0 = RegEnable(expIsZero, fire)
  val isZero_reg0 = RegEnable(isZero, fire)
  val isSubnormal_reg0 = RegEnable(isSubnormal, fire)
  val sigNotZero_reg0 = RegEnable(sigNotZero, fire)

  isNaN := is_fp2fp && is_widen && fp_in.exp.tail(3).andR && fp_in.sig.head(10).orR
  expIsOnes := is_fp2fp && is_widen && fp_in.exp.tail(3).andR
  expIsZero := is_fp2fp && is_widen && !fp_in.exp.tail(3).orR
  isZero := is_fp2fp && is_widen && !fp_in.exp.tail(3).orR && !fp_in.sig.head(10).orR
  isSubnormal := is_fp2fp && is_widen && !fp_in.exp.tail(3).orR && fp_in.sig.head(10).orR
  sigNotZero := is_fp2fp && is_widen && fp_in.sig.head(10).orR

  // cycle1
  val rounder = Module(new RoundingUnit(f32.precision))
  rounder.io.in := round_in_reg0
  rounder.io.roundIn := round_roundIn_reg0
  rounder.io.stickyIn := round_stickyIn_reg0
  rounder.io.signIn := round_signIn_reg0
  rounder.io.rm := rm_reg0

  val out_reg0 = Mux(is_fp2int_reg0 || is_int2fp_reg0, Mux(rounder.io.r_up, rounder.io.in + 1.U, rounder.io.in), 0.U)

  val cout_reg0 = rounder.io.r_up && Mux1H(
    Seq(fp32toint32_reg0 || fp16toint32_reg0,
      fp32toint16_reg0 || fp16toint16_reg0,
      fp16toint8_reg0,
      int16tofp32_reg0,
      int32tofp32_reg0,
      int8tofp16_reg0,
      int16tofp16_reg0 || int32tofp16_reg0
    ),
    Seq(rounder.io.in.andR,
      rounder.io.in.tail(9).andR,
      rounder.io.in.tail(17).andR,
      rounder.io.in.tail(8).andR,
      rounder.io.in.tail(1).andR,
      rounder.io.in.tail(16).andR,
      rounder.io.in.tail(14).andR
    )
  )

  val exp_reg0 = Mux1H(
    Seq(is_int2fp_reg0 && in_orR_reg0,
      is_int2fp_reg0 && !in_orR_reg0),
    Seq(exp_raw_reg0 + cout_reg0,
      0.U)
  )
  val sig_reg0 = Mux1H(
    Seq(fp32toint32_reg0,
      fp32toint16_reg0,
      fp16toint16_reg0 || fp16toint32_reg0,
      fp16toint8_reg0,
      is_int2fp_reg0),
    Seq(Cat(0.U((32 - f32.precision - 1).W), cout_reg0, out_reg0),
      Cat(0.U(8.W), out_reg0),
      Cat(0.U((32 - f16.precision - 1).W), cout_reg0, out_reg0(10, 0)),
      Cat(0.U(21.W), out_reg0(10, 0)),
      out_reg0)
  )

  val rpath_ix_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && !fp16toint8_reg0,
      is_fp2int_reg0 && fp16toint8_reg0),
    Seq(rounder.io.inexact,
      rounder.io.inexact || rpath_sig_shifted_reg0(7,0).orR)
  )
  val rpath_iv_reg0 = is_fp2int_reg0 && !is_signed_int_reg0 && in_reg0.head(1).asBool && sig_reg0.orR

  val rpath_pos_of_reg0 = is_fp2int_reg0 && !in_reg0.head(1).asBool &&
    Mux(is_signed_int_reg0,
      Mux1H(
        Seq(fp32toint16_reg0,
          fp16toint8_reg0),
        Seq((in_reg0.tail(1).head(8) === 142.U) || (in_reg0.tail(1).head(8) === 141.U) && cout_reg0,
          (in_reg0.tail(1).head(8) === 22.U) || (in_reg0.tail(1).head(8) === 21.U) && cout_reg0)
      ),
      Mux1H(
        Seq(fp32toint16_reg0,
          fp16toint8_reg0),
        Seq((in_reg0.tail(1).head(8) === 142.U) && cout_reg0,
          (in_reg0.tail(1).head(8) === 22.U) && cout_reg0)
      )
    )
  val rpath_neg_of_reg0 = is_fp2int_reg0 && in_reg0.head(1).asBool && Mux1H(
    Seq(fp32toint16_reg0,
      fp16toint8_reg0),
    Seq(in_reg0.tail(1).head(8) === 142.U && (rounder.io.in.tail(9).orR || rounder.io.r_up),
      in_reg0.tail(1).head(8) === 22.U && (rounder.io.in.tail(17).orR || rounder.io.r_up))
  )
  val rpath_of_reg0 = Mux1H(
    Seq(fp32toint16_reg0 || fp16toint8_reg0,
      fp32toint32_reg0 || fp16toint16_reg0 || fp16toint32_reg0),
    Seq(rpath_neg_of_reg0 || rpath_pos_of_reg0,
      cout_reg0)
  )



  val vfrsqrt7Table = Module(new Rsqrt7Table)
  val vfrec7Table = Module(new Rec7Table)

  val sig_in7 = Mux1H(
    Seq(in_is_fp16_reg0 && is_vfrsqrt7_reg0,
      in_is_fp16_reg0 && is_vfrec7_reg0,
      in_is_fp32_reg0 && is_vfrsqrt7_reg0,
      in_is_fp32_reg0 && is_vfrec7_reg0),
    Seq(Cat(exp_normalized_5_reg0(0), (sig_normalized_11_reg0 << Mux(is_normal_reg0, 0.U, clz_fp16_reg0))(9, 4)),
      (sig_normalized_11_reg0 << Mux(is_normal_reg0, 0.U, clz_fp16_reg0))(9, 3),
      Cat(exp_normalized_8_reg0(0), (sig_normalized_24_reg0 << Mux(is_normal_reg0, 0.U, clz_fp32_reg0))(22, 17)),
      (sig_normalized_24_reg0 << Mux(is_normal_reg0, 0.U, clz_fp32_reg0))(22, 16))
  )

  vfrsqrt7Table.src := sig_in7
  vfrec7Table.src := sig_in7

  val sig_out7_reg0 = Wire(UInt(7.W))
  sig_out7_reg0 := Mux1H(
    Seq(is_vfrsqrt7_reg0,
      is_vfrec7_reg0),
    Seq(vfrsqrt7Table.out,
      vfrec7Table.out)
  )

  val out_sig_10_reg0 = Mux1H(
    Seq(in_is_fp16_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0),
      in_is_fp16_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      in_is_fp16_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && !(is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0 || is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      in_is_fp16_reg0 && is_vfrec7_reg0 && !out_exp_zero_negone_reg0),
    Seq(Cat(0.U, 1.U, sig_out7_reg0, 0.U),
      Cat(1.U, sig_out7_reg0, 0.U(2.W)),
      Cat(1.U, sig_out7_reg0, 0.U(2.W)) >> 1,
      Cat(sig_out7_reg0, 0.U(3.W)))
  )
  val out_sig_23_reg0 = Mux1H(
    Seq(in_is_fp32_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0),
      in_is_fp32_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && (is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      in_is_fp32_reg0 && is_vfrec7_reg0 && out_exp_zero_negone_reg0 && !(is_neg2_bplus1_b_reg0 || is_pos2_b_bplus1_reg0 || is_neg2_b_bminus1_reg0 || is_pos2_bminus1_b_reg0),
      in_is_fp32_reg0 && is_vfrec7_reg0 && !out_exp_zero_negone_reg0),
    Seq(Cat(0.U, 1.U, sig_out7_reg0, 0.U(14.W)),
      Cat(1.U, sig_out7_reg0, 0.U(15.W)),
      Cat(1.U, sig_out7_reg0, 0.U(15.W)) >> 1,
      Cat(sig_out7_reg0, 0.U(16.W)))
  )

  val fp_result_fp16 = Wire(UInt(16.W))
  fp_result_fp16 := Mux1H(
    Seq(in_is_fp16_reg0 && is_vfrsqrt7_reg0,
      in_is_fp16_reg0 && is_vfrec7_reg0),
    Seq(Cat(out_sign_reg0, out_exp_fp16_reg0, sig_out7_reg0, 0.U(3.W)),
      Cat(sign_reg0, out_exp_fp16_reg0, out_sig_10_reg0))
  )
  val fp_result_fp32 = Wire(UInt(32.W))
  fp_result_fp32 := Mux1H(
    Seq(in_is_fp32_reg0 && is_vfrsqrt7_reg0,
      in_is_fp32_reg0 && is_vfrec7_reg0),
    Seq(Cat(out_sign_reg0, out_exp_fp32_reg0, sig_out7_reg0, 0.U(16.W)),
      Cat(sign_reg0, out_exp_fp32_reg0, out_sig_23_reg0))
  )

  val result_nan_fp16 = Wire(UInt(16.W))
  val result_nan_fp32 = Wire(UInt(32.W))

  result_nan_fp16 := Cat(0.U(1.W), Fill(6, 1.U), 0.U(9.W))
  result_nan_fp32 := Cat(0.U, Fill(9, 1.U), 0.U(22.W))

  val result_inf_fp16 = Wire(UInt(15.W))
  val result_inf_fp32 = Wire(UInt(31.W))

  result_inf_fp16 := Cat(Fill(5, 1.U), 0.U(10.W))
  result_inf_fp32 := Cat(Fill(8, 1.U), Fill(23, 0.U))

  val result_greatest_fin_fp16 = Wire(UInt(15.W))
  val result_greatest_fin_fp32 = Wire(UInt(31.W))
  result_greatest_fin_fp16 := Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U))
  result_greatest_fin_fp32 := Cat(Fill(7, 1.U), 0.U, Fill(23, 1.U))


  val of_reg0 = Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0 && out_is_fp32_reg0,
      is_int2fp_reg0 && int32tofp16_reg0,
      is_int2fp_reg0 && !out_is_fp32_reg0 && !int32tofp16_reg0),
    Seq(exp_of_reg0 || sel_lpath_reg0 && lpath_of_reg0 || !sel_lpath_reg0 && rpath_of_reg0,
      exp_reg0 === 255.U,
      exp_of_reg0 || exp_reg0 === 31.U,
      exp_reg0 === 31.U)
  )
  val iv_reg0 = is_fp2int_reg0 && (of_reg0 || sel_lpath_reg0 && lpath_iv_reg0 || !sel_lpath_reg0 && rpath_iv_reg0)

  val ix_reg0 = Mux1H(
    Seq(is_fp2int_reg0,
      is_int2fp_reg0 && (out_is_fp32_reg0 || int16tofp16_reg0 || int8tofp16_reg0),
      int32tofp16_reg0),
    Seq(!iv_reg0 && !sel_lpath_reg0 && rpath_ix_reg0,
      rounder.io.inexact,
      exp_of_reg0 || rounder.io.inexact)
  )

  val result_fp = Mux1H(
    Seq(is_int2fp_reg0),
    Seq(Cat(sign_reg0, Mux(RoundingUnit.is_rmin(rm_reg0, sign_reg0), Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U)), Cat(Fill(5, 1.U), Fill(10, 0.U)))))
  )

  val int_abs_reg0 = Mux1H(
    Seq(is_fp2int_reg0 && sel_lpath_reg0,
      is_fp2int_reg0 && !sel_lpath_reg0),
    Seq(lpath_sig_shifted_reg0,
      sig_reg0)
  )
  val int_reg0 = Mux1H(
    Seq(fp32toint16_reg0 || fp16toint16_reg0,
      fp32toint32_reg0 || fp16toint32_reg0,
      fp16toint8_reg0),
    Seq(Mux(in_reg0.head(1).asBool && is_signed_int_reg0, -int_abs_reg0.tail(16), int_abs_reg0.tail(16)),
      Mux(in_reg0.head(1).asBool && is_signed_int_reg0, -int_abs_reg0, int_abs_reg0),
      Mux(in_reg0.head(1).asBool && is_signed_int_reg0, -int_abs_reg0.tail(24), int_abs_reg0.tail(24)))
  )


  // fp2fp
  val nor_rounder = Module(new RoundingUnit(f16.precision - 1))
  nor_rounder.io.in := nor_in_reg0
  nor_rounder.io.roundIn := nor_roundBit_reg0
  nor_rounder.io.stickyIn := nor_stickyBit_reg0
  nor_rounder.io.signIn := nor_signBit_reg0
  nor_rounder.io.rm := rm_reg0

  val fp2fp_n_out_reg0 = Mux(nor_rounder.io.r_up, nor_rounder.io.in + 1.U, nor_rounder.io.in)
  val fp2fp_n_cout_reg0 = nor_rounder.io.r_up && nor_rounder.io.in.andR

  val nor_sig_rounded_reg0 = fp2fp_n_out_reg0
  val nor_exp_rounded_reg0 = Mux(fp2fp_n_cout_reg0, down_exp_reg0.asSInt + 1.S, down_exp_reg0.asSInt)
  val nor_of_reg0 =
    Mux(fp2fp_n_cout_reg0,
      down_exp_reg0.asSInt > (VectorFloat.maxNormExp(f16.expWidth) - 1).S,
      down_exp_reg0.asSInt > VectorFloat.maxNormExp(f16.expWidth).S).asBool

  val exp_uf_reg0 = Mux(fp2fp_n_cout_reg0, down_exp_reg0.asSInt < 0.S, down_exp_reg0.asSInt < 1.S).asBool
  val nor_ix_reg0 = nor_of_reg0 | nor_rounder.io.inexact

  val subnor_rounder = Module(new RoundingUnit(f16.precision - 1))
  subnor_rounder.io.in := subnor_in_reg0
  subnor_rounder.io.roundIn := subnor_roundBit_reg0
  subnor_rounder.io.stickyIn := subnor_stickyBit_reg0
  subnor_rounder.io.signIn := subnor_signBit_reg0
  subnor_rounder.io.rm := rm_reg0

  val fp2fp_w_out_reg0 = Mux(subnor_rounder.io.r_up, subnor_rounder.io.in + 1.U, subnor_rounder.io.in)
  val fp2fp_w_cout_reg0 = subnor_rounder.io.r_up && subnor_rounder.io.in.andR

  val subnor_sig_rounded_reg0 = fp2fp_w_out_reg0
  val subnor_exp_rounded_reg0 = Mux(fp2fp_w_cout_reg0, 1.U, 0.U)
  val subnor_ix_reg0 = subnor_rounder.io.inexact

  val common_exp = Mux1H(
    Seq(
      !may_be_subnor_reg0 & nor_of_reg0,
      !may_be_subnor_reg0 & !nor_of_reg0,
      may_be_subnor_reg0
    ),
    Seq(
      nor_of_exp_reg0,
      nor_exp_rounded_reg0(f16.expWidth - 1, 0),
      subnor_exp_rounded_reg0
    )
  )

  val common_sig = Mux1H(
    Seq(
      !may_be_subnor_reg0 & nor_of_reg0,
      !may_be_subnor_reg0 & !nor_of_reg0,
      may_be_subnor_reg0
    ),
    Seq(
      nor_of_sig_reg0,
      nor_sig_rounded_reg0,
      subnor_sig_rounded_reg0
    )
  )

  val fp2fp_of_reg0 = is_fp2fp_reg0 && is_narrow_reg0 && !special_case_reg0 && nor_of_reg0
  val fp2fp_uf_reg0 = is_fp2fp_reg0 && is_narrow_reg0 && !special_case_reg0 && may_be_subnor_reg0 && subnor_ix_reg0 && exp_uf_reg0
  val fp2fp_ix_reg0 = is_fp2fp_reg0 && is_narrow_reg0 && !special_case_reg0 && (!may_be_subnor_reg0 && nor_ix_reg0 || may_be_subnor_reg0 && subnor_ix_reg0)


  val result1H = VecInit(Seq(is_fp2int_reg0,

    int32tofp32_reg0,
    int16tofp32_reg0,
    int32tofp16_reg0 && of_reg0,
    int8tofp16_reg0,
    int32tofp16_reg0 && !of_reg0 || int16tofp16_reg0,

    in_is_fp16_reg0 && (is_vfrsqrt7_reg0 && (is_nan_reg0 || is_neginf_negzero_reg0) || is_vfrec7_reg0 && is_nan_reg0),
    in_is_fp16_reg0 && is_vfrsqrt7_reg0 && is_inf_reg0,
    in_is_fp16_reg0 && is_vfrsqrt7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
    in_is_fp16_reg0 && (is_vfrsqrt7_reg0 && !(is_nan_reg0 || is_neginf_negzero_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0) ||
      is_vfrec7_reg0 && !(is_nan_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0 || is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0)),
    in_is_fp16_reg0 && is_vfrec7_reg0 && is_inf_reg0,
    in_is_fp16_reg0 && is_vfrec7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
    in_is_fp16_reg0 && is_vfrec7_reg0 && is_neg2_negbminus1_negzero_reg0,
    in_is_fp16_reg0 && is_vfrec7_reg0 && is_pos2_poszero_negbminus1_reg0,

    in_is_fp32_reg0 && (is_vfrsqrt7_reg0 && (is_nan_reg0 || is_neginf_negzero_reg0) || is_vfrec7_reg0 && is_nan_reg0),
    in_is_fp32_reg0 && is_vfrsqrt7_reg0 && is_inf_reg0,
    in_is_fp32_reg0 && is_vfrsqrt7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
    in_is_fp32_reg0 && (is_vfrsqrt7_reg0 && !(is_nan_reg0 || is_neginf_negzero_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0) ||
      is_vfrec7_reg0 && !(is_nan_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0 || is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0)),
    in_is_fp32_reg0 && is_vfrec7_reg0 && is_inf_reg0,
    in_is_fp32_reg0 && is_vfrec7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
    in_is_fp32_reg0 && is_vfrec7_reg0 && is_neg2_negbminus1_negzero_reg0,
    in_is_fp32_reg0 && is_vfrec7_reg0 && is_pos2_poszero_negbminus1_reg0,

    is_fp2fp_reg0 && is_narrow_reg0,
    is_fp2fp_reg0 && is_widen_reg0
  )).asUInt
  dontTouch(result1H)

  result0 := Mux1H(
    result1H,
    Seq(Mux(iv_reg0, max_min_int_reg0, int_reg0),

      Cat(sign_reg0, exp_reg0, sig_reg0(22,0)),
      Cat(sign_reg0, exp_reg0, sig_reg0(15,0), 0.U(7.W)),
      result_fp,
      Cat(sign_reg0, exp_reg0(4, 0), sig_reg0(7, 0), 0.U(2.W)),
      Cat(sign_reg0, exp_reg0(4, 0), sig_reg0(9, 0)),

      result_nan_fp16,
      Mux(is_neginf_reg0, result_nan_fp16, 0.U),
      Mux(is_negzero_reg0, Cat(1.U, result_inf_fp16), Cat(0.U, result_inf_fp16)),
      fp_result_fp16,
      Mux(is_neginf_reg0, Cat(1.U, 0.U(15.W)), 0.U),
      Mux(is_negzero_reg0, Cat(Fill(6, 1.U), 0.U(10.W)), Cat(0.U(1.W), Fill(5, 1.U), 0.U(10.W))),
      Mux(rm_reg0 === RUP || rm_reg0 === RTZ, Cat(1.U, result_greatest_fin_fp16), Cat(1.U, result_inf_fp16)),
      Mux(rm_reg0 === RDN || rm_reg0 === RTZ, Cat(0.U, result_greatest_fin_fp16), Cat(0.U, result_inf_fp16)),

      result_nan_fp32,
      Mux(is_neginf_reg0, result_nan_fp32, 0.U),
      Mux(is_negzero_reg0, Cat(1.U, result_inf_fp32), Cat(0.U, result_inf_fp32)),
      fp_result_fp32,
      Mux(is_neginf_reg0, Cat(1.U, 0.U(31.W)), 0.U),
      Mux(is_negzero_reg0, Cat(Fill(9, 1.U), 0.U(23.W)), Cat(0.U(1.W), Fill(8, 1.U), 0.U(23.W))),
      Mux(rm_reg0 === RUP || rm_reg0 === RTZ, Cat(1.U, result_greatest_fin_fp32), Cat(1.U, result_inf_fp32)),
      Mux(rm_reg0 === RDN || rm_reg0 === RTZ, Cat(0.U, result_greatest_fin_fp32), Cat(0.U, result_inf_fp32)),

      Cat(
        in_isnan_neg_reg0,
        Mux1H(
          Seq(special_case_reg0, !special_case_reg0),
          Seq(~0.U(f16.expWidth.W), common_exp)
        ),
        Mux1H(
          Seq(special_case_reg0, !special_case_reg0),
          Seq(
            Cat(in_sigNotZero_reg0, 0.U((f16.precision - 2).W)),
            common_sig
          ))),
      Cat(
        !isNaN_reg0 & sign_reg0,
        Mux1H(
          Seq(
            expIsOnes_reg0,
            isZero_reg0,
            isSubnormal_reg0,
            !expIsOnes_reg0 & !expIsZero_reg0
          ),
          Seq(
            ~0.U(f32.expWidth.W),
            0.U(f32.expWidth.W),
            subnor_exp_reg0,
            nor_exp_reg0
          )
        ),
        Mux1H(
          Seq(
            expIsOnes_reg0,
            expIsZero_reg0,
            !expIsOnes_reg0 & !expIsZero_reg0
          ),
          Seq(
            Cat(sigNotZero_reg0, 0.U((f32.precision - 2).W)),
            Cat(subnor_sig_w_reg0, 0.U((f32.precision - f16.precision).W)),
            Cat(nor_sig_reg0, 0.U((f32.precision - f16.precision).W))
          )
        )
      )
    )
  )
  val fflags1H = VecInit(Seq(is_fp2int_reg0,
      is_int2fp_reg0,

      is_vfrsqrt7_reg0 && (is_nan_reg0 || is_neginf_negzero_reg0),
      is_vfrsqrt7_reg0 && is_inf_reg0,
      is_vfrsqrt7_reg0 && (is_negzero_reg0 || is_poszero_reg0) || is_vfrec7_reg0 && (is_negzero_reg0 || is_poszero_reg0),
      is_vfrsqrt7_reg0 && !(is_nan_reg0 || is_neginf_negzero_reg0 || is_inf_reg0 || is_negzero_reg0 || is_poszero_reg0) ||
        is_vfrec7_reg0 && (!(is_nan_reg0 || is_negzero_reg0 || is_poszero_reg0 || is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0) || is_inf_reg0),
      is_vfrec7_reg0 && is_nan_reg0,
      is_vfrec7_reg0 && (is_neg2_negbminus1_negzero_reg0 || is_pos2_poszero_negbminus1_reg0),

      is_fp2fp_reg0 && is_narrow_reg0,
      is_fp2fp_reg0 && is_widen_reg0)
  ).asUInt
  dontTouch(fflags1H)

  fflags0 := Mux1H(
    fflags1H,
    Seq(Cat(iv_reg0, false.B, false.B, false.B, ix_reg0),
      Cat(false.B, false.B, of_reg0, false.B, ix_reg0),

      Mux(is_snan_reg0 || is_neginf_negzero_reg0, "b10000".U, "b00000".U),
      Mux(is_neginf_reg0, "b10000".U, "b00000".U),
      "b01000".U,
      0.U,
      Mux(is_snan_reg0, "b10000".U, "b00000".U),
      "b00101".U,

      Cat(fp2fp_iv_reg0, fp2fp_dz_reg0, fp2fp_of_reg0, fp2fp_uf_reg0, fp2fp_ix_reg0),
      Cat(is_snan_reg0, 0.U(4.W)))
  )

  // cycle2
  result := result0_reg1
  fflags := fflags0_reg1

  io.result := result
  io.fflags := fflags

}