package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.LookupTree
import yunsuan.vector.VectorConvert.util.{CLZ, ShiftRightJam, VFRSqrtTable, VFRecTable, RoundingUnit}
import yunsuan.vector.VectorConvert.RoundingModle._


class CVT16(width: Int = 16) extends CVT(width){
    val expWidth = 5
    val precision = 11

    val is_single = io.opType.tail(3).head(2) === "b00".U
    val is_widen  = io.opType.tail(3).head(2) === "b01".U
    val is_narrow = io.opType.tail(3).head(2) === "b10".U

    val is_vfr = io.opType.head(3) === "b111".U
    val is_fp2int = io.opType.head(2) === "b10".U && !is_widen
    val is_int2fp = io.opType.head(2) === "b01".U && !is_narrow

    val is_signed_int = io.opType(0)

    val result = Wire(UInt(16.W))
    val NV, DZ, OF, UF, NX = WireInit(false.B)
    val fflags = WireInit(Cat(NV, DZ, OF, UF, NX))

    val rm = io.rm

    /**
   * fp16 -> ui16  vfcvt.xu.f.v  vfcvt.rtz.xu.f.v
   *      -> i16   vfcvt.x.f.v   vfcvt.rtz.x.f.v
   *      -> ui8   vfncvt.xu.f.w vfncvt.rtz.xu.f.w
   *      -> i8    vfncvt.x.f.w  vfncvt.rtz.x.f.w
   */
   when(is_fp2int) {
    val in = VectorFloat.fromUInt(io.src, expWidth, precision)
    val raw_in = RawVectorFloat.fromVFP(in, Some(in.decode.expNotZero))
    val raw_in_reg0 = RegNext(raw_in)
    
    // single max exp = bias +& 15.U, narrow max exp = bias +& 7.U
    val max_int_exp = Mux(is_single, 30.U, 22.U)
    val exp_of = raw_in.exp > max_int_exp

    // left
    // only f16->i16(ui16) can left shift, shamt is raw_in.exp - (bias + precision - 1)
    val lpath_shamt = Mux(is_single, raw_in.exp - 25.U, 0.U)
    // max shamt width = (15 - (precision - 1)).U.getWidth
    val lpath_sig_shifted = Mux(is_single, (raw_in.sig << lpath_shamt(4, 0))(15, 0), 0.U)
    // fp->ui16, if fp is negative, invalid
    val lpath_iv = Mux(is_single, (!is_signed_int & raw_in.sign).asUInt, 0.U).asBool
    // f16->i16, may overflow
    val lpath_may_of = Mux(is_single, (is_signed_int & (raw_in.exp === max_int_exp)).asUInt, 0.U).asBool
    val lpath_pos_of = Mux(is_single, (lpath_may_of & !raw_in.sign).asUInt, 0.U).asBool
    val lpath_neg_of = Mux(is_single, (lpath_may_of & raw_in.sign & raw_in.sig.tail(1).orR).asUInt, 0.U).asBool
    val lpath_of = lpath_pos_of | lpath_neg_of

    // right
    // f16->i8(ui8) always right shift
    val rpath_shamt = Mux(is_single, 25.U - raw_in.exp, 22.U - raw_in.exp);
    val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(Cat(raw_in.sig, 0.U), rpath_shamt)
    val rpath_rounder = Module(new RoundingUnit(precision))
    rpath_rounder.io.in := RegNext(Mux(is_single, rpath_sig_shifted.head(precision).asUInt, rpath_sig_shifted.head(8).asUInt))
    rpath_rounder.io.roundIn := RegNext(Mux(is_single, rpath_sig_shifted.tail(precision).asUInt, rpath_sig_shifted.tail(8).head(1).asUInt).asBool)
    rpath_rounder.io.stickyIn := RegNext(rpath_sticky | Mux(is_narrow, rpath_sig_shifted.tail(9).orR.asUInt, 0.U).asBool)
    rpath_rounder.io.signIn := RegNext(raw_in.sign)
    rpath_rounder.io.rm := RegNext(rm)

    val fp2int_rpath_out_reg0 = Mux(rpath_rounder.io.r_up, rpath_rounder.io.in + 1.U, rpath_rounder.io.in)
    val fp2int_rpath_cout_reg0 = rpath_rounder.io.r_up && Mux(RegNext(is_narrow), rpath_rounder.io.in.tail(4).andR.asUInt, rpath_rounder.io.in.andR.asUInt).asBool

    val rpath_sig_reg0 = Mux(RegNext(is_single), Cat(0.U(4.W), fp2int_rpath_cout_reg0, fp2int_rpath_out_reg0), Cat(0.U(5.W), fp2int_rpath_out_reg0))
    val rpath_ix_reg0 = rpath_rounder.io.inexact | RegNext(Mux(is_narrow, rpath_sig_shifted.tail(8).orR.asUInt, 0.U).asBool)
    val rpath_iv_reg0 = RegNext(!is_signed_int) & raw_in_reg0.sign & rpath_sig_reg0.orR
    val rpath_pos_of_reg0 = !raw_in_reg0.sign &
      Mux(RegNext(is_signed_int),
        (raw_in_reg0.exp === 22.U | (raw_in_reg0.exp === 21.U & fp2int_rpath_cout_reg0)),
        (raw_in_reg0.exp === 22.U & fp2int_rpath_cout_reg0))
    val rpath_neg_of_reg0 = raw_in_reg0.sign & raw_in_reg0.exp === 22.U & (rpath_rounder.io.in.tail(4).orR | rpath_rounder.io.r_up)
    val rpath_of_reg0 = Mux(RegNext(is_narrow), (rpath_pos_of_reg0 | rpath_neg_of_reg0), fp2int_rpath_cout_reg0)

    // select result
    val sel_lpath = raw_in.exp >= 25.U
    val of = RegNext(exp_of | sel_lpath & lpath_of) | (RegNext(!sel_lpath) & rpath_of_reg0)
    val iv = of | RegNext(sel_lpath & lpath_iv) | (RegNext(!sel_lpath) & rpath_iv_reg0)
    val ix = !iv & RegNext(!sel_lpath) & rpath_ix_reg0

    val int_abs = Mux(RegNext(sel_lpath), RegNext(lpath_sig_shifted), rpath_sig_reg0)
    val int = Mux(RegNext(is_narrow),
      Mux(raw_in_reg0.sign & RegNext(is_signed_int), -int_abs.tail(8), int_abs.tail(8)),
      Mux(raw_in_reg0.sign & RegNext(is_signed_int), -int_abs, int_abs))

    val max_int = Mux(is_single, Cat(!is_signed_int, ~0.U(15.W)), Cat(!is_signed_int, ~0.U(7.W)))
    val min_int = Mux(is_single, Cat(is_signed_int,   0.U(15.W)), Cat(is_signed_int,   0.U(7.W)))

    result := RegNext(Mux(iv, RegNext(Mux(in.decode.isNaN | !raw_in.sign, max_int, min_int)), int))
    fflags := RegNext(Cat(iv, false.B, false.B, false.B, ix))
   }.elsewhen(is_int2fp) {
/**
   * ui16 -> f16  vfcvt.f.xu.v
   * i16  ->      vfcvt.f.x.v
   * ui8  ->      vfwcvt.f.xu.v
   * i8   ->      vfwcvt.f.x.v
   */
    val sign = is_signed_int && Mux(is_widen, io.src(7), io.src(15))
    val in_sext = Cat(Fill(8, io.src(7)), io.src(7,0))
    val in = Mux(is_signed_int && is_widen, in_sext, io.src)
    val in_abs = Mux(sign, (~in).asUInt + 1.U, in)

    val lzc = CLZ(in_abs)
    val in_shift = (in_abs << lzc)(14, 0)
    val exp_raw = 30.U - lzc

    val sig_raw = Mux(is_widen, in_shift.head(8).asUInt, in_shift.head(10).asUInt)
    val round_bit = Mux(is_widen, in_shift.tail(8).head(1).asUInt, in_shift.tail(10).head(1).asUInt).asBool
    val sticky_bit = Mux(is_widen, in_shift.tail(9).orR.asUInt, in_shift.tail(precision).orR.asUInt).asBool
    val rounder = Module(new RoundingUnit(10))
    rounder.io.in := RegNext(sig_raw)
    rounder.io.roundIn := RegNext(round_bit)
    rounder.io.stickyIn := RegNext(sticky_bit)
    rounder.io.signIn := RegNext(sign)
    rounder.io.rm := RegNext(rm)

    val int2fp_out_reg0 = Mux(rounder.io.r_up, rounder.io.in + 1.U, rounder.io.in)
    val int2fp_cout_reg0 = rounder.io.r_up && rounder.io.in.andR.asBool

    val exp_reg0 = Mux(in.orR,RegNext(exp_raw) + int2fp_cout_reg0, 0.U)
    val sig_reg0 = int2fp_out_reg0

    val of = exp_reg0 === 31.U
    val ix = rounder.io.inexact

    result := RegNext(Cat(Mux(RegNext(is_signed_int), RegNext(sign.asUInt), 0.U), exp_reg0, Mux(RegNext(is_widen), Cat(sig_reg0.tail(2).asUInt, 0.U(2.W)), sig_reg0)))
    fflags := RegNext(Cat(false.B, false.B, of, false.B, ix))
   }.otherwise {    // vfr
    val result_1 = Wire(UInt(16.W))
    val fflags_1 = WireInit(Cat(NV, DZ, OF, UF, NX))
    val is_vfrsqrt7 = is_vfr & !is_signed_int
    val is_vfrec7 = is_vfr & is_signed_int
    val vfrsqrt7Table = Module(new Rsqrt7Table)
    val vfrec7Table = Module(new Rec7Table)
    val in = VectorFloat.fromUInt(io.src, expWidth, precision)

    val is_normal = in.decode.isNormal
    val is_subnormal = in.decode.isSubnormal
    val is_inf = in.decode.isInf
    val is_nan = in.decode.isNaN
    val is_neginf = in.sign & in.decode.isInf
    val is_neginf_negzero = in.sign & (is_normal | is_subnormal & in.decode.sigNotZero)
    val is_negzero = in.sign & in.decode.isZero
    val is_poszero = !in.sign & in.decode.isZero
    val is_poszero_posinf = !in.sign & (is_normal | is_subnormal & in.decode.sigNotZero)
    val is_snan = in.decode.isSNaN
    val is_neg2_bplus1_b = in.sign & (in.exp === 30.U)
    val is_neg2_b_bminus1 = in.sign & (in.exp === 29.U)
    val is_neg2_negbminus1_negzero = in.sign & (in.sig.head(2) === "b00".U) & is_subnormal & in.decode.sigNotZero
    val is_pos2_poszero_negbminus1 = !in.sign & (in.sig.head(2) === "b00".U) & is_subnormal & in.decode.sigNotZero
    val is_pos2_bminus1_b = !in.sign & (in.exp === 29.U)
    val is_pos2_b_bplus1 = !in.sign & (in.exp === 30.U)

    val zero_minus_lzc = 0.U - CLZ(in.sig) // 0 - count leading zero
    val exp_normalized = Mux(is_vfrsqrt7,
      Mux(is_poszero_posinf, Mux(is_normal, in.exp, Cat(Fill(expWidth - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc)), 0.U),
      Mux(is_normal, in.exp, Cat(Fill(expWidth - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc)))

    val exp_normalized_reg0 = RegNext(exp_normalized)

    val sig_normalized = Wire(UInt(11.W))
    sig_normalized := Mux(is_vfrsqrt7,
      Mux(is_poszero_posinf, Mux(is_normal, Cat(0.U, in.sig), (in.sig << 1.U).asUInt), 0.U),
      Mux(is_normal, Cat(0.U, in.sig), (in.sig << 1.U).asUInt))
    
    val sig_normalized_reg0 = Reg(UInt(11.W))
    sig_normalized_reg0 := RegNext(sig_normalized)

    val sig_in7 = Mux(RegNext(is_vfrsqrt7),
      Cat(exp_normalized_reg0(0), (sig_normalized_reg0 << Mux(RegNext(is_normal), 0.U, RegNext(CLZ(sig_normalized))))(9, 4)), // vfrsqrt7  Cat(exp_nor(0), sig_nor(9,4))
      (sig_normalized_reg0 << Mux(RegNext(is_normal), 0.U, RegNext(CLZ(sig_normalized))))(9,3)) // vfrec7 sig_nor(9,3)
    
    vfrsqrt7Table.src := sig_in7
    vfrec7Table.src := sig_in7

    val sig_out7 = Wire(UInt(7.W))
    sig_out7 := Mux(RegNext(is_vfrsqrt7), vfrsqrt7Table.out, vfrec7Table.out)

    val out_exp_normalized = Mux(is_vfrec7, 29.U - exp_normalized, 0.U) // 2 * bias - 1 - exp_nor
    val out_exp = Wire(UInt(5.W))
    out_exp := Mux(is_vfrsqrt7,
      Mux(is_normal, (44.U - in.exp) >> 1, (44.U + CLZ(in.sig)) >> 1), // if normal (3 * bias - 1 - exp) >> 1 else (3 * bias -1 + CLZ) >>1
      Mux(out_exp_normalized === 0.U | out_exp_normalized.andR, 0.U, out_exp_normalized))
    val out_sig =
      Mux(RegNext(is_vfrec7),
        Mux(RegNext(out_exp_normalized) === 0.U | RegNext(out_exp_normalized).andR,
          Mux(RegNext(is_neg2_bplus1_b | is_pos2_b_bplus1),
            Cat(0.U, 1.U, sig_out7, 0.U),
            Mux(RegNext(is_neg2_b_bminus1 | is_pos2_bminus1_b),
              Cat(1.U, sig_out7, 0.U(2.W)),
              Cat(1.U, sig_out7, 0.U(2.W)) >> 1.U)),
          Cat(sig_out7, 0.U(3.W))),
        0.U)

    val out_sign = RegNext(is_poszero_posinf & in.sign)

    val fp_result = Wire(UInt(16.W))
    fp_result := Mux(RegNext(is_vfrsqrt7), Cat(out_sign, RegNext(out_exp), sig_out7, 0.U(3.W)), Cat(RegNext(in.sign), RegNext(out_exp), out_sig))


    val result_nan = Cat(0.U, Fill(6, 1.U), 0.U(9.W))
    val result_inf = Cat(Fill(5, 1.U), Fill(10, 0.U))
    val result_greatest_fin = Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U))

    when(RegNext(is_vfrsqrt7)) {
      when(RegNext(is_nan | is_neginf_negzero)) {
        result_1 := result_nan
        fflags_1 := RegNext(Mux(is_snan | is_neginf_negzero, "b10000".U, "b00000".U))
      }.elsewhen(RegNext(is_inf)) {
        result_1 := RegNext(Mux(is_neginf, result_nan, 0.U))
        fflags_1 := RegNext(Mux(is_neginf, "b10000".U, "b00000".U))
      }.elsewhen(RegNext(is_negzero | is_poszero)) {
        result_1 := RegNext(Mux(is_negzero, Cat(1.U, result_inf), Cat(0.U, result_inf)))
        fflags_1 := "b01000".U
      }.otherwise {
        result_1 := fp_result
      }
    }.otherwise {
      when(RegNext(is_nan)) {
        result_1 := result_nan
        fflags_1 := RegNext(Mux(is_snan, "b10000".U, "b00000".U))
      }.elsewhen(RegNext(is_inf)) {
        result_1 := RegNext(Mux(is_neginf, Cat(1.U, 0.U(15.W)), 0.U))
      }.elsewhen(RegNext(is_negzero | is_poszero)) {
        result_1 := RegNext(Mux(is_negzero, Cat(Fill(6, 1.U), 0.U(10.W)), Cat(0.U, Fill(5, 1.U), 0.U(10.W))))
        fflags_1 := "b01000".U
      }.elsewhen(RegNext(is_neg2_negbminus1_negzero)) {
        result_1 := RegNext(Mux((rm === RUP) | (rm === RTZ), Cat(1.U, result_greatest_fin), Cat(1.U, result_inf)))
        fflags_1 := "b00101".U
      }.elsewhen(RegNext(is_pos2_poszero_negbminus1)) {
        result_1 := RegNext(Mux((rm === RDN) | (rm === RTZ), Cat(0.U, result_greatest_fin), Cat(0.U, result_inf)))
        fflags_1 := "b00101".U
      }.otherwise {
        result_1 := fp_result
      }
    }
    result := RegNext(result_1)
    fflags := RegNext(fflags_1)
   }

   io.result := result
   io.fflags := fflags
}
