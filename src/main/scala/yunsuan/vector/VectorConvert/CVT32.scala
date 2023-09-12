package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.util.LookupTree
import yunsuan.vector.VectorConvert.util.{CLZ, ShiftRightJam, VFRSqrtTable, VFRecTable, RoundingUnit}


class CVT32(width: Int = 32) extends CVT(width){
    val expWidth_f16 = 5
    val precision_f16 = 11
    val expWidth_f32 = 8
    val precision_f32 = 24

    val is_sew_16 = io.sew === "b01".U
    val is_sew_32 = io.sew === "b10".U

    val is_single = io.opType.tail(3).head(2) === "b00".U
    val is_widden = io.opType.tail(3).head(2) === "b01".U
    val is_narrow = io.opType.tail(3).head(2) === "b10".U

    val is_vfr = io.opType.head(3) === "b111".U
    val is_fp2int = io.opType.head(2) === "b10".U & !is_vfr
    val is_int2fp = io.opType.head(2) === "b01".U & !is_vfr

    val is_signed_int = io.opType(0)
    val rm = io.rm

    val result = Wire(UInt(32.W))
    val NV, DZ, OF, UF, NX = WireInit(false.B)
    val fflags = WireInit(Cat(NV, DZ, OF, UF, NX))

    val result_nan = Cat(0.U, Fill(6, 1.U), 0.U(9.W))
    val result_inf = Cat(Fill(5, 1.U), Fill(10, 0.U))


/**
   * fptoint       
   * fp32 -> ui32    
   *      ->  i32  
   *      -> ui16    
   *      ->  i16  
   *
   * fp16 -> ui16     
   *      ->  i16     
   *      -> ui32     
   *      ->  i32 
   *      -> ui8      
   *      ->  i8      
   */
    when(is_fp2int) {
        val in_is_fp32 = (is_sew_32 & is_single) | (is_sew_16 & is_narrow)
        when(in_is_fp32) {
            val in = VectorFloat.fromUInt(io.src, expWidth_f32, precision_f32)
            val raw_in = RawVectorFloat.fromVFP(in, Some(in.decode.expNotZero))
            val max_int_exp = Mux(is_single, VectorFloat.expBias(expWidth_f32).U +& 31.U, VectorFloat.expBias(expWidth_f32).U +& 15.U)
            val exp_of = raw_in.exp > max_int_exp

            val lpath_shamt = Mux(is_single, raw_in.exp - (VectorFloat.expBias(expWidth_f32) + 23).U, 0.U)
            val lpath_sig_shifted = Mux(is_single, (raw_in.sig << lpath_shamt(7, 0))(31, 0), 0.U)
            val lpath_iv = Mux(is_single, (!is_signed_int & raw_in.sign).asUInt, 0.U).asBool
            val lpath_may_of = Mux(is_single, (is_signed_int & (raw_in.exp === max_int_exp)).asUInt, 0.U).asBool
            val lpath_pos_of = Mux(is_single, (lpath_may_of & !raw_in.sign).asUInt, 0.U).asBool
            val lpath_neg_of = Mux(is_single, (lpath_may_of & raw_in.sign & raw_in.sig.tail(1).orR).asUInt, 0.U).asBool
            val lpath_of = lpath_pos_of | lpath_neg_of

            val rpath_shamt = Mux(is_single, (VectorFloat.expBias(expWidth_f32) + 23).U - raw_in.exp, (VectorFloat.expBias(expWidth_f32) + 15).U - raw_in.exp)
            val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(Cat(raw_in.sig, 0.U), rpath_shamt)

            val rpath_rounder = Module(new RoundingUnit(precision_f32))
            rpath_rounder.io.in := Mux(is_single, rpath_sig_shifted.head(precision_f32).asUInt, rpath_sig_shifted.head(16).asUInt)
            rpath_rounder.io.roundIn := Mux(is_single, rpath_sig_shifted.tail(precision_f32).asUInt, rpath_sig_shifted.tail(16).head(1).asUInt).asBool
            rpath_rounder.io.stickyIn := rpath_sticky | Mux(is_narrow, rpath_sig_shifted.tail(17).orR.asUInt, 0.U).asBool
            rpath_rounder.io.signIn := raw_in.sign
            rpath_rounder.io.rm := rm

            val out_r_up = rpath_rounder.io.in + 1.U
            val out = Mux(rpath_rounder.io.r_up, out_r_up, rpath_rounder.io.in)
            val cout = rpath_rounder.io.r_up && Mux(is_narrow, rpath_rounder.io.in.tail(9).andR.asUInt, rpath_rounder.io.in.andR.asUInt).asBool

            val rpath_sig = Mux(is_single, Cat(0.U(7.W), cout, out), Cat(0.U(8.W), out))
            val rpath_ix = rpath_rounder.io.inexact
            val rpath_iv = !is_signed_int & raw_in.sign & rpath_sig.orR

            val rpath_pos_of = !raw_in.sign &
                Mux(is_signed_int,
                    (raw_in.exp === 142.U | (raw_in.exp === 141.U & cout)).asUInt,
                    (raw_in.exp === 142.U & cout).asUInt).asBool
            val rpath_neg_of = raw_in.sign & (raw_in.exp === 142.U) & (rpath_rounder.io.in.tail(8).orR | rpath_rounder.io.r_up)
            val rpath_of = Mux(is_narrow, (rpath_neg_of | rpath_pos_of).asUInt, cout.asUInt).asBool

            val sel_lpath = raw_in.exp >= 150.U
            val of = exp_of | sel_lpath & lpath_of | !sel_lpath & rpath_of
            val iv = of | sel_lpath & lpath_iv | !sel_lpath & rpath_iv
            val ix = !iv & !sel_lpath & rpath_ix

            val int_abs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
            val int = Mux(is_narrow, 
                Mux(raw_in.sign & is_signed_int, -int_abs.tail(16), int_abs.tail(16)), 
                Mux(raw_in.sign & is_signed_int, -int_abs, int_abs))
        
            val max_int32 = Mux(is_single, Cat(!is_signed_int, ~0.U(31.W)), Cat(!is_signed_int, ~0.U(15.W)))
            val min_int32 = Mux(is_single, Cat(is_signed_int,   0.U(31.W)), Cat(is_signed_int,   0.U(15.W)))

            result := Mux(iv, Mux(in.decode.isNaN | !raw_in.sign, max_int32, min_int32), int)
            fflags := Cat(iv, false.B, false.B, false.B, ix)
        }.otherwise {
            val in = VectorFloat.fromUInt(io.src, expWidth_f16, precision_f16)
            val raw_in = RawVectorFloat.fromVFP(in, Some(in.decode.expNotZero))
            val max_int_exp = Mux(is_single, VectorFloat.expBias(expWidth_f16).U +& 15.U, Mux(is_widden, VectorFloat.expBias(expWidth_f16).U +& 31.U, VectorFloat.expBias(expWidth_f16).U +& 7.U))
            val exp_of = (raw_in.exp > max_int_exp) | in.decode.expIsOnes

            val lpath_shamt = Mux(is_single | is_widden, raw_in.exp - (VectorFloat.expBias(expWidth_f16) + precision_f16 - 1).U, 0.U)
            val lpath_max_shamt = Mux(is_single, (15 - (precision_f16 - 1)).U, Mux(is_widden, (31 - (precision_f16 - 1)).U, 0.U))
            val lpath_max_shamt_width = lpath_max_shamt.getWidth
            val lpath_sig_shifted =
                Mux(is_single, (raw_in.sig << lpath_shamt(lpath_max_shamt_width - 1, 0))(15, 0),
                    Mux(is_widden, (raw_in.sig << lpath_shamt(lpath_max_shamt_width - 1, 0))(31, 0), 0.U))
            val lpath_iv = Mux(is_single | is_widden, (!is_signed_int & raw_in.sign).asUInt, 0.U).asBool
            val lpath_may_of = Mux(is_single | is_widden, (is_signed_int & (raw_in.exp === max_int_exp)).asUInt, 0.U).asBool
            val lpath_pos_of = Mux(is_single | is_widden, (lpath_may_of & !raw_in.sign).asUInt, 0.U).asBool
            val lpath_neg_of = Mux(is_single | is_widden, (lpath_may_of & raw_in.sign & raw_in.sig.tail(1).orR).asUInt, 0.U).asBool
            val lpath_of = lpath_pos_of | lpath_neg_of

            val rpath_shamt = Mux(is_single | is_widden,
                (VectorFloat.expBias(expWidth_f16) + precision_f16 - 1).U - raw_in.exp,
                (VectorFloat.expBias(expWidth_f16) + 8 - 1).U - raw_in.exp)
            val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(Cat(raw_in.sig, 0.U), rpath_shamt)

            val rpath_rounder = Module(new RoundingUnit(precision_f16))
            rpath_rounder.io.in := Mux(is_single | is_widden, rpath_sig_shifted.head(precision_f16), rpath_sig_shifted.head(8))
            rpath_rounder.io.roundIn := Mux(is_single | is_widden, rpath_sig_shifted.tail(precision_f16), rpath_sig_shifted.tail(8).head(1)).asBool
            rpath_rounder.io.stickyIn := rpath_sticky || Mux(is_narrow, rpath_sig_shifted.tail(9).orR.asUInt, 0.U).asBool
            rpath_rounder.io.signIn := raw_in.sign.asBool
            rpath_rounder.io.rm := rm

            val out_r_up = rpath_rounder.io.in + 1.U
            val out = Mux(rpath_rounder.io.r_up, out_r_up, rpath_rounder.io.in)
            val cout = rpath_rounder.io.r_up && Mux(is_narrow, rpath_rounder.io.in.tail(4).andR.asUInt, rpath_rounder.io.in.andR.asUInt).asBool

            val rpath_sig = Mux(is_single | is_widden, Cat(0.U((32 - precision_f16 - 1).W), cout, out),
                Cat(0.U(21.W), out))

            val rpath_ix = rpath_rounder.io.inexact || Mux(is_narrow, rpath_sig_shifted.tail(8).orR.asUInt, 0.U).asBool
            val rpath_iv = !is_signed_int & raw_in.sign & rpath_sig.orR

            val rpath_pos_of = !raw_in.sign &
                Mux(is_signed_int,
                (raw_in.exp === 22.U) | (raw_in.exp === 21.U & cout),
                (raw_in.exp === 22.U) & cout)
            val rpath_neg_of = raw_in.sign & (raw_in.exp === 22.U) & (rpath_rounder.io.in.tail(4).orR | rpath_rounder.io.r_up)
            val rpath_of = Mux(is_narrow, (rpath_neg_of | rpath_pos_of).asUInt, cout.asUInt).asBool

            val sel_lpath = raw_in.exp >= (VectorFloat.expBias(expWidth_f16) + precision_f16 - 1).U

            val of = exp_of | (sel_lpath & lpath_of) | (!sel_lpath & rpath_of)
            val iv = of | (sel_lpath & lpath_iv) | (!sel_lpath & rpath_iv)
            val ix = !iv & !sel_lpath & rpath_ix

            val int_abs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
            val int =
                Mux(is_narrow,
                Mux(raw_in.sign & is_signed_int,
                    -int_abs.tail(24), int_abs.tail(24)),
                Mux(is_single,
                    Mux(raw_in.sign & is_signed_int, -int_abs.tail(16), int_abs.tail(16)),
                    Mux(raw_in.sign & is_signed_int, -int_abs, int_abs)))

            val max_int = Mux(is_single, Cat(!is_signed_int, ~0.U(15.W)), Mux(is_narrow, Cat(!is_signed_int, ~0.U(7.W)), Cat(!is_signed_int, ~0.U(31.W))))
            val min_int = Mux(is_single, Cat(is_signed_int,   0.U(15.W)), Mux(is_narrow, Cat(is_signed_int,   0.U(7.W)), Cat(is_signed_int,   0.U(31.W))))

            result := Mux(iv, Mux(in.decode.isNaN | !raw_in.sign, max_int, min_int).asUInt, int)
            fflags := Cat(iv, false.B, false.B, false.B, ix)
        }
    }.elsewhen(is_int2fp) {
/**
     * inttofp         
     * ui32 -> fp32      
     * i32 ->         
     * ui16 ->           
     * i16 ->        
     *
     * ui32 -> fp16    
     * i32 ->       
     * ui16 ->         
     * i16 ->       
     * ui8  ->        
     * i8  ->      
     *
     */
        val out_is_fp32 = (is_sew_32 & is_single) | (is_sew_16 & is_widden)
        when(out_is_fp32) {
            val sign = is_signed_int & Mux(is_widden, io.src(15), io.src(31))
            val in_sext = Cat(Fill(16, io.src(15)), io.src(15, 0))
            val in = Mux(is_signed_int & is_widden, in_sext, io.src)
            val in_abs = Mux(sign, (~in).asUInt + 1.U, in)

            val lzc = CLZ(in_abs)

            val in_shift = (in_abs << lzc)(30, 0)
            val exp_raw = (VectorFloat.expBias(expWidth_f32) + 31).U - lzc

            val sig_raw = Mux(is_widden, in_shift.head(16), in_shift.head(23))
            val round_bit = Mux(is_widden, in_shift.tail(16).head(1), in_shift.tail(23).head(1))
            val sticky_bit = Mux(is_widden, in_shift.tail(16).orR.asUInt, in_shift.tail(precision_f32).orR.asUInt).asBool
            val rounder = Module(new RoundingUnit(precision_f32 - 1))
            rounder.io.in := sig_raw
            rounder.io.roundIn := round_bit
            rounder.io.stickyIn := sticky_bit
            rounder.io.signIn := Mux(is_signed_int, sign.asUInt, 0.U).asBool
            rounder.io.rm := rm

            val out_r_up = rounder.io.in + 1.U
            val out = Mux(rounder.io.r_up, out_r_up, rounder.io.in)
            val cout = rounder.io.r_up && Mux(is_narrow, rounder.io.in.tail(9).andR.asUInt, rounder.io.in.andR.asUInt).asBool


            val exp = Mux(in === 0.U, 0.U, exp_raw + cout)
            val sig = out

            val of = exp === 255.U
            val ix = rounder.io.inexact

            result := Cat(Mux(is_signed_int, sign, 0.U), exp, Mux(is_widden, Cat(sig.tail(7), 0.U(7.W)), sig))
            fflags := Cat(0.U(2.W), of, 0.U, ix)

        }.otherwise {
            val sign = is_signed_int & Mux(is_single, io.src(15).asUInt, Mux(is_widden, io.src(7).asUInt, io.src.head(1).asUInt)).asBool
            val in_sext =
                Mux(is_single, Cat(Fill(16, io.src(15)), io.src(15, 0)),
                    Mux(is_widden, Cat(Fill(24, io.src(7)), io.src(7, 0)), io.src))
            val in = Mux(is_signed_int & (is_widden | is_single), in_sext, io.src)
            val in_abs = Mux(sign, (~in).asUInt + 1.U, in)

            val exp_of = Mux(is_narrow, in_abs.head(16).orR.asUInt, 0.U).asBool

            val lzc = Mux(is_narrow, Mux(!in_abs.head(16).orR, CLZ(in_abs(15, 0)), CLZ(in_abs)), CLZ(in_abs(15, 0)))

            val in_shift = Mux(is_narrow, Mux(!in_abs.head(16).orR, Cat((in_abs << lzc)(14, 0), 0.U(16.W)), (in_abs << lzc)(30, 0)), Cat((in_abs << lzc)(14, 0), 0.U(16.W)))
            val exp_raw = VectorFloat.expBias(expWidth_f16).U +& 15.U - lzc.asUInt

            val sig_raw = Mux(is_widden, in_shift.head(8).asUInt, in_shift.head(10).asUInt)
            val round_bit = Mux(is_widden, in_shift.tail(8).head(1), in_shift.tail(10).head(1))
            val sticky_bit = Mux(is_widden, in_shift.tail(8).orR.asUInt, in_shift.tail(precision_f16).orR.asUInt).asBool
            val rounder = Module(new RoundingUnit(precision_f16 - 1))
            rounder.io.in := sig_raw
            rounder.io.roundIn := round_bit
            rounder.io.stickyIn := sticky_bit
            rounder.io.signIn := sign
            rounder.io.rm := rm

            val out_r_up = rounder.io.in + 1.U
            val out = Mux(rounder.io.r_up, out_r_up, rounder.io.in)
            val cout = rounder.io.r_up && Mux(is_narrow, rounder.io.in.tail(9).andR.asUInt, rounder.io.in.andR.asUInt).asBool

            val exp = Mux(in === 0.U, 0.U, exp_raw + cout)
            val sig = out

            val of = Mux(is_narrow, (exp_of | (exp === 31.U)).asUInt, (exp === 31.U).asUInt).asBool
            val ix = Mux(is_narrow, exp_of | rounder.io.inexact, rounder.io.inexact)


            val result_fp = Cat(sign, Mux(RoundingUnit.is_rmin(rm, sign), Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U)), Cat(Fill(5, 1.U), Fill(10, 0.U))))

            result := Mux(is_narrow & of, result_fp, Cat(sign, exp, Mux(is_widden, Cat(sig.tail(2), 0.U(2.W)), sig)))
            fflags := Cat(0.U(2.W), of, 0.U, ix)
        }
    }.elsewhen(is_vfr) {
        val in_is_fp16 = is_sew_16
        val in_is_fp32 = is_sew_32
        val is_vfrsqrt7 = !io.opType(0).asBool
        val is_vfrec7 = io.opType(0).asBool
        when(in_is_fp16) {
            val in = io.src(15,0)
            val sign = in.head(1).asBool
            val exp = in.tail(1).head(expWidth_f16)
            val sig = in.tail(6)

            val is_normal = exp.orR & !exp.andR
            val is_subnormal = !exp.orR
            val is_inf = exp.andR & !sig.orR
            val is_nan = exp.andR & sig.orR
            val is_neginf = sign & is_inf
            val is_neginf_negzero = sign & (is_normal | is_subnormal & sig.orR)
            val is_negzero = sign & is_subnormal & !sig.orR
            val is_poszero = !sign & is_subnormal & !sig.orR
            val is_poszero_posinf = !sign & (is_normal | is_subnormal & sig.orR)
            val is_snan = !sig.head(1).asBool & is_nan
            val is_neg2_bplus1_b = sign & (exp === 30.U)
            val is_neg2_b_bminus1 = sign & (exp === 29.U)
            val is_neg2_negbminus1_negzero = sign & (sig.head(2) === "b00".U) & is_subnormal & sig.orR
            val is_pos2_poszero_negbminus1 = !sign & (sig.head(2) === "b00".U) & is_subnormal & sig.orR
            val is_pos2_bminus1_b = !sign & (exp === 29.U)
            val is_pos2_b_bplus1 = !sign & (exp === 30.U)

            val zero_minus_lzc = 0.U - CLZ(sig) // 0 - count leading zero
            val exp_normalized =
                Mux(is_vfrsqrt7,
                Mux(is_poszero_posinf,
                    Mux(is_normal, exp, Cat(Fill(expWidth_f16 - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc)), 0.U).asUInt,
                Mux(is_normal, exp, Cat(Fill(expWidth_f16 - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc))).asUInt

            val sig_normalized = Mux(is_vfrsqrt7,
                Mux(is_poszero_posinf, Mux(is_normal, Cat(0.U, sig), (sig << 1.U).asUInt).asUInt, 0.U),
                Mux(is_normal, Cat(0.U, sig), (sig << 1.U).asUInt))

            val sig_in7 = Mux(is_vfrsqrt7,
                Cat(exp_normalized(0), (sig_normalized << Mux(is_normal, 0.U, CLZ(sig_normalized)))(9, 4)).asUInt, // vfrsqrt7  Cat(exp_nor(0), sig_nor(9,4))
                (sig_normalized << Mux(is_normal, 0.U, CLZ(sig_normalized)))(9, 3)).asUInt // vfrec7 sig_nor(9,3)

            val sig_out7 = Wire(UInt(7.W))
            sig_out7 := Mux(is_vfrsqrt7, LookupTree(sig_in7, VFRSqrtTable.VFRSqrtTable), LookupTree(sig_in7, VFRecTable.VFRecTable)).asUInt

            val out_exp_normalized = Mux(is_vfrec7, 29.U - exp_normalized, 0.U).asUInt // 2 * bias - 1 - exp_nor todo
            val out_exp = Wire(UInt(5.W))
            out_exp := Mux(is_vfrsqrt7,
                Mux(is_normal, (44.U - exp) >> 1, (44.U + CLZ(sig)) >> 1), // if normal (3 * bias - 1 - exp) >> 1 else (3 * bias -1 + CLZ) >>1
                Mux(out_exp_normalized === 0.U | out_exp_normalized.andR, 0.U, out_exp_normalized))
            val out_sig =
                Mux(is_vfrec7,
                Mux(out_exp_normalized === 0.U | out_exp_normalized.andR,
                    Mux(is_neg2_bplus1_b | is_pos2_b_bplus1,
                    Cat(0.U, 1.U, sig_out7, 0.U),
                    Mux(is_neg2_b_bminus1 | is_pos2_bminus1_b,
                        Cat(1.U, sig_out7, 0.U(2.W)),
                        Cat(1.U, sig_out7, 0.U(2.W)) >> 1.U)),
                    Cat(sig_out7, 0.U(3.W))),
                0.U)

            val out_sign = is_poszero_posinf & sign

            val fp_result = Wire(UInt(16.W))
            fp_result := Mux(is_vfrsqrt7, Cat(out_sign, out_exp, sig_out7, 0.U(3.W)), Cat(sign, out_exp, out_sig))


            val result_nan = Cat(0.U, Fill(6, 1.U), 0.U(9.W))
            val result_inf = Cat(Fill(5, 1.U), Fill(10, 0.U))
            val result_greatest_fin = Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U))

            when(is_vfrsqrt7) {
                when(is_nan | is_neginf_negzero) {
                result := result_nan
                fflags := Mux(is_snan | is_neginf_negzero, "b10000".U, "b00000".U)
                }.elsewhen(is_inf) {
                result := Mux(is_neginf, result_nan, 0.U)
                fflags := Mux(is_neginf, "b10000".U, "b00000".U)
                }.elsewhen(is_negzero | is_poszero) {
                result := Mux(is_negzero, Cat(1.U, result_inf), Cat(0.U, result_inf))
                fflags := "b01000".U
                }.otherwise {
                result := fp_result
                }
            }.otherwise {
                when(is_nan) {
                result := result_nan
                fflags := Mux(is_snan, "b10000".U, "b00000".U)
                }.elsewhen(is_inf) {
                result := Mux(is_neginf, Cat(1.U, 0.U(15.W)), 0.U(16.W))
                }.elsewhen(is_negzero | is_poszero) {
                result := Mux(is_negzero, Cat(Fill(6, 1.U), 0.U(10.W)), Cat(0.U, Fill(5, 1.U), 0.U(10.W)))
                fflags := "b01000".U
                }.elsewhen(is_neg2_negbminus1_negzero) {
                result := Mux((rm === RoundingModle.RUP) | (rm === RoundingModle.RTZ), Cat(1.U, result_greatest_fin), Cat(1.U, result_inf))
                fflags := "b00101".U
                }.elsewhen(is_pos2_poszero_negbminus1) {
                result := Mux((rm === RoundingModle.RDN) | (rm === RoundingModle.RTZ), Cat(0.U, result_greatest_fin), Cat(0.U, result_inf))
                fflags := "b00101".U
                }.otherwise {
                result := fp_result
                }
            }
        }.otherwise {
            val in = io.src
            val sign = in.head(1).asBool
            val exp = in.tail(1).head(expWidth_f32)
            val sig = in.tail(1+expWidth_f32)

            val is_normal = exp.orR & !exp.andR
            val is_subnormal = !exp.orR
            val is_inf = exp.andR & !sig.orR
            val is_nan = exp.andR & sig.orR
            val is_neginf = sign & is_inf
            val is_neginf_negzero = sign & (is_normal | is_subnormal & sig.orR)
            val is_negzero = sign & is_subnormal & !sig.orR
            val is_poszero = !sign & is_subnormal & !sig.orR
            val is_poszero_posinf = !sign & (is_normal | is_subnormal & sig.orR)
            val is_snan = !sig.head(1).asBool & is_nan
            val is_neg2_bplus1_b = sign & (exp === 254.U)
            val is_neg2_b_bminus1 = sign & (exp === 253.U)
            val is_neg2_negbminus1_negzero = sign & (sig.head(2) === "b00".U) & is_subnormal & sig.orR
            val is_pos2_poszero_negbminus1 = !sign & (sig.head(2) === "b00".U) & is_subnormal & sig.orR
            val is_pos2_bminus1_b = !sign & (exp === 253.U)
            val is_pos2_b_bplus1 = !sign & (exp === 254.U)

            val zero_minus_lzc = 0.U - CLZ(sig) // 0 - count leading zero
            val exp_normalized = Mux(is_vfrsqrt7,
                Mux(is_poszero_posinf, Mux(is_normal, exp, Cat(Fill(expWidth_f32 - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc)), 0.U),
                Mux(is_normal, exp, Cat(Fill(expWidth_f32 - zero_minus_lzc.getWidth, zero_minus_lzc.head(1)), zero_minus_lzc)))

            val sig_normalized = Mux(is_vfrsqrt7,
                Mux(is_poszero_posinf, Mux(is_normal, Cat(0.U, sig), (sig << 1.U).asUInt), 0.U),
                Mux(is_normal, Cat(0.U, sig), (sig << 1.U).asUInt))

            val sig_in7 = Mux(is_vfrsqrt7,
                Cat(exp_normalized(0), (sig_normalized << Mux(is_normal, 0.U, CLZ(sig_normalized)))(22, 17)),
                (sig_normalized << Mux(is_normal, 0.U, CLZ(sig_normalized)))(22, 16))
            val sig_out7 = Wire(UInt(7.W))
            sig_out7 := Mux(is_vfrsqrt7, LookupTree(sig_in7, VFRSqrtTable.VFRSqrtTable), LookupTree(sig_in7, VFRecTable.VFRecTable))

            val out_exp_normalized = Mux(is_vfrec7, 253.U - exp_normalized, 0.U) // 2 * bias - 1 - exp_nor
            val out_exp = Wire(UInt(8.W))
            out_exp := Mux(is_vfrsqrt7,
                Mux(is_normal, (380.U - exp) >> 1, (380.U + CLZ(sig)) >> 1), // if normal (3 * bias - 1 - exp) >> 1 else (3 * bias -1 + CLZ) >>1
                Mux(out_exp_normalized === 0.U | out_exp_normalized.andR, 0.U, out_exp_normalized))
            val out_sig =
                Mux(is_vfrec7,
                Mux(out_exp_normalized === 0.U | out_exp_normalized.andR,
                    Mux(is_neg2_bplus1_b | is_pos2_b_bplus1,
                    Cat(0.U, 1.U, sig_out7, 0.U(14.W)),
                    Mux(is_neg2_b_bminus1 | is_pos2_bminus1_b,
                        Cat(1.U, sig_out7, 0.U(15.W)),
                        Cat(1.U, sig_out7, 0.U(15.W)) >> 1.U)),
                    Cat(sig_out7, 0.U(16.W))),
                0.U)

            val out_sign = is_poszero_posinf & sign

            val fp_result = Wire(UInt(32.W))
            fp_result := Mux(is_vfrsqrt7, Cat(out_sign, out_exp, sig_out7, 0.U(16.W)), Cat(sign, out_exp, out_sig))


            val result_nan = Cat(0.U, Fill(9, 1.U), 0.U(22.W))
            val result_inf = Cat(Fill(8, 1.U), Fill(23, 0.U))
            val result_greatest_fin = Cat(Fill(7, 1.U), 0.U, Fill(23, 1.U))

            when(is_vfrsqrt7) {
                when(is_nan | is_neginf_negzero) {
                result := result_nan
                fflags := Mux(is_snan | is_neginf_negzero, "b10000".U, "b00000".U)
                }.elsewhen(is_inf) {
                result := Mux(is_neginf, result_nan, 0.U)
                fflags := Mux(is_neginf, "b10000".U, "b00000".U)
                }.elsewhen(is_negzero | is_poszero) {
                result := Mux(is_negzero, Cat(1.U, result_inf), Cat(0.U, result_inf))
                fflags := "b01000".U
                }.otherwise {
                result := fp_result
                }
            }.otherwise {
                when(is_nan) {
                result := result_nan
                fflags := Mux(is_snan, "b10000".U, "b00000".U)
                }.elsewhen(is_inf) {
                result := Mux(is_neginf, Cat(1.U, 0.U(31.W)), 0.U(32.W))
                }.elsewhen(is_negzero | is_poszero) {
                result := Mux(is_negzero, Cat(Fill(9, 1.U), 0.U(23.W)), Cat(0.U, Fill(8, 1.U), 0.U(23.W)))
                fflags := "b01000".U
                }.elsewhen(is_neg2_negbminus1_negzero) {
                result := Mux((rm === RoundingModle.RUP) | (rm === RoundingModle.RTZ), Cat(1.U, result_greatest_fin), Cat(1.U, result_inf))
                fflags := "b00101".U
                }.elsewhen(is_pos2_poszero_negbminus1) {
                result := Mux((rm === RoundingModle.RDN) | (rm === RoundingModle.RTZ), Cat(0.U, result_greatest_fin), Cat(0.U, result_inf))
                fflags := "b00101".U
                }.otherwise {
                result := fp_result
                }
            }
        }
    }.otherwise {
    /**
     * fptofp
     * fp32 -> fp16 
     * fp16 -> fp32 
     */
        when(is_narrow) {
            val exp_delta = VectorFloat.expBias(expWidth_f32) - VectorFloat.expBias(expWidth_f16)

            val in = VectorFloat.fromUInt(io.src, expWidth_f32, precision_f32)
            val down_exp = in.exp.zext - exp_delta.S

            // normal
            val nor_sig = in.sig.head(precision_f16 - 1)
            val nor_roundBit = in.sig.tail(precision_f16 - 1).head(1).asBool
            val nor_stickyBit = in.sig.tail(precision_f16).orR

            val nor_rounder = Module(new RoundingUnit(precision_f16 - 1))
            nor_rounder.io.in := nor_sig
            nor_rounder.io.roundIn := nor_roundBit
            nor_rounder.io.stickyIn := nor_stickyBit
            nor_rounder.io.signIn := in.sign
            nor_rounder.io.rm := rm

            val out_r_up = nor_rounder.io.in + 1.U
            val out = Mux(nor_rounder.io.r_up, out_r_up, nor_rounder.io.in)
            val cout = nor_rounder.io.r_up && Mux(is_narrow, nor_rounder.io.in.tail(9).andR.asUInt, nor_rounder.io.in.andR.asUInt).asBool


            val nor_sig_rounded = out
            val nor_exp_rounded = Mux(cout, down_exp + 1.S, down_exp)
            val nor_of =
                Mux(cout,
                down_exp > (VectorFloat.maxNormExp(expWidth_f16) - 1).S,
                down_exp > VectorFloat.maxNormExp(expWidth_f16).S).asBool

            val exp_uf = Mux(cout, down_exp < 0.S, down_exp < 1.S).asBool
            val nor_ix = nor_of | nor_rounder.io.inexact

            // subnormal
            val shamt = (exp_delta + 1).U(expWidth_f32.W) - in.exp
            val (subnor_sig, shift_sticky) = ShiftRightJam(Cat(in.decode.expNotZero, in.sig.head(precision_f16)), shamt)
            val subnor_stickyBit = shift_sticky | nor_stickyBit
            val subnor_rounder = Module(new RoundingUnit(precision_f16 - 1))
            subnor_rounder.io.in := subnor_sig.tail(1).head(precision_f16 - 1)
            subnor_rounder.io.roundIn := subnor_sig(0)
            subnor_rounder.io.stickyIn := subnor_stickyBit
            subnor_rounder.io.signIn := in.sign
            subnor_rounder.io.rm := rm

            val out_r_up1 = subnor_rounder.io.in + 1.U
            val out1 = Mux(subnor_rounder.io.r_up, out_r_up1, subnor_rounder.io.in)
            val cout1 = subnor_rounder.io.r_up && Mux(is_narrow, subnor_rounder.io.in.tail(9).andR.asUInt, subnor_rounder.io.in.andR.asUInt).asBool


            val subnor_sig_rounded = out1
            val subnor_exp_rounded = Mux(cout1, 1.U, 0.U)
            val subnor_ix = subnor_rounder.io.inexact

            val may_be_subnor = down_exp < 1.S

            val rmin = rm === RoundingModle.RTZ || (rm === RoundingModle.RDN & !in.sign) || (rm === RoundingModle.RUP && in.sign)

            val nor_of_exp = Mux(rmin | io.opType(0).asBool, // ROD
                VectorFloat.maxNormExp(expWidth_f16).U(expWidth_f16.W),
                (VectorFloat.maxNormExp(expWidth_f16) + 1).U(expWidth_f16.W))

            // ROD
            val nor_of_sig = Mux(rmin | io.opType(0).asBool, ~0.U((precision_f16 - 1).W), 0.U((precision_f16 - 1).W))

            val common_exp = Mux1H(
                Seq(
                !may_be_subnor & nor_of,
                !may_be_subnor & !nor_of,
                may_be_subnor
                ),
                Seq(
                nor_of_exp,
                nor_exp_rounded(expWidth_f16 - 1, 0),
                subnor_exp_rounded
                )
            )

            val common_sig = Mux1H(
                Seq(
                !may_be_subnor & nor_of,
                !may_be_subnor & !nor_of,
                may_be_subnor
                ),
                Seq(
                nor_of_sig,
                nor_sig_rounded,
                subnor_sig_rounded
                )
            )

            val special_case = in.decode.expIsOnes

            val iv = in.decode.isSNaN
            val dz = false.B
            val of = !special_case & nor_of
            val uf = !special_case & may_be_subnor & exp_uf & subnor_ix
            val ix = !special_case & (
                (!may_be_subnor & nor_ix) |
                (may_be_subnor & subnor_ix)
            )

            result := Cat(
                !in.decode.isNaN & in.sign,
                Mux1H(
                Seq(special_case, !special_case),
                Seq(~0.U(expWidth_f16.W), common_exp)
                ),
                Mux1H(
                Seq(special_case, !special_case),
                Seq(
                    Cat(in.decode.sigNotZero, 0.U((precision_f16 - 2).W)),
                    common_sig
                )
                )
            )
            fflags := Cat(iv, dz, of ,uf, ix)
        }.otherwise {
            val in = VectorFloat.fromUInt(io.src(15,0), expWidth_f16, precision_f16)
            val exp_delta = VectorFloat.expBias(expWidth_f32) - VectorFloat.expBias(expWidth_f16)
            val nor_sig = in.sig
            val nor_exp = exp_delta.U(expWidth_f32.W) + in.exp

            val subnor_shamt = CLZ(in.sig)
            val subnor_sig = Cat((in.sig << subnor_shamt)(precision_f16 - 3, 0), 0.U(1.W))
            val subnor_exp = exp_delta.U(expWidth_f32.W) - subnor_shamt

            result := Cat(
                !in.decode.isNaN & in.sign,
                Mux1H(
                Seq(
                    in.decode.expIsOnes,
                    in.decode.isZero,
                    in.decode.isSubnormal,
                    !in.decode.expIsOnes & !in.decode.expIsZero
                ),
                Seq(
                    ~0.U(expWidth_f32.W),
                    0.U(expWidth_f32.W),
                    subnor_exp,
                    nor_exp
                )
                ),
                Mux1H(
                Seq(
                    in.decode.expIsOnes,
                    in.decode.expIsZero,
                    !in.decode.expIsOnes & !in.decode.expIsZero
                ),
                Seq(
                    Cat(in.decode.sigNotZero, 0.U((precision_f32 - 2).W)),
                    Cat(subnor_sig, 0.U((precision_f32 - precision_f16).W)),
                    Cat(nor_sig, 0.U((precision_f32 - precision_f16).W))
                )
                )
            )

            fflags := Cat(in.decode.isSNaN, 0.U(4.W))
        }
    }

    io.result := result
    io.fflags := fflags
}
