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
    val is_widen  = io.opType.tail(3).head(2) === "b01".U
    val is_narrow = io.opType.tail(3).head(2) === "b10".U

    val is_vfr = io.opType(5).asBool
    val is_fp2int = io.opType.head(2) === "b10".U & !is_vfr
    val is_int2fp = io.opType.head(2) === "b01".U & !is_vfr

    val in_is_fp = io.opType.head(1).asBool
    val out_is_fp = io.opType.tail(1).head(1).asBool

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
    val in_is_fp32 = (is_sew_32 & is_single | is_sew_16 & is_narrow).asBool
    val fp32toint32 = in_is_fp32 && is_single
    val fp32toint16 = in_is_fp32 && is_narrow
    val fp16toint32 = !in_is_fp32 && is_widen
    val fp16toint16 = !in_is_fp32 && is_single
    val fp16toint8 = !in_is_fp32 && is_narrow
    val src = Mux(in_is_fp32, io.src,
        io.src.tail(16).head(1) ## 0.U(3.W) ## io.src.tail(17).head(f16.expWidth) ## io.src.tail(f16.expWidth+17) ## 0.U(13.W))
    val in = VectorFloat.fromUInt(src, f32.expWidth, f32.precision)
    val expNotZero = Mux1H(
      Seq(fp32toint32 || fp32toint16,
        fp16toint32 || fp16toint16 || fp16toint8),
      Seq(in.decode.expNotZero,
        src.tail(4).head(f16.expWidth).orR)
    )
    val raw_in = RawVectorFloat.fromVFP(in, Some(expNotZero))
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
    val exp_of = raw_in.exp > max_int_exp || (!in_is_fp32 && src.tail(4).head(f16.expWidth).andR)

    // left
    val lpath_shamt = raw_in.exp - Mux1H(
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
    val lpath_sig_shifted = Mux1H(
      Seq(fp32toint32,
        fp16toint16 || fp16toint32),
      Seq((raw_in.sig << lpath_shamt(lpath_max_shamt.getWidth - 1, 0))(31, 0),
      (raw_in.sig << lpath_shamt(lpath_max_shamt.getWidth - 1, 0))(31, 13))
    )

    val lpath_iv = (fp32toint32 || fp16toint16 || fp16toint32) && !is_signed_int && raw_in.sign
    val lpath_may_of = (fp32toint32 || fp16toint16 || fp16toint32) && is_signed_int && (raw_in.exp === max_int_exp)
    val lpath_pos_of = (fp32toint32 || fp16toint16 || fp16toint32) && lpath_may_of && !raw_in.sign
    val lpath_neg_of = (fp32toint32 || fp16toint16 || fp16toint32) && lpath_may_of && raw_in.sign && raw_in.sig.tail(1).orR
    val lpath_of = lpath_pos_of || lpath_neg_of

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
    ) - raw_in.exp
    val (rpath_sig_shifted, rpath_sticky) = ShiftRightJam(Cat(raw_in.sig,0.U), rpath_shamt)

    val rpath_rounder = Module(new RoundingUnit(f32.precision))
    rpath_rounder.io.in := Mux1H(
      Seq(fp32toint32,
        fp32toint16,
        fp16toint16 || fp16toint32,
        fp16toint8),
      Seq(rpath_sig_shifted.head(f32.precision),
        rpath_sig_shifted.head(16),
        rpath_sig_shifted.head(f16.precision),
        rpath_sig_shifted.head(8))
    )
    rpath_rounder.io.roundIn := Mux1H(
      Seq(fp32toint32,
        fp32toint16,
        fp16toint16 || fp16toint32,
        fp16toint8),
      Seq(rpath_sig_shifted.tail(f32.precision).head(1),
        rpath_sig_shifted.tail(16).head(1),
        rpath_sig_shifted.tail(f16.precision).head(1),
        rpath_sig_shifted.tail(8).head(1))
    )
    rpath_rounder.io.stickyIn := Mux1H(
      Seq(fp32toint32,
        fp32toint16,
        fp16toint16 || fp16toint32,
        fp16toint8),
      Seq(rpath_sticky,
        rpath_sticky || rpath_sig_shifted.tail(17).orR,
        rpath_sticky || rpath_sig_shifted.tail(12).orR,
        rpath_sticky || rpath_sig_shifted.tail(9).orR)
    )
    rpath_rounder.io.signIn := raw_in.sign
    rpath_rounder.io.rm := rm

    val out = Mux(rpath_rounder.io.r_up, rpath_rounder.io.in + 1.U, rpath_rounder.io.in)
    val cout = rpath_rounder.io.r_up && Mux1H(
      Seq(fp32toint32 || fp16toint32,
        fp32toint16 || fp16toint16,
        fp16toint8),
      Seq(rpath_rounder.io.in.andR,
        rpath_rounder.io.in.tail(9).andR,
        rpath_rounder.io.in.tail(17).andR))

    val rpath_sig = Mux1H(
      Seq(fp32toint32,
        fp32toint16,
        fp16toint16 || fp16toint32,
        fp16toint8),
      Seq(Cat(0.U((32 - f32.precision - 1).W), cout, out),
        Cat(0.U(8.W), out),
        Cat(0.U((32 - f16.precision - 1).W), cout, out(10,0)),
        Cat(0.U(21.W), out(10,0)))
    )

    val rpath_ix = rpath_rounder.io.inexact || (fp16toint8 && rpath_sig_shifted.tail(8).orR)
    val rpath_iv = !is_signed_int && raw_in.sign && rpath_sig.orR

    val rpath_pos_of = !raw_in.sign &&
      Mux(is_signed_int,
        Mux1H(
          Seq(fp32toint16,
            fp16toint8),
          Seq((raw_in.exp === 142.U) || (raw_in.exp === 141.U) && cout,
            (raw_in.exp === 22.U) || (raw_in.exp === 21.U) && cout)
        ),
        Mux1H(
          Seq(fp32toint16,
            fp16toint8),
          Seq((raw_in.exp === 142.U) && cout,
            (raw_in.exp === 22.U) && cout)
        )
      )

    val rpath_neg_of = raw_in.sign && Mux1H(
      Seq(fp32toint16,
        fp16toint8),
      Seq(raw_in.exp === 142.U && (rpath_rounder.io.in.tail(8).orR || rpath_rounder.io.r_up),
        raw_in.exp === 22.U && (rpath_rounder.io.in.tail(17).orR || rpath_rounder.io.r_up))
    )
    val rpath_of = is_narrow && (rpath_pos_of || rpath_neg_of) || !is_narrow && cout

    val sel_lpath = raw_in.exp >= Mux1H(
      Seq(in_is_fp32,
        !in_is_fp32),
      Seq((VectorFloat.expBias(f32.expWidth) + f32.fracWidth).U,
        (VectorFloat.expBias(f16.expWidth) + f16.fracWidth).U)
    )

    val of = exp_of || sel_lpath && lpath_of || !sel_lpath && rpath_of
    val iv = of || sel_lpath && lpath_iv || !sel_lpath && rpath_iv
    val ix = !iv && !sel_lpath && rpath_ix

    val int_abs = Mux(sel_lpath, lpath_sig_shifted, rpath_sig)
    val sign = raw_in.sign && is_signed_int
    val raw_int = Mux1H(
      Seq(fp32toint16 || fp16toint16,
        fp32toint32 || fp16toint32,
        fp16toint8),
      Seq(int_abs.tail(16),
        int_abs,
        int_abs.tail(24))
    )
    val int = Mux(sign, -raw_int, raw_int)

    val max_int = Mux1H(
      Seq(fp32toint32 || fp16toint32,
        fp32toint16 || fp16toint16,
        fp16toint8),
      Seq(Cat(!is_signed_int, ~0.U(31.W)),
        Cat(!is_signed_int,   ~0.U(15.W)),
        Cat(!is_signed_int,   ~0.U(7.W)))
    )
    val min_int = Mux1H(
      Seq(fp32toint32 || fp16toint32,
        fp32toint16 || fp16toint16,
        fp16toint8),
      Seq(Cat(is_signed_int, 0.U(31.W)),
        Cat(is_signed_int,   0.U(15.W)),
        Cat(is_signed_int,   0.U(7.W)))
    )
    val sign_or_nan = Mux1H(
      Seq(fp32toint32 || fp32toint16,
        fp16toint32 || fp16toint16 || fp16toint8),
      Seq(in.decode.isNaN,
        src.tail(4).head(f16.expWidth).andR && src.tail(9).orR)
    ) | !raw_in.sign
    result := Mux(iv, Mux(sign_or_nan, max_int, min_int), int)
    fflags := Cat(iv, false.B, false.B, false.B, ix)
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
        val out_is_fp32 = (is_sew_32 && is_single) || (is_sew_16 && is_widen)
        val int32tofp32 = out_is_fp32 && is_single
        val int16tofp32 = out_is_fp32 && is_widen
        val int32tofp16 = !out_is_fp32 && is_narrow
        val int16tofp16 = !out_is_fp32 && is_single
        val int8tofp16 = !out_is_fp32 && is_widen
        val sign = is_signed_int && Mux1H(
        Seq(int32tofp32 || int32tofp16,
            int16tofp32 || int16tofp16,
            int8tofp16),
        Seq(io.src.head(1).asBool,
            io.src.tail(16).head(1).asBool,
            io.src.tail(24).head(1).asBool)
        )
        val in_sext = Mux1H(
        Seq(int32tofp32 || int32tofp16,
            int16tofp32 || int16tofp16,
            int8tofp16),
        Seq(io.src,
            Cat(Fill(16, io.src(15)), io.src(15,0)),
            Cat(Fill(24, io.src(7)), io.src(7,0)))
        )
        val in = Mux1H(
        Seq(is_signed_int && (int16tofp32 || int8tofp16 || int16tofp16),
            out_is_fp32 && !(is_signed_int && int16tofp32) || !out_is_fp32 && !(is_signed_int && (int8tofp16 || int16tofp16))),
        Seq(in_sext,
            io.src)
        )
        val in_abs = Mux(sign, (~in).asUInt + 1.U, in)

        val exp_of = int32tofp16 && in_abs.head(16).orR

        val lzc = Mux1H(
        Seq(out_is_fp32 || exp_of,
            int32tofp16 && !in_abs.head(16).orR || int16tofp16 || int8tofp16,
        ),
        Seq(CLZ(in_abs),
            CLZ(in_abs(15,0)))
        )

        val in_shift = Mux1H(
        Seq(out_is_fp32 || exp_of,
            int32tofp16 && !in_abs.head(16).orR || int16tofp16 || int8tofp16
        ),
        Seq((in_abs << lzc)(30, 0),
            Cat((in_abs << lzc)(14,0), 0.U(16.W)))
        )
        val exp_raw = Mux1H(
        Seq(out_is_fp32,
            !out_is_fp32),
        Seq(VectorFloat.expBias(f32.expWidth).asUInt +& 31.U - lzc,
            VectorFloat.expBias(f16.expWidth).asUInt +& 15.U - lzc)
        )

        val sig_raw = Mux1H(
        Seq(int16tofp32,
            int32tofp32,
            int8tofp16,
            int16tofp16 || int32tofp16),
        Seq(in_shift.head(16),
            in_shift.head(23),
            in_shift.head(8),
            in_shift.head(10))
        )
        val round_bit = Mux1H(
        Seq(int16tofp32,
            int32tofp32,
            int8tofp16,
            int16tofp16 || int32tofp16),
        Seq(in_shift.tail(16).head(1),
            in_shift.tail(23).head(1),
            in_shift.tail(8).head(1),
            in_shift.tail(10).head(1))
        )
        val sticky_bit = Mux1H(
        Seq(int16tofp32,
            int32tofp32,
            int8tofp16,
            int16tofp16 || int32tofp16),
        Seq(in_shift.tail(16).orR,
            in_shift.tail(f32.precision).orR,
            in_shift.tail(8).orR,
            in_shift.tail(f16.precision).orR)
        )
        val rounder = Module(new RoundingUnit(f32.fracWidth))
        rounder.io.in := sig_raw
        rounder.io.roundIn := round_bit
        rounder.io.stickyIn := sticky_bit
        rounder.io.signIn := sign
        rounder.io.rm := io.rm

        val out = Mux(rounder.io.r_up, rounder.io.in + 1.U, rounder.io.in)
        val cout = rounder.io.r_up && Mux1H(
        Seq(int16tofp32,
            int32tofp32,
            int8tofp16,
            int16tofp16 || int32tofp16),
        Seq(rounder.io.in.tail(7).andR,
            rounder.io.in.andR,
            rounder.io.in.tail(15).andR,
            rounder.io.in.tail(13).andR))

        val exp = Mux(in === 0.U, 0.U, exp_raw + cout)
        val sig = out

        val of = Mux1H(
        Seq(out_is_fp32,
            int32tofp16,
            !out_is_fp32 && !int32tofp16),
        Seq(exp === 255.U,
            exp_of || exp === 31.U,
            exp === 31.U)
        )
        val ix = Mux1H(
        Seq(out_is_fp32 || int16tofp16 || int8tofp16,
            int32tofp16),
        Seq(rounder.io.inexact,
            exp_of || rounder.io.inexact)
        )

        val result_fp = Cat(sign, Mux(RoundingUnit.is_rmin(io.rm, sign), Cat(Fill(4, 1.U), 0.U, Fill(10, 1.U)), Cat(Fill(5, 1.U), Fill(10, 0.U))))

        result := Mux1H(
        Seq(int32tofp32,
            int16tofp32,
            int32tofp16 && of,
            int8tofp16,
            int32tofp16 && !of || int16tofp16),
        Seq(Cat(sign, exp, sig),
            Cat(sign, exp, sig.tail(7), 0.U(7.W)),
            result_fp,
            Cat(sign, exp(4,0), sig(7,0), 0.U(2.W)),
            Cat(sign, exp(4,0), sig(9,0)))
        )
        fflags := Cat(false.B, false.B, of, false.B, ix)
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
