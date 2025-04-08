package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.{VfaddOpCode, VectorElementFormat}

  /*
   * new change: bf16
   * use format 2'b00
   * for 64 bit input, implement 4 bf16 adder
   * 
   * pass pytorch bf16 test
   * operation tested: 
   * vfadd
   * vfsub
   * vfmax
   * vfmin
   * VFEQ
   * VFNE
   * VFLT
   * VFLE
   * VFGT
   * VFGE
   * VFSGNJ 
   * VFSGNJN
   * VFSGNJX
   * other operation not tested
   * 
   */

class VectorBrainFloatAdder() extends Module {
  val VLEN = 128
  val exponentWidth = 11
  val significandWidth = 53
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire          = Input (Bool())
    val fp_a, fp_b    = Input (UInt(floatWidth.W)) // fp_a -> vs2, fp_b -> vs1
    val widen_a       = Input (UInt(floatWidth.W)) // widen_a -> Cat(vs2(95,64),vs2(31,0)) or Cat(vs2(127,96),vs2(63,32))
    val widen_b       = Input (UInt(floatWidth.W)) // widen_b -> Cat(vs1(95,64),vs1(31,0)) or Cat(vs1(127,96),vs1(63,32))
    val frs1          = Input (UInt(floatWidth.W)) // VS1(63,0)
    val is_frs1       = Input (Bool()) // VS1()
    val mask          = Input (UInt(4.W))
    val uop_idx       = Input (Bool())
    val is_vec        = Input (Bool())
    val round_mode    = Input (UInt(3.W))
    val fp_format     = Input (VectorElementFormat()) // b00->bf16 still under consideration
    val opb_widening  = Input (Bool())    // true -> opb widening
    val res_widening  = Input (Bool())    // true -> widening operation
    val op_code       = Input (UInt(5.W))
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())
    val maskForReduction = Input(UInt(8.W))
    val is_vfwredosum = Input (Bool()) // true -> vfwredosum inst
    val is_fold       = Input (UInt(3.W))
    val vs2_fold      = Input (UInt(VLEN.W))

    val fp_result     = Output(UInt(floatWidth.W))
    val fflags        = Output(UInt(20.W))
  })
  // TODO change fp_format is_vec logic
  // assert(io.fp_format=/=0.U) // TODO: add valid to enable assert
  val fire = io.fire

  val hasMinMaxCompare = true
  val fast_is_sub = io.op_code(0)

  // bf16
  val bf16_0_fp_a = io.fp_a(15, 0)
  val bf16_1_fp_a = io.fp_a(31, 16)
  val bf16_2_fp_a = io.fp_a(47, 32)
  val bf16_3_fp_a = io.fp_a(63, 48)
  val bf16_0_fp_b = io.fp_b(15, 0)
  val bf16_1_fp_b = io.fp_b(31, 16)
  val bf16_2_fp_b = io.fp_b(47, 32)
  val bf16_3_fp_b = io.fp_b(63, 48)

  val U_BF16_0 = Module(new FloatAdderBF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_BF16_0.io.fire := fire
  U_BF16_0.io.fp_a := bf16_0_fp_a
  U_BF16_0.io.fp_b := Mux(io.is_frs1,io.frs1(15,0),bf16_0_fp_b)
  U_BF16_0.io.mask := io.mask(0)
  U_BF16_0.io.is_sub := fast_is_sub
  U_BF16_0.io.round_mode := io.round_mode
  U_BF16_0.io.op_code    := io.op_code
  U_BF16_0.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_BF16_0.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_BF16_0.io.maskForReduction := Cat(io.maskForReduction(4), io.maskForReduction(0))
  val U_BF16_0_result = U_BF16_0.io.fp_c
  val U_BF16_0_fflags = U_BF16_0.io.fflags

  val U_BF16_1 = Module(new FloatAdderBF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_BF16_1.io.fire := fire
  U_BF16_1.io.fp_a := bf16_1_fp_a
  U_BF16_1.io.fp_b := Mux(io.is_frs1,io.frs1(15,0),bf16_1_fp_b)
  U_BF16_1.io.mask := io.mask(1)
  U_BF16_1.io.is_sub := fast_is_sub
  U_BF16_1.io.round_mode := io.round_mode
  U_BF16_1.io.op_code    := io.op_code
  U_BF16_1.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_BF16_1.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_BF16_1.io.maskForReduction := Cat(io.maskForReduction(5), io.maskForReduction(1))
  val U_BF16_1_result = U_BF16_1.io.fp_c
  val U_BF16_1_fflags = U_BF16_1.io.fflags

  val U_BF16_2 = Module(new FloatAdderBF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_BF16_2.io.fire := fire
  U_BF16_2.io.fp_a := bf16_2_fp_a
  U_BF16_2.io.fp_b := Mux(io.is_frs1,io.frs1(15,0),bf16_2_fp_b)
  U_BF16_2.io.mask := io.mask(2)
  U_BF16_2.io.is_sub := fast_is_sub
  U_BF16_2.io.round_mode := io.round_mode
  U_BF16_2.io.op_code    := io.op_code
  U_BF16_2.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_BF16_2.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_BF16_2.io.maskForReduction := Cat(io.maskForReduction(6), io.maskForReduction(2))
  val U_BF16_2_result = U_BF16_2.io.fp_c
  val U_BF16_2_fflags = U_BF16_2.io.fflags

  val U_BF16_3 = Module(new FloatAdderBF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_BF16_3.io.fire := fire
  U_BF16_3.io.fp_a := bf16_3_fp_a
  U_BF16_3.io.fp_b := Mux(io.is_frs1,io.frs1(15,0),bf16_3_fp_b)
  U_BF16_3.io.mask := io.mask(3)
  U_BF16_3.io.is_sub := fast_is_sub
  U_BF16_3.io.round_mode := io.round_mode
  U_BF16_3.io.op_code    := io.op_code
  U_BF16_3.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_BF16_3.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_BF16_3.io.maskForReduction := Cat(io.maskForReduction(7), io.maskForReduction(3))
  val U_BF16_3_result = U_BF16_3.io.fp_c
  val U_BF16_3_fflags = U_BF16_3.io.fflags
  
  val is_vec_reg = RegEnable(io.is_vec, fire)
  val fp_format_reg = RegEnable(io.fp_format, fire)
  val res_is_f16  = fp_format_reg === 1.U
  val res_is_f32  = fp_format_reg === 2.U
  val res_is_f64  = fp_format_reg === 3.U
  val res_is_bf16 = fp_format_reg === 0.U

  val bf16_result   = Cat(
    Fill(16, is_vec_reg) & U_BF16_3_result,
    Fill(16, is_vec_reg) & U_BF16_2_result,
    Fill(16, is_vec_reg) & U_BF16_1_result,
    U_BF16_0_result
  )

  io.fp_result := Mux1H(
    Seq(
      res_is_bf16
    ),
    Seq(
      bf16_result
    )
  )
  val bf16_fflags   = Cat(Fill(5, is_vec_reg) & U_BF16_3_fflags, Fill(5, is_vec_reg) & U_BF16_2_fflags, Fill(5, is_vec_reg) & U_BF16_1_fflags, U_BF16_0_fflags)

  io.fflags := Mux1H(
    Seq(
      res_is_bf16
    ),
    Seq(
      bf16_fflags
    )
  )
  
}

class FloatAdderBF16Pipeline(val is_print:Boolean = false,val hasMinMaxCompare:Boolean = false) extends Module {
  val exponentWidth = 8
  val significandWidth = 8
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire        = Input (Bool())
    val fp_a, fp_b  = Input (UInt(floatWidth.W))
    val fp_c        = Output(UInt(floatWidth.W))
    val is_sub      = Input (Bool())
    val mask        = Input (Bool())
    val round_mode  = Input (UInt(3.W))
    val fflags      = Output(UInt(5.W))
    val op_code     = if (hasMinMaxCompare) Input(UInt(5.W)) else Input(UInt(0.W))
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())
    val maskForReduction = Input(UInt(2.W))
  })
  val fire = io.fire
  val EOP = (io.fp_a.head(1) ^ io.is_sub ^ io.fp_b.head(1)).asBool
  val U_far_path = Module(new FarPathF16Pipeline(exponentWidth = exponentWidth,significandWidth = significandWidth, is_print = is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_far_path.io.fire := fire
  U_far_path.io.fp_a := io.fp_a
  U_far_path.io.fp_b := io.fp_b
  U_far_path.io.is_sub := io.is_sub
  U_far_path.io.round_mode := io.round_mode
  val U_close_path = Module(new ClosePathF16Pipeline(exponentWidth = exponentWidth,significandWidth = significandWidth, is_print = is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_close_path.io.fire := fire
  U_close_path.io.fp_a := io.fp_a
  U_close_path.io.fp_b := io.fp_b
  U_close_path.io.round_mode := io.round_mode
  val Efp_a = io.fp_a(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_b = io.fp_b(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_a_is_not_zero  = Efp_a.orR
  val Efp_b_is_not_zero  = Efp_b.orR
  val Efp_a_is_zero      = !Efp_a_is_not_zero
  val Efp_b_is_zero      = !Efp_b_is_not_zero
  val Efp_a_is_all_one   = Efp_a.andR
  val Efp_b_is_all_one   = Efp_b.andR
  val absEaSubEb = U_far_path.io.absEaSubEb
  val is_far_path     = !EOP | absEaSubEb(absEaSubEb.getWidth - 1, 1).orR | (absEaSubEb === 1.U & (Efp_a_is_not_zero ^ Efp_b_is_not_zero))
  val is_close_path   =  EOP & (!absEaSubEb(absEaSubEb.getWidth - 1, 1).orR)
  val fp_a_mantissa            = io.fp_a.tail(1 + exponentWidth)
  val fp_b_mantissa            = io.fp_b.tail(1 + exponentWidth)
  val fp_a_mantissa_isnot_zero = io.fp_a.tail(1 + exponentWidth).orR
  val fp_b_mantissa_isnot_zero = io.fp_b.tail(1 + exponentWidth).orR
  val fp_a_is_NAN        = io.fp_aIsFpCanonicalNAN | Efp_a_is_all_one & fp_a_mantissa_isnot_zero
  val fp_a_is_SNAN       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & fp_a_mantissa_isnot_zero & !io.fp_a(significandWidth-2)
  val fp_b_is_NAN        = io.fp_bIsFpCanonicalNAN | Efp_b_is_all_one & fp_b_mantissa_isnot_zero
  val fp_b_is_SNAN       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & fp_b_mantissa_isnot_zero & !io.fp_b(significandWidth-2)
  val fp_a_is_infinite   = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & (!fp_a_mantissa_isnot_zero)
  val fp_b_is_infinite   = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & (!fp_b_mantissa_isnot_zero)
  val float_adder_fflags = Wire(UInt(5.W))
  val float_adder_result = Wire(UInt(floatWidth.W))
  when(RegEnable((fp_a_is_SNAN | fp_b_is_SNAN) | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire)){
    float_adder_fflags := "b10000".U
  }.elsewhen(RegEnable(fp_a_is_NAN | fp_b_is_NAN | fp_a_is_infinite | fp_b_is_infinite, fire)){
    float_adder_fflags := "b00000".U
  }.otherwise{
    float_adder_fflags := Mux(RegEnable(is_far_path, fire),U_far_path.io.fflags,U_close_path.io.fflags)
  }
  when(RegEnable(fp_a_is_NAN | fp_b_is_NAN | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire)){

    float_adder_result := Cat(0.U,Fill(exponentWidth,1.U),1.U,Fill(significandWidth-2,0.U))
  }.elsewhen(RegEnable(fp_a_is_infinite | fp_b_is_infinite, fire)) {
    float_adder_result := Cat(RegEnable(Mux(fp_a_is_infinite,io.fp_a.head(1),io.is_sub^io.fp_b.head(1)), fire),Fill(exponentWidth,1.U),Fill(significandWidth-1,0.U))
  }.otherwise{
    float_adder_result := Mux(RegEnable(is_far_path, fire),U_far_path.io.fp_c,U_close_path.io.fp_c)
  }
  if (hasMinMaxCompare) {
    val fp_a_is_zero = !io.fp_aIsFpCanonicalNAN & Efp_a_is_zero && !fp_a_mantissa_isnot_zero
    val fp_b_is_zero = !io.fp_bIsFpCanonicalNAN & Efp_b_is_zero && !fp_b_mantissa_isnot_zero
    val is_add = io.op_code === VfaddOpCode.fadd
    val is_sub = io.op_code === VfaddOpCode.fsub
    val is_min = io.op_code === VfaddOpCode.fmin
    val is_max = io.op_code === VfaddOpCode.fmax
    val is_feq = io.op_code === VfaddOpCode.feq
    val is_fne = io.op_code === VfaddOpCode.fne
    val is_flt = io.op_code === VfaddOpCode.flt
    val is_fle = io.op_code === VfaddOpCode.fle
    val is_fgt = io.op_code === VfaddOpCode.fgt
    val is_fge = io.op_code === VfaddOpCode.fge
    val is_fsgnj  = io.op_code === VfaddOpCode.fsgnj 
    val is_fsgnjn = io.op_code === VfaddOpCode.fsgnjn
    val is_fsgnjx = io.op_code === VfaddOpCode.fsgnjx
    val is_fclass = io.op_code === VfaddOpCode.fclass
    val is_fmerge = io.op_code === VfaddOpCode.fmerge
    val is_fmove  = (io.op_code === VfaddOpCode.fmove) || (io.op_code === VfaddOpCode.fmv_f_s) || (io.op_code === VfaddOpCode.fmv_s_f)
    val is_fsum_ure = io.op_code === VfaddOpCode.fsum_ure
    val is_fmin_re = io.op_code === VfaddOpCode.fmin_re
    val is_fmax_re = io.op_code === VfaddOpCode.fmax_re
    val is_fsum_ore = io.op_code === VfaddOpCode.fsum_ore
    val fp_a_sign = io.fp_a.head(1)
    val fp_b_sign = io.fp_b.head(1)
    val fp_b_sign_is_greater = fp_a_sign & !fp_b_sign
    val fp_b_sign_is_equal   = fp_a_sign === fp_b_sign
    val fp_b_sign_is_smaller = !fp_a_sign & fp_b_sign
    val fp_b_exponent_is_greater = U_far_path.io.isEfp_bGreater
    val fp_b_exponent_is_equal   = Efp_a === Efp_b
    val fp_b_exponent_is_smaller = !fp_b_exponent_is_greater & !fp_b_exponent_is_equal
    val fp_b_significand_is_greater = !U_close_path.io.CS1.head(1) & (fp_a_mantissa =/= fp_b_mantissa)
    val fp_b_significand_is_equal   = fp_a_mantissa === fp_b_mantissa
    val fp_b_significand_is_smaller = !fp_b_significand_is_greater & !fp_b_significand_is_equal
    val fp_b_is_greater = (!fp_b_sign & ((fp_a_sign & !(fp_b_is_zero & fp_a_is_zero)) | fp_b_exponent_is_greater | (fp_b_exponent_is_equal & fp_b_significand_is_greater))) |
                          (fp_b_sign & fp_a_sign & (fp_b_exponent_is_smaller | (fp_b_exponent_is_equal & fp_b_significand_is_smaller)))
    val fp_b_is_equal = (fp_b_sign_is_equal & fp_b_exponent_is_equal & fp_b_significand_is_equal) | (fp_b_is_zero & fp_a_is_zero)
    val fp_b_is_less = !fp_b_is_greater & !fp_b_is_equal
    val result_min = Wire(UInt(floatWidth.W))
    val result_max = Wire(UInt(floatWidth.W))
    val result_feq = Wire(UInt(floatWidth.W))
    val result_fne = Wire(UInt(floatWidth.W))
    val result_flt = Wire(UInt(floatWidth.W))
    val result_fle = Wire(UInt(floatWidth.W))
    val result_fgt = Wire(UInt(floatWidth.W))
    val result_fge = Wire(UInt(floatWidth.W))
    val in_NAN = Cat(0.U,Fill(exponentWidth,1.U),1.U,Fill(significandWidth-2,0.U))
    val fp_aFix = Mux(io.fp_aIsFpCanonicalNAN, in_NAN, io.fp_a)
    val fp_bFix = Mux(io.fp_bIsFpCanonicalNAN, in_NAN, io.fp_b)
    val result_fsgnj  = Cat(fp_bFix.head(1), fp_aFix.tail(1))
    val result_fsgnjn = Cat(~fp_bFix.head(1), fp_aFix.tail(1))
    val result_fsgnjx = Cat(fp_bFix.head(1) ^ fp_aFix.head(1), fp_aFix.tail(1))
    val result_fclass = Wire(UInt(floatWidth.W))
    val result_fmerge = Mux(io.mask, fp_bFix, fp_aFix)
    val result_fmove  = fp_bFix
    val out_NAN = Cat(0.U,Fill(exponentWidth,1.U),1.U,Fill(significandWidth-2,0.U))
    val out_Nzero = Cat(Mux(io.round_mode ==="b010".U, 0.U, 1.U), Fill(floatWidth - 1, 0.U))
    result_min := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN &  fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN &  fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_less || (fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),io.fp_b,io.fp_a),
        io.fp_a,
        io.fp_b,
        out_NAN
      )
    )
    result_max := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN &  fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN &  fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_greater.asBool || (!fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),io.fp_b,io.fp_a),
        io.fp_a,
        io.fp_b,
        out_NAN
      )
    )
    result_feq := Mux(fp_a_is_NAN | fp_b_is_NAN,0.U,fp_b_is_equal)
    result_fne := !result_feq
    result_flt := Mux(fp_a_is_NAN | fp_b_is_NAN,0.U,fp_b_is_greater)
    result_fle := Mux(fp_a_is_NAN | fp_b_is_NAN,0.U,fp_b_is_greater | fp_b_is_equal)
    result_fgt := Mux(fp_a_is_NAN | fp_b_is_NAN,0.U,fp_b_is_less)
    result_fge := Mux(fp_a_is_NAN | fp_b_is_NAN,0.U,fp_b_is_less | fp_b_is_equal)
    result_fclass := Mux(io.fp_aIsFpCanonicalNAN, (1 << 9).U, Reverse(Cat(
      fp_a_sign & fp_a_is_infinite,
      fp_a_sign & !Efp_a_is_zero & !Efp_a_is_all_one,
      fp_a_sign & Efp_a_is_zero & fp_a_mantissa_isnot_zero,
      fp_a_sign & Efp_a_is_zero & !fp_a_mantissa_isnot_zero,
      ~fp_a_sign & Efp_a_is_zero & !fp_a_mantissa_isnot_zero,
      ~fp_a_sign & Efp_a_is_zero & fp_a_mantissa_isnot_zero,
      ~fp_a_sign & !Efp_a_is_zero & !Efp_a_is_all_one,
      ~fp_a_sign & fp_a_is_infinite,
      fp_a_is_SNAN,
      fp_a_is_NAN & !fp_a_is_SNAN
    )))
    val is_fsum_ure_notmasked = is_fsum_ure && io.maskForReduction.andR
    val is_fsum_ure_masked = is_fsum_ure && !io.maskForReduction.andR
    val is_fsum_ore_notmasked = is_fsum_ore && io.maskForReduction(0)
    val is_fsum_ore_masked = is_fsum_ore && !io.maskForReduction(0)
    val result_fsum_ure_masked = Mux(
      io.maskForReduction === 0.U,
      out_Nzero,
      Mux(io.maskForReduction(0), io.fp_a, io.fp_b)
    )
    val result_fsum_ore_masked = Mux(
      io.maskForReduction(0) === 0.U,
      io.fp_b,
      0.U(floatWidth.W)
    )
    val outInf = Cat(is_fmax_re, Fill(exponentWidth, 1.U), 0.U((significandWidth-1).W))
    val re_masked_one_out = Mux(
      io.maskForReduction(0),
      io.fp_a,
      io.fp_b
    )
    val result_fmax_re = Mux(
      io.maskForReduction === 0.U,
      out_NAN,
      Mux(io.maskForReduction.andR, result_max, re_masked_one_out)
    )
    val result_fmin_re = Mux(
      io.maskForReduction === 0.U,
      out_NAN,
      Mux(io.maskForReduction.andR, result_min, re_masked_one_out)
    )
    val result_stage0 = Mux1H(
      Seq(
        is_min,
        is_max,
        is_feq,
        is_fne,
        is_flt,
        is_fle,
        is_fgt,
        is_fge,
        is_fsgnj,
        is_fsgnjn,
        is_fsgnjx,
        is_fclass,
        is_fmerge,
        is_fmove,
        is_fsum_ure_masked,
        is_fmax_re,
        is_fmin_re,
        is_fsum_ore_masked,
      ),
      Seq(
        result_min,
        result_max,
        result_feq,
        result_fne,
        result_flt,
        result_fle,
        result_fgt,
        result_fge,
        result_fsgnj,
        result_fsgnjn,
        result_fsgnjx,
        result_fclass,
        result_fmerge,
        result_fmove,
        result_fsum_ure_masked,
        result_fmax_re,
        result_fmin_re,
        result_fsum_ore_masked,
      )
    )
    val fflags_NV_stage0 = ((is_min | is_max) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_feq | is_fne) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_flt | is_fle | is_fgt | is_fge) & (fp_a_is_NAN | fp_b_is_NAN)) |
      ((is_fmax_re | is_fmin_re) & ((io.maskForReduction(0) & fp_a_is_SNAN) | (io.maskForReduction(1) & fp_b_is_SNAN)))
    val fflags_stage0 = Cat(fflags_NV_stage0, 0.U(4.W))
    io.fp_c := Mux(RegEnable(is_add | is_sub | is_fsum_ure_notmasked | is_fsum_ore_notmasked, fire), float_adder_result, RegEnable(result_stage0, fire))
    io.fflags := Mux(RegEnable(is_add | is_sub | is_fsum_ure_notmasked | is_fsum_ore_notmasked, fire), float_adder_fflags, RegEnable(fflags_stage0, fire))
  }
  else {
    io.fp_c := float_adder_result
    io.fflags := float_adder_fflags
  }
}