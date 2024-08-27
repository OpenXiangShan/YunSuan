package yunsuan.fpu
import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.{FaddOpCode, VectorElementFormat}
class FloatAdder() extends Module  {
  val exponentWidth = 11
  val significandWidth = 53
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire          = Input (Bool())
    val fp_a, fp_b    = Input (UInt(floatWidth.W)) // fp_a -> vs2, fp_b -> vs1
    val round_mode    = Input (UInt(3.W))
    val fp_format     = Input (VectorElementFormat()) // result format b01->fp16,b10->fp32,b11->fp64
    val op_code       = Input (UInt(5.W))
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())
    val fp_result     = Output(UInt(floatWidth.W))
    val fflags        = Output(UInt(5.W))
  })
  val fp_format = io.fp_format-1.U //Cat(io.fp_format===3.U,io.fp_format(1))
  val fire = io.fire
  val hasMinMaxCompare = true
  val fast_is_sub = io.op_code(0)

  val f64_fp_a = Wire(UInt(floatWidth.W))
  val f32_fp_a = Wire(UInt(floatWidth.W))
  val f16_fp_a = Wire(UInt(floatWidth.W))

  val f64_fp_b = Wire(UInt(floatWidth.W))
  val f32_fp_b = Wire(UInt(floatWidth.W))
  val f16_fp_b = Wire(UInt(floatWidth.W))

  f64_fp_a := io.fp_a
  f32_fp_a := io.fp_a(31, 0)
  f16_fp_a := io.fp_a(15, 0)

  f64_fp_b := io.fp_b
  f32_fp_b := io.fp_b(31, 0)
  f16_fp_b := io.fp_b(15, 0)

  val F64Adder = Module(new FloatAdderF64Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  F64Adder.io.fire := fire
  F64Adder.io.fp_a := f64_fp_a
  F64Adder.io.fp_b := f64_fp_b
  F64Adder.io.is_sub := fast_is_sub
  F64Adder.io.round_mode := io.round_mode
  F64Adder.io.op_code := io.op_code
  F64Adder.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  F64Adder.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN

  val F64_result = F64Adder.io.fp_c
  val F64_fflags = F64Adder.io.fflags

  val F32Adder = Module(new FloatAdderF32F16MixedPipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  F32Adder.io.fire := fire
  F32Adder.io.fp_a := f32_fp_a
  F32Adder.io.fp_b := f32_fp_b
  F32Adder.io.is_sub := fast_is_sub
  F32Adder.io.round_mode := io.round_mode
  F32Adder.io.op_code := io.op_code
  F32Adder.io.fp_format := fp_format
  F32Adder.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  F32Adder.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN

  val F32_result = F32Adder.io.fp_c
  val F32_fflags = F32Adder.io.fflags
  val F16_result = F32Adder.io.fp_c(15,0)
  val F16_fflags = F32Adder.io.fflags

  val fp_format_reg = RegEnable(io.fp_format, fire)
  val res_is_f16 = fp_format_reg === 1.U
  val res_is_f32 = fp_format_reg === 2.U
  val res_is_f64 = fp_format_reg === 3.U
  val is_add    = io.op_code === FaddOpCode.fadd
  val is_sub    = io.op_code === FaddOpCode.fsub
  val is_min    = io.op_code === FaddOpCode.fmin
  val is_max    = io.op_code === FaddOpCode.fmax
  val is_fsgnj  = io.op_code === FaddOpCode.fsgnj
  val is_fsgnjn = io.op_code === FaddOpCode.fsgnjn
  val is_fsgnjx = io.op_code === FaddOpCode.fsgnjx
  val is_feq    = io.op_code === FaddOpCode.feq
  val is_flt    = io.op_code === FaddOpCode.flt
  val is_fle    = io.op_code === FaddOpCode.fle
  val is_fclass = io.op_code === FaddOpCode.fclass
  val is_fminm  = io.op_code === FaddOpCode.fminm
  val is_fmaxm  = io.op_code === FaddOpCode.fmaxm
  val resultNeedBox = RegEnable(is_add || is_sub || is_min || is_max || is_fsgnj || is_fsgnjn || is_fsgnjx || is_fminm || is_fmaxm, fire)
  val fp_f64_result = F64_result
  val fp_f32_result = Cat(Fill(32, resultNeedBox), F32_result)
  val fp_f16_result = Cat(Fill(48, resultNeedBox), F16_result)

  io.fp_result := Mux1H(
    Seq(
      res_is_f16,
      res_is_f32,
      res_is_f64
    ),
    Seq(
      fp_f16_result,
      fp_f32_result,
      fp_f64_result
    )
  )
  io.fflags := Mux1H(
    Seq(
      res_is_f16,
      res_is_f32,
      res_is_f64
    ),
    Seq(
      F16_fflags,
      F32_fflags,
      F64_fflags
    )
  )

}
private[fpu] class FloatAdderF32F16MixedPipeline(val is_print:Boolean = false,val hasMinMaxCompare:Boolean = false) extends Module {
  val exponentWidth = 8
  val significandWidth = 24
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire         = Input (Bool())
    val fp_a, fp_b   = Input (UInt(floatWidth.W))
    val fp_c         = Output(UInt(floatWidth.W))
    val is_sub       = Input (Bool())
    val round_mode   = Input (UInt(3.W))
    val fflags       = Output(UInt(5.W))
    val fp_format    = Input (UInt(2.W))
    val op_code = if (hasMinMaxCompare) Input(UInt(5.W)) else Input(UInt(0.W))
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())
  })
  val fire = io.fire
  val res_is_f32 = io.fp_format(0).asBool
  val fp_a_16as32 = Cat(io.fp_a(15), Cat(0.U(3.W),io.fp_a(14,10)), Cat(io.fp_a(9,0),0.U(13.W)))
  val fp_b_16as32 = Cat(io.fp_b(15), Cat(0.U(3.W),io.fp_b(14,10)), Cat(io.fp_b(9,0),0.U(13.W)))

  val fp_a_to32 = Mux(res_is_f32,io.fp_a,fp_a_16as32)

  val fp_b_to32 = Mux(res_is_f32,io.fp_b,fp_b_16as32)

  val EOP = (fp_a_to32.head(1) ^ io.is_sub ^ fp_b_to32.head(1)).asBool
  val U_far_path = Module(new FarPathF32WidenF16MixedPipeline(is_print=is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_far_path.io.fire := fire
  U_far_path.io.fp_a := fp_a_to32
  U_far_path.io.fp_b := fp_b_to32
  U_far_path.io.is_sub := io.is_sub
  U_far_path.io.round_mode := io.round_mode
  U_far_path.io.res_is_f32 := res_is_f32
  val U_close_path = Module(new ClosePathF32WidenF16MixedPipeline(is_print=is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_close_path.io.fire := fire
  U_close_path.io.fp_a := fp_a_to32
  U_close_path.io.fp_b := fp_b_to32
  U_close_path.io.round_mode := io.round_mode
  U_close_path.res_is_f32 := res_is_f32
  val absEaSubEb = U_far_path.io.absEaSubEb
  val is_close_path   =  EOP & (!absEaSubEb(absEaSubEb.getWidth - 1, 1).orR)
  val fp_a_mantissa            = fp_a_to32.tail(1 + exponentWidth)
  val fp_b_mantissa            = fp_b_to32.tail(1 + exponentWidth)
  val fp_a_mantissa_isnot_zero = fp_a_to32.tail(1 + exponentWidth).orR
  val fp_b_mantissa_isnot_zero = fp_b_to32.tail(1 + exponentWidth).orR
  val fp_a_is_f16 = !res_is_f32
  val fp_b_is_f16 = !res_is_f32
  val Efp_a = fp_a_to32(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_b = fp_b_to32(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_a_is_zero  = !Efp_a.orR | (fp_a_is_f16 & Efp_a==="b01100110".U)
  val Efp_b_is_zero  = !Efp_b.orR | (fp_b_is_f16 & Efp_b==="b01100110".U)
  val Efp_a_is_all_one   = Mux(
    fp_a_is_f16,
     io.fp_a(14,10).andR,
    io.fp_a(30,23).andR
  )
  val Efp_b_is_all_one   = Mux(
    fp_b_is_f16,
    io.fp_b(14,10).andR,
    io.fp_b(30,23).andR
  )
  val fp_a_is_NAN        = io.fp_aIsFpCanonicalNAN | Efp_a_is_all_one & fp_a_mantissa_isnot_zero
  val fp_a_is_SNAN       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & fp_a_mantissa_isnot_zero & !fp_a_to32(significandWidth-2)
  val fp_a_is_QNAN       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & fp_a_mantissa_isnot_zero &  fp_a_to32(significandWidth-2)
  val fp_b_is_NAN        = io.fp_bIsFpCanonicalNAN | Efp_b_is_all_one & fp_b_mantissa_isnot_zero
  val fp_b_is_SNAN       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & fp_b_mantissa_isnot_zero & !fp_b_to32(significandWidth-2)
  val fp_b_is_QNAN       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & fp_b_mantissa_isnot_zero &  fp_b_to32(significandWidth-2)
  val fp_a_is_infinite   = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & (!fp_a_mantissa_isnot_zero)
  val fp_b_is_infinite   = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & (!fp_b_mantissa_isnot_zero)
  val fp_a_is_zero       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_zero & !fp_a_mantissa_isnot_zero
  val fp_b_is_zero       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_zero & !fp_b_mantissa_isnot_zero


  val is_far_path     = !EOP | (EOP & absEaSubEb(absEaSubEb.getWidth - 1, 1).orR) | (absEaSubEb === 1.U & (Efp_a_is_zero ^ Efp_b_is_zero))
  val is_far_path_reg = RegEnable(is_far_path, fire)
  val float_adder_fflags = Wire(UInt(5.W))
  val float_adder_result = Wire(UInt(floatWidth.W))
  when(RegEnable((fp_a_is_SNAN | fp_b_is_SNAN) | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire)){
    float_adder_fflags := "b10000".U
  }.elsewhen(RegEnable(fp_a_is_NAN | fp_b_is_NAN | fp_a_is_infinite | fp_b_is_infinite | (fp_b_is_zero | fp_a_is_zero), fire)){
    float_adder_fflags := "b00000".U
  }.otherwise{
    float_adder_fflags := Mux(is_far_path_reg,U_far_path.io.fflags,U_close_path.io.fflags)
  }
  val res_is_f32_reg = RegEnable(res_is_f32, fire)
  val out_NAN_reg = Mux(res_is_f32_reg, Cat(0.U,Fill(8,1.U),1.U,0.U(22.W)), Cat(0.U(17.W),Fill(5,1.U),1.U,0.U(9.W)))
  val out_infinite_sign = Mux(fp_a_is_infinite,fp_a_to32.head(1),io.is_sub^fp_b_to32.head(1))
  val out_infinite_sign_reg = RegEnable(out_infinite_sign, fire)
  val out_infinite_reg = Mux(res_is_f32_reg, Cat(out_infinite_sign_reg,Fill(8,1.U),0.U(23.W)), Cat(0.U(16.W),out_infinite_sign_reg,Fill(5,1.U),0.U(10.W)))
  val out_fp32_reg = Mux(is_far_path_reg,U_far_path.io.fp_c,U_close_path.io.fp_c)
  val out_fp32_to_fp16_or_fp32_reg = Mux(res_is_f32_reg, out_fp32_reg, Cat(0.U(16.W),out_fp32_reg(31),out_fp32_reg(27,23),out_fp32_reg(22,13)))
  when(RegEnable(fp_a_is_NAN | fp_b_is_NAN | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire)){
    float_adder_result := out_NAN_reg
  }.elsewhen(RegEnable(fp_a_is_infinite | fp_b_is_infinite, fire)) {
    float_adder_result := out_infinite_reg
  }.elsewhen(RegEnable(fp_a_is_zero & fp_b_is_zero, fire)){
    float_adder_result := Cat(RegEnable(Mux(io.round_mode==="b010".U & EOP | (fp_a_to32.head(1).asBool & !EOP),1.U,0.U), fire),0.U(31.W))
  }.elsewhen(RegEnable(fp_a_is_zero, fire)){
    float_adder_result := RegEnable(Cat(io.is_sub ^ fp_b_to32.head(1),fp_b_to32(30,0)), fire)
  }.elsewhen(RegEnable(fp_b_is_zero, fire)){
    float_adder_result := RegEnable(fp_a_to32, fire)
  }.otherwise{
    float_adder_result := out_fp32_to_fp16_or_fp32_reg
  }
  if (hasMinMaxCompare) {
    val is_add = io.op_code === FaddOpCode.fadd
    val is_sub = io.op_code === FaddOpCode.fsub
    val is_min = io.op_code === FaddOpCode.fmin
    val is_max = io.op_code === FaddOpCode.fmax
    val is_feq = io.op_code === FaddOpCode.feq
    val is_flt = io.op_code === FaddOpCode.flt
    val is_fle = io.op_code === FaddOpCode.fle
    val is_fsgnj  = io.op_code === FaddOpCode.fsgnj
    val is_fsgnjn = io.op_code === FaddOpCode.fsgnjn
    val is_fsgnjx = io.op_code === FaddOpCode.fsgnjx
    val is_fclass = io.op_code === FaddOpCode.fclass
    val is_fminm  = io.op_code === FaddOpCode.fminm
    val is_fmaxm  = io.op_code === FaddOpCode.fmaxm
    val is_fleq   = io.op_code === FaddOpCode.fleq
    val is_fltq   = io.op_code === FaddOpCode.fltq

    val fp_a_sign = fp_a_to32.head(1)
    val fp_b_sign = fp_b_to32.head(1)
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
    val result_flt = Wire(UInt(floatWidth.W))
    val result_fle = Wire(UInt(floatWidth.W))
    val result_fclass = Wire(UInt(floatWidth.W))
    val result_fminm = Wire(UInt(floatWidth.W))
    val result_fmaxm = Wire(UInt(floatWidth.W))
    val in_NAN = Mux(res_is_f32, Cat(0.U(1.W),Fill(9, 1.U(1.W)),0.U(22.W)), Cat(0.U(17.W),Fill(6, 1.U(1.W)),0.U(9.W)))
    val fp_aFix = Mux(io.fp_aIsFpCanonicalNAN, in_NAN, io.fp_a)
    val fp_bFix = Mux(io.fp_bIsFpCanonicalNAN, in_NAN, io.fp_b)
    val result_fsgnj  = Mux(res_is_f32, Cat(fp_bFix.head(1) , fp_aFix(30, 0)), Cat(0.U(16.W), Cat(fp_bFix(15) , fp_aFix(14, 0))))
    val result_fsgnjn = Mux(res_is_f32, Cat(~fp_bFix.head(1), fp_aFix(30, 0)), Cat(0.U(16.W), Cat(~fp_bFix(15), fp_aFix(14, 0))))
    val result_fsgnjx = Mux(
      res_is_f32,
      Cat(fp_bFix.head(1) ^ fp_aFix.head(1), fp_aFix(30, 0)),
      Cat(0.U(16.W), Cat(fp_bFix(15) ^ fp_aFix(15), fp_aFix(14, 0)))
    )


    val out_NAN = Mux(res_is_f32, Cat(0.U,Fill(8,1.U),1.U,0.U(22.W)), Cat(0.U(17.W),Fill(5,1.U),1.U,0.U(9.W)))
    val fp_a_16_or_32 = Mux(res_is_f32, fp_aFix(31, 0), Cat(0.U(16.W), fp_aFix(15, 0)))
    val fp_b_16_or_32 = Mux(res_is_f32, fp_bFix(31, 0), Cat(0.U(16.W), fp_bFix(15, 0)))
    result_min := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN & fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN & fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_less || (fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero), fp_b_16_or_32, fp_a_16_or_32),
        fp_a_16_or_32,
        fp_b_16_or_32,
        out_NAN
      )
    )
    result_max := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN & fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN & fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_greater.asBool || (!fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero), fp_b_16_or_32, fp_a_16_or_32),
        fp_a_16_or_32,
        fp_b_16_or_32,
        out_NAN
      )
    )
    result_feq := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_equal)
    result_flt := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_greater)
    result_fle := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_greater | fp_b_is_equal)
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
    result_fminm := Mux(!fp_a_is_NAN & !fp_b_is_NAN,
      Mux(fp_b_is_less || (fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),
        fp_b_16_or_32,
        fp_a_16_or_32),
      out_NAN
    )
    result_fmaxm := Mux(!fp_a_is_NAN & !fp_b_is_NAN,
      Mux(fp_b_is_greater.asBool || (!fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),
        fp_b_16_or_32,
        fp_a_16_or_32),
      out_NAN
    )

    val result_stage0 = Mux1H(
      Seq(
        is_min,
        is_max,
        is_feq,
        is_flt | is_fltq,
        is_fle | is_fleq,
        is_fsgnj,
        is_fsgnjn,
        is_fsgnjx,
        is_fclass,
        is_fminm,
        is_fmaxm,
      ),
      Seq(
        result_min,
        result_max,
        result_feq,
        result_flt,
        result_fle,
        result_fsgnj,
        result_fsgnjn,
        result_fsgnjx,
        result_fclass,
        result_fminm,
        result_fmaxm,
      )
    )
    val fflags_NV_stage0 = ((is_min | is_max) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_feq ) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_flt | is_fle ) & (fp_a_is_NAN | fp_b_is_NAN)) |
      ((is_fminm | is_fmaxm) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_fltq | is_fleq) & (fp_a_is_SNAN | fp_b_is_SNAN))
    val fflags_stage0 = Cat(fflags_NV_stage0,0.U(4.W))
    io.fp_c := Mux(RegEnable(is_add | is_sub , fire),float_adder_result,RegEnable(result_stage0, fire))
    io.fflags := Mux(RegEnable(is_add | is_sub , fire),float_adder_fflags,RegEnable(fflags_stage0, fire))
  }
  else {
    io.fp_c := float_adder_result
    io.fflags := float_adder_fflags
  }
}
private[fpu] class FloatAdderF64Pipeline(val is_print:Boolean = false,val hasMinMaxCompare:Boolean = false) extends Module {
  val exponentWidth = 11
  val significandWidth = 53
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire        = Input (Bool())
    val fp_a, fp_b  = Input (UInt(floatWidth.W))
    val fp_c        = Output(UInt(floatWidth.W))
    val is_sub      = Input (Bool())
    val round_mode  = Input (UInt(3.W))
    val fflags      = Output(UInt(5.W))
    val op_code = if (hasMinMaxCompare) Input(UInt(5.W)) else Input(UInt(0.W))
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())
  })
  val fire = io.fire
  val fp_a_to64 = io.fp_a
  val fp_b_to64 = io.fp_b

  val EOP = (fp_a_to64.head(1) ^ io.is_sub ^ fp_b_to64.head(1)).asBool
  val U_far_path = Module(new FarPathFloatAdderF64WidenPipeline(exponentWidth = exponentWidth,significandWidth = significandWidth, is_print = is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_far_path.io.fire := fire
  U_far_path.io.fp_a := fp_a_to64
  U_far_path.io.fp_b := fp_b_to64
  U_far_path.io.is_sub := io.is_sub
  U_far_path.io.round_mode := io.round_mode
  val U_close_path = Module(new ClosePathFloatAdderF64WidenPipeline(exponentWidth = exponentWidth,significandWidth = significandWidth, is_print = is_print, hasMinMaxCompare=hasMinMaxCompare))
  U_close_path.io.fire := fire
  U_close_path.io.fp_a := fp_a_to64
  U_close_path.io.fp_b := fp_b_to64
  U_close_path.io.round_mode := io.round_mode
  val absEaSubEb = U_far_path.io.absEaSubEb

  val fp_a_mantissa            = fp_a_to64.tail(1 + exponentWidth)
  val fp_b_mantissa            = fp_b_to64.tail(1 + exponentWidth)
  val fp_a_mantissa_isnot_zero = fp_a_to64.tail(1 + exponentWidth).orR
  val fp_b_mantissa_isnot_zero = fp_b_to64.tail(1 + exponentWidth).orR
  val Efp_a = fp_a_to64(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_b = fp_b_to64(floatWidth-2, floatWidth-1-exponentWidth)
  val Efp_a_is_zero  = !Efp_a.orR
  val Efp_b_is_zero  = !Efp_b.orR
  val Efp_a_is_all_one   = Efp_a.andR
  val Efp_b_is_all_one   = Efp_b.andR
  val fp_a_is_NAN        = io.fp_aIsFpCanonicalNAN | Efp_a_is_all_one & fp_a_mantissa_isnot_zero
  val fp_a_is_SNAN       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & fp_a_mantissa_isnot_zero & !fp_a_to64(significandWidth-2)
  val fp_a_is_QNAN       = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & fp_a_mantissa_isnot_zero &  fp_a_to64(significandWidth-2)
  val fp_b_is_NAN        = io.fp_bIsFpCanonicalNAN | Efp_b_is_all_one & fp_b_mantissa_isnot_zero
  val fp_b_is_SNAN       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & fp_b_mantissa_isnot_zero & !fp_b_to64(significandWidth-2)
  val fp_b_is_QNAN       = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & fp_b_mantissa_isnot_zero &  fp_b_to64(significandWidth-2)
  val fp_a_is_infinite   = !io.fp_aIsFpCanonicalNAN & Efp_a_is_all_one & (!fp_a_mantissa_isnot_zero)
  val fp_b_is_infinite   = !io.fp_bIsFpCanonicalNAN & Efp_b_is_all_one & (!fp_b_mantissa_isnot_zero)
  val fp_a_is_zero = !io.fp_aIsFpCanonicalNAN & Efp_a_is_zero & !fp_a_mantissa_isnot_zero
  val fp_b_is_zero = !io.fp_bIsFpCanonicalNAN & Efp_b_is_zero & !fp_b_mantissa_isnot_zero
  val fp_a_is_zero_reg   = RegEnable(fp_a_is_zero, fire)
  val fp_b_is_zero_reg   = RegEnable(fp_b_is_zero, fire)

  val is_far_path_reg     = RegEnable(!EOP | absEaSubEb(absEaSubEb.getWidth - 1, 1).orR | (absEaSubEb === 1.U & (Efp_a_is_zero ^ Efp_b_is_zero)), fire)
  val float_adder_fflags = Wire(UInt(5.W))
  val float_adder_result = Wire(UInt(floatWidth.W))
  when(RegEnable((fp_a_is_SNAN | fp_b_is_SNAN) | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire)){
    float_adder_fflags := "b10000".U
  }.elsewhen(RegEnable(fp_a_is_NAN | fp_b_is_NAN | fp_a_is_infinite | fp_b_is_infinite, fire) | (fp_b_is_zero_reg | fp_a_is_zero_reg) ){
    float_adder_fflags := "b00000".U
  }.otherwise{
    float_adder_fflags := Mux(is_far_path_reg,U_far_path.io.fflags,U_close_path.io.fflags)
  }

  when(RegEnable(fp_a_is_NAN | fp_b_is_NAN | (EOP & fp_a_is_infinite & fp_b_is_infinite), fire) ){
    float_adder_result := RegEnable(Cat(0.U,Fill(exponentWidth,1.U),1.U,Fill(significandWidth-2,0.U)), fire)
  }.elsewhen(RegEnable(fp_a_is_infinite | fp_b_is_infinite, fire)) {
    float_adder_result := RegEnable(Cat(Mux(fp_a_is_infinite,fp_a_to64.head(1),io.is_sub^fp_b_to64.head(1)), Fill(exponentWidth,1.U),Fill(significandWidth-1,0.U)), fire)
  }.elsewhen(fp_a_is_zero_reg & fp_b_is_zero_reg){
    float_adder_result := RegEnable(Cat(Mux(io.round_mode==="b010".U & EOP | (fp_a_to64.head(1).asBool & !EOP),1.U,0.U),0.U(63.W)), fire)
  }.elsewhen(fp_a_is_zero_reg){
    float_adder_result := RegEnable(Cat(io.is_sub ^ fp_b_to64.head(1),fp_b_to64(62,0)), fire)
  }.elsewhen(fp_b_is_zero_reg){
    float_adder_result := RegEnable(fp_a_to64, fire)
  }.otherwise{
    float_adder_result := Mux(is_far_path_reg,U_far_path.io.fp_c,U_close_path.io.fp_c)
  }
  if (hasMinMaxCompare) {
    val is_add = io.op_code === FaddOpCode.fadd
    val is_sub = io.op_code === FaddOpCode.fsub
    val is_min = io.op_code === FaddOpCode.fmin
    val is_max = io.op_code === FaddOpCode.fmax
    val is_feq = io.op_code === FaddOpCode.feq
    val is_flt = io.op_code === FaddOpCode.flt
    val is_fle = io.op_code === FaddOpCode.fle
    val is_fsgnj  = io.op_code === FaddOpCode.fsgnj
    val is_fsgnjn = io.op_code === FaddOpCode.fsgnjn
    val is_fsgnjx = io.op_code === FaddOpCode.fsgnjx
    val is_fclass = io.op_code === FaddOpCode.fclass
    val is_fminm = io.op_code === FaddOpCode.fminm
    val is_fmaxm = io.op_code === FaddOpCode.fmaxm
    val is_fleq = io.op_code === FaddOpCode.fleq
    val is_fltq = io.op_code === FaddOpCode.fltq
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
    val result_flt = Wire(UInt(floatWidth.W))
    val result_fle = Wire(UInt(floatWidth.W))
    val result_fminm = Wire(UInt(floatWidth.W))
    val result_fmaxm = Wire(UInt(floatWidth.W))
    val in_NAN = Cat(0.U, Fill(exponentWidth, 1.U), 1.U, Fill(significandWidth - 2, 0.U))
    val fp_aFix = Mux(io.fp_aIsFpCanonicalNAN, in_NAN, io.fp_a)
    val fp_bFix = Mux(io.fp_bIsFpCanonicalNAN, in_NAN, io.fp_b)
    val result_fsgnj = Cat(fp_bFix.head(1), fp_aFix.tail(1))
    val result_fsgnjn = Cat(~fp_bFix.head(1), fp_aFix.tail(1))
    val result_fsgnjx = Cat(fp_bFix.head(1) ^ fp_aFix.head(1), fp_aFix.tail(1))
    val result_fclass = Wire(UInt(floatWidth.W))
    val out_NAN = Cat(0.U, Fill(exponentWidth, 1.U), 1.U, Fill(significandWidth - 2, 0.U))
    result_min := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN & fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN & fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_less || (fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero), io.fp_b, io.fp_a),
        io.fp_a,
        io.fp_b,
        out_NAN
      )
    )
    result_max := Mux1H(
      Seq(
        !fp_a_is_NAN & !fp_b_is_NAN,
        !fp_a_is_NAN & fp_b_is_NAN,
        fp_a_is_NAN & !fp_b_is_NAN,
        fp_a_is_NAN & fp_b_is_NAN,
      ),
      Seq(
        Mux(fp_b_is_greater.asBool || (!fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero), io.fp_b, io.fp_a),
        io.fp_a,
        io.fp_b,
        out_NAN
      )
    )
    result_feq := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_equal)
    result_flt := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_greater)
    result_fle := Mux(fp_a_is_NAN | fp_b_is_NAN, 0.U, fp_b_is_greater | fp_b_is_equal)
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
    result_fminm := Mux(!fp_a_is_NAN & !fp_b_is_NAN,
      Mux(fp_b_is_less || (fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),
        io.fp_b,
        io.fp_a),
      out_NAN
    )
    result_fmaxm := Mux(!fp_a_is_NAN & !fp_b_is_NAN,
      Mux(fp_b_is_greater.asBool || (!fp_b_sign.asBool && fp_b_is_zero && fp_a_is_zero),
        io.fp_b,
        io.fp_a),
      out_NAN
    )

    val result_stage0 = Mux1H(
      Seq(
        is_min,
        is_max,
        is_feq,
        is_flt | is_fltq,
        is_fle | is_fleq,
        is_fsgnj,
        is_fsgnjn,
        is_fsgnjx,
        is_fclass,
        is_fminm,
        is_fmaxm,
      ),
      Seq(
        result_min,
        result_max,
        result_feq,
        result_flt,
        result_fle,
        result_fsgnj,
        result_fsgnjn,
        result_fsgnjx,
        result_fclass,
        result_fminm,
        result_fmaxm,
      )
    )
    val fflags_NV_stage0 = ((is_min | is_max) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      (is_feq  & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_flt | is_fle ) & (fp_a_is_NAN | fp_b_is_NAN)) |
      ((is_fminm | is_fmaxm) & (fp_a_is_SNAN | fp_b_is_SNAN)) |
      ((is_fltq | is_fleq) & (fp_a_is_SNAN | fp_b_is_SNAN))
    val fflags_stage0 = Cat(fflags_NV_stage0, 0.U(4.W))
    io.fp_c := Mux(RegEnable(is_add | is_sub, fire), float_adder_result, RegEnable(result_stage0, fire))
    io.fflags := Mux(RegEnable(is_add | is_sub, fire), float_adder_fflags, RegEnable(fflags_stage0, fire))
  }
  else {
    io.fp_c := float_adder_result
    io.fflags := float_adder_fflags
  }
}