package yunsuan.fpulite
import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.{VfaddOpCode, VectorElementFormat}
class FloatAdder() extends Module {
  val VLEN = 128
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
  // TODO change fp_format is_vec logic
  // assert(io.fp_format=/=0.U) // TODO: add valid to enable assert
  val fp_format = io.fp_format-1.U //Cat(io.fp_format===3.U,io.fp_format(1))
  val fire = io.fire

  val hasMinMaxCompare = true
  val is_add    = io.op_code === VfaddOpCode.fadd
  val is_sub    = io.op_code === VfaddOpCode.fsub
  val is_min    = io.op_code === VfaddOpCode.fmin
  val is_max    = io.op_code === VfaddOpCode.fmax
  val is_fmerge = (io.op_code === VfaddOpCode.fmerge)
  val is_fmove  = io.op_code === VfaddOpCode.fmove || (io.op_code === VfaddOpCode.fmv_f_s) || (io.op_code === VfaddOpCode.fmv_s_f)
  val is_fsgnj  = io.op_code === VfaddOpCode.fsgnj
  val is_fsgnjn = io.op_code === VfaddOpCode.fsgnjn
  val is_fsgnjx = io.op_code === VfaddOpCode.fsgnjx
  val is_feq    = io.op_code === VfaddOpCode.feq
  val is_fne    = io.op_code === VfaddOpCode.fne
  val is_flt    = io.op_code === VfaddOpCode.flt
  val is_fle    = io.op_code === VfaddOpCode.fle
  val is_fgt    = io.op_code === VfaddOpCode.fgt
  val is_fge    = io.op_code === VfaddOpCode.fge
  val is_fclass = io.op_code === VfaddOpCode.fclass
  val is_fsum_ure = io.op_code === VfaddOpCode.fsum_ure
  val is_fmin_re  = io.op_code === VfaddOpCode.fmin_re
  val is_fmax_re  = io.op_code === VfaddOpCode.fmax_re
  val is_fsum_ore = io.op_code === VfaddOpCode.fsum_ore

  val fast_is_sub = io.op_code(0)

  val f64_fp_a = Wire(UInt(floatWidth.W))
  val f32_0_fp_a = Wire(UInt(floatWidth.W))
  val f32_1_fp_a = Wire(UInt(floatWidth.W))
  val f16_1_fp_a = Wire(UInt(floatWidth.W))
  val f16_3_fp_a = Wire(UInt(floatWidth.W))

  f64_fp_a := io.fp_a
  f32_0_fp_a := io.fp_a(31, 0)
  f32_1_fp_a := io.fp_a(63, 32)
  f16_1_fp_a := io.fp_a(31, 16)
  f16_3_fp_a := io.fp_a(63, 48)

  val U_F32_Mixed_0 = Module(new FloatAdderF32WidenF16MixedPipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F32_Mixed_0.io.fire := fire
  U_F32_Mixed_0.io.fp_a := f32_0_fp_a
  U_F32_Mixed_0.io.fp_b := io.fp_b(31,0)
  U_F32_Mixed_0.io.widen_a := 0.U
  U_F32_Mixed_0.io.widen_b := 0.U
  U_F32_Mixed_0.io.mask := false.B
  U_F32_Mixed_0.io.is_sub := fast_is_sub
  U_F32_Mixed_0.io.round_mode   := io.round_mode
  U_F32_Mixed_0.io.fp_format    := fp_format
  U_F32_Mixed_0.io.res_widening := false.B
  U_F32_Mixed_0.io.opb_widening := false.B
  U_F32_Mixed_0.io.op_code      := io.op_code
  U_F32_Mixed_0.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F32_Mixed_0.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_F32_Mixed_0.io.maskForReduction := 0.U
  U_F32_Mixed_0.io.is_vfwredosum := false.B
  val U_F32_0_result = U_F32_Mixed_0.io.fp_c
  val U_F32_0_fflags = U_F32_Mixed_0.io.fflags
  val U_F16_0_result = U_F32_Mixed_0.io.fp_c(15,0)
  val U_F16_0_fflags = U_F32_Mixed_0.io.fflags

  val U_F32_Mixed_1 = Module(new FloatAdderF32WidenF16MixedPipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F32_Mixed_1.io.fire := fire
  U_F32_Mixed_1.io.fp_a := f32_1_fp_a
  U_F32_Mixed_1.io.fp_b := io.fp_b(63,32)
  U_F32_Mixed_1.io.widen_a := 0.U
  U_F32_Mixed_1.io.widen_b := 0.U
  U_F32_Mixed_1.io.mask := false.B
  U_F32_Mixed_1.io.is_sub  := fast_is_sub
  U_F32_Mixed_1.io.round_mode   := io.round_mode
  U_F32_Mixed_1.io.fp_format    := fp_format
  U_F32_Mixed_1.io.res_widening := false.B
  U_F32_Mixed_1.io.opb_widening := false.B
  U_F32_Mixed_1.io.op_code      := io.op_code
  U_F32_Mixed_1.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F32_Mixed_1.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_F32_Mixed_1.io.maskForReduction :=  0.U
  U_F32_Mixed_1.io.is_vfwredosum := false.B
  val U_F32_1_result = U_F32_Mixed_1.io.fp_c
  val U_F32_1_fflags = U_F32_Mixed_1.io.fflags
  val U_F16_2_result = U_F32_Mixed_1.io.fp_c(15,0)
  val U_F16_2_fflags = U_F32_Mixed_1.io.fflags

  val U_F64_Widen_0 = Module(new FloatAdderF64WidenPipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F64_Widen_0.io.fire := fire
  U_F64_Widen_0.io.fp_a := f64_fp_a
  U_F64_Widen_0.io.fp_b := io.fp_b
  U_F64_Widen_0.io.widen_a := 0.U
  U_F64_Widen_0.io.widen_b := 0.U
  U_F64_Widen_0.io.mask := false.B
  U_F64_Widen_0.io.is_sub := fast_is_sub
  U_F64_Widen_0.io.round_mode := io.round_mode
  U_F64_Widen_0.io.res_widening := false.B
  U_F64_Widen_0.io.opb_widening := false.B
  U_F64_Widen_0.io.op_code      := io.op_code
  U_F64_Widen_0.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F64_Widen_0.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_F64_Widen_0.io.maskForReduction := 0.U
  U_F64_Widen_0.io.is_vfwredosum := false.B
  val U_F64_Widen_0_result = U_F64_Widen_0.io.fp_c
  val U_F64_Widen_0_fflags = U_F64_Widen_0.io.fflags

  val U_F16_1 = Module(new FloatAdderF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F16_1.io.fire := fire
  U_F16_1.io.fp_a := f16_1_fp_a
  U_F16_1.io.fp_b := io.fp_b(31,16)
  U_F16_1.io.mask := false.B
  U_F16_1.io.is_sub := fast_is_sub
  U_F16_1.io.round_mode := io.round_mode
  U_F16_1.io.op_code    := io.op_code
  U_F16_1.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F16_1.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_F16_1.io.maskForReduction := 0.U
  val U_F16_1_result = U_F16_1.io.fp_c
  val U_F16_1_fflags = U_F16_1.io.fflags

  val U_F16_3 = Module(new FloatAdderF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F16_3.io.fire := fire
  U_F16_3.io.fp_a := f16_3_fp_a
  U_F16_3.io.fp_b := io.fp_b(63,48)
  U_F16_3.io.mask := false.B
  U_F16_3.io.is_sub := fast_is_sub
  U_F16_3.io.round_mode := io.round_mode
  U_F16_3.io.op_code    := io.op_code
  U_F16_3.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F16_3.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  U_F16_3.io.maskForReduction := 0.U
  val U_F16_3_result = U_F16_3.io.fp_c
  val U_F16_3_fflags = U_F16_3.io.fflags

  val fp_format_reg = RegEnable(io.fp_format, fire)
  val res_is_f16 = fp_format_reg === 1.U
  val res_is_f32 = fp_format_reg === 2.U
  val res_is_f64 = fp_format_reg === 3.U

  val resultNeedBox = RegEnable(is_add || is_sub || is_min || is_max || is_fsgnj || is_fsgnjn || is_fsgnjx, fire)
  val fp_f64_result = U_F64_Widen_0_result
  val fp_f32_result = Cat(Fill(32, resultNeedBox), U_F32_0_result)
  val fp_f16_result = Cat(Fill(48, resultNeedBox), U_F16_0_result)

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
      U_F16_0_fflags,
      U_F32_0_fflags,
      U_F64_Widen_0_fflags
    )
  )
}
