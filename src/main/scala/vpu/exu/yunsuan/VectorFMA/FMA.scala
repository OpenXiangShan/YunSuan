package race.vpu.yunsuan

import chisel3._
import chisel3.util._
import race.vpu._
import race.vpu.yunsuan.util._
import scala.collection.mutable.ListBuffer
import race.vpu.VParams._
import scala.math._

class FloatFMAMixedWithDifferentFormat(
  val support_fp64: Boolean,
  val support_fp32: Boolean,
  val support_fp16: Boolean,
  val support_bf16: Boolean
) extends Module {
  val floatWidthBigger = if (support_fp64) {
    64
  } else if (support_fp32) {
    32
  } else {
    16
  }
  val io = IO(new Bundle() {
    val fire                 = Input (Bool())
    val fp_a, fp_b, fp_c     = Input (UInt(floatWidthBigger.W))  // fp_a->VS2,fp_b->VS1,fp_c->VD
    val round_mode           = Input (UInt(3.W))
    val fp_format            = Input (UInt(2.W))
    val op_code              = Input (UInt(4.W))
    val fp_result            = Output(UInt(floatWidthBigger.W))
    val fflags               = Output(UInt(5.W))
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())
    val fp_cIsFpCanonicalNAN = Input(Bool())
  })

  def shiftRightWithMuxSticky(srcValue: UInt, shiftValue: UInt): UInt = {
    val vecLength  = shiftValue.getWidth + 1
    val res_vec    = Wire(Vec(vecLength,UInt(srcValue.getWidth.W)))
    val sticky_vec = Wire(Vec(vecLength,UInt(1.W)))
    res_vec(0)    := srcValue
    sticky_vec(0) := 0.U
    for (i <- 0 until shiftValue.getWidth) {
      res_vec(i+1) := Mux(shiftValue(i), res_vec(i) >> (1<<i), res_vec(i))
      sticky_vec(i+1) := Mux(shiftValue(i), sticky_vec(i) | res_vec(i)((1<<i)-1,0).orR, sticky_vec(i))
    }
    Cat(res_vec(vecLength-1),sticky_vec(vecLength-1))
  }

  def sign_inv(src: UInt,sel:Bool): UInt = {
    Cat(Mux(sel,~src.head(1),src.head(1)),src.tail(1))
  }

def getMaxInputWidth(
  width_fp64: Int, width_fp32: Int, width_fp16: Int, width_bf16: Int
): Int = {
  val widths = Seq(
    if (support_fp64) Some(width_fp64) else None,
    if (support_fp32) Some(width_fp32) else None,
    if (support_fp16) Some(width_fp16) else None,
    if (support_bf16) Some(width_bf16) else None
  ).flatten
  if (widths.isEmpty) 0 else widths.max
}

  def CalcPipeLevel(n: Int): Int = {
    var n_next = n
    var CSA4to2Num = if(n_next==8 || n_next==4) n_next/4 else 0
    var CSA3to2Num = if(n_next==8 || n_next==4) 0 else n_next/3
    var remainder = n_next - CSA4to2Num*4 - CSA3to2Num*3
    var level = 0
    while (n_next > 2) {
      n_next = (CSA4to2Num+CSA3to2Num)*2 + remainder
      CSA4to2Num = if(n_next==8 || n_next==4) n_next/4 else 0
      CSA3to2Num = if(n_next==8 || n_next==4) 0 else n_next/3
      remainder = n_next - CSA4to2Num*4 - CSA3to2Num*3
      level += 1
    }
    level
  }
  val exponentWidthFP64 : Int = 11
  val significandWidthFP64 : Int = 53
  val floatWidthFP64 = exponentWidthFP64 + significandWidthFP64
  val rshiftBasicF64        = significandWidthFP64 + 3
  val rshiftMaxF64          = 3*significandWidthFP64 + 4
  val biasF64               = (1 << (exponentWidthFP64-1)) - 1

  val exponentWidthFP32 : Int = 8
  val significandWidthFP32 : Int = 24
  val floatWidthFP32 = exponentWidthFP32 + significandWidthFP32
  val rshiftBasicF32          = significandWidthFP32 + 3
  val rshiftMaxF32            = 3*significandWidthFP32 + 4
  val biasF32              = (1 << (exponentWidthFP32-1)) - 1

  val exponentWidthFP16 : Int = 5
  val significandWidthFP16 : Int = 11
  val floatWidthFP16 = exponentWidthFP16 + significandWidthFP16
  val rshiftBasicF16          = significandWidthFP16 + 3
  val rshiftMaxF16            = 3*significandWidthFP16 + 4
  val biasF16              = (1 << (exponentWidthFP16-1)) - 1 

  val exponentWidthBF16 : Int = 8
  val significandWidthBF16 : Int = 8
  val floatWidthBF16 = exponentWidthBF16 + significandWidthBF16
  val rshiftBasicBF16          = significandWidthBF16 + 3
  val rshiftMaxBF16            = 3*significandWidthBF16 + 4
  val biasBF16               = (1 << (exponentWidthBF16-1)) - 1

  val fire = io.fire
  val fire_reg0 = GatedValidRegNext(fire)
  val fire_reg1 = GatedValidRegNext(fire_reg0)

  val is_fmul   = io.op_code === VfmaOpCode.vfmul
  val is_fmacc  = io.op_code === VfmaOpCode.vfmacc
  val is_fnmacc = io.op_code === VfmaOpCode.vfnmacc
  val is_fmsac  = io.op_code === VfmaOpCode.vfmsac
  val is_fnmsac = io.op_code === VfmaOpCode.vfnmsac

  val fp_a_is_sign_inv = is_fnmacc || is_fnmsac
  val fp_c_is_sign_inv = is_fnmacc || is_fmsac

// declare first the signals, in case of fp64, fp32, fp16, bf16
  val is_fp64         = WireInit(false.B)
  val is_fp64_reg0    = WireInit(false.B)
  val is_fp64_reg1    = WireInit(false.B)
  val is_fp64_reg2    = WireInit(false.B)
  val is_fp32         = WireInit(false.B)
  val is_fp32_reg0    = WireInit(false.B)
  val is_fp32_reg1    = WireInit(false.B)
  val is_fp32_reg2    = WireInit(false.B)
  val is_fp16         = WireInit(false.B)
  val is_fp16_reg0    = WireInit(false.B)
  val is_fp16_reg1    = WireInit(false.B)
  val is_fp16_reg2    = WireInit(false.B)
  val is_bf16         = WireInit(false.B)
  val is_bf16_reg0    = WireInit(false.B)
  val is_bf16_reg1    = WireInit(false.B)
  val is_bf16_reg2    = WireInit(false.B)
  
  val is_sub_f64      = WireInit(false.B)
  val is_sub_f32      = WireInit(false.B)
  val is_sub_f16      = WireInit(false.B)
  val is_sub_bf16     = WireInit(false.B)
  val is_sub_f64_reg0       = WireInit(false.B)
  val is_sub_f64_reg1       = WireInit(false.B)
  val is_sub_f64_reg2       = WireInit(false.B)
  val is_sub_f32_reg0       = WireInit(false.B)
  val is_sub_f32_reg1       = WireInit(false.B)
  val is_sub_f32_reg2       = WireInit(false.B)
  val is_sub_f16_reg0       = WireInit(false.B)
  val is_sub_f16_reg1       = WireInit(false.B)
  val is_sub_f16_reg2       = WireInit(false.B)
  val is_sub_bf16_reg0      = WireInit(false.B)
  val is_sub_bf16_reg1      = WireInit(false.B)
  val is_sub_bf16_reg2      = WireInit(false.B)

  val rshift_guard_f64      = WireInit(false.B)
  val rshift_round_f64      = WireInit(false.B)
  val rshift_sticky_f64     = WireInit(false.B)
  val rshift_guard_f32      = WireInit(false.B)
  val rshift_round_f32      = WireInit(false.B)
  val rshift_sticky_f32     = WireInit(false.B)
  val rshift_guard_f16      = WireInit(false.B)
  val rshift_round_f16      = WireInit(false.B)
  val rshift_sticky_f16     = WireInit(false.B)
  val rshift_guard_bf16     = WireInit(false.B)
  val rshift_round_bf16     = WireInit(false.B)
  val rshift_sticky_bf16    = WireInit(false.B)

  val fp_a_significand_f64   = WireInit(0.U(significandWidthFP64.W))
  val fp_b_significand_f64   = WireInit(0.U(significandWidthFP64.W))
  val fp_c_significand_f64   = WireInit(0.U(significandWidthFP64.W))
  val fp_a_significand_f32   = WireInit(0.U(significandWidthFP32.W))
  val fp_b_significand_f32   = WireInit(0.U(significandWidthFP32.W))
  val fp_c_significand_f32   = WireInit(0.U(significandWidthFP32.W))
  val fp_a_significand_f16   = WireInit(0.U(significandWidthFP16.W))
  val fp_b_significand_f16   = WireInit(0.U(significandWidthFP16.W))
  val fp_c_significand_f16   = WireInit(0.U(significandWidthFP16.W))
  val fp_a_significand_bf16  = WireInit(0.U(significandWidthBF16.W))
  val fp_b_significand_bf16  = WireInit(0.U(significandWidthBF16.W))
  val fp_c_significand_bf16  = WireInit(0.U(significandWidthBF16.W))

  val fp_c_rshiftValue_inv_f64_reg0  = WireInit(0.U((2*rshiftMaxF64-1).W))
  val fp_c_rshiftValue_inv_f32_reg0  = WireInit(0.U((2*rshiftMaxF32-1).W))
  val fp_c_rshiftValue_inv_f16_reg0  = WireInit(0.U((2*rshiftMaxF16-1).W))
  val fp_c_rshiftValue_inv_bf16_reg0 = WireInit(0.U((2*rshiftMaxBF16-1).W))
  val fp_c_f64              = WireInit(0.U(floatWidthFP64.W))
  val fp_a_f64              = WireInit(0.U(floatWidthFP64.W))
  val fp_b_f64              = WireInit(0.U(floatWidthFP64.W))
  val fp_c_f32              = WireInit(0.U(floatWidthFP32.W))
  val fp_a_f32              = WireInit(0.U(floatWidthFP32.W))
  val fp_b_f32              = WireInit(0.U(floatWidthFP32.W))
  val fp_c_f16              = WireInit(0.U(floatWidthFP16.W))
  val fp_a_f16              = WireInit(0.U(floatWidthFP16.W))
  val fp_b_f16              = WireInit(0.U(floatWidthFP16.W))
  val fp_c_bf16             = WireInit(0.U(floatWidthBF16.W))
  val fp_a_bf16             = WireInit(0.U(floatWidthBF16.W))
  val fp_b_bf16             = WireInit(0.U(floatWidthBF16.W))

  val rshift_value_f64      = WireInit(0.S((exponentWidthFP64+2).W))
  val rshift_value_f32      = WireInit(0.S((exponentWidthFP32+2).W))
  val rshift_value_f16      = WireInit(0.S((exponentWidthFP16+2).W))
  val rshift_value_bf16     = WireInit(0.S((exponentWidthBF16+2).W))
  val Eab_f64               = WireInit(0.S((exponentWidthFP64+2).W))
  val Eab_f32               = WireInit(0.S((exponentWidthFP32+2).W))
  val Eab_f16               = WireInit(0.S((exponentWidthFP16+2).W))
  val Eab_bf16              = WireInit(0.S((exponentWidthBF16+2).W))
  val Ea_f64                = WireInit(0.U(exponentWidthFP64.W))
  val Ea_f32                = WireInit(0.U(exponentWidthFP32.W))
  val Ea_f16                = WireInit(0.U(exponentWidthFP16.W))
  val Ea_bf16               = WireInit(0.U(exponentWidthBF16.W))
  val Eb_f64                = WireInit(0.U(exponentWidthFP64.W))
  val Eb_f32                = WireInit(0.U(exponentWidthFP32.W))
  val Eb_f16                = WireInit(0.U(exponentWidthFP16.W))
  val Eb_bf16               = WireInit(0.U(exponentWidthBF16.W))
  val Ec_f64                = WireInit(0.U(exponentWidthFP64.W))
  val Ec_f32                = WireInit(0.U(exponentWidthFP32.W))
  val Ec_f16                = WireInit(0.U(exponentWidthFP16.W))
  val Ec_bf16               = WireInit(0.U(exponentWidthBF16.W))
  val Ec_fix_f64            = WireInit(0.U(exponentWidthFP64.W))
  val Ec_fix_f32            = WireInit(0.U(exponentWidthFP32.W))
  val Ec_fix_f16            = WireInit(0.U(exponentWidthFP16.W))
  val Ec_fix_bf16           = WireInit(0.U(exponentWidthBF16.W))

  val sign_a_b_f64          = WireInit(false.B)
  val sign_c_f64            = WireInit(false.B)
  val sign_a_b_f32          = WireInit(false.B)
  val sign_c_f32            = WireInit(false.B)
  val sign_a_b_f16          = WireInit(false.B)
  val sign_c_f16            = WireInit(false.B)
  val sign_a_b_bf16         = WireInit(false.B)
  val sign_c_bf16           = WireInit(false.B)

  if(support_fp64){
    is_fp64               := io.fp_format === 3.U(2.W)
    is_fp64_reg0          := RegEnable(is_fp64, fire)
    is_fp64_reg1          := RegEnable(is_fp64_reg0, fire_reg0)
    is_fp64_reg2          := RegEnable(is_fp64_reg1, fire_reg1)

    fp_a_f64              := sign_inv(io.fp_a(floatWidthFP64-1,0),fp_a_is_sign_inv)
    fp_b_f64              := io.fp_b(floatWidthFP64-1,0)
    fp_c_f64              := Mux(is_fmul, 0.U(floatWidthFP64.W), sign_inv(io.fp_c(floatWidthFP64-1,0),fp_c_is_sign_inv))

    sign_a_b_f64          := (fp_a_f64.head(1) ^ fp_b_f64.head(1)).asBool
    sign_c_f64            := fp_c_f64.head(1).asBool
    is_sub_f64            := sign_a_b_f64 ^ sign_c_f64
    is_sub_f64_reg0       := RegEnable(is_sub_f64, fire)
    is_sub_f64_reg1       := RegEnable(is_sub_f64_reg0, fire_reg0)
    is_sub_f64_reg2       := RegEnable(is_sub_f64_reg1, fire_reg1)
    
    Ea_f64                := fp_a_f64.tail(1).head(exponentWidthFP64)
    Eb_f64                := fp_b_f64.tail(1).head(exponentWidthFP64)
    Ec_f64                := fp_c_f64.tail(1).head(exponentWidthFP64)

    val Ea_f64_is_not_zero    = Ea_f64.orR
    val Eb_f64_is_not_zero    = Eb_f64.orR
    val Ec_f64_is_not_zero    = Ec_f64.orR
    fp_a_significand_f64  := Cat(Ea_f64_is_not_zero,fp_a_f64.tail(exponentWidthFP64+1))
    fp_b_significand_f64  := Cat(Eb_f64_is_not_zero,fp_b_f64.tail(exponentWidthFP64+1))
    fp_c_significand_f64  := Cat(Ec_f64_is_not_zero,fp_c_f64.tail(exponentWidthFP64+1))
  
    val Ea_fix_f64            = Cat(Ea_f64.head(exponentWidthFP64-1),!Ea_f64_is_not_zero | Ea_f64(0))
    val Eb_fix_f64            = Cat(Eb_f64.head(exponentWidthFP64-1),!Eb_f64_is_not_zero | Eb_f64(0))
    Ec_fix_f64            := Cat(Ec_f64.head(exponentWidthFP64-1),!Ec_f64_is_not_zero | Ec_f64(0))
    Eab_f64               := Cat(0.U,Ea_fix_f64 +& Eb_fix_f64).asSInt - biasF64.S + rshiftBasicF64.S
    rshift_value_f64      := Eab_f64 - Cat(0.U,Ec_fix_f64).asSInt

    val rshift_value_cut_f64  = rshift_value_f64(rshiftMaxF64.U.getWidth-1,0)
    val fp_c_significand_cat0_f64  = Cat(fp_c_significand_f64,0.U((rshiftMaxF64-significandWidthFP64).W))
    val rshift_result_with_grs_f64 = shiftRightWithMuxSticky(fp_c_significand_cat0_f64,rshift_value_cut_f64)
    val Ec_is_too_big_f64          = rshift_value_f64 <= 0.S
    val Ec_is_too_small_f64        = rshift_value_f64.asSInt > rshiftMaxF64.S
    val Ec_is_medium_f64           = !Ec_is_too_big_f64 & !Ec_is_too_small_f64
    rshift_guard_f64           := RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(2), 0.U), fire)
    rshift_round_f64           := RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(1), 0.U), fire)
    rshift_sticky_f64          := RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(0), Mux(Ec_is_too_big_f64, 0.U, fp_c_significand_f64.orR)), fire)

    val rshift_result_temp_f64     = rshift_result_with_grs_f64.head(rshiftMaxF64-2)
    val rshift_result_f64          = Mux(Ec_is_medium_f64,
      rshift_result_temp_f64,
      Mux(Ec_is_too_big_f64, fp_c_significand_cat0_f64.head(rshiftMaxF64-2), 0.U((rshiftMaxF64-2).W))
    )
    fp_c_rshiftValue_inv_f64_reg0 := RegEnable(Mux(is_sub_f64.asBool ,Cat(1.U,~rshift_result_f64),Cat(0.U,rshift_result_f64)), fire)
  }

  if(support_fp32){

    is_fp32               := io.fp_format === 2.U(2.W)
    is_fp32_reg0          := RegEnable(is_fp32, fire)
    is_fp32_reg1          := RegEnable(is_fp32_reg0, fire_reg0)
    is_fp32_reg2          := RegEnable(is_fp32_reg1, fire_reg1)

    fp_a_f32              := sign_inv(io.fp_a(floatWidthFP32-1,0),fp_a_is_sign_inv)
    fp_b_f32              := io.fp_b(floatWidthFP32-1,0)
    fp_c_f32              := Mux(is_fmul, 0.U(floatWidthFP32.W), sign_inv(io.fp_c(floatWidthFP32-1,0),fp_c_is_sign_inv))
    sign_a_b_f32          := (fp_a_f32.head(1) ^ fp_b_f32.head(1)).asBool  
    sign_c_f32            := fp_c_f32.head(1).asBool
    is_sub_f32            := sign_a_b_f32 ^ sign_c_f32
    is_sub_f32_reg0       := RegEnable(is_sub_f32, fire)
    is_sub_f32_reg1       := RegEnable(is_sub_f32_reg0, fire_reg0)
    is_sub_f32_reg2       := RegEnable(is_sub_f32_reg1, fire_reg1)

    val Ea_f32                = fp_a_f32.tail(1).head(exponentWidthFP32)
    val Eb_f32                = fp_b_f32.tail(1).head(exponentWidthFP32)
    val Ec_f32                = fp_c_f32.tail(1).head(exponentWidthFP32)

    val Ea_f32_is_not_zero    = Ea_f32.orR
    val Eb_f32_is_not_zero    = Eb_f32.orR
    val Ec_f32_is_not_zero    = Ec_f32.orR
    fp_a_significand_f32  := Cat(Ea_f32_is_not_zero,fp_a_f32.tail(exponentWidthFP32+1))
    fp_b_significand_f32  := Cat(Eb_f32_is_not_zero,fp_b_f32.tail(exponentWidthFP32+1))
    fp_c_significand_f32  := Cat(Ec_f32_is_not_zero,fp_c_f32.tail(exponentWidthFP32+1))
  
    val Ea_fix_f32            = Cat(Ea_f32.head(exponentWidthFP32-1),!Ea_f32_is_not_zero | Ea_f32(0))
    val Eb_fix_f32            = Cat(Eb_f32.head(exponentWidthFP32-1),!Eb_f32_is_not_zero | Eb_f32(0))
    Ec_fix_f32            := Cat(Ec_f32.head(exponentWidthFP32-1),!Ec_f32_is_not_zero | Ec_f32(0))
    Eab_f32               := Cat(0.U,Ea_fix_f32 +& Eb_fix_f32).asSInt - biasF32.S + rshiftBasicF32.S
    rshift_value_f32      := Eab_f32 - Cat(0.U,Ec_fix_f32).asSInt

    val rshift_value_cut_f32  = rshift_value_f32(rshiftMaxF32.U.getWidth-1,0)
    val fp_c_significand_cat0_f32  = Cat(fp_c_significand_f32,0.U((rshiftMaxF32-significandWidthFP32).W))
    val rshift_result_with_grs_f32 = shiftRightWithMuxSticky(fp_c_significand_cat0_f32,rshift_value_cut_f32)
    val Ec_is_too_big_f32          = rshift_value_f32 <= 0.S
    val Ec_is_too_small_f32        = rshift_value_f32.asSInt > rshiftMaxF32.S
    val Ec_is_medium_f32           = !Ec_is_too_big_f32 & !Ec_is_too_small_f32
    rshift_guard_f32           := RegEnable(Mux(Ec_is_medium_f32, rshift_result_with_grs_f32(2), 0.U), fire)
    rshift_round_f32           := RegEnable(Mux(Ec_is_medium_f32, rshift_result_with_grs_f32(1), 0.U), fire)
    rshift_sticky_f32          := RegEnable(Mux(Ec_is_medium_f32, rshift_result_with_grs_f32(0), Mux(Ec_is_too_big_f32, 0.U, fp_c_significand_f32.orR)), fire)

    val rshift_result_temp_f32     = rshift_result_with_grs_f32.head(rshiftMaxF32-2)
    val rshift_result_f32          = Mux(Ec_is_medium_f32,
      rshift_result_temp_f32,
      Mux(Ec_is_too_big_f32, fp_c_significand_cat0_f32.head(rshiftMaxF32-2), 0.U((rshiftMaxF32-2).W))
    )
    fp_c_rshiftValue_inv_f32_reg0 := RegEnable(Mux(is_sub_f32.asBool ,Cat(1.U,~rshift_result_f32),Cat(0.U,rshift_result_f32)), fire)
  }

  if(support_fp16){

    is_fp16               := io.fp_format === 1.U(2.W)
    is_fp16_reg0          := RegEnable(is_fp16, fire)
    is_fp16_reg1          := RegEnable(is_fp16_reg0, fire_reg0)
    is_fp16_reg2          := RegEnable(is_fp16_reg1, fire_reg1)

    fp_a_f16              := sign_inv(io.fp_a(floatWidthFP16-1,0),fp_a_is_sign_inv)
    fp_b_f16              := io.fp_b(floatWidthFP16-1,0)
    fp_c_f16              := Mux(is_fmul, 0.U(floatWidthFP16.W), sign_inv(io.fp_c(floatWidthFP16-1,0),fp_c_is_sign_inv)) 
    sign_a_b_f16          := (fp_a_f16.head(1) ^ fp_b_f16.head(1)).asBool
    sign_c_f16            := fp_c_f16.head(1).asBool
    is_sub_f16            := sign_a_b_f16 ^ sign_c_f16
    is_sub_f16_reg0       := RegEnable(is_sub_f16, fire)
    is_sub_f16_reg1       := RegEnable(is_sub_f16_reg0, fire_reg0)
    is_sub_f16_reg2       := RegEnable(is_sub_f16_reg1, fire_reg1)

    Ea_f16                := fp_a_f16.tail(1).head(exponentWidthFP16)
    Eb_f16                := fp_b_f16.tail(1).head(exponentWidthFP16)
    Ec_f16                := fp_c_f16.tail(1).head(exponentWidthFP16)
    val Ea_f16_is_not_zero    = Ea_f16.orR
    val Eb_f16_is_not_zero    = Eb_f16.orR
    val Ec_f16_is_not_zero    = Ec_f16.orR

    fp_a_significand_f16  := Cat(Ea_f16_is_not_zero,fp_a_f16.tail(exponentWidthFP16+1))
    fp_b_significand_f16  := Cat(Eb_f16_is_not_zero,fp_b_f16.tail(exponentWidthFP16+1))
    fp_c_significand_f16  := Cat(Ec_f16_is_not_zero,fp_c_f16.tail(exponentWidthFP16+1))

    val Ea_fix_f16            = Cat(Ea_f16.head(exponentWidthFP16-1),!Ea_f16_is_not_zero | Ea_f16(0))
    val Eb_fix_f16            = Cat(Eb_f16.head(exponentWidthFP16-1),!Eb_f16_is_not_zero | Eb_f16(0))
    Ec_fix_f16            := Cat(Ec_f16.head(exponentWidthFP16-1),!Ec_f16_is_not_zero | Ec_f16(0))
    Eab_f16               := Cat(0.U,Ea_fix_f16 +& Eb_fix_f16).asSInt - biasF16.S + rshiftBasicF16.S
    rshift_value_f16      := Eab_f16 - Cat(0.U,Ec_fix_f16).asSInt

    val rshift_value_cut_f16  = rshift_value_f16(rshiftMaxF16.U.getWidth-1,0)
    val fp_c_significand_cat0_f16  = Cat(fp_c_significand_f16,0.U((rshiftMaxF16-significandWidthFP16).W))
    val rshift_result_with_grs_f16 = shiftRightWithMuxSticky(fp_c_significand_cat0_f16,rshift_value_cut_f16)
    val Ec_is_too_big_f16          = rshift_value_f16 <= 0.S
    val Ec_is_too_small_f16        = rshift_value_f16.asSInt > rshiftMaxF16.S
    val Ec_is_medium_f16           = !Ec_is_too_big_f16 & !Ec_is_too_small_f16
    rshift_guard_f16           := RegEnable(Mux(Ec_is_medium_f16, rshift_result_with_grs_f16(2), 0.U), fire)
    rshift_round_f16           := RegEnable(Mux(Ec_is_medium_f16, rshift_result_with_grs_f16(1), 0.U), fire)
    rshift_sticky_f16          := RegEnable(Mux(Ec_is_medium_f16, rshift_result_with_grs_f16(0), Mux(Ec_is_too_big_f16, 0.U, fp_c_significand_f16.orR)), fire)

    val rshift_result_temp_f16     = rshift_result_with_grs_f16.head(rshiftMaxF16-2)
    val rshift_result_f16          = Mux(Ec_is_medium_f16,
      rshift_result_temp_f16,
      Mux(Ec_is_too_big_f16, fp_c_significand_cat0_f16.head(rshiftMaxF16-2), 0.U((rshiftMaxF16-2).W))
    )
    fp_c_rshiftValue_inv_f16_reg0 := RegEnable(Mux(is_sub_f16.asBool ,Cat(1.U,~rshift_result_f16),Cat(0.U,rshift_result_f16)), fire)
  }
  
  if(support_bf16){
    //default is bf16
    is_bf16               := io.fp_format === 0.U(2.W)
    is_bf16_reg0          := RegEnable(is_bf16, fire)
    is_bf16_reg1          := RegEnable(is_bf16_reg0, fire_reg0)
    is_bf16_reg2          := RegEnable(is_bf16_reg1, fire_reg1)

    fp_a_bf16              := sign_inv(io.fp_a(floatWidthBF16-1,0),fp_a_is_sign_inv)
    fp_b_bf16              := io.fp_b(floatWidthBF16-1,0)
    fp_c_bf16              := Mux(is_fmul, 0.U(floatWidthBF16.W), sign_inv(io.fp_c(floatWidthBF16-1,0),fp_c_is_sign_inv))
    sign_a_b_bf16          := (fp_a_bf16.head(1) ^ fp_b_bf16.head(1)).asBool
    sign_c_bf16            := fp_c_bf16.head(1).asBool
    is_sub_bf16            := sign_a_b_bf16 ^ sign_c_bf16
    is_sub_bf16_reg0       := RegEnable(is_sub_bf16, fire)
    is_sub_bf16_reg1       := RegEnable(is_sub_bf16_reg0, fire_reg0)
    is_sub_bf16_reg2       := RegEnable(is_sub_bf16_reg1, fire_reg1)
  
    Ea_bf16                := fp_a_bf16.tail(1).head(exponentWidthBF16)
    Eb_bf16                := fp_b_bf16.tail(1).head(exponentWidthBF16)
    Ec_bf16                := fp_c_bf16.tail(1).head(exponentWidthBF16)
    val Ea_bf16_is_not_zero    = Ea_bf16.orR
    val Eb_bf16_is_not_zero    = Eb_bf16.orR
    val Ec_bf16_is_not_zero    = Ec_bf16.orR

    fp_a_significand_bf16  := Cat(Ea_bf16_is_not_zero,fp_a_bf16.tail(exponentWidthBF16+1))
    fp_b_significand_bf16  := Cat(Eb_bf16_is_not_zero,fp_b_bf16.tail(exponentWidthBF16+1))
    fp_c_significand_bf16  := Cat(Ec_bf16_is_not_zero,fp_c_bf16.tail(exponentWidthBF16+1))

    val Ea_fix_bf16            = Cat(Ea_bf16.head(exponentWidthBF16-1),!Ea_bf16_is_not_zero | Ea_bf16(0))
    val Eb_fix_bf16            = Cat(Eb_bf16.head(exponentWidthBF16-1),!Eb_bf16_is_not_zero | Eb_bf16(0))
    Ec_fix_bf16            := Cat(Ec_bf16.head(exponentWidthBF16-1),!Ec_bf16_is_not_zero | Ec_bf16(0))
    Eab_bf16               := Cat(0.U,Ea_fix_bf16 +& Eb_fix_bf16).asSInt - biasBF16.S + rshiftBasicBF16.S
    rshift_value_bf16      := Eab_bf16 - Cat(0.U,Ec_fix_bf16).asSInt

    val rshift_value_cut_bf16  = rshift_value_bf16(rshiftMaxBF16.U.getWidth-1,0)
    val fp_c_significand_cat0_bf16  = Cat(fp_c_significand_bf16,0.U((rshiftMaxBF16-significandWidthBF16).W))
    val rshift_result_with_grs_bf16 = shiftRightWithMuxSticky(fp_c_significand_cat0_bf16,rshift_value_cut_bf16)
    val Ec_is_too_big_bf16          = rshift_value_bf16 <= 0.S
    val Ec_is_too_small_bf16        = rshift_value_bf16.asSInt > rshiftMaxBF16.S
    val Ec_is_medium_bf16           = !Ec_is_too_big_bf16 & !Ec_is_too_small_bf16
    rshift_guard_bf16           := RegEnable(Mux(Ec_is_medium_bf16, rshift_result_with_grs_bf16(2), 0.U), fire)
    rshift_round_bf16           := RegEnable(Mux(Ec_is_medium_bf16, rshift_result_with_grs_bf16(1), 0.U), fire)
    rshift_sticky_bf16          := RegEnable(Mux(Ec_is_medium_bf16, rshift_result_with_grs_bf16(0), Mux(Ec_is_too_big_bf16, 0.U, fp_c_significand_bf16.orR)), fire)

    val rshift_result_temp_bf16     = rshift_result_with_grs_bf16.head(rshiftMaxBF16-2)
    val rshift_result_bf16          = Mux(Ec_is_medium_bf16,
      rshift_result_temp_bf16,
      Mux(Ec_is_too_big_bf16, fp_c_significand_cat0_bf16.head(rshiftMaxBF16-2), 0.U((rshiftMaxBF16-2).W))
    )
    fp_c_rshiftValue_inv_bf16_reg0 := RegEnable(Mux(is_sub_bf16.asBool ,Cat(1.U,~rshift_result_bf16),Cat(0.U,rshift_result_bf16)), fire)

  }

  val U_Booth_Input_A = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16, significandWidthFP64, significandWidthFP32, significandWidthFP16, significandWidthBF16))
  val U_Booth_Input_B = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16, significandWidthFP64, significandWidthFP32, significandWidthFP16, significandWidthBF16))

  if(support_fp64){
    U_Booth_Input_A.io.input_f64 := fp_a_significand_f64
    U_Booth_Input_A.io.is_fp64 := is_fp64
    U_Booth_Input_B.io.input_f64 := fp_b_significand_f64
    U_Booth_Input_B.io.is_fp64 := is_fp64
  }
  else{
    U_Booth_Input_A.io.input_f64 := 0.U
    U_Booth_Input_A.io.is_fp64 := 0.U
    U_Booth_Input_B.io.input_f64 := 0.U
    U_Booth_Input_B.io.is_fp64 := 0.U
  }
  if(support_fp32){
    U_Booth_Input_A.io.input_f32 := fp_a_significand_f32
    U_Booth_Input_A.io.is_fp32 := is_fp32
    U_Booth_Input_B.io.input_f32 := fp_b_significand_f32
    U_Booth_Input_B.io.is_fp32 := is_fp32
  }
  else{
    U_Booth_Input_A.io.input_f32 := 0.U
    U_Booth_Input_A.io.is_fp32 := 0.U
    U_Booth_Input_B.io.input_f32 := 0.U
    U_Booth_Input_B.io.is_fp32 := 0.U
  }
  if(support_fp16){
    U_Booth_Input_A.io.input_f16 := fp_a_significand_f16
    U_Booth_Input_A.io.is_fp16 := is_fp16
    U_Booth_Input_B.io.input_f16 := fp_b_significand_f16
    U_Booth_Input_B.io.is_fp16 := is_fp16
  }
  else{
    U_Booth_Input_A.io.input_f16 := 0.U
    U_Booth_Input_A.io.is_fp16 := 0.U
    U_Booth_Input_B.io.input_f16 := 0.U
    U_Booth_Input_B.io.is_fp16 := 0.U
  }
  if(support_bf16){
    U_Booth_Input_A.io.input_bf16 := fp_a_significand_bf16
    U_Booth_Input_A.io.is_bf16 := is_bf16
    U_Booth_Input_B.io.input_bf16 := fp_b_significand_bf16
    U_Booth_Input_B.io.is_bf16 := is_bf16
  }
  else{
    U_Booth_Input_A.io.input_bf16 := 0.U
    U_Booth_Input_A.io.is_bf16 := 0.U
    U_Booth_Input_B.io.input_bf16 := 0.U
    U_Booth_Input_B.io.is_bf16 := 0.U
  }
  
  val booth_in_a = U_Booth_Input_A.io.output
  val booth_in_b = U_Booth_Input_B.io.output
  //need to change
  val U_BoothEncoder = Module(new BoothEncoderFP64FP32FP16BF16(booth_in_a.getWidth, true, support_fp64, support_fp32, support_fp16, support_bf16))
  U_BoothEncoder.io.in_a := booth_in_a
  U_BoothEncoder.io.in_b := booth_in_b

  val U_CSAnto2 = Module(new CSA_Nto2With3to2MainPipeline(U_BoothEncoder.io.out_pp.length,U_BoothEncoder.io.out_pp.head.getWidth,pipeLevel = CalcPipeLevel(U_BoothEncoder.io.out_pp.length)))
  U_CSAnto2.io.fire := fire
  U_CSAnto2.io.in := U_BoothEncoder.io.out_pp

  // need to check
  val CSA3to2_in_a = U_CSAnto2.io.out_sum
  val CSA3to2_Input_b = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16,2*significandWidthFP64+1, 2*significandWidthFP32+1, 2*significandWidthFP16+1, 2*significandWidthBF16+1))
  val CSA3to2_Input_c = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16,2*significandWidthFP64+1, 2*significandWidthFP32+1, 2*significandWidthFP16+1, 2*significandWidthBF16+1))
  
  if(support_fp64){
    U_BoothEncoder.io.is_fp64 := is_fp64
    CSA3to2_Input_b.io.input_f64 := Cat(U_CSAnto2.io.out_car(significandWidthFP64*2,1), is_sub_f64_reg0 & !rshift_guard_f64 & !rshift_round_f64 & !rshift_sticky_f64)
    CSA3to2_Input_b.io.is_fp64 := is_fp64_reg0
    CSA3to2_Input_c.io.input_f64 := Cat(0.U,fp_c_rshiftValue_inv_f64_reg0(2*significandWidthFP64-1,0))
    CSA3to2_Input_c.io.is_fp64 := is_fp64_reg0
  }
  else {
    U_BoothEncoder.io.is_fp64     := 0.U
    CSA3to2_Input_b.io.input_f64  := 0.U
    CSA3to2_Input_b.io.is_fp64    := 0.U
    CSA3to2_Input_c.io.input_f64  := 0.U
    CSA3to2_Input_c.io.is_fp64    := 0.U
  }
  if(support_fp32){
    U_BoothEncoder.io.is_fp32 := is_fp32
    CSA3to2_Input_b.io.input_f32 := Cat(U_CSAnto2.io.out_car(significandWidthFP32*2,1), is_sub_f32_reg0 & !rshift_guard_f32 & !rshift_round_f32 & !rshift_sticky_f32)
    CSA3to2_Input_b.io.is_fp32 := is_fp32_reg0
    CSA3to2_Input_c.io.input_f32 := Cat(0.U,fp_c_rshiftValue_inv_f32_reg0(2*significandWidthFP32-1,0))
    CSA3to2_Input_c.io.is_fp32 := is_fp32_reg0
  }
  else {
    U_BoothEncoder.io.is_fp32     := 0.U
    CSA3to2_Input_b.io.input_f32  := 0.U
    CSA3to2_Input_b.io.is_fp32    := 0.U
    CSA3to2_Input_c.io.input_f32  := 0.U
    CSA3to2_Input_c.io.is_fp32    := 0.U
  }  
  if(support_fp16){
    U_BoothEncoder.io.is_fp16 := is_fp16
    CSA3to2_Input_b.io.input_f16 := Cat(U_CSAnto2.io.out_car(significandWidthFP16*2,1), is_sub_f16_reg0 & !rshift_guard_f16 & !rshift_round_f16 & !rshift_sticky_f16)
    CSA3to2_Input_b.io.is_fp16 := is_fp16_reg0
    CSA3to2_Input_c.io.input_f16 := Cat(0.U,fp_c_rshiftValue_inv_f16_reg0(2*significandWidthFP16-1,0))
    CSA3to2_Input_c.io.is_fp16 := is_fp16_reg0
  }
  else {
    U_BoothEncoder.io.is_fp16     := 0.U
    CSA3to2_Input_b.io.input_f16  := 0.U
    CSA3to2_Input_b.io.is_fp16    := 0.U
    CSA3to2_Input_c.io.input_f16  := 0.U
    CSA3to2_Input_c.io.is_fp16    := 0.U
  }
  if(support_bf16){
    U_BoothEncoder.io.is_bf16 := is_bf16
    CSA3to2_Input_b.io.input_bf16 := Cat(U_CSAnto2.io.out_car(significandWidthBF16*2,1), is_sub_bf16_reg0 & !rshift_guard_bf16 & !rshift_round_bf16 & !rshift_sticky_bf16)
    CSA3to2_Input_b.io.is_bf16 := is_bf16_reg0
    CSA3to2_Input_c.io.input_bf16 := Cat(0.U,fp_c_rshiftValue_inv_bf16_reg0(2*significandWidthBF16-1,0))
    CSA3to2_Input_c.io.is_bf16 := is_bf16_reg0
  }
  else {
    U_BoothEncoder.io.is_bf16       := 0.U
    CSA3to2_Input_b.io.input_bf16   := 0.U
    CSA3to2_Input_b.io.is_bf16      := 0.U
    CSA3to2_Input_c.io.input_bf16   := 0.U
    CSA3to2_Input_c.io.is_bf16      := 0.U
  }

  val CSA3to2_in_b = CSA3to2_Input_b.io.output
  val CSA3to2_in_c = CSA3to2_Input_c.io.output

  val U_CSA3to2 = Module(new CSA3to2Mixed(width = U_CSAnto2.io.out_sum.getWidth))

  U_CSA3to2.io.in_a := CSA3to2_in_a
  U_CSA3to2.io.in_b := CSA3to2_in_b
  U_CSA3to2.io.in_c := CSA3to2_in_c

  // mul result
  val adder_lowbit = U_CSA3to2.io.out_sum + U_CSA3to2.io.out_car
  val adder_lowbit_f64  = if (support_fp64) adder_lowbit(significandWidthFP64*2,0) else 0.U(0.W)
  val adder_lowbit_f32  = if (support_fp32) adder_lowbit(significandWidthFP32*2,0) else 0.U(0.W)
  val adder_lowbit_f16  = if (support_fp16) adder_lowbit(significandWidthFP16*2,0) else 0.U(0.W)
  val adder_lowbit_bf16 = if (support_bf16) adder_lowbit(significandWidthBF16*2,0) else 0.U(0.W)

  val fp_c_rshift_result_high_inv_add0_f64 = if(support_fp64) fp_c_rshiftValue_inv_f64_reg0.head(significandWidthFP64+3) else 0.U(0.W)
  val fp_c_rshift_result_high_inv_add0_f32 = if(support_fp32) fp_c_rshiftValue_inv_f32_reg0.head(significandWidthFP32+3) else 0.U(0.W)
  val fp_c_rshift_result_high_inv_add0_f16 = if(support_fp16) fp_c_rshiftValue_inv_f16_reg0.head(significandWidthFP16+3) else 0.U(0.W)
  val fp_c_rshift_result_high_inv_add0_bf16 = if(support_bf16) fp_c_rshiftValue_inv_bf16_reg0.head(significandWidthBF16+3) else 0.U(0.W)

  val fp_c_rshift_result_high_inv_add1_seletor = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16, significandWidthFP64+6, significandWidthFP32+6, significandWidthFP16+6, significandWidthBF16+6))
  if(support_fp64){
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f64 := Cat(0.U(3.W), fp_c_rshift_result_high_inv_add0_f64)
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp64 := is_fp64_reg0
  }
  else{
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f64 := 0.U
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp64 := 0.U
  }
  if(support_fp32){
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f32 := Cat(0.U(3.W), fp_c_rshift_result_high_inv_add0_f32)
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp32 := is_fp32_reg0
  }
  else{
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f32 := 0.U
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp32 := 0.U
  }
  if(support_fp16){
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f16 := Cat(0.U(3.W), fp_c_rshift_result_high_inv_add0_f16)
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp16 := is_fp16_reg0
  }
  else{
    fp_c_rshift_result_high_inv_add1_seletor.io.input_f16 := 0.U
    fp_c_rshift_result_high_inv_add1_seletor.io.is_fp16 := 0.U
  }
  if(support_bf16){
    fp_c_rshift_result_high_inv_add1_seletor.io.input_bf16 := Cat(0.U(3.W), fp_c_rshift_result_high_inv_add0_bf16)
    fp_c_rshift_result_high_inv_add1_seletor.io.is_bf16 := is_bf16_reg0
  }
  else{
    fp_c_rshift_result_high_inv_add1_seletor.io.input_bf16 := 0.U
    fp_c_rshift_result_high_inv_add1_seletor.io.is_bf16 := 0.U
  }

  val fp_c_rshift_result_high_inv_add1 = fp_c_rshift_result_high_inv_add1_seletor.io.output + 1.U(1.W)

  val fp_result_sel = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16, floatWidthFP64, floatWidthFP32, floatWidthFP16, floatWidthBF16))
  val fflags_sel    = Module(new InputMixedSeletor(support_fp64, support_fp32, support_fp16, support_bf16, 5, 5, 5, 5))

  if(support_fp64){
    val output_fp64 = Module(new OutputGenerator(exponentWidth=exponentWidthFP64, significandWidth=significandWidthFP64))
    output_fp64.io.is_fmul                          := is_fmul
    output_fp64.io.fp_c                             := fp_c_f64    
    output_fp64.io.is_fmul                          := is_fmul
    output_fp64.io.round_mode                       := io.round_mode       
    output_fp64.io.sign_c                           := sign_c_f64
    output_fp64.io.sign_a_b                         := sign_a_b_f64      
    output_fp64.io.adder_lowbit                     := adder_lowbit_f64
    output_fp64.io.fp_c_rshift_result_high_inv_add1 := fp_c_rshift_result_high_inv_add1(significandWidthFP64+2, 0)
    output_fp64.io.fp_c_rshift_result_high_inv_add0 := fp_c_rshift_result_high_inv_add0_f64
    output_fp64.io.Eab                              := Eab_f64
    output_fp64.io.rshift_guard                     := rshift_guard_f64
    output_fp64.io.rshift_round                     := rshift_round_f64
    output_fp64.io.rshift_sticky                    := rshift_sticky_f64
    output_fp64.io.rshift_value                     := rshift_value_f64
    output_fp64.io.fp_aIsFpCanonicalNAN             := io.fp_aIsFpCanonicalNAN
    output_fp64.io.fp_bIsFpCanonicalNAN             := io.fp_bIsFpCanonicalNAN
    output_fp64.io.fp_cIsFpCanonicalNAN             := io.fp_cIsFpCanonicalNAN
    output_fp64.fp_a_significand                    := fp_a_significand_f64
    output_fp64.fp_b_significand                    := fp_b_significand_f64
    output_fp64.fp_c_significand                    := fp_c_significand_f64
    output_fp64.Ea                                  := Ea_f64
    output_fp64.Eb                                  := Eb_f64
    output_fp64.Ec                                  := Ec_f64
    output_fp64.Ec_fix                              := Ec_fix_f64
    output_fp64.io.is_sub_reg0                      := is_sub_f64_reg0  
    output_fp64.io.fire                             := io.fire
    output_fp64.io.fire_reg0                        := fire_reg0  
    output_fp64.io.fire_reg1                        := fire_reg1  

    fp_result_sel.io.input_f64  := output_fp64.io.result
    fp_result_sel.io.is_fp64    := is_fp64_reg2
    fflags_sel.io.input_f64  := output_fp64.io.fflags
    fflags_sel.io.is_fp64    := is_fp64_reg2
  }
  else{
    fp_result_sel.io.input_f64  := 0.U
    fp_result_sel.io.is_fp64    := 0.U
    fflags_sel.io.input_f64  := 0.U
    fflags_sel.io.is_fp64    := 0.U
  }
  if(support_fp32){
    val output_fp32 = Module(new OutputGenerator(exponentWidth=exponentWidthFP32, significandWidth=significandWidthFP32))
    output_fp32.io.is_fmul                          := is_fmul
    output_fp32.io.fp_c                             := fp_c_f32        
    output_fp32.io.round_mode                       := io.round_mode    
    output_fp32.io.sign_c                           := sign_c_f32
    output_fp32.io.sign_a_b                         := sign_a_b_f32  
    output_fp32.io.adder_lowbit                     := adder_lowbit_f32
    output_fp32.io.fp_c_rshift_result_high_inv_add1 := fp_c_rshift_result_high_inv_add1(significandWidthFP32+2, 0)
    output_fp32.io.fp_c_rshift_result_high_inv_add0 := fp_c_rshift_result_high_inv_add0_f32
    output_fp32.io.Eab                              := Eab_f32
    output_fp32.io.rshift_guard                     := rshift_guard_f32
    output_fp32.io.rshift_round                     := rshift_round_f32
    output_fp32.io.rshift_sticky                    := rshift_sticky_f32
    output_fp32.io.rshift_value                     := rshift_value_f32
    output_fp32.io.fp_aIsFpCanonicalNAN             := io.fp_aIsFpCanonicalNAN
    output_fp32.io.fp_bIsFpCanonicalNAN             := io.fp_bIsFpCanonicalNAN
    output_fp32.io.fp_cIsFpCanonicalNAN             := io.fp_cIsFpCanonicalNAN
    output_fp32.fp_a_significand                    := fp_a_significand_f32
    output_fp32.fp_b_significand                    := fp_b_significand_f32
    output_fp32.fp_c_significand                    := fp_c_significand_f32
    output_fp32.Ea                                  := Ea_f32
    output_fp32.Eb                                  := Eb_f32
    output_fp32.Ec                                  := Ec_f32
    output_fp32.Ec_fix                              := Ec_fix_f32
    output_fp32.io.is_sub_reg0                      := is_sub_f32_reg0  
    output_fp32.io.fire                             := io.fire
    output_fp32.io.fire_reg0                        := fire_reg0  
    output_fp32.io.fire_reg1                        := fire_reg1  

    fp_result_sel.io.input_f32  := output_fp32.io.result
    fp_result_sel.io.is_fp32    := is_fp32_reg2
    fflags_sel.io.input_f32  := output_fp32.io.fflags
    fflags_sel.io.is_fp32    := is_fp32_reg2
  }
  else{
    fp_result_sel.io.input_f32  := 0.U
    fp_result_sel.io.is_fp32    := 0.U
    fflags_sel.io.input_f32  := 0.U
    fflags_sel.io.is_fp32    := 0.U
  }
  if(support_fp16){
    val output_fp16 = Module(new OutputGenerator(exponentWidth=exponentWidthFP16, significandWidth=significandWidthFP16))
    output_fp16.io.is_fmul                          := is_fmul
    output_fp16.io.fp_c                             := fp_c_f16    
    output_fp16.io.round_mode                       := io.round_mode    
    output_fp16.io.sign_c                           := sign_c_f16
    output_fp16.io.sign_a_b                         := sign_a_b_f16  
    output_fp16.io.adder_lowbit                     := adder_lowbit_f16
    output_fp16.io.fp_c_rshift_result_high_inv_add1 := fp_c_rshift_result_high_inv_add1(significandWidthFP16+2, 0)
    output_fp16.io.fp_c_rshift_result_high_inv_add0 := fp_c_rshift_result_high_inv_add0_f16
    output_fp16.io.Eab                              := Eab_f16
    output_fp16.io.rshift_guard                     := rshift_guard_f16
    output_fp16.io.rshift_round                     := rshift_round_f16
    output_fp16.io.rshift_sticky                    := rshift_sticky_f16
    output_fp16.io.rshift_value                     := rshift_value_f16
    output_fp16.io.fp_aIsFpCanonicalNAN             := io.fp_aIsFpCanonicalNAN
    output_fp16.io.fp_bIsFpCanonicalNAN             := io.fp_bIsFpCanonicalNAN
    output_fp16.io.fp_cIsFpCanonicalNAN             := io.fp_cIsFpCanonicalNAN
    output_fp16.fp_a_significand                    := fp_a_significand_f16
    output_fp16.fp_b_significand                    := fp_b_significand_f16
    output_fp16.fp_c_significand                    := fp_c_significand_f16
    output_fp16.Ea                                  := Ea_f16
    output_fp16.Eb                                  := Eb_f16
    output_fp16.Ec                                  := Ec_f16
    output_fp16.Ec_fix                              := Ec_fix_f16
    output_fp16.io.is_sub_reg0                      := is_sub_f16_reg0  
    output_fp16.io.fire                             := io.fire
    output_fp16.io.fire_reg0                        := fire_reg0  
    output_fp16.io.fire_reg1                        := fire_reg1 

    fp_result_sel.io.input_f16  := output_fp16.io.result
    fp_result_sel.io.is_fp16    := is_fp16_reg2
    fflags_sel.io.input_f16  := output_fp16.io.fflags
    fflags_sel.io.is_fp16    := is_fp16_reg2 
  }
  else{
    fp_result_sel.io.input_f16  := 0.U
    fp_result_sel.io.is_fp16    := 0.U
    fflags_sel.io.input_f16  := 0.U
    fflags_sel.io.is_fp16    := 0.U
  }
  if(support_bf16){
    val output_bf16 = Module(new OutputGenerator(exponentWidth=exponentWidthBF16, significandWidth=significandWidthBF16))
    output_bf16.io.is_fmul                          := is_fmul
    output_bf16.io.fp_c                             := fp_c_bf16
    output_bf16.io.round_mode                       := io.round_mode
    output_bf16.io.sign_c                           := sign_c_bf16
    output_bf16.io.sign_a_b                         := sign_a_b_bf16  
    output_bf16.io.adder_lowbit                     := adder_lowbit_bf16
    output_bf16.io.fp_c_rshift_result_high_inv_add1 := fp_c_rshift_result_high_inv_add1(significandWidthBF16+2, 0)
    output_bf16.io.fp_c_rshift_result_high_inv_add0 := fp_c_rshift_result_high_inv_add0_bf16
    output_bf16.io.Eab                              := Eab_bf16
    output_bf16.io.rshift_guard                     := rshift_guard_bf16
    output_bf16.io.rshift_round                     := rshift_round_bf16
    output_bf16.io.rshift_sticky                    := rshift_sticky_bf16
    output_bf16.io.rshift_value                     := rshift_value_bf16
    output_bf16.io.fp_aIsFpCanonicalNAN             := io.fp_aIsFpCanonicalNAN
    output_bf16.io.fp_bIsFpCanonicalNAN             := io.fp_bIsFpCanonicalNAN
    output_bf16.io.fp_cIsFpCanonicalNAN             := io.fp_cIsFpCanonicalNAN
    output_bf16.fp_a_significand                    := fp_a_significand_bf16
    output_bf16.fp_b_significand                    := fp_b_significand_bf16
    output_bf16.fp_c_significand                    := fp_c_significand_bf16
    output_bf16.Ea                                  := Ea_bf16
    output_bf16.Eb                                  := Eb_bf16
    output_bf16.Ec                                  := Ec_bf16
    output_bf16.Ec_fix                              := Ec_fix_bf16
    output_bf16.io.is_sub_reg0                      := is_sub_bf16_reg0  
    output_bf16.io.fire                             := io.fire
    output_bf16.io.fire_reg0                        := fire_reg0  
    output_bf16.io.fire_reg1                        := fire_reg1  

    fp_result_sel.io.input_bf16 := output_bf16.io.result
    fp_result_sel.io.is_bf16    := is_bf16_reg2
    fflags_sel.io.input_bf16 := output_bf16.io.fflags
    fflags_sel.io.is_bf16    := is_bf16_reg2
  }  
  else{
    fp_result_sel.io.input_bf16  := 0.U
    fp_result_sel.io.is_bf16    := 0.U
    fflags_sel.io.input_bf16  := 0.U
    fflags_sel.io.is_bf16    := 0.U
  }

  // if(support_fp64){
  //   fp_result_sel.io.input_f64  := output_fp64.result
  //   fp_result_sel.io.is_fp64    := is_fp64_reg2
  //   fp_result_sel.io.input_f64  := output_fp64.fflags
  //   fp_result_sel.io.is_fp64    := is_fp64_reg2
  // }
  // if(support_fp32){
  //   fp_result_sel.io.input_f32  := output_fp32.result
  //   fp_result_sel.io.is_fp32    := is_fp32_reg2
  //   fp_result_sel.io.input_f32  := output_fp32.fflags
  //   fp_result_sel.io.is_fp32    := is_fp32_reg2
  // }
  // if(support_fp16){
  //   fp_result_sel.io.input_f16  := output_fp16.result
  //   fp_result_sel.io.is_fp16    := is_fp16_reg2
  //   fp_result_sel.io.input_f16  := output_fp16.fflags
  //   fp_result_sel.io.is_fp16    := is_fp16_reg2
  // }
  // if(support_bf16){
  //   fp_result_sel.io.input_bf16 := output_bf16.result
  //   fp_result_sel.io.is_bf16    := is_bf16_reg2
  //   fp_result_sel.io.input_bf16 := output_bf16.fflags
  //   fp_result_sel.io.is_bf16    := is_bf16_reg2
  // }

  io.fp_result  := fp_result_sel.io.output
  io.fflags     := fflags_sel.io.output
}


class VectorInputMixedSeletor(
  val supportFP64: Boolean = true,
  val supportFP32: Boolean = true,
  val supportFP16: Boolean = true,
  val supportBF16: Boolean = true,
  val out_width : Int,
  val vec_num   : Int
) extends Module {
  val io = IO(new Bundle {
    val is_fp64 = if(supportFP64) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_fp32 = if(supportFP32) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_fp16 = if(supportFP16) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_bf16 = if(supportBF16) Input(UInt(1.W)) else Input(UInt(0.W))
    val input_f64  = if(supportFP64) Input(Vec(vec_num,UInt(out_width.W))) else Input(UInt(0.W))
    val input_f32  = if(supportFP32) Input(Vec(vec_num,UInt(out_width.W))) else Input(UInt(0.W))
    val input_f16  = if(supportFP16) Input(Vec(vec_num,UInt(out_width.W))) else Input(UInt(0.W))
    val input_bf16 = if(supportBF16) Input(Vec(vec_num,UInt(out_width.W))) else Input(UInt(0.W))
    val output     = Output(Vec(vec_num,UInt(out_width.W)))
  })

val zeroVec = VecInit(Seq.fill(vec_num)(0.U(out_width.W)))

    val output =
      if (supportFP64 && supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, io.input_f32,
            Mux(io.is_fp16.asBool, io.input_f16,
              io.input_bf16)))
      } else if (supportFP64 && supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, io.input_f32,
            Mux(io.is_fp16.asBool, io.input_f16, zeroVec)))
      } else if (supportFP64 && supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, io.input_f32, 
            Mux(io.is_bf16.asBool, io.input_bf16, zeroVec)))
      } else if (supportFP64 && supportFP32 && !supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, io.input_f32, zeroVec))
      } else if (supportFP64 && !supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp16.asBool, io.input_f16,
            Mux(io.is_bf16.asBool, io.input_bf16, zeroVec)))
      } else if (supportFP64 && !supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp16.asBool, io.input_f16, zeroVec))
      } else if (supportFP64 && !supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_bf16.asBool, io.input_bf16, zeroVec))
      } else if (supportFP64 && !supportFP32 && !supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64, zeroVec)
      } else if (!supportFP64 && supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp32.asBool, io.input_f32,
          Mux(io.is_fp16.asBool, io.input_f16,
            Mux(io.is_bf16.asBool, io.input_bf16, zeroVec)))
      } else if (!supportFP64 && supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp32.asBool, io.input_f32,
          Mux(io.is_fp16.asBool, io.input_f16, zeroVec))
      } else if (!supportFP64 && supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp32.asBool, io.input_f32,
          Mux(io.is_bf16.asBool, io.input_bf16, zeroVec))
      } else if (!supportFP64 && supportFP32 && !supportFP16 && !supportBF16) {
          Mux(io.is_fp32.asBool, io.input_f32, zeroVec)
      } else if (!supportFP64 && !supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp16.asBool, io.input_f16,
          Mux(io.is_bf16.asBool, io.input_bf16, zeroVec))
      } else if (!supportFP64 && !supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp16.asBool, io.input_f16, zeroVec)
      } else if (!supportFP64 && !supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_bf16.asBool, io.input_bf16, zeroVec)
      } else {
        zeroVec  // none supported
      }
      io.output := output
}



class InputMixedSeletor(
  val supportFP64: Boolean = true,
  val supportFP32: Boolean = true,
  val supportFP16: Boolean = true,
  val supportBF16: Boolean = true,
  val widthfp64 :  Int,
  val widthfp32 :  Int,
  val widthfp16 :  Int,
  val widthbf16 :  Int,
) extends Module {
  val out_width = max(widthfp64, max(widthfp32, max(widthfp16, widthbf16)))
  val io = IO(new Bundle {
    val is_fp64 = if(supportFP64) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_fp32 = if(supportFP32) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_fp16 = if(supportFP16) Input(UInt(1.W)) else Input(UInt(0.W))
    val is_bf16 = if(supportBF16) Input(UInt(1.W)) else Input(UInt(0.W))
    val input_f64  = if(supportFP64) Input(UInt(widthfp64.W)) else Input(UInt(0.W))
    val input_f32  = if(supportFP32) Input(UInt(widthfp32.W)) else Input(UInt(0.W))
    val input_f16  = if(supportFP16) Input(UInt(widthfp16.W)) else Input(UInt(0.W))
    val input_bf16 = if(supportBF16) Input(UInt(widthbf16.W)) else Input(UInt(0.W))
    val output     = Output(UInt(out_width.W))
  })

  def pad(in: UInt, width: Int): UInt = {
    if (in.getWidth == width) in
    else Cat(0.U((width - in.getWidth).W), in)
  }

  // Select the appropriate significand based on the input type
    val output =
      if (supportFP64 && supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f64.getWidth),
            Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f64.getWidth),
              pad(io.input_bf16, io.input_f64.getWidth))))
      } else if (supportFP64 && supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f64.getWidth),
            Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f64.getWidth), 0.U)))
      } else if (supportFP64 && supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f64.getWidth),
            Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f64.getWidth), 0.U)))
      } else if (supportFP64 && supportFP32 && !supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f64.getWidth), 0.U))
      } else if (supportFP64 && !supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f64.getWidth),
            Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f64.getWidth), 0.U)))
      } else if (supportFP64 && !supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f64.getWidth), 0.U))
      } else if (supportFP64 && !supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64,
          Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f64.getWidth), 0.U))
      } else if (supportFP64 && !supportFP32 && !supportFP16 && !supportBF16) {
        Mux(io.is_fp64.asBool, io.input_f64, 0.U)
      } else if (!supportFP64 && supportFP32 && supportFP16 && supportBF16) {
        Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f32.getWidth),
          Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f32.getWidth),
            Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f32.getWidth), 0.U)))
      } else if (!supportFP64 && supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f32.getWidth),
          Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f32.getWidth), 0.U))
      } else if (!supportFP64 && supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_fp32.asBool, pad(io.input_f32, io.input_f32.getWidth),
          Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f32.getWidth), 0.U))
      } else if (!supportFP64 && supportFP32 && !supportFP16 && !supportBF16) {
          Mux(io.is_fp32.asBool, io.input_f32, 0.U)
      } else if (!supportFP64 && !supportFP32 && supportFP16 && supportBF16) {
          Mux(io.is_fp16.asBool, pad(io.input_f16, io.input_f16.getWidth),
            Mux(io.is_bf16.asBool, pad(io.input_bf16, io.input_f16.getWidth), 0.U))
      } else if (!supportFP64 && !supportFP32 && supportFP16 && !supportBF16) {
        Mux(io.is_fp16.asBool, io.input_f16, 0.U)
      } else if (!supportFP64 && !supportFP32 && !supportFP16 && supportBF16) {
        Mux(io.is_bf16.asBool, io.input_bf16, 0.U)
      } else {
        0.U  // none supported
      }
      io.output := output
}

class BoothEncoderFP64FP32FP16BF16(
                                              width :Int,
                                              is_addend_expand_1bit : Boolean,
                                              supportFP64: Boolean,
                                              supportFP32: Boolean,
                                              supportFP16: Boolean,
                                              supportBF16: Boolean,
                                              ) extends Module{
  val outNum  = width/2 + 1
  val addend_seq_width      : Int = if(is_addend_expand_1bit) 2*width+1 else 2*width
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val is_fp64 = if(supportFP64) Input(Bool()) else Input(UInt(0.W))
    val is_fp32 = if(supportFP32) Input(Bool()) else Input(UInt(0.W))
    val is_fp16 = if(supportFP16) Input(Bool()) else Input(UInt(0.W))
    val is_bf16 = if(supportBF16) Input(Bool()) else Input(UInt(0.W))
    val out_pp = Output(Vec(outNum,UInt(addend_seq_width.W)))
  })

  val significandWidthFP64 : Int = 53
  val significandWidthFP32 : Int = 24
  val significandWidthFP16 : Int = 11
  val significandWidthBF16 : Int = 8

  val addend_seq_width_FP64 : Int = if(is_addend_expand_1bit) 2*significandWidthFP64+1 else 2*significandWidthFP64
  val addend_seq_width_FP32 : Int = if(is_addend_expand_1bit) 2*significandWidthFP32+1 else 2*significandWidthFP32 
  val addend_seq_width_FP16 : Int = if(is_addend_expand_1bit) 2*significandWidthFP16+1 else 2*significandWidthFP16
  val addend_seq_width_BF16 : Int = if(is_addend_expand_1bit) 2*significandWidthBF16+1 else 2*significandWidthBF16 

  val outNum_FP64 = significandWidthFP64/2 + 1
  val outNum_FP32 = significandWidthFP32/2 + 1
  val outNum_FP16 = significandWidthFP16/2 + 1
  val outNum_BF16 = significandWidthBF16/2 + 1
  val outNumBeforeLastFP64 = outNum_FP64-2
  val outNumLastFP64       = outNum_FP64-1
  val outNumBeforeLastFP32 = outNum_FP32-2
  val outNumLastFP32       = outNum_FP32-1
  val outNumBeforeLastFP16 = outNum_FP16-2
  val outNumLastFP16       = outNum_FP16-1
  val outNumBeforeLastBF16 = outNum_BF16-2
  val outNumLastBF16       = outNum_BF16-1

  val in_b_sel_fp64 = Cat(0.U((2-(significandWidthFP64 % 2)).W), io.in_b(significandWidthFP64-1,0), 0.U)
  val in_b_sel_fp32 = if(supportFP32) Cat(0.U((2-(significandWidthFP32 % 2)).W), io.in_b(significandWidthFP32-1,0), 0.U) else 0.U(0.W)
  val in_b_sel_fp16 = if(supportFP16) Cat(0.U((2-(significandWidthFP16 % 2)).W), io.in_b(significandWidthFP16-1,0), 0.U) else 0.U(0.W)
  val in_b_sel_bf16 = if(supportBF16) Cat(0.U((2-(significandWidthBF16 % 2)).W), io.in_b(significandWidthBF16-1,0), 0.U) else 0.U(0.W)

  val in_b_sel = Module(new InputMixedSeletor(supportFP64, supportFP32, supportFP16, supportBF16, in_b_sel_fp64.getWidth, in_b_sel_fp32.getWidth, in_b_sel_fp16.getWidth, in_b_sel_bf16.getWidth))
  if(supportFP64){
    in_b_sel.io.input_f64 := in_b_sel_fp64
    in_b_sel.io.is_fp64 := io.is_fp64
  }
  else{
    in_b_sel.io.input_f64 := 0.U
    in_b_sel.io.is_fp64   := 0.U
  }
  if(supportFP32){
    in_b_sel.io.input_f32 := in_b_sel_fp32
    in_b_sel.io.is_fp32 := io.is_fp32
  }
  else{
    in_b_sel.io.input_f32 := 0.U
    in_b_sel.io.is_fp32   := 0.U
  }
  if(supportFP16){
    in_b_sel.io.input_f16 := in_b_sel_fp16
    in_b_sel.io.is_fp16 := io.is_fp16
  }
  else{
    in_b_sel.io.input_f16 := 0.U
    in_b_sel.io.is_fp16   := 0.U
  }
  if(supportBF16){
    in_b_sel.io.input_bf16 := in_b_sel_bf16
    in_b_sel.io.is_bf16 := io.is_bf16
  }
  else{
    in_b_sel.io.input_bf16 := 0.U
    in_b_sel.io.is_bf16    := 0.U
  }

  val in_b_cat = in_b_sel.io.output
  //get booth encode
  val booth_seq = Wire(Vec(outNum,UInt(3.W)))
  val booth_4bit_onehot = Wire(Vec(outNum,UInt(4.W)))
  for (i <- 0 until outNum) {
    booth_seq(i) := in_b_cat(i*2+2,i*2)
    booth_4bit_onehot(i) := 0.U
    switch(booth_seq(i)){
      is("b001".U) {booth_4bit_onehot(i) := "b1000".U}
      is("b010".U) {booth_4bit_onehot(i) := "b1000".U}
      is("b011".U) {booth_4bit_onehot(i) := "b0100".U}
      is("b100".U) {booth_4bit_onehot(i) := "b0001".U}
      is("b101".U) {booth_4bit_onehot(i) := "b0010".U}
      is("b110".U) {booth_4bit_onehot(i) := "b0010".U}
    }
  }
  //generate partial products
  val pp_seq_f64  = if(supportFP64) Wire(Vec(outNum,UInt((significandWidthFP64+1).W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val pp_seq_f32  = if(supportFP32) Wire(Vec(outNum,UInt((significandWidthFP32+1).W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val pp_seq_f16  = if(supportFP16) Wire(Vec(outNum,UInt((significandWidthFP16+1).W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val pp_seq_bf16 = if(supportBF16) Wire(Vec(outNum,UInt((significandWidthBF16+1).W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val sign_seq = Wire(Vec(outNum,UInt(1.W)))

  for (i <- 0 until outNum) {
    sign_seq(i) := booth_4bit_onehot(i)(1) | booth_4bit_onehot(i)(0)
    
    //f64
    if(supportFP64){
      if(i<=outNum_FP64){
        pp_seq_f64(i) := 
        Fill(significandWidthFP64 + 1, booth_4bit_onehot(i)(3)) & Cat(0.U, io.in_a) |
          Fill(significandWidthFP64 + 1, booth_4bit_onehot(i)(2)) & Cat(io.in_a, 0.U) |
          Fill(significandWidthFP64 + 1, booth_4bit_onehot(i)(1)) & Cat(1.U, ~io.in_a) |
          Fill(significandWidthFP64 + 1, booth_4bit_onehot(i)(0)) & Cat(~io.in_a, 1.U)
      }
    }
    //f32 only low 13 is valid
    if(supportFP32){
      if (i<=outNum_FP32) {
        pp_seq_f32(i) :=
          Fill(significandWidthFP32+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(significandWidthFP32-1,0))  |
            Fill(significandWidthFP32+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(significandWidthFP32-1,0),0.U)  |
            Fill(significandWidthFP32+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(significandWidthFP32-1,0)) |
            Fill(significandWidthFP32+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(significandWidthFP32-1,0),1.U)
      }
      else {
        pp_seq_f32(i) := Fill(significandWidthFP32+1, 0.U)
      }
    }
    //fp16
    if(supportFP16){
      if (i<=outNum_FP16) {
        pp_seq_f16(i) :=
          Fill(significandWidthFP16+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(significandWidthFP16-1,0))  |
            Fill(significandWidthFP16+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(significandWidthFP16-1,0),0.U)  |
            Fill(significandWidthFP16+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(significandWidthFP16-1,0)) |
            Fill(significandWidthFP16+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(significandWidthFP16-1,0),1.U)
      }
      else {
        pp_seq_f16(i) := Fill(significandWidthFP16+1, 0.U)
      }
    }
    //bf16
    if(supportBF16){
      if (i<=outNum_BF16) {
        pp_seq_bf16(i) :=
          Fill(significandWidthBF16+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(significandWidthBF16-1,0))  |
            Fill(significandWidthBF16+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(significandWidthBF16-1,0),0.U)  |
            Fill(significandWidthBF16+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(significandWidthBF16-1,0)) |
            Fill(significandWidthBF16+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(significandWidthBF16-1,0),1.U)
      }
      else {
        pp_seq_bf16(i) := Fill(significandWidthBF16+1, 0.U)
      }
    }
  }
  val addend_seq_f64  = if(supportFP64) Wire(Vec(outNum,UInt(addend_seq_width_FP64.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_f32  = if(supportFP32) Wire(Vec(outNum,UInt(addend_seq_width_FP32.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_f16  = if(supportFP16) Wire(Vec(outNum,UInt(addend_seq_width_FP16.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_bf16 = if(supportBF16) Wire(Vec(outNum,UInt(addend_seq_width_BF16.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_f32_to_longer  = if(supportFP32) Wire(Vec(outNum, UInt(addend_seq_width.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_f16_to_longer  = if(supportFP16) Wire(Vec(outNum, UInt(addend_seq_width.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))
  val addend_seq_bf16_to_longer = if(supportBF16) Wire(Vec(outNum, UInt(addend_seq_width.W))) else WireInit(VecInit.fill(outNum)(0.U(0.W)))

  if(supportFP64){
    for (i <- 0 until outNum_FP64) {
      val head_first_one_width = significandWidthFP64 - 4 - 2 * (i - 1)
      val tail_zero_width = 2 * (i - 1)
      i match {
        case 0 => addend_seq_f64(i) := Cat(0.U((significandWidthFP64 - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f64(0))
        case 1 => addend_seq_f64(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1))
        case `outNumBeforeLastFP64` =>
          if (significandWidthFP64 % 2 == 0) {
            if (is_addend_expand_1bit) addend_seq_f64(i) := Cat(1.U, ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
            else addend_seq_f64(i) := Cat(~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          }
          else addend_seq_f64(i) := Cat(1.U, ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        case `outNumLastFP64` =>
          if (significandWidthFP64 % 2 == 0) addend_seq_f64(i) := Cat(pp_seq_f64(i).tail(1), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
          else if (is_addend_expand_1bit) addend_seq_f64(i) := Cat(1.U, pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          else addend_seq_f64(i) := Cat(pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
        case _ => addend_seq_f64(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      }
    }
  }
  if(supportFP32){
    for (i <- 0 until outNum_FP32) {
      val head_first_one_width = significandWidthFP32 - 4 - 2 * (i - 1)
      val tail_zero_width = 2 * (i - 1)
      i match {
        case 0 => addend_seq_f32(i) := Cat(0.U((significandWidthFP32 - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f32(0))
        case 1 => addend_seq_f32(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1))
        case `outNumBeforeLastFP32` =>
          if (significandWidthFP32 % 2 == 0) {
            if (is_addend_expand_1bit) addend_seq_f32(i) := Cat(1.U, ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
            else addend_seq_f32(i) := Cat(~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          }
          else addend_seq_f32(i) := Cat(1.U, ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        case `outNumLastFP32` =>
          if (significandWidthFP32 % 2 == 0) addend_seq_f32(i) := Cat(pp_seq_f32(i).tail(1), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
          else if (is_addend_expand_1bit) addend_seq_f32(i) := Cat(1.U, pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          else addend_seq_f32(i) := Cat(pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
        case _ => addend_seq_f32(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      }
    }  
    for (i <- outNum_FP32 until outNum){
      addend_seq_f32(i) := 0.U
    }
  }
  if(supportFP16){
    for (i <- 0 until outNum_FP16) {
      val head_first_one_width = significandWidthFP16 - 4 - 2 * (i - 1)
      val tail_zero_width = 2 * (i - 1)
      i match {
        case 0 => addend_seq_f16(i) := Cat(0.U((significandWidthFP16 - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f16(0))
        case 1 => addend_seq_f16(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1))
        case `outNumBeforeLastFP16` =>
          if (significandWidthFP16 % 2 == 0) {
            if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
            else addend_seq_f16(i) := Cat(~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          }
          else addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        case `outNumLastFP16` =>
          if (significandWidthFP16 % 2 == 0) addend_seq_f16(i) := Cat(pp_seq_f16(i).tail(1), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
          else if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          else addend_seq_f16(i) := Cat(pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
        case _ => addend_seq_f16(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      }
    }  
    for (i <- outNum_FP16 until outNum){
      addend_seq_f16(i) := 0.U
    }
  }
  if(supportBF16){
    for (i <- 0 until outNum_BF16) {
      val head_first_one_width = significandWidthBF16 - 4 - 2 * (i - 1)
      val tail_zero_width = 2 * (i - 1)
      i match {
        case 0 => addend_seq_bf16(i) := Cat(0.U((significandWidthBF16 - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_bf16(0))
        case 1 => addend_seq_bf16(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_bf16(i), 0.U, sign_seq(i - 1))
        case `outNumBeforeLastBF16` =>
          if (significandWidthBF16 % 2 == 0) {
            if (is_addend_expand_1bit) addend_seq_bf16(i) := Cat(1.U, ~sign_seq(i), pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
            else addend_seq_bf16(i) := Cat(~sign_seq(i), pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          }
          else addend_seq_bf16(i) := Cat(1.U, ~sign_seq(i), pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        case `outNumLastBF16` =>
          if (significandWidthBF16 % 2 == 0) addend_seq_bf16(i) := Cat(pp_seq_bf16(i).tail(1), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
          else if (is_addend_expand_1bit) addend_seq_bf16(i) := Cat(1.U, pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          else addend_seq_bf16(i) := Cat(pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
        case _ => addend_seq_bf16(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_bf16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      }
    }  
    for (i <- outNum_BF16 until outNum){
      addend_seq_bf16(i) := 0.U
    }
  }
  if(supportFP32){
    if (addend_seq_width_FP32 < addend_seq_width) {
      for (i <- 0 until outNum) {
        addend_seq_f32_to_longer(i) := Cat(0.U((addend_seq_width - addend_seq_width_FP32).W), addend_seq_f32(i))
      }
    }
    else addend_seq_f32_to_longer := addend_seq_f32
  }
  if(supportFP16){
    if (addend_seq_width_FP16 < addend_seq_width) {
      for (i <- 0 until outNum) {
        addend_seq_f16_to_longer(i) := Cat(0.U((addend_seq_width - addend_seq_width_FP16).W), addend_seq_f16(i))
      }
    }
    else addend_seq_f16_to_longer := addend_seq_f16
  }
  if(supportBF16){
    if (addend_seq_width_BF16 < addend_seq_width) {
      for (i <- 0 until outNum) {
        addend_seq_bf16_to_longer(i) := Cat(0.U((addend_seq_width - addend_seq_width_BF16).W), addend_seq_bf16(i))
      }
    }
    else addend_seq_bf16_to_longer := addend_seq_bf16
  }

  val out_pp_sel = Module(new VectorInputMixedSeletor(supportFP64, supportFP32, supportFP16, supportBF16, addend_seq_width, outNum))
  if(supportFP64){
    out_pp_sel.io.input_f64 := addend_seq_f64
    out_pp_sel.io.is_fp64   := io.is_fp64
  }
  else{
    out_pp_sel.io.input_f64 := 0.U
    out_pp_sel.io.is_fp64   := 0.U
  }
  if(supportFP32){
    out_pp_sel.io.input_f32 := addend_seq_f32_to_longer
    out_pp_sel.io.is_fp32   := io.is_fp32
  }
  else{
    out_pp_sel.io.input_f32 := 0.U
    out_pp_sel.io.is_fp32   := 0.U
  }
  if(supportFP16){
    out_pp_sel.io.input_f16 := addend_seq_f16_to_longer
    out_pp_sel.io.is_fp16   := io.is_fp16
  }
  else{
    out_pp_sel.io.input_f16 := 0.U
    out_pp_sel.io.is_fp16   := 0.U
  }
  if(supportBF16){
    out_pp_sel.io.input_bf16 := addend_seq_bf16_to_longer
    out_pp_sel.io.is_bf16    := io.is_bf16
  }
  else{
    out_pp_sel.io.input_bf16 := 0.U
    out_pp_sel.io.is_bf16    := 0.U
  }

  io.out_pp := out_pp_sel.io.output
}

class OutputGenerator(
  exponentWidth : Int,
  significandWidth: Int,
  )extends Module {
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle {
    val round_mode                        = Input(UInt(3.W))
    val adder_lowbit                      = Input(UInt((significandWidth*2+1).W))
    val is_fmul                           = Input(Bool())
    val fp_c                              = Input(UInt(floatWidth.W))
    val fp_c_rshift_result_high_inv_add1  = Input(UInt((significandWidth+3).W))
    val fp_c_rshift_result_high_inv_add0  = Input(UInt((significandWidth+3).W))
    val Ec_fix                            = Input(UInt(exponentWidth.W))
    val rshift_guard                      = Input(UInt(1.W))
    val rshift_round                      = Input(UInt(1.W))
    val rshift_sticky                     = Input(UInt(1.W))
    val Eab                               = Input(SInt((exponentWidth+2).W))
    val rshift_value                      = Input(SInt((exponentWidth+2).W))
    val fp_aIsFpCanonicalNAN              = Input(Bool())
    val fp_bIsFpCanonicalNAN              = Input(Bool())
    val fp_cIsFpCanonicalNAN              = Input(Bool())
    val fp_a_significand                  = Input(UInt(significandWidth.W))
    val fp_b_significand                  = Input(UInt(significandWidth.W))
    val fp_c_significand                  = Input(UInt(significandWidth.W))
    val Ea                                = Input(UInt(exponentWidth.W))
    val Eb                                = Input(UInt(exponentWidth.W))
    val Ec                                = Input(UInt(exponentWidth.W))
    val sign_c                            = Input(Bool())
    val sign_a_b                          = Input(Bool())  
    val is_sub_reg0                       = Input(Bool())
    val fire                              = Input(Bool())
    val fire_reg0                         = Input(Bool())
    val fire_reg1                         = Input(Bool())
    val result                            = Output(UInt(floatWidth.W))
    val fflags                            = Output(UInt(5.W))
  })

  val fire          = io.fire
  val fire_reg0     = io.fire_reg0
  val fire_reg1     = io.fire_reg1
  val rshift_value  = io.rshift_value
  val Eab           = io.Eab
  val Ec_fix        = io.Ec_fix
  val sign_c        = io.sign_c
  val sign_a_b      = io.sign_a_b
  val round_mode    = io.round_mode
  val fp_aIsFpCanonicalNAN = io.fp_aIsFpCanonicalNAN
  val fp_bIsFpCanonicalNAN = io.fp_bIsFpCanonicalNAN
  val fp_cIsFpCanonicalNAN = io.fp_cIsFpCanonicalNAN
  val fp_a_significand = io.fp_a_significand
  val fp_b_significand = io.fp_b_significand
  val fp_c_significand = io.fp_c_significand
  val Ea = io.Ea
  val Eb = io.Eb
  val Ec = io.Ec
  val is_fmul = io.is_fmul
  val fp_c = io.fp_c

  def shiftLeftWithMux(srcValue: UInt, shiftValue: UInt): UInt = {
    val vecLength  = shiftValue.getWidth + 1
    val res_vec    = Wire(Vec(vecLength,UInt(srcValue.getWidth.W)))
    res_vec(0)    := srcValue
    for (i <- 0 until shiftValue.getWidth) {
      res_vec(i+1) := Mux(shiftValue(shiftValue.getWidth-1-i), res_vec(i) << (1<<(shiftValue.getWidth-1-i)), res_vec(i))
    }
    res_vec(vecLength-1)
  }


  val fp_c_rshift_result_high_inv_add1_fp = io.fp_c_rshift_result_high_inv_add1
  val adder       = Cat(Mux(io.adder_lowbit.head(1).asBool, fp_c_rshift_result_high_inv_add1_fp, io.fp_c_rshift_result_high_inv_add0),io.adder_lowbit.tail(1),
    Mux(io.is_sub_reg0, ((~Cat(io.rshift_guard,io.rshift_round,io.rshift_sticky)).asUInt+1.U).head(2), Cat(io.rshift_guard,io.rshift_round))
  )

  val adder_is_negative = adder.head(1).asBool
  val adder_is_negative_reg2 = RegEnable(RegEnable(adder_is_negative, fire_reg0), fire_reg1)

  val adder_inv       = Mux(adder_is_negative, (~adder.tail(1)).asUInt, adder.tail(1))
  // ab mul is greater then c
  val Eab_is_greater    = rshift_value > 0.S
  val E_greater_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater, Eab(exponentWidth,0).asUInt, Cat(0.U(1.W),Ec_fix)), fire), fire_reg0), fire_reg1)
  val lshift_value_max_reg0 = RegEnable(Mux(Eab_is_greater, Eab(exponentWidth,0).asUInt - 1.U, Cat(0.U,Ec_fix - 1.U)), fire)
  val LZDWidth = adder_inv.getWidth.U.getWidth

  //guard the exponent not be zero after lshift
  val lshift_value_mask = Mux(lshift_value_max_reg0.head(lshift_value_max_reg0.getWidth-LZDWidth).orR,
    0.U(adder_inv.getWidth.W),
    Fill(adder_inv.getWidth, 1.U) >> lshift_value_max_reg0.tail(lshift_value_max_reg0.getWidth-LZDWidth)
  ).asUInt

  //tail
  val tzd_adder_reg1     = LZD(RegEnable(Reverse(adder.asUInt), fire_reg0).asTypeOf(adder))
  val lzd_adder_inv_mask  = LZD(RegEnable(adder_inv | lshift_value_mask, fire_reg0).asTypeOf(adder_inv))

  val lzd_adder_inv_mask_reg1  = Wire(UInt(lzd_adder_inv_mask.getWidth.W))
  lzd_adder_inv_mask_reg1 := lzd_adder_inv_mask

  val lshift_mask_valid_reg1   = (RegEnable(adder_inv, fire_reg0) | RegEnable(lshift_value_mask, fire_reg0)) === RegEnable(lshift_value_mask, fire_reg0)
  val lshift_value_reg1        = lzd_adder_inv_mask_reg1
  val adder_reg1 = RegEnable(adder, fire_reg0)

  // left shift for norm
  val lshift_adder        = shiftLeftWithMux(adder_reg1, lshift_value_reg1)
  val lshift_adder_inv    = Cat(Mux(RegEnable(adder_is_negative, fire_reg0),~lshift_adder.head(significandWidth+4),lshift_adder.head(significandWidth+4)),lshift_adder.tail(significandWidth+4))

  val is_fix = (tzd_adder_reg1 + lzd_adder_inv_mask_reg1) === adder_inv.getWidth.U
  //after fix
  val lshift_adder_inv_fix = Mux(is_fix, lshift_adder_inv.head(adder_inv.getWidth), lshift_adder_inv.tail(1))
  val fraction_result_no_round_reg2  = RegEnable(lshift_adder_inv_fix.tail(1).head(significandWidth-1), fire_reg1)

  val fraction_result_round = fraction_result_no_round_reg2 +& 1.U

  val sign_result_temp_reg2 = RegEnable(RegEnable(Mux(adder_is_negative, RegEnable(sign_c, fire), RegEnable(sign_a_b, fire)), fire_reg0), fire_reg1)

  val RNE = io.round_mode === "b000".U
  val RTZ = io.round_mode === "b001".U
  val RDN = io.round_mode === "b010".U
  val RUP = io.round_mode === "b011".U
  val RMM = io.round_mode === "b100".U
  val RNE_reg2 = RegEnable(RegEnable(RegEnable(RNE, fire), fire_reg0), fire_reg1)
  val RTZ_reg2 = RegEnable(RegEnable(RegEnable(RTZ, fire), fire_reg0), fire_reg1)
  val RDN_reg2 = RegEnable(RegEnable(RegEnable(RDN, fire), fire_reg0), fire_reg1)
  val RUP_reg2 = RegEnable(RegEnable(RegEnable(RUP, fire), fire_reg0), fire_reg1)
  val RMM_reg2 = RegEnable(RegEnable(RegEnable(RMM, fire), fire_reg0), fire_reg1)

  val sticky_reg2  = RegEnable(RegEnable(io.rshift_sticky, fire_reg0) | (lzd_adder_inv_mask_reg1 + tzd_adder_reg1 < (adder_inv.getWidth-significandWidth-2).U), fire_reg1)
  val sticky_uf_reg2  = RegEnable(RegEnable(io.rshift_sticky, fire_reg0) | (lzd_adder_inv_mask_reg1 + tzd_adder_reg1 < (adder_inv.getWidth-significandWidth-3).U), fire_reg1)
  val round_lshift_reg2 = RegEnable(lshift_adder_inv_fix.tail(significandWidth+1).head(1), fire_reg1)
  val guard_lshift_reg2 = RegEnable(lshift_adder_inv_fix.tail(significandWidth).head(1), fire_reg1)

  val round   = Mux(adder_is_negative_reg2, round_lshift_reg2 ^ !sticky_reg2, round_lshift_reg2)
  val guard   = Mux(adder_is_negative_reg2, guard_lshift_reg2 ^ (!sticky_reg2 & round_lshift_reg2), guard_lshift_reg2)

  val guard_uf   = round
  
  val round_lshift_uf_reg2 = RegEnable(lshift_adder_inv_fix.tail(significandWidth+2).head(1), fire_reg1)
  val round_uf   = Mux(adder_is_negative_reg2, round_lshift_uf_reg2 ^ !sticky_uf_reg2, round_lshift_uf_reg2)

  val round_add1 = Wire(UInt(1.W))
  round_add1 := RNE_reg2 & (guard & (fraction_result_no_round_reg2(0) | round | sticky_reg2)) |
    RDN_reg2 & sign_result_temp_reg2 & (guard|round|sticky_reg2) |
    RUP_reg2 & !sign_result_temp_reg2 & (guard|round|sticky_reg2) |
    RMM_reg2 & guard |
    adder_is_negative_reg2 & !guard & !round & !sticky_reg2

  val round_add1_uf = RNE_reg2 & (guard_uf & (guard | round_uf | sticky_uf_reg2)) |
    RDN_reg2 & sign_result_temp_reg2 & (guard_uf|round_uf|sticky_uf_reg2) |
    RUP_reg2 & !sign_result_temp_reg2 & (guard_uf|round_uf|sticky_uf_reg2) |
    RMM_reg2 & guard_uf

  val exponent_add_1 = fraction_result_no_round_reg2.andR & round_add1.asBool

  val exponent_result_add_value = Mux(exponent_add_1 | RegEnable(is_fix, fire_reg1),
    E_greater_reg2 - RegEnable(lshift_value_reg1, fire_reg1) + 1.U,
    E_greater_reg2 - RegEnable(lshift_value_reg1, fire_reg1)
  )

  val exponent_overflow         = exponent_result_add_value.head(1).asBool | exponent_result_add_value.tail(1).andR
  val exponent_is_min           = RegEnable(!lshift_adder_inv_fix.head(1).asBool & lshift_mask_valid_reg1 & !is_fix, fire_reg1)

  val exponent_result_temp      = Mux(exponent_is_min,
    Cat(0.U((exponentWidth-1).W),exponent_add_1),
    exponent_result_add_value(exponentWidth-1,0)
  )

  val fraction_result_temp  = Mux(round_add1.asBool, fraction_result_round.tail(1), fraction_result_no_round_reg2)

  val NV,DZ,OF,UF,NX  = WireInit(false.B)

  val fflags = WireInit(Cat(NV,DZ,OF,UF,NX))

  NX := guard | round | sticky_reg2
  UF := NX & exponent_is_min & ( !exponent_add_1 | !(guard & round_add1_uf))


  val fp_a_is_zero = !io.fp_aIsFpCanonicalNAN & !fp_a_significand.orR
  val fp_b_is_zero = !io.fp_bIsFpCanonicalNAN & !fp_b_significand.orR
  val fp_c_is_zero = !io.fp_cIsFpCanonicalNAN & !fp_c_significand.orR

  val normal_result_is_zero_reg2 = RegEnable(RegEnable(!adder.orR, fire_reg0), fire_reg1)
  val has_zero_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero | fp_b_is_zero | fp_c_is_zero, fire), fire_reg0), fire_reg1) | normal_result_is_zero_reg2
  val normal_result = Cat(sign_result_temp_reg2,exponent_result_temp,fraction_result_temp)

  val is_overflow_reg2 = exponent_overflow

  val result_overflow_up   = Cat(sign_result_temp_reg2, Fill(exponentWidth,1.U), 0.U((significandWidth-1).W))
  val result_overflow_down = Cat(sign_result_temp_reg2, Fill(exponentWidth-1,1.U), 0.U, Fill(significandWidth-1,1.U))

  val fp_a_is_nan = io.fp_aIsFpCanonicalNAN | Ea.andR & fp_a_significand.tail(1).orR
  val fp_b_is_nan = io.fp_bIsFpCanonicalNAN | Eb.andR & fp_b_significand.tail(1).orR
  val fp_c_is_nan = io.fp_cIsFpCanonicalNAN | Ec.andR & fp_c_significand.tail(1).orR

  val has_nan = fp_a_is_nan | fp_b_is_nan | fp_c_is_nan

  val fp_a_is_snan =  !io.fp_aIsFpCanonicalNAN & Ea.andR & !fp_a_significand.tail(1).head(1) & fp_a_significand.tail(2).orR
  val fp_b_is_snan =  !io.fp_bIsFpCanonicalNAN & Eb.andR & !fp_b_significand.tail(1).head(1) & fp_b_significand.tail(2).orR
  val fp_c_is_snan =  !io.fp_cIsFpCanonicalNAN & Ec.andR & !fp_c_significand.tail(1).head(1) & fp_c_significand.tail(2).orR
  
  val has_snan = fp_a_is_snan | fp_b_is_snan | fp_c_is_snan

  val fp_a_is_inf = !io.fp_aIsFpCanonicalNAN & Ea.andR & !fp_a_significand.tail(1).orR
  val fp_b_is_inf = !io.fp_bIsFpCanonicalNAN & Eb.andR & !fp_b_significand.tail(1).orR
  val fp_c_is_inf = !io.fp_cIsFpCanonicalNAN & Ec.andR & !fp_c_significand.tail(1).orR

  val has_inf = fp_a_is_inf | fp_b_is_inf | fp_c_is_inf

  val result_inf = Cat(Fill(exponentWidth,1.U),Fill(significandWidth-1,0.U))

  val result_nan = Cat(0.U,Fill(exponentWidth+1,1.U),0.U((significandWidth-2).W))

  val fp_result = Wire(UInt(floatWidth.W))

  val has_nan_reg2       = RegEnable(RegEnable(RegEnable(has_nan, fire), fire_reg0), fire_reg1)
  val has_nan_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan.asBool | (fp_a_is_inf & fp_b_is_zero) | (fp_a_is_zero & fp_b_is_inf),
    fire), fire_reg0), fire_reg1)
  val has_inf_reg2       = RegEnable(RegEnable(RegEnable(has_inf, fire), fire_reg0), fire_reg1)
  val has_inf_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf & fp_b_is_zero) | (fp_a_is_zero & fp_b_is_inf)) | (fp_c_is_inf & (fp_a_is_inf | fp_b_is_inf) & (sign_c ^ sign_a_b)),
    fire), fire_reg0), fire_reg1)
  val has_inf_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf|fp_b_is_inf,sign_a_b,sign_c),
    fire), fire_reg0), fire_reg1)
  val is_overflow_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_reg2.asBool) | (RUP_reg2 & sign_result_temp_reg2.asBool)
  val fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero | fp_b_is_zero, fire), fire_reg0), fire_reg1)
  val fp_result_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero,
        Mux(is_fmul, sign_a_b, (sign_a_b & sign_c) | (RDN & (sign_a_b ^ sign_c)) ),
        fp_c.head(1)
      ),
      fp_c.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_reg2){
    fp_result := result_nan
    fflags := Mux(has_nan_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_reg2){
    fp_result := Mux(has_inf_is_NV_reg2, result_nan, Cat(has_inf_result_inf_sign_reg2,result_inf))
    fflags := Mux(has_inf_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_reg2){
    fp_result := Mux(is_overflow_down_reg2,result_overflow_down,result_overflow_up)
    fflags := "b00101".U
  }.elsewhen(has_zero_reg2){
    fp_result := Mux(fp_a_or_b_is_zero_reg2,
      fp_result_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_reg2, Cat(RDN_reg2,0.U((floatWidth-1).W)), normal_result)
    )
    fflags := Mux(fp_a_or_b_is_zero_reg2 | normal_result_is_zero_reg2, 0.U, Cat(NV,DZ,OF,UF,NX))
  }.otherwise{
    fp_result := normal_result
  }

  io.result := fp_result
  io.fflags := fflags
}


class CSA3to2Mixed(width :Int) extends Module{
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val in_c   = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))

  })
  io.out_sum := io.in_a ^ io.in_b ^ io.in_c
  io.out_car := Cat( ( (io.in_a & io.in_b) | (io.in_a & io.in_c) | (io.in_b & io.in_c) )(width-2,0),0.U)

}