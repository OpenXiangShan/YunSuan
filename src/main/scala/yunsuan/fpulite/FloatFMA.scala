package yunsuan.fpulite
import chisel3._
import chisel3.util._
import yunsuan.FmaOpCode
import yunsuan.util.GatedValidRegNext
import yunsuan.VfmaOpCode
import yunsuan.vector._

import scala.collection.mutable.ListBuffer
class FloatFMA() extends Module{
  val exponentWidth : Int = 11
  val significandWidth : Int = 53
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fire                 = Input (Bool())
    val fp_a, fp_b, fp_c     = Input (UInt(floatWidth.W))  // fp_a->VS2,fp_b->VS1,fp_c->VD
    val round_mode           = Input (UInt(3.W))
    val fp_format            = Input (UInt(2.W)) // result format b01->fp16,b10->fp32,b11->fp64
    val op_code              = Input (UInt(4.W))
    val fp_result            = Output(UInt(floatWidth.W))
    val fflags               = Output(UInt(5.W))
    val fp_aIsFpCanonicalNAN = Input(Bool())
    val fp_bIsFpCanonicalNAN = Input(Bool())
    val fp_cIsFpCanonicalNAN = Input(Bool())
  })
  val printfen: Boolean = false
  def shiftLeftWithMux(srcValue: UInt, shiftValue: UInt): UInt = {
    val vecLength  = shiftValue.getWidth + 1
    val res_vec    = Wire(Vec(vecLength,UInt(srcValue.getWidth.W)))
    res_vec(0)    := srcValue
    for (i <- 0 until shiftValue.getWidth) {
      res_vec(i+1) := Mux(shiftValue(shiftValue.getWidth-1-i), res_vec(i) << (1<<(shiftValue.getWidth-1-i)), res_vec(i))
    }
    res_vec(vecLength-1)
  }

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
  def printfEn(pable: Printable): Unit ={
    if (printfen) printf(pable)
  }
  val fire = io.fire
  val fire_reg0 = GatedValidRegNext(fire)
  val fire_reg1 = GatedValidRegNext(fire_reg0)
  val is_vfmul   = io.op_code === VfmaOpCode.vfmul
  val is_vfmacc  = io.op_code === VfmaOpCode.vfmacc
  val is_vfnmacc = io.op_code === VfmaOpCode.vfnmacc
  val is_vfmsac  = io.op_code === VfmaOpCode.vfmsac
  val is_vfnmsac = io.op_code === VfmaOpCode.vfnmsac
  val is_vfmadd  = io.op_code === VfmaOpCode.vfmadd
  val is_vfnmadd = io.op_code === VfmaOpCode.vfnmadd
  val is_vfmsub  = io.op_code === VfmaOpCode.vfmsub
  val is_vfnmsub = io.op_code === VfmaOpCode.vfnmsub
  val is_fp64                 = io.fp_format === 3.U(2.W)
  val is_fp64_reg0            = RegEnable(is_fp64, fire)
  val is_fp64_reg1            = RegEnable(is_fp64_reg0, fire_reg0)
  val is_fp64_reg2            = RegEnable(is_fp64_reg1, fire_reg1)
  val is_fp32                 = io.fp_format === 2.U(2.W)
  val is_fp32_reg0            = RegEnable(is_fp32, fire)
  val is_fp32_reg1            = RegEnable(is_fp32_reg0, fire_reg0)
  val is_fp32_reg2            = RegEnable(is_fp32_reg1, fire_reg1)
  def sign_inv(src: UInt,sel:Bool): UInt = {
    Cat(Mux(sel,~src.head(1),src.head(1)),src.tail(1))
  }
  val fp_a_is_sign_inv = is_vfnmacc || is_vfnmsac || is_vfnmadd || is_vfnmsub
  val fp_c_is_sign_inv = is_vfnmacc || is_vfmsac || is_vfnmadd || is_vfmsub
  val swap_fp_a_fp_c = is_vfmadd || is_vfnmadd || is_vfmsub || is_vfnmsub
  val fp_a_f64                = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(63,0),io.fp_a(63,0)),fp_a_is_sign_inv)
  val fp_b_f64                = io.fp_b(63,0)
  val fp_c_f64                = Mux(is_vfmul,0.U(64.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(63,0),io.fp_c(63,0)),fp_c_is_sign_inv))
  val fp_a_f32_0              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(31,0 ),io.fp_a(31,0 )),fp_a_is_sign_inv)
  val fp_a_f32_1              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(63,32),io.fp_a(63,32)),fp_a_is_sign_inv)
  val fp_b_f32_0              = io.fp_b(31,0 )
  val fp_b_f32_1              = io.fp_b(63,32)
  val fp_c_f32_0              = Mux(is_vfmul,0.U(32.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(31,0 ),io.fp_c(31,0 )),fp_c_is_sign_inv))
  val fp_c_f32_1              = Mux(is_vfmul,0.U(32.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(63,32),io.fp_c(63,32)),fp_c_is_sign_inv))
  val fp_a_f16_0              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(15,0 ),io.fp_a(15,0 )),fp_a_is_sign_inv)
  val fp_a_f16_1              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(31,16),io.fp_a(31,16)),fp_a_is_sign_inv)
  val fp_a_f16_2              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(47,32),io.fp_a(47,32)),fp_a_is_sign_inv)
  val fp_a_f16_3              = sign_inv(Mux(swap_fp_a_fp_c,io.fp_c(63,48),io.fp_a(63,48)),fp_a_is_sign_inv)
  val fp_b_f16_0              = io.fp_b(15,0 )
  val fp_b_f16_1              = io.fp_b(31,16)
  val fp_b_f16_2              = io.fp_b(47,32)
  val fp_b_f16_3              = io.fp_b(63,48)
  val fp_c_f16_0              = Mux(is_vfmul,0.U(16.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(15,0 ),io.fp_c(15,0 )),fp_c_is_sign_inv))
  val fp_c_f16_1              = Mux(is_vfmul,0.U(16.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(31,16),io.fp_c(31,16)),fp_c_is_sign_inv))
  val fp_c_f16_2              = Mux(is_vfmul,0.U(16.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(47,32),io.fp_c(47,32)),fp_c_is_sign_inv))
  val fp_c_f16_3              = Mux(is_vfmul,0.U(16.W),sign_inv(Mux(swap_fp_a_fp_c,io.fp_a(63,48),io.fp_c(63,48)),fp_c_is_sign_inv))
  val sign_a_b_f16_0          = (fp_a_f16_0.head(1) ^ fp_b_f16_0.head(1)).asBool
  val sign_a_b_f16_1          = (fp_a_f16_1.head(1) ^ fp_b_f16_1.head(1)).asBool
  val sign_a_b_f16_2          = (fp_a_f16_2.head(1) ^ fp_b_f16_2.head(1)).asBool
  val sign_a_b_f16_3          = (fp_a_f16_3.head(1) ^ fp_b_f16_3.head(1)).asBool


  val sign_a_b_f32_0          = (fp_a_f32_0.head(1) ^ fp_b_f32_0.head(1)).asBool
  val sign_a_b_f32_1          = (fp_a_f32_1.head(1) ^ fp_b_f32_1.head(1)).asBool
  val sign_a_b_f64            = (fp_a_f64(63) ^ fp_b_f64(63)).asBool
  val sign_c_f64              = fp_c_f64(63).asBool
  val sign_c_f32_0            = fp_c_f32_0.head(1).asBool
  val sign_c_f32_1            = fp_c_f32_1.head(1).asBool
  val sign_c_f16_0            = fp_c_f16_0.head(1).asBool
  val sign_c_f16_1            = fp_c_f16_1.head(1).asBool
  val sign_c_f16_2            = fp_c_f16_2.head(1).asBool
  val sign_c_f16_3            = fp_c_f16_3.head(1).asBool
  val is_sub_f64              = sign_a_b_f64 ^ sign_c_f64
  val is_sub_f64_reg0         = RegEnable(is_sub_f64, fire)
  val is_sub_f64_reg1         = RegEnable(is_sub_f64_reg0, fire_reg0)
  val is_sub_f64_reg2         = RegEnable(is_sub_f64_reg1, fire_reg1)
  val is_sub_f32_0            = sign_a_b_f32_0 ^ sign_c_f32_0
  val is_sub_f32_1            = sign_a_b_f32_1 ^ sign_c_f32_1
  val is_sub_f16_0            = sign_a_b_f16_0 ^ sign_c_f16_0
  val is_sub_f16_1            = sign_a_b_f16_1 ^ sign_c_f16_1
  val is_sub_f16_2            = sign_a_b_f16_2 ^ sign_c_f16_2
  val is_sub_f16_3            = sign_a_b_f16_3 ^ sign_c_f16_3
  val Ea_f64                  = fp_a_f64.tail(1).head(exponentWidth)
  val Eb_f64                  = fp_b_f64.tail(1).head(exponentWidth)
  val Ec_f64                  = fp_c_f64.tail(1).head(exponentWidth)
  val Ea_f32_0                = fp_a_f32_0(30,23)
  val Eb_f32_0                = fp_b_f32_0(30,23)
  val Ec_f32_0                = fp_c_f32_0(30,23)
  val Ea_f32_1                = fp_a_f32_1(30,23)
  val Eb_f32_1                = fp_b_f32_1(30,23)
  val Ec_f32_1                = fp_c_f32_1(30,23)
  val Ea_f16_0                = fp_a_f16_0(14,10)
  val Eb_f16_0                = fp_b_f16_0(14,10)
  val Ec_f16_0                = fp_c_f16_0(14,10)
  val Ea_f16_1                = fp_a_f16_1(14,10)
  val Eb_f16_1                = fp_b_f16_1(14,10)
  val Ec_f16_1                = fp_c_f16_1(14,10)
  val Ea_f16_2                = fp_a_f16_2(14,10)
  val Eb_f16_2                = fp_b_f16_2(14,10)
  val Ec_f16_2                = fp_c_f16_2(14,10)
  val Ea_f16_3                = fp_a_f16_3(14,10)
  val Eb_f16_3                = fp_b_f16_3(14,10)
  val Ec_f16_3                = fp_c_f16_3(14,10)
  val Ea_f64_is_not_zero      = Ea_f64.orR
  val Eb_f64_is_not_zero      = Eb_f64.orR
  val Ec_f64_is_not_zero      = Ec_f64.orR
  val Ea_f32_0_is_not_zero    = Ea_f32_0.orR
  val Eb_f32_0_is_not_zero    = Eb_f32_0.orR
  val Ec_f32_0_is_not_zero    = Ec_f32_0.orR
  val Ea_f32_1_is_not_zero    = Ea_f32_1.orR
  val Eb_f32_1_is_not_zero    = Eb_f32_1.orR
  val Ec_f32_1_is_not_zero    = Ec_f32_1.orR
  val Ea_f16_0_is_not_zero    = Ea_f16_0.orR
  val Eb_f16_0_is_not_zero    = Eb_f16_0.orR
  val Ec_f16_0_is_not_zero    = Ec_f16_0.orR
  val Ea_f16_1_is_not_zero    = Ea_f16_1.orR
  val Eb_f16_1_is_not_zero    = Eb_f16_1.orR
  val Ec_f16_1_is_not_zero    = Ec_f16_1.orR
  val Ea_f16_2_is_not_zero    = Ea_f16_2.orR
  val Eb_f16_2_is_not_zero    = Eb_f16_2.orR
  val Ec_f16_2_is_not_zero    = Ec_f16_2.orR
  val Ea_f16_3_is_not_zero    = Ea_f16_3.orR
  val Eb_f16_3_is_not_zero    = Eb_f16_3.orR
  val Ec_f16_3_is_not_zero    = Ec_f16_3.orR
  val fp_a_significand_f16_0  = Cat(Ea_f16_0_is_not_zero,fp_a_f16_0(9,0))
  val fp_b_significand_f16_0  = Cat(Eb_f16_0_is_not_zero,fp_b_f16_0(9,0))
  val fp_c_significand_f16_0  = Cat(Ec_f16_0_is_not_zero,fp_c_f16_0(9,0))
  val fp_a_significand_f16_1  = Cat(Ea_f16_1_is_not_zero,fp_a_f16_1(9,0))
  val fp_b_significand_f16_1  = Cat(Eb_f16_1_is_not_zero,fp_b_f16_1(9,0))
  val fp_c_significand_f16_1  = Cat(Ec_f16_1_is_not_zero,fp_c_f16_1(9,0))
  val fp_a_significand_f16_2  = Cat(Ea_f16_2_is_not_zero,fp_a_f16_2(9,0))
  val fp_b_significand_f16_2  = Cat(Eb_f16_2_is_not_zero,fp_b_f16_2(9,0))
  val fp_c_significand_f16_2  = Cat(Ec_f16_2_is_not_zero,fp_c_f16_2(9,0))
  val fp_a_significand_f16_3  = Cat(Ea_f16_3_is_not_zero,fp_a_f16_3(9,0))
  val fp_b_significand_f16_3  = Cat(Eb_f16_3_is_not_zero,fp_b_f16_3(9,0))
  val fp_c_significand_f16_3  = Cat(Ec_f16_3_is_not_zero,fp_c_f16_3(9,0))
  val fp_a_significand_f32_0  = Cat(Ea_f32_0_is_not_zero,fp_a_f32_0(22,0))
  val fp_b_significand_f32_0  = Cat(Eb_f32_0_is_not_zero,fp_b_f32_0(22,0))
  val fp_c_significand_f32_0  = Cat(Ec_f32_0_is_not_zero,fp_c_f32_0(22,0))
  val fp_a_significand_f32_1  = Cat(Ea_f32_1_is_not_zero,fp_a_f32_1(22,0))
  val fp_b_significand_f32_1  = Cat(Eb_f32_1_is_not_zero,fp_b_f32_1(22,0))
  val fp_c_significand_f32_1  = Cat(Ec_f32_1_is_not_zero,fp_c_f32_1(22,0))
  val fp_a_significand_f64    = Cat(Ea_f64_is_not_zero,fp_a_f64.tail(exponentWidth+1))
  val fp_b_significand_f64    = Cat(Eb_f64_is_not_zero,fp_b_f64.tail(exponentWidth+1))
  val fp_c_significand_f64    = Cat(Ec_f64_is_not_zero,fp_c_f64.tail(exponentWidth+1))
  val rshiftBasicF64          = significandWidth + 3
  val rshiftMaxF64            = 3*significandWidth + 4
  val rshiftBasicF32          = 24 + 3
  val rshiftMaxF32            = 3*24 + 4
  val rshiftBasicF16          = 11 + 3
  val rshiftMaxF16            = 3*11 + 4
  val Ea_fix_f64              = Cat(Ea_f64.head(exponentWidth-1),!Ea_f64_is_not_zero | Ea_f64(0))
  val Eb_fix_f64              = Cat(Eb_f64.head(exponentWidth-1),!Eb_f64_is_not_zero | Eb_f64(0))
  val Ec_fix_f64              = Cat(Ec_f64.head(exponentWidth-1),!Ec_f64_is_not_zero | Ec_f64(0))
  val Ea_fix_f32_0            = Cat(Ea_f32_0.head(8-1),!Ea_f32_0_is_not_zero | Ea_f32_0(0))
  val Eb_fix_f32_0            = Cat(Eb_f32_0.head(8-1),!Eb_f32_0_is_not_zero | Eb_f32_0(0))
  val Ec_fix_f32_0            = Cat(Ec_f32_0.head(8-1),!Ec_f32_0_is_not_zero | Ec_f32_0(0))
  val Ea_fix_f32_1            = Cat(Ea_f32_1.head(8-1),!Ea_f32_1_is_not_zero | Ea_f32_1(0))
  val Eb_fix_f32_1            = Cat(Eb_f32_1.head(8-1),!Eb_f32_1_is_not_zero | Eb_f32_1(0))
  val Ec_fix_f32_1            = Cat(Ec_f32_1.head(8-1),!Ec_f32_1_is_not_zero | Ec_f32_1(0))
  val Ea_fix_f16_0            = Cat(Ea_f16_0.head(5-1),!Ea_f16_0_is_not_zero | Ea_f16_0(0))
  val Eb_fix_f16_0            = Cat(Eb_f16_0.head(5-1),!Eb_f16_0_is_not_zero | Eb_f16_0(0))
  val Ec_fix_f16_0            = Cat(Ec_f16_0.head(5-1),!Ec_f16_0_is_not_zero | Ec_f16_0(0))
  val Ea_fix_f16_1            = Cat(Ea_f16_1.head(5-1),!Ea_f16_1_is_not_zero | Ea_f16_1(0))
  val Eb_fix_f16_1            = Cat(Eb_f16_1.head(5-1),!Eb_f16_1_is_not_zero | Eb_f16_1(0))
  val Ec_fix_f16_1            = Cat(Ec_f16_1.head(5-1),!Ec_f16_1_is_not_zero | Ec_f16_1(0))
  val Ea_fix_f16_2            = Cat(Ea_f16_2.head(5-1),!Ea_f16_2_is_not_zero | Ea_f16_2(0))
  val Eb_fix_f16_2            = Cat(Eb_f16_2.head(5-1),!Eb_f16_2_is_not_zero | Eb_f16_2(0))
  val Ec_fix_f16_2            = Cat(Ec_f16_2.head(5-1),!Ec_f16_2_is_not_zero | Ec_f16_2(0))
  val Ea_fix_f16_3            = Cat(Ea_f16_3.head(5-1),!Ea_f16_3_is_not_zero | Ea_f16_3(0))
  val Eb_fix_f16_3            = Cat(Eb_f16_3.head(5-1),!Eb_f16_3_is_not_zero | Eb_f16_3(0))
  val Ec_fix_f16_3            = Cat(Ec_f16_3.head(5-1),!Ec_f16_3_is_not_zero | Ec_f16_3(0))
  val biasF64 = (1 << (exponentWidth-1)) - 1
  val biasF32 = (1 << (8-1)) - 1
  val biasF16 = (1 << (5-1)) - 1
  val Ea_fix_f64_widening =  Ea_fix_f64
  val Eb_fix_f64_widening =  Eb_fix_f64
  val Ea_fix_f32_widening_0 = Ea_fix_f32_0
  val Eb_fix_f32_widening_0 =  Eb_fix_f32_0
  val Ea_fix_f32_widening_1 =  Ea_fix_f32_1
  val Eb_fix_f32_widening_1 =  Eb_fix_f32_1
  val Eab_f64                     = Cat(0.U,Ea_fix_f64_widening +& Eb_fix_f64_widening).asSInt - biasF64.S + rshiftBasicF64.S
  val Eab_f32_0                   = Cat(0.U,Ea_fix_f32_widening_0 +& Eb_fix_f32_widening_0).asSInt - biasF32.S + rshiftBasicF32.S
  val Eab_f32_1                   = Cat(0.U,Ea_fix_f32_widening_1 +& Eb_fix_f32_widening_1).asSInt - biasF32.S + rshiftBasicF32.S
  val Eab_f16_0                   = Cat(0.U,Ea_fix_f16_0 +& Eb_fix_f16_0).asSInt - biasF16.S + rshiftBasicF16.S
  val Eab_f16_1                   = Cat(0.U,Ea_fix_f16_1 +& Eb_fix_f16_1).asSInt - biasF16.S + rshiftBasicF16.S
  val Eab_f16_2                   = Cat(0.U,Ea_fix_f16_2 +& Eb_fix_f16_2).asSInt - biasF16.S + rshiftBasicF16.S
  val Eab_f16_3                   = Cat(0.U,Ea_fix_f16_3 +& Eb_fix_f16_3).asSInt - biasF16.S + rshiftBasicF16.S
  val rshift_value_f64            = Eab_f64 - Cat(0.U,Ec_fix_f64).asSInt
  val rshift_value_f32_0          = Eab_f32_0 - Cat(0.U,Ec_fix_f32_0).asSInt
  val rshift_value_f32_1          = Eab_f32_1 - Cat(0.U,Ec_fix_f32_1).asSInt
  val rshift_value_f16_0          = Eab_f16_0 - Cat(0.U,Ec_fix_f16_0).asSInt
  val rshift_value_f16_1          = Eab_f16_1 - Cat(0.U,Ec_fix_f16_1).asSInt
  val rshift_value_f16_2          = Eab_f16_2 - Cat(0.U,Ec_fix_f16_2).asSInt
  val rshift_value_f16_3          = Eab_f16_3 - Cat(0.U,Ec_fix_f16_3).asSInt
  val rshift_value_cut_f64        = rshift_value_f64(rshiftMaxF64.U.getWidth-1,0)
  val rshift_value_cut_f32_0      = rshift_value_f32_0(rshiftMaxF32.U.getWidth-1,0)
  val rshift_value_cut_f32_1      = rshift_value_f32_1(rshiftMaxF32.U.getWidth-1,0)
  val rshift_value_cut_f16_0      = rshift_value_f16_0(rshiftMaxF16.U.getWidth-1,0)
  val rshift_value_cut_f16_1      = rshift_value_f16_1(rshiftMaxF16.U.getWidth-1,0)
  val rshift_value_cut_f16_2      = rshift_value_f16_2(rshiftMaxF16.U.getWidth-1,0)
  val rshift_value_cut_f16_3      = rshift_value_f16_3(rshiftMaxF16.U.getWidth-1,0)
  val fp_c_significand_cat0_f64   = Cat(fp_c_significand_f64,0.U((rshiftMaxF64-significandWidth).W))
  val fp_c_significand_cat0_f32_0 = Cat(fp_c_significand_f32_0,0.U((rshiftMaxF32-24).W))
  val fp_c_significand_cat0_f32_1 = Cat(fp_c_significand_f32_1,0.U((rshiftMaxF32-24).W))
  val fp_c_significand_cat0_f16_0 = Cat(fp_c_significand_f16_0,0.U((rshiftMaxF16-11).W))
  val fp_c_significand_cat0_f16_1 = Cat(fp_c_significand_f16_1,0.U((rshiftMaxF16-11).W))
  val fp_c_significand_cat0_f16_2 = Cat(fp_c_significand_f16_2,0.U((rshiftMaxF16-11).W))
  val fp_c_significand_cat0_f16_3 = Cat(fp_c_significand_f16_3,0.U((rshiftMaxF16-11).W))
  val rshift_result_with_grs_f64  = shiftRightWithMuxSticky(fp_c_significand_cat0_f64,rshift_value_cut_f64)
  val rshift_result_with_grs_f64_f32_1 = shiftRightWithMuxSticky(
    Mux(is_fp64,fp_c_significand_cat0_f64,fp_c_significand_cat0_f32_1.asTypeOf(fp_c_significand_cat0_f64)),
    Mux(is_fp64,rshift_value_cut_f64,rshift_value_cut_f32_1.asTypeOf(rshift_value_cut_f64))
  )
  val rshift_result_with_grs_f32_0 = shiftRightWithMuxSticky(fp_c_significand_cat0_f32_0,rshift_value_cut_f32_0)
  val rshift_result_with_grs_f32_1 = rshift_result_with_grs_f64_f32_1.asTypeOf(rshift_result_with_grs_f32_0)
  val rshift_result_with_grs_f16_0 = shiftRightWithMuxSticky(fp_c_significand_cat0_f16_0,rshift_value_cut_f16_0)
  val rshift_result_with_grs_f16_1 = shiftRightWithMuxSticky(fp_c_significand_cat0_f16_1,rshift_value_cut_f16_1)
  val rshift_result_with_grs_f16_2 = shiftRightWithMuxSticky(fp_c_significand_cat0_f16_2,rshift_value_cut_f16_2)
  val rshift_result_with_grs_f16_3 = shiftRightWithMuxSticky(fp_c_significand_cat0_f16_3,rshift_value_cut_f16_3)
  val Ec_is_too_big_f64            = rshift_value_f64 <= 0.S
  val Ec_is_too_big_f32_0          = rshift_value_f32_0 <= 0.S
  val Ec_is_too_big_f32_1          = rshift_value_f32_1 <= 0.S
  val Ec_is_too_big_f16_0          = rshift_value_f16_0 <= 0.S
  val Ec_is_too_big_f16_1          = rshift_value_f16_1 <= 0.S
  val Ec_is_too_big_f16_2          = rshift_value_f16_2 <= 0.S
  val Ec_is_too_big_f16_3          = rshift_value_f16_3 <= 0.S
  val Ec_is_too_small_f64          = rshift_value_f64.asSInt > rshiftMaxF64.S
  val Ec_is_too_small_f32_0        = rshift_value_f32_0.asSInt > rshiftMaxF32.S
  val Ec_is_too_small_f32_1        = rshift_value_f32_1.asSInt > rshiftMaxF32.S
  val Ec_is_too_small_f16_0        = rshift_value_f16_0.asSInt > rshiftMaxF16.S
  val Ec_is_too_small_f16_1        = rshift_value_f16_1.asSInt > rshiftMaxF16.S
  val Ec_is_too_small_f16_2        = rshift_value_f16_2.asSInt > rshiftMaxF16.S
  val Ec_is_too_small_f16_3        = rshift_value_f16_3.asSInt > rshiftMaxF16.S
  val Ec_is_medium_f64             = !Ec_is_too_big_f64 & !Ec_is_too_small_f64
  val Ec_is_medium_f32_0           = !Ec_is_too_big_f32_0 & !Ec_is_too_small_f32_0
  val Ec_is_medium_f32_1           = !Ec_is_too_big_f32_1 & !Ec_is_too_small_f32_1
  val Ec_is_medium_f16_0           = !Ec_is_too_big_f16_0 & !Ec_is_too_small_f16_0
  val Ec_is_medium_f16_1           = !Ec_is_too_big_f16_1 & !Ec_is_too_small_f16_1
  val Ec_is_medium_f16_2           = !Ec_is_too_big_f16_2 & !Ec_is_too_small_f16_2
  val Ec_is_medium_f16_3           = !Ec_is_too_big_f16_3 & !Ec_is_too_small_f16_3

  val rshift_guard_f64             = RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(2), 0.U), fire)
  val rshift_guard_f32_0           = RegEnable(Mux(Ec_is_medium_f32_0, rshift_result_with_grs_f32_0(2), 0.U), fire)
  val rshift_guard_f32_1           = RegEnable(Mux(Ec_is_medium_f32_1, rshift_result_with_grs_f32_1(2), 0.U), fire)
  val rshift_guard_f16_0           = RegEnable(Mux(Ec_is_medium_f16_0, rshift_result_with_grs_f16_0(2), 0.U), fire)
  val rshift_guard_f16_1           = RegEnable(Mux(Ec_is_medium_f16_1, rshift_result_with_grs_f16_1(2), 0.U), fire)
  val rshift_guard_f16_2           = RegEnable(Mux(Ec_is_medium_f16_2, rshift_result_with_grs_f16_2(2), 0.U), fire)
  val rshift_guard_f16_3           = RegEnable(Mux(Ec_is_medium_f16_3, rshift_result_with_grs_f16_3(2), 0.U), fire)
  val rshift_round_f64             = RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(1), 0.U), fire)
  val rshift_round_f32_0           = RegEnable(Mux(Ec_is_medium_f32_0, rshift_result_with_grs_f32_0(1), 0.U), fire)
  val rshift_round_f32_1           = RegEnable(Mux(Ec_is_medium_f32_1, rshift_result_with_grs_f32_1(1), 0.U), fire)
  val rshift_round_f16_0           = RegEnable(Mux(Ec_is_medium_f16_0, rshift_result_with_grs_f16_0(1), 0.U), fire)
  val rshift_round_f16_1           = RegEnable(Mux(Ec_is_medium_f16_1, rshift_result_with_grs_f16_1(1), 0.U), fire)
  val rshift_round_f16_2           = RegEnable(Mux(Ec_is_medium_f16_2, rshift_result_with_grs_f16_2(1), 0.U), fire)
  val rshift_round_f16_3           = RegEnable(Mux(Ec_is_medium_f16_3, rshift_result_with_grs_f16_3(1), 0.U), fire)
  val rshift_sticky_f64            = RegEnable(Mux(Ec_is_medium_f64, rshift_result_with_grs_f64(0), Mux(Ec_is_too_big_f64, 0.U, fp_c_significand_f64.orR)), fire)
  val rshift_sticky_f32_0          = RegEnable(Mux(Ec_is_medium_f32_0, rshift_result_with_grs_f32_0(0), Mux(Ec_is_too_big_f32_0, 0.U, fp_c_significand_f32_0.orR)), fire)
  val rshift_sticky_f32_1          = RegEnable(Mux(Ec_is_medium_f32_1, rshift_result_with_grs_f32_1(0), Mux(Ec_is_too_big_f32_1, 0.U, fp_c_significand_f32_1.orR)), fire)
  val rshift_sticky_f16_0          = RegEnable(Mux(Ec_is_medium_f16_0, rshift_result_with_grs_f16_0(0), Mux(Ec_is_too_big_f16_0, 0.U, fp_c_significand_f16_0.orR)), fire)
  val rshift_sticky_f16_1          = RegEnable(Mux(Ec_is_medium_f16_1, rshift_result_with_grs_f16_1(0), Mux(Ec_is_too_big_f16_1, 0.U, fp_c_significand_f16_1.orR)), fire)
  val rshift_sticky_f16_2          = RegEnable(Mux(Ec_is_medium_f16_2, rshift_result_with_grs_f16_2(0), Mux(Ec_is_too_big_f16_2, 0.U, fp_c_significand_f16_2.orR)), fire)
  val rshift_sticky_f16_3          = RegEnable(Mux(Ec_is_medium_f16_3, rshift_result_with_grs_f16_3(0), Mux(Ec_is_too_big_f16_3, 0.U, fp_c_significand_f16_3.orR)), fire)

  val rshift_result_temp_f64       = rshift_result_with_grs_f64.head(rshiftMaxF64-2)
  val rshift_result_temp_f32_0     = rshift_result_with_grs_f32_0.head(rshiftMaxF32-2)
  val rshift_result_temp_f32_1     = rshift_result_with_grs_f32_1.head(rshiftMaxF32-2)
  val rshift_result_temp_f16_0     = rshift_result_with_grs_f16_0.head(rshiftMaxF16-2)
  val rshift_result_temp_f16_1     = rshift_result_with_grs_f16_1.head(rshiftMaxF16-2)
  val rshift_result_temp_f16_2     = rshift_result_with_grs_f16_2.head(rshiftMaxF16-2)
  val rshift_result_temp_f16_3     = rshift_result_with_grs_f16_3.head(rshiftMaxF16-2)
  val rshift_result_f64            = Mux(Ec_is_medium_f64,
    rshift_result_temp_f64,
    Mux(Ec_is_too_big_f64, fp_c_significand_cat0_f64.head(rshiftMaxF64-2), 0.U((rshiftMaxF64-2).W))
  )
  val rshift_result_f32_0         = Mux(Ec_is_medium_f32_0,
    rshift_result_temp_f32_0,
    Mux(Ec_is_too_big_f32_0, fp_c_significand_cat0_f32_0.head(rshiftMaxF32-2), 0.U((rshiftMaxF32-2).W))
  )
  val rshift_result_f32_1         = Mux(Ec_is_medium_f32_1,
    rshift_result_temp_f32_1,
    Mux(Ec_is_too_big_f32_1, fp_c_significand_cat0_f32_1.head(rshiftMaxF32-2), 0.U((rshiftMaxF32-2).W))
  )
  val rshift_result_f16_0         = Mux(Ec_is_medium_f16_0,
    rshift_result_temp_f16_0,
    Mux(Ec_is_too_big_f16_0, fp_c_significand_cat0_f16_0.head(rshiftMaxF16-2), 0.U((rshiftMaxF16-2).W))
  )
  val rshift_result_f16_1         = Mux(Ec_is_medium_f16_1,
    rshift_result_temp_f16_1,
    Mux(Ec_is_too_big_f16_1, fp_c_significand_cat0_f16_1.head(rshiftMaxF16-2), 0.U((rshiftMaxF16-2).W))
  )
  val rshift_result_f16_2         = Mux(Ec_is_medium_f16_2,
    rshift_result_temp_f16_2,
    Mux(Ec_is_too_big_f16_2, fp_c_significand_cat0_f16_2.head(rshiftMaxF16-2), 0.U((rshiftMaxF16-2).W))
  )
  val rshift_result_f16_3         = Mux(Ec_is_medium_f16_3,
    rshift_result_temp_f16_3,
    Mux(Ec_is_too_big_f16_3, fp_c_significand_cat0_f16_3.head(rshiftMaxF16-2), 0.U((rshiftMaxF16-2).W))
  )

  val fp_c_rshiftValue_inv_f64_reg0   = RegEnable(Mux(is_sub_f64.asBool   ,Cat(1.U,~rshift_result_f64),Cat(0.U,rshift_result_f64))    , fire)
  val fp_c_rshiftValue_inv_f32_0_reg0 = RegEnable(Mux(is_sub_f32_0.asBool ,Cat(1.U,~rshift_result_f32_0),Cat(0.U,rshift_result_f32_0)), fire)
  val fp_c_rshiftValue_inv_f32_1_reg0 = RegEnable(Mux(is_sub_f32_1.asBool ,Cat(1.U,~rshift_result_f32_1),Cat(0.U,rshift_result_f32_1)), fire)
  val fp_c_rshiftValue_inv_f16_0_reg0 = RegEnable(Mux(is_sub_f16_0.asBool ,Cat(1.U,~rshift_result_f16_0),Cat(0.U,rshift_result_f16_0)), fire)
  val fp_c_rshiftValue_inv_f16_1_reg0 = RegEnable(Mux(is_sub_f16_1.asBool ,Cat(1.U,~rshift_result_f16_1),Cat(0.U,rshift_result_f16_1)), fire)
  val fp_c_rshiftValue_inv_f16_2_reg0 = RegEnable(Mux(is_sub_f16_2.asBool ,Cat(1.U,~rshift_result_f16_2),Cat(0.U,rshift_result_f16_2)), fire)
  val fp_c_rshiftValue_inv_f16_3_reg0 = RegEnable(Mux(is_sub_f16_3.asBool ,Cat(1.U,~rshift_result_f16_3),Cat(0.U,rshift_result_f16_3)), fire)

  val booth_in_a = Mux(
    is_fp64,
    fp_a_significand_f64,
    Mux(
      is_fp32,
      Cat(fp_a_significand_f32_1,0.U(5.W),fp_a_significand_f32_0),
      Cat(Cat(fp_a_significand_f16_3,0.U(2.W),fp_a_significand_f16_2),0.U(5.W),Cat(fp_a_significand_f16_1,0.U(2.W),fp_a_significand_f16_0))
    )
  )
  val booth_in_b = Mux(
    is_fp64,
    fp_b_significand_f64,
    Mux(
      is_fp32,
      Cat(fp_b_significand_f32_1,0.U(5.W),fp_b_significand_f32_0),
      Cat(Cat(fp_b_significand_f16_3,0.U(2.W),fp_b_significand_f16_2),0.U(5.W),Cat(fp_b_significand_f16_1,0.U(2.W),fp_b_significand_f16_0))
    )
  )
  val U_BoothEncoder = Module(new BoothEncoderF64F32F16(width = significandWidth, is_addend_expand_1bit = true))
  U_BoothEncoder.io.in_a := booth_in_a
  U_BoothEncoder.io.in_b := booth_in_b
  U_BoothEncoder.io.is_fp64 := is_fp64
  U_BoothEncoder.io.is_fp32 := is_fp32

  val U_CSAnto2 = Module(new CSA_Nto2With3to2MainPipeline(U_BoothEncoder.io.out_pp.length,U_BoothEncoder.io.out_pp.head.getWidth,pipeLevel = 5))
  U_CSAnto2.io.fire := fire
  U_CSAnto2.io.in := U_BoothEncoder.io.out_pp

  val CSA3to2_in_a = U_CSAnto2.io.out_sum
  val CSA3to2_in_b = Mux(
    is_fp64_reg0,
    Cat(U_CSAnto2.io.out_car.head(106), is_sub_f64_reg0 & !rshift_guard_f64 & !rshift_round_f64 & !rshift_sticky_f64),
    Mux(
      is_fp32_reg0,
      Cat(U_CSAnto2.io.out_car.head(48), RegEnable(is_sub_f32_1, fire) & !rshift_guard_f32_1 & !rshift_round_f32_1 & !rshift_sticky_f32_1,
        U_CSAnto2.io.out_car(57,49),
        U_CSAnto2.io.out_car(48,1), RegEnable(is_sub_f32_0, fire) & !rshift_guard_f32_0 & !rshift_round_f32_0 & !rshift_sticky_f32_0
      ),
      Cat(Cat(U_CSAnto2.io.out_car.head(22), RegEnable(is_sub_f16_3, fire) & !rshift_guard_f16_3 & !rshift_round_f16_3 & !rshift_sticky_f16_3,
        U_CSAnto2.io.out_car(25+58,23+58),
        U_CSAnto2.io.out_car(22+58,1+58), RegEnable(is_sub_f16_2, fire) & !rshift_guard_f16_2 & !rshift_round_f16_2 & !rshift_sticky_f16_2),
        U_CSAnto2.io.out_car(57,49),
        Cat(U_CSAnto2.io.out_car(48,27), RegEnable(is_sub_f16_1, fire) & !rshift_guard_f16_1 & !rshift_round_f16_1 & !rshift_sticky_f16_1,
          U_CSAnto2.io.out_car(25,23),
          U_CSAnto2.io.out_car(22,1), RegEnable(is_sub_f16_0, fire) & !rshift_guard_f16_0 & !rshift_round_f16_0 & !rshift_sticky_f16_0)
      )
    )
  )
  val CSA3to2_in_c = Mux(
    is_fp64_reg0,
    Cat(0.U,fp_c_rshiftValue_inv_f64_reg0(2*significandWidth-1,0)),
    Mux(
      is_fp32_reg0,
      Cat(0.U,fp_c_rshiftValue_inv_f32_1_reg0(2*24-1,0),0.U(10.W),fp_c_rshiftValue_inv_f32_0_reg0(2*24-1,0)),
      Cat(
        Cat(0.U,fp_c_rshiftValue_inv_f16_3_reg0(2*11-1,0),0.U(4.W),fp_c_rshiftValue_inv_f16_2_reg0(2*11-1,0)),
        0.U(10.W),
        Cat(fp_c_rshiftValue_inv_f16_1_reg0(2*11-1,0),0.U(4.W),fp_c_rshiftValue_inv_f16_0_reg0(2*11-1,0))
      )
    )
  )
  val U_CSA3to2 = Module(new CSA3to2(width = U_CSAnto2.io.out_sum.getWidth))
  U_CSA3to2.io.in_a := CSA3to2_in_a
  U_CSA3to2.io.in_b := CSA3to2_in_b
  U_CSA3to2.io.in_c := CSA3to2_in_c

  val adder_lowbit_f64 = U_CSA3to2.io.out_sum + U_CSA3to2.io.out_car
  val adder_lowbit_f32_0 = adder_lowbit_f64(48,0)
  val adder_lowbit_f32_1 = adder_lowbit_f64(106,58)
  val adder_lowbit_f16_0 = adder_lowbit_f32_0(22,0)
  val adder_lowbit_f16_1 = adder_lowbit_f32_0(48,26)
  val adder_lowbit_f16_2 = adder_lowbit_f32_1(22,0)
  val adder_lowbit_f16_3 = adder_lowbit_f32_1(48,26)
  val fp_c_rshift_result_high_inv_add0_f64 = fp_c_rshiftValue_inv_f64_reg0.head(significandWidth+3)
  val fp_c_rshift_result_high_inv_add0_f32_0 = fp_c_rshiftValue_inv_f32_0_reg0.head(24+3)
  val fp_c_rshift_result_high_inv_add0_f32_1 = fp_c_rshiftValue_inv_f32_1_reg0.head(24+3)
  val fp_c_rshift_result_high_inv_add0_f16_0 = fp_c_rshiftValue_inv_f16_0_reg0.head(11+3)
  val fp_c_rshift_result_high_inv_add0_f16_1 = fp_c_rshiftValue_inv_f16_1_reg0.head(11+3)
  val fp_c_rshift_result_high_inv_add0_f16_2 = fp_c_rshiftValue_inv_f16_2_reg0.head(11+3)
  val fp_c_rshift_result_high_inv_add0_f16_3 = fp_c_rshiftValue_inv_f16_3_reg0.head(11+3)

  val fp_c_rshift_result_high_inv_add1 = Mux(is_fp64_reg0,
    Cat(0.U(3.W),fp_c_rshiftValue_inv_f64_reg0.head(significandWidth+3)),
    Mux(
      is_fp32_reg0,
      Cat(fp_c_rshiftValue_inv_f32_1_reg0.head(24+3),0.U(5.W),fp_c_rshiftValue_inv_f32_0_reg0.head(24+3)),
      Cat(
        fp_c_rshiftValue_inv_f16_3_reg0.head(11+3),0.U,fp_c_rshiftValue_inv_f16_2_reg0.head(11+3),
        0.U,fp_c_rshiftValue_inv_f16_1_reg0.head(11+3),0.U,fp_c_rshiftValue_inv_f16_0_reg0.head(11+3)
      )
    )
  ) + Cat(!is_fp32_reg0 & !is_fp64_reg0, 0.U(12.W), is_fp32_reg0, 0.U, !is_fp32_reg0 & !is_fp64_reg0, 0.U(14.W), !is_fp32_reg0 & !is_fp64_reg0, 0.U(14.W), 1.U)
  val fp_c_rshift_result_high_inv_add1_f64 = fp_c_rshift_result_high_inv_add1(55,0)
  val fp_c_rshift_result_high_inv_add1_f32_0 = fp_c_rshift_result_high_inv_add1(26,0)
  val fp_c_rshift_result_high_inv_add1_f32_1 = fp_c_rshift_result_high_inv_add1(58,32)
  val fp_c_rshift_result_high_inv_add1_f16_0 = fp_c_rshift_result_high_inv_add1(13,0)
  val fp_c_rshift_result_high_inv_add1_f16_1 = fp_c_rshift_result_high_inv_add1(28,15)
  val fp_c_rshift_result_high_inv_add1_f16_2 = fp_c_rshift_result_high_inv_add1(43,30)
  val fp_c_rshift_result_high_inv_add1_f16_3 = fp_c_rshift_result_high_inv_add1(58,45)

  val fra_mul = booth_in_a * booth_in_b


  val adder_f64         = Cat(Mux(adder_lowbit_f64.head(1).asBool, fp_c_rshift_result_high_inv_add1_f64, fp_c_rshift_result_high_inv_add0_f64),adder_lowbit_f64.tail(1),
    Mux(is_sub_f64_reg0, ((~Cat(rshift_guard_f64,rshift_round_f64,rshift_sticky_f64)).asUInt+1.U).head(2), Cat(rshift_guard_f64,rshift_round_f64))
  )
  val adder_f32_0       = Cat(Mux(adder_lowbit_f32_0.head(1).asBool, fp_c_rshift_result_high_inv_add1_f32_0, fp_c_rshift_result_high_inv_add0_f32_0),adder_lowbit_f32_0.tail(1),
    Mux(RegEnable(is_sub_f32_0, fire), ((~Cat(rshift_guard_f32_0,rshift_round_f32_0,rshift_sticky_f32_0)).asUInt+1.U).head(2), Cat(rshift_guard_f32_0,rshift_round_f32_0))
  )
  val adder_f32_1      = Cat(Mux(adder_lowbit_f32_1.head(1).asBool, fp_c_rshift_result_high_inv_add1_f32_1, fp_c_rshift_result_high_inv_add0_f32_1),adder_lowbit_f32_1.tail(1),
    Mux(RegEnable(is_sub_f32_1, fire), ((~Cat(rshift_guard_f32_1,rshift_round_f32_1,rshift_sticky_f32_1)).asUInt+1.U).head(2), Cat(rshift_guard_f32_1,rshift_round_f32_1))
  )
  val adder_f16_0       = Cat(Mux(adder_lowbit_f16_0.head(1).asBool, fp_c_rshift_result_high_inv_add1_f16_0, fp_c_rshift_result_high_inv_add0_f16_0),adder_lowbit_f16_0.tail(1),
    Mux(RegEnable(is_sub_f16_0, fire), ((~Cat(rshift_guard_f16_0,rshift_round_f16_0,rshift_sticky_f16_0)).asUInt+1.U).head(2), Cat(rshift_guard_f16_0,rshift_round_f16_0))
  )
  val adder_f16_1      = Cat(Mux(adder_lowbit_f16_1.head(1).asBool, fp_c_rshift_result_high_inv_add1_f16_1, fp_c_rshift_result_high_inv_add0_f16_1),adder_lowbit_f16_1.tail(1),
    Mux(RegEnable(is_sub_f16_1, fire), ((~Cat(rshift_guard_f16_1,rshift_round_f16_1,rshift_sticky_f16_1)).asUInt+1.U).head(2), Cat(rshift_guard_f16_1,rshift_round_f16_1))
  )
  val adder_f16_2      = Cat(Mux(adder_lowbit_f16_2.head(1).asBool, fp_c_rshift_result_high_inv_add1_f16_2, fp_c_rshift_result_high_inv_add0_f16_2),adder_lowbit_f16_2.tail(1),
    Mux(RegEnable(is_sub_f16_2, fire), ((~Cat(rshift_guard_f16_2,rshift_round_f16_2,rshift_sticky_f16_2)).asUInt+1.U).head(2), Cat(rshift_guard_f16_2,rshift_round_f16_2))
  )
  val adder_f16_3      = Cat(Mux(adder_lowbit_f16_3.head(1).asBool, fp_c_rshift_result_high_inv_add1_f16_3, fp_c_rshift_result_high_inv_add0_f16_3),adder_lowbit_f16_3.tail(1),
    Mux(RegEnable(is_sub_f16_3, fire), ((~Cat(rshift_guard_f16_3,rshift_round_f16_3,rshift_sticky_f16_3)).asUInt+1.U).head(2), Cat(rshift_guard_f16_3,rshift_round_f16_3))
  )

  val adder_is_negative_f64   = adder_f64.head(1).asBool
  val adder_is_negative_f32_0 = adder_f32_0.head(1).asBool
  val adder_is_negative_f32_1 = adder_f32_1.head(1).asBool
  val adder_is_negative_f16_0 = adder_f16_0.head(1).asBool
  val adder_is_negative_f16_1 = adder_f16_1.head(1).asBool
  val adder_is_negative_f16_2 = adder_f16_2.head(1).asBool
  val adder_is_negative_f16_3 = adder_f16_3.head(1).asBool
  val adder_is_negative_f64_reg2   = RegEnable(RegEnable(adder_is_negative_f64  , fire_reg0), fire_reg1)
  val adder_is_negative_f32_0_reg2 = RegEnable(RegEnable(adder_is_negative_f32_0, fire_reg0), fire_reg1)
  val adder_is_negative_f32_1_reg2 = RegEnable(RegEnable(adder_is_negative_f32_1, fire_reg0), fire_reg1)
  val adder_is_negative_f16_0_reg2 = RegEnable(RegEnable(adder_is_negative_f16_0, fire_reg0), fire_reg1)
  val adder_is_negative_f16_1_reg2 = RegEnable(RegEnable(adder_is_negative_f16_1, fire_reg0), fire_reg1)
  val adder_is_negative_f16_2_reg2 = RegEnable(RegEnable(adder_is_negative_f16_2, fire_reg0), fire_reg1)
  val adder_is_negative_f16_3_reg2 = RegEnable(RegEnable(adder_is_negative_f16_3, fire_reg0), fire_reg1)

  val adder_inv_f64         = Mux(adder_is_negative_f64  , (~adder_f64.tail(1)).asUInt, adder_f64.tail(1))
  val adder_inv_f32_0       = Mux(adder_is_negative_f32_0, (~adder_f32_0.tail(1)).asUInt, adder_f32_0.tail(1))
  val adder_inv_f32_1       = Mux(adder_is_negative_f32_1, (~adder_f32_1.tail(1)).asUInt, adder_f32_1.tail(1))
  val adder_inv_f16_0       = Mux(adder_is_negative_f16_0, (~adder_f16_0.tail(1)).asUInt, adder_f16_0.tail(1))
  val adder_inv_f16_1       = Mux(adder_is_negative_f16_1, (~adder_f16_1.tail(1)).asUInt, adder_f16_1.tail(1))
  val adder_inv_f16_2       = Mux(adder_is_negative_f16_2, (~adder_f16_2.tail(1)).asUInt, adder_f16_2.tail(1))
  val adder_inv_f16_3       = Mux(adder_is_negative_f16_3, (~adder_f16_3.tail(1)).asUInt, adder_f16_3.tail(1))
  val Eab_is_greater_f64    = rshift_value_f64 > 0.S
  val Eab_is_greater_f32_0    = rshift_value_f32_0 > 0.S
  val Eab_is_greater_f32_1    = rshift_value_f32_1 > 0.S
  val Eab_is_greater_f16_0    = rshift_value_f16_0 > 0.S
  val Eab_is_greater_f16_1    = rshift_value_f16_1 > 0.S
  val Eab_is_greater_f16_2    = rshift_value_f16_2 > 0.S
  val Eab_is_greater_f16_3    = rshift_value_f16_3 > 0.S
  val Ec_is_greater_f64     = !Eab_is_greater_f64
  val Ec_is_greater_f32_0     = !Eab_is_greater_f32_0
  val Ec_is_greater_f32_1     = !Eab_is_greater_f32_1
  val Ec_is_greater_f16_0     = !Eab_is_greater_f16_0
  val Ec_is_greater_f16_1     = !Eab_is_greater_f16_1
  val Ec_is_greater_f16_2     = !Eab_is_greater_f16_2
  val Ec_is_greater_f16_3     = !Eab_is_greater_f16_3

  val E_greater_f64_reg2   = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f64, Eab_f64(exponentWidth,0).asUInt, Cat(0.U(1.W),Ec_fix_f64)), fire), fire_reg0), fire_reg1)
  val E_greater_f32_0_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f32_0, Eab_f32_0(8,0).asUInt, Cat(0.U(1.W),Ec_fix_f32_0)), fire), fire_reg0), fire_reg1)
  val E_greater_f32_1_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f32_1, Eab_f32_1(8,0).asUInt, Cat(0.U(1.W),Ec_fix_f32_1)), fire), fire_reg0), fire_reg1)
  val E_greater_f16_0_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f16_0, Eab_f16_0(5,0).asUInt, Cat(0.U(1.W),Ec_fix_f16_0)), fire), fire_reg0), fire_reg1)
  val E_greater_f16_1_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f16_1, Eab_f16_1(5,0).asUInt, Cat(0.U(1.W),Ec_fix_f16_1)), fire), fire_reg0), fire_reg1)
  val E_greater_f16_2_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f16_2, Eab_f16_2(5,0).asUInt, Cat(0.U(1.W),Ec_fix_f16_2)), fire), fire_reg0), fire_reg1)
  val E_greater_f16_3_reg2 = RegEnable(RegEnable(RegEnable(Mux(Eab_is_greater_f16_3, Eab_f16_3(5,0).asUInt, Cat(0.U(1.W),Ec_fix_f16_3)), fire), fire_reg0), fire_reg1)

  val lshift_value_max_f64_reg0   = RegEnable(Mux(Eab_is_greater_f64, Eab_f64(exponentWidth,0).asUInt - 1.U, Cat(0.U,Ec_fix_f64 - 1.U)), fire)
  val lshift_value_max_f32_0_reg0 = RegEnable(Mux(Eab_is_greater_f32_0, Eab_f32_0(8,0).asUInt - 1.U, Cat(0.U,Ec_fix_f32_0 - 1.U)), fire)
  val lshift_value_max_f32_1_reg0 = RegEnable(Mux(Eab_is_greater_f32_1, Eab_f32_1(8,0).asUInt - 1.U, Cat(0.U,Ec_fix_f32_1 - 1.U)), fire)
  val lshift_value_max_f16_0_reg0 = RegEnable(Mux(Eab_is_greater_f16_0, Eab_f16_0(5,0).asUInt - 1.U, Cat(0.U,Ec_fix_f16_0 - 1.U)), fire)
  val lshift_value_max_f16_1_reg0 = RegEnable(Mux(Eab_is_greater_f16_1, Eab_f16_1(5,0).asUInt - 1.U, Cat(0.U,Ec_fix_f16_1 - 1.U)), fire)
  val lshift_value_max_f16_2_reg0 = RegEnable(Mux(Eab_is_greater_f16_2, Eab_f16_2(5,0).asUInt - 1.U, Cat(0.U,Ec_fix_f16_2 - 1.U)), fire)
  val lshift_value_max_f16_3_reg0 = RegEnable(Mux(Eab_is_greater_f16_3, Eab_f16_3(5,0).asUInt - 1.U, Cat(0.U,Ec_fix_f16_3 - 1.U)), fire)

  val LZDWidth_f64 = adder_inv_f64.getWidth.U.getWidth
  val LZDWidth_f32_0 = adder_inv_f32_0.getWidth.U.getWidth
  val LZDWidth_f32_1 = adder_inv_f32_1.getWidth.U.getWidth
  val LZDWidth_f16_0 = adder_inv_f16_0.getWidth.U.getWidth
  val LZDWidth_f16_1 = adder_inv_f16_1.getWidth.U.getWidth
  val LZDWidth_f16_2 = adder_inv_f16_2.getWidth.U.getWidth
  val LZDWidth_f16_3 = adder_inv_f16_3.getWidth.U.getWidth
  val lshift_value_mask_f64 = Mux(lshift_value_max_f64_reg0.head(lshift_value_max_f64_reg0.getWidth-LZDWidth_f64).orR,
    0.U(adder_inv_f64.getWidth.W),
    Fill(adder_inv_f64.getWidth, 1.U) >> lshift_value_max_f64_reg0.tail(lshift_value_max_f64_reg0.getWidth-LZDWidth_f64)
  ).asUInt
  val lshift_value_mask_f32_0 = Mux(lshift_value_max_f32_0_reg0.head(lshift_value_max_f32_0_reg0.getWidth-LZDWidth_f32_0).orR,
    0.U(adder_inv_f32_0.getWidth.W),
    Fill(adder_inv_f32_0.getWidth, 1.U) >> lshift_value_max_f32_0_reg0.tail(lshift_value_max_f32_0_reg0.getWidth-LZDWidth_f32_0)
  ).asUInt
  val lshift_value_mask_f32_1 = Mux(lshift_value_max_f32_1_reg0.head(lshift_value_max_f32_1_reg0.getWidth-LZDWidth_f32_1).orR,
    0.U(adder_inv_f32_1.getWidth.W),
    Fill(adder_inv_f32_1.getWidth, 1.U) >> lshift_value_max_f32_1_reg0.tail(lshift_value_max_f32_1_reg0.getWidth-LZDWidth_f32_1)
  ).asUInt
  val lshift_value_mask_f16_0 = Mux(lshift_value_max_f16_0_reg0.head(lshift_value_max_f16_0_reg0.getWidth-LZDWidth_f16_0).orR,
    0.U(adder_inv_f16_0.getWidth.W),
    Fill(adder_inv_f16_0.getWidth, 1.U) >> lshift_value_max_f16_0_reg0.tail(lshift_value_max_f16_0_reg0.getWidth-LZDWidth_f16_0)
  ).asUInt
  val lshift_value_mask_f16_1 = Mux(lshift_value_max_f16_1_reg0.head(lshift_value_max_f16_1_reg0.getWidth-LZDWidth_f16_1).orR,
    0.U(adder_inv_f16_1.getWidth.W),
    Fill(adder_inv_f16_1.getWidth, 1.U) >> lshift_value_max_f16_1_reg0.tail(lshift_value_max_f16_1_reg0.getWidth-LZDWidth_f16_1)
  ).asUInt
  val lshift_value_mask_f16_2 = Mux(lshift_value_max_f16_2_reg0.head(lshift_value_max_f16_2_reg0.getWidth-LZDWidth_f16_2).orR,
    0.U(adder_inv_f16_2.getWidth.W),
    Fill(adder_inv_f16_2.getWidth, 1.U) >> lshift_value_max_f16_2_reg0.tail(lshift_value_max_f16_2_reg0.getWidth-LZDWidth_f16_2)
  ).asUInt
  val lshift_value_mask_f16_3 = Mux(lshift_value_max_f16_3_reg0.head(lshift_value_max_f16_3_reg0.getWidth-LZDWidth_f16_3).orR,
    0.U(adder_inv_f16_3.getWidth.W),
    Fill(adder_inv_f16_3.getWidth, 1.U) >> lshift_value_max_f16_3_reg0.tail(lshift_value_max_f16_3_reg0.getWidth-LZDWidth_f16_3)
  ).asUInt

  val tzd_adder_f64_reg1       = LZD(RegEnable(Reverse(Mux(is_fp64_reg0,adder_f64.asUInt,Cat(1.U(87.W),adder_f32_1.asUInt))), fire_reg0).asTypeOf(adder_f64))
  val tzd_adder_f32_0_reg1     = LZD(RegEnable(Reverse(adder_f32_0.asUInt), fire_reg0).asTypeOf(adder_f32_0))
  val tzd_adder_f32_1_reg1     = tzd_adder_f64_reg1.asTypeOf(tzd_adder_f32_0_reg1)
  val tzd_adder_f16_0_reg1     = LZD(RegEnable(Reverse(adder_f16_0.asUInt), fire_reg0).asTypeOf(adder_f16_0))
  val tzd_adder_f16_1_reg1     = LZD(RegEnable(Reverse(adder_f16_1.asUInt), fire_reg0).asTypeOf(adder_f16_1))
  val tzd_adder_f16_2_reg1     = LZD(RegEnable(Reverse(adder_f16_2.asUInt), fire_reg0).asTypeOf(adder_f16_2))
  val tzd_adder_f16_3_reg1     = LZD(RegEnable(Reverse(adder_f16_3.asUInt), fire_reg0).asTypeOf(adder_f16_3))
  val lzd_adder_inv_mask_f64    = LZD(RegEnable(Mux(is_fp64_reg0,adder_inv_f64 | lshift_value_mask_f64,Cat(adder_inv_f32_1 | lshift_value_mask_f32_1,1.U << 86)), fire_reg0).asTypeOf(adder_inv_f64))
  val lzd_adder_inv_mask_f32_0  = LZD(RegEnable(adder_inv_f32_0 | lshift_value_mask_f32_0, fire_reg0).asTypeOf(adder_inv_f32_0))
  val lzd_adder_inv_mask_f32_1  = lzd_adder_inv_mask_f64.asTypeOf(lzd_adder_inv_mask_f32_0)
  val lzd_adder_inv_mask_f16_0  = LZD(RegEnable(adder_inv_f16_0 | lshift_value_mask_f16_0, fire_reg0).asTypeOf(adder_inv_f16_0))
  val lzd_adder_inv_mask_f16_1  = LZD(RegEnable(adder_inv_f16_1 | lshift_value_mask_f16_1, fire_reg0).asTypeOf(adder_inv_f16_1))
  val lzd_adder_inv_mask_f16_2  = LZD(RegEnable(adder_inv_f16_2 | lshift_value_mask_f16_2, fire_reg0).asTypeOf(adder_inv_f16_2))
  val lzd_adder_inv_mask_f16_3  = LZD(RegEnable(adder_inv_f16_3 | lshift_value_mask_f16_3, fire_reg0).asTypeOf(adder_inv_f16_3))
  val lzd_adder_inv_mask_f64_reg1    = Wire(UInt(lzd_adder_inv_mask_f64.getWidth.W))
  val lzd_adder_inv_mask_f32_0_reg1  = Wire(UInt(lzd_adder_inv_mask_f32_0.getWidth.W))
  val lzd_adder_inv_mask_f32_1_reg1  = Wire(UInt(lzd_adder_inv_mask_f32_1.getWidth.W))
  val lzd_adder_inv_mask_f16_0_reg1  = Wire(UInt(lzd_adder_inv_mask_f16_0.getWidth.W))
  val lzd_adder_inv_mask_f16_1_reg1  = Wire(UInt(lzd_adder_inv_mask_f16_1.getWidth.W))
  val lzd_adder_inv_mask_f16_2_reg1  = Wire(UInt(lzd_adder_inv_mask_f16_2.getWidth.W))
  val lzd_adder_inv_mask_f16_3_reg1  = Wire(UInt(lzd_adder_inv_mask_f16_3.getWidth.W))
  lzd_adder_inv_mask_f64_reg1   := lzd_adder_inv_mask_f64
  lzd_adder_inv_mask_f32_0_reg1 := lzd_adder_inv_mask_f32_0
  lzd_adder_inv_mask_f32_1_reg1 := lzd_adder_inv_mask_f32_1
  lzd_adder_inv_mask_f16_0_reg1 := lzd_adder_inv_mask_f16_0
  lzd_adder_inv_mask_f16_1_reg1 := lzd_adder_inv_mask_f16_1
  lzd_adder_inv_mask_f16_2_reg1 := lzd_adder_inv_mask_f16_2
  lzd_adder_inv_mask_f16_3_reg1 := lzd_adder_inv_mask_f16_3

  val lshift_mask_valid_f64_reg1     = (RegEnable(adder_inv_f64  , fire_reg0) | RegEnable(lshift_value_mask_f64  , fire_reg0)) === RegEnable(lshift_value_mask_f64  , fire_reg0)
  val lshift_mask_valid_f32_0_reg1   = (RegEnable(adder_inv_f32_0, fire_reg0) | RegEnable(lshift_value_mask_f32_0, fire_reg0)) === RegEnable(lshift_value_mask_f32_0, fire_reg0)
  val lshift_mask_valid_f32_1_reg1   = (RegEnable(adder_inv_f32_1, fire_reg0) | RegEnable(lshift_value_mask_f32_1, fire_reg0)) === RegEnable(lshift_value_mask_f32_1, fire_reg0)
  val lshift_mask_valid_f16_0_reg1   = (RegEnable(adder_inv_f16_0, fire_reg0) | RegEnable(lshift_value_mask_f16_0, fire_reg0)) === RegEnable(lshift_value_mask_f16_0, fire_reg0)
  val lshift_mask_valid_f16_1_reg1   = (RegEnable(adder_inv_f16_1, fire_reg0) | RegEnable(lshift_value_mask_f16_1, fire_reg0)) === RegEnable(lshift_value_mask_f16_1, fire_reg0)
  val lshift_mask_valid_f16_2_reg1   = (RegEnable(adder_inv_f16_2, fire_reg0) | RegEnable(lshift_value_mask_f16_2, fire_reg0)) === RegEnable(lshift_value_mask_f16_2, fire_reg0)
  val lshift_mask_valid_f16_3_reg1   = (RegEnable(adder_inv_f16_3, fire_reg0) | RegEnable(lshift_value_mask_f16_3, fire_reg0)) === RegEnable(lshift_value_mask_f16_3, fire_reg0)
  val lshift_value_f64_reg1          = lzd_adder_inv_mask_f64_reg1
  val lshift_value_f32_0_reg1        = lzd_adder_inv_mask_f32_0_reg1
  val lshift_value_f32_1_reg1        = lzd_adder_inv_mask_f32_1_reg1
  val lshift_value_f16_0_reg1        = lzd_adder_inv_mask_f16_0_reg1
  val lshift_value_f16_1_reg1        = lzd_adder_inv_mask_f16_1_reg1
  val lshift_value_f16_2_reg1        = lzd_adder_inv_mask_f16_2_reg1
  val lshift_value_f16_3_reg1        = lzd_adder_inv_mask_f16_3_reg1

  val adder_f64_reg1   = RegEnable(adder_f64  , fire_reg0)
  val adder_f32_0_reg1 = RegEnable(adder_f32_0, fire_reg0)
  val adder_f32_1_reg1 = RegEnable(adder_f32_1, fire_reg0)
  val adder_f16_0_reg1 = RegEnable(adder_f16_0, fire_reg0)
  val adder_f16_1_reg1 = RegEnable(adder_f16_1, fire_reg0)
  val adder_f16_2_reg1 = RegEnable(adder_f16_2, fire_reg0)
  val adder_f16_3_reg1 = RegEnable(adder_f16_3, fire_reg0)

  val lshift_adder_f64        = shiftLeftWithMux(
    Mux(is_fp64_reg1,adder_f64_reg1,adder_f32_1_reg1.asTypeOf(adder_f64_reg1)),
    Mux(is_fp64_reg1,lshift_value_f64_reg1,lshift_value_f32_1_reg1.asTypeOf(lshift_value_f64_reg1))
  )
  val lshift_adder_f32_0        = shiftLeftWithMux(adder_f32_0_reg1, lshift_value_f32_0_reg1)
  val lshift_adder_f32_1        = lshift_adder_f64.asTypeOf(lshift_adder_f32_0)
  val lshift_adder_f16_0        = shiftLeftWithMux(adder_f16_0_reg1, lshift_value_f16_0_reg1)
  val lshift_adder_f16_1        = shiftLeftWithMux(adder_f16_1_reg1, lshift_value_f16_1_reg1)
  val lshift_adder_f16_2        = shiftLeftWithMux(adder_f16_2_reg1, lshift_value_f16_2_reg1)
  val lshift_adder_f16_3        = shiftLeftWithMux(adder_f16_3_reg1, lshift_value_f16_3_reg1)


  val lshift_adder_inv_f64      = Cat(Mux(RegEnable(adder_is_negative_f64  , fire_reg0),~lshift_adder_f64.head(significandWidth+4),lshift_adder_f64.head(significandWidth+4)),lshift_adder_f64.tail(significandWidth+4))
  val lshift_adder_inv_f32_0    = Cat(Mux(RegEnable(adder_is_negative_f32_0, fire_reg0),~lshift_adder_f32_0.head(24+4),lshift_adder_f32_0.head(24+4)),lshift_adder_f32_0.tail(24+4))
  val lshift_adder_inv_f32_1    = Cat(Mux(RegEnable(adder_is_negative_f32_1, fire_reg0),~lshift_adder_f32_1.head(24+4),lshift_adder_f32_1.head(24+4)),lshift_adder_f32_1.tail(24+4))
  val lshift_adder_inv_f16_0    = Cat(Mux(RegEnable(adder_is_negative_f16_0, fire_reg0),~lshift_adder_f16_0.head(11+4),lshift_adder_f16_0.head(11+4)),lshift_adder_f16_0.tail(11+4))
  val lshift_adder_inv_f16_1    = Cat(Mux(RegEnable(adder_is_negative_f16_1, fire_reg0),~lshift_adder_f16_1.head(11+4),lshift_adder_f16_1.head(11+4)),lshift_adder_f16_1.tail(11+4))
  val lshift_adder_inv_f16_2    = Cat(Mux(RegEnable(adder_is_negative_f16_2, fire_reg0),~lshift_adder_f16_2.head(11+4),lshift_adder_f16_2.head(11+4)),lshift_adder_f16_2.tail(11+4))
  val lshift_adder_inv_f16_3    = Cat(Mux(RegEnable(adder_is_negative_f16_3, fire_reg0),~lshift_adder_f16_3.head(11+4),lshift_adder_f16_3.head(11+4)),lshift_adder_f16_3.tail(11+4))

  val is_fix_f64 = (tzd_adder_f64_reg1 + lzd_adder_inv_mask_f64_reg1) === adder_inv_f64.getWidth.U
  val is_fix_f32_0 = (tzd_adder_f32_0_reg1 + lzd_adder_inv_mask_f32_0_reg1) === adder_inv_f32_0.getWidth.U
  val is_fix_f32_1 = (tzd_adder_f32_1_reg1 + lzd_adder_inv_mask_f32_1_reg1) === adder_inv_f32_1.getWidth.U
  val is_fix_f16_0 = (tzd_adder_f16_0_reg1 + lzd_adder_inv_mask_f16_0_reg1) === adder_inv_f16_0.getWidth.U
  val is_fix_f16_1 = (tzd_adder_f16_1_reg1 + lzd_adder_inv_mask_f16_1_reg1) === adder_inv_f16_1.getWidth.U
  val is_fix_f16_2 = (tzd_adder_f16_2_reg1 + lzd_adder_inv_mask_f16_2_reg1) === adder_inv_f16_2.getWidth.U
  val is_fix_f16_3 = (tzd_adder_f16_3_reg1 + lzd_adder_inv_mask_f16_3_reg1) === adder_inv_f16_3.getWidth.U
  val lshift_adder_inv_fix_f64   = Mux(is_fix_f64, lshift_adder_inv_f64.head(adder_inv_f64.getWidth), lshift_adder_inv_f64.tail(1))
  val lshift_adder_inv_fix_f32_0 = Mux(is_fix_f32_0, lshift_adder_inv_f32_0.head(adder_inv_f32_0.getWidth), lshift_adder_inv_f32_0.tail(1))
  val lshift_adder_inv_fix_f32_1 = Mux(is_fix_f32_1, lshift_adder_inv_f32_1.head(adder_inv_f32_1.getWidth), lshift_adder_inv_f32_1.tail(1))
  val lshift_adder_inv_fix_f16_0 = Mux(is_fix_f16_0, lshift_adder_inv_f16_0.head(adder_inv_f16_0.getWidth), lshift_adder_inv_f16_0.tail(1))
  val lshift_adder_inv_fix_f16_1 = Mux(is_fix_f16_1, lshift_adder_inv_f16_1.head(adder_inv_f16_1.getWidth), lshift_adder_inv_f16_1.tail(1))
  val lshift_adder_inv_fix_f16_2 = Mux(is_fix_f16_2, lshift_adder_inv_f16_2.head(adder_inv_f16_2.getWidth), lshift_adder_inv_f16_2.tail(1))
  val lshift_adder_inv_fix_f16_3 = Mux(is_fix_f16_3, lshift_adder_inv_f16_3.head(adder_inv_f16_3.getWidth), lshift_adder_inv_f16_3.tail(1))

  val fraction_result_no_round_f64_reg2    = RegEnable(lshift_adder_inv_fix_f64.tail(1).head(significandWidth-1), fire_reg1)
  val fraction_result_no_round_f32_0_reg2  = RegEnable(lshift_adder_inv_fix_f32_0.tail(1).head(24-1), fire_reg1)
  val fraction_result_no_round_f32_1_reg2  = RegEnable(lshift_adder_inv_fix_f32_1.tail(1).head(24-1), fire_reg1)
  val fraction_result_no_round_f16_0_reg2  = RegEnable(lshift_adder_inv_fix_f16_0.tail(1).head(11-1), fire_reg1)
  val fraction_result_no_round_f16_1_reg2  = RegEnable(lshift_adder_inv_fix_f16_1.tail(1).head(11-1), fire_reg1)
  val fraction_result_no_round_f16_2_reg2  = RegEnable(lshift_adder_inv_fix_f16_2.tail(1).head(11-1), fire_reg1)
  val fraction_result_no_round_f16_3_reg2  = RegEnable(lshift_adder_inv_fix_f16_3.tail(1).head(11-1), fire_reg1)

  val fraction_result_round_f64   = fraction_result_no_round_f64_reg2 +& 1.U
  val fraction_result_round_f32_0 = fraction_result_no_round_f32_0_reg2 +& 1.U
  val fraction_result_round_f32_1 = fraction_result_no_round_f32_1_reg2 +& 1.U
  val fraction_result_round_f16_0 = fraction_result_no_round_f16_0_reg2 +& 1.U
  val fraction_result_round_f16_1 = fraction_result_no_round_f16_1_reg2 +& 1.U
  val fraction_result_round_f16_2 = fraction_result_no_round_f16_2_reg2 +& 1.U
  val fraction_result_round_f16_3 = fraction_result_no_round_f16_3_reg2 +& 1.U

  val sign_result_temp_f64_reg2   = RegEnable(RegEnable(Mux(adder_is_negative_f64  , RegEnable(sign_c_f64  , fire), RegEnable(sign_a_b_f64  , fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f32_0_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f32_0, RegEnable(sign_c_f32_0, fire), RegEnable(sign_a_b_f32_0, fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f32_1_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f32_1, RegEnable(sign_c_f32_1, fire), RegEnable(sign_a_b_f32_1, fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f16_0_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f16_0, RegEnable(sign_c_f16_0, fire), RegEnable(sign_a_b_f16_0, fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f16_1_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f16_1, RegEnable(sign_c_f16_1, fire), RegEnable(sign_a_b_f16_1, fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f16_2_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f16_2, RegEnable(sign_c_f16_2, fire), RegEnable(sign_a_b_f16_2, fire)), fire_reg0), fire_reg1)
  val sign_result_temp_f16_3_reg2 = RegEnable(RegEnable(Mux(adder_is_negative_f16_3, RegEnable(sign_c_f16_3, fire), RegEnable(sign_a_b_f16_3, fire)), fire_reg0), fire_reg1)

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

  val sticky_f64_reg2    = RegEnable(RegEnable(rshift_sticky_f64  , fire_reg0) | (lzd_adder_inv_mask_f64_reg1 + tzd_adder_f64_reg1 < (adder_inv_f64.getWidth-significandWidth-2).U), fire_reg1)
  val sticky_f32_0_reg2  = RegEnable(RegEnable(rshift_sticky_f32_0, fire_reg0) | (lzd_adder_inv_mask_f32_0_reg1 + tzd_adder_f32_0_reg1 < (adder_inv_f32_0.getWidth-24-2).U), fire_reg1)
  val sticky_f32_1_reg2  = RegEnable(RegEnable(rshift_sticky_f32_1, fire_reg0) | (lzd_adder_inv_mask_f32_1_reg1 + tzd_adder_f32_1_reg1 < (adder_inv_f32_1.getWidth-24-2).U), fire_reg1)
  val sticky_f16_0_reg2  = RegEnable(RegEnable(rshift_sticky_f16_0, fire_reg0) | (lzd_adder_inv_mask_f16_0_reg1 + tzd_adder_f16_0_reg1 < (adder_inv_f16_0.getWidth-11-2).U), fire_reg1)
  val sticky_f16_1_reg2  = RegEnable(RegEnable(rshift_sticky_f16_1, fire_reg0) | (lzd_adder_inv_mask_f16_1_reg1 + tzd_adder_f16_1_reg1 < (adder_inv_f16_1.getWidth-11-2).U), fire_reg1)
  val sticky_f16_2_reg2  = RegEnable(RegEnable(rshift_sticky_f16_2, fire_reg0) | (lzd_adder_inv_mask_f16_2_reg1 + tzd_adder_f16_2_reg1 < (adder_inv_f16_2.getWidth-11-2).U), fire_reg1)
  val sticky_f16_3_reg2  = RegEnable(RegEnable(rshift_sticky_f16_3, fire_reg0) | (lzd_adder_inv_mask_f16_3_reg1 + tzd_adder_f16_3_reg1 < (adder_inv_f16_3.getWidth-11-2).U), fire_reg1)
  val sticky_uf_f64_reg2    = RegEnable(RegEnable(rshift_sticky_f64  , fire_reg0) | (lzd_adder_inv_mask_f64_reg1 + tzd_adder_f64_reg1 < (adder_inv_f64.getWidth-significandWidth-3).U), fire_reg1)
  val sticky_uf_f32_0_reg2  = RegEnable(RegEnable(rshift_sticky_f32_0, fire_reg0) | (lzd_adder_inv_mask_f32_0_reg1 + tzd_adder_f32_0_reg1 < (adder_inv_f32_0.getWidth-24-3).U), fire_reg1)
  val sticky_uf_f32_1_reg2  = RegEnable(RegEnable(rshift_sticky_f32_1, fire_reg0) | (lzd_adder_inv_mask_f32_1_reg1 + tzd_adder_f32_1_reg1 < (adder_inv_f32_1.getWidth-24-3).U), fire_reg1)
  val sticky_uf_f16_0_reg2  = RegEnable(RegEnable(rshift_sticky_f16_0, fire_reg0) | (lzd_adder_inv_mask_f16_0_reg1 + tzd_adder_f16_0_reg1 < (adder_inv_f16_0.getWidth-11-3).U), fire_reg1)
  val sticky_uf_f16_1_reg2  = RegEnable(RegEnable(rshift_sticky_f16_1, fire_reg0) | (lzd_adder_inv_mask_f16_1_reg1 + tzd_adder_f16_1_reg1 < (adder_inv_f16_1.getWidth-11-3).U), fire_reg1)
  val sticky_uf_f16_2_reg2  = RegEnable(RegEnable(rshift_sticky_f16_2, fire_reg0) | (lzd_adder_inv_mask_f16_2_reg1 + tzd_adder_f16_2_reg1 < (adder_inv_f16_2.getWidth-11-3).U), fire_reg1)
  val sticky_uf_f16_3_reg2  = RegEnable(RegEnable(rshift_sticky_f16_3, fire_reg0) | (lzd_adder_inv_mask_f16_3_reg1 + tzd_adder_f16_3_reg1 < (adder_inv_f16_3.getWidth-11-3).U), fire_reg1)

  val round_lshift_f64_reg2   = RegEnable(lshift_adder_inv_fix_f64.tail(significandWidth+1).head(1), fire_reg1)
  val round_lshift_f32_0_reg2 = RegEnable(lshift_adder_inv_fix_f32_0.tail(24+1).head(1), fire_reg1)
  val round_lshift_f32_1_Reg2 = RegEnable(lshift_adder_inv_fix_f32_1.tail(24+1).head(1), fire_reg1)
  val round_lshift_f16_0_reg2 = RegEnable(lshift_adder_inv_fix_f16_0.tail(11+1).head(1), fire_reg1)
  val round_lshift_f16_1_reg2 = RegEnable(lshift_adder_inv_fix_f16_1.tail(11+1).head(1), fire_reg1)
  val round_lshift_f16_2_reg2 = RegEnable(lshift_adder_inv_fix_f16_2.tail(11+1).head(1), fire_reg1)
  val round_lshift_f16_3_reg2 = RegEnable(lshift_adder_inv_fix_f16_3.tail(11+1).head(1), fire_reg1)
  val guard_lshift_f64_reg2   = RegEnable(lshift_adder_inv_fix_f64.tail(significandWidth).head(1), fire_reg1)
  val guard_lshift_f32_0_reg2 = RegEnable(lshift_adder_inv_fix_f32_0.tail(24).head(1), fire_reg1)
  val guard_lshift_f32_1_reg2 = RegEnable(lshift_adder_inv_fix_f32_1.tail(24).head(1), fire_reg1)
  val guard_lshift_f16_0_reg2 = RegEnable(lshift_adder_inv_fix_f16_0.tail(11).head(1), fire_reg1)
  val guard_lshift_f16_1_reg2 = RegEnable(lshift_adder_inv_fix_f16_1.tail(11).head(1), fire_reg1)
  val guard_lshift_f16_2_reg2 = RegEnable(lshift_adder_inv_fix_f16_2.tail(11).head(1), fire_reg1)
  val guard_lshift_f16_3_reg2 = RegEnable(lshift_adder_inv_fix_f16_3.tail(11).head(1), fire_reg1)
  val round_f64     = Mux(adder_is_negative_f64_reg2, round_lshift_f64_reg2 ^ !sticky_f64_reg2, round_lshift_f64_reg2)
  val round_f32_0   = Mux(adder_is_negative_f32_0_reg2, round_lshift_f32_0_reg2 ^ !sticky_f32_0_reg2, round_lshift_f32_0_reg2)
  val round_f32_1   = Mux(adder_is_negative_f32_1_reg2, round_lshift_f32_1_Reg2 ^ !sticky_f32_1_reg2, round_lshift_f32_1_Reg2)
  val round_f16_0   = Mux(adder_is_negative_f16_0_reg2, round_lshift_f16_0_reg2 ^ !sticky_f16_0_reg2, round_lshift_f16_0_reg2)
  val round_f16_1   = Mux(adder_is_negative_f16_1_reg2, round_lshift_f16_1_reg2 ^ !sticky_f16_1_reg2, round_lshift_f16_1_reg2)
  val round_f16_2   = Mux(adder_is_negative_f16_2_reg2, round_lshift_f16_2_reg2 ^ !sticky_f16_2_reg2, round_lshift_f16_2_reg2)
  val round_f16_3   = Mux(adder_is_negative_f16_3_reg2, round_lshift_f16_3_reg2 ^ !sticky_f16_3_reg2, round_lshift_f16_3_reg2)
  val guard_f64     = Mux(adder_is_negative_f64_reg2, guard_lshift_f64_reg2 ^ (!sticky_f64_reg2 & round_lshift_f64_reg2), guard_lshift_f64_reg2)
  val guard_f32_0   = Mux(adder_is_negative_f32_0_reg2, guard_lshift_f32_0_reg2 ^ (!sticky_f32_0_reg2 & round_lshift_f32_0_reg2), guard_lshift_f32_0_reg2)
  val guard_f32_1   = Mux(adder_is_negative_f32_1_reg2, guard_lshift_f32_1_reg2 ^ (!sticky_f32_1_reg2 & round_lshift_f32_1_Reg2), guard_lshift_f32_1_reg2)
  val guard_f16_0   = Mux(adder_is_negative_f16_0_reg2, guard_lshift_f16_0_reg2 ^ (!sticky_f16_0_reg2 & round_lshift_f16_0_reg2), guard_lshift_f16_0_reg2)
  val guard_f16_1   = Mux(adder_is_negative_f16_1_reg2, guard_lshift_f16_1_reg2 ^ (!sticky_f16_1_reg2 & round_lshift_f16_1_reg2), guard_lshift_f16_1_reg2)
  val guard_f16_2   = Mux(adder_is_negative_f16_2_reg2, guard_lshift_f16_2_reg2 ^ (!sticky_f16_2_reg2 & round_lshift_f16_2_reg2), guard_lshift_f16_2_reg2)
  val guard_f16_3   = Mux(adder_is_negative_f16_3_reg2, guard_lshift_f16_3_reg2 ^ (!sticky_f16_3_reg2 & round_lshift_f16_3_reg2), guard_lshift_f16_3_reg2)
  val guard_uf_f64   = round_f64
  val guard_uf_f32_0   = round_f32_0
  val guard_uf_f32_1   = round_f32_1
  val guard_uf_f16_0   = round_f16_0
  val guard_uf_f16_1   = round_f16_1
  val guard_uf_f16_2   = round_f16_2
  val guard_uf_f16_3   = round_f16_3
  val round_lshift_uf_f64_reg2   = RegEnable(lshift_adder_inv_fix_f64.tail(significandWidth+2).head(1), fire_reg1)
  val round_lshift_uf_f32_0_reg2 = RegEnable(lshift_adder_inv_fix_f32_0.tail(24+2).head(1), fire_reg1)
  val round_lshift_uf_f32_1_reg2 = RegEnable(lshift_adder_inv_fix_f32_1.tail(24+2).head(1), fire_reg1)
  val round_lshift_uf_f16_0_reg2 = RegEnable(lshift_adder_inv_fix_f16_0.tail(11+2).head(1), fire_reg1)
  val round_lshift_uf_f16_1_reg2 = RegEnable(lshift_adder_inv_fix_f16_1.tail(11+2).head(1), fire_reg1)
  val round_lshift_uf_f16_2_reg2 = RegEnable(lshift_adder_inv_fix_f16_2.tail(11+2).head(1), fire_reg1)
  val round_lshift_uf_f16_3_reg2 = RegEnable(lshift_adder_inv_fix_f16_3.tail(11+2).head(1), fire_reg1)
  val round_uf_f64     = Mux(adder_is_negative_f64_reg2, round_lshift_uf_f64_reg2 ^ !sticky_uf_f64_reg2, round_lshift_uf_f64_reg2)
  val round_uf_f32_0   = Mux(adder_is_negative_f32_0_reg2, round_lshift_uf_f32_0_reg2 ^ !sticky_uf_f32_0_reg2, round_lshift_uf_f32_0_reg2)
  val round_uf_f32_1   = Mux(adder_is_negative_f32_1_reg2, round_lshift_uf_f32_1_reg2 ^ !sticky_uf_f32_1_reg2, round_lshift_uf_f32_1_reg2)
  val round_uf_f16_0   = Mux(adder_is_negative_f16_0_reg2, round_lshift_uf_f16_0_reg2 ^ !sticky_uf_f16_0_reg2, round_lshift_uf_f16_0_reg2)
  val round_uf_f16_1   = Mux(adder_is_negative_f16_1_reg2, round_lshift_uf_f16_1_reg2 ^ !sticky_uf_f16_1_reg2, round_lshift_uf_f16_1_reg2)
  val round_uf_f16_2   = Mux(adder_is_negative_f16_2_reg2, round_lshift_uf_f16_2_reg2 ^ !sticky_uf_f16_2_reg2, round_lshift_uf_f16_2_reg2)
  val round_uf_f16_3   = Mux(adder_is_negative_f16_3_reg2, round_lshift_uf_f16_3_reg2 ^ !sticky_uf_f16_3_reg2, round_lshift_uf_f16_3_reg2)

  val round_add1_f64 = Wire(UInt(1.W))
  round_add1_f64 := RNE_reg2 & (guard_f64 & (fraction_result_no_round_f64_reg2(0) | round_f64 | sticky_f64_reg2)) |
    RDN_reg2 & sign_result_temp_f64_reg2 & (guard_f64|round_f64|sticky_f64_reg2) |
    RUP_reg2 & !sign_result_temp_f64_reg2 & (guard_f64|round_f64|sticky_f64_reg2) |
    RMM_reg2 & guard_f64 |
    adder_is_negative_f64_reg2 & !guard_f64 & !round_f64 & !sticky_f64_reg2
  val round_add1_f32_0 = Wire(UInt(1.W))
  round_add1_f32_0 := RNE_reg2 & (guard_f32_0 & (fraction_result_no_round_f32_0_reg2(0) | round_f32_0 | sticky_f32_0_reg2)) |
    RDN_reg2 & sign_result_temp_f32_0_reg2 & (guard_f32_0|round_f32_0|sticky_f32_0_reg2) |
    RUP_reg2 & !sign_result_temp_f32_0_reg2 & (guard_f32_0|round_f32_0|sticky_f32_0_reg2) |
    RMM_reg2 & guard_f32_0 |
    adder_is_negative_f32_0_reg2 & !guard_f32_0 & !round_f32_0 & !sticky_f32_0_reg2
  val round_add1_f32_1 = Wire(UInt(1.W))
  round_add1_f32_1 := RNE_reg2 & (guard_f32_1 & (fraction_result_no_round_f32_1_reg2(0) | round_f32_1 | sticky_f32_1_reg2)) |
    RDN_reg2 & sign_result_temp_f32_1_reg2 & (guard_f32_1|round_f32_1|sticky_f32_1_reg2) |
    RUP_reg2 & !sign_result_temp_f32_1_reg2 & (guard_f32_1|round_f32_1|sticky_f32_1_reg2) |
    RMM_reg2 & guard_f32_1 |
    adder_is_negative_f32_1_reg2 & !guard_f32_1 & !round_f32_1 & !sticky_f32_1_reg2
  val round_add1_f16_0 = Wire(UInt(1.W))
  round_add1_f16_0 := RNE_reg2 & (guard_f16_0 & (fraction_result_no_round_f16_0_reg2(0) | round_f16_0 | sticky_f16_0_reg2)) |
    RDN_reg2 & sign_result_temp_f16_0_reg2 & (guard_f16_0|round_f16_0|sticky_f16_0_reg2) |
    RUP_reg2 & !sign_result_temp_f16_0_reg2 & (guard_f16_0|round_f16_0|sticky_f16_0_reg2) |
    RMM_reg2 & guard_f16_0 |
    adder_is_negative_f16_0_reg2 & !guard_f16_0 & !round_f16_0 & !sticky_f16_0_reg2
  val round_add1_f16_1 = Wire(UInt(1.W))
  round_add1_f16_1 := RNE_reg2 & (guard_f16_1 & (fraction_result_no_round_f16_1_reg2(0) | round_f16_1 | sticky_f16_1_reg2)) |
    RDN_reg2 & sign_result_temp_f16_1_reg2 & (guard_f16_1|round_f16_1|sticky_f16_1_reg2) |
    RUP_reg2 & !sign_result_temp_f16_1_reg2 & (guard_f16_1|round_f16_1|sticky_f16_1_reg2) |
    RMM_reg2 & guard_f16_1 |
    adder_is_negative_f16_1_reg2 & !guard_f16_1 & !round_f16_1 & !sticky_f16_1_reg2
  val round_add1_f16_2 = Wire(UInt(1.W))
  round_add1_f16_2 := RNE_reg2 & (guard_f16_2 & (fraction_result_no_round_f16_2_reg2(0) | round_f16_2 | sticky_f16_2_reg2)) |
    RDN_reg2 & sign_result_temp_f16_2_reg2 & (guard_f16_2|round_f16_2|sticky_f16_2_reg2) |
    RUP_reg2 & !sign_result_temp_f16_2_reg2 & (guard_f16_2|round_f16_2|sticky_f16_2_reg2) |
    RMM_reg2 & guard_f16_2 |
    adder_is_negative_f16_2_reg2 & !guard_f16_2 & !round_f16_2 & !sticky_f16_2_reg2
  val round_add1_f16_3 = Wire(UInt(1.W))
  round_add1_f16_3 := RNE_reg2 & (guard_f16_3 & (fraction_result_no_round_f16_3_reg2(0) | round_f16_3 | sticky_f16_3_reg2)) |
    RDN_reg2 & sign_result_temp_f16_3_reg2 & (guard_f16_3|round_f16_3|sticky_f16_3_reg2) |
    RUP_reg2 & !sign_result_temp_f16_3_reg2 & (guard_f16_3|round_f16_3|sticky_f16_3_reg2) |
    RMM_reg2 & guard_f16_3 |
    adder_is_negative_f16_3_reg2 & !guard_f16_3 & !round_f16_3 & !sticky_f16_3_reg2
  val round_add1_uf_f64 = RNE_reg2 & (guard_uf_f64 & (guard_f64 | round_uf_f64 | sticky_uf_f64_reg2)) |
    RDN_reg2 & sign_result_temp_f64_reg2 & (guard_uf_f64|round_uf_f64|sticky_uf_f64_reg2) |
    RUP_reg2 & !sign_result_temp_f64_reg2 & (guard_uf_f64|round_uf_f64|sticky_uf_f64_reg2) |
    RMM_reg2 & guard_uf_f64
  val round_add1_uf_f32_0 = RNE_reg2 & (guard_uf_f32_0 & (guard_f32_0 | round_uf_f32_0 | sticky_uf_f32_0_reg2)) |
    RDN_reg2 & sign_result_temp_f32_0_reg2 & (guard_uf_f32_0|round_uf_f32_0|sticky_uf_f32_0_reg2) |
    RUP_reg2 & !sign_result_temp_f32_0_reg2 & (guard_uf_f32_0|round_uf_f32_0|sticky_uf_f32_0_reg2) |
    RMM_reg2 & guard_uf_f32_0
  val round_add1_uf_f32_1 = RNE_reg2 & (guard_uf_f32_1 & (guard_f32_1 | round_uf_f32_1 | sticky_uf_f32_1_reg2)) |
    RDN_reg2 & sign_result_temp_f32_1_reg2 & (guard_uf_f32_1|round_uf_f32_1|sticky_uf_f32_1_reg2) |
    RUP_reg2 & !sign_result_temp_f32_1_reg2 & (guard_uf_f32_1|round_uf_f32_1|sticky_uf_f32_1_reg2) |
    RMM_reg2 & guard_uf_f32_1
  val round_add1_uf_f16_0 = RNE_reg2 & (guard_uf_f16_0 & (guard_f16_0 | round_uf_f16_0 | sticky_uf_f16_0_reg2)) |
    RDN_reg2 & sign_result_temp_f16_0_reg2 & (guard_uf_f16_0|round_uf_f16_0|sticky_uf_f16_0_reg2) |
    RUP_reg2 & !sign_result_temp_f16_0_reg2 & (guard_uf_f16_0|round_uf_f16_0|sticky_uf_f16_0_reg2) |
    RMM_reg2 & guard_uf_f16_0
  val round_add1_uf_f16_1 = RNE_reg2 & (guard_uf_f16_1 & (guard_f16_1 | round_uf_f16_1 | sticky_uf_f16_1_reg2)) |
    RDN_reg2 & sign_result_temp_f16_1_reg2 & (guard_uf_f16_1|round_uf_f16_1|sticky_uf_f16_1_reg2) |
    RUP_reg2 & !sign_result_temp_f16_1_reg2 & (guard_uf_f16_1|round_uf_f16_1|sticky_uf_f16_1_reg2) |
    RMM_reg2 & guard_uf_f16_1
  val round_add1_uf_f16_2 = RNE_reg2 & (guard_uf_f16_2 & (guard_f16_2 | round_uf_f16_2 | sticky_uf_f16_2_reg2)) |
    RDN_reg2 & sign_result_temp_f16_2_reg2 & (guard_uf_f16_2|round_uf_f16_2|sticky_uf_f16_2_reg2) |
    RUP_reg2 & !sign_result_temp_f16_2_reg2 & (guard_uf_f16_2|round_uf_f16_2|sticky_uf_f16_2_reg2) |
    RMM_reg2 & guard_uf_f16_2
  val round_add1_uf_f16_3 = RNE_reg2 & (guard_uf_f16_3 & (guard_f16_3 | round_uf_f16_3 | sticky_uf_f16_3_reg2)) |
    RDN_reg2 & sign_result_temp_f16_3_reg2 & (guard_uf_f16_3|round_uf_f16_3|sticky_uf_f16_3_reg2) |
    RUP_reg2 & !sign_result_temp_f16_3_reg2 & (guard_uf_f16_3|round_uf_f16_3|sticky_uf_f16_3_reg2) |
    RMM_reg2 & guard_uf_f16_3
  val exponent_add_1_f64 = fraction_result_no_round_f64_reg2.andR & round_add1_f64.asBool
  val exponent_add_1_f32_0 = fraction_result_no_round_f32_0_reg2.andR & round_add1_f32_0.asBool
  val exponent_add_1_f32_1 = fraction_result_no_round_f32_1_reg2.andR & round_add1_f32_1.asBool
  val exponent_add_1_f16_0 = fraction_result_no_round_f16_0_reg2.andR & round_add1_f16_0.asBool
  val exponent_add_1_f16_1 = fraction_result_no_round_f16_1_reg2.andR & round_add1_f16_1.asBool
  val exponent_add_1_f16_2 = fraction_result_no_round_f16_2_reg2.andR & round_add1_f16_2.asBool
  val exponent_add_1_f16_3 = fraction_result_no_round_f16_3_reg2.andR & round_add1_f16_3.asBool





























  val exponent_result_add_value_f64 = Mux(exponent_add_1_f64 | RegEnable(is_fix_f64, fire_reg1),
    E_greater_f64_reg2 - RegEnable(lshift_value_f64_reg1, fire_reg1) + 1.U,
    E_greater_f64_reg2 - RegEnable(lshift_value_f64_reg1, fire_reg1)
  )
  val exponent_result_add_value_f32_0 = Mux(exponent_add_1_f32_0 | RegEnable(is_fix_f32_0, fire_reg1),
    E_greater_f32_0_reg2 - RegEnable(lshift_value_f32_0_reg1, fire_reg1) + 1.U,
    E_greater_f32_0_reg2 - RegEnable(lshift_value_f32_0_reg1, fire_reg1)
  )
  val exponent_result_add_value_f32_1 = Mux(exponent_add_1_f32_1 | RegEnable(is_fix_f32_1, fire_reg1),
    E_greater_f32_1_reg2 - RegEnable(lshift_value_f32_1_reg1, fire_reg1) + 1.U,
    E_greater_f32_1_reg2 - RegEnable(lshift_value_f32_1_reg1, fire_reg1)
  )
  val exponent_result_add_value_f16_0 = Mux(exponent_add_1_f16_0 | RegEnable(is_fix_f16_0, fire_reg1),
    E_greater_f16_0_reg2 - RegEnable(lshift_value_f16_0_reg1, fire_reg1) + 1.U,
    E_greater_f16_0_reg2 - RegEnable(lshift_value_f16_0_reg1, fire_reg1)
  )
  val exponent_result_add_value_f16_1 = Mux(exponent_add_1_f16_1 | RegEnable(is_fix_f16_1, fire_reg1),
    E_greater_f16_1_reg2 - RegEnable(lshift_value_f16_1_reg1, fire_reg1) + 1.U,
    E_greater_f16_1_reg2 - RegEnable(lshift_value_f16_1_reg1, fire_reg1)
  )
  val exponent_result_add_value_f16_2 = Mux(exponent_add_1_f16_2 | RegEnable(is_fix_f16_2, fire_reg1),
    E_greater_f16_2_reg2 - RegEnable(lshift_value_f16_2_reg1, fire_reg1) + 1.U,
    E_greater_f16_2_reg2 - RegEnable(lshift_value_f16_2_reg1, fire_reg1)
  )
  val exponent_result_add_value_f16_3 = Mux(exponent_add_1_f16_3 | RegEnable(is_fix_f16_3, fire_reg1),
    E_greater_f16_3_reg2 - RegEnable(lshift_value_f16_3_reg1, fire_reg1) + 1.U,
    E_greater_f16_3_reg2 - RegEnable(lshift_value_f16_3_reg1, fire_reg1)
  )
  val exponent_overflow_f64           = exponent_result_add_value_f64.head(1).asBool | exponent_result_add_value_f64.tail(1).andR
  val exponent_overflow_f32_0         = exponent_result_add_value_f32_0.head(1).asBool | exponent_result_add_value_f32_0.tail(1).andR
  val exponent_overflow_f32_1         = exponent_result_add_value_f32_1.head(1).asBool | exponent_result_add_value_f32_1.tail(1).andR
  val exponent_overflow_f16_0         = exponent_result_add_value_f16_0.head(1).asBool | exponent_result_add_value_f16_0.tail(1).andR
  val exponent_overflow_f16_1         = exponent_result_add_value_f16_1.head(1).asBool | exponent_result_add_value_f16_1.tail(1).andR
  val exponent_overflow_f16_2         = exponent_result_add_value_f16_2.head(1).asBool | exponent_result_add_value_f16_2.tail(1).andR
  val exponent_overflow_f16_3         = exponent_result_add_value_f16_3.head(1).asBool | exponent_result_add_value_f16_3.tail(1).andR
  val exponent_is_min_f64             = RegEnable(!lshift_adder_inv_fix_f64.head(1).asBool & lshift_mask_valid_f64_reg1 & !is_fix_f64,       fire_reg1)
  val exponent_is_min_f32_0           = RegEnable(!lshift_adder_inv_fix_f32_0.head(1).asBool & lshift_mask_valid_f32_0_reg1 & !is_fix_f32_0, fire_reg1)
  val exponent_is_min_f32_1           = RegEnable(!lshift_adder_inv_fix_f32_1.head(1).asBool & lshift_mask_valid_f32_1_reg1 & !is_fix_f32_1, fire_reg1)
  val exponent_is_min_f16_0           = RegEnable(!lshift_adder_inv_fix_f16_0.head(1).asBool & lshift_mask_valid_f16_0_reg1 & !is_fix_f16_0, fire_reg1)
  val exponent_is_min_f16_1           = RegEnable(!lshift_adder_inv_fix_f16_1.head(1).asBool & lshift_mask_valid_f16_1_reg1 & !is_fix_f16_1, fire_reg1)
  val exponent_is_min_f16_2           = RegEnable(!lshift_adder_inv_fix_f16_2.head(1).asBool & lshift_mask_valid_f16_2_reg1 & !is_fix_f16_2, fire_reg1)
  val exponent_is_min_f16_3           = RegEnable(!lshift_adder_inv_fix_f16_3.head(1).asBool & lshift_mask_valid_f16_3_reg1 & !is_fix_f16_3, fire_reg1)
  val exponent_result_temp_f64      = Mux(exponent_is_min_f64,
    Cat(0.U((exponentWidth-1).W),exponent_add_1_f64),
    exponent_result_add_value_f64(exponentWidth-1,0)
  )
  val exponent_result_temp_f32_0      = Mux(exponent_is_min_f32_0,
    Cat(0.U((5-1).W),exponent_add_1_f32_0),
    exponent_result_add_value_f32_0(8-1,0)
  )
  val exponent_result_temp_f32_1      = Mux(exponent_is_min_f32_1,
    Cat(0.U((5-1).W),exponent_add_1_f32_1),
    exponent_result_add_value_f32_1(8-1,0)
  )
  val exponent_result_temp_f16_0      = Mux(exponent_is_min_f16_0,
    Cat(0.U((5-1).W),exponent_add_1_f16_0),
    exponent_result_add_value_f16_0(5-1,0)
  )
  val exponent_result_temp_f16_1      = Mux(exponent_is_min_f16_1,
    Cat(0.U((5-1).W),exponent_add_1_f16_1),
    exponent_result_add_value_f16_1(5-1,0)
  )
  val exponent_result_temp_f16_2      = Mux(exponent_is_min_f16_2,
    Cat(0.U((5-1).W),exponent_add_1_f16_2),
    exponent_result_add_value_f16_2(5-1,0)
  )
  val exponent_result_temp_f16_3      = Mux(exponent_is_min_f16_3,
    Cat(0.U((5-1).W),exponent_add_1_f16_3),
    exponent_result_add_value_f16_3(5-1,0)
  )
  val fraction_result_temp_f64  = Mux(round_add1_f64.asBool, fraction_result_round_f64.tail(1), fraction_result_no_round_f64_reg2)
  val fraction_result_temp_f32_0  = Mux(round_add1_f32_0.asBool, fraction_result_round_f32_0.tail(1), fraction_result_no_round_f32_0_reg2)
  val fraction_result_temp_f32_1  = Mux(round_add1_f32_1.asBool, fraction_result_round_f32_1.tail(1), fraction_result_no_round_f32_1_reg2)
  val fraction_result_temp_f16_0  = Mux(round_add1_f16_0.asBool, fraction_result_round_f16_0.tail(1), fraction_result_no_round_f16_0_reg2)
  val fraction_result_temp_f16_1  = Mux(round_add1_f16_1.asBool, fraction_result_round_f16_1.tail(1), fraction_result_no_round_f16_1_reg2)
  val fraction_result_temp_f16_2  = Mux(round_add1_f16_2.asBool, fraction_result_round_f16_2.tail(1), fraction_result_no_round_f16_2_reg2)
  val fraction_result_temp_f16_3  = Mux(round_add1_f16_3.asBool, fraction_result_round_f16_3.tail(1), fraction_result_no_round_f16_3_reg2)

  val NV_f64,DZ_f64,OF_f64,UF_f64,NX_f64  = WireInit(false.B)
  val NV_f32_0,DZ_f32_0,OF_f32_0,UF_f32_0,NX_f32_0  = WireInit(false.B)
  val NV_f32_1,DZ_f32_1,OF_f32_1,UF_f32_1,NX_f32_1  = WireInit(false.B)
  val NV_f16_0,DZ_f16_0,OF_f16_0,UF_f16_0,NX_f16_0  = WireInit(false.B)
  val NV_f16_1,DZ_f16_1,OF_f16_1,UF_f16_1,NX_f16_1  = WireInit(false.B)
  val NV_f16_2,DZ_f16_2,OF_f16_2,UF_f16_2,NX_f16_2  = WireInit(false.B)
  val NV_f16_3,DZ_f16_3,OF_f16_3,UF_f16_3,NX_f16_3  = WireInit(false.B)
  val fflags_f64 = WireInit(Cat(NV_f64,DZ_f64,OF_f64,UF_f64,NX_f64))
  val fflags_f32_0 = WireInit(Cat(NV_f32_0,DZ_f32_0,OF_f32_0,UF_f32_0,NX_f32_0))
  val fflags_f32_1 = WireInit(Cat(NV_f32_1,DZ_f32_1,OF_f32_1,UF_f32_1,NX_f32_1))
  val fflags_f16_0 = WireInit(Cat(NV_f16_0,DZ_f16_0,OF_f16_0,UF_f16_0,NX_f16_0))
  val fflags_f16_1 = WireInit(Cat(NV_f16_1,DZ_f16_1,OF_f16_1,UF_f16_1,NX_f16_1))
  val fflags_f16_2 = WireInit(Cat(NV_f16_2,DZ_f16_2,OF_f16_2,UF_f16_2,NX_f16_2))
  val fflags_f16_3 = WireInit(Cat(NV_f16_3,DZ_f16_3,OF_f16_3,UF_f16_3,NX_f16_3))
  NX_f64   := guard_f64 | round_f64 | sticky_f64_reg2
  NX_f32_0 := guard_f32_0 | round_f32_0 | sticky_f32_0_reg2
  NX_f32_1 := guard_f32_1 | round_f32_1 | sticky_f32_1_reg2
  NX_f16_0 := guard_f16_0 | round_f16_0 | sticky_f16_0_reg2
  NX_f16_1 := guard_f16_1 | round_f16_1 | sticky_f16_1_reg2
  NX_f16_2 := guard_f16_2 | round_f16_2 | sticky_f16_2_reg2
  NX_f16_3 := guard_f16_3 | round_f16_3 | sticky_f16_3_reg2

  UF_f64 := NX_f64 & exponent_is_min_f64 & ( !exponent_add_1_f64 | !(guard_f64 & round_add1_uf_f64))
  UF_f32_0 := NX_f32_0 & exponent_is_min_f32_0 & ( !exponent_add_1_f32_0 | !(guard_f32_0 & round_add1_uf_f32_0))
  UF_f32_1 := NX_f32_1 & exponent_is_min_f32_1 & ( !exponent_add_1_f32_1 | !(guard_f32_1 & round_add1_uf_f32_1))
  UF_f16_0 := NX_f16_0 & exponent_is_min_f16_0 & ( !exponent_add_1_f16_0 | !(guard_f16_0 & round_add1_uf_f16_0))
  UF_f16_1 := NX_f16_1 & exponent_is_min_f16_1 & ( !exponent_add_1_f16_1 | !(guard_f16_1 & round_add1_uf_f16_1))
  UF_f16_2 := NX_f16_2 & exponent_is_min_f16_2 & ( !exponent_add_1_f16_2 | !(guard_f16_2 & round_add1_uf_f16_2))
  UF_f16_3 := NX_f16_3 & exponent_is_min_f16_3 & ( !exponent_add_1_f16_3 | !(guard_f16_3 & round_add1_uf_f16_3))
  val fp_a_is_zero_f64   = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f64.orR
  val fp_a_is_zero_f32_0 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f32_0.orR
  val fp_a_is_zero_f32_1 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f32_1.orR
  val fp_a_is_zero_f16_0 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f16_0.orR
  val fp_a_is_zero_f16_1 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f16_1.orR
  val fp_a_is_zero_f16_2 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f16_2.orR
  val fp_a_is_zero_f16_3 = !io.fp_aIsFpCanonicalNAN & !fp_a_significand_f16_3.orR
  val fp_b_is_zero_f64   = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f64.orR
  val fp_b_is_zero_f32_0 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f32_0.orR
  val fp_b_is_zero_f32_1 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f32_1.orR
  val fp_b_is_zero_f16_0 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f16_0.orR
  val fp_b_is_zero_f16_1 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f16_1.orR
  val fp_b_is_zero_f16_2 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f16_2.orR
  val fp_b_is_zero_f16_3 = !io.fp_bIsFpCanonicalNAN & !fp_b_significand_f16_3.orR
  val fp_c_is_zero_f64   = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f64.orR
  val fp_c_is_zero_f32_0 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f32_0.orR
  val fp_c_is_zero_f32_1 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f32_1.orR
  val fp_c_is_zero_f16_0 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f16_0.orR
  val fp_c_is_zero_f16_1 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f16_1.orR
  val fp_c_is_zero_f16_2 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f16_2.orR
  val fp_c_is_zero_f16_3 = !io.fp_cIsFpCanonicalNAN & !fp_c_significand_f16_3.orR

  val normal_result_is_zero_f64_reg2   = RegEnable(RegEnable(!adder_f64.orR  , fire_reg0), fire_reg1)
  val normal_result_is_zero_f32_0_reg2 = RegEnable(RegEnable(!adder_f32_0.orR, fire_reg0), fire_reg1)
  val normal_result_is_zero_f32_1_reg2 = RegEnable(RegEnable(!adder_f32_1.orR, fire_reg0), fire_reg1)
  val normal_result_is_zero_f16_0_reg2 = RegEnable(RegEnable(!adder_f16_0.orR, fire_reg0), fire_reg1)
  val normal_result_is_zero_f16_1_reg2 = RegEnable(RegEnable(!adder_f16_1.orR, fire_reg0), fire_reg1)
  val normal_result_is_zero_f16_2_reg2 = RegEnable(RegEnable(!adder_f16_2.orR, fire_reg0), fire_reg1)
  val normal_result_is_zero_f16_3_reg2 = RegEnable(RegEnable(!adder_f16_3.orR, fire_reg0), fire_reg1)
  val has_zero_f64_reg2   = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f64   | fp_b_is_zero_f64   | fp_c_is_zero_f64  , fire), fire_reg0), fire_reg1) | normal_result_is_zero_f64_reg2
  val has_zero_f32_0_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f32_0 | fp_b_is_zero_f32_0 | fp_c_is_zero_f32_0, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f32_0_reg2
  val has_zero_f32_1_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f32_1 | fp_b_is_zero_f32_1 | fp_c_is_zero_f32_1, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f32_1_reg2
  val has_zero_f16_0_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_0 | fp_b_is_zero_f16_0 | fp_c_is_zero_f16_0, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f16_0_reg2
  val has_zero_f16_1_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_1 | fp_b_is_zero_f16_1 | fp_c_is_zero_f16_1, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f16_1_reg2
  val has_zero_f16_2_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_2 | fp_b_is_zero_f16_2 | fp_c_is_zero_f16_2, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f16_2_reg2
  val has_zero_f16_3_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_3 | fp_b_is_zero_f16_3 | fp_c_is_zero_f16_3, fire), fire_reg0), fire_reg1) | normal_result_is_zero_f16_3_reg2
  val normal_result_f64 = Cat(sign_result_temp_f64_reg2,exponent_result_temp_f64,fraction_result_temp_f64)
  val normal_result_f32_0 = Cat(sign_result_temp_f32_0_reg2,exponent_result_temp_f32_0,fraction_result_temp_f32_0)
  val normal_result_f32_1 = Cat(sign_result_temp_f32_1_reg2,exponent_result_temp_f32_1,fraction_result_temp_f32_1)
  val normal_result_f16_0 = Cat(sign_result_temp_f16_0_reg2,exponent_result_temp_f16_0,fraction_result_temp_f16_0)
  val normal_result_f16_1 = Cat(sign_result_temp_f16_1_reg2,exponent_result_temp_f16_1,fraction_result_temp_f16_1)
  val normal_result_f16_2 = Cat(sign_result_temp_f16_2_reg2,exponent_result_temp_f16_2,fraction_result_temp_f16_2)
  val normal_result_f16_3 = Cat(sign_result_temp_f16_3_reg2,exponent_result_temp_f16_3,fraction_result_temp_f16_3)
  val is_overflow_f64_reg2   = exponent_overflow_f64
  val is_overflow_f32_0_reg2 = exponent_overflow_f32_0
  val is_overflow_f32_1_reg2 = exponent_overflow_f32_1
  val is_overflow_f16_0_reg2 = exponent_overflow_f16_0
  val is_overflow_f16_1_reg2 = exponent_overflow_f16_1
  val is_overflow_f16_2_reg2 = exponent_overflow_f16_2
  val is_overflow_f16_3_reg2 = exponent_overflow_f16_3
  val result_overflow_up_f64   = Cat(sign_result_temp_f64_reg2, Fill(exponentWidth,1.U), 0.U((significandWidth-1).W))
  val result_overflow_up_f32_0   = Cat(sign_result_temp_f32_0_reg2, Fill(8,1.U), 0.U((24-1).W))
  val result_overflow_up_f32_1   = Cat(sign_result_temp_f32_1_reg2, Fill(8,1.U), 0.U((24-1).W))
  val result_overflow_up_f16_0   = Cat(sign_result_temp_f16_0_reg2, Fill(5,1.U), 0.U((11-1).W))
  val result_overflow_up_f16_1   = Cat(sign_result_temp_f16_1_reg2, Fill(5,1.U), 0.U((11-1).W))
  val result_overflow_up_f16_2   = Cat(sign_result_temp_f16_2_reg2, Fill(5,1.U), 0.U((11-1).W))
  val result_overflow_up_f16_3   = Cat(sign_result_temp_f16_3_reg2, Fill(5,1.U), 0.U((11-1).W))
  val result_overflow_down_f64 = Cat(sign_result_temp_f64_reg2, Fill(exponentWidth-1,1.U), 0.U, Fill(significandWidth-1,1.U))
  val result_overflow_down_f32_0 = Cat(sign_result_temp_f32_0_reg2, Fill(8-1,1.U), 0.U, Fill(24-1,1.U))
  val result_overflow_down_f32_1 = Cat(sign_result_temp_f32_1_reg2, Fill(8-1,1.U), 0.U, Fill(24-1,1.U))
  val result_overflow_down_f16_0 = Cat(sign_result_temp_f16_0_reg2, Fill(5-1,1.U), 0.U, Fill(11-1,1.U))
  val result_overflow_down_f16_1 = Cat(sign_result_temp_f16_1_reg2, Fill(5-1,1.U), 0.U, Fill(11-1,1.U))
  val result_overflow_down_f16_2 = Cat(sign_result_temp_f16_2_reg2, Fill(5-1,1.U), 0.U, Fill(11-1,1.U))
  val result_overflow_down_f16_3 = Cat(sign_result_temp_f16_3_reg2, Fill(5-1,1.U), 0.U, Fill(11-1,1.U))
  val fp_a_is_nan_f64   = io.fp_aIsFpCanonicalNAN | Ea_f64.andR   & fp_a_significand_f64.tail(1).orR
  val fp_a_is_nan_f32_0 = io.fp_aIsFpCanonicalNAN | Ea_f32_0.andR & fp_a_significand_f32_0.tail(1).orR
  val fp_a_is_nan_f32_1 = io.fp_aIsFpCanonicalNAN | Ea_f32_1.andR & fp_a_significand_f32_1.tail(1).orR
  val fp_a_is_nan_f16_0 = io.fp_aIsFpCanonicalNAN | Ea_f16_0.andR & fp_a_significand_f16_0.tail(1).orR
  val fp_a_is_nan_f16_1 = io.fp_aIsFpCanonicalNAN | Ea_f16_1.andR & fp_a_significand_f16_1.tail(1).orR
  val fp_a_is_nan_f16_2 = io.fp_aIsFpCanonicalNAN | Ea_f16_2.andR & fp_a_significand_f16_2.tail(1).orR
  val fp_a_is_nan_f16_3 = io.fp_aIsFpCanonicalNAN | Ea_f16_3.andR & fp_a_significand_f16_3.tail(1).orR
  val fp_b_is_nan_f64   = io.fp_bIsFpCanonicalNAN | Eb_f64.andR   & fp_b_significand_f64.tail(1).orR
  val fp_b_is_nan_f32_0 = io.fp_bIsFpCanonicalNAN | Eb_f32_0.andR & fp_b_significand_f32_0.tail(1).orR
  val fp_b_is_nan_f32_1 = io.fp_bIsFpCanonicalNAN | Eb_f32_1.andR & fp_b_significand_f32_1.tail(1).orR
  val fp_b_is_nan_f16_0 = io.fp_bIsFpCanonicalNAN | Eb_f16_0.andR & fp_b_significand_f16_0.tail(1).orR
  val fp_b_is_nan_f16_1 = io.fp_bIsFpCanonicalNAN | Eb_f16_1.andR & fp_b_significand_f16_1.tail(1).orR
  val fp_b_is_nan_f16_2 = io.fp_bIsFpCanonicalNAN | Eb_f16_2.andR & fp_b_significand_f16_2.tail(1).orR
  val fp_b_is_nan_f16_3 = io.fp_bIsFpCanonicalNAN | Eb_f16_3.andR & fp_b_significand_f16_3.tail(1).orR
  val fp_c_is_nan_f64   = io.fp_cIsFpCanonicalNAN | Ec_f64.andR & fp_c_significand_f64.tail(1).orR
  val fp_c_is_nan_f32_0 = io.fp_cIsFpCanonicalNAN | Ec_f32_0.andR & fp_c_significand_f32_0.tail(1).orR
  val fp_c_is_nan_f32_1 = io.fp_cIsFpCanonicalNAN | Ec_f32_1.andR & fp_c_significand_f32_1.tail(1).orR
  val fp_c_is_nan_f16_0 = io.fp_cIsFpCanonicalNAN | Ec_f16_0.andR & fp_c_significand_f16_0.tail(1).orR
  val fp_c_is_nan_f16_1 = io.fp_cIsFpCanonicalNAN | Ec_f16_1.andR & fp_c_significand_f16_1.tail(1).orR
  val fp_c_is_nan_f16_2 = io.fp_cIsFpCanonicalNAN | Ec_f16_2.andR & fp_c_significand_f16_2.tail(1).orR
  val fp_c_is_nan_f16_3 = io.fp_cIsFpCanonicalNAN | Ec_f16_3.andR & fp_c_significand_f16_3.tail(1).orR

  val fp_a_is_snan_f64   = !io.fp_aIsFpCanonicalNAN & Ea_f64.andR   & !fp_a_significand_f64.tail(1).head(1) & fp_a_significand_f64.tail(2).orR
  val fp_a_is_snan_f32_0 = !io.fp_aIsFpCanonicalNAN &Ea_f32_0.andR & !fp_a_significand_f32_0.tail(1).head(1) & fp_a_significand_f32_0.tail(2).orR
  val fp_a_is_snan_f32_1 = !io.fp_aIsFpCanonicalNAN &Ea_f32_1.andR & !fp_a_significand_f32_1.tail(1).head(1) & fp_a_significand_f32_1.tail(2).orR
  val fp_a_is_snan_f16_0 = !io.fp_aIsFpCanonicalNAN &Ea_f16_0.andR & !fp_a_significand_f16_0.tail(1).head(1) & fp_a_significand_f16_0.tail(2).orR
  val fp_a_is_snan_f16_1 = !io.fp_aIsFpCanonicalNAN &Ea_f16_1.andR & !fp_a_significand_f16_1.tail(1).head(1) & fp_a_significand_f16_1.tail(2).orR
  val fp_a_is_snan_f16_2 = !io.fp_aIsFpCanonicalNAN &Ea_f16_2.andR & !fp_a_significand_f16_2.tail(1).head(1) & fp_a_significand_f16_2.tail(2).orR
  val fp_a_is_snan_f16_3 = !io.fp_aIsFpCanonicalNAN &Ea_f16_3.andR & !fp_a_significand_f16_3.tail(1).head(1) & fp_a_significand_f16_3.tail(2).orR
  val fp_b_is_snan_f64   = !io.fp_aIsFpCanonicalNAN &Eb_f64.andR   & !fp_b_significand_f64.tail(1).head(1) & fp_b_significand_f64.tail(2).orR
  val fp_b_is_snan_f32_0 = !io.fp_bIsFpCanonicalNAN &Eb_f32_0.andR & !fp_b_significand_f32_0.tail(1).head(1) & fp_b_significand_f32_0.tail(2).orR
  val fp_b_is_snan_f32_1 = !io.fp_bIsFpCanonicalNAN &Eb_f32_1.andR & !fp_b_significand_f32_1.tail(1).head(1) & fp_b_significand_f32_1.tail(2).orR
  val fp_b_is_snan_f16_0 = !io.fp_bIsFpCanonicalNAN &Eb_f16_0.andR & !fp_b_significand_f16_0.tail(1).head(1) & fp_b_significand_f16_0.tail(2).orR
  val fp_b_is_snan_f16_1 = !io.fp_bIsFpCanonicalNAN &Eb_f16_1.andR & !fp_b_significand_f16_1.tail(1).head(1) & fp_b_significand_f16_1.tail(2).orR
  val fp_b_is_snan_f16_2 = !io.fp_bIsFpCanonicalNAN &Eb_f16_2.andR & !fp_b_significand_f16_2.tail(1).head(1) & fp_b_significand_f16_2.tail(2).orR
  val fp_b_is_snan_f16_3 = !io.fp_bIsFpCanonicalNAN &Eb_f16_3.andR & !fp_b_significand_f16_3.tail(1).head(1) & fp_b_significand_f16_3.tail(2).orR
  val fp_c_is_snan_f64   = !io.fp_cIsFpCanonicalNAN &Ec_f64.andR & !fp_c_significand_f64.tail(1).head(1) & fp_c_significand_f64.tail(2).orR
  val fp_c_is_snan_f32_0 = !io.fp_cIsFpCanonicalNAN &Ec_f32_0.andR & !fp_c_significand_f32_0.tail(1).head(1) & fp_c_significand_f32_0.tail(2).orR
  val fp_c_is_snan_f32_1 = !io.fp_cIsFpCanonicalNAN &Ec_f32_1.andR & !fp_c_significand_f32_1.tail(1).head(1) & fp_c_significand_f32_1.tail(2).orR
  val fp_c_is_snan_f16_0 = !io.fp_cIsFpCanonicalNAN &Ec_f16_0.andR & !fp_c_significand_f16_0.tail(1).head(1) & fp_c_significand_f16_0.tail(2).orR
  val fp_c_is_snan_f16_1 = !io.fp_cIsFpCanonicalNAN &Ec_f16_1.andR & !fp_c_significand_f16_1.tail(1).head(1) & fp_c_significand_f16_1.tail(2).orR
  val fp_c_is_snan_f16_2 = !io.fp_cIsFpCanonicalNAN &Ec_f16_2.andR & !fp_c_significand_f16_2.tail(1).head(1) & fp_c_significand_f16_2.tail(2).orR
  val fp_c_is_snan_f16_3 = !io.fp_cIsFpCanonicalNAN &Ec_f16_3.andR & !fp_c_significand_f16_3.tail(1).head(1) & fp_c_significand_f16_3.tail(2).orR
  val has_nan_f64 = fp_a_is_nan_f64 | fp_b_is_nan_f64 | fp_c_is_nan_f64
  val has_nan_f32_0 = fp_a_is_nan_f32_0 | fp_b_is_nan_f32_0 | fp_c_is_nan_f32_0
  val has_nan_f32_1 = fp_a_is_nan_f32_1 | fp_b_is_nan_f32_1 | fp_c_is_nan_f32_1
  val has_nan_f16_0 = fp_a_is_nan_f16_0 | fp_b_is_nan_f16_0 | fp_c_is_nan_f16_0
  val has_nan_f16_1 = fp_a_is_nan_f16_1 | fp_b_is_nan_f16_1 | fp_c_is_nan_f16_1
  val has_nan_f16_2 = fp_a_is_nan_f16_2 | fp_b_is_nan_f16_2 | fp_c_is_nan_f16_2
  val has_nan_f16_3 = fp_a_is_nan_f16_3 | fp_b_is_nan_f16_3 | fp_c_is_nan_f16_3
  val has_snan_f64 = fp_a_is_snan_f64 | fp_b_is_snan_f64 | fp_c_is_snan_f64
  val has_snan_f32_0 = fp_a_is_snan_f32_0 | fp_b_is_snan_f32_0 | fp_c_is_snan_f32_0
  val has_snan_f32_1 = fp_a_is_snan_f32_1 | fp_b_is_snan_f32_1 | fp_c_is_snan_f32_1
  val has_snan_f16_0 = fp_a_is_snan_f16_0 | fp_b_is_snan_f16_0 | fp_c_is_snan_f16_0
  val has_snan_f16_1 = fp_a_is_snan_f16_1 | fp_b_is_snan_f16_1 | fp_c_is_snan_f16_1
  val has_snan_f16_2 = fp_a_is_snan_f16_2 | fp_b_is_snan_f16_2 | fp_c_is_snan_f16_2
  val has_snan_f16_3 = fp_a_is_snan_f16_3 | fp_b_is_snan_f16_3 | fp_c_is_snan_f16_3
  val fp_a_is_inf_f64   = !io.fp_aIsFpCanonicalNAN & Ea_f64.andR   & !fp_a_significand_f64.tail(1).orR
  val fp_a_is_inf_f32_0 = !io.fp_aIsFpCanonicalNAN & Ea_f32_0.andR & !fp_a_significand_f32_0.tail(1).orR
  val fp_a_is_inf_f32_1 = !io.fp_aIsFpCanonicalNAN & Ea_f32_1.andR & !fp_a_significand_f32_1.tail(1).orR
  val fp_a_is_inf_f16_0 = !io.fp_aIsFpCanonicalNAN & Ea_f16_0.andR & !fp_a_significand_f16_0.tail(1).orR
  val fp_a_is_inf_f16_1 = !io.fp_aIsFpCanonicalNAN & Ea_f16_1.andR & !fp_a_significand_f16_1.tail(1).orR
  val fp_a_is_inf_f16_2 = !io.fp_aIsFpCanonicalNAN & Ea_f16_2.andR & !fp_a_significand_f16_2.tail(1).orR
  val fp_a_is_inf_f16_3 = !io.fp_aIsFpCanonicalNAN & Ea_f16_3.andR & !fp_a_significand_f16_3.tail(1).orR
  val fp_b_is_inf_f64   = !io.fp_aIsFpCanonicalNAN & Eb_f64.andR   & !fp_b_significand_f64.tail(1).orR
  val fp_b_is_inf_f32_0 = !io.fp_bIsFpCanonicalNAN & Eb_f32_0.andR & !fp_b_significand_f32_0.tail(1).orR
  val fp_b_is_inf_f32_1 = !io.fp_bIsFpCanonicalNAN & Eb_f32_1.andR & !fp_b_significand_f32_1.tail(1).orR
  val fp_b_is_inf_f16_0 = !io.fp_bIsFpCanonicalNAN & Eb_f16_0.andR & !fp_b_significand_f16_0.tail(1).orR
  val fp_b_is_inf_f16_1 = !io.fp_bIsFpCanonicalNAN & Eb_f16_1.andR & !fp_b_significand_f16_1.tail(1).orR
  val fp_b_is_inf_f16_2 = !io.fp_bIsFpCanonicalNAN & Eb_f16_2.andR & !fp_b_significand_f16_2.tail(1).orR
  val fp_b_is_inf_f16_3 = !io.fp_bIsFpCanonicalNAN & Eb_f16_3.andR & !fp_b_significand_f16_3.tail(1).orR
  val fp_c_is_inf_f64   = !io.fp_cIsFpCanonicalNAN & Ec_f64.andR & !fp_c_significand_f64.tail(1).orR
  val fp_c_is_inf_f32_0 = !io.fp_cIsFpCanonicalNAN & Ec_f32_0.andR & !fp_c_significand_f32_0.tail(1).orR
  val fp_c_is_inf_f32_1 = !io.fp_cIsFpCanonicalNAN & Ec_f32_1.andR & !fp_c_significand_f32_1.tail(1).orR
  val fp_c_is_inf_f16_0 = !io.fp_cIsFpCanonicalNAN & Ec_f16_0.andR & !fp_c_significand_f16_0.tail(1).orR
  val fp_c_is_inf_f16_1 = !io.fp_cIsFpCanonicalNAN & Ec_f16_1.andR & !fp_c_significand_f16_1.tail(1).orR
  val fp_c_is_inf_f16_2 = !io.fp_cIsFpCanonicalNAN & Ec_f16_2.andR & !fp_c_significand_f16_2.tail(1).orR
  val fp_c_is_inf_f16_3 = !io.fp_cIsFpCanonicalNAN & Ec_f16_3.andR & !fp_c_significand_f16_3.tail(1).orR
  val has_inf_f64 = fp_a_is_inf_f64 | fp_b_is_inf_f64 | fp_c_is_inf_f64
  val has_inf_f32_0 = fp_a_is_inf_f32_0 | fp_b_is_inf_f32_0 | fp_c_is_inf_f32_0
  val has_inf_f32_1 = fp_a_is_inf_f32_1 | fp_b_is_inf_f32_1 | fp_c_is_inf_f32_1
  val has_inf_f16_0 = fp_a_is_inf_f16_0 | fp_b_is_inf_f16_0 | fp_c_is_inf_f16_0
  val has_inf_f16_1 = fp_a_is_inf_f16_1 | fp_b_is_inf_f16_1 | fp_c_is_inf_f16_1
  val has_inf_f16_2 = fp_a_is_inf_f16_2 | fp_b_is_inf_f16_2 | fp_c_is_inf_f16_2
  val has_inf_f16_3 = fp_a_is_inf_f16_3 | fp_b_is_inf_f16_3 | fp_c_is_inf_f16_3
  val result_inf_f64 = Cat(Fill(exponentWidth,1.U),Fill(significandWidth-1,0.U))
  val result_inf_f32_0 = Cat(Fill(8,1.U),Fill(24-1,0.U))
  val result_inf_f32_1 = Cat(Fill(8,1.U),Fill(24-1,0.U))
  val result_inf_f16_0 = Cat(Fill(5,1.U),Fill(11-1,0.U))
  val result_inf_f16_1 = Cat(Fill(5,1.U),Fill(11-1,0.U))
  val result_inf_f16_2 = Cat(Fill(5,1.U),Fill(11-1,0.U))
  val result_inf_f16_3 = Cat(Fill(5,1.U),Fill(11-1,0.U))
  val result_nan_f64 = Cat(0.U,Fill(exponentWidth+1,1.U),0.U((significandWidth-2).W))
  val result_nan_f32_0 = Cat(0.U,Fill(8+1,1.U),0.U((24-2).W))
  val result_nan_f32_1 = Cat(0.U,Fill(8+1,1.U),0.U((24-2).W))
  val result_nan_f16_0 = Cat(0.U,Fill(5+1,1.U),0.U((11-2).W))
  val result_nan_f16_1 = Cat(0.U,Fill(5+1,1.U),0.U((11-2).W))
  val result_nan_f16_2 = Cat(0.U,Fill(5+1,1.U),0.U((11-2).W))
  val result_nan_f16_3 = Cat(0.U,Fill(5+1,1.U),0.U((11-2).W))
  val fp_result_f64 = Wire(UInt(64.W))
  val fp_result_f32_0 = Wire(UInt(32.W))
  val fp_result_f32_1 = Wire(UInt(32.W))
  val fp_result_f16_0 = Wire(UInt(16.W))
  val fp_result_f16_1 = Wire(UInt(16.W))
  val fp_result_f16_2 = Wire(UInt(16.W))
  val fp_result_f16_3 = Wire(UInt(16.W))

  val has_nan_f64_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f64, fire), fire_reg0), fire_reg1)
  val has_nan_f64_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f64.asBool | (fp_a_is_inf_f64 & fp_b_is_zero_f64) | (fp_a_is_zero_f64 & fp_b_is_inf_f64),
    fire), fire_reg0), fire_reg1)
  val has_inf_f64_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f64, fire), fire_reg0), fire_reg1)
  val has_inf_f64_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f64 & fp_b_is_zero_f64) | (fp_a_is_zero_f64 & fp_b_is_inf_f64)) | (fp_c_is_inf_f64 & (fp_a_is_inf_f64 | fp_b_is_inf_f64) & (sign_c_f64 ^ sign_a_b_f64)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f64_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f64|fp_b_is_inf_f64,sign_a_b_f64,sign_c_f64),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f64_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f64_reg2.asBool) | (RUP_reg2 & sign_result_temp_f64_reg2.asBool)
  val fp_a_or_b_is_zero_f64_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f64 | fp_b_is_zero_f64, fire), fire_reg0), fire_reg1)
  val fp_result_f64_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f64,
        Mux(is_vfmul, sign_a_b_f64, (sign_a_b_f64 & sign_c_f64) | (RDN & (sign_a_b_f64 ^ sign_c_f64)) ),
        fp_c_f64.head(1)
      ),
      fp_c_f64.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f64_reg2){
    fp_result_f64 := result_nan_f64
    fflags_f64 := Mux(has_nan_f64_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f64_reg2){
    fp_result_f64 := Mux(has_inf_f64_is_NV_reg2, result_nan_f64, Cat(has_inf_f64_result_inf_sign_reg2,result_inf_f64))
    fflags_f64 := Mux(has_inf_f64_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f64_reg2){
    fp_result_f64 := Mux(is_overflow_f64_down_reg2,result_overflow_down_f64,result_overflow_up_f64)
    fflags_f64 := "b00101".U
  }.elsewhen(has_zero_f64_reg2){
    fp_result_f64 := Mux(fp_a_or_b_is_zero_f64_reg2,
      fp_result_f64_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f64_reg2, Cat(RDN_reg2,0.U((64-1).W)), normal_result_f64)
    )
    fflags_f64 := Mux(fp_a_or_b_is_zero_f64_reg2 | normal_result_is_zero_f64_reg2, 0.U, Cat(NV_f64,DZ_f64,OF_f64,UF_f64,NX_f64))
  }.otherwise{
    fp_result_f64 := normal_result_f64
  }



  val has_nan_f32_0_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f32_0, fire), fire_reg0), fire_reg1)
  val has_nan_f32_0_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f32_0.asBool | (fp_a_is_inf_f32_0 & fp_b_is_zero_f32_0) | (fp_a_is_zero_f32_0 & fp_b_is_inf_f32_0),
    fire), fire_reg0), fire_reg1)
  val has_inf_f32_0_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f32_0, fire), fire_reg0), fire_reg1)
  val has_inf_f32_0_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f32_0 & fp_b_is_zero_f32_0) | (fp_a_is_zero_f32_0 & fp_b_is_inf_f32_0)) | (fp_c_is_inf_f32_0 & (fp_a_is_inf_f32_0 | fp_b_is_inf_f32_0) & (sign_c_f32_0 ^ sign_a_b_f32_0)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f32_0_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f32_0|fp_b_is_inf_f32_0,sign_a_b_f32_0,sign_c_f32_0),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f32_0_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f32_0_reg2.asBool) | (RUP_reg2 & sign_result_temp_f32_0_reg2.asBool)
  val fp_a_or_b_is_zero_f32_0_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f32_0 | fp_b_is_zero_f32_0, fire), fire_reg0), fire_reg1)
  val fp_result_f32_0_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f32_0,
        Mux(is_vfmul, sign_a_b_f32_0, (sign_a_b_f32_0 & sign_c_f32_0) | (RDN & (sign_a_b_f32_0 ^ sign_c_f32_0)) ),
        fp_c_f32_0.head(1)
      ),
      fp_c_f32_0.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f32_0_reg2){
    fp_result_f32_0 := result_nan_f32_0
    fflags_f32_0 := Mux(has_nan_f32_0_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f32_0_reg2){
    fp_result_f32_0 := Mux(has_inf_f32_0_is_NV_reg2, result_nan_f32_0, Cat(has_inf_f32_0_result_inf_sign_reg2,result_inf_f32_0))
    fflags_f32_0 := Mux(has_inf_f32_0_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f32_0_reg2){
    fp_result_f32_0 := Mux(is_overflow_f32_0_down_reg2,result_overflow_down_f32_0,result_overflow_up_f32_0)
    fflags_f32_0 := "b00101".U
  }.elsewhen(has_zero_f32_0_reg2){
    fp_result_f32_0 := Mux(fp_a_or_b_is_zero_f32_0_reg2,
      fp_result_f32_0_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f32_0_reg2, Cat(RDN_reg2,0.U((32-1).W)), normal_result_f32_0)
    )
    fflags_f32_0 := Mux(fp_a_or_b_is_zero_f32_0_reg2 | normal_result_is_zero_f32_0_reg2, 0.U, Cat(NV_f32_0,DZ_f32_0,OF_f32_0,UF_f32_0,NX_f32_0))
  }.otherwise{
    fp_result_f32_0 := normal_result_f32_0
  }
  val has_nan_f32_1_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f32_1, fire), fire_reg0), fire_reg1)
  val has_nan_f32_1_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f32_1.asBool | (fp_a_is_inf_f32_1 & fp_b_is_zero_f32_1) | (fp_a_is_zero_f32_1 & fp_b_is_inf_f32_1),
    fire), fire_reg0), fire_reg1)
  val has_inf_f32_1_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f32_1, fire), fire_reg0), fire_reg1)
  val has_inf_f32_1_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f32_1 & fp_b_is_zero_f32_1) | (fp_a_is_zero_f32_1 & fp_b_is_inf_f32_1)) | (fp_c_is_inf_f32_1 & (fp_a_is_inf_f32_1 | fp_b_is_inf_f32_1) & (sign_c_f32_1 ^ sign_a_b_f32_1)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f32_1_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f32_1|fp_b_is_inf_f32_1,sign_a_b_f32_1,sign_c_f32_1),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f32_1_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f32_1_reg2.asBool) | (RUP_reg2 & sign_result_temp_f32_1_reg2.asBool)
  val fp_a_or_b_is_zero_f32_1_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f32_1 | fp_b_is_zero_f32_1, fire), fire_reg0), fire_reg1)
  val fp_result_f32_1_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f32_1,
        Mux(is_vfmul, sign_a_b_f32_1, (sign_a_b_f32_1 & sign_c_f32_1) | (RDN & (sign_a_b_f32_1 ^ sign_c_f32_1)) ),
        fp_c_f32_1.head(1)
      ),
      fp_c_f32_1.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f32_1_reg2){
    fp_result_f32_1 := result_nan_f32_1
    fflags_f32_1 := Mux(has_nan_f32_1_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f32_1_reg2){
    fp_result_f32_1 := Mux(has_inf_f32_1_is_NV_reg2, result_nan_f32_1, Cat(has_inf_f32_1_result_inf_sign_reg2,result_inf_f32_1))
    fflags_f32_1 := Mux(has_inf_f32_1_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f32_1_reg2){
    fp_result_f32_1 := Mux(is_overflow_f32_1_down_reg2,result_overflow_down_f32_1,result_overflow_up_f32_1)
    fflags_f32_1 := "b00101".U
  }.elsewhen(has_zero_f32_1_reg2){
    fp_result_f32_1 := Mux(fp_a_or_b_is_zero_f32_1_reg2,
      fp_result_f32_1_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f32_1_reg2, Cat(RDN_reg2,0.U((32-1).W)), normal_result_f32_1)
    )
    fflags_f32_1 := Mux(fp_a_or_b_is_zero_f32_1_reg2 | normal_result_is_zero_f32_1_reg2, 0.U, Cat(NV_f32_1,DZ_f32_1,OF_f32_1,UF_f32_1,NX_f32_1))
  }.otherwise{
    fp_result_f32_1 := normal_result_f32_1
  }
  val has_nan_f16_0_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f16_0, fire), fire_reg0), fire_reg1)
  val has_nan_f16_0_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f16_0.asBool | (fp_a_is_inf_f16_0 & fp_b_is_zero_f16_0) | (fp_a_is_zero_f16_0 & fp_b_is_inf_f16_0),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_0_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f16_0, fire), fire_reg0), fire_reg1)
  val has_inf_f16_0_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f16_0 & fp_b_is_zero_f16_0) | (fp_a_is_zero_f16_0 & fp_b_is_inf_f16_0)) | (fp_c_is_inf_f16_0 & (fp_a_is_inf_f16_0 | fp_b_is_inf_f16_0) & (sign_c_f16_0 ^ sign_a_b_f16_0)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_0_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f16_0|fp_b_is_inf_f16_0,sign_a_b_f16_0,sign_c_f16_0),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f16_0_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f16_0_reg2.asBool) | (RUP_reg2 & sign_result_temp_f16_0_reg2.asBool)
  val fp_a_or_b_is_zero_f16_0_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_0 | fp_b_is_zero_f16_0, fire), fire_reg0), fire_reg1)
  val fp_result_f16_0_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f16_0,
        Mux(is_vfmul, sign_a_b_f16_0, (sign_a_b_f16_0 & sign_c_f16_0) | (RDN & (sign_a_b_f16_0 ^ sign_c_f16_0)) ),
        fp_c_f16_0.head(1)
      ),
      fp_c_f16_0.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f16_0_reg2){
    fp_result_f16_0 := result_nan_f16_0
    fflags_f16_0 := Mux(has_nan_f16_0_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f16_0_reg2){
    fp_result_f16_0 := Mux(has_inf_f16_0_is_NV_reg2, result_nan_f16_0, Cat(has_inf_f16_0_result_inf_sign_reg2,result_inf_f16_0))
    fflags_f16_0 := Mux(has_inf_f16_0_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f16_0_reg2){
    fp_result_f16_0 := Mux(is_overflow_f16_0_down_reg2,result_overflow_down_f16_0,result_overflow_up_f16_0)
    fflags_f16_0 := "b00101".U
  }.elsewhen(has_zero_f16_0_reg2){
    fp_result_f16_0 := Mux(fp_a_or_b_is_zero_f16_0_reg2,
      fp_result_f16_0_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f16_0_reg2, Cat(RDN_reg2,0.U((16-1).W)), normal_result_f16_0)
    )
    fflags_f16_0 := Mux(fp_a_or_b_is_zero_f16_0_reg2 | normal_result_is_zero_f16_0_reg2, 0.U, Cat(NV_f16_0,DZ_f16_0,OF_f16_0,UF_f16_0,NX_f16_0))
  }.otherwise{
    fp_result_f16_0 := normal_result_f16_0
  }
  val has_nan_f16_1_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f16_1, fire), fire_reg0), fire_reg1)
  val has_nan_f16_1_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f16_1.asBool | (fp_a_is_inf_f16_1 & fp_b_is_zero_f16_1) | (fp_a_is_zero_f16_1 & fp_b_is_inf_f16_1),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_1_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f16_1, fire), fire_reg0), fire_reg1)
  val has_inf_f16_1_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f16_1 & fp_b_is_zero_f16_1) | (fp_a_is_zero_f16_1 & fp_b_is_inf_f16_1)) | (fp_c_is_inf_f16_1 & (fp_a_is_inf_f16_1 | fp_b_is_inf_f16_1) & (sign_c_f16_1 ^ sign_a_b_f16_1)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_1_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f16_1|fp_b_is_inf_f16_1,sign_a_b_f16_1,sign_c_f16_1),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f16_1_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f16_1_reg2.asBool) | (RUP_reg2 & sign_result_temp_f16_1_reg2.asBool)
  val fp_a_or_b_is_zero_f16_1_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_1 | fp_b_is_zero_f16_1, fire), fire_reg0), fire_reg1)
  val fp_result_f16_1_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f16_1,
        Mux(is_vfmul, sign_a_b_f16_1, (sign_a_b_f16_1 & sign_c_f16_1) | (RDN & (sign_a_b_f16_1 ^ sign_c_f16_1)) ),
        fp_c_f16_1.head(1)
      ),
      fp_c_f16_1.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f16_1_reg2){
    fp_result_f16_1 := result_nan_f16_1
    fflags_f16_1 := Mux(has_nan_f16_1_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f16_1_reg2){
    fp_result_f16_1 := Mux(has_inf_f16_1_is_NV_reg2, result_nan_f16_1, Cat(has_inf_f16_1_result_inf_sign_reg2,result_inf_f16_1))
    fflags_f16_1 := Mux(has_inf_f16_1_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f16_1_reg2){
    fp_result_f16_1 := Mux(is_overflow_f16_1_down_reg2,result_overflow_down_f16_1,result_overflow_up_f16_1)
    fflags_f16_1 := "b00101".U
  }.elsewhen(has_zero_f16_1_reg2){
    fp_result_f16_1 := Mux(fp_a_or_b_is_zero_f16_1_reg2,
      fp_result_f16_1_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f16_1_reg2, Cat(RDN_reg2,0.U((16-1).W)), normal_result_f16_1)
    )
    fflags_f16_1 := Mux(fp_a_or_b_is_zero_f16_1_reg2 | normal_result_is_zero_f16_1_reg2, 0.U, Cat(NV_f16_1,DZ_f16_1,OF_f16_1,UF_f16_1,NX_f16_1))
  }.otherwise{
    fp_result_f16_1 := normal_result_f16_1
  }
  val has_nan_f16_2_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f16_2, fire), fire_reg0), fire_reg1)
  val has_nan_f16_2_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f16_2.asBool | (fp_a_is_inf_f16_2 & fp_b_is_zero_f16_2) | (fp_a_is_zero_f16_2 & fp_b_is_inf_f16_2),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_2_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f16_2, fire), fire_reg0), fire_reg1)
  val has_inf_f16_2_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f16_2 & fp_b_is_zero_f16_2) | (fp_a_is_zero_f16_2 & fp_b_is_inf_f16_2)) | (fp_c_is_inf_f16_2 & (fp_a_is_inf_f16_2 | fp_b_is_inf_f16_2) & (sign_c_f16_2 ^ sign_a_b_f16_2)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_2_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f16_2|fp_b_is_inf_f16_2,sign_a_b_f16_2,sign_c_f16_2),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f16_2_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f16_2_reg2.asBool) | (RUP_reg2 & sign_result_temp_f16_2_reg2.asBool)
  val fp_a_or_b_is_zero_f16_2_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_2 | fp_b_is_zero_f16_2, fire), fire_reg0), fire_reg1)
  val fp_result_f16_2_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f16_2,
        Mux(is_vfmul, sign_a_b_f16_2, (sign_a_b_f16_2 & sign_c_f16_2) | (RDN & (sign_a_b_f16_2 ^ sign_c_f16_2)) ),
        fp_c_f16_2.head(1)
      ),
      fp_c_f16_2.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f16_2_reg2){
    fp_result_f16_2 := result_nan_f16_2
    fflags_f16_2 := Mux(has_nan_f16_2_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f16_2_reg2){
    fp_result_f16_2 := Mux(has_inf_f16_2_is_NV_reg2, result_nan_f16_2, Cat(has_inf_f16_2_result_inf_sign_reg2,result_inf_f16_2))
    fflags_f16_2 := Mux(has_inf_f16_2_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f16_2_reg2){
    fp_result_f16_2 := Mux(is_overflow_f16_2_down_reg2,result_overflow_down_f16_2,result_overflow_up_f16_2)
    fflags_f16_2 := "b00101".U
  }.elsewhen(has_zero_f16_2_reg2){
    fp_result_f16_2 := Mux(fp_a_or_b_is_zero_f16_2_reg2,
      fp_result_f16_2_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f16_2_reg2, Cat(RDN_reg2,0.U((16-1).W)), normal_result_f16_2)
    )
    fflags_f16_2 := Mux(fp_a_or_b_is_zero_f16_2_reg2 | normal_result_is_zero_f16_2_reg2, 0.U, Cat(NV_f16_2,DZ_f16_2,OF_f16_2,UF_f16_2,NX_f16_2))
  }.otherwise{
    fp_result_f16_2 := normal_result_f16_2
  }
  val has_nan_f16_3_reg2       = RegEnable(RegEnable(RegEnable(has_nan_f16_3, fire), fire_reg0), fire_reg1)
  val has_nan_f16_3_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    has_snan_f16_3.asBool | (fp_a_is_inf_f16_3 & fp_b_is_zero_f16_3) | (fp_a_is_zero_f16_3 & fp_b_is_inf_f16_3),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_3_reg2       = RegEnable(RegEnable(RegEnable(has_inf_f16_3, fire), fire_reg0), fire_reg1)
  val has_inf_f16_3_is_NV_reg2 = RegEnable(RegEnable(RegEnable(
    ((fp_a_is_inf_f16_3 & fp_b_is_zero_f16_3) | (fp_a_is_zero_f16_3 & fp_b_is_inf_f16_3)) | (fp_c_is_inf_f16_3 & (fp_a_is_inf_f16_3 | fp_b_is_inf_f16_3) & (sign_c_f16_3 ^ sign_a_b_f16_3)),
    fire), fire_reg0), fire_reg1)
  val has_inf_f16_3_result_inf_sign_reg2 = RegEnable(RegEnable(RegEnable(
    Mux(fp_a_is_inf_f16_3|fp_b_is_inf_f16_3,sign_a_b_f16_3,sign_c_f16_3),
    fire), fire_reg0), fire_reg1)
  val is_overflow_f16_3_down_reg2 = RTZ_reg2 | (RDN_reg2 & !sign_result_temp_f16_3_reg2.asBool) | (RUP_reg2 & sign_result_temp_f16_3_reg2.asBool)
  val fp_a_or_b_is_zero_f16_3_reg2 = RegEnable(RegEnable(RegEnable(fp_a_is_zero_f16_3 | fp_b_is_zero_f16_3, fire), fire_reg0), fire_reg1)
  val fp_result_f16_3_fp_a_or_b_is_zero_reg2 = RegEnable(RegEnable(RegEnable(
    Cat(
      Mux(
        fp_c_is_zero_f16_3,
        Mux(is_vfmul, sign_a_b_f16_3, (sign_a_b_f16_3 & sign_c_f16_3) | (RDN & (sign_a_b_f16_3 ^ sign_c_f16_3)) ),
        fp_c_f16_3.head(1)
      ),
      fp_c_f16_3.tail(1)
    ),
    fire), fire_reg0), fire_reg1
  )
  when(has_nan_f16_3_reg2){
    fp_result_f16_3 := result_nan_f16_3
    fflags_f16_3 := Mux(has_nan_f16_3_is_NV_reg2,"b10000".U,"b00000".U)
  }.elsewhen(has_inf_f16_3_reg2){
    fp_result_f16_3 := Mux(has_inf_f16_3_is_NV_reg2, result_nan_f16_3, Cat(has_inf_f16_3_result_inf_sign_reg2,result_inf_f16_3))
    fflags_f16_3 := Mux(has_inf_f16_3_is_NV_reg2, "b10000".U, "b00000".U)
  }.elsewhen(is_overflow_f16_3_reg2){
    fp_result_f16_3 := Mux(is_overflow_f16_3_down_reg2,result_overflow_down_f16_3,result_overflow_up_f16_3)
    fflags_f16_3 := "b00101".U
  }.elsewhen(has_zero_f16_3_reg2){
    fp_result_f16_3 := Mux(fp_a_or_b_is_zero_f16_3_reg2,
      fp_result_f16_3_fp_a_or_b_is_zero_reg2,
      Mux(normal_result_is_zero_f16_3_reg2, Cat(RDN_reg2,0.U((16-1).W)), normal_result_f16_3)
    )
    fflags_f16_3 := Mux(fp_a_or_b_is_zero_f16_3_reg2 | normal_result_is_zero_f16_3_reg2, 0.U, Cat(NV_f16_3,DZ_f16_3,OF_f16_3,UF_f16_3,NX_f16_3))
  }.otherwise{
    fp_result_f16_3 := normal_result_f16_3
  }
  io.fp_result := Mux(
    is_fp64_reg2,
    fp_result_f64,
    Mux(
      is_fp32_reg2,
      Cat(Fill(32, 1.U),fp_result_f32_0),
      Cat(Fill(48, 1.U),fp_result_f16_0)
    )
  )
  val is_fp16_reg2 = !is_fp32_reg2 & !is_fp64_reg2
  io.fflags := Mux(
    is_fp64_reg2,
    fflags_f64,
    Mux(is_fp32_reg2,fflags_f32_0,fflags_f16_0)
  )
}
private[fpulite] class BoothEncoderF64F32F16(
                                             width :Int = 53,
                                             is_addend_expand_1bit : Boolean = true
                                           ) extends Module{

  val addend_seq_width = if (is_addend_expand_1bit) 2*width+1 else 2*width
  val outNum  = width/2 + 1
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val is_fp64= Input(Bool())
    val is_fp32= Input(Bool())

    val out_pp = Output(Vec(outNum,UInt(addend_seq_width.W)))
    val res_mul = Output(UInt((2*width).W))
  })
  val in_b_cat = Mux(
    io.is_fp64,
    Cat(Fill(2- (width % 2),0.U),io.in_b,0.U),
    Mux(
      io.is_fp32,
      Cat(0.U(2.W),io.in_b.head(24),0.U(4.W),io.in_b.tail(29),0.U),
      Cat(0.U,io.in_b(52,42),0.U(3.W),io.in_b(39,29),0.U(3.W),io.in_b(23,13),0.U(3.W),io.in_b(10,0),0.U)
    )
  )
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
  val pp_seq_f64 = Wire(Vec(outNum,UInt((width+1).W)))
  val pp_seq_f32 = Wire(Vec(outNum,UInt((24+1).W)))
  val pp_seq_f16 = Wire(Vec(outNum,UInt((11+1).W)))
  val sign_seq = Wire(Vec(outNum,UInt(1.W)))
  for (i <- 0 until outNum) {
    sign_seq(i) := booth_4bit_onehot(i)(1) | booth_4bit_onehot(i)(0)
    pp_seq_f64(i) := Fill(width+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a)  |
      Fill(width+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a,0.U)  |
      Fill(width+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a) |
      Fill(width+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a,1.U)
    if (i<=13) {
      pp_seq_f32(i) :=
        Fill(24+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(23,0))  |
          Fill(24+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(23,0),0.U)  |
          Fill(24+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(23,0)) |
          Fill(24+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(23,0),1.U)
    }
    else {
      pp_seq_f32(i) :=
        Fill(24+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a.head(24))  |
          Fill(24+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a.head(24),0.U)  |
          Fill(24+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a.head(24)) |
          Fill(24+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a.head(24),1.U)
    }
    if (i<=6) {
      pp_seq_f16(i) :=
        Fill(11+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(10,0))  |
          Fill(11+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(10,0),0.U)  |
          Fill(11+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(10,0)) |
          Fill(11+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(10,0),1.U)
    }
    else if (i<=13) {
      pp_seq_f16(i) :=
        Fill(11+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(23,13))  |
          Fill(11+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(23,13),0.U)  |
          Fill(11+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(23,13)) |
          Fill(11+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(23,13),1.U)
    }
    else if (i<=20) {
      pp_seq_f16(i) :=
        Fill(11+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(39,29))  |
          Fill(11+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(39,29),0.U)  |
          Fill(11+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(39,29)) |
          Fill(11+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(39,29),1.U)
    }
    else {
      pp_seq_f16(i) :=
        Fill(11+1,booth_4bit_onehot(i)(3)) & Cat(0.U,io.in_a(52,42))  |
          Fill(11+1,booth_4bit_onehot(i)(2)) & Cat(io.in_a(52,42),0.U)  |
          Fill(11+1,booth_4bit_onehot(i)(1)) & Cat(1.U,~io.in_a(52,42)) |
          Fill(11+1,booth_4bit_onehot(i)(0)) & Cat(~io.in_a(52,42),1.U)
    }
  }
  val addend_seq_f64 = Wire(Vec(outNum,UInt(addend_seq_width.W)))
  val addend_seq_f32_to_f64 = Wire(Vec(outNum,UInt(addend_seq_width.W)))
  val addend_seq_f16_to_f64 = Wire(Vec(outNum,UInt(addend_seq_width.W)))
  val addend_seq_f32 = Wire(Vec(outNum,UInt(49.W)))
  val addend_seq_f16 = Wire(Vec(outNum,UInt(23.W)))

  for (i <- 0 until 6) {
    val head_first_one_width = 11-4-2*(i-1)
    val tail_zero_width = 2*(i-1)
    i match {
      case 0 => addend_seq_f16(i) := Cat( 0.U((11-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f16(i) )
      case 1 => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i-1) )
      case 4 => addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 5 => if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, pp_seq_f16(i), 0.U,sign_seq(i-1), 0.U(tail_zero_width.W))
      else addend_seq_f16(i) := Cat(pp_seq_f16(i),0.U,sign_seq(i-1), 0.U((2*(i-1)).W))
      case _ => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f16_to_f64(i) := Cat(0.U(84.W),addend_seq_f16(i))
  }
  addend_seq_f16(6) := 0.U
  addend_seq_f16_to_f64(6) := Cat(0.U(84.W),addend_seq_f16(6))
  for (i <- 7 until 13) {
    val head_first_one_width = 11-4-2*(i-1-7)
    val tail_zero_width = 2*(i-1-7)
    i match {
      case 7 => addend_seq_f16(i) := Cat( 0.U((11-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f16(i) )
      case 8 => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i-1) )
      case 11 => addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 12 => if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, pp_seq_f16(i), 0.U,sign_seq(i-1), 0.U(tail_zero_width.W))
      else addend_seq_f16(i) := Cat(pp_seq_f16(i),0.U,sign_seq(i-1), 0.U((2*(i-1)).W))
      case _ => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f16_to_f64(i) := Cat(0.U(58.W),addend_seq_f16(i),0.U(26.W))
  }
  addend_seq_f16(13) := 0.U
  addend_seq_f16_to_f64(13) := Cat(0.U(58.W),addend_seq_f16(13),0.U(26.W))
  for (i <- 14 until 20) {
    val head_first_one_width = 11-4-2*(i-1-14)
    val tail_zero_width = 2*(i-1-14)
    i match {
      case 14 => addend_seq_f16(i) := Cat( 0.U((11-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f16(i) )
      case 15 => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i-1) )
      case 18 => addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 19 => if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, pp_seq_f16(i), 0.U,sign_seq(i-1), 0.U(tail_zero_width.W))
      else addend_seq_f16(i) := Cat(pp_seq_f16(i),0.U,sign_seq(i-1), 0.U((2*(i-1)).W))
      case _ => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f16_to_f64(i) := Cat(0.U(26.W),addend_seq_f16(i),0.U(58.W))
  }
  addend_seq_f16(20) := 0.U
  addend_seq_f16_to_f64(20) := Cat(0.U(26.W),addend_seq_f16(20),0.U(58.W))
  for (i <- 21 until 27) {
    val head_first_one_width = 11-4-2*(i-1-21)
    val tail_zero_width = 2*(i-1-21)
    i match {
      case 21 => addend_seq_f16(i) := Cat( 0.U((11-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f16(i) )
      case 22 => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i-1) )
      case 25 => addend_seq_f16(i) := Cat(1.U, ~sign_seq(i), pp_seq_f16(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 26 => if (is_addend_expand_1bit) addend_seq_f16(i) := Cat(1.U, pp_seq_f16(i), 0.U,sign_seq(i-1), 0.U(tail_zero_width.W))
      else addend_seq_f16(i) := Cat(pp_seq_f16(i),0.U,sign_seq(i-1), 0.U((2*(i-1)).W))
      case _ => addend_seq_f16(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f16(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f16_to_f64(i) := Cat(addend_seq_f16(i),0.U(84.W))
  }


  for (i <- 0 until 13) {
    val head_first_one_width = 24-4-2*(i-1)
    val tail_zero_width = 2*(i-1)
    i match {
      case 0 => addend_seq_f32(i) := Cat( 0.U((24-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f32(i) )
      case 1 => addend_seq_f32(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i-1) )
      case 11 => addend_seq_f32(i) := Cat(1.U, ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 12 => addend_seq_f32(i) := Cat(pp_seq_f32(i).tail(1), 0.U, sign_seq(i-1), 0.U(tail_zero_width.W))
      case _ => addend_seq_f32(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f32_to_f64(i) := Cat(0.U(58.W),addend_seq_f32(i))
  }
  addend_seq_f32(13) := 0.U
  addend_seq_f32_to_f64(13) := Cat(0.U(58.W),addend_seq_f32(13))
  for (i <- 14 until 27) {
    val head_first_one_width = 24-4-2*(i-1-14)
    val tail_zero_width = 2*(i-1-14)
    i match {
      case 14 => addend_seq_f32(i) := Cat( 0.U((24-4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f32(i) )
      case 15 => addend_seq_f32(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i-1) )
      case 25 => addend_seq_f32(i) := Cat(1.U, ~sign_seq(i), pp_seq_f32(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case 26 => addend_seq_f32(i) := Cat(pp_seq_f32(i).tail(1), 0.U, sign_seq(i-1), 0.U(tail_zero_width.W))
      case _ => addend_seq_f32(i) := Cat( 1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f32(i),0.U, sign_seq(i-1), 0.U(tail_zero_width.W) )
    }
    addend_seq_f32_to_f64(i) := Cat(addend_seq_f32(i),0.U(58.W))
  }
  val outNumBeforeLast = outNum-2
  val outNumLast = outNum-1
  for (i <- 0 until outNum) {
    val head_first_one_width = width - 4 - 2 * (i - 1)
    val tail_zero_width = 2 * (i - 1)
    i match {
      case 0 => addend_seq_f64(i) := Cat(0.U((width - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq_f64(0))
      case 1 => addend_seq_f64(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1))
      case `outNumBeforeLast` =>
        if (width % 2 == 0) {
          if (is_addend_expand_1bit) addend_seq_f64(i) := Cat(1.U, ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
          else addend_seq_f64(i) := Cat(~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        }
        else addend_seq_f64(i) := Cat(1.U, ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case `outNumLast` =>
        if (width % 2 == 0) addend_seq_f64(i) := Cat(pp_seq_f64(i).tail(1), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
        else if (is_addend_expand_1bit) addend_seq_f64(i) := Cat(1.U, pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        else addend_seq_f64(i) := Cat(pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addend_seq_f64(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq_f64(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
    }
  }
  io.out_pp := Mux(
    io.is_fp64,
    addend_seq_f64,
    Mux(io.is_fp32,addend_seq_f32_to_f64,addend_seq_f16_to_f64)
  )
  io.res_mul := Mux(
    io.is_fp64,
    addend_seq_f64.reduce(_+_),
    Mux(io.is_fp32,
      addend_seq_f32_to_f64.reduce(_+_) & "h7fffffffffffc01ffffffffffff".U,
      addend_seq_f16_to_f64.reduce(_+_) & (("h1fffffc7fffff".U << 58).asUInt + "h1fffffc7fffff".U),
    )
  )

}
private[fpulite] class CSA_Nto2With3to2MainPipeline(n :Int, width :Int, pipeLevel :Int) extends Module{
  val io = IO(new Bundle() {
    val fire    = Input(Bool())
    val in      = Input(Vec(n,UInt(width.W)))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))

  })
  val fire = io.fire
  val in = ListBuffer[UInt]()
  io.in.foreach(a => in += a)
  val in_next = ListBuffer[UInt]()
  var n_next = n
  var CSA4to2Num = if(n_next==8 || n_next==4) n_next/4 else 0
  var CSA3to2Num = if(n_next==8 || n_next==4) 0 else n_next/3
  var remainder = n_next - CSA4to2Num*4 - CSA3to2Num*3

  var ceng = 0
  var is_piped = false
  while (CSA4to2Num!=0 || CSA3to2Num!=0){
    ceng = ceng + 1

    if (ceng == pipeLevel) is_piped = true

    in_next.remove(0,in_next.length)
    for (i <- 0 until CSA4to2Num) {
      val U_CSA4to2 = Module(new CSA4to2(width = width))
      U_CSA4to2.io.in_a := (if (is_piped) RegEnable(in(i*4+0), fire) else in(i*4+0))
      U_CSA4to2.io.in_b := (if (is_piped) RegEnable(in(i*4+1), fire) else in(i*4+1))
      U_CSA4to2.io.in_c := (if (is_piped) RegEnable(in(i*4+2), fire) else in(i*4+2))
      U_CSA4to2.io.in_d := (if (is_piped) RegEnable(in(i*4+3), fire) else in(i*4+3))
      in_next += U_CSA4to2.io.out_sum
      in_next += U_CSA4to2.io.out_car
    }
    for (i <- 0 until CSA3to2Num) {
      val U_CSA3to2 = Module(new CSA3to2(width = width))
      U_CSA3to2.io.in_a := (if (is_piped) RegEnable(in(i*3+0), fire) else in(i*3+0))
      U_CSA3to2.io.in_b := (if (is_piped) RegEnable(in(i*3+1), fire) else in(i*3+1))
      U_CSA3to2.io.in_c := (if (is_piped) RegEnable(in(i*3+2), fire) else in(i*3+2))
      in_next += U_CSA3to2.io.out_sum
      in_next += U_CSA3to2.io.out_car
    }

    if (remainder == 1) in_next += in.last
    if (remainder == 2) {
      in_next += in(in.length-2)
      in_next += in.last
    }
    in.remove(0,in.length)
    in_next.foreach(a => in += a)
    n_next = (CSA4to2Num+CSA3to2Num)*2 + remainder
    CSA4to2Num = if(n_next==8 || n_next==4) n_next/4 else 0
    CSA3to2Num = if(n_next==8 || n_next==4) 0 else n_next/3
    remainder = n_next - CSA4to2Num*4 - CSA3to2Num*3
  }

  io.out_sum := (if (pipeLevel>ceng) RegEnable(in_next(0), fire) else in_next(0))
  io.out_car := (if (pipeLevel>ceng) RegEnable(in_next(1), fire) else in_next(1))



}
private[fpulite] class CSA3to2(width :Int) extends Module{
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
private[fpulite] class CSA4to2(width :Int) extends Module{
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val in_c   = Input(UInt(width.W))
    val in_d   = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))

  })
  val cout_vec  = Wire(Vec(width,UInt(1.W)))
  val sum_vec   = Wire(Vec(width,UInt(1.W)))
  val carry_vec = Wire(Vec(width,UInt(1.W)))
  val cin = 0.U
  for (i <- 0 until width) {
    cout_vec(i) := Mux(io.in_a(i) ^ io.in_b(i), io.in_c(i), io.in_a(i))
    if (i ==0){
      sum_vec(i)   := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cin , io.in_d(i))
    }
    else {
      sum_vec(i)   :=  io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i) ^ cout_vec(i-1)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cout_vec(i-1), io.in_d(i))
    }
  }

  val sum_temp_vec   = Wire(Vec(width,UInt(1.W)))
  val carry_temp_vec = Wire(Vec(width,UInt(1.W)))
  carry_temp_vec(0) := 0.U
  sum_temp_vec(0)   := sum_vec(0)
  for (i <- 1 until width) {
    if (i%2==1) {
      carry_temp_vec(i) := sum_vec(i)
      sum_temp_vec(i)   := carry_vec(i-1)
    }
    else {
      carry_temp_vec(i) := carry_vec(i-1)
      sum_temp_vec(i)   := sum_vec(i)
    }
  }


  io.out_sum := sum_temp_vec.asUInt
  io.out_car := carry_temp_vec.asUInt



}

