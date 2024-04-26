package yunsuan.vector

import chisel3._
import chisel3.util._

class FloatAdderReductionF32WidenF16(val is_print:Boolean = false) extends Module {
  val floatType : String = "f32"
  var exponentWidth : Int = 0
  var significandWidth : Int = 0
  val floatTypeMap = Map("f16"->(5,11), "f32"->(8,24), "f64"->(11,53))
  require(floatTypeMap.contains(floatType) & exponentWidth ==0 & significandWidth == 0)
  exponentWidth = floatTypeMap.getOrElse(floatType,(exponentWidth,significandWidth))._1
  significandWidth = floatTypeMap.getOrElse(floatType,(exponentWidth,significandWidth))._2
  require(exponentWidth > 0 & significandWidth > 0)
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val fp_0         = Input (UInt(floatWidth.W)) // only first number use
    val fp_n         = Input (UInt(floatWidth.W))
    val is_init      = Input (Bool())
    val round_mode   = Input (UInt(3.W))
    val fp_result    = Output(UInt(floatWidth.W))
    val fflags       = Output(UInt(5.W))
    val res_widening = Input (Bool())
    val fp_format    = Input (UInt(2.W)) // if b01 -> f16 else f32
  })

  val U_Stage_N = Module(new FloatAdderReductionF32WidenF16StageN(floatType = floatType, is_print = is_print))
  U_Stage_N.io.stageN_in  := RegNext(U_Stage_N.io.stageN_out.asUInt).asTypeOf(new StageNInputBundleF32Widen(exponentWidth = exponentWidth, significandWidth = significandWidth))
  U_Stage_N.io.is_init      := io.is_init
  U_Stage_N.io.fp_0         := io.fp_0
  U_Stage_N.io.fp_n         := io.fp_n
  U_Stage_N.io.round_mode   := io.round_mode
  U_Stage_N.io.res_widening := io.res_widening
  U_Stage_N.io.fp_format    := io.fp_format

  io.fp_result := U_Stage_N.io.fp_result
  io.fflags := Mux(RegNext(io.is_init), U_Stage_N.io.fflags, U_Stage_N.io.fflags | RegNext(io.fflags))
}

private[vector] class StageNInputBundleF32WidenF16(val exponentWidth:Int, val significandWidth:Int) extends Bundle{

  val fp_a_sign              = Input(Bool())
  val fp_b_sign              = Input(Bool())
  val isEfp_bGreater         = Input(Bool())
  val round_mode             = Input(UInt(3.W))
  val fp_a_is_NAN            = Input(Bool())
  val fp_a_is_SNAN           = Input(Bool())
  val fp_b_is_NAN            = Input(Bool())
  val fp_b_is_SNAN           = Input(Bool())
  val fp_a_is_infinite       = Input(Bool())
  val fp_b_is_infinite       = Input(Bool())
  val is_far_path            = Input(Bool())
  // far IO
  val far_EOP                = Input(Bool())
  val far_A                  = Input(UInt((significandWidth+1).W))
  val far_E_greater          = Input(UInt(exponentWidth.W))
  val far_smallerSignificand_widen_1bit = Input(UInt((significandWidth+1).W))
  val far_B_rshift_value     = Input(UInt(exponentWidth.W))
  val far_sign_result        = Input(Bool())
  // close IO
  val close_EA               = Input(UInt(exponentWidth.W))
  val close_absEaSubEb       = Input(UInt(1.W))
  val close_smaller_significand_lsb = Input(Bool())
  val CS0_op0                = Input(UInt((significandWidth+1).W))
  val CS0_op1                = Input(UInt((significandWidth+1).W))
  val CS1_op0                = Input(UInt((significandWidth+1).W))
  val CS1_op1                = Input(UInt((significandWidth+1).W))
  val mask_Efp_a_onehot      = Input(UInt((significandWidth+1).W))
  val mask_Efp_b_onehot      = Input(UInt((significandWidth+1).W))
  val res_widening           = Input(Bool())
}

private[vector] class StageNInputBundleF32Widen(val exponentWidth:Int, val significandWidth:Int) extends Bundle{

  val fp_a_sign              = Input(Bool())
  val fp_b_sign              = Input(Bool())
  val isEfp_bGreater         = Input(Bool())
  val round_mode             = Input(UInt(3.W))
  val fp_a_is_NAN            = Input(Bool())
  val fp_a_is_SNAN           = Input(Bool())
  val fp_b_is_NAN            = Input(Bool())
  val fp_b_is_SNAN           = Input(Bool())
  val fp_a_is_infinite       = Input(Bool())
  val fp_b_is_infinite       = Input(Bool())
  val is_far_path            = Input(Bool())
  // far IO
  val far_EOP                = Input(Bool())
  val far_A                  = Input(UInt((significandWidth+1).W))
  val far_E_greater          = Input(UInt(exponentWidth.W))
  val far_smallerSignificand_widen_1bit = Input(UInt((significandWidth+1).W))
  val far_B_rshift_value     = Input(UInt(exponentWidth.W))
  val far_sign_result        = Input(Bool())
  // close IO
  val close_EA               = Input(UInt(exponentWidth.W))
  val close_absEaSubEb       = Input(UInt(1.W))
  val close_smaller_significand_lsb = Input(Bool())
  val CS0_op0                = Input(UInt((significandWidth+1).W))
  val CS0_op1                = Input(UInt((significandWidth+1).W))
  val CS1_op0                = Input(UInt((significandWidth+1).W))
  val CS1_op1                = Input(UInt((significandWidth+1).W))
  val mask_Efp_a_onehot      = Input(UInt((significandWidth+1).W))
  val mask_Efp_b_onehot      = Input(UInt((significandWidth+1).W))
  val res_widening           = Input(Bool())
}


private[vector] class FarPathAdderF32WidenF16(val AW:Int, val AdderType:String) extends Module {
  val io = IO(new Bundle() {
    val A   = Input (UInt(AW.W))
    val B   = Input (UInt(AW.W))
    val EOP = Input(Bool())
    val result = Output(UInt(AW.W))
  })
  if (AdderType == "FS0") {
    io.result  := io.A + io.B
  }
  if (AdderType == "FS1") {
    io.result  := io.A + io.B
  }
}

private[vector] class ClosePathAdderReductionF32WidenF16(val adderWidth:Int, val adderType:String) extends Module {
  val io = IO(new Bundle() {
    val adder_op0    = Input(UInt(adderWidth.W))
    val adder_op1    = Input(UInt(adderWidth.W))
    val adder_op2    = Input(UInt(14.W))
    val result       = Output(UInt(adderWidth.W))
  })
  if (adderType == "CS0") {
    io.result  := io.adder_op0 + io.adder_op1 + io.adder_op2
  }
  if (adderType == "CS1") {
    io.result  := io.adder_op0 + io.adder_op1 + io.adder_op2
  }
}


class ShiftLeftPriorityWithLZDResult(val srcW:Int, priorityShiftValueW:Int) extends Module {
  val io = IO(new Bundle() {
    val src        = Input (UInt(srcW.W))
    val priority_shiftValue = Input (UInt(priorityShiftValueW.W))
    val lshift_result  = Output(UInt(srcW.W))
    val lzd_result     = Output(UInt(srcW.U.getWidth.W))
  })
  def shiftLeftPriorityWithLZDResult(srcValue: UInt, priorityShiftValue: UInt): UInt = {
    val width = srcValue.getWidth
    val lzdWidth = srcValue.getWidth.U.getWidth
    def do_shiftLeftPriority(srcValue: UInt, priorityShiftValue: UInt, i:Int): UInt = {
      if (i==0) Cat(
        Mux(
          priorityShiftValue(i),
          Cat(srcValue(0),0.U((width-1).W)),
          0.U(width.W)
        ),
        Mux(
          priorityShiftValue(i),
          (width-i-1).U(lzdWidth.W),
          (width-i).U(lzdWidth.W)
        )
      )
      else Mux(
        priorityShiftValue(i),
        if (i==width-1) Cat(srcValue(i,0),(width-i-1).U(lzdWidth.W)) else Cat(Cat(srcValue(i,0),0.U((width-1-i).W)),(width-i-1).U(lzdWidth.W)),
        do_shiftLeftPriority(srcValue = srcValue, priorityShiftValue = priorityShiftValue, i = i - 1)
      )
    }
    do_shiftLeftPriority(srcValue = srcValue, priorityShiftValue = priorityShiftValue, i = width-1)
  }
  val lshift_result_lzd = shiftLeftPriorityWithLZDResult(io.src,io.priority_shiftValue)
  io.lshift_result := lshift_result_lzd.head(srcW)
  io.lzd_result := lshift_result_lzd.tail(srcW)
}


private[vector] class MuxModule extends Module {
  val io = IO(new Bundle() {
    val sel  = Input (Bool())
    val in_0 = Input (UInt())
    val in_1 = Input (UInt())
    val out  = Output(UInt())
  })
  io.out := Mux(
    io.sel,
    io.in_1,
    io.in_0
  )
}


private[vector] class Mux1HModule(val inNum:Int, val inW:Int) extends Module {
  val io = IO(new Bundle() {
    val sel = Input (Vec(inNum,Bool()))
    val in  = Input (Vec(inNum,UInt(inW.W)))
    val out = Output(UInt(inW.W))
  })
  io.out := Mux1H(
    io.sel,
    io.in
  )
}

private[vector] class FloatAdderReductionF32WidenF16StageN(val floatType : String = "f16", var exponentWidth : Int = 0, var significandWidth : Int = 0, val is_print:Boolean = false) extends Module {

  val floatTypeMap = Map("f16"->(5,11), "f32"->(8,24), "f64"->(11,53))
  require(floatTypeMap.contains(floatType) & exponentWidth ==0 & significandWidth == 0)
  exponentWidth = floatTypeMap.getOrElse(floatType,(exponentWidth,significandWidth))._1
  significandWidth = floatTypeMap.getOrElse(floatType,(exponentWidth,significandWidth))._2
  require(exponentWidth > 0 & significandWidth > 0)
  val floatWidth = exponentWidth + significandWidth
  val io = IO(new Bundle() {
    val is_init      = Input (Bool())
    val fp_0         = Input (UInt(floatWidth.W))
    val fp_n         = Input (UInt(floatWidth.W))
    val round_mode   = Input (UInt(3.W))
    val stageN_in    = new StageNInputBundleF32Widen(exponentWidth, significandWidth)
    val stageN_out   = Flipped(new StageNInputBundleF32Widen(exponentWidth, significandWidth))
    val fp_result    = Output(UInt(floatWidth.W))
    val fflags       = Output(UInt(5.W))
    val res_widening = Input(Bool())
    val fp_format    = Input (UInt(2.W))
  })
  val round_mode             = io.stageN_in.round_mode
  val isEfp_bGreater         = io.stageN_in.isEfp_bGreater
  val far_E_greater          = io.stageN_in.far_E_greater
  val EOP                    = io.stageN_in.far_EOP
  val far_EA_sub1            = far_E_greater-1.U
  val far_EA                 = Mux(EOP, far_EA_sub1, far_E_greater)
  val far_A                  = io.stageN_in.far_A
  val far_smallerSignificand_widen_1bit = io.stageN_in.far_smallerSignificand_widen_1bit
  val far_B_rshift_value     = io.stageN_in.far_B_rshift_value
  val far_sign_result        = io.stageN_in.far_sign_result
  val close_EA               = io.stageN_in.close_EA
  val close_absEaSubEb       = io.stageN_in.close_absEaSubEb.asBool
  val close_fp_a_sign        = io.stageN_in.fp_a_sign
  val close_fp_b_sign        = io.stageN_in.fp_b_sign
  val close_smaller_significand_lsb = io.stageN_in.close_smaller_significand_lsb
  val CS0_op0                = io.stageN_in.CS0_op0
  val CS0_op1                = io.stageN_in.CS0_op1
  val CS1_op0                = io.stageN_in.CS1_op0
  val CS1_op1                = io.stageN_in.CS1_op1
  val mask_Efp_a_onehot      = io.stageN_in.mask_Efp_a_onehot
  val mask_Efp_b_onehot      = io.stageN_in.mask_Efp_b_onehot

  val res_is_fp16            = io.fp_format === 1.U
  val RNE = round_mode === "b000".U  //Round to Nearest, ties to Even
  val RTZ = round_mode === "b001".U  //Round towards Zero
  val RDN = round_mode === "b010".U  //Round Down (towards −∞)
  val RUP = round_mode === "b011".U  //Round Up (towards +∞)
  val RMM = round_mode === "b100".U  //Round to Nearest, ties to Max Magnitude
  val NV  = WireInit(false.B)  //Invalid Operation
  val DZ  = WireInit(false.B)  //Divide by Zero
  val OF  = WireInit(false.B)  //Overflow
  val UF  = WireInit(false.B)  //Underflow
  val NX  = WireInit(false.B)  //Inexact
  io.fflags := Cat(NV,DZ,OF,UF,NX)
  // -----far path-----
  val far_absEaSubEb = far_B_rshift_value
  val farmaxShiftValue   = Mux(res_is_fp16,(11+2).U,(significandWidth+2).U)
  val widenWidth = significandWidth + 3
  val far_smallerSignificand_widen = Cat(far_smallerSignificand_widen_1bit,Fill(widenWidth,EOP))
  val U_far_B_rshift = Module(new FarShiftRightWithMuxInvFirst(far_smallerSignificand_widen.getWidth,farmaxShiftValue.getWidth))
  U_far_B_rshift.io.src := far_smallerSignificand_widen
  U_far_B_rshift.io.shiftValue := far_B_rshift_value(farmaxShiftValue.getWidth-1,0)
  U_far_B_rshift.io.EOP := EOP
  val far_rshift_widen_result = U_far_B_rshift.io.result
  val absEaSubEb_is_too_greater = far_absEaSubEb > Mux(res_is_fp16,(11+3).U,(significandWidth+3).U)
  val far_B = Mux(absEaSubEb_is_too_greater,Fill(significandWidth+1,EOP),far_rshift_widen_result.head(significandWidth+1))
  val B_guard_normal     = Mux(
    absEaSubEb_is_too_greater,
    false.B,
    Mux(
      EOP,
      Mux(res_is_fp16,!far_rshift_widen_result.head(11+2)(0).asBool,!far_rshift_widen_result.head(significandWidth+2)(0).asBool),
      Mux(res_is_fp16,far_rshift_widen_result.head(11+2)(0).asBool,far_rshift_widen_result.head(significandWidth+2)(0).asBool)
    )
  )
  val B_round_normal     = Mux(
    absEaSubEb_is_too_greater,
    false.B,
    Mux(
      EOP,
      Mux(res_is_fp16,!far_rshift_widen_result.head(11+3)(0).asBool,!far_rshift_widen_result.head(significandWidth+3)(0).asBool),
      Mux(res_is_fp16,far_rshift_widen_result.head(11+3)(0).asBool,far_rshift_widen_result.head(significandWidth+3)(0).asBool)
    )
  )
  val B_sticky_normal    = Mux(
    absEaSubEb_is_too_greater,
    Mux(EOP,(~far_smallerSignificand_widen_1bit).asUInt.orR,far_smallerSignificand_widen_1bit.orR),
    Mux(
      EOP,
      Mux(res_is_fp16,(~far_rshift_widen_result.tail(11+3)).asUInt.orR,(~far_rshift_widen_result.tail(significandWidth+3)).asUInt.orR),
      Mux(res_is_fp16,far_rshift_widen_result.tail(11+3).orR,far_rshift_widen_result.tail(significandWidth+3).orR)
    )
  )
  val B_rsticky_normal   = B_round_normal | B_sticky_normal
  val B_guard_overflow   = Mux(
    absEaSubEb_is_too_greater,
    false.B,
    Mux(
      EOP,
      Mux(res_is_fp16,!far_rshift_widen_result.head(11+1)(0).asBool,!far_rshift_widen_result.head(significandWidth+1)(0).asBool),
      Mux(res_is_fp16,far_rshift_widen_result.head(11+1)(0).asBool,far_rshift_widen_result.head(significandWidth+1)(0).asBool)
    )
  )
  val B_round_overflow   = B_guard_normal
  val B_sticky_overflow  = B_round_normal | B_sticky_normal
  val B_rsticky_overflow = B_round_overflow | B_sticky_overflow
  val U_FS0 = Module(new FarPathAdderF32WidenF16(AW = far_A.getWidth, AdderType = "FS0"))
  U_FS0.io.A := far_A
  U_FS0.io.B := far_B
  U_FS0.io.EOP := EOP
  val FS0 = U_FS0.io.result
  val U_FS1 = Module(new FarPathAdderF32WidenF16(AW = far_A.getWidth, AdderType = "FS1"))
  U_FS1.io.A := far_A + Mux(res_is_fp16,2.U<<13,2.U(15.W))
  U_FS1.io.B := far_B
  U_FS1.io.EOP := EOP
  val FS1 = U_FS1.io.result
  val far_case_normal   = !FS0.head(1).asBool
  val far_case_overflow = FS0.head(1).asBool
  val FS0_0 = Mux(res_is_fp16,FS0(0+13),FS0(0))
  val FS0_1 = Mux(res_is_fp16,FS0(1+13),FS0(1))
  val lgs_normal = Cat(
    FS0_0,
    Mux(EOP,(~Cat(B_guard_normal,B_rsticky_normal)).asUInt+1.U,Cat(B_guard_normal,B_rsticky_normal))
  )
  val far_case_normal_round_up = (EOP & !lgs_normal(1) & !lgs_normal(0)) |
    (RNE & lgs_normal(1) & (lgs_normal(2) | lgs_normal(0))) |
    (RDN & far_sign_result & (lgs_normal(1) | lgs_normal(0))) |
    (RUP & !far_sign_result & (lgs_normal(1) | lgs_normal(0))) |
    (RMM & lgs_normal(1))
  val normal_fsel0 = (!FS0_0 & far_case_normal_round_up) | !far_case_normal_round_up
  val normal_fsel1 = FS0_0 & far_case_normal_round_up
  val far_A_0 = Mux(res_is_fp16,far_A(0+13),far_A(0))
  val far_B_0 = Mux(res_is_fp16,far_B(0+13),far_B(0))
  val grs_overflow = Mux(EOP,Cat(far_A_0,0.U,0.U) - Cat(~far_B_0,B_guard_normal,B_rsticky_normal),Cat(FS0_0,B_guard_normal,B_rsticky_normal))
  val lgs_overflow = Cat(FS0_1,grs_overflow(2),grs_overflow(1) | grs_overflow(0))
  val far_case_overflow_round_up = (EOP & !lgs_overflow(1) & !lgs_overflow(0)) |
    (RNE & lgs_overflow(1) & (lgs_overflow(2) | lgs_overflow(0))) |
    (RDN & far_sign_result & (lgs_overflow(1) | lgs_overflow(0))) |
    (RUP & !far_sign_result & (lgs_overflow(1) | lgs_overflow(0))) |
    (RMM & lgs_overflow(1))
  val overflow_fsel0 = (!FS0_1 & far_case_overflow_round_up) | !far_case_overflow_round_up
  val overflow_fsel1 =  FS0_1 & far_case_overflow_round_up

  val far_exponent_result = Wire(UInt(exponentWidth.W))
  val far_EA_add1         = far_EA + 1.U

  val far_EA_add1_is_all_one = Mux(res_is_fp16,far_EA_add1(4,0).andR,far_EA_add1.andR)
  val far_OF = far_EA_add1_is_all_one & (far_case_overflow | (FS1.head(1).asBool & FS0_0 & far_case_normal_round_up))
  val far_result_overflow_round_to_normal = far_OF & (RTZ | (RDN & !far_sign_result) | (RUP & far_sign_result))
  val far_NX = Mux(far_case_normal,lgs_normal(1,0).orR,lgs_overflow(1,0).orR) | OF
  val far_fraction_result = Wire(UInt((significandWidth-1).W))
  val is_far_EA_add1 = ((far_case_overflow | (FS1.head(1).asBool & FS0_0 & far_case_normal_round_up)) &
    ((far_EA_add1_is_all_one & !(RTZ | (RDN & !far_sign_result) | (RUP & far_sign_result))) | !far_EA_add1_is_all_one)) |
    (!far_EA.orR & FS0.tail(1).head(1).asBool)
  far_exponent_result := Mux(
    is_far_EA_add1,
    far_EA_add1,
    far_EA
  )
  far_fraction_result := Mux1H(
    Seq(
      far_case_normal & normal_fsel0,
      far_case_normal & normal_fsel1,
      far_case_overflow & overflow_fsel0,
      far_case_overflow & overflow_fsel1
    ),
    Seq(
      Cat(
        FS0(significandWidth-2,1+13),
        Mux(res_is_fp16,FS0_0 ^ far_case_normal_round_up,FS0(13)),FS0(12,1),FS0_0 ^ far_case_normal_round_up
      ),
      Cat(
        FS1(significandWidth-2,1+13),
        Mux(res_is_fp16,0.U,FS1(13)),FS1(12,1),0.U),
      Cat(FS0(significandWidth-1,2+13),
        Mux(res_is_fp16,FS0_1 ^ far_case_overflow_round_up,FS0(14)),FS0(13,2),FS0_1 ^ far_case_overflow_round_up) |
        Fill(significandWidth-1,far_result_overflow_round_to_normal),
      FS1(significandWidth-1,1) | Fill(significandWidth-1,far_result_overflow_round_to_normal)
    )
  )
  val far_result = Cat(
    far_sign_result,
    far_exponent_result,
    Mux(far_OF,Fill(significandWidth-1,far_result_overflow_round_to_normal),far_fraction_result)
  )
  //----------------------------------
  //---------close path---------------
  //----------------------------------
  val close_B_guard = close_absEaSubEb & close_smaller_significand_lsb
  val close_B_round = false.B
  val close_B_sticky = false.B
  val U_CS0 = Module(new ClosePathAdderReductionF32WidenF16(adderWidth = significandWidth+1, adderType = "CS0"))
  U_CS0.io.adder_op0 := CS0_op0
  U_CS0.io.adder_op1 := CS0_op1
  U_CS0.io.adder_op2 := Mux(res_is_fp16,1.U<<13,1.U(14.W))
  val CS0 = Cat(U_CS0.io.result(significandWidth-1,0+13),Mux(res_is_fp16,0.U(13.W),U_CS0.io.result(12,0)))
  val priority_lshift_mask_0 = CS0 | mask_Efp_a_onehot(mask_Efp_a_onehot.getWidth-1,1)
  val U_CS1 = Module(new ClosePathAdderReductionF32WidenF16(adderWidth = significandWidth+1, adderType = "CS1"))
  U_CS1.io.adder_op0 := CS1_op0
  U_CS1.io.adder_op1 := CS1_op1
  U_CS1.io.adder_op2 := Mux(res_is_fp16,1.U<<13,1.U(14.W))
  val CS1 = Mux(
    close_absEaSubEb,
    Cat(U_CS1.io.result(significandWidth,0+13),Mux(res_is_fp16,0.U(13.W),U_CS1.io.result(12,0))),
    Cat(U_CS1.io.result(significandWidth-1,0+13),Mux(res_is_fp16,0.U(14.W),Cat(U_CS1.io.result(12,0),0.U)))
  )
  val priority_lshift_mask_1 = CS1 | Mux(close_absEaSubEb,Mux(isEfp_bGreater,mask_Efp_b_onehot,mask_Efp_a_onehot),mask_Efp_b_onehot)
  val close_sign_result = Wire(Bool())
  val close_exponent_result = Wire(UInt(exponentWidth.W))
  val close_fraction_result = Wire(UInt((significandWidth - 1).W))
  val close_greater_sign    = far_sign_result
  val CS1_1 = Mux(res_is_fp16,CS1(1+13),CS1(1))
  val CS1_0 = Mux(res_is_fp16,CS1(0+13),CS1(0))
  val round_up = close_smaller_significand_lsb & (
    (RUP & !close_greater_sign) | (RDN & close_greater_sign) | (RNE & CS1_1 & CS1_0) | RMM
    )
  val sel_CS0 = (!close_absEaSubEb & !U_CS0.io.result.head(1).asBool) | (close_absEaSubEb & U_CS1.io.result.head(1).asBool & round_up)
  val sel_CS1 = !sel_CS0
  val U_Lshift_0 = Module(new ShiftLeftPriorityWithLZDResult(srcW = CS0.getWidth, priorityShiftValueW = priority_lshift_mask_0.getWidth))
  U_Lshift_0.io.src := CS0
  U_Lshift_0.io.priority_shiftValue := priority_lshift_mask_0
  val CS0_lshift_result = U_Lshift_0.io.lshift_result(significandWidth-1,0)
  val U_Lshift_1 = Module(new ShiftLeftPriorityWithLZDResult(srcW = CS1.getWidth, priorityShiftValueW = priority_lshift_mask_1.getWidth))
  U_Lshift_1.io.src := CS1
  U_Lshift_1.io.priority_shiftValue := priority_lshift_mask_1
  val CS1_lshift_result = U_Lshift_1.io.lshift_result(significandWidth,1)
  val lzd_01 = Mux(sel_CS0,U_Lshift_0.io.lzd_result,U_Lshift_1.io.lzd_result)
  val CS_01_result = Mux(sel_CS0,CS0,CS1)
  val mask_onehot = Mux(
    sel_CS0,
    mask_Efp_a_onehot,
    Mux(
      close_absEaSubEb,
      mask_Efp_a_onehot,
      mask_Efp_b_onehot
    )
  )
  val close_NX = Mux(
    close_absEaSubEb & U_CS1.io.result.head(1).asBool,
    close_B_guard | round_up,
    0.U
  )
  close_fraction_result := Mux(sel_CS0,CS0_lshift_result.tail(1),CS1_lshift_result.tail(1))

  val U_lshift_msb_is_one_Mux = Module(new MuxModule)
  U_lshift_msb_is_one_Mux.io.sel := sel_CS0
  U_lshift_msb_is_one_Mux.io.in_1 := U_Lshift_0.io.lshift_result.head(1)
  U_lshift_msb_is_one_Mux.io.in_0 := U_Lshift_1.io.lshift_result.head(1)
  val lshift_msb_is_one = U_lshift_msb_is_one_Mux.io.out.asTypeOf(0.U).asBool
  val close_exponent_result_temp = Mux(
    lshift_msb_is_one,
    close_EA - lzd_01.asTypeOf(close_EA),
    0.U.asTypeOf(close_EA)
  )
  close_exponent_result := close_exponent_result_temp
  close_sign_result := Mux(
    !close_absEaSubEb & sel_CS1,
    close_fp_b_sign,
    Mux(
      !close_absEaSubEb & sel_CS0 & !U_CS0.io.result.head(1).asBool & !U_CS1.io.result.head(1).asBool,
      RDN,
      close_greater_sign
    )
  )
  val close_result = Cat(close_sign_result,close_exponent_result,close_fraction_result)

  val is_far_path        = io.stageN_in.is_far_path
  val fp_a_is_NAN        = io.stageN_in.fp_a_is_NAN
  val fp_a_is_SNAN       = io.stageN_in.fp_a_is_SNAN
  val fp_b_is_NAN        = io.stageN_in.fp_b_is_NAN
  val fp_b_is_SNAN       = io.stageN_in.fp_b_is_SNAN
  val fp_a_is_infinite   = io.stageN_in.fp_a_is_infinite
  val fp_b_is_infinite   = io.stageN_in.fp_b_is_infinite
  NX := Mux(is_far_path,far_NX,close_NX)
  OF := far_OF
  when((fp_a_is_SNAN | fp_b_is_SNAN) | (EOP & fp_a_is_infinite & fp_b_is_infinite)){
    io.fflags := "b10000".U
  }.elsewhen(fp_a_is_NAN | fp_b_is_NAN | fp_a_is_infinite | fp_b_is_infinite){
    io.fflags := "b00000".U
  }
  val fp_result_is_NAN = fp_a_is_NAN | fp_b_is_NAN | (EOP & fp_a_is_infinite & fp_b_is_infinite)
  val fp_result_is_infinite = !fp_result_is_NAN & (fp_a_is_infinite | fp_b_is_infinite)
  val fp_result_is_normal   = !fp_result_is_NAN & !fp_result_is_infinite
  val U_fp_result_Mux1H = Module(new Mux1HModule(inNum = 4, inW = floatWidth))
  U_fp_result_Mux1H.io.sel := VecInit(
    fp_result_is_NAN,
    fp_result_is_infinite,
    fp_result_is_normal & is_far_path,
    fp_result_is_normal & !is_far_path
  )
  U_fp_result_Mux1H.io.in := VecInit(
    Mux(res_is_fp16,Cat(0.U(17.W),Fill(5,1.U),1.U,Fill(11-2,0.U)),Cat(0.U,Fill(exponentWidth,1.U),1.U,Fill(significandWidth-2,0.U))),
    Mux(
      res_is_fp16,
      Cat(0.U(16.W),Mux(fp_a_is_infinite,close_fp_a_sign,close_fp_b_sign),Fill(5,1.U),Fill(11-1,0.U)),
      Cat(Mux(fp_a_is_infinite,close_fp_a_sign,close_fp_b_sign),Fill(exponentWidth,1.U),Fill(significandWidth-1,0.U))
    ),
    Mux(res_is_fp16,Cat(0.U(16.W),Cat(far_result.head(1),far_result(27,23),far_result(22,13))),far_result),
    Mux(res_is_fp16,Cat(0.U(16.W),Cat(close_result.head(1),close_result(27,23),close_result(22,13))),close_result)
  )
  io.fp_result := U_fp_result_Mux1H.io.out
  val normal_exponent_result = Mux(is_far_path,far_exponent_result,close_exponent_result)
  val normal_fraction_result = Mux(is_far_path,far_fraction_result,close_fraction_result) & Cat(Fill(10,1.U),Fill(13,!res_is_fp16))
  val exponent_result_is_not_zero = Mux(
    is_far_path,
    far_EA.orR | FS0.tail(1).head(1).asBool,
    lshift_msb_is_one
  )

  if (is_print){
    printf(p"is_far_path = ${Binary(is_far_path)}\n")
    printf(p"far_result = ${Binary(far_result)}\n")
    printf(p"far_A = ${Binary(far_A)}\n")
    printf(p"far_B = ${Binary(far_B)}\n")
    printf(p"far_case_overflow = ${Binary(far_case_overflow)}\n")
    printf(p"lgs_overflow = ${Binary(lgs_overflow)}\n")
    printf(p"round_up = ${Binary(round_up)}\n")
    printf(p"far_OF = ${Binary(far_OF)}\n")
    printf(p"far_result_overflow_round_to_normal = ${Binary(far_result_overflow_round_to_normal)}\n")
    printf(p"is_far_EA_add1 = ${Binary(is_far_EA_add1)}\n")
    printf(p"FS0 = ${Binary(FS0)}\n")
    printf(p"FS1 = ${Binary(FS1)}\n")
    printf(p"far_fraction_result = ${Binary(far_fraction_result)}\n")
    printf(p"CS0_op0 = ${Binary(CS0_op0)}\n")
    printf(p"CS0_op1 = ${Binary(CS0_op1)}\n")
    printf(p"CS1_op0 = ${Binary(CS1_op0)}\n")
    printf(p"CS1_op1 = ${Binary(CS1_op1)}\n")
    printf(p"far_B_rshift_value = ${Binary(far_B_rshift_value)}\n")
    printf(p"sel_CS0 = ${Binary(sel_CS0)}\n")
    printf(p"sel_CS1 = ${Binary(sel_CS1)}\n")
    printf(p"U_CS0.io.result          = ${Binary(U_CS0.io.result)}\n")
    printf(p"U_CS1.io.result          = ${Binary(U_CS1.io.result)}\n")
    printf(p"CS0                      = ${Binary(CS0)}\n")
    printf(p"CS1                      = ${Binary(CS1)}\n")
    printf(p"priority_lshift_mask_1   = ${Binary(priority_lshift_mask_1)}\n")
    printf(p"CS0_lshift_result        = ${Binary(CS0_lshift_result)}\n")
    printf(p"CS1_lshift_result        = ${Binary(CS1_lshift_result)}\n")
    printf(p"close_fraction_result    = ${Binary(close_fraction_result)}\n")
    printf(p"mask_Efp_a_onehot        = ${Binary(mask_Efp_a_onehot)}\n")
    printf(p"U_Lshift_1.io.lzd_result = ${Binary(U_Lshift_1.io.lzd_result)}\n")
    printf(p"lzd_01 = ${Binary(lzd_01)}\n")
    printf(p"lshift_result_head_is_one = ${Binary(lshift_msb_is_one)}\n")
  }


  val fp_0_16as32 = Cat(io.fp_0(15), Cat(0.U(3.W),io.fp_0(14,10)), Cat(io.fp_0(9,0),0.U(13.W)))
  val fp_n_16as32 = Cat(io.fp_n(15), Cat(0.U(3.W),io.fp_n(14,10)), Cat(io.fp_n(9,0),0.U(13.W)))

  val fp_n_16to32_is_denormal = !io.fp_n(14,10).orR
  val fp_n_lshift = Wire(UInt(10.W))
  val U_fp_n_is_denormal_to_f32 = Module(new ShiftLeftPriorityWithF32EXPResult(srcW = 10, priorityShiftValueW = 10, expW = 8))
  U_fp_n_is_denormal_to_f32.io.src := io.fp_n(9,0)
  U_fp_n_is_denormal_to_f32.io.priority_shiftValue := io.fp_n(9,0)
  fp_n_lshift := U_fp_n_is_denormal_to_f32.io.lshift_result
  val fp_n_is_denormal_to_f32_exp = U_fp_n_is_denormal_to_f32.io.exp_result
  val fp_n_16to32_mantissa = Cat(Mux(fp_n_16to32_is_denormal,Cat(fp_n_lshift.tail(1),0.U),io.fp_n(9,0)),0.U(13.W))
  val fp_n_16to32_exp = Mux(
    fp_n_16to32_is_denormal,
    fp_n_is_denormal_to_f32_exp,
    Mux(io.fp_n(14), Cat("b1000".U,io.fp_n(13,10)), Cat("b0111".U,io.fp_n(13,10)))
  )
  val fp_n_to32 = Mux(
    io.res_widening,
    Cat(io.fp_n(15), Mux(io.fp_n(14,0).orR,Cat(fp_n_16to32_exp, fp_n_16to32_mantissa),0.U(15.W))),
    Mux(res_is_fp16,fp_n_16as32,io.fp_n)
  )
  val fp_0_to32 = Mux(res_is_fp16,fp_0_16as32,io.fp_0)

  val is_init = io.is_init
  val E_fp_0 = fp_0_to32.tail(1).head(exponentWidth)
  val next_fp_a_sign = Mux(is_init,fp_0_to32.head(1).asBool,Mux(res_is_fp16,io.fp_result(15),io.fp_result.head(1).asBool))
  val next_fp_b_sign = fp_n_to32.head(1).asBool
  val next_EOP = (next_fp_a_sign ^ next_fp_b_sign).asBool
  val next_Efp_a = Mux(is_init,E_fp_0,normal_exponent_result)
  val next_Efp_b = fp_n_to32.tail(1).head(exponentWidth)
  val next_Efp_a_is_not_zero = Mux(is_init,E_fp_0.orR,exponent_result_is_not_zero)
  val next_Efp_b_is_not_zero = next_Efp_b.orR
  val next_significand_fp_a = Cat(
    next_Efp_a_is_not_zero,
    Mux(is_init,
      fp_0_to32.tail(exponentWidth + 1),
      normal_fraction_result)
  )
  val next_significand_fp_b = Cat(next_Efp_b_is_not_zero, fp_n_to32.tail(exponentWidth + 1))
  val EA = Mux(is_far_path,far_EA,close_EA)
  val EN = next_Efp_b
  val ER = next_Efp_a

  val U_EA_sub_EN = Module(new Adder(EA.getWidth, EA.getWidth, EA.getWidth+1, is_sub = true))
  U_EA_sub_EN.io.a := Mux(is_init,E_fp_0,EA)
  U_EA_sub_EN.io.b := EN
  val U_EN_sub_EA = Module(new Adder(EN.getWidth, EN.getWidth, EN.getWidth+1, is_sub = true))
  U_EN_sub_EA.io.a := EN
  U_EN_sub_EA.io.b := Mux(is_init,E_fp_0,EA)
  val EA_sub_EN = U_EA_sub_EN.io.c
  val EN_sub_EA = U_EN_sub_EA.io.c
  val abs_EA_sub_EN = Mux(EN_sub_EA.head(1).asBool, EA_sub_EN.tail(1), EN_sub_EA.tail(1))
  val EA_sub_EN_add_1 = EA_sub_EN + 1.U
  val EN_sub_EA_sub_1 = EN_sub_EA - 1.U
  val abs_EA_sub_EN_add_1 = Mux(EN_sub_EA_sub_1.head(1).asBool, EA_sub_EN_add_1.tail(1), EN_sub_EA_sub_1.tail(1))

  val U_EA_sub_EN_sub_lzd = Module(new Adder(EA_sub_EN_add_1.getWidth, EA_sub_EN_add_1.getWidth, EA_sub_EN_add_1.getWidth, is_sub = false))
  U_EA_sub_EN_sub_lzd.io.a := EA_sub_EN_add_1
  U_EA_sub_EN_sub_lzd.io.b := (~lzd_01.asTypeOf(EA_sub_EN_add_1)).asUInt
  val U_EN_sub_EA_add_lzd = Module(new Adder(EN_sub_EA.getWidth, EN_sub_EA.getWidth, EN_sub_EA.getWidth, is_sub = false))
  U_EN_sub_EA_add_lzd.io.a := EN_sub_EA
  U_EN_sub_EA_add_lzd.io.b := lzd_01
  val EA_sub_EN_sub_lzd = U_EA_sub_EN_sub_lzd.io.c
  val EN_sub_EA_add_lzd = U_EN_sub_EA_add_lzd.io.c

  val U_abs_EA_sub_EN_sub_lzd_Mux = Module(new MuxModule)
  U_abs_EA_sub_EN_sub_lzd_Mux.io.sel  := EN_sub_EA_add_lzd.head(1).asBool
  U_abs_EA_sub_EN_sub_lzd_Mux.io.in_1 := EA_sub_EN_sub_lzd.tail(1)
  U_abs_EA_sub_EN_sub_lzd_Mux.io.in_0 := EN_sub_EA_add_lzd.tail(1)
  val abs_EA_sub_EN_sub_lzd = U_abs_EA_sub_EN_sub_lzd_Mux.io.out

  val U_abs_ER_sub_EN_Mux1H = Module(new Mux1HModule(inNum = 4, inW = ER.getWidth))
  U_abs_ER_sub_EN_Mux1H.io.sel := VecInit(
    is_init | (is_far_path & !is_far_EA_add1),
    !is_init & is_far_path &  is_far_EA_add1,
    !is_init & !is_far_path & lshift_msb_is_one,
    !is_init & !is_far_path & !lshift_msb_is_one
  )
  U_abs_ER_sub_EN_Mux1H.io.in := VecInit(
    abs_EA_sub_EN,
    abs_EA_sub_EN_add_1,
    abs_EA_sub_EN_sub_lzd,
    0.U(exponentWidth.W)
  )
  val abs_ER_sub_EN = U_abs_ER_sub_EN_Mux1H.io.out

  val U_next_Efp_a_sub_1_Mux1H = Module(new Mux1HModule(inNum = 4, inW = exponentWidth))
  U_next_Efp_a_sub_1_Mux1H.io.sel := VecInit(
    is_init,
    !is_init & is_far_path & !is_far_EA_add1,
    !is_init & is_far_path & is_far_EA_add1,
    !is_init & !is_far_path
  )
  U_next_Efp_a_sub_1_Mux1H.io.in := VecInit(
    E_fp_0-1.U,
    far_EA_sub1-EOP.asUInt,
    far_EA,
    far_EA_sub1-lzd_01.asTypeOf(far_EA_sub1)
  )
  val next_Efp_a_sub_1 = U_next_Efp_a_sub_1_Mux1H.io.out

  val U_next_absEaSubEb_Mux = Module(new MuxModule)
  U_next_absEaSubEb_Mux.io.sel  := next_Efp_a_is_not_zero ^ next_Efp_b_is_not_zero
  U_next_absEaSubEb_Mux.io.in_1 := Mux(next_Efp_b_is_not_zero,next_Efp_b-1.U,next_Efp_a_sub_1)
  U_next_absEaSubEb_Mux.io.in_0 := abs_ER_sub_EN
  val next_absEaSubEb = U_next_absEaSubEb_Mux.io.out

  val U_next_isEfp_bGreater_Mux1H = Module(new Mux1HModule(inNum = 4, inW = 1))
  U_next_isEfp_bGreater_Mux1H.io.sel := VecInit(
    is_init | (is_far_path & !is_far_EA_add1),
    !is_init & is_far_path & is_far_EA_add1,
    !is_init & !is_far_path & lshift_msb_is_one,
    !is_init & !is_far_path & !lshift_msb_is_one
  )
  U_next_isEfp_bGreater_Mux1H.io.in := VecInit(
    EA_sub_EN.head(1).asBool,
    EA_sub_EN_add_1.head(1).asBool,
    EA_sub_EN.head(1).asBool | (EA_sub_EN<lzd_01),
    next_Efp_b_is_not_zero
  )
  val next_isEfp_bGreater = U_next_isEfp_bGreater_Mux1H.io.out.asBool

  val U_next_E_greater_Mux = Module(new MuxModule)
  U_next_E_greater_Mux.io.sel  := next_isEfp_bGreater
  U_next_E_greater_Mux.io.in_1 := next_Efp_b
  U_next_E_greater_Mux.io.in_0 := next_Efp_a
  val U_next_greaterSignificand_Mux = Module(new MuxModule)
  U_next_greaterSignificand_Mux.io.sel  := next_isEfp_bGreater
  U_next_greaterSignificand_Mux.io.in_1 := next_significand_fp_b
  U_next_greaterSignificand_Mux.io.in_0 := next_significand_fp_a
  val U_next_smallerSignificand_Mux = Module(new MuxModule)
  U_next_smallerSignificand_Mux.io.sel  := next_isEfp_bGreater
  U_next_smallerSignificand_Mux.io.in_1 := next_significand_fp_a
  U_next_smallerSignificand_Mux.io.in_0 := next_significand_fp_b
  val next_E_greater          = U_next_E_greater_Mux.io.out
  val next_far_E_greater      = next_E_greater

  val next_greaterSignificand = U_next_greaterSignificand_Mux.io.out
  val next_smallerSignificand = U_next_smallerSignificand_Mux.io.out

  val U_next_far_A_Mux = Module(new MuxModule)
  U_next_far_A_Mux.io.sel  := next_EOP
  U_next_far_A_Mux.io.in_1 := Cat(next_greaterSignificand,0.U)
  U_next_far_A_Mux.io.in_0 := Cat(0.U,next_greaterSignificand)
  val U_next_far_sign_result_Mux = Module(new MuxModule)
  U_next_far_sign_result_Mux.io.sel  := next_isEfp_bGreater
  U_next_far_sign_result_Mux.io.in_1 := next_fp_b_sign
  U_next_far_sign_result_Mux.io.in_0 := next_fp_a_sign
  val next_far_EOP                = next_EOP
  val next_far_A                  = U_next_far_A_Mux.io.out
  val next_far_smallerSignificand = next_smallerSignificand
  val U_next_far_smallerSignificand_widen_1bit_Mux = Module(new MuxModule)
  U_next_far_smallerSignificand_widen_1bit_Mux.io.sel  := next_EOP
  U_next_far_smallerSignificand_widen_1bit_Mux.io.in_1 := Cat(~next_far_smallerSignificand,1.U)
  U_next_far_smallerSignificand_widen_1bit_Mux.io.in_0 := Cat(0.U,next_far_smallerSignificand)
  val next_far_smallerSignificand_widen_1bit = U_next_far_smallerSignificand_widen_1bit_Mux.io.out
  val next_far_B_rshift_value     = next_absEaSubEb
  val next_far_sign_result        = U_next_far_sign_result_Mux.io.out

  val next_close_EA    = next_E_greater
  val next_close_absEaSubEb = next_Efp_a_is_not_zero & next_Efp_b_is_not_zero & (next_Efp_a(0) ^ next_Efp_b(0))
  val next_close_greater_significand = next_greaterSignificand
  val next_close_smaller_significand = next_smallerSignificand
  val next_close_smaller_significand_lsb = Mux(res_is_fp16,next_smallerSignificand(0+13).asBool,next_smallerSignificand(0).asBool)
  val next_CS0_op0 = Cat(0.U,next_close_greater_significand)

  val U_next_CS0_op1_Mux = Module(new MuxModule)
  U_next_CS0_op1_Mux.io.sel  := next_close_absEaSubEb
  U_next_CS0_op1_Mux.io.in_1 := Cat(1.U,1.U,~next_close_smaller_significand(significandWidth-1,1))
  U_next_CS0_op1_Mux.io.in_0 := Cat(1.U,~next_close_smaller_significand)
  val U_next_CS1_op0_Mux = Module(new MuxModule)
  U_next_CS1_op0_Mux.io.sel  := next_close_absEaSubEb
  U_next_CS1_op0_Mux.io.in_1 := Cat(next_close_greater_significand,0.U)
  U_next_CS1_op0_Mux.io.in_0 := Cat(0.U,next_close_smaller_significand)
  val U_next_CS1_op1_Mux = Module(new MuxModule)
  U_next_CS1_op1_Mux.io.sel  := next_close_absEaSubEb
  U_next_CS1_op1_Mux.io.in_1 := Cat(1.U,~next_close_smaller_significand)
  U_next_CS1_op1_Mux.io.in_0 := Cat(1.U,~next_close_greater_significand)
  val next_CS0_op1 = U_next_CS0_op1_Mux.io.out
  val next_CS1_op0 = U_next_CS1_op0_Mux.io.out
  val next_CS1_op1 = U_next_CS1_op1_Mux.io.out

  val next_mask_Efp_a_onehot = Cat(
    next_Efp_a === 0.U | next_Efp_a === 1.U,
    (for (i <- 2 until significandWidth + 2) yield
      (next_Efp_a === i.U).asUInt
      ).reduce(Cat(_, _))
  )
  val next_mask_Efp_b_onehot = Cat(
    next_Efp_b === 0.U | next_Efp_b === 1.U,
    (for (i <- 2 until significandWidth + 2) yield
      (next_Efp_b === i.U).asUInt
      ).reduce(Cat(_, _))
  )
  val next_fp_a_mantissa = fp_0_to32.tail(1+exponentWidth)
  val next_fp_b_mantissa = fp_n_to32.tail(1+exponentWidth)
  val next_Efp_a_is_all_one   = Mux(res_is_fp16,E_fp_0(4,0).andR,E_fp_0.andR)
  val next_Efp_b_is_all_one   = Mux(io.res_widening | res_is_fp16,io.fp_n.tail(17).head(5).andR,next_Efp_b.andR)
  val next_fp_a_mantissa_isnot_zero = next_fp_a_mantissa.orR
  val next_fp_b_mantissa_isnot_zero = Mux(io.res_widening | res_is_fp16,io.fp_n.tail(16+6).orR,next_fp_b_mantissa.orR)
  val next_fp_a_is_NAN        = Mux(
    is_init,
    next_Efp_a_is_all_one & next_fp_a_mantissa_isnot_zero,
    fp_result_is_NAN
  )
  val next_fp_a_is_SNAN       = is_init & next_Efp_a_is_all_one & next_fp_a_mantissa_isnot_zero & !next_fp_a_mantissa.head(1)
  val next_fp_b_is_NAN        = next_Efp_b_is_all_one & next_fp_b_mantissa_isnot_zero
  val next_fp_b_is_SNAN       = next_Efp_b_is_all_one & next_fp_b_mantissa_isnot_zero & !next_fp_b_mantissa.head(1)
  val next_fp_a_is_infinite   = Mux(
    is_init,
    next_Efp_a_is_all_one & (!next_fp_a_mantissa_isnot_zero),
    fp_result_is_infinite | (!fp_result_is_NAN & is_far_path & far_OF & !(RTZ | (RDN & !far_sign_result) | (RUP & far_sign_result)))
  )
  val next_fp_b_is_infinite   = next_Efp_b_is_all_one & (!next_fp_b_mantissa_isnot_zero)
  val next_is_far_path        = !next_far_EOP | next_absEaSubEb(exponentWidth-1, 1).orR | (next_absEaSubEb(0) === 1.U & (next_Efp_a_is_not_zero ^ next_Efp_b_is_not_zero))

  io.stageN_out.isEfp_bGreater            := next_isEfp_bGreater
  io.stageN_out.round_mode                := io.round_mode
  io.stageN_out.fp_a_is_NAN               := next_fp_a_is_NAN
  io.stageN_out.fp_a_is_SNAN              := next_fp_a_is_SNAN
  io.stageN_out.fp_b_is_NAN               := next_fp_b_is_NAN
  io.stageN_out.fp_b_is_SNAN              := next_fp_b_is_SNAN
  io.stageN_out.fp_a_is_infinite          := next_fp_a_is_infinite
  io.stageN_out.fp_b_is_infinite          := next_fp_b_is_infinite
  io.stageN_out.is_far_path               := next_is_far_path

  io.stageN_out.far_E_greater             := next_far_E_greater
  io.stageN_out.far_EOP                   := next_far_EOP
  io.stageN_out.far_A                     := next_far_A
  io.stageN_out.far_smallerSignificand_widen_1bit := next_far_smallerSignificand_widen_1bit
  io.stageN_out.far_B_rshift_value        := next_far_B_rshift_value
  io.stageN_out.far_sign_result           := next_far_sign_result
  io.stageN_out.close_EA                  := next_close_EA
  io.stageN_out.close_absEaSubEb          := next_close_absEaSubEb
  io.stageN_out.close_smaller_significand_lsb := next_close_smaller_significand_lsb
  io.stageN_out.fp_a_sign                 := next_fp_a_sign
  io.stageN_out.fp_b_sign                 := next_fp_b_sign
  io.stageN_out.CS0_op0                   := next_CS0_op0
  io.stageN_out.CS0_op1                   := next_CS0_op1
  io.stageN_out.CS1_op0                   := next_CS1_op0
  io.stageN_out.CS1_op1                   := next_CS1_op1
  io.stageN_out.mask_Efp_a_onehot         := next_mask_Efp_a_onehot
  io.stageN_out.mask_Efp_b_onehot         := next_mask_Efp_b_onehot
  io.stageN_out.res_widening              := io.res_widening
  if(is_print){
    printf(p"fp_n_to32                               = ${Binary(fp_n_to32                              )}\n")
    printf(p"io.stageN_out.fp_a_is_NAN               = ${Binary(io.stageN_out.fp_a_is_NAN              )}\n")
    printf(p"io.stageN_out.fp_a_is_SNAN              = ${Binary(io.stageN_out.fp_a_is_SNAN             )}\n")
    printf(p"io.stageN_out.fp_b_is_NAN               = ${Binary(io.stageN_out.fp_b_is_NAN              )}\n")
    printf(p"io.stageN_out.fp_b_is_SNAN              = ${Binary(io.stageN_out.fp_b_is_SNAN             )}\n")
    printf(p"io.stageN_out.fp_a_is_infinite          = ${Binary(io.stageN_out.fp_a_is_infinite         )}\n")
    printf(p"io.stageN_out.fp_b_is_infinite          = ${Binary(io.stageN_out.fp_b_is_infinite         )}\n")
    printf(p"io.stageN_out.is_far_path               = ${Binary(io.stageN_out.is_far_path              )}\n")
    printf(p"lshift_msb_is_one                       = ${Binary(lshift_msb_is_one                      )}\n")
    printf(p"EN_sub_EA_add_lzd                       = ${Binary(EN_sub_EA_add_lzd                      )}\n")
    printf(p"io.stageN_out.isEfp_bGreater            = ${Binary(io.stageN_out.isEfp_bGreater           )}\n")
    printf(p"io.stageN_out.round_mode                = ${Binary(io.stageN_out.round_mode               )}\n")
    printf(p"io.stageN_out.next_far_E_greater        = ${Binary(io.stageN_out.far_E_greater            )}\n")
    printf(p"io.stageN_out.far_EOP                   = ${Binary(io.stageN_out.far_EOP                  )}\n")
    printf(p"io.stageN_out.far_A                     = ${Binary(io.stageN_out.far_A                    )}\n")
    printf(p"io.stageN_out.far_smallerSignificand_widen_1bit = ${Binary(io.stageN_out.far_smallerSignificand_widen_1bit)}\n")
    printf(p"EA_sub_EN                               = ${Binary(EA_sub_EN                              )}\n")
    printf(p"EN_sub_EA                               = ${Binary(EN_sub_EA                              )}\n")
    printf(p"abs_EA_sub_EN                           = ${Binary(abs_EA_sub_EN                          )}\n")
    printf(p"abs_EA_sub_EN_add_1                     = ${Binary(abs_EA_sub_EN_add_1                    )}\n")
    printf(p"EA_sub_EN_sub_lzd                       = ${Binary(EA_sub_EN_sub_lzd                      )}\n")
    printf(p"EN_sub_EA_add_lzd                       = ${Binary(EN_sub_EA_add_lzd                      )}\n")
    printf(p"abs_EA_sub_EN_sub_lzd                   = ${Binary(abs_EA_sub_EN_sub_lzd                  )}\n")
    printf(p"ER                                      = ${Binary(ER                                     )}\n")
    printf(p"abs_ER_sub_EN                           = ${Binary(abs_ER_sub_EN                          )}\n")
    printf(p"io.stageN_out.far_B_rshift_value        = ${Binary(io.stageN_out.far_B_rshift_value       )}\n")
    printf(p"io.stageN_out.far_sign_result           = ${Binary(io.stageN_out.far_sign_result          )}\n")
    printf(p"io.stageN_out.close_EA                  = ${Binary(io.stageN_out.close_EA                 )}\n")
    printf(p"io.stageN_out.close_absEaSubEb          = ${Binary(io.stageN_out.close_absEaSubEb         )}\n")
    printf(p"io.stageN_out.close_fp_a_sign           = ${Binary(io.stageN_out.fp_a_sign                )}\n")
    printf(p"io.stageN_out.close_fp_b_sign           = ${Binary(io.stageN_out.fp_b_sign                )}\n")
    printf(p"io.stageN_out.CS0_op0                   = ${Binary(io.stageN_out.CS0_op0                  )}\n")
    printf(p"io.stageN_out.CS0_op1                   = ${Binary(io.stageN_out.CS0_op1                  )}\n")
    printf(p"io.stageN_out.CS1_op0                   = ${Binary(io.stageN_out.CS1_op0                  )}\n")
    printf(p"io.stageN_out.CS1_op1                   = ${Binary(io.stageN_out.CS1_op1                  )}\n")
    printf(p"io.stageN_out.mask_Efp_a_onehot         = ${Binary(io.stageN_out.mask_Efp_a_onehot        )}\n")
    printf(p"io.stageN_out.mask_Efp_b_onehot         = ${Binary(io.stageN_out.mask_Efp_b_onehot        )}\n")
  }
}
