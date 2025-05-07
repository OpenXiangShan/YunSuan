package race.vpu.yunsuan

import chisel3._
import chisel3.util._

import race.vpu.yunsuan._
import race.vpu.VParams._
import race.vpu._

class Vfreduction extends Module{
  val io = IO(new Bundle {
    val in      = Input(new VfredInput)
    val out     = ValidIO(new VfredOutput)
  })

  val is_fp16 = io.in.uop.csr.vsew(1, 0) === "b01".U
  val is_fp32 = io.in.uop.csr.vsew(1, 0) === "b10".U
  val is_bf16 = io.in.uop.csr.vsew(1, 0) === "b00".U


  val fp19tofp32_res = Wire(Vec(VLEN/32, UInt(32.W)))

  for (i <- 0 until (VLEN / 32)) {
      val fp_a = io.in.vs2(16-1+i*32, 0+i*32)
      val fp_b = io.in.vs2(32-1+i*32, 16+i*32)
      val mask = io.in.mask(i*4+3, i*4)
      val vfadd_fp16mixedbf16 = Module(new FloatAdderMixedFP16BF16_Wrapped)
      vfadd_fp16mixedbf16.io.fire      := io.in.fire && (is_fp16 || is_bf16)
      vfadd_fp16mixedbf16.io.mask      := io.in.mask   
      vfadd_fp16mixedbf16.io.round_mode:= io.in.uop.csr.frm
      vfadd_fp16mixedbf16.io.op_code   := io.in.op_code
      vfadd_fp16mixedbf16.io.fp_format := io.in.uop.csr.vsew(1,0)
      vfadd_fp16mixedbf16.io.fp_a      := fp_a
      vfadd_fp16mixedbf16.io.fp_b      := fp_b
      vfadd_fp16mixedbf16.io.fp_aIsFpCanonicalNAN := 0.U
      vfadd_fp16mixedbf16.io.fp_bIsFpCanonicalNAN := 0.U
      fp19tofp32_res(i) := vfadd_fp16mixedbf16.io.fp_result
  }

  val vctrl_input = Wire(new Vfredctrl)
  vctrl_input.mask      := io.in.mask   
  vctrl_input.op_code   := io.in.op_code
  vctrl_input.uop       := io.in.uop    
  vctrl_input.fire      := io.in.fire
  vctrl_input.vs1       := io.in.vs1
  val vctrl_fp19 = RegNext(RegNext(Mux(io.in.fire && (is_fp16 || is_bf16), vctrl_input, 0.U.asTypeOf(new Vfredctrl))))

  // result format b01->fp16,b10->fp32,b11->fp64
  val stages = log2Ceil(VLEN / 32)
  val vredu_pipes = Seq.tabulate(stages) { i =>
    val num = (VLEN / 32) / scala.math.pow(2, i + 1).toInt
    Module(new VfredFP32_Pipelined(num = num))
  }

  for (i <- 0 until stages) {
  if (i == 0) {
    vredu_pipes(i).io.vctrl_pipe_in           := Mux(is_fp32, vctrl_input, vctrl_fp19)
    vredu_pipes(i).io.vd_pipe_in              := io.in.vs2
  } else {
    vredu_pipes(i).io.vctrl_pipe_in := vredu_pipes(i - 1).io.vctrl_pipe_out
    vredu_pipes(i).io.vd_pipe_in    := vredu_pipes(i - 1).io.vd_pipe_out
  }
  }

  // final stage for fp32
  // with one reg for accumulating the result
  val vctrl_pipe_stage_last = vredu_pipes(stages-1).io.vctrl_pipe_out
  val vd_pipe_stage_last    = vredu_pipes(stages-1).io.vd_pipe_out

  val vfred_pipe_fp32       = Module(new VfredFP32_Pipelined(num = 1))
  val vfred_fp32_result     = vfred_pipe_fp32.io.vd_pipe_out
  // val fp32_fflags = vfred_pipe_fp32.io.fflags
  val fp32_fflags = 0.U(5.W)

  val reg_res1 = vfred_fp32_result
  val reg_res2 = RegEnable(vfred_fp32_result, vfred_pipe_fp32.io.vctrl_pipe_out.uop.uopIdx(0) === 0.U(1.W))

  val vs1 = Mux(vctrl_pipe_stage_last.uop.csr.vsew(1, 0) === "b01".U, Cat(Fill(16, 0.U), vctrl_pipe_stage_last.vs1(15, 0)), 
                            Mux(vctrl_pipe_stage_last.uop.csr.vsew(1, 0) === "b10".U, vctrl_pipe_stage_last.vs1(31, 0), 0.U(32.W)))
  
  val fp32_fp_a   = vd_pipe_stage_last
  val fp32_fp_b   = Mux(vctrl_pipe_stage_last.uop.uopIdx === 0.U, vs1, 
                      Mux(vctrl_pipe_stage_last.uop.uopIdx === "b001".U, 0.U(32.W), vfred_fp32_result))
  vfred_pipe_fp32.io.vctrl_pipe_in := vctrl_pipe_stage_last
  vfred_pipe_fp32.io.vd_pipe_in := Cat(fp32_fp_a, fp32_fp_b)
  
  val vlmul_pipe_fp32_cvt = MuxLookup(vfred_pipe_fp32.io.vctrl_pipe_out.uop.csr.vlmul, "b000".U, Seq(
    "b000".U -> "b000".U,   // LMUL = 1 → 000
    "b001".U -> "b001".U,   // LMUL = 2 → 001
    "b010".U -> "b011".U,   // LMUL = 4 → 011
    "b011".U -> "b111".U    // LMUL = 8 → 111
  ))

  val adder_for_lmul  = Module(new VfredFP32_Pipelined(num = 1))
  adder_for_lmul.io.vctrl_pipe_in :=  Mux(vfred_pipe_fp32.io.vctrl_pipe_out.uop.uopIdx === vlmul_pipe_fp32_cvt, 
                                          vfred_pipe_fp32.io.vctrl_pipe_out, 0.U.asTypeOf(new Vfredctrl))
  adder_for_lmul.io.vd_pipe_in := Cat(reg_res1, reg_res2)
  

  // arbitrate the result
  val uop  = Mux(vfred_pipe_fp32.io.vctrl_pipe_out.uop.csr.vlmul === 0.U, vfred_pipe_fp32.io.vctrl_pipe_out.uop, adder_for_lmul.io.vctrl_pipe_out.uop)
  val fp_finish = Mux(vfred_pipe_fp32.io.vctrl_pipe_out.uop.csr.vlmul === 0.U, vfred_pipe_fp32.io.vctrl_pipe_out.fire, adder_for_lmul.io.vctrl_pipe_out.fire)
  val is_res_fp32 = uop.csr.vsew(1, 0) === "b10".U
  val is_res_fp16 = uop.csr.vsew(1, 0) === "b01".U
  val is_res_bf16 = uop.csr.vsew(1, 0) === "b00".U

  // here need narrow the result to fp16 and bf16
  val fp32_result = Mux(vfred_pipe_fp32.io.vctrl_pipe_out.uop.csr.vlmul === 0.U, vfred_fp32_result, adder_for_lmul.io.vd_pipe_out)
  val fp16_result = Cat(Fill(16, 0.U), Fill(16, 0.U))
  val bf16_result = Cat(Fill(16, 0.U), Fill(16, 0.U))

  io.out.bits.result := Mux(is_res_fp32 && fp_finish, fp32_result,
  Mux(is_res_fp16 && fp_finish, Cat(Fill(16, 0.U), fp16_result), 0.U))

  io.out.bits.fflags := Mux(fp_finish, fp32_fflags, 0.U)

  io.out.valid := fp_finish

  io.out.bits.uop :=  Mux(fp_finish, uop, 0.U.asTypeOf(new VUop))
}

// num = VLEN/XLEN/2^i
class VfredFP32_Pipelined(val num: Int = 0) extends Module {
  val io = IO(new Bundle() {
    val vctrl_pipe_in = Input(new Vfredctrl)
    val vctrl_pipe_out = Output(new Vfredctrl)
    val vd_pipe_in = Input(UInt((num*32*2).W))
    val vd_pipe_out = Output(UInt((num*32).W))
  })

  val result_pipe_out  = Wire(Vec(num, UInt(32.W)))
  val fflags_pipe_out  = Wire(Vec(num, UInt(5.W)))

  for (i <- 0 until num) {
      val fp_a = io.vd_pipe_in(31+i*64, 0+i*64)
      val fp_b = io.vd_pipe_in(63+i*64, 32+i*64)
      val mask = io.vctrl_pipe_in.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_Wrapped)
      vfred.io.fire := io.vctrl_pipe_in.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := io.vctrl_pipe_in.uop.ctrl.vv
      vfred.io.round_mode := io.vctrl_pipe_in.uop.csr.frm
      
      vfred.io.fp_format  :=   "b10".U(2.W)
      vfred.io.mask := mask

      vfred.io.op_code    := io.vctrl_pipe_in.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe_out(i) := vfred.io.fp_result
      fflags_pipe_out(i) := vfred.io.fflags
  }

  val vctrl_pipe_out = RegNext(RegNext(io.vctrl_pipe_in))

  io.vd_pipe_out    := result_pipe_out.asTypeOf(io.vd_pipe_out)
  io.vctrl_pipe_out := vctrl_pipe_out
}

// latency + 1 = 2
class VfredFP32WidenMixedFP16_Wrapped() extends Module {
    val exponentWidth = 8
    val significandWidth = 24
    val floatWidth = exponentWidth + significandWidth
    val io = IO(new Bundle() {
    val fire          = Input (Bool())
    val fp_a, fp_b    = Input (UInt(floatWidth.W)) 

    val mask          = Input (UInt(4.W))
    val is_vec        = Input (Bool())
    val round_mode    = Input (UInt(3.W))
    val fp_format     = Input (UInt(2.W)) 

    val op_code       = Input (UInt(5.W)) 
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())

    val fp_result     = Output(UInt(floatWidth.W))
    val fflags        = Output(UInt(5.W))
  })

  val fire = io.fire
  val fire_next = RegNext(fire)
  val is_vec_reg    = RegEnable(io.is_vec, fire)
  val fp_format_reg = RegEnable(io.fp_format, fire)

  val fast_is_sub = io.op_code(0)
  val res_is_f16 = fp_format_reg === 1.U
  val res_is_f32 = fp_format_reg === 2.U

 
  
  val hasMinMaxCompare = true
  val fp_format = io.fp_format-1.U


  val U_F16 = Module(new FloatAdderF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F16.io.fire := fire
  U_F16.io.fp_a := io.fp_a(31,16)
  U_F16.io.fp_b := io.fp_b(31,16)
  U_F16.io.mask    := false.B
  U_F16.io.maskForReduction := 0.U

  U_F16.io.is_sub := fast_is_sub
  U_F16.io.round_mode := io.round_mode
  U_F16.io.mask := false.B
  U_F16.io.op_code    := io.op_code
  U_F16.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F16.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN

  val U_F16_1_result = U_F16.io.fp_c
  val U_F16_1_fflags = U_F16.io.fflags


  val U_F32_Mixed = Module(new FloatAdderF32WidenF16MixedPipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  U_F32_Mixed.io.fire := fire
  U_F32_Mixed.io.fp_a := io.fp_a
  U_F32_Mixed.io.fp_b := io.fp_b
  U_F32_Mixed.io.widen_a := 0.U
  U_F32_Mixed.io.widen_b := 0.U
  U_F32_Mixed.io.mask    := false.B
  U_F32_Mixed.io.res_widening := false.B
  U_F32_Mixed.io.opb_widening := false.B
  U_F32_Mixed.io.maskForReduction := 0.U
  U_F32_Mixed.io.is_sub       := 0.U
  U_F32_Mixed.io.round_mode   := io.round_mode
  U_F32_Mixed.io.is_vfwredosum := false.B

  // TODO:
  U_F32_Mixed.io.fp_format    := fp_format
  U_F32_Mixed.io.op_code      := io.op_code
  U_F32_Mixed.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  U_F32_Mixed.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN

  val U_F32_result = U_F32_Mixed.io.fp_c
  val U_F32_fflags = U_F32_Mixed.io.fflags
  val U_F16_0_result = U_F32_Mixed.io.fp_c(15,0)
  val U_F16_0_fflags = U_F32_Mixed.io.fflags

  val fp_f32_result = U_F32_result
  val fp_f16_result = Cat(Fill(16, is_vec_reg) & U_F16_1_result, U_F16_0_result)


  val fp_result = Mux1H(
    Seq(
      res_is_f16,
      res_is_f32
    ),
    Seq(
      fp_f16_result,
      fp_f32_result
    )
  )

  val fflags = Mux(res_is_f32, U_F32_fflags, (U_F16_1_fflags | U_F16_0_fflags))

  io.fp_result := RegNext(fp_result)
  io.fflags    := RegNext(fflags)
}

class FloatAdderF16Pipeline_Wrapped (val is_print:Boolean = false,val hasMinMaxCompare:Boolean = false) extends Module {
  val exponentWidth = 5
  val significandWidth = 11
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

  val fp16_adder  = Module(new FloatAdderF16Pipeline(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  val fp16_result = fp16_adder.io.fp_c
  val fp16_fflags = fp16_adder.io.fflags

  fp16_adder.io.fire        := io.fire
  fp16_adder.io.fp_a        := io.fp_a
  fp16_adder.io.fp_b        := io.fp_b
  fp16_adder.io.is_sub      := io.is_sub
  fp16_adder.io.round_mode  := io.round_mode
  fp16_adder.io.mask        := io.mask
  fp16_adder.io.op_code     := io.op_code
  fp16_adder.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  fp16_adder.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  fp16_adder.io.maskForReduction := io.maskForReduction

  io.fp_c   := RegNext(fp16_result)
  io.fflags := RegNext(fp16_fflags)
}

class FloatAdderMixedFP16BF16_Wrapped() extends Module {
  val exponentWidthBF16 : Int = 8
  val significandWidthBF16 : Int = 8
  val exponentWidthFP16 : Int = 5
  val significandWidthFP16 : Int = 11
  val exponentWidthFP32 : Int = 8
  val significandWidthFP32 : Int = 24
  
  val io = IO(new Bundle() {
    val fire          = Input (Bool())
    val fp_a, fp_b    = Input (UInt(16.W)) // fp_a -> vs2, fp_b -> vs1
    val mask          = Input (Bool())
    val round_mode    = Input (UInt(3.W))
    val fp_format     = Input (VectorElementFormat()) // result format b01->fp16,b10->fp32,b11->fp64
    val op_code       = Input (UInt(5.W))
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())
    val fp_result     = Output(UInt(32.W))
    val fflags        = Output(UInt(20.W))
  })


  val fp16_to_fp19_fpa = Module(new FloatAdderRandomWidenFormat(
    src_exponentWidth = exponentWidthFP16,
    src_significandWidth = significandWidthFP16,
    dst_exponentWidth = exponentWidthBF16,
    dst_significandWidth = significandWidthFP16
  ))

  val fp16_to_fp19_fpb = Module(new FloatAdderRandomWidenFormat(
    src_exponentWidth = exponentWidthFP16,
    src_significandWidth = significandWidthFP16,
    dst_exponentWidth = exponentWidthBF16,
    dst_significandWidth = significandWidthFP16
  ))

  val fp16_to_fp19_frs1 = Module(new FloatAdderRandomWidenFormat(
    src_exponentWidth = exponentWidthFP16,
    src_significandWidth = significandWidthFP16,
    dst_exponentWidth = exponentWidthBF16,
    dst_significandWidth = significandWidthFP16
  ))

  fp16_to_fp19_fpa.io.widen_src := io.fp_a
  val fp16_a_as_fp19 = fp16_to_fp19_fpa.io.widen_dst
  fp16_to_fp19_fpb.io.widen_src := io.fp_a
  val fp16_b_as_fp19 = fp16_to_fp19_fpb.io.widen_dst
  fp16_to_fp19_frs1.io.widen_src := io.fp_a
  val fp16_frs1_as_fp19 = fp16_to_fp19_frs1.io.widen_dst
  
  val fire = io.fire
  val fp_format_reg = RegEnable(io.fp_format, fire)
  val is_bf16 = fp_format_reg === 0.U
  val is_fp16 = fp_format_reg === 1.U

  val bf16_a_as_fp19 = Cat(io.fp_a(15), io.fp_a.tail(1).head(exponentWidthBF16), Cat(io.fp_a(significandWidthBF16-2,0),0.U((significandWidthFP16 - significandWidthBF16).W)))
  val bf16_b_as_fp19 = Cat(io.fp_b(15), io.fp_b.tail(1).head(exponentWidthBF16), Cat(io.fp_b(significandWidthBF16-2,0),0.U((significandWidthFP16 - significandWidthBF16).W)))
  
  val fp19_a = MuxCase(0.U(19.W), Seq(
    is_bf16 -> bf16_a_as_fp19,
    is_fp16 -> fp16_a_as_fp19
  ))
  val fp19_b = MuxCase(0.U(19.W), Seq(
    is_bf16 -> bf16_b_as_fp19,
    is_fp16 -> fp16_b_as_fp19
  ))


  val hasMinMaxCompare = true
  val fast_is_sub = io.op_code(0)

  val Mixed_Adder = Module(new FloatAdderMixedFP19(is_print = false,hasMinMaxCompare = hasMinMaxCompare))
  Mixed_Adder.io.fire := fire
  Mixed_Adder.io.fp_a := fp19_a
  Mixed_Adder.io.fp_b := fp19_b
  Mixed_Adder.io.mask := io.mask
  Mixed_Adder.io.is_sub := fast_is_sub
  Mixed_Adder.io.round_mode := io.round_mode
  Mixed_Adder.io.op_code    := io.op_code
  Mixed_Adder.io.fp_aIsFpCanonicalNAN := io.fp_aIsFpCanonicalNAN
  Mixed_Adder.io.fp_bIsFpCanonicalNAN := io.fp_bIsFpCanonicalNAN
  Mixed_Adder.io.maskForReduction := 0.U

  io.fp_result   := RegNext(Mixed_Adder.io.fp_c ## 0.U(13.W))
  io.fflags := RegNext(Mixed_Adder.io.fflags)

}


class FloatAdderMixedFP19(val is_print:Boolean = false,val hasMinMaxCompare:Boolean = false) extends Module {
  val exponentWidth = 8
  val significandWidth = 11
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


class FloatAdderRandomWidenFormat(
  val src_exponentWidth : Int,
  val src_significandWidth : Int,
  val dst_exponentWidth : Int,
  val dst_significandWidth : Int,
) extends Module {
  val src_w = src_exponentWidth + src_significandWidth
  val dst_w = dst_exponentWidth + dst_significandWidth
  val io = IO(new Bundle() {
    val widen_src = Input(UInt(src_w.W))
    val widen_dst = Output(UInt(dst_w.W))
  })
  def Widen(in:UInt): UInt= {
    assert(in.getWidth == src_w)
    val w = src_w
    val exp_w = src_exponentWidth
    val sig_w = src_significandWidth
    val dest_exp_w = dst_exponentWidth
    val dest_sig_w = dst_significandWidth

    val fp_a_is_denormal = !in(w-2,sig_w-1).orR
    val fp_a_lshift = Wire(UInt((sig_w-1).W))
    val fp_a_is_denormal_to_widen_exp = Wire(UInt(dest_exp_w.W))
    val U_fp_a_is_denormal_to_widen = Module(new ShiftLeftPriorityWithEXPResult(srcW = sig_w-1, priorityShiftValueW = sig_w-1, expW = dest_exp_w))
    U_fp_a_is_denormal_to_widen.io.src := in(sig_w-2,0)
    U_fp_a_is_denormal_to_widen.io.priority_shiftValue := in(sig_w-2,0)
    fp_a_lshift := U_fp_a_is_denormal_to_widen.io.lshift_result
    fp_a_is_denormal_to_widen_exp := U_fp_a_is_denormal_to_widen.io.exp_result
    val fp_a_widen_mantissa = Cat(
      Mux(fp_a_is_denormal,Cat(fp_a_lshift.tail(1),0.U),in(sig_w-2,0)),
      0.U((dest_sig_w-sig_w).W)
    )
    val const_1 =  if (in.getWidth == 16) "b1000".U else "b1000".U
    val const_0 =  if (in.getWidth == 16) "b0111".U else "b0111".U
    val fp_a_widen_exp = Mux(
      fp_a_is_denormal,
      fp_a_is_denormal_to_widen_exp,
      Mux(in(w-2), Cat(const_1,in(w-3,w-1-exp_w)), Cat(const_0,in(w-3,w-1-exp_w)))
    )
    Cat(in(w-1), fp_a_widen_exp, fp_a_widen_mantissa)
  }
  io.widen_dst := Widen(io.widen_src)
}


class ShiftLeftPriorityWithEXPResult(val srcW:Int, priorityShiftValueW:Int, expW:Int) extends Module {
  val io = IO(new Bundle() {
    val src        = Input (UInt(srcW.W))
    val priority_shiftValue = Input (UInt(priorityShiftValueW.W))
    val lshift_result  = Output(UInt(srcW.W))
    val exp_result     = Output(UInt(expW.W))
  })
  def shiftLeftPriorityWithEXPResult(srcValue: UInt, priorityShiftValue: UInt): UInt = {
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
          "b01110000".U-(width-i-1).U(8.W),
          "b01110000".U-(width-i).U(8.W)
        )
      )
      else Mux(
        priorityShiftValue(i),
        if (i==width-1) Cat(srcValue(i,0),"b01110000".U-(width-i-1).U(8.W)) else Cat(Cat(srcValue(i,0),0.U((width-1-i).W)),"b01110000".U-(width-i-1).U(8.W)),
        do_shiftLeftPriority(srcValue = srcValue, priorityShiftValue = priorityShiftValue, i = i - 1)
      )
    }
    do_shiftLeftPriority(srcValue = srcValue, priorityShiftValue = priorityShiftValue, i = width-1)
  }
  val lshift_result_lzd_exp = shiftLeftPriorityWithEXPResult(io.src,io.priority_shiftValue)
  io.lshift_result := lshift_result_lzd_exp.head(srcW)
  io.exp_result := lshift_result_lzd_exp.tail(srcW)
}
