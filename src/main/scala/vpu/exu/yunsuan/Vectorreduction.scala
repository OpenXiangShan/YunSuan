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

  // result format b01->fp16,b10->fp32,b11->fp64
  val stages = log2Ceil(VLEN / 32)

  val vredu_pipes = Seq.tabulate(stages) { i =>
    val num = (VLEN / 32) / scala.math.pow(2, i + 1).toInt
    Module(new VfredFP32WidenMixedFP16_Pipelined(num = num))
  }

  for (i <- 0 until stages) {
  if (i == 0) {
    vredu_pipes(i).io.vctrl_pipe_in.mask      := io.in.mask   
    vredu_pipes(i).io.vctrl_pipe_in.op_code   := io.in.op_code
    vredu_pipes(i).io.vctrl_pipe_in.uop       := io.in.uop    
    vredu_pipes(i).io.vctrl_pipe_in.fire      := io.in.fire   
    vredu_pipes(i).io.vctrl_pipe_in.vs1       := io.in.vs1
    vredu_pipes(i).io.vd_pipe_in              := io.in.vs2
  } else {
    vredu_pipes(i).io.vctrl_pipe_in := vredu_pipes(i - 1).io.vctrl_pipe_out
    vredu_pipes(i).io.vd_pipe_in    := vredu_pipes(i - 1).io.vd_pipe_out
  }
  }

  // final stage for fp32
  // with one reg for accumulating the result
  val vctrl_pipe_stage_last = vredu_pipes(stages-1).io.vctrl_pipe_out
  val vctrl_pipe_fp32       = RegNext(RegNext(vctrl_pipe_stage_last))

  val vd_pipe_stage_last    = vredu_pipes(stages-1).io.vd_pipe_out
  val vfred_pipe_fp32 = Module(new VfredFP32WidenMixedFP16_Wrapped)

  val vfred_pipe6_vs1 = Mux(vctrl_pipe_stage_last.uop.csr.vsew(1, 0) === "b01".U, Cat(Fill(16, 0.U), vctrl_pipe_stage_last.vs1(15, 0)), Mux(vctrl_pipe_stage_last.uop.csr.vsew(1, 0) === "b10".U, vctrl_pipe_stage_last.vs1(31, 0), 0.U))
  val fp32_result = vfred_pipe_fp32.io.fp_result
  val fp32_fflags = vfred_pipe_fp32.io.fflags
  val fp32_uop    = vctrl_pipe_fp32.uop

  vfred_pipe_fp32.io.fire := vctrl_pipe_stage_last.fire
  vfred_pipe_fp32.io.fp_a := vd_pipe_stage_last 
  vfred_pipe_fp32.io.fp_b := Mux(vctrl_pipe_stage_last.uop.uopIdx =/= 0.U, fp32_result, vfred_pipe6_vs1)
  vfred_pipe_fp32.io.is_vec := vctrl_pipe_stage_last.uop.ctrl.vv
  vfred_pipe_fp32.io.round_mode := vctrl_pipe_stage_last.uop.csr.frm
  
  vfred_pipe_fp32.io.fp_format  := vctrl_pipe_stage_last.uop.csr.vsew(1, 0)  
  vfred_pipe_fp32.io.mask := 0.U

  vfred_pipe_fp32.io.op_code    := vctrl_pipe_stage_last.op_code
  vfred_pipe_fp32.io.fp_aIsFpCanonicalNAN := 0.U
  vfred_pipe_fp32.io.fp_bIsFpCanonicalNAN := 0.U

  val vlmul_pipe_fp32_cvt = MuxLookup(vctrl_pipe_fp32.uop.csr.vlmul, "b000".U, Seq(
    "b000".U -> "b000".U,   // LMUL = 1 → 000
    "b001".U -> "b001".U,   // LMUL = 2 → 001
    "b010".U -> "b011".U,   // LMUL = 4 → 011
    "b011".U -> "b111".U    // LMUL = 8 → 111
  ))

  // pipeline for fp16
  // if fp16, need another stage to caculate
  val vctrl_pipe_fp16  = RegNext(RegNext(vctrl_pipe_fp32))

  val vfred_pipe_fp16 = Module(new FloatAdderF16Pipeline_Wrapped)
  val fp16_result = vfred_pipe_fp16.io.fp_c
  val fp16_fflags = vfred_pipe_fp16.io.fflags
  val fp16_uop    = vctrl_pipe_fp16.uop

  vfred_pipe_fp16.io.fire := vctrl_pipe_fp32.fire
  vfred_pipe_fp16.io.fp_a := fp32_result(31,16)
  vfred_pipe_fp16.io.fp_b := fp32_result(15,0)
  vfred_pipe_fp16.io.is_sub := vctrl_pipe_fp32.op_code(0)
  vfred_pipe_fp16.io.round_mode := vctrl_pipe_fp32.uop.csr.frm
  vfred_pipe_fp16.io.mask := 0.U
  vfred_pipe_fp16.io.op_code    := vctrl_pipe_fp32.op_code
  vfred_pipe_fp16.io.fp_aIsFpCanonicalNAN := 0.U
  vfred_pipe_fp16.io.fp_bIsFpCanonicalNAN := 0.U
  vfred_pipe_fp16.io.maskForReduction := 0.U

  val vctrl_pipe_fp16_cvt = MuxLookup(vctrl_pipe_fp16.uop.csr.vlmul, "b000".U, Seq(
    "b000".U -> "b000".U,   // LMUL = 1 → 000
    "b001".U -> "b001".U,   // LMUL = 2 → 001
    "b010".U -> "b011".U,   // LMUL = 4 → 011
    "b011".U -> "b111".U    // LMUL = 8 → 111
  ))

  // arbitrate the result
  val fp32_finish = (vctrl_pipe_fp32.uop.uopIdx === vlmul_pipe_fp32_cvt) && vctrl_pipe_fp32.fire
  val is_fp32 = vctrl_pipe_fp32.uop.csr.vsew(1, 0) === "b10".U
  val fp16_finish = (vctrl_pipe_fp16.uop.uopIdx === vctrl_pipe_fp16_cvt) && vctrl_pipe_fp16.fire
  val is_fp16 = vctrl_pipe_fp16.uop.csr.vsew(1, 0) === "b01".U

  io.out.bits.result := Mux(is_fp32 && fp32_finish, fp32_result, 
  Mux(is_fp16 && fp16_finish, Cat(Fill(16, 0.U), fp16_result), 0.U))

  io.out.bits.fflags := Mux(is_fp32 && fp32_finish, fp32_fflags, 
  Mux(is_fp16 && fp16_finish, fp16_fflags, 0.U))

  io.out.valid := (is_fp32 && fp32_finish) || (is_fp16 && fp16_finish)

  io.out.bits.uop :=  Mux(is_fp32 && fp32_finish, fp32_uop, 
  Mux(is_fp16 && fp16_finish, fp16_uop, 0.U.asTypeOf(new VUop)))

}

// num = VLEN/XLEN/2^i
class VfredFP32WidenMixedFP16_Pipelined(val num: Int = 0) extends Module {
  val io = IO(new Bundle() {
    val vctrl_pipe_in = Input(new Vfredctrl)
    val vctrl_pipe_out = Output(new Vfredctrl)
    val vd_pipe_in = Input(UInt((num*32*2).W))
    val vd_pipe_out = Output(UInt((num*32).W))
  })

  val result_pipe_out  = Wire(Vec(num, UInt(32.W)))
  val fflags_pipe_out  = Wire(Vec(num, UInt(5.W)))

  for (i <- 0 until num) {
      val fp_a = io.vd_pipe_in(31-1+i*64, 0+i*64)
      val fp_b = io.vd_pipe_in(64-1+i*64, 32+i*64)
      val mask = io.vctrl_pipe_in.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_Wrapped)
      vfred.io.fire := io.vctrl_pipe_in.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := io.vctrl_pipe_in.uop.ctrl.vv
      vfred.io.round_mode := io.vctrl_pipe_in.uop.csr.frm
      
      vfred.io.fp_format  := io.vctrl_pipe_in.uop.csr.vsew(1, 0)   
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