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

  // tmp contrl signal need to chang to regenable
  // result format b01->fp16,b10->fp32,b11->fp64
  // pipeline 0 
  val result_pipe0  = Wire(Vec((VLEN/XLEN)/2, UInt(XLEN.W)))
  val fflags_pipe0  = Wire(Vec((VLEN/XLEN)/2, UInt(5.W)))
  val vd_pipe0      = Wire(UInt((VLEN/2).W))
  vd_pipe0 := Cat(result_pipe0.reverse)

  val vctrl_pipe0   = RegEnable(io.in, 0.U.asTypeOf(new VfredInput), io.in.fire)

  for (i <- 0 until ((VLEN/XLEN)/2)) {

      val fp_a = io.in.vs1(31-1+i*64, 0+i*64)
      val fp_b = io.in.vs1(64-1+i*64, 32+i*64)
      val mask = io.in.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := io.in.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := io.in.uop.ctrl.vv
      vfred.io.round_mode := io.in.uop.csr.frm
      
      vfred.io.fp_format  := io.in.uop.csr.vsew(1, 0) 
      vfred.io.mask := mask

      vfred.io.op_code    := io.in.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe0(i) := vfred.io.fp_result
      fflags_pipe0(i) := vfred.io.fflags
  }

  // pipeline 1
  val result_pipe1  = Wire(Vec((VLEN/XLEN)/4, UInt(XLEN.W)))
  val fflags_pipe1  = Wire(Vec((VLEN/XLEN)/4, UInt(5.W)))
  val vd_pipe1      = Wire(UInt((VLEN/4).W))
  vd_pipe1 := Cat(result_pipe1.reverse)

  val vctrl_pipe1   = RegEnable(vctrl_pipe0, 0.U.asTypeOf(new VfredInput), vctrl_pipe0.fire)

  for (i <- 0 until ((VLEN/XLEN)/4)) {

      val fp_a = vd_pipe0(31-1+i*64, 0+i*64)
      val fp_b = vd_pipe0(64-1+i*64, 32+i*64)
      val mask = vctrl_pipe0.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := vctrl_pipe0.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := vctrl_pipe0.uop.ctrl.vv
      vfred.io.round_mode := vctrl_pipe0.uop.csr.frm
      
      vfred.io.fp_format  := vctrl_pipe0.uop.csr.vsew(1, 0)  
      vfred.io.mask := mask

      vfred.io.op_code    := vctrl_pipe0.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe1(i) := vfred.io.fp_result
      fflags_pipe1(i) := vfred.io.fflags
  }

  // pipeline 2
  val result_pipe2  = Wire(Vec((VLEN/XLEN)/8, UInt(XLEN.W)))
  val fflags_pipe2  = Wire(Vec((VLEN/XLEN)/8, UInt(5.W)))
  val vd_pipe2      = Wire(UInt((VLEN/8).W))
  vd_pipe2 := Cat(result_pipe2.reverse)

  val vctrl_pipe2   = RegEnable(vctrl_pipe1, 0.U.asTypeOf(new VfredInput), vctrl_pipe1.fire)

  for (i <- 0 until ((VLEN/XLEN)/8)) {

      val fp_a = vd_pipe1(31-1+i*64, 0+i*64)
      val fp_b = vd_pipe1(64-1+i*64, 32+i*64)
      val mask = vctrl_pipe1.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := vctrl_pipe1.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := vctrl_pipe1.uop.ctrl.vv
      vfred.io.round_mode := vctrl_pipe1.uop.csr.frm
      
      vfred.io.fp_format  := vctrl_pipe1.uop.csr.vsew(1, 0)  
      vfred.io.mask := mask

      vfred.io.op_code    := vctrl_pipe1.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe2(i) := vfred.io.fp_result
      fflags_pipe2(i) := vfred.io.fflags
  }

  // pipeline 3
  val result_pipe3  = Wire(Vec((VLEN/XLEN)/16, UInt(XLEN.W)))
  val fflags_pipe3  = Wire(Vec((VLEN/XLEN)/16, UInt(5.W)))
  val vd_pipe3      = Wire(UInt((VLEN/16).W))
  vd_pipe3 := Cat(result_pipe3.reverse)

  val vctrl_pipe3   = RegEnable(vctrl_pipe2, 0.U.asTypeOf(new VfredInput), vctrl_pipe2.fire)

  for (i <- 0 until ((VLEN/XLEN)/16)) {

      val fp_a = vd_pipe2(31-1+i*64, 0+i*64)
      val fp_b = vd_pipe2(64-1+i*64, 32+i*64)
      val mask = vctrl_pipe2.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := vctrl_pipe2.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := vctrl_pipe2.uop.ctrl.vv
      vfred.io.round_mode := vctrl_pipe2.uop.csr.frm
      
      vfred.io.fp_format  := vctrl_pipe2.uop.csr.vsew(1, 0)  
      vfred.io.mask := mask

      vfred.io.op_code    := vctrl_pipe2.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe3(i) := vfred.io.fp_result
      fflags_pipe3(i) := vfred.io.fflags
  }

  // pipeline 4
  val result_pipe4  = Wire(Vec((VLEN/XLEN)/32, UInt(XLEN.W)))
  val fflags_pipe4  = Wire(Vec((VLEN/XLEN)/32, UInt(5.W)))
  val vd_pipe4      = Wire(UInt((VLEN/32).W))
  vd_pipe4 := Cat(result_pipe4.reverse)

  val vctrl_pipe4   = RegEnable(vctrl_pipe3, 0.U.asTypeOf(new VfredInput), vctrl_pipe3.fire)

  for (i <- 0 until ((VLEN/XLEN)/32)) {

      val fp_a = vd_pipe3(31-1+i*64, 0+i*64)
      val fp_b = vd_pipe3(64-1+i*64, 32+i*64)
      val mask = vctrl_pipe3.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := vctrl_pipe3.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := vctrl_pipe3.uop.ctrl.vv
      vfred.io.round_mode := vctrl_pipe3.uop.csr.frm
      
      vfred.io.fp_format  := vctrl_pipe3.uop.csr.vsew(1, 0)  
      vfred.io.mask := mask

      vfred.io.op_code    := vctrl_pipe3.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe4(i) := vfred.io.fp_result
      fflags_pipe4(i) := vfred.io.fflags
  }

  // pipeline 5
  // vlen = 2048
  // vlen/32 = 64 = 2^6

  val result_pipe5  = Wire(Vec((VLEN/XLEN)/64, UInt(XLEN.W)))
  val fflags_pipe5  = Wire(Vec((VLEN/XLEN)/64, UInt(5.W)))
  val vd_pipe5      = Wire(UInt((VLEN/64).W))
  vd_pipe5 := Cat(result_pipe5.reverse)

  val vctrl_pipe5   = RegEnable(vctrl_pipe4, 0.U.asTypeOf(new VfredInput), vctrl_pipe4.fire)

  for (i <- 0 until ((VLEN/XLEN)/64)) {

      val fp_a = vd_pipe4(31-1+i*64, 0+i*64)
      val fp_b = vd_pipe4(64-1+i*64, 32+i*64)
      val mask = vctrl_pipe4.mask(i*4+3, i*4)
      val vfred = Module(new VfredFP32WidenMixedFP16_SingleUnit)
      vfred.io.fire := vctrl_pipe4.fire
      vfred.io.fp_a := fp_a
      vfred.io.fp_b := fp_b
      vfred.io.is_vec := vctrl_pipe4.uop.ctrl.vv
      vfred.io.round_mode := vctrl_pipe4.uop.csr.frm
      
      
      vfred.io.fp_format  := vctrl_pipe4.uop.csr.vsew(1, 0)  
      vfred.io.mask := mask

      vfred.io.op_code    := vctrl_pipe4.op_code
      vfred.io.fp_aIsFpCanonicalNAN := 0.U
      vfred.io.fp_bIsFpCanonicalNAN := 0.U

      result_pipe5(i) := vfred.io.fp_result
      fflags_pipe5(i) := vfred.io.fflags
  }


  // pipeline 6
  // final stage
  // with one reg for accumulating the result
  val vctrl_pipe6       = RegEnable(vctrl_pipe5, 0.U.asTypeOf(new VfredInput), vctrl_pipe5.fire)

  val vfred_pipe6 = Module(new VfredFP32WidenMixedFP16_SingleUnit)
  val vfred_pipe6_vs2 = Mux(vctrl_pipe5.uop.csr.vsew(1, 0) === "b01".U, Cat(Fill(16, 0.U), vctrl_pipe5.vs2(15, 0)), Mux(vctrl_pipe5.uop.csr.vsew(1, 0) === "b10".U, vctrl_pipe5.vs2(31, 0), 0.U))
  val fp32_result = vfred_pipe6.io.fp_result
  val fp32_fflags = vfred_pipe6.io.fflags
  val fp32_uop    = vctrl_pipe6.uop

  vfred_pipe6.io.fire := vctrl_pipe5.fire
  vfred_pipe6.io.fp_a := Mux(vctrl_pipe5.uop.uopIdx =/= 0.U, fp32_result, vfred_pipe6_vs2)
  vfred_pipe6.io.fp_b := vd_pipe5
  vfred_pipe6.io.is_vec := vctrl_pipe5.uop.ctrl.vv
  vfred_pipe6.io.round_mode := vctrl_pipe5.uop.csr.frm
  
  vfred_pipe6.io.fp_format  := vctrl_pipe5.uop.csr.vsew(1, 0)  
  vfred_pipe6.io.mask := 0.U

  vfred_pipe6.io.op_code    := vctrl_pipe5.op_code
  vfred_pipe6.io.fp_aIsFpCanonicalNAN := 0.U
  vfred_pipe6.io.fp_bIsFpCanonicalNAN := 0.U


  val vlmul_pipe6_cvt = MuxLookup(vctrl_pipe6.uop.csr.vlmul, "b000".U, Seq(
    "b000".U -> "b000".U,   // LMUL = 1 → 000
    "b001".U -> "b001".U,   // LMUL = 2 → 001
    "b010".U -> "b011".U,   // LMUL = 4 → 011
    "b011".U -> "b111".U    // LMUL = 8 → 111
  ))

  // pipeline 7
  // if fp16, need another stage to caculate
  val vctrl_pipe7_fp16  = RegEnable(vctrl_pipe6, 0.U.asTypeOf(new VfredInput), vctrl_pipe6.fire)

  val vfred_pipe7_fp16 = Module(new FloatAdderF16Pipeline)
  val fp16_result = vfred_pipe7_fp16.io.fp_c
  val fp16_fflags = vfred_pipe7_fp16.io.fflags
  val fp16_uop    = vctrl_pipe7_fp16.uop

  vfred_pipe7_fp16.io.fire := vctrl_pipe6.fire
  vfred_pipe7_fp16.io.fp_a := fp32_result(31,16)
  vfred_pipe7_fp16.io.fp_b := fp32_result(15,0)
  vfred_pipe7_fp16.io.is_sub := vctrl_pipe6.op_code(0)
  vfred_pipe7_fp16.io.round_mode := vctrl_pipe6.uop.csr.frm
  vfred_pipe7_fp16.io.mask := 0.U
  vfred_pipe7_fp16.io.op_code    := vctrl_pipe6.op_code
  vfred_pipe7_fp16.io.fp_aIsFpCanonicalNAN := 0.U
  vfred_pipe7_fp16.io.fp_bIsFpCanonicalNAN := 0.U
  vfred_pipe7_fp16.io.maskForReduction := 0.U

  val vctrl_pipe7_fp16_cvt = MuxLookup(vctrl_pipe7_fp16.uop.csr.vlmul, "b000".U, Seq(
    "b000".U -> "b000".U,   // LMUL = 1 → 000
    "b001".U -> "b001".U,   // LMUL = 2 → 001
    "b010".U -> "b011".U,   // LMUL = 4 → 011
    "b011".U -> "b111".U    // LMUL = 8 → 111
  ))

  // arbitrate the result
  val fp32_finish = (vctrl_pipe6.uop.uopIdx === vlmul_pipe6_cvt) & vctrl_pipe6.fire
  val is_fp32 = vctrl_pipe6.uop.csr.vsew(1, 0) === "b10".U
  val fp16_finish = (vctrl_pipe7_fp16.uop.uopIdx === vctrl_pipe7_fp16_cvt) & vctrl_pipe7_fp16.fire
  val is_fp16 = vctrl_pipe7_fp16.uop.csr.vsew(1, 0) === "b01".U

  io.out.bits.result := Mux(is_fp32 & fp32_finish, fp32_result, 
  Mux(is_fp16 & fp16_finish, Cat(Fill(16, 0.U), fp16_result), 0.U))

  io.out.bits.fflags := Mux(is_fp32 & fp32_finish, fp32_fflags, 
  Mux(is_fp16 & fp16_finish, fp16_fflags, 0.U))

  io.out.valid := (is_fp32 & fp32_finish) | (is_fp16 & fp16_finish)

  io.out.bits.uop :=  Mux(is_fp32 & fp32_finish, fp32_uop, 
  Mux(is_fp16 & fp16_finish, fp16_uop, 0.U.asTypeOf(new VUop)))

}


class VfredFP32WidenMixedFP16_SingleUnit() extends Module {
    val exponentWidth = 8
    val significandWidth = 24
    val floatWidth = exponentWidth + significandWidth
    val io = IO(new Bundle() {
    val fire          = Input (Bool())
    val fp_a, fp_b    = Input (UInt(floatWidth.W)) // fp_a -> vs2, fp_b -> vs1

    val mask          = Input (UInt(4.W))
    val is_vec        = Input (Bool())
    val round_mode    = Input (UInt(3.W))
    val fp_format     = Input (UInt(2.W)) // result format b01->fp16,b10->fp32,b11->fp64

    val op_code       = Input (UInt(5.W)) // TODO: op_code width
    val fp_aIsFpCanonicalNAN = Input (Bool())
    val fp_bIsFpCanonicalNAN = Input (Bool())

    val fp_result     = Output(UInt(floatWidth.W))
    val fflags        = Output(UInt(5.W))
  })

  val fire = io.fire
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

  io.fp_result := Mux1H(
    Seq(
      res_is_f16,
      res_is_f32
    ),
    Seq(
      fp_f16_result,
      fp_f32_result
    )
  )

  io.fflags := Mux(res_is_f32, U_F32_fflags, (U_F16_1_fflags | U_F16_0_fflags))
}