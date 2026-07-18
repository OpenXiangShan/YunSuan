package yunsuan.vector.vfmul

import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.FloatMulInput
//import yunsuan.fpu.fmul.utils._
import yunsuan.fpu.fmul.utils.{FMULS0ToS1Bundle, FMULS0}
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule, VFAlgoUtils}
import yunsuan.vector.Common.VSew

class VFloatMULS0ToS1Bundle extends VFBundle {
  val Vfmul_s0_16ToS1 = Vec(elemNum("fp16"), new FMULS0ToS1Bundle(16))
  val Vfmul_s0_32ToS1 = Vec(elemNum("fp32"), new FMULS0ToS1Bundle(32))
  val Vfmul_s0_64ToS1 = new FMULS0ToS1Bundle(64)
  val isF16        = Bool()
  val isF32        = Bool()
  val isF64        = Bool()
}

class VectorFMULS0 extends VFModule {
  val io = IO(new Bundle() {
    val in = Input(new VectorFMulInput)
    val outToS1 = Output(new VFloatMULS0ToS1Bundle)
  })

  val isFMUL = io.in.isFMUL
  val isFMA  = !isFMUL
  val isNeg  = io.in.isNeg

  val isfp64   = io.in.fp_fmt === VSew.e64
  val isfp32   = io.in.fp_fmt === VSew.e32
  val isfp16   = io.in.fp_fmt === VSew.e16

  val fp64A   = io.in.fp_a
  val fp64AIn = Mux(isNeg, VFAlgoUtils.fpNeg(fp64A), fp64A)
  val fp64BIn = io.in.fp_b

  val fp32As  = VFAlgoUtils.split2Vec(io.in.fp_a, 2)
  val fp32AsIn = Mux(isNeg, VecInit(fp32As.map(VFAlgoUtils.fpNeg(_))), fp32As)
  val fp32BsIn = VFAlgoUtils.split2Vec(io.in.fp_b, 2)

  val fp16As  = VFAlgoUtils.split2Vec(io.in.fp_a, 4)
  val fp16AsIn = Mux(isNeg, VecInit(fp16As.map(VFAlgoUtils.fpNeg(_))), fp16As)
  val fp16BsIn = VFAlgoUtils.split2Vec(io.in.fp_b, 4)

  val fmulsS0Fp16 = Seq.fill(elemNum("fp16"))(Module(new FMULS0(16)))
  fmulsS0Fp16.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.isFMA                 := isFMA
    fmul.io.fp_a                  := fp16AsIn(i)
    fmul.io.fp_b                  := fp16BsIn(i)
    fmul.io.fpAisCanonicalNaN     := false.B
    fmul.io.fpBisCanonicalNaN     := false.B
    fmul.io.rm                    := io.in.round_mode
    io.outToS1.Vfmul_s0_16ToS1(i) := fmul.io.outToS1
  }

  val fmulsS0Fp32 = Seq.fill(elemNum("fp32"))(Module(new FMULS0(32)))
  fmulsS0Fp32.zipWithIndex.foreach { case (fmul, i) =>
    fmul.io.isFMA                 := isFMA
    fmul.io.fp_a                  := fp32AsIn(i)
    fmul.io.fp_b                  := fp32BsIn(i)
    fmul.io.fpAisCanonicalNaN     := false.B
    fmul.io.fpBisCanonicalNaN     := false.B
    fmul.io.rm                    := io.in.round_mode
    io.outToS1.Vfmul_s0_32ToS1(i) := fmul.io.outToS1
  }

  val fmulS0Fp64 = Module(new FMULS0(64))
  fmulS0Fp64.io.isFMA             := isFMA
  fmulS0Fp64.io.fp_a              := fp64AIn
  fmulS0Fp64.io.fp_b              := fp64BIn
  fmulS0Fp64.io.fpAisCanonicalNaN := false.B
  fmulS0Fp64.io.fpBisCanonicalNaN := false.B
  fmulS0Fp64.io.rm                := io.in.round_mode
  io.outToS1.Vfmul_s0_64ToS1      := fmulS0Fp64.io.outToS1

  io.outToS1.isF16 := isfp16
  io.outToS1.isF32 := isfp32
  io.outToS1.isF64 := isfp64
}