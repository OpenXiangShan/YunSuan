package yunsuan.fpu.fmul

import chisel3._
import chisel3.util._
import yunsuan.fpu.fmul.FloatMulInput
import yunsuan.fpu.fmul.utils._
import yunsuan.vector.Common.VSew

class FloatMULS0ToS1Bundle extends Bundle {
  val FMUL16S0ToS1 = new FMULS0ToS1Bundle(16)
  val FMUL32S0ToS1 = new FMULS0ToS1Bundle(32)
  val FMUL64S0ToS1 = new FMULS0ToS1Bundle(64)
  val isF16        = Bool()
  val isF32        = Bool()
  val isF64        = Bool()
}

class FloatMULS0 extends Module{
  val io = IO(new Bundle() {
    val in = Input(new FloatMulInput)
    val outToS1 = Output(new FloatMULS0ToS1Bundle)
  })

  val isFMUL = io.in.isFMUL
  val isFMA  = !isFMUL
  val isNeg  = io.in.isNeg

  val isfp64   = io.in.fp_fmt === VSew.e64
  val isfp32   = io.in.fp_fmt === VSew.e32
  val isfp16   = io.in.fp_fmt === VSew.e16

  val fp64A   = io.in.fp_a
  val fp64AIn = Mux(isNeg, Cat(~fp64A.head(1), fp64A.tail(1)), fp64A)
  val fp64BIn = io.in.fp_b

  val fp32A   = io.in.fp_a.tail(32)
  val fp32AIn = Mux(isNeg, Cat(~fp32A.head(1), fp32A.tail(1)), fp32A)
  val fp32BIn = io.in.fp_b.tail(32)
  
  val fp16A = io.in.fp_a.tail(48)
  val fp16AIn = Mux(isNeg, Cat(~fp16A.head(1), fp16A.tail(1)), fp16A)
  val fp16BIn = io.in.fp_b.tail(48)

  val fp64AIsCanonicalNaN = false.B
  val fp64BIsCanonicalNaN = false.B
  val fp32AIsCanonicalNaN = isfp32 && !io.in.fp_a.head(32).andR
  val fp32BIsCanonicalNaN = isfp32 && !io.in.fp_b.head(32).andR
  val fp16AIsCanonicalNaN = isfp16 && !io.in.fp_a.head(48).andR
  val fp16BIsCanonicalNaN = isfp16 && !io.in.fp_b.head(48).andR

  val FMUL16S0 = Module(new FMULS0(16))
  FMUL16S0.io.isFMA             := isFMA
  FMUL16S0.io.fp_a              := fp16AIn
  FMUL16S0.io.fp_b              := fp16BIn
  FMUL16S0.io.fpAisCanonicalNaN := fp16AIsCanonicalNaN
  FMUL16S0.io.fpBisCanonicalNaN := fp16BIsCanonicalNaN
  FMUL16S0.io.rm                := io.in.round_mode
  io.outToS1.FMUL16S0ToS1 := FMUL16S0.io.outToS1

  val FMUL32S0 = Module(new FMULS0(32))
  FMUL32S0.io.isFMA             := isFMA
  FMUL32S0.io.fp_a              := fp32AIn
  FMUL32S0.io.fp_b              := fp32BIn
  FMUL32S0.io.fpAisCanonicalNaN := fp32AIsCanonicalNaN
  FMUL32S0.io.fpBisCanonicalNaN := fp32BIsCanonicalNaN
  FMUL32S0.io.rm                := io.in.round_mode
  io.outToS1.FMUL32S0ToS1 := FMUL32S0.io.outToS1

  val FMUL64S0 = Module(new FMULS0(64))
  FMUL64S0.io.isFMA             := isFMA
  FMUL64S0.io.fp_a              := fp64AIn
  FMUL64S0.io.fp_b              := fp64BIn
  FMUL64S0.io.fpAisCanonicalNaN := fp64AIsCanonicalNaN
  FMUL64S0.io.fpBisCanonicalNaN := fp64BIsCanonicalNaN
  FMUL64S0.io.rm                := io.in.round_mode
  io.outToS1.FMUL64S0ToS1 := FMUL64S0.io.outToS1

  io.outToS1.isF16 := isfp16
  io.outToS1.isF32 := isfp32
  io.outToS1.isF64 := isfp64
}