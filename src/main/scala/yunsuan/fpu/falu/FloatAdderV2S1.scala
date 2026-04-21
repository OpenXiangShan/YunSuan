package yunsuan.fpu.falu

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu.FloatAdderV2Output
import yunsuan.fpu.falu.utils._

class FloatAdderV2S1Output extends Bundle {
  val fp_result = Output(UInt(64.W))
  val fflags    = Output(UInt(5.W))
}

class FloatAdderV2S1 extends Module {
  val io = IO(new Bundle() {
    val fromS0 = Input(new FloatAdderV2S0ToS1Bundle())
    val out   = Output(new FloatAdderV2S1Output())
  })

  val FADD16S1 = Module(new FALUAddS1(16))
  FADD16S1.io.fromS0 := io.fromS0.FADD16S0ToS1
  val outResF16                = Cat(Fill(48, 1.U(1.W)), FADD16S1.io.outRes)
  val outFlagsF16              = FADD16S1.io.outFlags
  
  val FADD32S1 = Module(new FALUAddS1(32))
  FADD32S1.io.fromS0 := io.fromS0.FADD32S0ToS1
  val outResF32                = Cat(Fill(32, 1.U(1.W)), FADD32S1.io.outRes)
  val outFlagsF32              = FADD32S1.io.outFlags

  val FADD64S1 = Module(new FALUAddS1(64))
  FADD64S1.io.fromS0 := io.fromS0.FADD64S0ToS1
  val outResF64                = FADD64S1.io.outRes
  val outFlagsF64              = FADD64S1.io.outFlags

  val outResMisc      = io.fromS0.FALUMiscS0ToS1.result
  val outFlagsMisc    = io.fromS0.FALUMiscS0ToS1.fflags

  io.out.fp_result := Mux1H(Seq(
    io.fromS0.isArithF16 -> outResF16,
    io.fromS0.isArithF32 -> outResF32,
    io.fromS0.isArithF64 -> outResF64,
    io.fromS0.isMisc     -> outResMisc
  ))
  io.out.fflags := Mux1H(Seq(
    io.fromS0.isArithF16 -> outFlagsF16,
    io.fromS0.isArithF32 -> outFlagsF32,
    io.fromS0.isArithF64 -> outFlagsF64,
    io.fromS0.isMisc     -> outFlagsMisc
  ))
}
