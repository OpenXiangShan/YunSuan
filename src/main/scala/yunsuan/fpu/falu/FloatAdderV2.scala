package yunsuan.fpu.falu

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu._
import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode

class FloatAdderV2Input extends Bundle {
  val fp_fmt               = UInt(2.W)
  val op_code              = FMacOpcode()
  val fp_a, fp_b           = UInt(64.W)
  val fpAAppend            = UInt(53.W)
  val round_mode           = UInt(3.W)
  val inCtrlFromFMUL       = new FMULToFADDCtrlBundle(64)
  val isSubFromFMUL        = Bool()
}

class FloatAdderV2Output extends Bundle {
  val fp_result = Output(UInt(64.W))
  val fflags    = Output(UInt(5.W))
}

class FloatAdderV2IO extends Bundle {
  val fire = Input(Bool())
  val in = Input(new FloatAdderV2Input())
  val out = Output(new FloatAdderV2Output())
}

class FloatAdderV2() extends Module {
  val io = IO(new FloatAdderV2IO())

  val FloatAdderV2S0 = Module(new FloatAdderV2S0)
  FloatAdderV2S0.io.in := io.in
  val FloatAdderV2toS1Reg = RegEnable(FloatAdderV2S0.io.toS1, io.fire)

  val FloatAdderV2S1 = Module(new FloatAdderV2S1)
  FloatAdderV2S1.io.fromS0 := FloatAdderV2toS1Reg
  io.out                   := FloatAdderV2S1.io.out
}
