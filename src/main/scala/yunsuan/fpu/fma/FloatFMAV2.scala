package yunsuan.fpu

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode
import yunsuan.fpu.falu.FloatAdderV2
import yunsuan.fpu.fmul.FloatMUL
import yunsuan.util.GatedValidRegNext

class FloatFMAV2Input extends Bundle {
  val fire                 = Bool()
  val fp_fmt               = UInt(2.W)
  val op_code              = UInt(4.W)
  val fp_a, fp_b, fp_c     = UInt(64.W)
  val round_mode           = UInt(3.W)
}

class FloatFMAV2Output extends Bundle {
  val fp_result = Output(UInt(64.W))
  val fflags    = Output(UInt(5.W))
}

class FloatFMAV2IO extends Bundle {
  val in = Input(new FloatFMAV2Input())
  val out = Output(new FloatFMAV2Output())
}

class FloatFMAV2() extends Module {
  val io = IO(new FloatFMAV2IO())

  val fire = io.in.fire
  val fireS1 = GatedValidRegNext(fire)
  val fireS2 = GatedValidRegNext(fireS1)
  val opcode = io.in.op_code

  val isNeg = FMacOpcode.isFnmacc(opcode) || FMacOpcode.isFnmsac(opcode)
  val isSub = FMacOpcode.isFnmacc(opcode) || FMacOpcode.isFmsac(opcode)

  val fpFmtS2 = RegEnable(RegEnable(io.in.fp_fmt, fire), fireS1)
  val rmS2    = RegEnable(RegEnable(io.in.round_mode, fire), fireS1)
  val fpCS2   = RegEnable(RegEnable(io.in.fp_c, fire), fireS1)
  val isSubS2 = RegEnable(RegEnable(isSub, fire), fireS1)

  val fmul = Module(new FloatMUL)
  fmul.io.fire       := fire
  fmul.io.in.isFMUL     := false.B
  fmul.io.in.isNeg      := isNeg
  fmul.io.in.fp_fmt     := io.in.fp_fmt
  fmul.io.in.fp_a       := io.in.fp_a
  fmul.io.in.fp_b       := io.in.fp_b
  fmul.io.in.round_mode := io.in.round_mode

  val fpAS2 = RegEnable(fmul.io.outToFADD.fpA, fireS1)
  val fpAAppendS2 = RegEnable(fmul.io.outToFADD.fpAAppend, fireS1)
  val fmulCtrlS2 = RegEnable(fmul.io.outToFADD.FMULToFADDCtrl, fireS1)

  val fadd = Module(new FloatAdderV2)
  fadd.io.fire := fireS2
  fadd.io.in.fp_fmt := fpFmtS2
  // dirty code
  fadd.io.in.op_code := 0.U
  fadd.io.in.fp_a := fpAS2
  fadd.io.in.fp_b := fpCS2
  fadd.io.in.fpAAppend := fpAAppendS2
  fadd.io.in.round_mode := rmS2
  fadd.io.in.inCtrlFromFMUL := fmulCtrlS2
  fadd.io.in.isSubFromFMUL := isSubS2

  io.out.fp_result := fadd.io.out.fp_result
  io.out.fflags := fadd.io.out.fflags
}
