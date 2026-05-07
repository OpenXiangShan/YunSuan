package yunsuan.fpu.fmul

import chisel3._
import chisel3.util._
import yunsuan.util.GatedValidRegNext
import yunsuan.fpu.fmul._
import yunsuan.fpu.fmul.utils._
import yunsuan.vector.Common.VSew


class FloatMulInput extends Bundle{
  val isFMUL               = Bool()
  val isNeg                = Bool()
  val fp_fmt               = UInt(2.W)
  val fp_a, fp_b           = UInt(64.W)
  val round_mode           = UInt(3.W)
}

class FloatMULtoFloatAdderOutput extends Bundle{
  val FMULToFADDCtrl = new FMULToFADDCtrlBundle(64)
  val fpA = UInt(64.W)
  val fpAAppend = UInt(53.W)
}

class FloatMulOutput extends Bundle{
  val fp_result  = UInt(64.W)
  val fflags     = UInt(5.W)
}

class FloatMulIO extends Bundle{
  val fire      = Input(Bool())
  val in        = Input(new FloatMulInput())
  val outToFADD = Output(new FloatMULtoFloatAdderOutput())
  val out       = Output(new FloatMulOutput())
}

class FloatMUL() extends Module{
  val io = IO(new FloatMulIO())

  val fire   = io.fire
  val isFMUL = io.in.isFMUL
  val isNeg  = io.in.isNeg
  
  val fireS1       = GatedValidRegNext(fire)
  val isFMULS1     = RegEnable(isFMUL, fire)
  val isFMULFireS1 = fireS1 && isFMULS1

  val FloatMULS0 = Module(new FloatMULS0)
  FloatMULS0.io.in := io.in
  val FloatMULtoS1Reg = RegEnable(FloatMULS0.io.outToS1, fire)

  val FloatMULS1 = Module(new FloatMULS1)
  FloatMULS1.io.inFromS0 := FloatMULtoS1Reg
  io.outToFADD := FloatMULS1.io.outToFADD
  val FloatMULtoS2Reg = RegEnable(FloatMULS1.io.outToS2, isFMULFireS1)

  val FloatMULS2 = Module(new FloatMULS2)
  FloatMULS2.io.inFromS1 := FloatMULtoS2Reg
  io.out := FloatMULS2.io.out
}