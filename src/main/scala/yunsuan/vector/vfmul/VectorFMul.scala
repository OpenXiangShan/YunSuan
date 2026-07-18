package yunsuan.vector.vfmul

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.VFMacOpcode
import yunsuan.util.GatedValidRegNext
import yunsuan.vector.vfmul.utils.VFAlgoUtils
//import yunsuan.fpu.fmul.utils._
import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule}


class VectorFMulInput extends VFBundle  {
  val isFMUL      = Bool()
  val isNeg       = Bool()
  val fp_fmt      = UInt(2.W)
  val fp_a        = UInt(fpMaxWidth.W)
  val fp_b        = UInt(fpMaxWidth.W)
  val round_mode  = UInt(rmWidth.W)
}

class VFMul2VFALUCtrlBundle extends VFBundle {
  val isFMA = Bool()
  val rm = UInt(rmWidth.W)
  val sticky    = Vec(fpMaxWidth / floatWidth("fp16"), Bool())
  val resIsNaN  = Vec(fpMaxWidth / floatWidth("fp16"), Bool())
  val resIsZero = Vec(fpMaxWidth / floatWidth("fp16"), Bool())
  val resIsInf  = Vec(fpMaxWidth / floatWidth("fp16"), Bool())
  val flagsNV   = Vec(fpMaxWidth / floatWidth("fp16"), Bool())
  val flagsOF   = Vec(fpMaxWidth / floatWidth("fp16"), Bool())

  def toFMULToFADDCtrlBundle(idx: Int): FMULToFADDCtrlBundle = {
    val ctrl = Wire(new FMULToFADDCtrlBundle(fpMaxWidth))
    ctrl.isFMA := isFMA
    ctrl.rm := rm
    ctrl.sticky := sticky(idx)
    ctrl.resIsNaN := resIsNaN(idx)
    ctrl.resIsZero := resIsZero(idx)
    ctrl.resIsInf := resIsInf(idx)
    ctrl.flagsNV := flagsNV(idx)
    ctrl.flagsOF := flagsOF(idx)
    ctrl
  }
}

class VFMul2VFALUOutput extends VFBundle {
  val FMULToFADDCtrl = new VFMul2VFALUCtrlBundle
  val fpA = UInt(fpMaxWidth.W)
  val fpAAppend = UInt(53.W)
}

class VectorFMulOutput extends VFBundle {
  val fpResult  = UInt(fpMaxWidth.W)
  val fflagsVec = Vec(fpMaxWidth / byte, UInt(flagsWidth.W))
}

class VectorFMulIO extends VFBundle {
  val fire      = Input(Bool())
  val in        = Input(new VectorFMulInput)
  val outToFADD = Output(new VFMul2VFALUOutput)
  val out       = Output(new VectorFMulOutput)
}

class VectorFMUL extends Module {
  val io = IO(new VectorFMulIO())

  val fire   = io.fire
  val isFMUL = io.in.isFMUL
  val isNeg  = io.in.isNeg

  val fireS1       = GatedValidRegNext(fire)
  val isFMUL_s1     = RegEnable(isFMUL, fire)
  val isFMULFire_s1 = fireS1 && isFMUL_s1

  val fmuls_s0 = Module(new VectorFMULS0)
  fmuls_s0.io.in := io.in
  val fmulsToS1Reg = RegEnable(fmuls_s0.io.outToS1, fire)

  val fmuls_s1 = Module(new VectorFMULS1)
  fmuls_s1.io.inFromS0 := fmulsToS1Reg
  io.outToFADD := fmuls_s1.io.outToFADD
  val fmulsToS2Reg = RegEnable(fmuls_s1.io.outToS2, isFMULFire_s1)

  val fmuls_s2 = Module(new VectorFMULS2)
  fmuls_s2.io.inFromS1 := fmulsToS2Reg
  io.out := fmuls_s2.io.out
}