package yunsuan.vector.vfalu

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu
//import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode
import yunsuan.vector.vfmul.utils._
import yunsuan.vector.vfmul.VFMul2VFALUCtrlBundle

class VectorFALUInput extends VFBundle {
  val opcode          = FMacOpcode()
  val fpA             = UInt(fpMaxWidth.W)
  val fpB             = UInt(fpMaxWidth.W)
  val fpAAppend       = UInt(fpAAppendMaxWidth.W)
  val roundMode       = UInt(rmWidth.W)
  val inCtrlFromVFMul = new VFMul2VFALUCtrlBundle
  val isSubFromVFMul  = Bool()

  def fpAAppends32: Vec[UInt] = VFAlgoUtils.split2VecManual(fpAAppend, fpMaxWidth / floatWidth("fp32"), fpAAppendWidth("fp32"))
  def fpAAppends16: Vec[UInt] = VFAlgoUtils.split2VecManual(fpAAppend, fpMaxWidth / floatWidth("fp16"), fpAAppendWidth("fp16"))
}

class VectorFALUOutput extends VFBundle {
  val fpResult = Output(UInt(fpMaxWidth.W))
  val fflagsVec = Output(Vec(fpMaxWidth / byte, UInt(flagsWidth.W)))
}

class VectorFALUIO extends VFBundle {
  val fire = Input(Bool())
  val in = Input(new VectorFALUInput())
  val out = Output(new VectorFALUOutput())
}

class VectorFALU extends VFModule {
  val io = IO(new VectorFALUIO())

  val VectorFALUS0 = Module(new VectorFALUS0)
  VectorFALUS0.io.in := io.in
  val VectorFALUtoS1Reg = RegEnable(VectorFALUS0.io.toS1, io.fire)

  val VectorFALUS1 = Module(new VectorFALUS1)
  VectorFALUS1.io.fromS0 := VectorFALUtoS1Reg
  io.out                 := VectorFALUS1.io.out
}
