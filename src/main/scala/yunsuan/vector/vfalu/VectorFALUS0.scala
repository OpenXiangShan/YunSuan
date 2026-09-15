package yunsuan.vector.vfalu

import chisel3.stage.ChiselGeneratorAnnotation
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu.utils.{FALUAddS0, FALUAddS0ToS1Bundle}
import yunsuan.vector.Common.VSew
import yunsuan.encoding.Opcode.Opcodes.FMacOpcode
//import yunsuan.fpu.fmul.utils
import yunsuan.vector.vfmul.utils.{VFBundle, VFModule, VFAlgoUtils}

class VectorFALUS0ToS1Bundle extends VFBundle {
  val arithS0ToS1Fp16 = Vec(elemNum("fp16"), new FALUAddS0ToS1Bundle(16))
  val arithS0ToS1Fp32 = Vec(elemNum("fp32"), new FALUAddS0ToS1Bundle(32))
  val arithS0ToS1Fp64 = new FALUAddS0ToS1Bundle(64)
  val miscS0ToS1      = new VectorFloatMiscOutput
  val isArithFp16     = Bool()
  val isArithFp32     = Bool()
  val isArithFp64     = Bool()
  val isMisc          = Bool()
}

class VectorFALUS0 extends VFModule {
  val io = IO(new Bundle() {
    val in   = Input(new VectorFALUInput())
    val toS1 = Output(new VectorFALUS0ToS1Bundle())
  })

  implicit val op: UInt = io.in.opcode
  val rm            = io.in.roundMode
  val isSubFromFMUL = io.in.isSubFromVFMul
  val inCtrl        = io.in.inCtrlFromVFMul
  val isFMA         = inCtrl.isFMA

  val fpFmt = FMacOpcode.getDataType
  val isfp64 = fpFmt === VSew.e64
  val isfp32 = fpFmt === VSew.e32
  val isfp16 = fpFmt === VSew.e16

  val fp64A = io.in.fpA
  val fp64B = io.in.fpB
  val fpAAppend64 = io.in.fpAAppend

  val fp32As = VFAlgoUtils.split2Vec(io.in.fpA, 2)
  val fp32Bs = VFAlgoUtils.split2Vec(io.in.fpB, 2)
  val fp32AAppends = io.in.fpAAppends32

  val fp16As = VFAlgoUtils.split2Vec(io.in.fpA, 4)
  val fp16Bs = VFAlgoUtils.split2Vec(io.in.fpB, 4)
  val fp16AAppends = io.in.fpAAppends16

  // op -> ctrl signal: fadd/fsub go to the adder, max/min/sgnj/class all go to the misc unit below
  val isFclass = FMacOpcode.isVfclass
  val isSub    = (isFMA && isSubFromFMUL) || (!isFMA && FMacOpcode.isFsub)
  val isArith  = !isFclass && (isFMA || FMacOpcode.isFadd || FMacOpcode.isFsub)
  val isMisc   = !isArith

  val faddersS0Fp16 = Seq.fill(elemNum("fp16"))(Module(new FALUAddS0(16)))
  faddersS0Fp16.zipWithIndex.foreach { case (fadder, i) =>
    fadder.io.fpA               := fp16As(i)
    fadder.io.fpB               := fp16Bs(i)
    fadder.io.fpAAppend         := fp16AAppends(i)
    fadder.io.inCtrlFromFMUL    := inCtrl.toFMULToFADDCtrlBundle(i)
    fadder.io.fpAisCanonicalNaN := false.B
    fadder.io.fpBisCanonicalNaN := false.B
    fadder.io.rm                := rm
    fadder.io.isSub             := isSub
    io.toS1.arithS0ToS1Fp16(i)  := fadder.io.toS1
  }

  val faddersS0Fp32 = Seq.fill(elemNum("fp32"))(Module(new FALUAddS0(32)))
  faddersS0Fp32.zipWithIndex.foreach { case (fadder, i) =>
    fadder.io.fpA               := fp32As(i)
    fadder.io.fpB               := fp32Bs(i)
    fadder.io.fpAAppend         := fp32AAppends(i)
    fadder.io.inCtrlFromFMUL    := inCtrl.toFMULToFADDCtrlBundle(i)
    fadder.io.fpAisCanonicalNaN := false.B
    fadder.io.fpBisCanonicalNaN := false.B
    fadder.io.rm                := rm
    fadder.io.isSub             := isSub
    io.toS1.arithS0ToS1Fp32(i)  := fadder.io.toS1
  }

  val fadderS0Fp64 = Module(new FALUAddS0(64))
  fadderS0Fp64.io.fpA               := fp64A
  fadderS0Fp64.io.fpB               := fp64B
  fadderS0Fp64.io.fpAAppend         := fpAAppend64
  fadderS0Fp64.io.inCtrlFromFMUL    := inCtrl.toFMULToFADDCtrlBundle(0)
  fadderS0Fp64.io.fpAisCanonicalNaN := false.B
  fadderS0Fp64.io.fpBisCanonicalNaN := false.B
  fadderS0Fp64.io.rm                := rm
  fadderS0Fp64.io.isSub             := isSub
  io.toS1.arithS0ToS1Fp64           := fadderS0Fp64.io.toS1

  val miscS0 = Module(new VectorFloatMisc)
  miscS0.io.in.src0   := io.in.fpA
  miscS0.io.in.src1   := io.in.fpB
  miscS0.io.in.isfp16 := isfp16
  miscS0.io.in.isfp32 := isfp32
  miscS0.io.in.isfp64 := isfp64
  miscS0.io.in.op     := op
  io.toS1.miscS0ToS1  := miscS0.io.out

  /*
    Vector Float-point Reduction Unit
   */
  io.toS1.isArithFp16 := isArith && isfp16
  io.toS1.isArithFp32 := isArith && isfp32
  io.toS1.isArithFp64 := isArith && isfp64
  io.toS1.isMisc      := isMisc
}
