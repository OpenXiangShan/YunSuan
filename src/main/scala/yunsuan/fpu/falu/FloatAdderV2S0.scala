package yunsuan.fpu.falu

import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.fpu.falu.FloatAdderV2Input
import yunsuan.fpu.falu.utils._
import yunsuan.vector.alu.VSew
import yunsuan.FaddOpCode
import yunsuan.fpu.fmul.utils.FMULToFADDCtrlBundle

class FloatAdderV2S0ToS1Bundle extends Bundle {
  val FADD16S0ToS1   = new FALUAddS0ToS1Bundle(16)
  val FADD32S0ToS1   = new FALUAddS0ToS1Bundle(32)
  val FADD64S0ToS1   = new FALUAddS0ToS1Bundle(64)
  val FALUMiscS0ToS1 = new FALUMiscOutput
  val isArithF16     = Bool()
  val isArithF32     = Bool()
  val isArithF64     = Bool()
  val isMisc         = Bool()
}

class FloatAdderV2S0 extends Module {
  val io = IO(new Bundle() {
    val in   = Input(new FloatAdderV2Input())
    val toS1 = Output(new FloatAdderV2S0ToS1Bundle())
  })

  val opcode        = io.in.op_code
  val rm            = io.in.round_mode
  val isSubFromFMUL = io.in.isSubFromFMUL
  val inCtrl        = io.in.inCtrlFromFMUL
  val isFMA         = inCtrl.isFMA
  
  val isfp64   = io.in.fp_fmt === VSew.e64
  val isfp32   = io.in.fp_fmt === VSew.e32
  val isfp16   = io.in.fp_fmt === VSew.e16
  
  val fp32A       = io.in.fp_a.tail(32)
  val fp32AAppend = io.in.fpAAppend.tail(29)
  val fp32B       = io.in.fp_b.tail(32)
  
  val fp16A       = io.in.fp_a.tail(48)
  val fp16AAppend = io.in.fpAAppend.tail(42)
  val fp16B       = io.in.fp_b.tail(48)

  val fp64AIsCanonicalNaN = false.B
  val fp64BIsCanonicalNaN = false.B
  val fp32AIsCanonicalNaN = isfp32 && !io.in.fp_a.head(32).andR
  val fp32BIsCanonicalNaN = isfp32 && !io.in.fp_b.head(32).andR
  val fp16AIsCanonicalNaN = isfp16 && !io.in.fp_a.head(48).andR
  val fp16BIsCanonicalNaN = isfp16 && !io.in.fp_b.head(48).andR

  // opcode -> ctrl signal: fadd/fsub
  val isFaddOpcode  = opcode === FaddOpCode.fadd
  val isFsubOpcode  = opcode === FaddOpCode.fsub
  val isSub         = (isFMA && isSubFromFMUL) || (~isFMA && isFsubOpcode)
  val isArith       = isFMA || (~isFMA && (isFaddOpcode || isFsubOpcode))
  val isArithF16    = isArith && isfp16
  val isArithF32    = isArith && isfp32
  val isArithF64    = isArith && isfp64
  
  // opcode -> ctrl signal: misc
  val isFmaxOpcode   = opcode === FaddOpCode.fmax
  val isFminOpcode   = opcode === FaddOpCode.fmin
  val isFmaxmOpcode  = opcode === FaddOpCode.fmaxm
  val isFminmOpcode  = opcode === FaddOpCode.fminm
  val isFmax         = ~isFMA && (isFmaxOpcode  || isFmaxmOpcode)
  val isFmin         = ~isFMA && (isFminOpcode  || isFminmOpcode)
  val isMaxMinM      = ~isFMA && (isFmaxmOpcode || isFminmOpcode)
  val isFsgnj        = opcode === FaddOpCode.fsgnj
  val isFsgnjn       = opcode === FaddOpCode.fsgnjn
  val isFsgnjx       = opcode === FaddOpCode.fsgnjx
  val isFsgn         = isFsgnj || isFsgnjn || isFsgnjx
  val isMisc         = ~isArith

  val FADD16S0 = Module(new FALUAddS0(16))
  FADD16S0.io.fpA               := fp16A
  FADD16S0.io.fpB               := fp16B
  FADD16S0.io.fpAAppend         := fp16AAppend
  FADD16S0.io.inCtrlFromFMUL    := inCtrl
  FADD16S0.io.fpAisCanonicalNaN := fp16AIsCanonicalNaN
  FADD16S0.io.fpBisCanonicalNaN := fp16BIsCanonicalNaN
  FADD16S0.io.rm                := rm
  FADD16S0.io.isSub             := isSub
  io.toS1.FADD16S0ToS1          := FADD16S0.io.toS1

  val FADD32S0 = Module(new FALUAddS0(32))
  FADD32S0.io.fpA               := fp32A
  FADD32S0.io.fpB               := fp32B
  FADD32S0.io.fpAAppend         := fp32AAppend
  FADD32S0.io.inCtrlFromFMUL    := inCtrl
  FADD32S0.io.fpAisCanonicalNaN := fp32AIsCanonicalNaN
  FADD32S0.io.fpBisCanonicalNaN := fp32BIsCanonicalNaN
  FADD32S0.io.rm                := rm
  FADD32S0.io.isSub             := isSub
  io.toS1.FADD32S0ToS1          := FADD32S0.io.toS1

  val FADD64S0 = Module(new FALUAddS0(64))
  FADD64S0.io.fpA               := io.in.fp_a
  FADD64S0.io.fpB               := io.in.fp_b
  FADD64S0.io.fpAAppend         := io.in.fpAAppend
  FADD64S0.io.inCtrlFromFMUL    := inCtrl
  FADD64S0.io.fpAisCanonicalNaN := fp64AIsCanonicalNaN
  FADD64S0.io.fpBisCanonicalNaN := fp64BIsCanonicalNaN
  FADD64S0.io.rm                := rm
  FADD64S0.io.isSub             := isSub
  io.toS1.FADD64S0ToS1          := FADD64S0.io.toS1

  val FALUMiscS0 = Module(new FALUMisc)
  FALUMiscS0.io.in.src0                  := io.in.fp_a
  FALUMiscS0.io.in.src1                  := io.in.fp_b
  FALUMiscS0.io.in.isfp16                := isfp16
  FALUMiscS0.io.in.isfp32                := isfp32
  FALUMiscS0.io.in.isfp64                := isfp64
  FALUMiscS0.io.in.src0F16IsCanonicalNaN := fp16AIsCanonicalNaN
  FALUMiscS0.io.in.src0F32IsCanonicalNaN := fp32AIsCanonicalNaN
  FALUMiscS0.io.in.src1F16IsCanonicalNaN := fp16BIsCanonicalNaN
  FALUMiscS0.io.in.src1F32IsCanonicalNaN := fp32BIsCanonicalNaN
  FALUMiscS0.io.in.isFmax                := isFmax
  FALUMiscS0.io.in.isFmin                := isFmin
  FALUMiscS0.io.in.isMaxMinM             := isMaxMinM
  FALUMiscS0.io.in.isFsgnj               := isFsgnj
  FALUMiscS0.io.in.isFsgnjn              := isFsgnjn
  FALUMiscS0.io.in.isFsgnjx              := isFsgnjx
  FALUMiscS0.io.in.isFsgn                := isFsgn
  io.toS1.FALUMiscS0ToS1                 := FALUMiscS0.io.out

  io.toS1.isArithF16 := isArithF16
  io.toS1.isArithF32 := isArithF32
  io.toS1.isArithF64 := isArithF64
  io.toS1.isMisc     := isMisc
}
