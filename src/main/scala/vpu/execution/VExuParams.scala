package race.vpu.yunsuan

import chisel3._
import chisel3.util._

object VectorElementFormat {
  def width = 2
  def b = "b00".U(width.W)
  def h = "b01".U(width.W)  // f16
  def w = "b10".U(width.W)  // f32
  def d = "b11".U(width.W)  // f64

  def apply() = UInt(width.W)
}

object Vfauop_format {
  def width = 2
  def b = "b00".U(width.W)
  def h = "b01".U(width.W)  // f16
  def w = "b10".U(width.W)  // f32
  def d = "b11".U(width.W)  // f64

  def apply() = UInt(width.W)
}

object Vfaddfunc6 {
  def width = 6
  def dummy    = "b111111".U(width.W)
  def fadd     = "b000000".U(width.W)
  def fsub     = "b000010".U(width.W)
  def fmin     = "b000100".U(width.W)
  def fmax     = "b000110".U(width.W)
  def fmerge   = "b010111".U(width.W)
  def fmove    = "b010111".U(width.W)
  def fsgnj    = "b001000".U(width.W)
  def fsgnjn   = "b001001".U(width.W)
  def fsgnjx   = "b001010".U(width.W)
  def feq      = "b011000".U(width.W)
  def fne      = "b011100".U(width.W)
  def flt      = "b011011".U(width.W)
  def fle      = "b011001".U(width.W)
  def fgt      = "b011101".U(width.W)
  def fge      = "b011111".U(width.W)
  def fclass   = "b010011".U(width.W)
  def fmv_f_s  = "b010000".U(width.W)
  def fmv_s_f  = "b010000".U(width.W)
  def fredsum_u = "b000001".U(width.W) // unordered
  def fredmin   = "b000101".U(width.W)
  def fredmax   = "b000111".U(width.W)
  def fredsum_o = "b000011".U(width.W) // ordered

  def fminm     = "b011110".U(width.W) // zfa extension
  def fmaxm     = "b010011".U(width.W) // zfa extension
  def fleq      = "b011100".U(width.W) // zfa extension
  def fltq      = "b011011".U(width.W) // zfa extension
}

object VfaddOpCode {
  def dummy    = "b11111".U(5.W)
  def fadd     = "b00000".U(5.W)
  def fsub     = "b00001".U(5.W)
  def fmin     = "b00010".U(5.W)
  def fmax     = "b00011".U(5.W)
  def fmerge   = "b00100".U(5.W)
  def fmove    = "b00101".U(5.W)
  def fsgnj    = "b00110".U(5.W)
  def fsgnjn   = "b00111".U(5.W)
  def fsgnjx   = "b01000".U(5.W)
  def feq      = "b01001".U(5.W)
  def fne      = "b01010".U(5.W)
  def flt      = "b01011".U(5.W)
  def fle      = "b01100".U(5.W)
  def fgt      = "b01101".U(5.W)
  def fge      = "b01110".U(5.W)
  def fclass   = "b01111".U(5.W)
  def fmv_f_s  = "b10001".U(5.W)
  def fmv_s_f  = "b10010".U(5.W)
  def fsum_ure = "b11010".U(5.W) // unordered
  def fmin_re  = "b10100".U(5.W)
  def fmax_re  = "b10101".U(5.W)
  def fsum_ore = "b10110".U(5.W) // ordered
  def fminm    = "b11110".U(5.W)
  def fmaxm    = "b10011".U(5.W)
  def fleq     = "b11100".U(5.W)
  def fltq     = "b11011".U(5.W)
}

// fma
object Vfffunc6 {
  def width = 6
  def dummy     = "b111111".U(width.W)
  def fmul      = "b100100".U(width.W)
  def fmacc     = "b101100".U(width.W)
  def fnmacc    = "b101101".U(width.W)
  def fmsac     = "b000000".U(width.W)
  def fnmsac    = "b101110".U(width.W)
  def fmadd     = "b101000".U(width.W)
  def fnmadd    = "b101001".U(width.W)
  def fmsub     = "b101010".U(width.W)
  def fnmsub    = "b101011".U(width.W)

}

object VfmaOpCode {
  def dummy   = "b1111".U(4.W)
  def vfmul   = "b0000".U(4.W)
  def vfmacc  = "b0001".U(4.W)
  def vfnmacc = "b0010".U(4.W)
  def vfmsac  = "b0011".U(4.W)
  def vfnmsac = "b0100".U(4.W)
  def vfmadd  = "b0101".U(4.W)
  def vfnmadd = "b0110".U(4.W)
  def vfmsub  = "b0111".U(4.W)
  def vfnmsub = "b1000".U(4.W)
}


// fdiv
object Vfdfunc6 {
  def width = 6
  def dummy     = "b111111".U(width.W)
  def fdiv      = "b100000".U(width.W)
  def frdiv     = "b100001".U(width.W)
  def fsqrt     = "b010011".U(width.W)
}

object VfdOpCode {
  def vfdiv   = "b0".U(1.W)
  def vfsqrt  = "b1".U(1.W)
}


// fcvt
object Vfcvtfunc6 {
  def width = 6
  def dummy       = "b111111".U(width.W)
  def vfncvt_xfw  = "b010010".U(width.W)
  def vfwcvt_fxv  = "b010010".U(width.W)
}

object VfcvtOpCode {
  def dummy         = "b11111111".U(8.W)
  def vfncvt_xfw    = "b10000000".U(8.W)
  def vfwcvt_fxv    = "b01001011".U(8.W)
}

// vrg
object Vrgfunc6 {
  def width = 6
  def dummy       = "b111111".U(width.W)
  def vrgather    = "b001100".U(width.W)
}

object VrgOpCode {
  def dummy      = "b0".U(1.W)
  def vrgather   = "b1".U(1.W)
}


// vfreduction
object Vfredfunc6 {
  def width = 6
  def dummy         = "b111111".U(width.W)
  def vfredusum     = "b000001".U(width.W)
  def vfredosum     = "b000011".U(width.W)
  def vfredmax      = "b000111".U(width.W)
}

// same as vfa
object VfredOpCode {
  def dummy    = "b11111".U(5.W)
  def fadd     = "b00000".U(5.W)
  def fsub     = "b00001".U(5.W)
  def fmin     = "b00010".U(5.W)
  def fmax     = "b00011".U(5.W)
}


//latency
object Yunsuan_latency {
  val VFD_latency: Int = 99 // unknown
  val VFA_latency: Int = 1
  val VCVT_latency: Int = 2 // ??
}
