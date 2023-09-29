package yunsuan.encoding.Opcode

import chisel3._

object VfCvtOpcode {
  def width = 8
  // 000_000 -> 000_101 single
  // 001_000 -> 001_110 widen
  // 010_000 -> 010_111 norrow
  // 100_000 1/sqrt7(x)
  // 100_001 1/x7
  //  f/i(input) ## f/i(output) ## intr's func1
  def vfcvt_xufv        = "b10_000000".U(8.W)
  def vfcvt_xfv         = "b10_000001".U(8.W)
  def vfcvt_fxuv        = "b01_000010".U(8.W)
  def vfcvt_fxv         = "b01_000011".U(8.W)
  def vfcvt_rtz_xufv    = "b10_000110".U(8.W)
  def vfcvt_rtz_xfv     = "b10_000111".U(8.W)

  def vfrsqrt7          = "b11_100000".U(8.W)
  def vfrec7            = "b11_100001".U(8.W)

  def vfwcvt_xufv       = "b10_001000".U(8.W)
  def vfwcvt_xfv        = "b10_001001".U(8.W)
  def vfwcvt_fxuv       = "b01_001010".U(8.W)
  def vfwcvt_fxv        = "b01_001011".U(8.W)
  def vfwcvt_ffv        = "b11_001100".U(8.W)
  def vfwcvt_rtz_xufv   = "b10_001110".U(8.W)
  def vfwcvt_rtz_xfv    = "b10_001111".U(8.W)

  def vfncvt_xufw       = "b10_010000".U(8.W)
  def vfncvt_xfw        = "b10_010001".U(8.W)
  def vfncvt_fxuw       = "b01_010010".U(8.W)
  def vfncvt_fxw        = "b01_010011".U(8.W)
  def vfncvt_ffw        = "b11_010100".U(8.W)
  def vfncvt_rod_ffw    = "b11_010101".U(8.W)
  def vfncvt_rtz_xufw   = "b10_010110".U(8.W)
  def vfncvt_rtz_xfw    = "b10_010111".U(8.W)
}
