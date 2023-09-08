package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._

package object utils{
  //  extend int to 65bit
  def intExtend(x: Bits, signed: Bool = true.B): Bits = {
    if (x.getWidth >= 65) // extend 1 bit => 65 bit
      x
    else {
      val fillBit = signed
      Fill(65 - x.getWidth, fillBit) ## x.asUInt
    }
  }

  //  extend float to 65bit
  def floatExtend(x: Bits, fp: Int): Bits = {
    if (fp == 0 || fp == 3) return (x ## false.B)
    val floatFormat: FloatFormat = fp match {
      case 1 => f16
      case 2 => f32
    }
    val sign = x.head(1)
    val exp = x.tail(1).head(floatFormat.expWidth)
    val frac = x.tail(1 + floatFormat.expWidth)
    sign ## 0.U((f64.expWidth - floatFormat.expWidth).W) ## exp ## frac ## 0.U((f64.fracWidth - floatFormat.fracWidth).W) ## false.B //tail is false ->65 bit
  }

  // extend to Int
  def extend(x: Bits, len: Int, signed: Boolean = true): Bits = {
    if (x.getWidth >= len)
      x
    else {
      val fillBit = if (signed) x.head(1) else 0.B
      Fill(len - x.getWidth, fillBit) ## x.asUInt
    }
  }
}