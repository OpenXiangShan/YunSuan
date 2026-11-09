package yunsuan.util

import chisel3._
import chisel3.util._

/** Arithmetic simulation models use ordinary Chisel arithmetic and state. */
object MultiplyBehavior {
  /** Exact 2*w-bit product without a 65x65 multiplication for mixed signs. */
  def product(a: UInt, b: UInt, signedA: Bool, signedB: Bool): UInt = {
    val w = a.getWidth
    require(w == b.getWidth && w > 0 && w <= 64)
    val unsigned = a * b
    val high = unsigned(2*w-1, w) - Mux(signedA && a(w-1), b, 0.U(w.W)) -
      Mux(signedB && b(w-1), a, 0.U(w.W))
    Cat(high, unsigned(w-1, 0))
  }

  /** Match YunSuan's enabled stages: reset valid, leave payload uninitialized. */
  def pipeline[T <: Data](value: T, valid: Bool, stages: Int): T = {
    require(stages >= 1)
    var result = value
    var enable = valid
    for (stage <- 0 until stages) {
      result = RegEnable(result, enable)
      if (stage + 1 < stages) enable = GatedValidRegNext(enable)
    }
    result
  }

  /** Exact independent significand products in the existing CSA lane layout. */
  def significands(a: UInt, b: UInt, fp64: Bool, fp32: Bool, vector: Boolean): UInt = {
    require(a.getWidth == 53 && b.getWidth == 53)
    val wide = (a * b).pad(107)
    val single0 = a(23, 0) * b(23, 0)
    val half0 = a(10, 0) * b(10, 0)
    val single = if (vector) Cat(0.U(1.W), a(52, 29) * b(52, 29), 0.U(10.W), single0)
      else single0.pad(107)
    val half = if (vector) Cat(0.U(1.W), a(52, 42) * b(52, 42), 0.U(4.W),
      a(39, 29) * b(39, 29), 0.U(10.W), a(23, 13) * b(23, 13), 0.U(4.W), half0)
      else half0.pad(107)
    Mux(fp64, wide, Mux(fp32, single, half))
  }
}
