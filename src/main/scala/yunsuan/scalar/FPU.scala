package yunsuan.scalar

import chisel3._
import chisel3.util._

object FPU {

  case class FType(expWidth: Int, precision: Int) {
    val sigWidth = precision - 1
    val len = expWidth + precision
  }

  val f16 = FType(5, 11)
  val f32 = FType(8, 24)
  val f64 = FType(11, 53)

  val ftypes = List(f16, f32, f64)

  val H = ftypes.indexOf(f16).U(log2Ceil(ftypes.length).W)
  val S = ftypes.indexOf(f32).U(log2Ceil(ftypes.length).W)
  val D = ftypes.indexOf(f64).U(log2Ceil(ftypes.length).W)


  def box(x: UInt, typeTag: UInt): UInt = {
    require(x.getWidth == 64)
    Mux(typeTag === D, x, Mux(typeTag === S, Cat(~0.U(32.W), x(31, 0)), Cat(~0.U(48.W), x(15, 0))))
  }

  def box(x: UInt, t: FType): UInt = {
    if(t == f32){
      Cat(~0.U(32.W), x(31, 0))
    } else if(t == f64){
      x(63, 0)
    } else {
      assert(cond = false, "Unknown ftype!")
      0.U
    }
  }

}
