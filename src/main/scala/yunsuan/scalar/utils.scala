package yunsuan.scalar

import chisel3._
import chisel3.util._


object FloatPoint {
  def expBias(expWidth: Int): BigInt = {
    (BigInt(1) << (expWidth - 1)) - 1
  }
  def maxNormExp(expWidth: Int): BigInt = {
    (BigInt(1) << expWidth) - 2
  }

}
object SignExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    val signBit = a(aLen-1)
    if (aLen >= len) a(len-1,0) else Cat(Fill(len - aLen, signBit), a)
  }
}

object ZeroExt {
  def apply(a: UInt, len: Int): UInt = {
    val aLen = a.getWidth
    if (aLen >= len) a(len-1,0) else Cat(0.U((len - aLen).W), a)
  }
}

class LzaIO(val len: Int) extends Bundle {
  val a, b = Input(UInt(len.W))
  val f = Output(UInt(len.W))
}

class LZA(len: Int) extends Module {
  val io = IO(new LzaIO(len))

  val (a, b) = (io.a, io.b)

  val p, k, f = Wire(Vec(len, Bool()))
  for (i <- 0 until len) {
    p(i) := a(i) ^ b(i)
    k(i) := (!a(i)) & (!b(i))
    if (i == 0) {
      f(i) := false.B
    } else {
      f(i) := p(i) ^ (!k(i - 1))
    }
  }
  io.f := Cat(f.reverse)
}

class CLZ(len: Int, zero: Boolean) extends Module {

  val inWidth = len
  val outWidth = (inWidth - 1).U.getWidth

  val io = IO(new Bundle() {
    val in = Input(UInt(inWidth.W))
    val out = Output(UInt(outWidth.W))
  })

  io.out := PriorityEncoder(io.in.asBools.reverse)
}

object CLZ {
  def apply(value: UInt): UInt = {
    val clz = Module(new CLZ(value.getWidth, true))
    clz.io.in := value
    clz.io.out
  }
  def apply(xs: Seq[Bool]): UInt = {
    apply(Cat(xs.reverse))
  }
}