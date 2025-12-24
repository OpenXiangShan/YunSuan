package yunsuan.util

import chisel3._
import chisel3.util._

case class LzcPart(z: Vec[Bool], v: Bool)

object Lzc {
  def apply(in: UInt): UInt = {
    val lzc = Module(new Lzc(in.getWidth))
    lzc.io.in := in
    lzc.io.out
  }
}

object LzcPart {
  def merge(l: LzcPart, h: LzcPart): LzcPart = {
    val Z_neg = Wire(Vec(l.z.length + 1, Bool()))
    val V_neg = l.v | h.v

    Z_neg(l.z.length) := h.v

    for (j <- 0 until l.z.length) {
      val temp1 = h.v & h.z(j)
      val temp2 = (!h.v) & l.z(j)
      Z_neg(j) := temp1 | temp2
    }

    LzcPart(Z_neg, V_neg)
  }
}

class Lzc(bitWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(bitWidth.W))
    val out = Output(UInt(log2Up(bitWidth).W))
    val isZero = Output(Bool())
  })
  require(bitWidth > 0, "Bit width must be greater than 0.")
  val width = Math.pow(2, log2Up(bitWidth)).toInt
  val in = (io.in.asBools.reverse ++ Seq.fill(width - bitWidth)(true.B)).reverse
  val groups: Seq[Seq[Bool]] = in.grouped(8).toSeq

  val localPart: Seq[LzcPart] = groups.map {group =>
    val zLocal = Wire(Vec(3, Bool()))
    val vLocal = Wire(Bool())

    zLocal(2) := group(7) | group(6) | group(5) | group(4)
    zLocal(1) := group(7) | group(6) | !group(5) & !group(4) & (group(3) | group(2))
    zLocal(0) := group(7) | (!group(6) & group(5)) | (!group(6) & !group(4) & group(3)) | (!group(6) & !group(4) & !group(2) & group(1))
    vLocal := group.reduce(_ | _)

    LzcPart(zLocal, vLocal)
  }

  def mergeAll(parts: Seq[LzcPart]): LzcPart = {
    if (parts.length == 1) parts.head
    else {
      val paired = parts.grouped(2).map {
        case Seq(l, h) => LzcPart.merge(l, h)
        case Seq(l) => l
      }.toSeq
      mergeAll(paired)
    }
  }

  val finalPart = mergeAll(localPart)
  val finalZ = VecInit(finalPart.z.map(!_))
  val z_out = finalZ.asUInt
  val v_out = !finalPart.v
  io.out := z_out
  io.isZero := v_out
}
