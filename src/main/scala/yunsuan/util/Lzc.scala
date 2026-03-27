package yunsuan.util

import chisel3._
import chisel3.util._

case class LzcPart(z: Vec[Bool], v: Bool)

object Lzc {
  def apply(in: UInt): LzcOutBundle = {
    val lzc = Module(new Lzc(in.getWidth))
    lzc.io.in := in
    lzc.io.out
  }
  def apply(in: UInt, isVector: Boolean): Lzc = {
    val lzc = Module(new Lzc(in.getWidth, isVector))
    lzc.io.in := in
    lzc
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

class LzcOutBundle(bitWidth: Int) extends Bundle {
  val data = UInt(log2Up(bitWidth).W)
  val isZero = Bool()
}

class VectorOutBundle extends Bundle {
  val out8  = Vec(8, UInt(4.W))
  val out16 = Vec(4, UInt(5.W))
  val out32 = Vec(2, UInt(6.W))
  val out64 = UInt(7.W)
}

class Lzc(bitWidth: Int, isVector: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(bitWidth.W))
    val out = Output(new LzcOutBundle(bitWidth))
    val vectorOut = Option.when(isVector)(Output(new VectorOutBundle))
  })
  require(bitWidth > 0, "Bit width must be greater than 0.")
  val width = Math.pow(2, log2Up(bitWidth)).toInt
  val in = (io.in.asBools.reverse ++ Seq.fill(width - bitWidth)(false.B)).reverse
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
  io.out.data := z_out
  io.out.isZero := v_out

  if (isVector) {
    def mergePart(parts: Seq[LzcPart]): Seq[LzcPart] = {
      val paired = parts.grouped(2).map {
        case Seq(l, h) => LzcPart.merge(l, h)
        case Seq(l) => l
      }.toSeq
      paired
    }

    val outPartLevel1 = Wire(Vec(8, UInt(4.W)))
    for (i <- 0 until 8) {
      val zPart8 = VecInit(localPart(i).z.map(!_))
      val vPart8 = !localPart(i).v
      outPartLevel1(i) := Mux(vPart8, "b1000".U, zPart8.asUInt)
      io.vectorOut.get.out8(i) := outPartLevel1(i)
    }

    val localPart16: Seq[LzcPart] = mergePart(localPart)
    val outPartLevel2 = Wire(Vec(4, UInt(5.W)))
    for (i <- 0 until 4) {
      val zPart16 = VecInit(localPart16(i).z.map(!_))
      val vPart16 = !localPart16(i).v
      outPartLevel2(i) := Mux(vPart16, "b10000".U, zPart16.asUInt)
      io.vectorOut.get.out16(i) := outPartLevel2(i)
    }

    val localPart32: Seq[LzcPart] = mergePart(localPart16)
    val outPartLevel3 = Wire(Vec(2, UInt(6.W)))
    for (i <- 0 until 2) {
      val zPart32 = VecInit(localPart32(i).z.map(!_))
      val vPart32 = !localPart32(i).v
      outPartLevel3(i) := Mux(vPart32, "b100000".U, zPart32.asUInt)
      io.vectorOut.get.out32(i) := outPartLevel3(i)
    }

    val localPart64: Seq[LzcPart] = mergePart(localPart32)
    val outPartLevel4 = Wire(UInt(7.W))
    val zPart64 = VecInit(localPart64.head.z.map(!_))
    val vPart64 = !localPart64.head.v
    outPartLevel4 := Mux(vPart64, "b1000000".U, zPart64.asUInt)
    io.vectorOut.get.out64 := outPartLevel4
  }
}
