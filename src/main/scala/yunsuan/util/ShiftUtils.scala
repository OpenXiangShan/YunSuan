package yunsuan.util

import chisel3._
import chisel3.util._

import math.pow

object doShiftRotateLeft {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftRotateLeft(in, shamt)
  }

  def doShiftRotateLeft[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val shifted =
      Mux(shift.head(1).asBool, Cat(in.tail(amount), in.head(amount)), in)
    if (length == 1) shifted else doShiftRotateLeft(shifted, shift.tail(1))
  }
}

object doShiftRotateRight {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftRotateRight(in, shamt)
  }

  def doShiftRotateRight[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val inWidth = in.getWidth
    val amount = pow(2, length - 1).intValue
    val shifted = Mux(
      shift.head(1).asBool,
      Cat(in.tail(inWidth - amount), in.head(inWidth - amount)),
      in
    )
    if (length == 1) shifted else doShiftRotateRight(shifted, shift.tail(1))
  }
}

object doShiftLeft {
  def apply(in: UInt, shamt: UInt): Bits = {
    doShiftLeft(in, shamt)
  }

  def doShiftLeft[TI <: Bits, TS <: Bits](in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val shifted =
      Mux(shift.head(1).asBool, Cat(in.tail(amount), 0.U(amount.W)), in)
    if (length == 1) shifted else doShiftLeft(shifted, shift.tail(1))
  }
}
