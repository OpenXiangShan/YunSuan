package yunsuan.vector.VectorConvert.util

import chisel3._
import chisel3.util._

class VFPDecodeBundle extends Bundle {
  val expNotZero  = Bool()
  val expIsZero   = Bool()
  val expIsOnes   = Bool()
  val sigNotZero  = Bool()
  val sigIsZero   = Bool()
  val isNormal    = Bool()
  val isSubnormal = Bool()
  val isInf       = Bool()
  val isZero      = Bool()
  val isNaN       = Bool()
  val isSNaN      = Bool()
  val isQNaN      = Bool()
}

class VectorFloat(val expWidth: Int, val precision: Int) extends Bundle {
  def sigWidth = precision - 1
  val sign = Bool()
  val exp = UInt(expWidth.W)
  val sig = UInt(sigWidth.W)
  def decode: VFPDecodeBundle = {
    val expNotZero = exp.orR
    val expIsOnes = exp.andR
    val sigNotZero = sig.orR
    val bundle = Wire(new VFPDecodeBundle)
    bundle.expNotZero := expNotZero
    bundle.expIsZero := !expNotZero
    bundle.expIsOnes := expIsOnes
    bundle.sigNotZero := sigNotZero
    bundle.sigIsZero := !sigNotZero
    bundle.isNormal := expNotZero && !expIsOnes
    bundle.isSubnormal := bundle.expIsZero && sigNotZero
    bundle.isInf := bundle.expIsOnes && bundle.sigIsZero
    bundle.isZero := bundle.expIsZero && bundle.sigIsZero
    bundle.isNaN := bundle.expIsOnes && bundle.sigNotZero
    bundle.isSNaN := bundle.isNaN && !sig.head(1).asBool
    bundle.isQNaN := bundle.isNaN && sig.head(1).asBool
    bundle
  }
}

object VectorFloat {
  def expBias(expWidth: Int): BigInt = {
    (BigInt(1) << (expWidth - 1)) - 1
  }
  def maxNormExp(expWidth: Int): BigInt = {
    (BigInt(1) << expWidth) - 2
  }
  def fromUInt(x: UInt, expWidth: Int, pc: Int): VectorFloat = {
    val vfp = Wire(new VectorFloat(expWidth, pc))
    vfp.sign := x(expWidth + pc - 1)
    vfp.exp := x(expWidth + pc - 2, pc - 1)
    vfp.sig := x(pc - 2, 0)
    vfp
  }
}

class RawVectorFloat(val expWidth: Int, val precision: Int) extends Bundle {
  val sign = Bool()
  val exp = UInt(expWidth.W)
  val sig = UInt(precision.W)
}

object RawVectorFloat {
  def fromVFP(vfp: VectorFloat, expNotZero: Option[Bool] = None): RawVectorFloat = {
    val inner = Wire(new RawVectorFloat(vfp.expWidth, vfp.precision))
    val nz = if (expNotZero.isDefined) expNotZero.get else vfp.exp.orR
    inner.sign := vfp.sign
    inner.exp := vfp.exp | !nz
    inner.sig := Cat(nz, vfp.sig)
    inner
  }
}