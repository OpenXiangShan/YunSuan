package yunsuan.vector

import chisel3.{UInt, _}
import chisel3.util._
import chisel3.experimental.SourceInfo
import yunsuan.util.NamedUInt

import scala.language.implicitConversions

object Common {
  trait VectorConfig {
    val vlen: Int
    val VIdxWidth = log2Up(8)
    val VstartWidth = log2Up(vlen)
    val VlWidth = log2Up(vlen) + 1
    val VLENB = vlen / 8
    val ElemIdxWidth = log2Up(VLENB)

    def UIntVlen: UInt = UInt(vlen.W)

    def VecE8: Vec[UInt] = Vec(vlen / 8, UInt(8.W))

    def VecE16: Vec[UInt] = Vec(vlen / 16, UInt(16.W))

    def VecE32: Vec[UInt] = Vec(vlen / 32, UInt(32.W))

    def VecE64: Vec[UInt] = Vec(vlen / 64, UInt(64.W))
  }

  /**
    * vtype bundle, should not used as csr reg
    */
  class VType extends Bundle {
    val illegal = Bool()
    val vma = Bool()
    val vta = Bool()
    val vsew = VSew()
    val vlmul = VLmul()
  }

  // modify the width when support more vector data width
  object VSew extends NamedUInt(2) {
    def e8 : UInt = "b000".U(width.W)

    def e16: UInt = "b001".U(width.W)

    def e32: UInt = "b010".U(width.W)

    def e64: UInt = "b011".U(width.W)

    def reserved: BitPat = BitPat("b1??")

    def isReserved(sew: UInt): Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        sew === reserved
      } else {
        false.B
      }
    }
  }

  object SewOH extends NamedUInt(4) {
    def e8 : UInt = "b0001".U(width.W)

    def e16: UInt = "b0010".U(width.W)

    def e32: UInt = "b0100".U(width.W)

    def e64: UInt = "b1000".U(width.W)

    def convertFromVSew(vsew: UInt): UInt = {
      require(vsew.getWidth >= 2 && vsew.getWidth <= 3)
      UIntToOH(vsew, this.width)
    }
  }

  object VLmul extends NamedUInt(3) {
    def m1: UInt = "b000".U(width.W)

    def m2: UInt = "b001".U(width.W)

    def m4: UInt = "b010".U(width.W)

    def m8: UInt = "b011".U(width.W)

    def mf2: UInt = "b111".U(width.W)

    def mf4: UInt = "b110".U(width.W)

    def mf8: UInt = "b101".U(width.W)

    def reserved: BitPat = BitPat("b100")

    def isReserved(vlmul: UInt): Bool = {
      require(vlmul.getWidth == 3)
      vlmul === reserved
    }

    def makeNoLessThanM1(uint: UInt): UInt = {
      checkInputWidth(uint)
      Mux(uint(2), m1, uint)
    }
  }

  class VecUIntUtil(val uint: UInt) {
    val width = uint.getWidth

    def this(v: Vec[UInt]) = {
      this(v.asUInt)
    }

    def to8bitVec: Vec[UInt] = {
      require(width % 8 == 0)
      this.splitToVec(width / 8, 8)
    }

    def to16bitVec: Vec[UInt] = {
      require(width % 16 == 0)
      this.splitToVec(width / 16, 16)
    }

    def to32bitVec: Vec[UInt] = {
      require(width % 32 == 0)
      this.splitToVec(width / 32, 32)
    }

    def to64bitVec: Vec[UInt] = {
      require(width % 64 == 0)
      this.splitToVec(width / 64, 64)
    }

    def toVf2Vec: Vec[UInt] = {
      require(width % 2 == 0)
      this.splitToVec(2, width / 2)
    }

    def toVf4Vec: Vec[UInt] = {
      require(width % 4 == 0)
      this.splitToVec(4, width / 4)
    }

    def toVf8Vec: Vec[UInt] = {
      require(width % 8 == 0)
      this.splitToVec(8, width / 8)
    }

    def splitToVec(num: Int, w: Int): Vec[UInt] = {
      require(num * w == uint.getWidth)
      val splitedVec = Wire(Vec(num, UInt(w.W)))
      splitedVec := uint.asTypeOf(splitedVec)
      splitedVec
    }
  }

  implicit def castToUIntUtil(uint: UInt): VecUIntUtil = new VecUIntUtil(uint)

  implicit def castToUIntUtil(v: Vec[UInt]): VecUIntUtil = new VecUIntUtil(v)

  def fill8b1s  = Fill(8, 1.U(1.W))
  def fill16b1s = Fill(16, 1.U(1.W))
  def fill32b1s = Fill(32, 1.U(1.W))
  def fill64b1s = Fill(64, 1.U(1.W))

  def WireInitFixedWidth[T <: Data](init: T)(implicit sourceInfo: SourceInfo): T = {
    WireInit(UInt(init.getWidth.W), init).asInstanceOf[T]
  }
}
