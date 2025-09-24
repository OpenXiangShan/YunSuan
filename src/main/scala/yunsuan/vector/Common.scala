package yunsuan.vector

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo
import yunsuan.util.{NamedUInt, ParallelOR}
import chisel3.experimental.BundleLiterals._

import scala.language.implicitConversions

object Common {
  trait VectorConfig {
    val vlen: Int
    val VIdxWidth = log2Up(8)
    def VstartWidth = log2Up(vlen)
    def VlWidth = log2Up(vlen) + 1
    def VLENB = vlen / 8
    def ElemIdxWidth = log2Up(VLENB)
    val MaxLMUL = 8
    val VdIdxWidth = log2Up(MaxLMUL)

    def UIntVlen: UInt = UInt(vlen.W)

    def VecE8: Vec[UInt] = Vec(vlen / 8, UInt(8.W))

    def VecE16: Vec[UInt] = Vec(vlen / 16, UInt(16.W))

    def VecE32: Vec[UInt] = Vec(vlen / 32, UInt(32.W))

    def VecE64: Vec[UInt] = Vec(vlen / 64, UInt(64.W))

    def genMaskE8  = UInt(VLENB.W)
    def genMaskE16 = UInt((VLENB / 2).W)
    def genMaskE32 = UInt((VLENB / 4).W)
    def genMaskE64 = UInt((VLENB / 8).W)

    def UIntVlenb: UInt = UInt(VLENB.W)

    def Vlenb1s = Fill(VLENB, 1.U(1.W))

    def byte1s = "hff".U

    def VdIdx: UInt = UInt(VdIdxWidth.W)

    val MinDataWidth = 8
  }

  /**
    * vtype bundle, should not used as csr reg
    */
  class VType extends Bundle {
    val vill = Bool()
    val vma = Bool()
    val vta = Bool()
    val vsew = VSew()
    val vlmul = VLmul()
  }

  object VType {
    def apply(): VType = {
      new VType
    }

    def mu: UInt = 0.U(1.W)

    def ma: UInt = 1.U(1.W)

    def tu: UInt = 0.U(1.W)

    def ta: UInt = 1.U(1.W)
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

  class SewOH extends Bundle { // 0   1   2   3
    val oneHot = UInt(4.W) // b0-b3: 8, 16, 32, 64

    def is8 = oneHot(0)
    def is16 = oneHot(1)
    def is32 = oneHot(2)
    def is64 = oneHot(3)

    def toWiden: SewOH = this.widenUInt.asTypeOf(new SewOH)

    def widenUInt: UInt = Cat(oneHot.take(3), 0.U(1.W))
    // remove E64 OH bit
    def rmE64: UInt = this.oneHot.take(3)
    // remove E32 and E64 OH bit
    def rmGeE32: UInt = this.oneHot.take(2)
    // remove E8 OH bit
    def rmE8: UInt = this.oneHot.drop(1)
    // remove E8 and E16 OH bit
    def rmLeE16: UInt = this.oneHot.drop(2)
  }

  object SewOH {
    def e8 : UInt = "b0001".U(4.W)
    def e16: UInt = "b0010".U(4.W)
    def e32: UInt = "b0100".U(4.W)
    def e64: UInt = "b1000".U(4.W)

    def convertFromVSew(vsew: UInt): UInt = {
      require(vsew.getWidth >= 2 && vsew.getWidth <= 3)
      UIntToOH(vsew, 4)
    }

    def apply(vsew: UInt): SewOH = {
      val sew = Wire(new SewOH)
      sew.oneHot := VecInit(Seq.tabulate(4)(i => vsew === i.U)).asUInt
      sew
    }

    def apply(): SewOH = new SewOH
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

  class Vxrm extends Bundle {
    val bits = UInt(2.W)

    def isRnu = Vxrm.rnu === bits
    def isRne = Vxrm.rne === bits
    def isRdn = Vxrm.rdn === bits
    def isRod = Vxrm.rod === bits
  }

  object Vxrm {
    def apply(): Vxrm = new Vxrm()

    def rnu = 0.U(2.W)
    def rne = 1.U(2.W)
    def rdn = 2.U(2.W)
    def rod = 3.U(2.W)
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

    def splitToVec(num: Int, w: Int, name: String = null): Vec[UInt] = {
      require(num * w == uint.getWidth)
      val splitedVec = Wire(Vec(num, UInt(w.W)))
      if (name != null)
        splitedVec.suggestName(name)
      splitedVec := uint.asTypeOf(splitedVec)
      splitedVec
    }

    def splitToVecN(num: Int, name: String = null): Vec[UInt] = {
      val w = uint.getWidth / num
      this.splitToVec(num, w, name)
    }

    def splitToVecByWidth(w: Int, name: String = null): Vec[UInt] = {
      val num = uint.getWidth / w
      this.splitToVec(num, w, name)
    }
  }

  implicit def castToUIntUtil(uint: UInt): VecUIntUtil = new VecUIntUtil(uint)

  implicit def castToUIntUtil(v: Vec[UInt]): VecUIntUtil = new VecUIntUtil(v)

  implicit def vecBoolCastToVecUInt(vb: Vec[Bool]): Vec[UInt] = VecInit(vb.map(_.asUInt))

  class VecUtilType[T <: Data](val vec: Vec[T]) {
    val length = vec.length
    def rotateUp(n: Int): Vec[T] = {
      n match {
        case _ if (n == 0)      => vec
        case _ if (n >= length) => rotateUp(n % length)
        case _ if (n < 0)       => rotateDown(-n)
        case _                  => VecInit(vec.takeRight(n) ++ vec.dropRight(n))
      }
    }

    def rotateUp(n: UInt): Vec[T] = {
      VectorShuffle.RotateUp(vec, n)
    }

    def rotateDown(n: Int): Vec[T] = {
      n match {
        case _ if n == 0      => vec
        case _ if n >= length => rotateDown(n % length)
        case _ if n < 0       => rotateUp(-n)
        case _                => VecInit(vec.drop(n) ++ vec.take(n))
      }
    }

    def rotateDown(n: UInt): Vec[T] = {
      val slideNum = n.take(log2Up(length))
      VecInit.tabulate(length)(l => this.rotateDown(l))(slideNum)
    }

    def rotateLeft(n: Int): Vec[T] = this.rotateUp(n)

    def rotateRight(n: Int): Vec[T] = this.rotateDown(n)

    def compress(mask: UInt): Vec[Valid[T]] = {
      VectorShuffle.Compress(mask, vec)
    }

    def takeAsVec(n: Int): Vec[T] = {
      VecInit(this.vec.take(n))
    }
  }

  implicit def caseToVecUtilType[T <: Data](v: Vec[T]): VecUtilType[T] = new VecUtilType[T](v)

  class UIntUtil(val uint: UInt) {
    def this(bits: Bits) = this(bits.asUInt)

    val length = uint.getWidth

    def rev8: UInt = {
      require(isPow2(length) && length >= 8, s"Can not do rev8 on UInt($length.W)")
      Cat((0 until length / 8).map(i => uint(8 * (i + 1) - 1, 8 * i)))
    }

    def take(n: Int): UInt = {
      require(0 <= n && n <= length, s"Can not take $n bits, since the operand is $length bits width")
      if (n == 0)
        0.U(0.W)
      else
        uint(n - 1, 0)
    }

    def drop(n: Int): UInt = {
      require(n < length, s"Can not drop $n bits, since the operand is $length bits width")
      uint(length - 1, n)
    }

    def takeOrPad(n: Int): UInt = {
      if (n <= length)
        this.take(n)
      else
        uint.pad(n)
    }

    def &>(b: Bool): UInt = {
      Mux(b, uint, 0.U)
    }

    def isOneOf(seq: Seq[UInt]): Bool = {
      require(seq.nonEmpty)
      seq.map(_ === this.uint).reduce(_ || _)
    }

    def isOneOf(a: UInt, seq: UInt*): Bool = this.isOneOf(a +: seq)

    def isOneOf[T](t: T)(a: T => UInt, seq: (T => UInt)*): Bool = this.isOneOf(a(t) +: seq.map(_(t)))

    def bitReverse: UInt = Cat(this.uint.asBools)

    def bitDup(n: Int): UInt = this.uint.asBools.map(b => Fill(n, b)).reverse.fold(0.U(0.W))(_ ## _)

    def map[T](f: Bool => T): Seq[T] = {
      uint.asBools.map(f)
    }
  }

  implicit def caseToUIntUtil(uint: UInt): UIntUtil = new UIntUtil(uint)
  implicit def caseToUIntUtil(bits: Bits): UIntUtil = new UIntUtil(bits)

  def Mux1HValidIO[T <: Data](seq: Seq[ValidIO[T]]): ValidIO[T] = {
    val valid = ParallelOR(seq.map(_.valid))
    val bits = chisel3.util.Mux1H(seq.map(x => x.valid -> x.bits))
    makeValidIO(valid, bits)
  }

  def makeValidIO[T <: Data](valid: Bool, bits: T): ValidIO[T] = {
    Pipe(valid, bits, 0)
  }

  def fill8b1s  = Fill(8, 1.U(1.W))
  def fill16b1s = Fill(16, 1.U(1.W))
  def fill32b1s = Fill(32, 1.U(1.W))
  def fill64b1s = Fill(64, 1.U(1.W))

  def WireInitFixedWidth[T <: Data](init: T)(implicit sourceInfo: SourceInfo): T = {
    WireInit(UInt(init.getWidth.W), init).asInstanceOf[T]
  }

  object BundleMaker {
    implicit class BundleMakeConstructor[T <: Data](items: Seq[T]) {
      def makeBundle[TB <: Bundle](bundleGen: => TB): TB = {
        val node = Wire(bundleGen)
        for ((sink, source) <- node.getElements.zip(items)) {
          sink := source
        }
        node
      }
    }
  }

  implicit def validAutoUnwrap[T <: Data](valid: Valid[T]): T = valid.bits
}
