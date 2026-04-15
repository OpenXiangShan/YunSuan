package yunsuan.util

import chisel3._
import chisel3.util.experimental.decode._
import chisel3.util.{BitPat, Cat}

import scala.language.implicitConversions

object ChiselExt {
  implicit class BinaryStringHelper(private val sc: StringContext) extends AnyVal {
    /**
     * Create fixed width BitPat by binary string
     */
    def bb(args: Any*): BitPat = {
      val str: String = StringContext.standardInterpolator(x => x, args, sc.parts)
      require(str.forall(c => Seq('0', '1', '?', '_').contains(c) || c.isWhitespace))
      BitPat("b" + str)
    }

    /**
     * Create fixed width UInt by binary string
     */
    def ub(args: Any*): UInt = {
      val str: String = StringContext.standardInterpolator(x => x, args, sc.parts)
        .filterNot(x => x == ' ' || x == '_')
      require(str.forall(c => c == '0' || c == '1'))
      ("b" + str).U(str.length.W)
    }
  }

  class BitPatExt(val bitPat: BitPat) {
    def ## (that: UInt): BitPat = this.bitPat ## BitPat(that)

    def head(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n <= this.bitPat.width)
      this.bitPat.apply(this.bitPat.width - 1, this.bitPat.width - n)
    }

    def tail(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.bitPat.width, "The width of result should be greater than 0")
      this.bitPat.apply(this.bitPat.width - n, 0)
    }

    def lsb(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n <= this.bitPat.width)
      this.bitPat.apply(n - 1, 0)
    }

    def msb(n: Int): BitPat = this.head(n)

    def drop(n: Int): BitPat = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.bitPat.width, "The width of result should be greater than 0")
      this.bitPat.apply(this.bitPat.width - 1, n)
    }

    def take(n: Int): BitPat = this.lsb(n)

    def pad0To(n: Int): BitPat = {
      if (bitPat.width >= n)
        this.bitPat
      else
        BitPat.N(n - bitPat.width) ## this.bitPat
    }

    def padDcTo(n: Int): BitPat = {
      if (bitPat.width >= n)
        this.bitPat
      else
        BitPat.dontCare(n - bitPat.width) ## this.bitPat
    }

    def isOneOf(bps: BitPat*): Boolean = {
      bps.exists(_.cover(bitPat))
    }
  }

  sealed trait UIntCanCompare[T] {
    def toBitPat(x: T): BitPat
  }

  implicit object UIntComparer extends UIntCanCompare[UInt] {
    def toBitPat(x: UInt): BitPat = BitPat(x)
  }

  implicit object BitPatComparer extends UIntCanCompare[BitPat] {
    def toBitPat(x: BitPat): BitPat = x
  }

  class UIntExt(val value: UInt) {
    def toFixWidthBitPat(n: Int): BitPat = {
      BitPat(this.value.pad(n))
    }

    def toBitPat: BitPat = {
      BitPat(this.value)
    }

    def drop(n: Int): UInt = {
      require(n > 0, s"The arg should be greater than 0, but get $n")
      require(n < this.value.getWidth, "The width of result should be greater than 0")
      this.value.apply(this.value.getWidth - 1, n)
    }

    def isOneOf(bp1: BitPat, bp2: BitPat*): Bool = isOneOf(bp1 +: bp2)

    def isOneOf(bp1: UInt, bp2: UInt*): Bool = isOneOf(bp1 +: bp2)

    def isOneOf[T: UIntCanCompare](data: Iterable[T])(implicit comparer: UIntCanCompare[T]): Bool = decoder(
      QMCMinimizer,
      value,
      TruthTable(
        data.map(x => comparer.toBitPat(x) -> BitPat("b1")),
        BitPat("b0")
      )
    ).asBool

    /**
     * Literal value cat
     * @param that: a literal UInt value whose width is known
     * @return a result of Literal value, the width is sum of two literal value.
     */
    def ###(that: UInt): UInt = {
      require(this.value.widthKnown && that.value.widthKnown)
      require(this.value.isLit && that.value.isLit)
      val thisWidth = this.value.getWidth
      val thatWidth = that.value.getWidth

      ((this.value.litValue << thatWidth) | that.litValue).U((thisWidth + thatWidth).W)
    }

    /**
     * Literal value or
     * @param that: a literal UInt value whose width is known
     * @return a result of Literal value, the width is max of two literal value.
     */
    def #|#(that: UInt): UInt = {
      require(this.value.widthKnown && that.value.widthKnown)
      require(this.value.isLit && that.value.isLit)
      val maxWidth = this.value.getWidth max that.value.getWidth
      (this.value.litValue | that.litValue).U(maxWidth.W)
    }

    /**
     * Literal value shift left
     * @param shamt: shift amount
     * @return a result of Literal value, the width will be plused shamt.
     */
    def #<<(shamt: Int): UInt = {
      require(this.value.widthKnown)
      (this.value.litValue << shamt).U((this.value.getWidth + shamt).W)
    }
  }

  class EnumTypeExt(val value: EnumType) {
    def toBitPat: BitPat = {
      new BitPat(value.litValue, (BigInt(1) << value.getWidth) - 1, value.getWidth)
    }
  }

  class DataExt[T <: Data](val value: T) {
    def isOneOf(data: Iterable[T]): Bool = {
      Cat(data.map(_ === value).toSeq).orR
    }
  }

  implicit def UIntToExt(uint: UInt): UIntExt = new UIntExt(uint)
  implicit def BitPatToExt(bitPat: BitPat): BitPatExt = new BitPatExt(bitPat)
  implicit def EnumTypeToExt(enum: EnumType): EnumTypeExt = new EnumTypeExt(enum)
  implicit def DataToExt[T <: Data](data: T): DataExt[T] = new DataExt(data)
}
