package yunsuan.vector.v2.Crypto

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

import scala.language.implicitConversions

package object Utils {
  /**
   * Utils for SHA256/512
   */
  object Zvknhb {
    private[Zvknhb] class sum1chModule(EEW: Int) extends Module {
      val h, g, f, e, kw = IO(Input(UInt(EEW.W)))
      val t1 = IO(Output(UInt(EEW.W)))

      t1 := h + sum1(EEW)(e) + ch(e, f, g) + kw
    }

    private[Zvknhb] object sum1chModule {
      def apply(
        EEW: Int,
      )(
        h: UInt, g: UInt, f: UInt, e: UInt, kw: UInt,
      ): UInt = {
        val sum1chMod = Module(new sum1chModule(EEW))
        sum1chMod.h := h
        sum1chMod.g := g
        sum1chMod.f := f
        sum1chMod.e := e
        sum1chMod.kw := kw
        sum1chMod.t1
      }
    }

    private[Zvknhb] class sum0majModule(EEW: Int) extends Module {
      val c, b, a = IO(Input(UInt(EEW.W)))
      val t2 = IO(Output(UInt(EEW.W)))

      t2 := sum0(EEW)(a) + maj(a, b, c)
    }

    private[Zvknhb] object sum0majModule {
      def apply(
        EEW: Int,
      )(
        c: UInt, b: UInt, a: UInt,
      ): UInt = {
        val sum0majMod = Module(new sum0majModule(EEW))
        sum0majMod.c := c
        sum0majMod.b := b
        sum0majMod.a := a
        sum0majMod.t2
      }
    }

    object SHA512 {
      val EEW = 64

      def sum0(x: UInt): UInt = x.rotateRight(28) ^ x.rotateRight(34) ^ x.rotateRight(39)

      def sum1(x: UInt): UInt = x.rotateRight(14) ^ x.rotateRight(18) ^ x.rotateRight(41)

      def sum0maj: (UInt, UInt, UInt) => UInt = sum0majModule(EEW)

      def sum1ch: (UInt, UInt, UInt, UInt, UInt) => UInt = sum1chModule(EEW)

      def sig0(x: UInt): UInt = x.rotateRight(1) ^ x.rotateRight(8) ^ x.>>(7).asUInt

      def sig1(x: UInt): UInt = x.rotateRight(19) ^ x.rotateRight(61) ^ x.>>(6).asUInt
    }

    object SHA256 {
      val EEW = 32

      def sum0(x: UInt): UInt = x.rotateRight(2) ^ x.rotateRight(13) ^ x.rotateRight(22)

      def sum1(x: UInt): UInt = x.rotateRight(6) ^ x.rotateRight(11) ^ x.rotateRight(25)

      def sum0maj: (UInt, UInt, UInt) => UInt = sum0majModule(EEW)

      def sum1ch: (UInt, UInt, UInt, UInt, UInt) => UInt = sum1chModule(EEW)

      def sig0(x: UInt): UInt = x.rotateRight(7) ^ x.rotateRight(18) ^ x.>>(3).asUInt

      def sig1(x: UInt): UInt = x.rotateRight(17) ^ x.rotateRight(19) ^ x.>>(10).asUInt
    }

    def ch(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ ((~x).asUInt & z)

    def maj(x: UInt, y: UInt, z: UInt): UInt = (x & y) ^ (x & z) ^ (y & z)

    def sum0(EEW: Int)(x: UInt): UInt = EEW match {
      case 32 => SHA256.sum0(x)
      case 64 => SHA512.sum0(x)
    }

    def sum1(EEW: Int)(x: UInt): UInt = EEW match {
      case 32 => SHA256.sum1(x)
      case 64 => SHA512.sum1(x)
    }
  }

  /**
   * Utils for AES
   */
  object Zvkned {
    def addRound128(state: UInt, rkey: UInt): UInt = {
      require(state.getWidth == 128 && rkey.getWidth == 128)

      val ark = state ^ rkey
      ark
    }

    def subBytes(state: UInt): UInt = {
      require(state.getWidth == 128)
      val bytes = state.splitToVec(num = 16, w = 8)
      Cat(bytes.map(B => kVAESXEncSBox(B)).reverse)
    }

    def subBytesInv(state: UInt): UInt = {
      require(state.getWidth == 128)
      val bytes = state.splitToVec(num = 16, w = 8)
      Cat(bytes.map(B => kVAESXDecSBox(B)).reverse)
    }

    //       col3 col2 col1 col0
    // row3   b15  b11  b7   b3
    // row2   b14  b10  b6   b2
    // row1   b13  b9   b5   b1
    // row0   b12  b8   b4   b0
    def shiftRows(state: UInt): UInt = {
      // matrix4x4(0,0): b0 -> state(7,0)
      // matrix4x4(0,1): b4 -> state(39:32)
      // ...
      val matrix4x4: Seq[Seq[UInt]] = state.splitToVec(num = 4, w = 32).map(_.splitToVec(num = 4, w = 8)).transpose
      val shiftedRows = matrix4x4.zipWithIndex.map {
        case (row, i) => VecInit(row).rotateDown(i)
      }
      Cat(shiftedRows.transpose.flatten.reverse)
    }

    def shiftRowsInv(state: UInt): UInt = {
      val matrix4x4: Seq[Seq[UInt]] = state.splitToVec(num = 4, w = 32).map(_.splitToVec(num = 4, w = 8)).transpose
      val shiftedRows = matrix4x4.zipWithIndex.map {
        case (row, i) => VecInit(row).rotateUp(i)
      }
      Cat(shiftedRows.transpose.flatten.reverse)
    }

    def mixColumns(state: UInt): UInt = {
      val cols = state.splitToVec(num = 4, w = 32)
      Cat(cols.map(mixColumn).reverse)
    }

    def mixColumnsInv(state: UInt): UInt = {
      val cols = state.splitToVec(num = 4, w = 32)
      Cat(cols.map(inMixColumn).reverse)
    }

    def mixColumn(col: UInt): UInt = {
      val bytes = col.splitToVec(num = 4, w = 8)
      val gfMul1s = VecInit(bytes.map(gfMul1))
      val gfMul2s = VecInit(bytes.map(gfMul2))
      val gfMul3s = VecInit(bytes.map(gfMul3))
      val b0 = gfMul2s(0) ^ gfMul3s(1) ^ gfMul1s(2) ^ gfMul1s(3)
      val b1 = gfMul2s(1) ^ gfMul3s(2) ^ gfMul1s(3) ^ gfMul1s(0)
      val b2 = gfMul2s(2) ^ gfMul3s(3) ^ gfMul1s(0) ^ gfMul1s(1)
      val b3 = gfMul2s(3) ^ gfMul3s(0) ^ gfMul1s(1) ^ gfMul1s(2)
      Cat(b3, b2, b1, b0)
    }

    def inMixColumn(col: UInt): UInt = {
      val bytes = col.splitToVec(num = 4, w = 8)
      val gfMul9s = VecInit(bytes.map(gfMul9))
      val gfMulBs = VecInit(bytes.map(gfMulB))
      val gfMulDs = VecInit(bytes.map(gfMulD))
      val gfMulEs = VecInit(bytes.map(gfMulE))
      val b0 = gfMulEs(0) ^ gfMulBs(1) ^ gfMulDs(2) ^ gfMul9s(3)
      val b1 = gfMulEs(1) ^ gfMulBs(2) ^ gfMulDs(3) ^ gfMul9s(0)
      val b2 = gfMulEs(2) ^ gfMulBs(3) ^ gfMulDs(0) ^ gfMul9s(1)
      val b3 = gfMulEs(3) ^ gfMulBs(0) ^ gfMulDs(1) ^ gfMul9s(2)
      Cat(b3, b2, b1, b0)
    }

    // It's just a fake zero(0)
    private val O = false.B

    def gfMul1(x: UInt) = x

    def gfMul2(x: UInt) = {
      //  lsb                                msb
      val a :: b :: c :: d :: e :: f :: g :: h :: Nil = x.asBools.toList
      // (x << 1).asUInt ^ Mux(x.head(1).asBool, 0x1b.U, 0.U)
      // 0 ^ x = x
      0.U ^
        Cat(g, f, e, d, c, b, a, O) ^
        Cat(O, O, O, h, h, O, h, h)
    }

    def gfMul4(x: UInt): UInt = {
      val a :: b :: c :: d :: e :: f :: g :: h :: Nil = x.asBools.toList
      0.U ^
        Cat(f, e, d, c, b, a, O, O) ^
        Cat(O, O, h, h, O, h, h, O) ^
        Cat(O, O, O, g, g, O, g, g)
    }

    def gfMul8(x: UInt): UInt = {
      val a :: b :: c :: d :: e :: f :: g :: h :: Nil = x.asBools.toList
      0.U ^
        Cat(e, d, c, b, a, O, O, O) ^
        Cat(O, h, h, O, h, h, O, O) ^
        Cat(O, O, g, g, O, g, g, O) ^
        Cat(O, O, O, f, f, O, f, f)
    }

    def gfMul3(x: UInt): UInt = gfMul2(x) ^ gfMul1(x)

    def gfMul9(x: UInt): UInt = gfMul8(x) ^ gfMul1(x)

    def gfMulB(x: UInt): UInt = gfMul8(x) ^ gfMul2(x) ^ gfMul1(x)

    def gfMulD(x: UInt): UInt = gfMul8(x) ^ gfMul4(x) ^ gfMul1(x)

    def gfMulE(x: UInt): UInt = gfMul8(x) ^ gfMul4(x) ^ gfMul2(x)

    val kVAESXEncSBox = VecInit(Seq(
      //        00    01    02    03    04    05    06    07    08    09    0A    0B    0C    0D    0E    0F
      /* 00 */ 0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
      /* 10 */ 0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
      /* 20 */ 0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
      /* 30 */ 0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
      /* 40 */ 0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
      /* 50 */ 0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
      /* 60 */ 0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
      /* 70 */ 0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
      /* 80 */ 0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
      /* 90 */ 0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
      /* A0 */ 0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
      /* B0 */ 0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
      /* C0 */ 0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
      /* D0 */ 0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
      /* E0 */ 0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
      /* F0 */ 0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16,
    ).map(_.U(8.W)))

    val kVAESXDecSBox = VecInit(Seq(
      //        00    01    02    03    04    05    06    07    08    09    0A    0B    0C    0D    0E    0F
      /* 00 */ 0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
      /* 10 */ 0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
      /* 20 */ 0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
      /* 30 */ 0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
      /* 40 */ 0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
      /* 50 */ 0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
      /* 60 */ 0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
      /* 70 */ 0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
      /* 80 */ 0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
      /* 90 */ 0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
      /* A0 */ 0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
      /* B0 */ 0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
      /* C0 */ 0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
      /* D0 */ 0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
      /* E0 */ 0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
      /* F0 */ 0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D,
    ).map(_.U(8.W)))
  }
}
