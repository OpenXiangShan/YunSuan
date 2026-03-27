package yunsuan.scalar

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.MULOpType

class MulInput(len: Int) extends Bundle {
  val fuOpType = UInt(5.W)
  val src = Vec(2, UInt(len.W))
}

class MulIO(len: Int) extends Bundle {
  val in = Input(ValidIO(new MulInput(len)))
  val out = Output(UInt(len.W))
}

class Mul(xlen: Int) extends Module {
  val io = IO(new MulIO(xlen))

  private val len = xlen

  val validS0 = io.in.valid
  val validS1 = GatedValidRegNext(validS0)
  val src = io.in.bits.src
  val func = io.in.bits.fuOpType
  val isW = MULOpType.isW(func)
  val isHi = MULOpType.isH(func)
  val isWS2 = RegEnable(RegEnable(isW, validS0), validS1)
  val isHiS2 = RegEnable(RegEnable(isHi, validS0), validS1)

  private val mulModuleS0 = Module(new MulModuleS0(len))
  mulModuleS0.io.in.a := src(0)
  mulModuleS0.io.in.b := src(1)
  mulModuleS0.io.in.isMul32 := isW
  mulModuleS0.io.in.isMul64 := ~isW
  mulModuleS0.io.in.aIsUnSigned := func(1) & func(0)
  mulModuleS0.io.in.bIsSigned := MULOpType.isSign(func)
  mulModuleS0.io.in.isHi := isHi

  private val ppOutS0 = Wire(Vec(4, UInt(128.W)))
  ppOutS0 := mulModuleS0.io.out.ppS0

  private val ppInS1 = Wire(Vec(4, UInt(128.W)))
  ppInS1 := RegEnable(ppOutS0, validS0)

  private val mulModuleS1 = Module(new MulModuleS1(len))
  mulModuleS1.io.in.ppIn := ppInS1

  private val resHighS1 = Wire(Vec(2, UInt(len.W)))
  private val resLowS1 = Wire(UInt((len + 1).W))
  resHighS1 := mulModuleS1.io.out.resHigh
  resLowS1 := mulModuleS1.io.out.resLow

  private val resHighS2 = Wire(Vec(2, UInt(len.W)))
  private val resLowS2 = Wire(UInt((len + 1).W))
  resHighS2 := RegEnable(resHighS1, validS1)
  resLowS2 := RegEnable(resLowS1, validS1)

  private val mulModuleS2 = Module(new MulModuleS2(len))
  mulModuleS2.io.in.resIn := resHighS2
  mulModuleS2.io.in.resLow := resLowS2
  mulModuleS2.io.in.isHi := isHiS2
  mulModuleS2.io.in.isW := isWS2

  private val result = Wire(UInt(len.W))
  result := mulModuleS2.io.out.result

  io.out := result
}

class Booth4 extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(3.W))
    val out = Output(UInt(4.W))
  })
  /**
   * 000  |   0100     0*M
   * 001  |   1001    +M*2i
   * 010  |   1001    +M*2i
   * 011  |   0101    +2M*2i
   * 100  |   0110    -2M*2i
   * 101  |   1010    -M*2i
   * 110  |   1010    -M*2i
   * 111  |   0100     0*M
   *
   * [3]: 1
   * [2]: 2
   * [1]: neg
   * [0]: pos
   */
  private val in = io.in
  private val code = Wire(Vec(4, Bool()))
  code(0) := (in(1) | in(0)) & ~in(2)
  code(1) := ((~in(1)).asBool | (~in(0)).asBool).asBool & in(2)
  code(2) := in(1) === in(0)
  code(3) := in(1) ^ in(0)
  io.out := code.asUInt
}

class PPGenBundleInput(len: Int) extends Bundle {
  val b = UInt(len.W)
  val booth = UInt(4.W)
  val sign = Bool()
}

class PPGenBundleOutput(len: Int) extends Bundle {
  val ppOut = UInt((len + 2).W)
  val ppCOut = Bool()
}

class PPGen(len: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new PPGenBundleInput(len))
    val out = Output(new PPGenBundleOutput(len))
  })
  val ppLen = len + 2
  val (b, sign, code) = (io.in.b, io.in.sign, io.in.booth)
  val (noShift, shift, bNeg, bPos) = (code(3), code(2), code(1), code(0))

  val ppPos = Wire(UInt(ppLen.W))
  val ppNeg = Wire(UInt(ppLen.W))

  ppPos := Fill(ppLen, shift) & Cat(sign, b, 0.U(1.W)) |
           Fill(ppLen, noShift) & Cat(sign, sign, b)

  ppNeg := Fill(ppLen, shift) & Cat(~sign, ~b, 1.U(1.W)) |
           Fill(ppLen, noShift) & Cat(~sign, ~sign, ~b)

  val pp = Wire(UInt(ppLen.W))
  pp := Fill(ppLen, bPos) & ppPos | Fill(ppLen, bNeg) & ppNeg

  io.out.ppOut := pp
  io.out.ppCOut := bNeg
}

class MulModuleS0Input(len: Int) extends Bundle {
  val a = UInt(len.W)
  val b = UInt(len.W)
  val isMul32 = Bool()
  val isMul64 = Bool()
  val aIsUnSigned = Bool()
  val bIsSigned = Bool()
  val isHi = Bool()
}

class MulModuleS0Output extends Bundle {
  val ppS0 = Vec(4, UInt(128.W))
}

class CSA8to2BundleInput extends Bundle {
  val ppIn = Vec(8, UInt(66.W))
  val ppCin = UInt(8.W)
  val isMul64 = Bool()
}

class CSA8to2BundleOutput extends Bundle {
  val ppOut = Vec(2, UInt(80.W))
  val cout48 = Bool()
  val cout80 = Bool()
}

class CSA8to2 extends Module {
  val io = IO(new Bundle {
    val in = Input(new CSA8to2BundleInput)
    val out = Output(new CSA8to2BundleOutput)
  })
  private val (pp0, pp1, pp2, pp3, pp4, pp5, pp6, pp7) = (io.in.ppIn(0), io.in.ppIn(1), io.in.ppIn(2), io.in.ppIn(3),
                                                          io.in.ppIn(4), io.in.ppIn(5), io.in.ppIn(6), io.in.ppIn(7))
  private val (ppC0, ppC1, ppC2, ppC3, ppC4, ppC5, ppC6, ppC7) = (io.in.ppCin(0), io.in.ppCin(1), io.in.ppCin(2),
                                                                  io.in.ppCin(3), io.in.ppCin(4), io.in.ppCin(5),
                                                                  io.in.ppCin(6), io.in.ppCin(7))
  private val isMul32 = !io.in.isMul64
  private val isMul64 = io.in.isMul64

  private val l0ppLen0 = 71
  private val l0ppLen1 = 73
  private val l0ppLen2 = 70

  private val l0pp0 = Wire(UInt(l0ppLen0.W))  // [70:0]
  private val l0pp1 = Wire(UInt(l0ppLen0.W))
  private val l0pp2 = Wire(UInt(l0ppLen0.W))
  private val l0pp3 = Wire(UInt(l0ppLen1.W))  // [76:4]
  private val l0pp4 = Wire(UInt(l0ppLen1.W))
  private val l0pp5 = Wire(UInt(l0ppLen1.W))
  private val l0pp6 = Wire(UInt(l0ppLen2.W))  // [79:10]
  private val l0pp7 = Wire(UInt(l0ppLen2.W))
  private val l0pp8 = Wire(UInt(l0ppLen2.W))

  private val l0pp0IsMul32 = Fill(38, isMul32) & Cat(0.U(35.W), ~pp0(33), pp0(33), pp0(33))
  private val l0pp1IsMul32 = Fill(38, isMul32) & Cat(0.U(34.W), 1.U(1.W), ~pp1(33), pp1(32, 31))
  private val l0pp2IsMul32 = Fill(38, isMul32) & Cat(0.U(32.W), 1.U(1.W), ~pp2(33), pp2(32, 29))

  private val l0pp3IsMul32 = Fill(44, isMul32) & Cat(0.U(36.W), 1.U(1.W), ~pp3(33), pp3(32, 27))
  private val l0pp4IsMul32 = Fill(44, isMul32) & Cat(0.U(34.W), 1.U(1.W), ~pp4(33), pp4(32, 25))
  private val l0pp5IsMul32 = Fill(44, isMul32) & Cat(0.U(32.W), 1.U(1.W), ~pp5(33), pp5(32, 23))

  private val l0pp6IsMul32 = Fill(47, isMul32) & Cat(0.U(33.W), 1.U(1.W), ~pp6(33), pp6(32, 21))
  private val l0pp7IsMul32 = Fill(47, isMul32) & Cat(0.U(32.W), ~pp7(33), pp7(32, 19))

  private val l0pp0IsMul64 = Fill(38, isMul64) & Cat(0.U(3.W), ~pp0(65), pp0(65), pp0(65), pp0(64, 33))
  private val l0pp1IsMul64 = Fill(38, isMul64) & Cat(0.U(2.W), 1.U(1.W), ~pp1(65), pp1(64, 31))
  private val l0pp2IsMul64 = Fill(38, isMul64) & Cat(1.U(1.W), ~pp2(65), pp2(64, 29))

  private val l0pp3IsMul64 = Fill(44, isMul64) & Cat(0.U(4.W), 1.U(1.W), ~pp3(65), pp3(64, 27))
  private val l0pp4IsMul64 = Fill(44, isMul64) & Cat(0.U(2.W), 1.U(1.W), ~pp4(65), pp4(64, 25))
  private val l0pp5IsMul64 = Fill(44, isMul64) & Cat(1.U(1.W), ~pp5(65), pp5(64, 23))

  private val l0pp6IsMul64 = Fill(47, isMul64) & Cat(0.U(1.W), 1.U(1.W), ~pp6(65), pp6(64, 21))
  private val l0pp7IsMul64 = Fill(47, isMul64) & Cat(~pp7(65), pp7(64, 19))

  l0pp0 := Cat(l0pp0IsMul32 | l0pp0IsMul64, pp0(32, 0))
  l0pp1 := Cat(l0pp1IsMul32 | l0pp1IsMul64, pp1(30, 0), 0.U(1.W), ppC0)
  l0pp2 := Cat(l0pp2IsMul32 | l0pp2IsMul64, pp2(28, 0), 0.U(1.W), ppC1, 0.U(2.W))

  l0pp3 := Cat(l0pp3IsMul32 | l0pp3IsMul64, pp3(26, 0), 0.U(1.W), ppC2)
  l0pp4 := Cat(l0pp4IsMul32 | l0pp4IsMul64, pp4(24, 0), 0.U(1.W), ppC3, 0.U(2.W))
  l0pp5 := Cat(l0pp5IsMul32 | l0pp5IsMul64, pp5(22, 0), 0.U(1.W), ppC4, 0.U(4.W))

  l0pp6 := Cat(l0pp6IsMul32 | l0pp6IsMul64, pp6(20, 0), 0.U(1.W), ppC5)
  l0pp7 := Cat(l0pp7IsMul32 | l0pp7IsMul64, pp7(18, 0), 0.U(1.W), ppC6, 0.U(2.W))
  l0pp8 := Cat(0.U(65.W), ppC7, 0.U(4.W))

  private val l1ppS0 = Wire(UInt(l0ppLen0.W))  // (70, 0)
  private val l1ppC0 = Wire(UInt(l0ppLen0.W))  // (71, 1)
  private val l1ppS1 = Wire(UInt(l0ppLen1.W))  // (76, 4)
  private val l1ppC1 = Wire(UInt(l0ppLen1.W))  // (77, 5)
  private val l1ppS2 = Wire(UInt(l0ppLen2.W))  // (79, 10)
  private val l1ppC2 = Wire(UInt((l0ppLen2 - 1).W))  // (79, 11)

  l1ppS0 := l0pp0 ^ l0pp1 ^ l0pp2
  l1ppC0 := l0pp0 & l0pp1 | l0pp0 & l0pp2 | l0pp1 & l0pp2
  l1ppS1 := l0pp3 ^ l0pp4 ^ l0pp5
  l1ppC1 := l0pp3 & l0pp4 | l0pp3 & l0pp5 | l0pp4 & l0pp5
  l1ppS2 := l0pp6 ^ l0pp7 ^ l0pp8
  l1ppC2 := l0pp6(l0ppLen2 - 2, 0) & l0pp7(l0ppLen2 - 2, 0) |
            l0pp6(l0ppLen2 - 2, 0) & l0pp8(l0ppLen2 - 2, 0) |
            l0pp7(l0ppLen2 - 2, 0) & l0pp8(l0ppLen2 - 2, 0)

  private val l1ppLen = 76
  private val l1pp0 = Wire(UInt(l1ppLen.W))
  private val l1pp1 = Wire(UInt(l1ppLen.W))
  private val l1pp2 = Wire(UInt(l1ppLen.W))
  private val l1pp3 = Wire(UInt(l1ppLen.W))
  private val l1pp4 = Wire(UInt(l1ppLen.W))
  private val l1pp5 = Wire(UInt(l1ppLen.W))

  l1pp0 := Cat(0.U(9.W), l1ppS0(l0ppLen0 - 1, 4))
  l1pp1 := Cat(0.U(3.W), l1ppS1)
  l1pp2 := Cat(l1ppS2, 0.U(6.W))
  l1pp3 := Cat(0.U(8.W), l1ppC0(l0ppLen0 - 1, 3))
  l1pp4 := Cat(0.U(2.W), l1ppC1, 0.U(1.W))
  l1pp5 := Cat(l1ppC2(l0ppLen2 - 2, 0), 0.U(7.W))

  private val l2ppS0 = Wire(UInt(l1ppLen.W))  // (79, 4)
  private val l2ppC0 = Wire(UInt(l1ppLen.W))  // (80, 5)
  private val l2ppS1 = Wire(UInt(l1ppLen.W))  // (79, 4)
  private val l2ppC1 = Wire(UInt(l1ppLen.W))  // (80, 5)

  l2ppS0 := l1pp0 ^ l1pp1 ^ l1pp2
  l2ppC0 := l1pp0 & l1pp1 | l1pp0 & l1pp2 | l1pp1 & l1pp2
  l2ppS1 := l1pp3 ^ l1pp4 ^ l1pp5
  l2ppC1 := l1pp3 & l1pp4 | l1pp3 & l1pp5 | l1pp4 & l1pp5

  private val l2ppLen = 75
  private val l2pp0 = Wire(UInt(l2ppLen.W))  // (79, 5)
  private val l2pp1 = Wire(UInt(l2ppLen.W))  // 0, (78, 5)
  private val l2pp2 = Wire(UInt(l2ppLen.W))  // (79, 5)
  private val l2pp3 = Wire(UInt(l2ppLen.W))  // 0, (78, 5)

  l2pp0 := l2ppS0(l1ppLen - 1, 1)
  l2pp1 := Cat(0.U(1.W), l2ppC0(l1ppLen - 3, 0))
  l2pp2 := l2ppS1(l1ppLen - 1, 1)
  l2pp3 := Cat(0.U(1.W), l2ppC1(l1ppLen - 3, 0))

  private val l3ppS = Wire(UInt(l2ppLen.W))  // (79, 5)
  private val l3ppC = Wire(UInt(l2ppLen.W))  // (80, 6)

  l3ppS := l2pp1 ^ l2pp2 ^ l2pp3
  l3ppC := l2pp1 & l2pp2 | l2pp1 & l2pp3 | l2pp2 & l2pp3

  private val l3ppLen = 74
  private val l3pp0 = Wire(UInt(l3ppLen.W))  // (79, 6)
  private val l3pp1 = Wire(UInt(l3ppLen.W))  // (79, 6)
  private val l3pp2 = Wire(UInt(l3ppLen.W))  // (79, 6)

  l3pp0 := l2pp0(l2ppLen - 1, 1)
  l3pp1 := l3ppS(l2ppLen - 1, 1)
  l3pp2 := l3ppC(l2ppLen - 2, 0)

  private val l4ppS = Wire(UInt(l3ppLen.W))  // (79, 6)
  private val l4ppC = Wire(UInt(l3ppLen.W))  // (80, 7)

  l4ppS := l3pp0 ^ l3pp1 ^ l3pp2
  l4ppC := l3pp0 & l3pp1 | l3pp0 & l3pp2 | l3pp1 & l3pp2

  private val ppOut0 = Wire(UInt(80.W))
  private val ppOut1 = Wire(UInt(80.W))
  ppOut0 := Cat(l4ppS, l2pp0(0), l2ppS0(0), l1ppS0(3, 0))
  ppOut1 := Cat(l4ppC(l3ppLen - 2, 0), 0.U(1.W), l3ppS(0), l2ppS1(0), l1ppC0(2, 0), 0.U(1.W))

  io.out.ppOut(0) := ppOut0
  io.out.ppOut(1) := ppOut1
  io.out.cout48 := l4ppC(41)
  io.out.cout80 := l4ppC(l3ppLen - 1)
}

class CSA9to2BundleInput extends Bundle {
  val ppIn = Vec(8, UInt(66.W))
  val pp8In = UInt(64.W)
  val ppCin = UInt(8.W)
  val isMul64 = Bool()
}

class CSA9to2BundleOutput extends CSA8to2BundleOutput

class CSA9to2 extends Module {
  val io = IO(new Bundle {
    val in = Input(new CSA9to2BundleInput)
    val out = Output(new CSA9to2BundleOutput)
  })
  private val (pp0, pp1, pp2, pp3, pp4, pp5, pp6, pp7) = (io.in.ppIn(0), io.in.ppIn(1), io.in.ppIn(2), io.in.ppIn(3),
    io.in.ppIn(4), io.in.ppIn(5), io.in.ppIn(6), io.in.ppIn(7))
  private val (ppC0, ppC1, ppC2, ppC3, ppC4, ppC5, ppC6, ppC7) = (io.in.ppCin(0), io.in.ppCin(1), io.in.ppCin(2),
    io.in.ppCin(3), io.in.ppCin(4), io.in.ppCin(5),
    io.in.ppCin(6), io.in.ppCin(7))
  private val pp8 = io.in.pp8In

  private val isMul32 = !io.in.isMul64
  private val isMul64 = io.in.isMul64

  private val l0ppLen0 = 71
  private val l0ppLen1 = 73
  private val l0ppLen2 = 70

  private val l0pp0 = Wire(UInt(l0ppLen0.W))
  private val l0pp1 = Wire(UInt(l0ppLen0.W))
  private val l0pp2 = Wire(UInt(l0ppLen0.W))
  private val l0pp3 = Wire(UInt(l0ppLen1.W))
  private val l0pp4 = Wire(UInt(l0ppLen1.W))
  private val l0pp5 = Wire(UInt(l0ppLen1.W))
  private val l0pp6 = Wire(UInt(l0ppLen2.W))
  private val l0pp7 = Wire(UInt(l0ppLen2.W))
  private val l0pp8 = Wire(UInt(l0ppLen2.W))

  private val l0pp0IsMul32 = Fill(38, isMul32) & Cat(0.U(35.W), ~pp0(33), pp0(33), pp0(33))
  private val l0pp1IsMul32 = Fill(38, isMul32) & Cat(0.U(34.W), 1.U(1.W), ~pp1(33), pp1(32, 31))
  private val l0pp2IsMul32 = Fill(38, isMul32) & Cat(0.U(32.W), 1.U(1.W), ~pp2(33), pp2(32, 29))

  private val l0pp3IsMul32 = Fill(44, isMul32) & Cat(0.U(36.W), 1.U(1.W), ~pp3(33), pp3(32, 27))
  private val l0pp4IsMul32 = Fill(44, isMul32) & Cat(0.U(34.W), 1.U(1.W), ~pp4(33), pp4(32, 25))
  private val l0pp5IsMul32 = Fill(44, isMul32) & Cat(0.U(32.W), 1.U(1.W), ~pp5(33), pp5(32, 23))

  private val l0pp6IsMul32 = Fill(47, isMul32) & Cat(0.U(33.W), 1.U(1.W), ~pp6(33), pp6(32, 21))
  private val l0pp7IsMul32 = Fill(47, isMul32) & Cat(0.U(32.W), ~pp7(33), pp7(32, 19))

  private val l0pp0IsMul64 = Fill(38, isMul64) & Cat(0.U(3.W), ~pp0(65), pp0(65), pp0(65), pp0(64, 33))
  private val l0pp1IsMul64 = Fill(38, isMul64) & Cat(0.U(2.W), 1.U(1.W), ~pp1(65), pp1(64, 31))
  private val l0pp2IsMul64 = Fill(38, isMul64) & Cat(1.U(1.W), ~pp2(65), pp2(64, 29))

  private val l0pp3IsMul64 = Fill(44, isMul64) & Cat(0.U(4.W), 1.U(1.W), ~pp3(65), pp3(64, 27))
  private val l0pp4IsMul64 = Fill(44, isMul64) & Cat(0.U(2.W), 1.U(1.W), ~pp4(65), pp4(64, 25))
  private val l0pp5IsMul64 = Fill(44, isMul64) & Cat(1.U(1.W), ~pp5(65), pp5(64, 23))

  private val l0pp6IsMul64 = Fill(47, isMul64) & Cat(0.U(1.W), 1.U(1.W), ~pp6(65), pp6(64, 21))
  private val l0pp7IsMul64 = Fill(47, isMul64) & Cat(~pp7(65), pp7(64, 19))

  l0pp0 := Cat(l0pp0IsMul32 | l0pp0IsMul64, pp0(32, 0))
  l0pp1 := Cat(l0pp1IsMul32 | l0pp1IsMul64, pp1(30, 0), 0.U(1.W), ppC0)
  l0pp2 := Cat(l0pp2IsMul32 | l0pp2IsMul64, pp2(28, 0), 0.U(1.W), ppC1, 0.U(2.W))

  l0pp3 := Cat(l0pp3IsMul32 | l0pp3IsMul64, pp3(26, 0), 0.U(1.W), ppC2)
  l0pp4 := Cat(l0pp4IsMul32 | l0pp4IsMul64, pp4(24, 0), 0.U(1.W), ppC3, 0.U(2.W))
  l0pp5 := Cat(l0pp5IsMul32 | l0pp5IsMul64, pp5(22, 0), 0.U(1.W), ppC4, 0.U(4.W))

  l0pp6 := Cat(l0pp6IsMul32 | l0pp6IsMul64, pp6(20, 0), 0.U(1.W), ppC5)
  l0pp7 := Cat(l0pp7IsMul32 | l0pp7IsMul64, pp7(18, 0), 0.U(1.W), ppC6, 0.U(2.W))
  l0pp8 := Cat(pp8, 0.U(1.W), ppC7, 0.U(4.W))

  private val l1ppS0 = Wire(UInt(l0ppLen0.W)) // (70, 0)
  private val l1ppC0 = Wire(UInt(l0ppLen0.W)) // (71, 1)
  private val l1ppS1 = Wire(UInt(l0ppLen1.W)) // (76, 4)
  private val l1ppC1 = Wire(UInt(l0ppLen1.W)) // (77, 5)
  private val l1ppS2 = Wire(UInt(l0ppLen2.W)) // (79, 10)
  private val l1ppC2 = Wire(UInt(l0ppLen2.W)) // (80, 11)

  l1ppS0 := l0pp0 ^ l0pp1 ^ l0pp2
  l1ppC0 := l0pp0 & l0pp1 | l0pp0 & l0pp2 | l0pp1 & l0pp2
  l1ppS1 := l0pp3 ^ l0pp4 ^ l0pp5
  l1ppC1 := l0pp3 & l0pp4 | l0pp3 & l0pp5 | l0pp4 & l0pp5
  l1ppS2 := l0pp6 ^ l0pp7 ^ l0pp8
  l1ppC2 := l0pp6 & l0pp7 | l0pp6 & l0pp8 | l0pp7 & l0pp8

  private val l1ppLen = 76
  private val l1pp0 = Wire(UInt(l1ppLen.W))
  private val l1pp1 = Wire(UInt(l1ppLen.W))
  private val l1pp2 = Wire(UInt(l1ppLen.W))
  private val l1pp3 = Wire(UInt(l1ppLen.W))
  private val l1pp4 = Wire(UInt(l1ppLen.W))
  private val l1pp5 = Wire(UInt(l1ppLen.W))

  l1pp0 := Cat(0.U(9.W), l1ppS0(l0ppLen0 - 1, 4))
  l1pp1 := Cat(0.U(3.W), l1ppS1)
  l1pp2 := Cat(l1ppS2, 0.U(6.W))
  l1pp3 := Cat(0.U(8.W), l1ppC0(l0ppLen0 - 1, 3))
  l1pp4 := Cat(0.U(2.W), l1ppC1, 0.U(1.W))
  l1pp5 := Cat(l1ppC2(l0ppLen2 - 2, 0), 0.U(7.W))

  private val l2ppS0 = Wire(UInt(l1ppLen.W)) // (79, 4)
  private val l2ppC0 = Wire(UInt(l1ppLen.W)) // (80, 5)
  private val l2ppS1 = Wire(UInt(l1ppLen.W)) // (79, 4)
  private val l2ppC1 = Wire(UInt(l1ppLen.W)) // (80, 5)

  l2ppS0 := l1pp0 ^ l1pp1 ^ l1pp2
  l2ppC0 := l1pp0 & l1pp1 | l1pp0 & l1pp2 | l1pp1 & l1pp2
  l2ppS1 := l1pp3 ^ l1pp4 ^ l1pp5
  l2ppC1 := l1pp3 & l1pp4 | l1pp3 & l1pp5 | l1pp4 & l1pp5

  private val l2ppLen = 75
  private val l2pp0 = Wire(UInt(l2ppLen.W)) // (79, 5)
  private val l2pp1 = Wire(UInt(l2ppLen.W)) // 0, (78, 5)
  private val l2pp2 = Wire(UInt(l2ppLen.W)) // (79, 5)
  private val l2pp3 = Wire(UInt(l2ppLen.W)) // 0, (78, 5)

  l2pp0 := l2ppS0(l1ppLen - 1, 1)
  l2pp1 := Cat(0.U(1.W), l2ppC0(l1ppLen - 3, 0))
  l2pp2 := l2ppS1(l1ppLen - 1, 1)
  l2pp3 := Cat(0.U(1.W), l2ppC1(l1ppLen - 3, 0))

  private val l3ppS = Wire(UInt(l2ppLen.W)) // (79, 5)
  private val l3ppC = Wire(UInt(l2ppLen.W)) // (80, 6)

  l3ppS := l2pp1 ^ l2pp2 ^ l2pp3
  l3ppC := l2pp1 & l2pp2 | l2pp1 & l2pp3 | l2pp2 & l2pp3

  private val l3ppLen = 74
  private val l3pp0 = Wire(UInt(l3ppLen.W)) // (79, 6)
  private val l3pp1 = Wire(UInt(l3ppLen.W)) // (79, 6)
  private val l3pp2 = Wire(UInt(l3ppLen.W)) // (79, 6)

  l3pp0 := l2pp0(l2ppLen - 1, 1)
  l3pp1 := l3ppS(l2ppLen - 1, 1)
  l3pp2 := l3ppC(l2ppLen - 2, 0)

  private val l4ppS = Wire(UInt(l3ppLen.W)) // (79, 6)
  private val l4ppC = Wire(UInt(l3ppLen.W)) // (80, 7)

  l4ppS := l3pp0 ^ l3pp1 ^ l3pp2
  l4ppC := l3pp0 & l3pp1 | l3pp0 & l3pp2 | l3pp1 & l3pp2

  private val ppOut0 = Wire(UInt(80.W))
  private val ppOut1 = Wire(UInt(80.W))

  ppOut0 := Cat(l4ppS, l2pp0(0), l2ppS0(0), l1ppS0(3, 0))
  ppOut1 := Cat(l4ppC(l3ppLen - 2, 0), 0.U(1.W), l3ppS(0), l2ppS1(0), l1ppC0(2, 0), 0.U(1.W))

  io.out.ppOut(0) := ppOut0
  io.out.ppOut(1) := ppOut1
  io.out.cout48 := l1ppC2(37) | l4ppC(41)
  io.out.cout80 := l1ppC2(l0ppLen2 - 1) | l4ppC(l3ppLen - 1)
}

class CSA4to2(len: Int) extends Module {
  val io = IO(new Bundle {
    val ppIn = Input(Vec(4, UInt(len.W)))
    val ppOut = Output(Vec(2, UInt(len.W)))
  })
  private val (pp0, pp1, pp2, pp3) = (io.ppIn(0), io.ppIn(1), io.ppIn(2), io.ppIn(3))

  private val l0ppS = Wire(UInt(len.W))
  private val l0ppC = Wire(UInt(len.W))

  l0ppS := pp0 ^ pp1 ^ pp2
  l0ppC := pp0 & pp1 | pp0 & pp2 | pp1 & pp2

  private val l1pp0 = Wire(UInt(len.W))
  private val l1pp1 = Wire(UInt(len.W))
  private val l1pp2 = Wire(UInt(len.W))

  l1pp0 := l0ppS
  l1pp1 := Cat(l0ppC(len - 2, 0), 0.U(1.W))
  l1pp2 := pp3

  private val l2ppS = Wire(UInt(len.W))
  private val l2ppC = Wire(UInt(len.W))

  l2ppS := l1pp0 ^ l1pp1 ^ l1pp2
  l2ppC := l1pp0 & l1pp1 | l1pp0 & l1pp2 | l1pp1 & l1pp2

  private val ppOut0 = Wire(UInt(len.W))
  private val ppOut1 = Wire(UInt(len.W))

  ppOut0 := l2ppS
  ppOut1 := Cat(l2ppC(len - 2, 0), 0.U(1.W))

  io.ppOut(0) := ppOut0
  io.ppOut(1) := ppOut1

}

class MulModuleS0(len: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new MulModuleS0Input(len))
    val out = Output(new MulModuleS0Output)
  })
  val (a, b) = (io.in.a, io.in.b)
  private val (isMul32, isMul64) = (io.in.isMul32, io.in.isMul64)
  private val (aIsUnSigned, bIsSigned) = (io.in.aIsUnSigned, io.in.bIsSigned)
  private val isHi = io.in.isHi
  private val signB = bIsSigned & Mux(isMul32, b(31), b(63))

  private val a0 = Wire(UInt(17.W))
  private val a1 = Wire(UInt(17.W))
  private val a2 = Wire(UInt(33.W))

  a0 := Cat(a(15, 0), 0.U)
  a1 := a(31, 15)
  a2 := Fill(33, isMul64) & a(63, 31)

  private val ppLen = len + 2
  private val boothCode0 = Wire(Vec(8, UInt(4.W)))
  private val boothCode1 = Wire(Vec(8, UInt(4.W)))
  private val boothCode2 = Wire(Vec(16, UInt(4.W)))

  private def genBooth4(in: UInt, num: Int): Vec[UInt] = {
    val boothCode = Wire(Vec(num, UInt(4.W)))
    for (i <- Range(0, num, 1)) {
      val booth4 = Module(new Booth4)
      booth4.io.in := in(i * 2 + 2, i * 2)
      boothCode(i) := booth4.io.out
    }
    boothCode
  }

  boothCode0 := genBooth4(a0, 8)
  boothCode1 := genBooth4(a1, 8)
  boothCode2 := genBooth4(a2, 16)

  private val b0 = Wire(UInt(len.W))
  private val b1 = Wire(UInt(len.W))
  private val b2 = Wire(UInt(len.W))
  b0 := b
  b1 := b
  b2 := Fill(len, isMul64) & b

  private val signOfUnSignAMul32 = isMul32 & aIsUnSigned & a(31)
  private val signOfUnSignAMul64 = isMul64 & aIsUnSigned & a(63)
  private val ppUnSignBMul32 = Fill(len, signOfUnSignAMul32) & b
  private val ppUnSignBMul64 = Fill(len, signOfUnSignAMul64) & b

  private def genPP(in: UInt, boothCode: Vec[UInt], signB: Bool, num: Int): (Vec[UInt], UInt) = {
    val ppOut = Wire(Vec(num, UInt(ppLen.W)))
    val ppCOut = Wire(Vec(num, Bool()))
    for (i <- Range(0, num, 1)) {
      val ppGen = Module(new PPGen(len))
      ppGen.io.in.b := in
      ppGen.io.in.booth := boothCode(i)
      ppGen.io.in.sign := signB
      ppOut(i) := ppGen.io.out.ppOut
      ppCOut(i) := ppGen.io.out.ppCOut
    }
    (ppOut, ppCOut.asUInt)
  }

  private val (pp0, ppC0) = genPP(b0, boothCode0, signB, 8)
  private val (pp1, ppC1) = genPP(b1, boothCode1, signB, 8)
  private val (pp2Tmp, ppC2Tmp) = genPP(b2, boothCode2, signB, 16)

  private val pp2 = VecInit.tabulate(8) { i => pp2Tmp(i) }
  private val pp3 = VecInit.tabulate(8) { i => pp2Tmp(i + 8) }
  private val ppC2 = ppC2Tmp(7, 0)
  private val ppC3 = ppC2Tmp(15, 8)

  private val cout48Vec = Wire(Vec(4, Bool()))
  private val cout80Vec = Wire(Vec(4, Bool()))
  private val ppVec = Wire(Vec(8, UInt(80.W)))

  private def genCSA8to2(ppIn: Vec[UInt], ppCin: UInt, isMul64: Bool): (Vec[UInt], Bool, Bool) = {
    val ppOut = Wire(Vec(2, UInt(80.W)))
    val cout48 = Wire(Bool())
    val cout80 = Wire(Bool())
    val c82 = Module(new CSA8to2)
    for (i <- 0 until 8) {
      c82.io.in.ppIn(i) := ppIn(i)
    }
    c82.io.in.ppCin := ppCin
    c82.io.in.isMul64 := isMul64
    for (i <- 0 until 2) {
      ppOut(i) := c82.io.out.ppOut(i)
    }
    cout48 := c82.io.out.cout48
    cout80 := c82.io.out.cout80
    (ppOut, cout48, cout80)
  }

  def genCSA9to2(ppIn: Vec[UInt], pp8In: UInt, ppCin: UInt, isMul64: Bool): (Vec[UInt], Bool, Bool) = {
    val ppOut = Wire(Vec(2, UInt(80.W)))
    val cout48 = Wire(Bool())
    val cout80 = Wire(Bool())
    val c92 = Module(new CSA9to2)
    for (i <- 0 until 8) {
      c92.io.in.ppIn(i) := ppIn(i)
    }
    c92.io.in.pp8In := pp8In
    c92.io.in.ppCin := ppCin
    c92.io.in.isMul64 := isMul64
    for (i <- 0 until 2) {
      ppOut(i) := c92.io.out.ppOut(i)
    }
    cout48 := c92.io.out.cout48
    cout80 := c92.io.out.cout80
    (ppOut, cout48, cout80)
  }

  private val (ppVec0, cout48Vec0, cout80Vec0) = genCSA8to2(pp0, ppC0, isMul64)
  private val (ppVec1, cout48Vec1, cout80Vec1) = genCSA9to2(pp1, ppUnSignBMul32, ppC1, isMul64)
  private val (ppVec2, cout48Vec2, cout80Vec2) = genCSA8to2(pp2, ppC2, true.B)
  private val (ppVec3, cout48Vec3, cout80Vec3) = genCSA9to2(pp3, ppUnSignBMul64, ppC3, true.B)

  ppVec := Cat(ppVec3.asUInt, ppVec2.asUInt, ppVec1.asUInt, ppVec0.asUInt).asTypeOf(ppVec)
  cout48Vec := Cat(cout48Vec3, cout48Vec2, cout48Vec1, cout48Vec0).asTypeOf(cout48Vec)
  cout80Vec := Cat(cout80Vec3, cout80Vec2, cout80Vec1, cout80Vec0).asTypeOf(cout80Vec)

  private val ppOutLen = 128
  private val ppa = WireInit(0.U(ppOutLen.W))
  private val ppb = WireInit(0.U(ppOutLen.W))
  private val ppc = WireInit(0.U(ppOutLen.W))
  private val ppd = WireInit(0.U(ppOutLen.W))
  private val ppe = WireInit(0.U(ppOutLen.W))
  private val ppf = WireInit(0.U(ppOutLen.W))
  private val ppg = WireInit(0.U(ppOutLen.W))
  private val pph = WireInit(0.U(ppOutLen.W))

  ppa := Mux(isMul64, Cat(cout80Vec(0), ppVec(0)), Cat(cout48Vec(0), ppVec(0)(47, 0)))
  ppb := Mux(isMul64, Cat(Fill(48, isHi), ppVec(1)), ppVec(1)(47, 0))
  ppc := Mux(isMul64, Cat(cout80Vec(1), ppVec(2), 0.U(16.W)), Cat(ppVec(2)(47, 0), 0.U(16.W)))
  ppd := Mux(isMul64, Cat(Fill(32, isHi), ppVec(3), 0.U(16.W)), Cat(ppVec(3)(47, 0), 0.U(16.W)))
  ppe := Mux(isMul64, Cat(cout80Vec(2), ppVec(4), 0.U(32.W)), 0.U)
  ppf := Mux(isMul64, Cat(Fill(16, isHi), ppVec(5), 0.U(32.W)), 0.U)
  ppg := Mux(isMul64, Cat(ppVec(6), 0.U(48.W)), 0.U)
  pph := Mux(isMul64, Cat(ppVec(7), 0.U(48.W)), 0.U)

  private def genCSA4to2(ppIn: Vec[UInt]): Vec[UInt] = {
    val ppOut = Wire(Vec(2, UInt(ppOutLen.W)))
    val c42 = Module(new CSA4to2(ppOutLen))
    c42.io.ppIn := ppIn
    for (i <- 0 until 2) {
      ppOut(i) := c42.io.ppOut(i)
    }
    ppOut
  }

  private val ppResult = Wire(Vec(4, UInt(ppOutLen.W)))

  private val ppInC42 = VecInit(VecInit(Seq(ppa, ppb, ppc, ppd)), VecInit(Seq(ppe, ppf, ppg, pph)))
  private val ppResult0 = genCSA4to2(ppInC42(0))
  private val ppResult1 = genCSA4to2(ppInC42(1))

  ppResult := Cat(ppResult1.asUInt, ppResult0.asUInt).asTypeOf(ppResult)

  io.out.ppS0 := ppResult

}

class MulModuleS1Input extends Bundle {
  val ppIn = Vec(4, UInt(128.W))
}

class MulModuleS1Output extends Bundle {
  val resHigh = Vec(2, UInt(64.W))
  val resLow = UInt(65.W)
}

class MulModuleS1(len: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new MulModuleS1Input)
    val out = Output(new MulModuleS1Output)
  })
  val resIn = io.in.ppIn

  private val ppLen = 128
  private val resS = Wire(UInt(ppLen.W))
  private val resC = Wire(UInt(ppLen.W))

  private val c42 = Module(new CSA4to2(128))
  c42.io.ppIn := resIn

  resS := c42.io.ppOut(0)
  resC := c42.io.ppOut(1)

  private val res = Wire(UInt((len + 1).W))
  res := resS.tail(len) +& resC.tail(len)

  private val resHigh0 = Wire(UInt(len.W))
  private val resHigh1 = Wire(UInt(len.W))
  resHigh0 := resS.head(len)
  resHigh1 := resC.head(len)

  io.out.resHigh(0) := resHigh0
  io.out.resHigh(1) := resHigh1
  io.out.resLow := res
}

class MulModuleS2Input(len: Int) extends Bundle {
  val resIn = Vec(2, UInt(len.W))
  val resLow = UInt((len + 1).W)
  val isHi = Bool()
  val isW = Bool()
}

class MulModuleS2Output(len: Int) extends Bundle {
  val result = UInt(len.W)
}

class MulModuleS2(len: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new MulModuleS2Input(len))
    val out = Output(new MulModuleS2Output(len))
  })
  val res0 = io.in.resIn(0)
  val res1 = io.in.resIn(1)
  val resLow = io.in.resLow
  val isHi = io.in.isHi
  val isW = io.in.isW

  private val resultTmp = Wire(UInt(len.W))
  resultTmp := res0 + res1 + resLow(len)

  private val result = Wire(UInt(len.W))
  result := Mux(isW, SignExt(resLow(31, 0), len),
    Mux(isHi, resultTmp, resLow))

  io.out.result := result
}
