package yunsuan.vector.VectorFMA

import chisel3._
import chisel3.util._
import scala.collection.mutable.ListBuffer

private[VectorFMA] class VectorFMACSA3to2(width: Int) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val in_c = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
  })

  io.out_sum := io.in_a ^ io.in_b ^ io.in_c
  io.out_car := Cat(((io.in_a & io.in_b) | (io.in_a & io.in_c) | (io.in_b & io.in_c))(width - 2, 0), 0.U)
}

private[VectorFMA] class VectorFMACSA4to2(width: Int) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val in_c = Input(UInt(width.W))
    val in_d = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
  })

  val coutVec = Wire(Vec(width, UInt(1.W)))
  val sumVec = Wire(Vec(width, UInt(1.W)))
  val carryVec = Wire(Vec(width, UInt(1.W)))
  val cin0 = 0.U
  for (i <- 0 until width) {
    coutVec(i) := Mux(io.in_a(i) ^ io.in_b(i), io.in_c(i), io.in_a(i))
    if (i == 0) {
      sumVec(i) := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i)
      carryVec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cin0, io.in_d(i))
    } else {
      sumVec(i) := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i) ^ coutVec(i - 1)
      carryVec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), coutVec(i - 1), io.in_d(i))
    }
  }

  val sumTempVec = Wire(Vec(width, UInt(1.W)))
  val carryTempVec = Wire(Vec(width, UInt(1.W)))
  carryTempVec(0) := 0.U
  sumTempVec(0) := sumVec(0)
  for (i <- 1 until width) {
    if (i % 2 == 1) {
      carryTempVec(i) := sumVec(i)
      sumTempVec(i) := carryVec(i - 1)
    } else {
      carryTempVec(i) := carryVec(i - 1)
      sumTempVec(i) := sumVec(i)
    }
  }

  io.out_sum := sumTempVec.asUInt
  io.out_car := carryTempVec.asUInt
}

private[VectorFMA] class VectorFMABoothEncoder(
  width: Int = 53,
  isAddendExpand1bit: Boolean = true
) extends Module {
  val addendSeqWidth = if (isAddendExpand1bit) 2 * width + 1 else 2 * width
  val outNum = width / 2 + 1
  val io = IO(new Bundle {
    val in_a = Input(UInt(width.W))
    val in_b = Input(UInt(width.W))
    val is_fp64 = Input(Bool())
    val is_fp32 = Input(Bool())

    val out_pp = Output(Vec(outNum, UInt(addendSeqWidth.W)))
    val res_mul = Output(UInt((2 * width).W))
  })

  val inBCat = Mux(
    io.is_fp64,
    Cat(Fill(2 - (width % 2), 0.U), io.in_b, 0.U),
    Mux(
      io.is_fp32,
      Cat(0.U(2.W), io.in_b.head(24), 0.U(4.W), io.in_b.tail(29), 0.U),
      Cat(0.U, io.in_b(52, 42), 0.U(3.W), io.in_b(39, 29), 0.U(3.W), io.in_b(23, 13), 0.U(3.W), io.in_b(10, 0), 0.U)
    )
  )
  val boothSeq = Wire(Vec(outNum, UInt(3.W)))
  val booth4bitOnehot = Wire(Vec(outNum, UInt(4.W)))
  for (i <- 0 until outNum) {
    boothSeq(i) := inBCat(i * 2 + 2, i * 2)
    booth4bitOnehot(i) := 0.U
    switch(boothSeq(i)) {
      is("b001".U) { booth4bitOnehot(i) := "b1000".U }
      is("b010".U) { booth4bitOnehot(i) := "b1000".U }
      is("b011".U) { booth4bitOnehot(i) := "b0100".U }
      is("b100".U) { booth4bitOnehot(i) := "b0001".U }
      is("b101".U) { booth4bitOnehot(i) := "b0010".U }
      is("b110".U) { booth4bitOnehot(i) := "b0010".U }
    }
  }
  val ppSeqF64 = Wire(Vec(outNum, UInt((width + 1).W)))
  val ppSeqF32 = Wire(Vec(outNum, UInt((24 + 1).W)))
  val ppSeqF16 = Wire(Vec(outNum, UInt((11 + 1).W)))
  val signSeq = Wire(Vec(outNum, UInt(1.W)))
  for (i <- 0 until outNum) {
    signSeq(i) := booth4bitOnehot(i)(1) | booth4bitOnehot(i)(0)
    ppSeqF64(i) := Fill(width + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a) |
      Fill(width + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a, 0.U) |
      Fill(width + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a) |
      Fill(width + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a, 1.U)
    if (i <= 13) {
      ppSeqF32(i) :=
        Fill(24 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a(23, 0)) |
          Fill(24 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a(23, 0), 0.U) |
          Fill(24 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a(23, 0)) |
          Fill(24 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a(23, 0), 1.U)
    } else {
      ppSeqF32(i) :=
        Fill(24 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a.head(24)) |
          Fill(24 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a.head(24), 0.U) |
          Fill(24 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a.head(24)) |
          Fill(24 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a.head(24), 1.U)
    }
    if (i <= 6) {
      ppSeqF16(i) :=
        Fill(11 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a(10, 0)) |
          Fill(11 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a(10, 0), 0.U) |
          Fill(11 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a(10, 0)) |
          Fill(11 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a(10, 0), 1.U)
    } else if (i <= 13) {
      ppSeqF16(i) :=
        Fill(11 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a(23, 13)) |
          Fill(11 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a(23, 13), 0.U) |
          Fill(11 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a(23, 13)) |
          Fill(11 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a(23, 13), 1.U)
    } else if (i <= 20) {
      ppSeqF16(i) :=
        Fill(11 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a(39, 29)) |
          Fill(11 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a(39, 29), 0.U) |
          Fill(11 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a(39, 29)) |
          Fill(11 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a(39, 29), 1.U)
    } else {
      ppSeqF16(i) :=
        Fill(11 + 1, booth4bitOnehot(i)(3)) & Cat(0.U, io.in_a(52, 42)) |
          Fill(11 + 1, booth4bitOnehot(i)(2)) & Cat(io.in_a(52, 42), 0.U) |
          Fill(11 + 1, booth4bitOnehot(i)(1)) & Cat(1.U, ~io.in_a(52, 42)) |
          Fill(11 + 1, booth4bitOnehot(i)(0)) & Cat(~io.in_a(52, 42), 1.U)
    }
  }
  val addendSeqF64 = Wire(Vec(outNum, UInt(addendSeqWidth.W)))
  val addendSeqF32ToF64 = Wire(Vec(outNum, UInt(addendSeqWidth.W)))
  val addendSeqF16ToF64 = Wire(Vec(outNum, UInt(addendSeqWidth.W)))
  val addendSeqF32 = Wire(Vec(outNum, UInt(49.W)))
  val addendSeqF16 = Wire(Vec(outNum, UInt(23.W)))

  for (i <- 0 until 6) {
    val headFirstOneWidth = 11 - 4 - 2 * (i - 1)
    val tailZeroWidth = 2 * (i - 1)
    i match {
      case 0 => addendSeqF16(i) := Cat(0.U((11 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF16(i))
      case 1 => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1))
      case 4 => addendSeqF16(i) := Cat(1.U, ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 5 =>
        if (isAddendExpand1bit) addendSeqF16(i) := Cat(1.U, ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        else addendSeqF16(i) := Cat(ppSeqF16(i), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF16ToF64(i) := Cat(0.U(84.W), addendSeqF16(i))
  }
  addendSeqF16(6) := 0.U
  addendSeqF16ToF64(6) := Cat(0.U(84.W), addendSeqF16(6))
  for (i <- 7 until 13) {
    val headFirstOneWidth = 11 - 4 - 2 * (i - 1 - 7)
    val tailZeroWidth = 2 * (i - 1 - 7)
    i match {
      case 7 => addendSeqF16(i) := Cat(0.U((11 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF16(i))
      case 8 => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1))
      case 11 => addendSeqF16(i) := Cat(1.U, ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 12 =>
        if (isAddendExpand1bit) addendSeqF16(i) := Cat(1.U, ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        else addendSeqF16(i) := Cat(ppSeqF16(i), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF16ToF64(i) := Cat(0.U(58.W), addendSeqF16(i), 0.U(26.W))
  }
  addendSeqF16(13) := 0.U
  addendSeqF16ToF64(13) := Cat(0.U(58.W), addendSeqF16(13), 0.U(26.W))
  for (i <- 14 until 20) {
    val headFirstOneWidth = 11 - 4 - 2 * (i - 1 - 14)
    val tailZeroWidth = 2 * (i - 1 - 14)
    i match {
      case 14 => addendSeqF16(i) := Cat(0.U((11 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF16(i))
      case 15 => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1))
      case 18 => addendSeqF16(i) := Cat(1.U, ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 19 =>
        if (isAddendExpand1bit) addendSeqF16(i) := Cat(1.U, ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        else addendSeqF16(i) := Cat(ppSeqF16(i), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF16ToF64(i) := Cat(0.U(26.W), addendSeqF16(i), 0.U(58.W))
  }
  addendSeqF16(20) := 0.U
  addendSeqF16ToF64(20) := Cat(0.U(26.W), addendSeqF16(20), 0.U(58.W))
  for (i <- 21 until 27) {
    val headFirstOneWidth = 11 - 4 - 2 * (i - 1 - 21)
    val tailZeroWidth = 2 * (i - 1 - 21)
    i match {
      case 21 => addendSeqF16(i) := Cat(0.U((11 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF16(i))
      case 22 => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1))
      case 25 => addendSeqF16(i) := Cat(1.U, ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 26 =>
        if (isAddendExpand1bit) addendSeqF16(i) := Cat(1.U, ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        else addendSeqF16(i) := Cat(ppSeqF16(i), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addendSeqF16(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF16(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF16ToF64(i) := Cat(addendSeqF16(i), 0.U(84.W))
  }

  for (i <- 0 until 13) {
    val headFirstOneWidth = 24 - 4 - 2 * (i - 1)
    val tailZeroWidth = 2 * (i - 1)
    i match {
      case 0 => addendSeqF32(i) := Cat(0.U((24 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF32(i))
      case 1 => addendSeqF32(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1))
      case 11 => addendSeqF32(i) := Cat(1.U, ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 12 => addendSeqF32(i) := Cat(ppSeqF32(i).tail(1), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case _ => addendSeqF32(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF32ToF64(i) := Cat(0.U(58.W), addendSeqF32(i))
  }
  addendSeqF32(13) := 0.U
  addendSeqF32ToF64(13) := Cat(0.U(58.W), addendSeqF32(13))
  for (i <- 14 until 27) {
    val headFirstOneWidth = 24 - 4 - 2 * (i - 1 - 14)
    val tailZeroWidth = 2 * (i - 1 - 14)
    i match {
      case 14 => addendSeqF32(i) := Cat(0.U((24 - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF32(i))
      case 15 => addendSeqF32(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1))
      case 25 => addendSeqF32(i) := Cat(1.U, ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case 26 => addendSeqF32(i) := Cat(ppSeqF32(i).tail(1), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
      case _ => addendSeqF32(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF32(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
    addendSeqF32ToF64(i) := Cat(addendSeqF32(i), 0.U(58.W))
  }
  val outNumBeforeLast = outNum - 2
  val outNumLast = outNum - 1
  for (i <- 0 until outNum) {
    val headFirstOneWidth = width - 4 - 2 * (i - 1)
    val tailZeroWidth = 2 * (i - 1)
    i match {
      case 0 => addendSeqF64(i) := Cat(0.U((width - 4).W), ~signSeq(i), signSeq(i), signSeq(i), ppSeqF64(0))
      case 1 => addendSeqF64(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF64(i), 0.U, signSeq(i - 1))
      case `outNumBeforeLast` =>
        if (width % 2 == 0) {
          if (isAddendExpand1bit) addendSeqF64(i) := Cat(1.U, ~signSeq(i), ppSeqF64(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
          else addendSeqF64(i) := Cat(~signSeq(i), ppSeqF64(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        } else {
          addendSeqF64(i) := Cat(1.U, ~signSeq(i), ppSeqF64(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        }
      case `outNumLast` =>
        if (width % 2 == 0) addendSeqF64(i) := Cat(ppSeqF64(i).tail(1), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
        else if (isAddendExpand1bit) addendSeqF64(i) := Cat(1.U, ppSeqF64(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
        else addendSeqF64(i) := Cat(ppSeqF64(i), 0.U, signSeq(i - 1), 0.U((2 * (i - 1)).W))
      case _ => addendSeqF64(i) := Cat(1.U(headFirstOneWidth.W), ~signSeq(i), ppSeqF64(i), 0.U, signSeq(i - 1), 0.U(tailZeroWidth.W))
    }
  }
  io.out_pp := Mux(
    io.is_fp64,
    addendSeqF64,
    Mux(io.is_fp32, addendSeqF32ToF64, addendSeqF16ToF64)
  )
  io.res_mul := Mux(
    io.is_fp64,
    addendSeqF64.reduce(_ + _),
    Mux(
      io.is_fp32,
      addendSeqF32ToF64.reduce(_ + _) & "h7fffffffffffc01ffffffffffff".U,
      addendSeqF16ToF64.reduce(_ + _) & (("h1fffffc7fffff".U << 58).asUInt + "h1fffffc7fffff".U)
    )
  )
}

private[VectorFMA] class VectorFMACompressTo4(n: Int, width: Int) extends Module {
  require(n >= 4)

  val io = IO(new Bundle {
    val in = Input(Vec(n, UInt(width.W)))
    val out = Output(Vec(4, UInt(width.W)))
  })

  val inBuf = ListBuffer[UInt]()
  io.in.foreach(a => inBuf += a)
  val nextBuf = ListBuffer[UInt]()
  var nNext = n
  var csa4to2Num = if (nNext == 8 || nNext == 4) nNext / 4 else 0
  var csa3to2Num = if (nNext == 8 || nNext == 4) 0 else nNext / 3
  var remainder = nNext - csa4to2Num * 4 - csa3to2Num * 3

  while (nNext > 4) {
    nextBuf.remove(0, nextBuf.length)

    for (i <- 0 until csa4to2Num) {
      val csa4to2 = Module(new VectorFMACSA4to2(width = width))
      csa4to2.io.in_a := inBuf(i * 4 + 0)
      csa4to2.io.in_b := inBuf(i * 4 + 1)
      csa4to2.io.in_c := inBuf(i * 4 + 2)
      csa4to2.io.in_d := inBuf(i * 4 + 3)
      nextBuf += csa4to2.io.out_sum
      nextBuf += csa4to2.io.out_car
    }

    for (i <- 0 until csa3to2Num) {
      val csa3to2 = Module(new VectorFMACSA3to2(width = width))
      csa3to2.io.in_a := inBuf(i * 3 + 0)
      csa3to2.io.in_b := inBuf(i * 3 + 1)
      csa3to2.io.in_c := inBuf(i * 3 + 2)
      nextBuf += csa3to2.io.out_sum
      nextBuf += csa3to2.io.out_car
    }

    if (remainder == 1) nextBuf += inBuf.last
    if (remainder == 2) {
      nextBuf += inBuf(inBuf.length - 2)
      nextBuf += inBuf.last
    }

    inBuf.remove(0, inBuf.length)
    nextBuf.foreach(a => inBuf += a)
    nNext = inBuf.length
    csa4to2Num = if (nNext == 8 || nNext == 4) nNext / 4 else 0
    csa3to2Num = if (nNext == 8 || nNext == 4) 0 else nNext / 3
    remainder = nNext - csa4to2Num * 4 - csa3to2Num * 3
  }

  require(inBuf.length == 4)
  io.out := VecInit(inBuf.toSeq)
}
