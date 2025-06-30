/**
  * Integer multiplier to perform:
  *   Two 12*12 UInt multiplications, OR one 24*24 UInt multiplication
  */

package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._

class IntMUL_12_24 extends Module {
  val io = IO(new Bundle {
    val valid_in = Input(Bool())
    val a_in = Input(UInt(24.W))
    val b_in = Input(UInt(24.W))
    val is_16 = Input(Bool())
    val valid_out = Output(Bool())
    val res_out = Output(UInt(48.W))
  })

  val vs2 = io.a_in
  val vs1 = io.b_in
  val eleWidthOH = Seq(io.is_16, !io.is_16)

  /**
   *  First pipeline stage:
   *    (1) Booth-recoding partial products generation (2) Wallace tree (part)
   */
  // Radix-4 Booth recoding
  class boothRecode(d: UInt) {
    val positive = !d(2) && d(1,0) =/= 0.U
    val negative = d(2) && d(1,0) =/= 3.U
    val one = d(1) =/= d(0)
    val double = d(2) =/= d(1) && d(1) === d(0)
  }
  val vs1Booth3b = Wire(Vec(12, UInt(3.W)))
  vs1Booth3b(0) := Cat(vs1(1, 0), false.B)

  for (i <- 1 until 12) {
    if (i % 6 != 0) {
      vs1Booth3b(i) := vs1(2*i+1, 2*i-1)
    } else {
      vs1Booth3b(i) := Mux(!io.is_16, vs1(2*i+1, 2*i-1), Cat(vs1(2*i+1, 2*i), false.B))
    }
  }
  val vs1Booth = Seq.tabulate(12)(i => new boothRecode(vs1Booth3b(i)))
  // Partial products
  val partProd = Wire(Vec(12+1, UInt(48.W)))
  def calcPartProd(i: Int, sew: Int) = {
      val blockIdx = (2*i)/sew
      val elementVs2 = UIntSplit(vs2, sew)(blockIdx)
      val elementVs2L = elementVs2.pad(2*sew) // width: 2*sew
      val boothDouble = Wire(UInt((2*sew).W))
      boothDouble := Mux1H(Seq(vs1Booth(i).one    -> elementVs2L,
                               vs1Booth(i).double -> (elementVs2L << 1)))
      val boothResult = Mux1H(Seq(vs1Booth(i).positive -> boothDouble,
                                  vs1Booth(i).negative -> (~boothDouble)))
      val shiftBits = 2 * i - sew * blockIdx
      val shifted = (boothResult << shiftBits)(2*sew-1, 0)
      if (sew == 24) {shifted}
      else if (blockIdx == 0) {Cat(0.U((48-(blockIdx+1)*sew*2).W), shifted)}
      else if (blockIdx == 24/sew - 1) {Cat(shifted, 0.U((blockIdx*sew*2).W))}
      else {Cat(0.U((48-(blockIdx+1)*sew*2).W), shifted, 0.U((blockIdx*sew*2).W))}
  }
  for (i <- 0 until 12) {
    partProd(i) := Mux1H(eleWidthOH, Seq(12, 24).map(sew => calcPartProd(i, sew)))
  }
  // Generate an extra addend (partProd(12)) to:
  // (1) Handle +1 for all negative numbers
  // (2) Handle unsinged vs1
  def blockPlus1(sew: Int, booth_neg: UInt, highVs1: Bool, data: UInt): UInt = {
    val hi = Wire(UInt(sew.W)) //Handle unsinged vs1  
    hi := Mux(highVs1, data, 0.U)
    val lo = Wire(UInt(sew.W)) //Handle +1 for all negative numbers
    lo := VecInit(Seq.tabulate(sew/2)(i => Cat(false.B, booth_neg(i)))).asUInt
    Cat(hi, lo)
  }
  val plusUnsignedSew24 = Wire(UInt(48.W))
  val plusUnsignedSew12 = Wire(UInt(48.W))
  val booth_neg = VecInit(vs1Booth.map(_.negative)).asUInt
  plusUnsignedSew24 := blockPlus1(24, booth_neg, vs1(23), vs2)
  plusUnsignedSew12 := VecInit(Seq.tabulate(2)(i => blockPlus1(12, UIntSplit(booth_neg, 6)(i),
                               UIntSplit(vs1, 12)(i)(11), UIntSplit(vs2, 12)(i)))).asUInt
  partProd(12) := Mux1H(eleWidthOH, Seq(plusUnsignedSew12, plusUnsignedSew24))

  /**
   *  Wallace tree
   */
  // 3-bit full-adder
  def add3_UInt(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val cout = (a & b) | (b & c) | (a & c)
    val sum = a ^ b ^ c
    (cout, sum)
  }
  // ---- Perform wallace reduction once (3 -> 2) ----
  // Input data: n x 48    Output: ([n/3]*2 + n%3) x 48
  def reduce3to2(data: Seq[UInt]): Seq[UInt] = {
    val nAddends = data.size
    val n3Group = nAddends / 3
    val cout = Seq.fill(n3Group)(Wire(UInt(48.W)))
    val sum = Seq.fill(n3Group)(Wire(UInt(48.W)))
    for (i <- 0 until n3Group) {
      val temp_result = add3_UInt(data(3*i), data(3*i+1), data(3*i+2))
      cout(i) := temp_result._1 &
                 Mux1H(Seq(io.is_16  -> "h7fffff7fffff".U(48.W),
                           !io.is_16 -> "hffffffffffff".U(48.W) ))
      sum(i) := temp_result._2
    }
    val cin = cout.map(x => Cat(x(46, 0), 0.U(1.W)))
    sum ++ cin ++ data.drop(3 * n3Group)
  }

  // Generate an array to store number of addends of each wallace stage (e.g., [13, 9, 6, 4, 3])
  def nAddendsSeqGen(num_of_addends: Int): Seq[Int] = {
    num_of_addends match {
      case 3 => Seq(3)
      case n => n +: nAddendsSeqGen((n / 3) * 2 + n % 3)
    }
  }
  val nAddendsSeq = nAddendsSeqGen(13)  // e.g., [13, 9, 6, 4, 3]

  // Perform all wallace stages.
  def wallaceStage(stageIdx: Int): Seq[UInt] = {
    stageIdx match {
      case 0 => reduce3to2(partProd)
      case k => reduce3to2(wallaceStage(k-1))
    }
  }
  val wallaceOut = wallaceStage(nAddendsSeq.size - 1)  // Seq(2)(UInt(48.W))

  /**
   *  Second pipeline stage: 48 + 48
   */
  val is16S1 = RegEnable(io.is_16, io.valid_in)
  val wallaceOutReg = wallaceOut map {x => RegEnable(x, io.valid_in)}

  //---- Sum of final two 48b numbers ----
  // 1. Low 24 bits sum
  val lowSum25 = Wire(UInt(25.W))
  lowSum25 := wallaceOutReg(0)(23, 0) +& wallaceOutReg(1)(23, 0)
  // 2. High 24 bits sum
  val highSum25 = Cat(wallaceOutReg(0)(47, 24), Mux(is16S1, false.B, lowSum25(24))) +
                  Cat(wallaceOutReg(1)(47, 24), Mux(is16S1, false.B, lowSum25(24)))
  val highSum24 = highSum25(24, 1)

  io.valid_out := RegNext(io.valid_in)
  io.res_out := Cat(highSum24, lowSum25(23, 0))
}