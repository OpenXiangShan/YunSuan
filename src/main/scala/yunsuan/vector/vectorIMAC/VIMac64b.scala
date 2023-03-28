package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._

/** 64-bit vector multiply and accumlation unit
 *  
 *  Support these instructions: 11.10, 11.12, 11.13, 11.14, 12.3
 */
class VIMac64b extends Module {
  val io = IO(new Bundle {
    // val opcode = Input(new VIMacOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val oldVd = Input(UInt(8.W)) 
    val highHalf = Input(Bool())
    val isMacc = Input(Bool())
    val isMadd = Input(Bool())
    val isSub = Input(Bool())
    val widen = Input(Bool())
    val isFixP = Input(Bool())

    val vd = Output(UInt(64.W))
    val vxsat = Output(Bool())
  })

  val vs2 = io.vs2
  val vs1 = io.vs1
  val vs2_is_signed = io.srcType(0)(2)
  val vs1_is_signed = io.srcType(1)(2)
  val eewVs2 = SewOH(io.srcType(0)(1, 0))
  val sew = eewVs2

  /**
   *  First pipeline: partial products generation
   */
  val partProd = Wire(Vec(64, UInt(128.W)))
  partProd(0) := Mux(!vs1(0), 0.U, Mux1H(sew.oneHot, Seq(8,16,32,64).map(k => 
                 Cat(Fill(128-k, vs2_is_signed && vs2(k-1)), vs2(k-1, 0)))))
  for (i <- 1 until 64) {
    partProd(i) := Mux(!vs1(i), 0.U, Mux1H(sew.oneHot, Seq(8,16,32,64).map(k => 
                   Cat(Fill(128-k-i-i/k*k, vs2_is_signed && vs2(i/k * k + k-1)), vs2(i/k * k + k-1, i/k * k), 0.U((i + i/k * k).W)))))
  }
  // If the highest bit of signed vs1 element is 1, reverse partial product and +1 (+1 is handled in next next code block)
  val partProdReverse = Wire(Vec(64, UInt(128.W)))
  for (i <- 0 until 64) {
    if (i % 8 != 7) {
      partProdReverse(i) := partProd(i)
    } else if (i == 63) {
      partProdReverse(i) := Mux(vs1(i) && vs1_is_signed, ~partProd(i), partProd(i))
    } else if (i == 31) {
      partProdReverse(i) := Mux(vs1(i) && vs1_is_signed && !sew.is64, ~partProd(i), partProd(i))
    } else if (i % 16 == 15) {
      partProdReverse(i) := Mux(vs1(i) && vs1_is_signed && !sew.is64 && !sew.is32, ~partProd(i), partProd(i))
    } else {
      partProdReverse(i) := Mux(vs1(i) && vs1_is_signed && sew.is8, ~partProd(i), partProd(i))
    }
  }
  // Set zero zone to seperate different elements
  val partProdSet0_sew8 = Wire(Vec(64, UInt(128.W)))
  val partProdSet0_sew16 = Wire(Vec(64, UInt(128.W)))
  val partProdSet0_sew32 = Wire(Vec(64, UInt(128.W)))
  for (i <- 0 until 64) {
    val eIdx8 = i / 8  // element idx
    if (eIdx8 == 0) {partProdSet0_sew8(i) := Cat(0.U(112.W), partProdReverse(i)(15,0))}
    else if (eIdx8 == 7) {partProdSet0_sew8(i) := Cat(partProdReverse(i)(127,112), 0.U(112.W))}
    else {partProdSet0_sew8(i) := Cat(0.U((112-eIdx8*16).W), partProdReverse(i)(eIdx8*16+15, eIdx8*16), 0.U((eIdx8*16).W))}
    val eIdx16 = i / 16  // element idx
    if (eIdx16 == 0) {partProdSet0_sew16(i) := Cat(0.U(96.W), partProdReverse(i)(31,0))}
    else if (eIdx16 == 3) {partProdSet0_sew16(i) := Cat(partProdReverse(i)(127,96), 0.U(96.W))}
    else {partProdSet0_sew16(i) := Cat(0.U((96-eIdx16*32).W), partProdReverse(i)(eIdx16*32+31, eIdx16*32), 0.U((eIdx16*32).W))}
    val eIdx32 = i / 32  // element idx
    if (eIdx32 == 0) {partProdSet0_sew32(i) := Cat(0.U(64.W), partProdReverse(i)(63,0))}
    else {partProdSet0_sew32(i) := Cat(partProdReverse(i)(127,64), 0.U(64.W))}
  }
  val partProdSet0 = Mux1H(Seq(
    sew.is8  -> partProdSet0_sew8,
    sew.is16  -> partProdSet0_sew16,
    sew.is32  -> partProdSet0_sew32,
    sew.is64  -> partProdReverse
  ))
  // Handle +1 operation upon reverse result
  def set1b(data: UInt, position: Int): UInt = {
    val result = Wire(Vec(128, Bool()))
    for (i <- 0 until 128) {
      if (i == position) {result(i) := true.B}
      else {result(i) := data(i)}
    }
    result.asUInt
  }
  val partProdFinal = Reg(Vec(65, UInt(128.W)))
  for (i <- 0 until 64) {
    if (i % 8 != 0 | i == 0) {
      partProdFinal(i) := partProdSet0(i)
    } else {
      partProdFinal(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n =>
      if (i % n == 0) Mux(vs1(i-1) && vs1_is_signed, set1b(partProdSet0(i), 2*i - 2*n), partProdSet0(i))
      else partProdSet0(i)))
    }
  }
  partProdFinal(64) := Mux(vs1(63) && vs1_is_signed, 
    Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => set1b(0.U(128.W), 128 - 2*n))), 0.U)

  /**
   *  Second pipeline: wallace tree
   */
  val sewS1 = RegNext(sew)
  val highHalfS1 = RegNext(io.highHalf)
  // 3-bit full-adder
  def add3_UInt(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val cout = (a & b) | (b & c) | (a & c)
    val sum = a ^ b ^ c
    (cout, sum)
  }
  // ---- Perform wallace reduction once (3 -> 2) ----
  // Input data: n x 128    Output: ([n/3]*2 + n%3) x 128
  def reduce3to2(data: Seq[UInt]): Seq[UInt] = {
    val nAddends = data.size
    val n3Group = nAddends / 3
    val cout = Seq.fill(n3Group)(Wire(UInt(128.W)))
    val sum = Seq.fill(n3Group)(Wire(UInt(128.W)))
    for (i <- 0 until n3Group) {
      cout(i) := add3_UInt(data(3*i), data(3*i+1), data(3*i+2))._1 &
                 Mux1H(Seq(sewS1.is8  -> "h7fff7fff7fff7fff7fff7fff7fff7fff".U(128.W),
                           sewS1.is16 -> "h7fffffff7fffffff7fffffff7fffffff".U(128.W),
                           sewS1.is32 -> "h7fffffffffffffff7fffffffffffffff".U(128.W),
                           sewS1.is64 -> ~0.U(128.W) ))
      sum(i) := add3_UInt(data(3*i), data(3*i+1), data(3*i+2))._2
    }
    val cin = cout.map(x => Cat(x(126, 0), 0.U(1.W)))
    sum ++ cin ++ data.drop(3 * n3Group)
  }

  // Generate an array to store number of addends of each wallace stage (e.g., [33, 22, 15, ..., 3])
  def nAddendsSeqGen(num_of_addends: Int): Seq[Int] = {
    num_of_addends match {
      case 3 => Seq(3)
      case n => n +: nAddendsSeqGen((n / 3) * 2 + n % 3)
    }
  }
  val nAddendsSeq = nAddendsSeqGen(65)  // e.g., [33, 22, 15, ..., 3]

  // Perform all wallace stages.
  def wallaceStage(stageIdx: Int): Seq[UInt] = {
    stageIdx match {
      case 0 => reduce3to2(partProdFinal)
      case k => reduce3to2(wallaceStage(k-1))
    }
  }
  val wallaceOut = wallaceStage(nAddendsSeq.size - 1)  // Seq(2)(UInt(128.W))

  // Sum of final two 128b numbers
  class Adder_16b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +  Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(17), bits(16, 1))
  }
  val cin_wo = Wire(Vec(8, Bool()))
  val cout_wo = Wire(Vec(8, Bool()))
  val sum_wo = Wire(Vec(8, UInt(16.W)))
  for (i <- 0 until 8) {
    val adder_16b = new Adder_16b(wallaceOut(0)(16*i+15, 16*i), wallaceOut(1)(16*i+15, 16*i), cin_wo(i))
    cout_wo(i) := adder_16b.cout
    cin_wo(i) := Mux1H(sewS1.oneHot, Seq(1,2,4,8).map(n => if (i % n == 0) false.B else cout_wo(i-1)))
    sum_wo(i) := adder_16b.out
  }
  val walOut= sum_wo.asUInt

  io.vd := Mux1H(Seq(
           sewS1.is8  -> VecInit(UIntSplit(walOut, 16).map(x => Mux(highHalfS1, x(15, 8) ,x(7, 0)))).asUInt,
           sewS1.is16 -> VecInit(UIntSplit(walOut, 32).map(x => Mux(highHalfS1, x(31, 16) ,x(15, 0)))).asUInt,
           sewS1.is32 -> VecInit(UIntSplit(walOut, 64).map(x => Mux(highHalfS1, x(63, 32) ,x(31, 0)))).asUInt,
           sewS1.is64 -> Mux(highHalfS1, walOut(127, 64), walOut(63, 0))
         ))
  io.vxsat := false.B
}