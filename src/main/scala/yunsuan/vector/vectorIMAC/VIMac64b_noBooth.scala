package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._

/** 64-bit vector multiply and accumlation unit
 *  
 *  Support these instructions: 11.10, 11.12, 11.13, 11.14, 12.3
 */
class VIMac64b_noBooth extends Module {
  val io = IO(new Bundle {
    // val opcode = Input(new VIMacOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val oldVd = Input(UInt(64.W)) 
    val highHalf = Input(Bool())
    val isMacc = Input(Bool()) // (w)macc(nmsac)/madd(nmsub)
    val isSub = Input(Bool())
    val widen = Input(Bool())
    val isFixP = Input(Bool())

    val vd = Output(UInt(64.W))
    val vxsat = Output(UInt(8.W))
  })

  val vs2 = io.vs2
  val vs1 = io.vs1
  val oldVd = io.oldVd
  val vs2_is_signed = io.srcType(0)(2) // vs2 & vd should be signed numbers for vmadd/vnmsub
  val vs1_is_signed = io.srcType(1)(2)
  val eewVs2 = SewOH(io.srcType(0)(1, 0))
  val sew = eewVs2
  val uopIdx = io.info.uopIdx

  /**
   *  First pipeline stage:
   *    (1) Partial products generation (2) Wallace tree
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
  val partProdFinal = Wire(Vec(65+2, UInt(128.W)))
  for (i <- 0 until 64) {
    if (i % 8 != 0 | i == 0) {
      partProdFinal(i) := partProdSet0(i)
    // } else {
    } else if (i == 32) {
      partProdFinal(i) := Mux(vs1(i-1) && vs1_is_signed && !sew.is64, partProdSet0(i) | 
                              Mux1H(sew.oneHot(2, 0), Seq(8, 16, 32).map(n => 1.U << (2*i - 2*n))), partProdSet0(i))
    } else if (i % 16 == 0) {
      partProdFinal(i) := Mux(vs1(i-1) && vs1_is_signed && (sew.is16 || sew.is8), partProdSet0(i) | 
                              Mux1H(sew.oneHot(1, 0), Seq(8, 16).map(n => 1.U << (2*i - 2*n))), partProdSet0(i))
    } else {
      partProdFinal(i) := Mux(vs1(i-1) && vs1_is_signed && sew.is8, partProdSet0(i) | 1.U << (2*i - 16), partProdSet0(i))
    }
  }
  partProdFinal(64) := Mux(vs1(63) && vs1_is_signed, 
    Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => 1.U << (128 - 2*n))), 0.U)

  /**
   *  Wallace tree
   */
  // Add old_vd and its reverse+1 as an additional addend
  val oldVdReorg = Mux1H(sew.oneHot, Seq(8,16,32,64).map(sew => 
                   VecInit(UIntSplit(oldVd, sew).map(x => BitsExtend(x, 2*sew, false.B))).asUInt))
  partProdFinal(65) := Mux(io.isMacc, Mux(io.widen, Cat(oldVd, oldVd), Mux(io.isSub, ~oldVdReorg, oldVdReorg)), 0.U)
  partProdFinal(66) := Mux(io.isSub, Mux1H(sew.oneHot, Seq(8,16,32,64).map(sew =>
                           Fill(64/sew, 1.U((2*sew).W)))), 0.U)
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
                 Mux1H(Seq(sew.is8  -> "h7fff7fff7fff7fff7fff7fff7fff7fff".U(128.W),
                           sew.is16 -> "h7fffffff7fffffff7fffffff7fffffff".U(128.W),
                           sew.is32 -> "h7fffffffffffffff7fffffffffffffff".U(128.W),
                           sew.is64 -> ~0.U(128.W) ))
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
  val nAddendsSeq = nAddendsSeqGen(67)  // e.g., [33, 22, 15, ..., 3]

  // Perform all wallace stages.
  def wallaceStage(stageIdx: Int): Seq[UInt] = {
    stageIdx match {
      case 0 => reduce3to2(partProdFinal)
      case k => reduce3to2(wallaceStage(k-1))
    }
  }
  val wallaceOut = wallaceStage(nAddendsSeq.size - 1)  // Seq(2)(UInt(128.W))

  /**
   *  Second pipeline stage: 128 + 128
   */
  val sewS1 = RegNext(sew)
  val highHalfS1 = RegNext(io.highHalf)
  val widenS1 = RegNext(io.widen)
  val uopIdxS1 = RegNext(uopIdx)
  val wallaceOutReg = wallaceOut map {x => RegNext(x)}
  // Sum of final two 128b numbers
  class Adder_16b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +  Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(17), bits(16, 1))
  }
  val cin_wo = Wire(Vec(8, Bool()))
  val cout_wo = Wire(Vec(8, Bool()))
  val sum_wo = Wire(Vec(8, UInt(16.W)))
  for (i <- 0 until 8) {
    val adder_16b = new Adder_16b(wallaceOutReg(0)(16*i+15, 16*i), wallaceOutReg(1)(16*i+15, 16*i), cin_wo(i))
    cout_wo(i) := adder_16b.cout
    if (i == 0) {
      cin_wo(i) := false.B
    } else if (i == 4) {
      cin_wo(i) := Mux(sewS1.is64, cout_wo(i-1), false.B)
    } else if (i % 2 == 0) {
      cin_wo(i) := Mux(sewS1.is64 || sewS1.is32, cout_wo(i-1), false.B)
    } else {
      cin_wo(i) := Mux(sewS1.is8, false.B, cout_wo(i-1))
    }
    // cin_wo(i) := Mux1H(sewS1.oneHot, Seq(1,2,4,8).map(n => if (i % n == 0) false.B else cout_wo(i-1)))
    sum_wo(i) := adder_16b.out
  }

  /**
   *  Third pipeline stage
   *    (1) get vd  (2) vsmul  (3) vs1*vs2-vd --> -vs1*vs2+vd (vnmsac/vnmsub)
   */
  val walOut = Wire(UInt(128.W))
  walOut := RegNext(sum_wo.asUInt)
  val sewS2 = RegNext(sewS1)
  val highHalfS2 = RegNext(highHalfS1)
  val widenS2 = RegNext(widenS1)
  val uopIdxS2 = RegNext(uopIdxS1)
  val vxrmS2 = RegNext(RegNext(io.info.vxrm))
  val isSubS2 = RegNext(RegNext(io.isSub))
  val isFixPS2 = RegNext(RegNext(io.isFixP))
  val vdS2 = PriorityMux(Seq(
           (sewS2.is64 || widenS2) -> Mux(sewS2.is64 && highHalfS2 || widenS2 && uopIdxS2(0), 
                                          walOut(127, 64), walOut(63, 0)),
           sewS2.is32 -> VecInit(UIntSplit(walOut, 64).map(x => Mux(highHalfS2, x(63, 32), x(31, 0)))).asUInt,
           sewS2.is16 -> VecInit(UIntSplit(walOut, 32).map(x => Mux(highHalfS2, x(31, 16), x(15, 0)))).asUInt,
           sewS2.is8  -> VecInit(UIntSplit(walOut, 16).map(x => Mux(highHalfS2, x(15, 8), x(7, 0)))).asUInt
         ))
  // 12.3 vsmul
  val sat = UIntSplit(walOut, 16).map(x => x(15, 14) === 1.U) //Saturate
  val vxsat = Mux1H(Seq(
    sewS2.is8  -> Cat(sat.reverse),
    sewS2.is16 -> Cat(Fill(2, sat(7)), Fill(2, sat(5)), Fill(2, sat(3)), Fill(2, sat(1))),
    sewS2.is32 -> Cat(Fill(4, sat(7)), Fill(4, sat(3))),
    sewS2.is64 -> Fill(8, sat(7))
  ))
  val walOutRnd8 = VecInit(UIntSplit(walOut, 16).map(x => x(14, 7))).asUInt
  val walOutRnd16 = VecInit(UIntSplit(walOut, 32).map(x => x(30, 15))).asUInt
  val walOutRnd32 = VecInit(UIntSplit(walOut, 64).map(x => x(62, 31))).asUInt
  val walOutRnd64 = walOut(126, 63)
  val walOutRnd = Mux1H(sewS2.oneHot, Seq(walOutRnd8, walOutRnd16, walOutRnd32, walOutRnd64))
  def rndIncGen(v_d: Bool, v_d_1: Bool, tail: UInt): Bool = {
    Mux1H(Seq((vxrmS2 === 0.U) -> v_d_1,
              (vxrmS2 === 1.U) -> (v_d_1 && (tail =/= 0.U || v_d)),
              (vxrmS2 === 2.U) -> false.B,
              (vxrmS2 === 3.U) -> (!v_d && Cat(v_d_1, tail) =/= 0.U) ))
  }
  val rndInc8 = UIntSplit(walOut, 16).map(x => rndIncGen(x(7), x(6), x(5, 0)))
  val rndInc16 = UIntSplit(walOut, 32).map(x => rndIncGen(x(15), x(14), x(13, 0)))
  val rndInc32 = UIntSplit(walOut, 64).map(x => rndIncGen(x(31), x(30), x(29, 0)))
  val rndInc64 = rndIncGen(walOut(63), walOut(62), walOut(61, 0))
  val rndInc = Mux1H(Seq(
    sewS2.is8  -> Cat(rndInc8.reverse),
    sewS2.is16 -> Cat(false.B, rndInc16(3), false.B, rndInc16(2), false.B, rndInc16(1), false.B, rndInc16(0)),
    sewS2.is32 -> Cat(0.U(3.W), rndInc32(1), 0.U(3.W), rndInc32(0)),
    sewS2.is64 -> Cat(0.U(7.W), rndInc64)
  ))
  // cin is 1 bit carry-in
  class Adder_8b_rnd(in1: UInt, cin: Bool) {
    private val bits = in1 + cin.asUInt
    val (cout, out) = (in1 === "b1111_1111".U && cin, bits)
  }
  // Chain up eight 8-bit adders
  def Adder_chain_rnd(data: Seq[UInt], rndInc: UInt): Seq[UInt] = {
    val cin = Wire(Vec(8, Bool()))
    val cout = Wire(Vec(8, Bool()))
    val out = Wire(Vec(8, UInt(8.W)))
    for (i <- 0 until 8) {
      val adder_8b_rnd = new Adder_8b_rnd(data(i), cin(i))
      if (i == 0) {
        cin(i) := rndInc(i)
      } else if (i == 4) {
        cin(i) := Mux(sewS2.is64, cout(i-1), rndInc(i))
      } else if (i % 2 == 0) {
        cin(i) := Mux(sewS2.is64 || sewS2.is32, cout(i-1), rndInc(i))
      } else {
        cin(i) := Mux(sewS2.is8, rndInc(i), cout(i-1))
      }
      // cin(i) := Mux1H(sewS2.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) rndInc(i) else cout(i-1)))
      cout(i) := adder_8b_rnd.cout
      out(i) := adder_8b_rnd.out
    }
    out
  }
  // vs1*vs2-vd --> -vs1*vs2+vd (vnmsac/vnmsub)
  val adderChainData = Mux(isSubS2, ~vdS2, walOutRnd)
  val adderChainCin = Mux(isSubS2, ~0.U(8.W), rndInc)
  val adderChainOut = Adder_chain_rnd(UIntSplit(adderChainData, 8), adderChainCin)
  val vdFixP = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    // If this 8-bit portion is the highest bits of a SEW-bit element
    val highestBits = Mux1H(Seq(
      sewS2.is8  -> true.B,
      sewS2.is16 -> {if ((i % 2) == 1) true.B else false.B},
      sewS2.is32 -> {if ((i % 4) == 3) true.B else false.B},
      sewS2.is64 -> {if (i == 7) true.B else false.B},
    ))
    vdFixP(i) := Mux(vxsat(i), Cat(!highestBits, ~0.U(7.W)), adderChainOut(i))
  }

  io.vd := Mux(isFixPS2, vdFixP.asUInt, Mux(isSubS2, VecInit(adderChainOut).asUInt, vdS2))
  io.vxsat := Mux(isFixPS2, vxsat, 0.U)
}