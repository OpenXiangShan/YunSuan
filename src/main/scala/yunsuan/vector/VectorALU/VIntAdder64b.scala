
/**
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.4  vadc, vmadc, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     Part of 12.1
  *     Part of 12.2
  */
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._

class VIntAdder64b extends Module {
  val io = IO(new Bundle {
    val opcode = Input(new VAluOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val oldVd = Input(UInt(8.W))
    val isSub = Input(Bool())  // subtract
    val widen = Input(Bool())
    val widen_vs2 = Input(Bool())

    val vd = Output(UInt(64.W))
    val cmpOut = Output(UInt(8.W)) // compare or add-with-carry carry output
    val toFixP = Output(new AdderToFixP)
  })

  val opcode = io.opcode
  val srcTypeVs2 = io.srcType(0)
  val srcTypeVs1 = io.srcType(1)
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(io.vdType(1, 0))
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vmask = io.vmask
  val vm = io.info.vm
  val uopIdx = io.info.uopIdx
  val sub = io.isSub
  val signed = srcTypeVs1(3, 2) === 1.U
  val addWithCarry = opcode.isAddWithCarry

  // Widen vs1 & vs2
  val vs2_32b = Mux(uopIdx(0), vs2(63, 32), vs2(31, 0))
  val vs2Widen = Mux1H(eewVs1.oneHot(2, 0), Seq(8, 16, 32).map(sew => 
                           Cat(UIntSplit(vs2_32b, sew).map(BitsExtend(_, 2*sew, signed)).reverse)))
  val vs1_32b = Mux(uopIdx(0), vs1(63, 32), vs1(31, 0))
  val vs1Widen = Mux1H(eewVs1.oneHot(2,0), Seq(8, 16, 32).map(sew => 
                           Cat(UIntSplit(vs1_32b, sew).map(BitsExtend(_, 2*sew, signed)).reverse)))
  val vs2_adjust = Mux(io.widen_vs2, vs2Widen, vs2)
  // Subtract: bit negate
  val vs1_adjust = Mux(io.widen, vs1Widen, vs1) ^ Fill(64, sub)

  /**
    * Chain all eight 8bit-adders
    */
  class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +
                       Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(9), bits(8, 1))
  }
  val cin  = Wire(Vec(8, Bool()))
  val cout = Wire(Vec(8, Bool()))
  val vd   = Wire(Vec(8, UInt(8.W)))
  val carryIn = Wire(Vec(8, Bool()))
  // Adjust vmask. sew==32: 00000011 -> 00010001   sew==16: 00001111 -> 01010101
  val vmask_adjust = MuxCase(vmask, Seq(
    eewVs1.is16 -> Cat(false.B, vmask(3), false.B, vmask(2), false.B, vmask(1), false.B, vmask(0)),
    eewVs1.is32 -> Cat(0.U(3.W), vmask(1), 0.U(3.W), vmask(0))
  ))

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs1_adjust(8*i+7, 8*i), vs2_adjust(8*i+7, 8*i), cin(i))
    // Generate carry-in from sub and vmask(11.4 Add-with-Carry/Sub-with_Borrow)
    carryIn(i) := Mux(addWithCarry, Mux(vm, sub, vmask_adjust(i) ^ sub), sub)
    // Generate final carry-in: cin
    val eewCin = Wire(new SewOH)
    eewCin.oneHot := Mux(opcode.isAddSub, eewVd.oneHot, eewVs1.oneHot)
    if (i == 0) {
      cin(i) := carryIn(i)
    } else if (i == 4) {
      cin(i) := Mux(eewCin.is64, cout(i-1), carryIn(i))
    } else if (i % 2 == 0) {
      cin(i) := Mux(eewCin.is64 || eewCin.is32, cout(i-1), carryIn(i))
    } else {
      cin(i) := Mux(eewCin.is8, carryIn(i), cout(i-1))
    }
    // cin(i) := Mux1H(Mux(opcode.isAddSub, eewVd.oneHot, eewVs1.oneHot), Seq(1, 2, 4, 8).map(n => 
    //   if ((i % n) == 0) carryIn(i) else cout(i-1))
    // )
    cout(i) := adder_8b.cout
    vd(i) := adder_8b.out
  }

  /**
    * Integer Compare & Min/Max instructions
    */
  val lessThan_vec = Wire(Vec(8, Bool()))
  val equal_vec = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    lessThan_vec(i) := Mux(signed, (vs2(8*i+7) ^ vs1_adjust(8*i+7)) ^ cout(i), !cout(i))
    equal_vec(i) := vs2(8*i+7, 8*i) === vs1(8*i+7, 8*i)
  }
  val equal = equal_vec.asUInt
  val cmpEq = Mux1H(Seq(
    eewVs1.is8  -> equal,
    eewVs1.is16 -> Cat(Fill(2, equal(7, 6).andR), Fill(2, equal(5, 4).andR), Fill(2, equal(3, 2).andR), Fill(2, equal(1, 0).andR)),
    eewVs1.is32 -> Cat(Fill(4, equal(7, 4).andR), Fill(4, equal(3, 0).andR)),
    eewVs1.is64 -> Fill(8, equal.andR)
  ))
  val cmpNe = ~cmpEq
  val lessThan = lessThan_vec.asUInt
  val cmpResult = Mux1H(Seq(
    (opcode.isVmseq) -> cmpEq,
    (opcode.isVmsne) -> cmpNe,
    (opcode.isVmslt) -> lessThan,
    (opcode.isVmsle) -> (lessThan | cmpEq),
    (opcode.isVmsgt) -> ~(lessThan | cmpEq)
  ))

  //-------- Min/Max --------
  val minMaxResult = Wire(Vec(8, UInt(8.W)))
  val selectVs1 = lessThan_vec.map(_ === (opcode.isVmax))
  for (i <- 0 until 8) {
    val sel = Mux1H(Seq(
      eewVs1.is8  -> selectVs1(i),
      eewVs1.is16 -> selectVs1((i/2)*2+1),
      eewVs1.is32 -> selectVs1((i/4)*4+3),
      eewVs1.is64 -> selectVs1(7),
    ))
    minMaxResult(i) := Mux(sel, vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }

  io.vd := Mux(opcode.isVmin || opcode.isVmax, minMaxResult.asUInt, vd.asUInt)

  val cmpOut = Mux(addWithCarry, Mux(opcode.isVmsbc, ~(cout.asUInt), cout.asUInt), cmpResult)
  val cmpOutAdjust = Mux1H(Seq(
    eewVs1.is8  -> cmpOut,
    eewVs1.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
    eewVs1.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
    eewVs1.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
  ))
  io.cmpOut := Mux(addWithCarry, cmpOutAdjust,
    Cat(Seq.tabulate(8)(i => Mux(!vm && !vmask(i), Mux(io.info.ma, true.B, io.oldVd(i)), cmpOutAdjust(i))).reverse))

  //---- To Fixed-Point unit ----
  for (i <- 0 until 8) {
    io.toFixP.vs2H(i) := vs2(8*i+7)
    io.toFixP.vs1H(i) := vs1(8*i+7)
    io.toFixP.vd(i) := vd(i)
    io.toFixP.cout(i) := cout(i)
  }
}
