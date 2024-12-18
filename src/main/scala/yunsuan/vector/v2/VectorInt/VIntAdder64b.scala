package yunsuan.vector.v2.VectorInt

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

/**
  * Supported ops: normal add, sub. widen, cmp, carry
  * vd: maxmin, add, sub
  * cmpOut: cmp, madc. The result is compressed toward LSB
  * toFixP: result of adder
  */
class VIntAdder64b extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val sewOH = SewOH()

      // vadd: opClass all 0s, op.sub = 0
      // vsbc: opClass.carry = 1, op.sub = 1
      val opClass = new Bundle {
        val widen, widen2 = Bool()
        val carry = Bool()
        val minmax = Bool()
      }
      val op = new Bundle {
        // 0: add, 1: sub
        val sub = Bool()
        // 0: max, 1: min
        val min = Bool()

        val eq = Bool()
        val ne = Bool()
        val lt = Bool()
        val le = Bool()
        val gt = Bool()
      }

      val signed = Bool()
      val vm = Bool()
      val vs1 = UInt(64.W)
      val vs2 = UInt(64.W)
      val vs2Widen = UInt(64.W)
      val vs1Widen = UInt(64.W)
      val vmask = UInt(8.W)
    })
    val out = Output(new Bundle {
      val vd = Output(UInt(64.W))
      // compare or add-with-carry carry output
      val cmpOutE8  = Output(UInt(8.W))
      val cmpOutE16 = Output(UInt(4.W))
      val cmpOutE32 = Output(UInt(2.W))
      val cmpOutE64 = Output(UInt(1.W))
      val toFixP = Output(new AdderToFixP)
    })
  })

  val sewOH = io.in.sewOH
  val opClass = io.in.opClass
  val op = io.in.op
  val vs1 = io.in.vs1
  val vs2 = io.in.vs2
  val vmask = io.in.vmask
  val vm = io.in.vm
  val sub = op.sub
  val signed = io.in.signed
  val addWithCarry = opClass.carry

  // Widen vs1 & vs2
  val vs2Widen = io.in.vs2Widen
  val vs1Widen = io.in.vs1Widen
  val vs2_adjust = Mux(opClass.widen2, vs2Widen, vs2)
  // Subtract: bit negate
  val vs1_adjust = Mux(opClass.widen || opClass.widen2, vs1Widen, vs1) ^ Fill(64, sub)

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
    sewOH.is16 -> Cat(false.B, vmask(3), false.B, vmask(2), false.B, vmask(1), false.B, vmask(0)),
    sewOH.is32 -> Cat(0.U(3.W), vmask(1), 0.U(3.W), vmask(0))
  ))

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs1_adjust(8*i+7, 8*i), vs2_adjust(8*i+7, 8*i), cin(i))
    // Generate carry-in from sub and vmask(11.4 Add-with-Carry/Sub-with_Borrow)
    carryIn(i) := Mux(addWithCarry, Mux(vm, sub, vmask_adjust(i) ^ sub), sub)
    // Generate final carry-in: cin
    val eewCin = Wire(new SewOH)
    eewCin := Mux(opClass.widen || opClass.widen2, sewOH.toWiden, sewOH)
    if (i == 0) {
      cin(i) := carryIn(i)
    } else if (i == 4) {
      cin(i) := Mux(eewCin.is64, cout(i-1), carryIn(i))
    } else if (i % 2 == 0) {
      cin(i) := Mux(eewCin.is64 || eewCin.is32, cout(i-1), carryIn(i))
    } else {
      cin(i) := Mux(eewCin.is8, carryIn(i), cout(i-1))
    }
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
    sewOH.is8  -> equal,
    sewOH.is16 -> Cat(Fill(2, equal(7, 6).andR), Fill(2, equal(5, 4).andR), Fill(2, equal(3, 2).andR), Fill(2, equal(1, 0).andR)),
    sewOH.is32 -> Cat(Fill(4, equal(7, 4).andR), Fill(4, equal(3, 0).andR)),
    sewOH.is64 -> Fill(8, equal.andR)
  ))
  val cmpNe = ~cmpEq
  val lessThan = lessThan_vec.asUInt
  val cmpResult = Mux1H(Seq(
    (op.eq) -> cmpEq,
    (op.ne) -> cmpNe,
    (op.lt) -> lessThan,
    (op.le) -> (lessThan | cmpEq),
    (op.gt) -> ~(lessThan | cmpEq)
  ))

  //-------- Min/Max --------
  val minMaxResult = Wire(Vec(8, UInt(8.W)))
  val selectVs1 = lessThan_vec.map(_ === (!op.min))
  for (i <- 0 until 8) {
    val sel = Mux1H(Seq(
      sewOH.is8  -> selectVs1(i),
      sewOH.is16 -> selectVs1((i/2)*2+1),
      sewOH.is32 -> selectVs1((i/4)*4+3),
      sewOH.is64 -> selectVs1(7),
    ))
    minMaxResult(i) := Mux(sel, vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }

  io.out.vd := Mux(opClass.minmax, minMaxResult.asUInt, vd.asUInt)

  val cmpOut = Mux(addWithCarry, Mux(op.sub, (~cout.asUInt).asUInt, cout.asUInt), cmpResult)
  val cmpOutAdjust = Mux1H(Seq(
    sewOH.is8  -> cmpOut,
    sewOH.is16 -> Cat(~(0.U(4.W)), cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1)),
    sewOH.is32 -> Cat(~(0.U(6.W)), cmpOut(7), cmpOut(3)),
    sewOH.is64 -> Cat(~(0.U(7.W)), cmpOut(7))
  ))

  io.out.cmpOutE8  := cmpOut
  io.out.cmpOutE16 := Cat(cmpOut(7), cmpOut(5), cmpOut(3), cmpOut(1))
  io.out.cmpOutE32 := Cat(cmpOut(7), cmpOut(3))
  io.out.cmpOutE64 := Cat(cmpOut(7))

  //---- To Fixed-Point unit ----
  for (i <- 0 until 8) {
    io.out.toFixP.vs2H(i) := vs2(8*i+7)
    io.out.toFixP.vs1H(i) := vs1(8*i+7)
    io.out.toFixP.vd(i) := vd(i)
    io.out.toFixP.cout(i) := cout(i)
  }
}

object VIntAdder64bMain extends App {
  println("Generating the VIntAdder64b hardware")
  emitVerilog(new VIntAdder64b(), Array("--target-dir", "build/vector", "--throw-on-first-error", "--full-stacktrace"))
  println("done")
}
