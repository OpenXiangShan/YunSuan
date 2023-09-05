package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._

class Reduction extends Module {
  val VLEN = 128
  val xLen = 64
  val NLanes = VLEN / 64
  val vlenb = VLEN / 8
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = Output(new VIFuOutput)
    val alu_in = ValidIO(new VIFuInput)
  })

  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.old_vd
  val vmask = io.in.bits.mask
  val opcode = io.in.bits.opcode
  val srcTypeVs2 = io.in.bits.srcType(0)
  val srcTypeVs1 = io.in.bits.srcType(1)
  val vdType = io.in.bits.vdType
  val vm = io.in.bits.info.vm
  val ma = io.in.bits.info.ma
  val ta = io.in.bits.info.ta
  val vlmul = io.in.bits.info.vlmul
  val vl = io.in.bits.info.vl
  val uopIdx = io.in.bits.info.uopIdx
  val fire = io.in.valid

  val vsew = srcTypeVs2(1, 0)
  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val vd_vsew = vdType(1, 0)
  val vd_vsew_bytes = 1.U << vd_vsew
  val vd_vsew_bits = 8.U << vd_vsew
  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val vd_mask = (~0.U(VLEN.W))

  val vredsum_vs = opcode.isVredsum
  val vredmax_vs = opcode.isVredmax && srcTypeVs2(2).asBool
  val vredmaxu_vs = opcode.isVredmax && !srcTypeVs2(2).asBool
  val vredmin_vs = opcode.isVredmin && srcTypeVs2(2).asBool
  val vredminu_vs = opcode.isVredmin && !srcTypeVs2(2).asBool
  val vredand_vs = opcode.isVredand
  val vredor_vs = opcode.isVredor
  val vredxor_vs = opcode.isVredxor
  val vwredsum_vs = opcode.isVredsum && srcTypeVs2(2).asBool && (vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U))
  val vwredsumu_vs = opcode.isVredsum && !srcTypeVs2(2).asBool && (vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U))
  val vred_uop = opcode.isReduction

  def zero(w: Int) = 0.U(w.W)

  def umax(w: Int) = ~(0.U(w.W))

  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))

  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  val vs12 = Cat(vs1, vs2)
  val ele64 = Wire(UInt(64.W))

  ele64 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele64 := Mux1H(eewVs2.oneHot, Seq(8, 16, 32, 64).map(n => smin(n)))
    }.elsewhen(vredmin_vs) {
      ele64 := Mux1H(eewVs2.oneHot, Seq(8, 16, 32, 64).map(n => smax(n)))
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele64 := Mux1H(eewVs2.oneHot, Seq(8, 16, 32, 64).map(n => umax(n)))
    }
  }

  val shift = Mux(widen, uopIdx << vsew_plus1, uopIdx << (vsew_plus1 + 1.U))
  val vmask_bits = vmask >> shift
  vlRemain := Mux(vl >= shift, vl - shift, 0.U)

  val reduction_uop = !widen && ((uopIdx === 0.U) ||
    ((vlmul === 2.U) && (uopIdx < 2.U)) ||
    ((vlmul === 3.U) && (uopIdx < 4.U)))

  val widen_reduction_uop = widen && ((uopIdx === 0.U) ||
    ((vlmul === 1.U) && (uopIdx < 2.U)) ||
    ((vlmul === 2.U) && (uopIdx < 4.U)) ||
    ((vlmul === 3.U) && (uopIdx < 8.U)))

  val alu_uop = !widen && (((vlmul === 1.U) && (uopIdx < 1.U)) ||
    ((vlmul === 2.U) && (uopIdx < 3.U)) ||
    ((vlmul === 3.U) && (uopIdx < 7.U)))

  val widen_alu_uop = widen && ((uopIdx === 0.U) ||
    ((vlmul === 1.U) && (uopIdx < 3.U)) ||
    ((vlmul === 2.U) && (uopIdx < 7.U)) ||
    ((vlmul === 3.U) && (uopIdx < 15.U)))

  val vs12_bytes = VecInit(Seq.tabulate(2 * vlenb)(i => vs12((i + 1) * 8 - 1, i * 8)))
  val vs12m_bytes = Wire(Vec(2 * vlenb, UInt(8.W)))
  val vs12m_bits = Cat(vs12m_bytes.reverse)

  for (i <- 0 until 2 * vlenb) {
    vs12m_bytes(i) := vs12_bytes(i)
    when(((!vm && !vmask_bits(i.U / vsew_bytes)) || (i.U >= vlRemainBytes)) && (reduction_uop || widen_reduction_uop)) {
      when(vsew === 0.U) {
        vs12m_bytes(i) := ele64(7, 0)
      }.elsewhen(vsew === 1.U) {
        vs12m_bytes(i) := ele64((i % 2 + 1) * 8 - 1, (i % 2) * 8)
      }.elsewhen(vsew === 2.U) {
        vs12m_bytes(i) := ele64((i % 4 + 1) * 8 - 1, (i % 4) * 8)
      }.elsewhen(vsew === 3.U) {
        vs12m_bytes(i) := ele64((i % 8 + 1) * 8 - 1, (i % 8) * 8)
      }
    }
  }

  io.alu_in.valid := (alu_uop || widen_alu_uop) && fire && vred_uop
  io.alu_in.bits.opcode.op := VAluOpcode.vadd
  when(vredsum_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vadd
  }.elsewhen(vredmax_vs || vredmaxu_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vmax
  }.elsewhen(vredmin_vs || vredminu_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vmin
  }.elsewhen(vredand_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vand
  }.elsewhen(vredor_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vor
  }.elsewhen(vredxor_vs) {
    io.alu_in.bits.opcode.op := VAluOpcode.vxor
  }

  io.alu_in.bits.info.vm := true.B
  io.alu_in.bits.info.ma := true.B
  io.alu_in.bits.info.ta := true.B
  io.alu_in.bits.info.vlmul := 0.U
  io.alu_in.bits.info.vl := VLEN.U / vd_vsew_bits
  io.alu_in.bits.info.vstart := 0.U
  io.alu_in.bits.info.uopIdx := 0.U
  io.alu_in.bits.info.vxrm := 0.U

  io.alu_in.bits
    .srcType(0) := Mux(widen && !widen_reduction_uop, Mux(signed, srcTypeVs2 - 3.U, srcTypeVs2 + 1.U), srcTypeVs2)
  io.alu_in.bits
    .srcType(1) := Mux(widen && !widen_reduction_uop, Mux(signed, srcTypeVs2 - 3.U, srcTypeVs2 + 1.U), srcTypeVs2)
  io.alu_in.bits.vdType := Mux(widen && signed && !widen_reduction_uop, vdType - 4.U, vdType)
  io.alu_in.bits.vs1 := Mux(widen_reduction_uop, vs12m_bits(VLEN - 1, VLEN / 2), vs12m_bits(2 * VLEN - 1, VLEN))
  io.alu_in.bits.vs2 := Mux(widen_reduction_uop, vs12m_bits(VLEN / 2 - 1, 0), vs12m_bits(VLEN - 1, 0))
  io.alu_in.bits.old_vd := 0.U
  io.alu_in.bits.mask := 0.U

  class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +
      Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(9), bits(8, 1))
  }

  val vs1_zero = Wire(UInt(64.W))
  val vs1_zero_logical = Wire(UInt(64.W))
  val cin0 = Wire(Vec(8, Bool()))
  val cout0 = Wire(Vec(8, Bool()))
  val vs_hi = Wire(Vec(5, UInt(64.W)))
  val vs_lo = Wire(Vec(5, UInt(64.W)))
  val vs_lo_adjust = Wire(Vec(5, UInt(64.W)))

  val vd = Wire(Vec(5, Vec(8, UInt(8.W))))
  val vd_max = Wire(Vec(5, Vec(8, UInt(8.W))))
  val less = Wire(Vec(5, Vec(8, Bool())))
  val sel_lo = Wire(Vec(5, Vec(8, Bool())))
  val vd_logical = Wire(Vec(4, UInt(64.W)))

  vs1_zero := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0))

  vs1_zero_logical := Mux1H(
    eewVd.oneHot,
    Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0)
  )
  when(vredand_vs) {
    vs1_zero_logical := Mux1H(
      eewVd.oneHot,
      Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 1.U), vs1(n - 1, 0))) :+ vs1(63, 0)
    )
  }

  for (i <- 0 until 5) {
    vs_hi(i) := 0.U
    vs_lo(i) := 0.U
    vs_lo_adjust(i) := 0.U
  }

  for (i <- 0 until 5) {
    for (j <- 0 until 8) {
      vd(i)(j) := 0.U
      vd_max(i)(j) := 0.U
      less(i)(j) := false.B
      sel_lo(i)(j) := false.B
    }
  }

  for (i <- 0 until 4) {
    vd_logical(i) := 0.U
  }

  when(vredand_vs) {
    vd_logical(0) := vs1_zero_logical & vs12m_bits(127, 64) & vs12m_bits(63, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(0) := vs1_zero_logical | vs12m_bits(127, 64) | vs12m_bits(63, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(0) := vs1_zero_logical ^ vs12m_bits(127, 64) ^ vs12m_bits(63, 0)
  }

  when(vredand_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) & vd_logical(0)(31, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) | vd_logical(0)(31, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(1) := vd_logical(0)(63, 32) ^ vd_logical(0)(31, 0)
  }

  when(vredand_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) & vd_logical(1)(15, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) | vd_logical(1)(15, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(2) := vd_logical(1)(31, 16) ^ vd_logical(1)(15, 0)
  }

  when(vredand_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) & vd_logical(2)(7, 0)
  }.elsewhen(vredor_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) | vd_logical(2)(7, 0)
  }.elsewhen(vredxor_vs) {
    vd_logical(3) := vd_logical(2)(15, 8) ^ vd_logical(2)(7, 0)
  }

  val sub = opcode.isVredmax || opcode.isVredmin
  vs_hi(0) := vs12m_bits(127, 64)
  vs_lo(0) := vs12m_bits(63, 0)
  // Subtract: bit negate
  vs_lo_adjust(0) := vs12m_bits(63, 0) ^ Fill(64, sub)

  for (i <- 0 until 8) {
    less(0)(i) := Mux(signed, (vs_hi(0)(8 * i + 7) ^ vs_lo_adjust(0)(8 * i + 7)) ^ cout0(i), !cout0(i))
  }

  sel_lo(0) := less(0).map(_ === opcode.isVredmax)

  for (i <- 0 until 8) {
    val sel0 = Mux1H(
      Seq(
        eewVd.is8 -> sel_lo(0)(i),
        eewVd.is16 -> sel_lo(0)((i / 2) * 2 + 1),
        eewVd.is32 -> sel_lo(0)((i / 4) * 4 + 3),
        eewVd.is64 -> sel_lo(0)(7)
      )
    )
    vd_max(0)(i) := Mux(sel0, vs_lo(0)(8 * i + 7, 8 * i), vs_hi(0)(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs_lo_adjust(0)(8 * i + 7, 8 * i), vs_hi(0)(8 * i + 7, 8 * i), cin0(i))
    cin0(i) := Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) sub else cout0(i - 1)))
    cout0(i) := adder_8b.cout
    vd(0)(i) := adder_8b.out
  }

  val data_max1 = Mux(sub, Cat(vd_max(0).reverse), Cat(vd(0).reverse))
  val vi1 =
    Mux(vd_vsew === 3.U, Cat(vs1_zero, data_max1), Cat(0.U(32.W), data_max1(63, 32), 0.U(32.W), data_max1(31, 0)))
  val cin1 = Wire(Vec(8, Bool()))
  val cout1 = Wire(Vec(8, Bool()))
  vs_hi(1) := vi1(127, 64)
  vs_lo(1) := vi1(63, 0)
  // Subtract: bit negate
  vs_lo_adjust(1) := vi1(63, 0) ^ Fill(64, sub)

  for (i <- 0 until 8) {
    less(1)(i) := Mux(signed, (vs_hi(1)(8 * i + 7) ^ vs_lo_adjust(1)(8 * i + 7)) ^ cout1(i), !cout1(i))
  }

  sel_lo(1) := less(1).map(_ === opcode.isVredmax)

  for (i <- 0 until 8) {
    val sel1 = Mux1H(
      Seq(
        eewVd.is8 -> sel_lo(1)(i),
        eewVd.is16 -> sel_lo(1)((i / 2) * 2 + 1),
        eewVd.is32 -> sel_lo(1)((i / 4) * 4 + 3),
        eewVd.is64 -> sel_lo(1)(7)
      )
    )
    vd_max(1)(i) := Mux(sel1, vs_lo(1)(8 * i + 7, 8 * i), vs_hi(1)(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 8) {
    val adder_8b = new Adder_8b(vs_lo_adjust(1)(8 * i + 7, 8 * i), vs_hi(1)(8 * i + 7, 8 * i), cin1(i))
    cin1(i) := Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) sub else cout1(i - 1)))
    cout1(i) := adder_8b.cout
    vd(1)(i) := adder_8b.out
  }

  val data_max2 = Mux(sub, Cat(vd_max(1).reverse)(31, 0), Cat(vd(1).reverse)(31, 0))
  val vi2 = Mux(
    vd_vsew === 2.U,
    Cat(vs1_zero(31, 0), data_max2),
    Cat(0.U(16.W), data_max2(31, 16), 0.U(16.W), data_max2(15, 0))
  )
  val cin2 = Wire(Vec(4, Bool()))
  val cout2 = Wire(Vec(4, Bool()))
  vs_hi(2) := vi2(63, 32)
  vs_lo(2) := vi2(31, 0)
  // Subtract: bit negate
  vs_lo_adjust(2) := vi2(31, 0) ^ Fill(32, sub)

  for (i <- 0 until 4) {
    less(2)(i) := Mux(signed, (vs_hi(2)(8 * i + 7) ^ vs_lo_adjust(2)(8 * i + 7)) ^ cout2(i), !cout2(i))
  }

  sel_lo(2) := less(2).map(_ === opcode.isVredmax)

  for (i <- 0 until 4) {
    val sel2 = Mux1H(
      Seq(
        eewVd.is8 -> sel_lo(2)(i),
        eewVd.is16 -> sel_lo(2)((i / 2) * 2 + 1),
        eewVd.is32 -> sel_lo(2)((i / 4) * 4 + 3)
      )
    )
    vd_max(2)(i) := Mux(sel2, vs_lo(2)(8 * i + 7, 8 * i), vs_hi(2)(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 4) {
    val adder_8b = new Adder_8b(vs_lo_adjust(2)(8 * i + 7, 8 * i), vs_hi(2)(8 * i + 7, 8 * i), cin2(i))
    cin2(i) := Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) sub else cout2(i - 1)))
    cout2(i) := adder_8b.cout
    vd(2)(i) := adder_8b.out
  }

  val data_max3 = Mux(sub, Cat(vd_max(2).reverse)(15, 0), Cat(vd(2).reverse)(15, 0))
  val vi3 =
    Mux(vd_vsew === 1.U, Cat(vs1_zero(15, 0), data_max3), Cat(0.U(8.W), data_max3(15, 8), 0.U(8.W), data_max3(7, 0)))
  val cin3 = Wire(Vec(2, Bool()))
  val cout3 = Wire(Vec(2, Bool()))
  vs_hi(3) := vi3(31, 16)
  vs_lo(3) := vi3(15, 0)
  // Subtract: bit negate
  vs_lo_adjust(3) := vi3(15, 0) ^ Fill(16, sub)

  for (i <- 0 until 2) {
    less(3)(i) := Mux(signed, (vs_hi(3)(8 * i + 7) ^ vs_lo_adjust(3)(8 * i + 7)) ^ cout3(i), !cout3(i))
  }

  sel_lo(3) := less(3).map(_ === opcode.isVredmax)

  for (i <- 0 until 2) {
    val sel3 = Mux1H(
      Seq(
        eewVd.is8 -> sel_lo(3)(i),
        eewVd.is16 -> sel_lo(3)((i / 2) * 2 + 1)
      )
    )
    vd_max(3)(i) := Mux(sel3, vs_lo(3)(8 * i + 7, 8 * i), vs_hi(3)(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 2) {
    val adder_8b = new Adder_8b(vs_lo_adjust(3)(8 * i + 7, 8 * i), vs_hi(3)(8 * i + 7, 8 * i), cin3(i))
    cin3(i) := Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) sub else cout3(i - 1)))
    cout3(i) := adder_8b.cout
    vd(3)(i) := adder_8b.out
  }

  val data_max4 = Mux(sub, Cat(vd_max(3).reverse)(7, 0), Cat(vd(3).reverse)(7, 0))
  val vi4 = Mux(vd_vsew === 0.U, Cat(vs1_zero(7, 0), data_max4), 0.U)
  val cin4 = Wire(Vec(1, Bool()))
  val cout4 = Wire(Vec(1, Bool()))
  vs_hi(4) := vi4(15, 8)
  vs_lo(4) := vi4(7, 0)
  // Subtract: bit negate
  vs_lo_adjust(4) := vi4(7, 0) ^ Fill(8, sub)

  for (i <- 0 until 1) {
    less(4)(i) := Mux(signed, (vs_hi(4)(8 * i + 7) ^ vs_lo_adjust(4)(8 * i + 7)) ^ cout4(i), !cout4(i))
  }

  sel_lo(4) := less(4).map(_ === opcode.isVredmax)

  for (i <- 0 until 1) {
    val sel4 = Mux1H(
      Seq(
        eewVd.is8 -> sel_lo(4)(i)
      )
    )
    vd_max(4)(i) := Mux(sel4, vs_lo(4)(8 * i + 7, 8 * i), vs_hi(4)(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 1) {
    val adder_8b = new Adder_8b(vs_lo_adjust(4)(8 * i + 7, 8 * i), vs_hi(4)(8 * i + 7, 8 * i), cin4(i))
    cin4(i) := Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(n => if ((i % n) == 0) sub else cout4(i - 1)))
    cout4(i) := adder_8b.cout
    vd(4)(i) := adder_8b.out
  }

  val sum_red_sel = Wire(UInt(3.W))
  sum_red_sel := Cat(0.U, ~vdType(1, 0)) + 1.U
  val sum_vd = Cat(0.U((VLEN - 64).W), Cat(vd(sum_red_sel).reverse))
  val logical_vd = Cat(0.U((VLEN - 64).W), vd_logical(~vdType(1, 0)))
  val max_vd = Cat(0.U((VLEN - 64).W), Cat(vd_max(sum_red_sel).reverse))
  val red_vd = Wire(UInt(VLEN.W))

  red_vd := sum_vd
  when(vredand_vs || vredor_vs || vredxor_vs) {
    red_vd := logical_vd
  }.elsewhen(opcode.isVredmax || opcode.isVredmin) {
    red_vd := max_vd
  }

  val red_vd_tail_one = (vd_mask << vd_vsew_bits) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits)))
  val red_vd_tail_vd = (old_vd & (vd_mask << vd_vsew_bits)) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits)))

  val red_vd_tail = Mux(vl === 0.U, old_vd, Mux(ta, red_vd_tail_one, red_vd_tail_vd))
  val vd_reg = RegInit(0.U(VLEN.W))

  when(vred_uop && fire) {
    vd_reg := red_vd_tail
  }

  io.out.vd := vd_reg
  io.out.vxsat := false.B

}

object VerilogRed extends App {
  println("Generating the VPU Reduction hardware")
  emitVerilog(new Reduction(), Array("--target-dir", "build/vifu"))
}
