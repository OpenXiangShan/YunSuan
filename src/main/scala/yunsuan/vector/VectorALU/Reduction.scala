
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
  val vsew_plus3 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  vsew_plus3 := Cat(0.U(1.W), vsew) + 3.U
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
  // to ensure timing requirements delay onecyle
  val fire_reg_s1 = RegNext(fire)
  val old_vd_reg_s1 = RegEnable(old_vd,0.U,fire)
  val ta_reg_s1 = RegEnable(ta, false.B, fire)
  val vl_reg_s1 = RegEnable(vl, 0.U, fire)

  val vd_vsew_reg_s1 = RegEnable(vd_vsew,0.U,fire)
  val eewVs2_reg_s1 = RegEnable(eewVs2.oneHot(2,0),0.U,fire)
  val signed_reg_s1 = RegEnable(signed,false.B,fire)

  val vredand_vs_reg_s1 = RegEnable(vredand_vs,false.B,fire)
  val vredor_vs_reg_s1 = RegEnable(vredor_vs,false.B,fire)
  val vredxor_vs_reg_s1 = RegEnable(vredxor_vs,false.B,fire)


  val vdType_reg_s1 = RegEnable(vdType,0.U,fire)
  val isVredmax_reg_s1 = RegEnable(opcode.isVredmax,false.B,fire)
  val isVredmin_reg_s1 = RegEnable(opcode.isVredmin,false.B,fire)

  // stage 0
  val vs12 = Cat(vs1, vs2)
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

  def zero(w: Int) = 0.U(w.W)

  def umax(w: Int) = ~(0.U(w.W))

  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))

  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  val ele8 = Wire(UInt(8.W))
  val ele16 = Wire(UInt(16.W))
  val ele32 = Wire(UInt(32.W))
  val ele64 = Wire(UInt(64.W))
  // stage 0
  ele8 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele8 := smin(8)
    }.elsewhen(vredmin_vs) {
      ele8 := smax(8)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele8 := umax(8)
    }
  }

  ele16 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele16 := smin(16)
    }.elsewhen(vredmin_vs) {
      ele16 := smax(16)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele16 := umax(16)
    }
  }

  ele32 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele32 := smin(32)
    }.elsewhen(vredmin_vs) {
      ele32 := smax(32)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele32 := umax(32)
    }
  }

  ele64 := 0.U
  when(fire) {
    when(vredmax_vs) {
      ele64 := smin(64)
    }.elsewhen(vredmin_vs) {
      ele64 := smax(64)
    }.elsewhen(vredminu_vs || vredand_vs) {
      ele64 := umax(64)
    }
  }

  val vs12_bytes = VecInit(Seq.tabulate(2 * vlenb)(i => vs12((i + 1) * 8 - 1, i * 8)))
  val vs12m_bytes_sew8 = Wire(Vec(2 * vlenb, UInt(8.W)))
  val vs12m_bytes_sew16 = Wire(Vec(2 * vlenb, UInt(8.W)))
  val vs12m_bytes_sew32 = Wire(Vec(2 * vlenb, UInt(8.W)))
  val vs12m_bytes_sew64 = Wire(Vec(2 * vlenb, UInt(8.W)))
  val vs12m_bits = Wire(UInt((2 * VLEN).W))
  val vs12m_bits_sew8 = Cat(vs12m_bytes_sew8.reverse)
  val vs12m_bits_sew16 = Cat(vs12m_bytes_sew16.reverse)
  val vs12m_bits_sew32 = Cat(vs12m_bytes_sew32.reverse)
  val vs12m_bits_sew64 = Cat(vs12m_bytes_sew64.reverse)

  // to ensure timing requirements delay onecyle
  val vs12m_bits_reg_s1 = RegEnable(vs12m_bits,0.U,fire)
  val widen_reduction_uop_reg_s1 = RegEnable(widen_reduction_uop,false.B,fire)
  val alu_uop_reg_s1 = RegEnable(alu_uop,false.B,fire)
  val widen_alu_uop_reg_s1 = RegEnable(widen_alu_uop,false.B,fire)



  // stage 0
  for (i <- 0 until 2 * vlenb) {
    vs12m_bytes_sew8(i) := vs12_bytes(i)
    when(((!vm && !vmask_bits(i)) || (i.U >= vlRemainBytes)) && (reduction_uop || widen_reduction_uop)) {
      vs12m_bytes_sew8(i) := ele8
    }
  }

  for (i <- 0 until vlenb) {
    for (j <- 0 until 2) {
      vs12m_bytes_sew16(2 * i + j) := vs12_bytes(2 * i + j)
      when(((!vm && !vmask_bits(i)) || ((Cat(i.U, 0.U(1.W)) + j.U) >= vlRemainBytes)) && (reduction_uop || widen_reduction_uop)) {
        vs12m_bytes_sew16(2 * i + j) := ele16(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until vlenb / 2) {
    for (j <- 0 until 4) {
      vs12m_bytes_sew32(4 * i + j) := vs12_bytes(4 * i + j)
      when(((!vm && !vmask_bits(i)) || ((Cat(i.U, 0.U(2.W)) + j.U) >= vlRemainBytes)) && (reduction_uop || widen_reduction_uop)) {
        vs12m_bytes_sew32(4 * i + j) := ele32(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  for (i <- 0 until vlenb / 4) {
    for (j <- 0 until 8) {
      vs12m_bytes_sew64(8 * i + j) := vs12_bytes(8 * i + j)
      when(((!vm && !vmask_bits(i)) || ((Cat(i.U, 0.U(3.W)) + j.U) >= vlRemainBytes)) && (reduction_uop || widen_reduction_uop)) {
        vs12m_bytes_sew64(8 * i + j) := ele64(8 * (j + 1) - 1, 8 * j)
      }
    }
  }

  vs12m_bits := vs12m_bits_sew8
  when(vsew === 1.U) {
    vs12m_bits := vs12m_bits_sew16
  }.elsewhen(vsew === 2.U) {
    vs12m_bits := vs12m_bits_sew32
  }.elsewhen(vsew === 3.U) {
    vs12m_bits := vs12m_bits_sew64
  }

  class Adder_8b(in1: UInt, in2: UInt, cin: UInt) {
    private val bits = Cat(0.U(1.W), in1, cin) +
      Cat(0.U(1.W), in2, cin)
    val (cout, out) = (bits(9), bits(8, 1))
  }

  val cin = Wire(Vec(16, Bool()))
  val cout = Wire(Vec(16, Bool()))
  val hi_vs = Wire(UInt(128.W))
  val lo_vs = Wire(UInt(128.W))

  val vd_logical_alu = Wire(UInt(128.W))
  // stage 1
  vd_logical_alu := 0.U
  when(vredand_vs_reg_s1) {
    vd_logical_alu := vs12m_bits_reg_s1(2 * VLEN - 1, VLEN) & vs12m_bits_reg_s1(VLEN - 1, 0)
  }.elsewhen(vredor_vs_reg_s1) {
    vd_logical_alu := vs12m_bits_reg_s1(2 * VLEN - 1, VLEN) | vs12m_bits_reg_s1(VLEN - 1, 0)
  }.elsewhen(vredxor_vs_reg_s1) {
    vd_logical_alu := vs12m_bits_reg_s1(2 * VLEN - 1, VLEN) ^ vs12m_bits_reg_s1(VLEN - 1, 0)
  }

  val sub = isVredmax_reg_s1 || isVredmin_reg_s1

  // Widen
  // stage 1
  val vs_hi_widen = Mux1H(eewVs2_reg_s1, Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs12m_bits_reg_s1(127, 64), sew).map(BitsExtend(_, 2 * sew, signed_reg_s1)).reverse)))
  val vs_lo_widen = Mux1H(eewVs2_reg_s1, Seq(8, 16, 32).map(sew =>
    Cat(UIntSplit(vs12m_bits_reg_s1(63, 0), sew).map(BitsExtend(_, 2 * sew, signed_reg_s1)).reverse)))
  hi_vs := Mux(widen_reduction_uop_reg_s1, vs_hi_widen, vs12m_bits_reg_s1(255, 128))
  lo_vs := Mux(widen_reduction_uop_reg_s1, vs_lo_widen, vs12m_bits_reg_s1(127, 0))

  val vs1_zero = Wire(UInt(64.W))
  val vs1_zero_logical = Wire(UInt(64.W))
  val vd_logical = Wire(Vec(4, UInt(64.W)))
  // to ensure timing requirements delay onecyle
  val vs1_zero_reg_s1 = RegEnable(vs1_zero, 0.U, fire)
  val vs1_zero_logical_reg_s1 = RegEnable(vs1_zero_logical, 0.U, fire)

  // stage 0
  vs1_zero := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0))

  vs1_zero_logical := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 0.U), vs1(n - 1, 0))) :+ vs1(63, 0))
  when(vredand_vs) {
    vs1_zero_logical := Mux1H(eewVd.oneHot, Seq(8, 16, 32).map(n => Cat(Fill(xLen - n, 1.U), vs1(n - 1, 0))) :+ vs1(63, 0))
  }
  // end stage 0
  for (i <- 0 until 4) {
    vd_logical(i) := 0.U
  }
  // stage 1
  when(vredand_vs_reg_s1) {
    vd_logical(0) := vs1_zero_logical_reg_s1 & vs12m_bits_reg_s1(127, 64) & vs12m_bits_reg_s1(63, 0)
  }.elsewhen(vredor_vs_reg_s1) {
    vd_logical(0) := vs1_zero_logical_reg_s1 | vs12m_bits_reg_s1(127, 64) | vs12m_bits_reg_s1(63, 0)
  }.elsewhen(vredxor_vs_reg_s1) {
    vd_logical(0) := vs1_zero_logical_reg_s1 ^ vs12m_bits_reg_s1(127, 64) ^ vs12m_bits_reg_s1(63, 0)
  }

  when(vredand_vs_reg_s1) {
    vd_logical(1) := vd_logical(0)(63, 32) & vd_logical(0)(31, 0)
  }.elsewhen(vredor_vs_reg_s1) {
    vd_logical(1) := vd_logical(0)(63, 32) | vd_logical(0)(31, 0)
  }.elsewhen(vredxor_vs_reg_s1) {
    vd_logical(1) := vd_logical(0)(63, 32) ^ vd_logical(0)(31, 0)
  }

  when(vredand_vs_reg_s1) {
    vd_logical(2) := vd_logical(1)(31, 16) & vd_logical(1)(15, 0)
  }.elsewhen(vredor_vs_reg_s1) {
    vd_logical(2) := vd_logical(1)(31, 16) | vd_logical(1)(15, 0)
  }.elsewhen(vredxor_vs_reg_s1) {
    vd_logical(2) := vd_logical(1)(31, 16) ^ vd_logical(1)(15, 0)
  }

  when(vredand_vs_reg_s1) {
    vd_logical(3) := vd_logical(2)(15, 8) & vd_logical(2)(7, 0)
  }.elsewhen(vredor_vs_reg_s1) {
    vd_logical(3) := vd_logical(2)(15, 8) | vd_logical(2)(7, 0)
  }.elsewhen(vredxor_vs_reg_s1) {
    vd_logical(3) := vd_logical(2)(15, 8) ^ vd_logical(2)(7, 0)
  }
  // end stage 1
  // stage 1 to 2
  val vd_reg = RegInit(0.U(128.W))
  val old_vd_reg = RegEnable(Mux(alu_uop_reg_s1 || widen_alu_uop_reg_s1, lo_vs, old_vd_reg_s1), 0.U, fire_reg_s1)
  val signed_reg = RegEnable(signed_reg_s1, false.B, fire_reg_s1)
  val vd_vsew_reg = RegEnable(vd_vsew_reg_s1, 0.U, fire_reg_s1)
  val ta_reg = RegEnable(ta_reg_s1, false.B, fire_reg_s1)
  val vl_reg = RegEnable(vl_reg_s1, 0.U, fire_reg_s1)
  val isVredmax_reg = RegEnable(isVredmax_reg_s1, false.B, fire_reg_s1)
  val isVredmin_reg = RegEnable(isVredmin_reg_s1, false.B, fire_reg_s1)
  val reg_vredand_vs = RegEnable(vredand_vs_reg_s1, false.B, fire_reg_s1)
  val reg_vredor_vs = RegEnable(vredor_vs_reg_s1, false.B, fire_reg_s1)
  val reg_vredxor_vs = RegEnable(vredxor_vs_reg_s1, false.B, fire_reg_s1)
  val alu_uop_reg = RegEnable(alu_uop_reg_s1, false.B, fire_reg_s1)
  val widen_alu_uop_reg = RegEnable(widen_alu_uop_reg_s1, false.B, fire_reg_s1)
  val vdType_reg = RegEnable(vdType_reg_s1(1,0), 0.U, fire_reg_s1)

  val vd_vsew_bits_reg = 8.U << vd_vsew_reg
  val sub_reg = isVredmax_reg || isVredmin_reg
  val eewVd_reg = SewOH(vdType_reg)
  val reg_vred_logical = reg_vredand_vs || reg_vredor_vs || reg_vredxor_vs
  // stage 2
  // alu
  val vs_hi = Wire(UInt(128.W))
  val vs_lo = Wire(UInt(128.W))
  val vs_lo_adjust = Wire(UInt(128.W))

  val vd = Wire(Vec(16, UInt(8.W)))
  val vd_max = Wire(Vec(16, UInt(8.W)))
  val alu_vd = Wire(UInt(128.W))
  val less = Wire(Vec(16, Bool()))
  val sel_lo = Wire(Vec(16, Bool()))

  vs_hi := vd_reg
  vs_lo := old_vd_reg
  // Subtract: bit negate
  vs_lo_adjust := vs_lo ^ Fill(128, sub_reg)

  for (i <- 0 until 16) {
    less(i) := Mux(signed_reg, (vs_hi(8 * i + 7) ^ vs_lo_adjust(8 * i + 7)) ^ cout(i), !cout(i))
  }

  sel_lo := less.map(_ === isVredmax_reg)

  for (i <- 0 until 16) {
    val sel = Mux1H(Seq(
      eewVd_reg.is8 -> sel_lo(i),
      eewVd_reg.is16 -> sel_lo((i / 2) * 2 + 1),
      eewVd_reg.is32 -> sel_lo((i / 4) * 4 + 3),
      eewVd_reg.is64 -> sel_lo((i / 8) * 8 + 7),
    ))
    vd_max(i) := Mux(sel, vs_lo(8 * i + 7, 8 * i), vs_hi(8 * i + 7, 8 * i))
  }

  for (i <- 0 until 16) {
    val adder_8b = new Adder_8b(vs_lo_adjust(8 * i + 7, 8 * i), vs_hi(8 * i + 7, 8 * i), cin(i))
    cin(i) := Mux1H(eewVd_reg.oneHot, Seq(1, 2, 4, 8).map(n =>
      if ((i % n) == 0) sub_reg else cout(i - 1))
    )
    cout(i) := adder_8b.cout
    vd(i) := adder_8b.out
  }

  alu_vd := Mux(reg_vred_logical, vd_reg, Mux(sub_reg, Cat(vd_max.reverse), Cat(vd.reverse)))
  // end part stage 2
  // sew64 sum
  val sum_sew64 = Wire(UInt(64.W))
  val carry_sew64 = Wire(UInt(64.W))
  val vd_sew64 = Wire(UInt(64.W))
  // stage 1
  val csa_3to2_sew64 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64.io.in_a := vs12m_bits_reg_s1(63, 0)
  csa_3to2_sew64.io.in_b := vs12m_bits_reg_s1(127, 64)
  csa_3to2_sew64.io.in_c := vs1_zero_reg_s1
  sum_sew64 := csa_3to2_sew64.io.out_sum
  carry_sew64 := csa_3to2_sew64.io.out_car
  // stage 2
  vd_sew64 := vd_reg(127, 64) + vd_reg(63, 0)

  // sew32 sum
  val sum_sew32 = Wire(Vec(2, UInt(32.W)))
  val carry_sew32 = Wire(Vec(2, UInt(32.W)))
  val sum_add_sew32 = Wire(UInt(32.W))
  val vd_sew32 = Wire(UInt(32.W))
  // stage 1
  val csa_3to2_sew32_0 = Module(new CSA3to2(width = 32))
  csa_3to2_sew32_0.io.in_a := vs12m_bits_reg_s1(31, 0)
  csa_3to2_sew32_0.io.in_b := vs12m_bits_reg_s1(63, 32)
  csa_3to2_sew32_0.io.in_c := vs12m_bits_reg_s1(95, 64)
  sum_sew32(0) := csa_3to2_sew32_0.io.out_sum
  carry_sew32(0) := csa_3to2_sew32_0.io.out_car
  sum_add_sew32 := vs12m_bits_reg_s1(127, 96) + vs1_zero_reg_s1(31, 0)

  val csa_3to2_sew32_1 = Module(new CSA3to2(width = 32))
  csa_3to2_sew32_1.io.in_a := sum_sew32(0)
  csa_3to2_sew32_1.io.in_b := carry_sew32(0)
  csa_3to2_sew32_1.io.in_c := sum_add_sew32
  sum_sew32(1) := csa_3to2_sew32_1.io.out_sum
  carry_sew32(1) := csa_3to2_sew32_1.io.out_car
  // stage 2
  vd_sew32 := vd_reg(63, 32) + vd_reg(31, 0)

  // sew16 sum
  val sum0_sew16 = Wire(Vec(3, UInt(16.W)))
  val carry0_sew16 = Wire(Vec(3, UInt(16.W)))
  val sum1_sew16 = Wire(Vec(2, UInt(16.W)))
  val carry1_sew16 = Wire(Vec(2, UInt(16.W)))
  val sum2_sew16 = Wire(UInt(16.W))
  val carry2_sew16 = Wire(UInt(16.W))
  val vd_sew16 = Wire(UInt(16.W))

  val in0_sew16 = Cat(vs1_zero_reg_s1(15, 0), vs12m_bits_reg_s1(127, 0))
  val in1_sew16 = Cat(Cat(sum0_sew16.reverse), Cat(carry0_sew16.reverse))
  val in2_sew16 = Cat(Cat(sum1_sew16.reverse), Cat(carry1_sew16.reverse))
  // stage 1
  for (i <- 0 until 3) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 16))
    csa_3to2_sew16.io.in_a := in0_sew16(48 * i + 15, 48 * i + 0)
    csa_3to2_sew16.io.in_b := in0_sew16(48 * i + 31, 48 * i + 16)
    csa_3to2_sew16.io.in_c := in0_sew16(48 * i + 47, 48 * i + 32)
    sum0_sew16(i) := csa_3to2_sew16.io.out_sum
    carry0_sew16(i) := csa_3to2_sew16.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 16))
    csa_3to2_sew16.io.in_a := in1_sew16(48 * i + 15, 48 * i + 0)
    csa_3to2_sew16.io.in_b := in1_sew16(48 * i + 31, 48 * i + 16)
    csa_3to2_sew16.io.in_c := in1_sew16(48 * i + 47, 48 * i + 32)
    sum1_sew16(i) := csa_3to2_sew16.io.out_sum
    carry1_sew16(i) := csa_3to2_sew16.io.out_car
  }

  val csa_4to2_sew16 = Module(new CSA4to2(width = 16))
  csa_4to2_sew16.io.in_a := in2_sew16(15, 0)
  csa_4to2_sew16.io.in_b := in2_sew16(31, 16)
  csa_4to2_sew16.io.in_c := in2_sew16(47, 32)
  csa_4to2_sew16.io.in_d := in2_sew16(63, 48)
  sum2_sew16 := csa_4to2_sew16.io.out_sum
  carry2_sew16 := csa_4to2_sew16.io.out_car
  // stage 2
  vd_sew16 := vd_reg(31, 16) + vd_reg(15, 0)

  // sew8 sum
  val sum0_sew8 = Wire(Vec(4, UInt(8.W)))
  val carry0_sew8 = Wire(Vec(4, UInt(8.W)))
  val sum1_sew8 = Wire(Vec(3, UInt(8.W)))
  val carry1_sew8 = Wire(Vec(3, UInt(8.W)))
  val sum2_sew8 = Wire(Vec(2, UInt(8.W)))
  val carry2_sew8 = Wire(Vec(2, UInt(8.W)))
  val sum3_sew8 = Wire(UInt(8.W))
  val carry3_sew8 = Wire(UInt(8.W))
  val vd_sew8 = Wire(UInt(8.W))

  val in0_sew8 = vs12m_bits_reg_s1(127, 0)
  val in1_sew8 = Cat(vs1_zero_reg_s1(7, 0), Cat(sum0_sew8.reverse), Cat(carry0_sew8.reverse))
  val in2_sew8 = Cat(Cat(sum1_sew8.reverse), Cat(carry1_sew8.reverse))
  val in3_sew8 = Cat(Cat(sum2_sew8.reverse), Cat(carry2_sew8.reverse))
  // stage 1
  for (i <- 0 until 4) {
    val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
    csa_4to2_sew8.io.in_a := in0_sew8(32 * i + 7, 32 * i + 0)
    csa_4to2_sew8.io.in_b := in0_sew8(32 * i + 15, 32 * i + 8)
    csa_4to2_sew8.io.in_c := in0_sew8(32 * i + 23, 32 * i + 16)
    csa_4to2_sew8.io.in_d := in0_sew8(32 * i + 31, 32 * i + 24)
    sum0_sew8(i) := csa_4to2_sew8.io.out_sum
    carry0_sew8(i) := csa_4to2_sew8.io.out_car
  }

  for (i <- 0 until 3) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 8))
    csa_3to2_sew8.io.in_a := in1_sew8(24 * i + 7, 24 * i + 0)
    csa_3to2_sew8.io.in_b := in1_sew8(24 * i + 15, 24 * i + 8)
    csa_3to2_sew8.io.in_c := in1_sew8(24 * i + 23, 24 * i + 16)
    sum1_sew8(i) := csa_3to2_sew8.io.out_sum
    carry1_sew8(i) := csa_3to2_sew8.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 8))
    csa_3to2_sew8.io.in_a := in2_sew8(24 * i + 7, 24 * i + 0)
    csa_3to2_sew8.io.in_b := in2_sew8(24 * i + 15, 24 * i + 8)
    csa_3to2_sew8.io.in_c := in2_sew8(24 * i + 23, 24 * i + 16)
    sum2_sew8(i) := csa_3to2_sew8.io.out_sum
    carry2_sew8(i) := csa_3to2_sew8.io.out_car
  }

  val csa_4to2_sew8 = Module(new CSA4to2(width = 8))
  csa_4to2_sew8.io.in_a := in3_sew8(7, 0)
  csa_4to2_sew8.io.in_b := in3_sew8(15, 8)
  csa_4to2_sew8.io.in_c := in3_sew8(23, 16)
  csa_4to2_sew8.io.in_d := in3_sew8(31, 24)
  sum3_sew8 := csa_4to2_sew8.io.out_sum
  carry3_sew8 := csa_4to2_sew8.io.out_car
  // stage 2
  vd_sew8 := vd_reg(15, 8) + vd_reg(7, 0)

  // sew64 max/min
  val vd_max_sew64 = Wire(UInt(64.W))
  // stage 1
  val compare_3to1_sew64 = Module(new compare_3to1(w = 64))
  compare_3to1_sew64.io.a := vs12m_bits_reg_s1(63, 0)
  compare_3to1_sew64.io.b := vs12m_bits_reg_s1(127, 64)
  compare_3to1_sew64.io.c := vs1_zero_reg_s1
  compare_3to1_sew64.io.max := isVredmax_reg_s1
  compare_3to1_sew64.io.signed := signed_reg_s1
  vd_max_sew64 := compare_3to1_sew64.io.d

  // sew32 max/min
  // stage 1
  val vd0_max_sew32 = Wire(Vec(2, UInt(32.W)))
  val vd1_max_sew32 = Wire(UInt(32.W))

  val compare_3to1_sew32 = Module(new compare_3to1(w = 32))
  compare_3to1_sew32.io.a := vs12m_bits_reg_s1(31, 0)
  compare_3to1_sew32.io.b := vs12m_bits_reg_s1(63, 32)
  compare_3to1_sew32.io.c := vs12m_bits_reg_s1(95, 64)
  compare_3to1_sew32.io.max := isVredmax_reg_s1
  compare_3to1_sew32.io.signed := signed_reg_s1
  vd0_max_sew32(0) := compare_3to1_sew32.io.d

  val compare0_2to1_sew32 = Module(new compare_2to1(w = 32))
  compare0_2to1_sew32.io.a := vs12m_bits_reg_s1(127, 96)
  compare0_2to1_sew32.io.b := vs1_zero_reg_s1(31, 0)
  compare0_2to1_sew32.io.max := isVredmax_reg_s1
  compare0_2to1_sew32.io.signed := signed_reg_s1
  vd0_max_sew32(1) := compare0_2to1_sew32.io.c

  //stage 2
  val compare1_2to1_sew32 = Module(new compare_2to1(w = 32))
  compare1_2to1_sew32.io.a := vd_reg(31, 0)
  compare1_2to1_sew32.io.b := vd_reg(63, 32)
  compare1_2to1_sew32.io.max := isVredmax_reg
  compare1_2to1_sew32.io.signed := signed_reg
  vd1_max_sew32 := compare1_2to1_sew32.io.c

  // sew16 max/min
  // stage 1
  val vd0_max_sew16 = Wire(Vec(3, UInt(16.W)))
  val vd1_max_sew16 = Wire(UInt(16.W))
  val in0_max_sew16 = Cat(vs1_zero_reg_s1(15, 0), vs12m_bits_reg_s1(127, 0))

  for (i <- 0 until 3) {
    val compare_3to1_sew16 = Module(new compare_3to1(w = 16))
    compare_3to1_sew16.io.a := in0_max_sew16(48 * i + 15, 48 * i + 0)
    compare_3to1_sew16.io.b := in0_max_sew16(48 * i + 31, 48 * i + 16)
    compare_3to1_sew16.io.c := in0_max_sew16(48 * i + 47, 48 * i + 32)
    compare_3to1_sew16.io.max := isVredmax_reg_s1
    compare_3to1_sew16.io.signed := signed_reg_s1
    vd0_max_sew16(i) := compare_3to1_sew16.io.d
  }
  // stage 2
  val compare1_3to1_sew16 = Module(new compare_3to1(w = 16))
  compare1_3to1_sew16.io.a := vd_reg(15, 0)
  compare1_3to1_sew16.io.b := vd_reg(31, 16)
  compare1_3to1_sew16.io.c := vd_reg(47, 32)
  compare1_3to1_sew16.io.max := isVredmax_reg
  compare1_3to1_sew16.io.signed := signed_reg
  vd1_max_sew16 := compare1_3to1_sew16.io.d

  // sew8 max/min
  // stage 1
  val vd0_max_sew8 = Wire(Vec(6, UInt(8.W)))
  val vd1_max_sew8 = Wire(Vec(2, UInt(8.W)))
  val vd2_max_sew8 = Wire(UInt(8.W))
  val in2_max_sew8 = Cat(vd1_max_sew8.reverse)

  for (i <- 0 until 5) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := vs12m_bits_reg_s1(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := vs12m_bits_reg_s1(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := vs12m_bits_reg_s1(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := isVredmax_reg_s1
    compare_3to1_sew8.io.signed := signed_reg_s1
    vd0_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare0_2to1_sew8 = Module(new compare_2to1(w = 8))
  compare0_2to1_sew8.io.a := vs12m_bits_reg_s1(127, 120)
  compare0_2to1_sew8.io.b := vs1_zero_reg_s1(7, 0)
  compare0_2to1_sew8.io.max := isVredmax_reg_s1
  compare0_2to1_sew8.io.signed := signed_reg_s1
  vd0_max_sew8(5) := compare0_2to1_sew8.io.c
  // stage 2
  for (i <- 0 until 2) {
    val compare_3to1_sew8 = Module(new compare_3to1(w = 8))
    compare_3to1_sew8.io.a := vd_reg(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := vd_reg(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := vd_reg(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := isVredmax_reg
    compare_3to1_sew8.io.signed := signed_reg
    vd1_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare1_2to1_sew8 = Module(new compare_2to1(w = 8))
  compare1_2to1_sew8.io.a := in2_max_sew8(15, 8)
  compare1_2to1_sew8.io.b := in2_max_sew8(7, 0)
  compare1_2to1_sew8.io.max := isVredmax_reg
  compare1_2to1_sew8.io.signed := signed_reg
  vd2_max_sew8 := compare1_2to1_sew8.io.c

  val sum_vd = Wire(UInt(64.W))
  val max_vd = Wire(UInt(64.W))
  // stage 1
  val logical_vd = Mux(alu_uop_reg_s1, vd_logical_alu, Cat(0.U((VLEN - 64).W), vd_logical(~vdType_reg_s1(1, 0))))
  val red_vd = Wire(UInt(VLEN.W))
  // stage 2
  sum_vd := vd_sew64
  when(vd_vsew_reg === 0.U) {
    sum_vd := vd_sew8
  }.elsewhen(vd_vsew_reg === 1.U) {
    sum_vd := vd_sew16
  }.elsewhen(vd_vsew_reg === 2.U) {
    sum_vd := vd_sew32
  }

  max_vd := vd_reg
  when(vd_vsew_reg === 0.U) {
    max_vd := vd2_max_sew8
  }.elsewhen(vd_vsew_reg === 1.U) {
    max_vd := vd1_max_sew16
  }.elsewhen(vd_vsew_reg === 2.U) {
    max_vd := vd1_max_sew32
  }

  red_vd := sum_vd
  when(reg_vredand_vs || reg_vredor_vs || reg_vredxor_vs) {
    red_vd := vd_reg
  }.elsewhen(isVredmax_reg || isVredmin_reg) {
    red_vd := max_vd
  }

  val red_vd_tail_one = (vd_mask << vd_vsew_bits_reg) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits_reg)))
  val red_vd_tail_vd = (old_vd_reg & (vd_mask << vd_vsew_bits_reg)) | (red_vd & (vd_mask >> (VLEN.U - vd_vsew_bits_reg)))

  val red_vd_tail = Mux(vl_reg === 0.U, old_vd_reg, Mux(ta_reg, red_vd_tail_one, red_vd_tail_vd))
  // stage 1 to 2
  when(fire_reg_s1) {
    when(vredand_vs_reg_s1 || vredor_vs_reg_s1 || vredxor_vs_reg_s1) {
      vd_reg := logical_vd
    }.elsewhen(isVredmax_reg_s1 || isVredmin_reg_s1) {
      when(alu_uop_reg_s1) {
        vd_reg := hi_vs
      }.otherwise {
        when(vd_vsew_reg_s1 === 0.U) {
          vd_reg := Cat(vd0_max_sew8.reverse)
        }.elsewhen(vd_vsew_reg_s1 === 1.U) {
          vd_reg := Cat(vd0_max_sew16.reverse)
        }.elsewhen(vd_vsew_reg_s1 === 2.U) {
          vd_reg := Cat(vd0_max_sew32.reverse)
        }.elsewhen(vd_vsew_reg_s1 === 3.U) {
          vd_reg := vd_max_sew64
        }
      }
    }.otherwise {
      when(alu_uop_reg_s1 || widen_alu_uop_reg_s1) {
        vd_reg := hi_vs
      }.otherwise {
        when(vd_vsew_reg_s1 === 0.U) {
          vd_reg := Cat(sum3_sew8, carry3_sew8)
        }.elsewhen(vd_vsew_reg_s1 === 1.U) {
          vd_reg := Cat(sum2_sew16, carry2_sew16)
        }.elsewhen(vd_vsew_reg_s1 === 2.U) {
          vd_reg := Cat(sum_sew32(1), carry_sew32(1))
        }.elsewhen(vd_vsew_reg_s1 === 3.U) {
          vd_reg := Cat(sum_sew64, carry_sew64)
        }
      }
    }
  }

  io.out.vd := Mux(alu_uop_reg || widen_alu_uop_reg, alu_vd, red_vd_tail)
  io.out.vxsat := false.B
}

class Adder_xb(w: Int) extends Module {
  val io = IO(new Bundle() {
    val in1 = Input(UInt(w.W))
    val in2 = Input(UInt(w.W))
    val cin = Input(UInt(1.W))
    val cout = Output(UInt(1.W))
  })

  private val bits = Cat(0.U(1.W), io.in1, io.cin) + Cat(0.U(1.W), io.in2, io.cin)
  io.cout := bits(w + 1)
}

class compare_2to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val c = Output(UInt(w.W))
  })

  // a-b
  val b_inv = ~io.b
  val cout = Wire(Bool())
  val less = Wire(Bool())

  val adder_xb = Module(new Adder_xb(w = w))
  adder_xb.io.in1 := b_inv
  adder_xb.io.in2 := io.a
  adder_xb.io.cin := 1.U
  cout := adder_xb.io.cout
  less := Mux(io.signed, io.a(w - 1) ^ b_inv(w - 1) ^ cout, !cout)
  io.c := Mux(less === io.max, io.b, io.a)
}

class compare_3to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val c = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val d = Output(UInt(w.W))
  })

  // a-b, a-c, b-c
  val vs_hi = Cat(io.a, io.a, io.b)
  val vs_lo = Cat(io.b, io.c, io.c)
  val vs_lo_inv = ~vs_lo
  val cout = Wire(Vec(3, Bool()))
  val less = Wire(Vec(3, Bool()))

  for (i <- 0 until 3) {
    val adder_xb = Module(new Adder_xb(w = w))
    adder_xb.io.in1 := vs_lo_inv(w * (i + 1) - 1, w * i)
    adder_xb.io.in2 := vs_hi(w * (i + 1) - 1, w * i)
    adder_xb.io.cin := 1.U
    cout(i) := adder_xb.io.cout
    less(i) := Mux(io.signed, vs_hi(w * (i + 1) - 1) ^ vs_lo_inv(w * (i + 1) - 1) ^ cout(i), !cout(i))
  }

  io.d := 0.U
  when((less(2) && less(1) && !io.max) || (!less(2) && !less(1) && io.max)) {
    io.d := io.a
  }.elsewhen((!less(2) && less(0) && !io.max) || (less(2) && !less(0) && io.max)) {
    io.d := io.b
  }.elsewhen((!less(1) && !less(0) && !io.max) || (less(1) && less(0) && io.max)) {
    io.d := io.c
  }
}


object VerilogRed extends App {
  println("Generating the VPU Reduction hardware")
  emitVerilog(new Reduction(), Array("--target-dir", "build/vifu"))
}



