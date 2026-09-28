package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._
import yunsuan.vector.Common._
import yunsuan.vector.alu.VAluOpcode._
import yunsuan.encoding.Opcode.Opcodes.VRedOpcodes._
import yunsuan.encoding.Opcode.Opcodes.VRedOpcodes

trait ReductionParam {
  val VLEN = 128
  val XLEN = 64
  val vlenb = VLEN / 8
}

// TODO:
// 1. set final outputs properly
// 2. move vs1 initialization to top module instead of submodules
// 3. modify name of input port
// FIXME: considering vstart
class NewReduction extends Module with ReductionParam {
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
  val vsew = srcTypeVs2(1, 0)
  val srcTypeVs1 = io.in.bits.srcType(1)
  val vdType = io.in.bits.vdType
  val vm = io.in.bits.info.vm
  val ma = io.in.bits.info.ma
  val ta = io.in.bits.info.ta
  val vlmul = io.in.bits.info.vlmul
  val vl = io.in.bits.info.vl
  val uopIdx = io.in.bits.info.uopIdx
  val InValid = io.in.valid

  // LMUL>1 is split into sequential per-register vred_*/vwredsum_* uops.
  // Each uop must only see its slice of VL; feeding the full VL would
  // over-activate a later register (vredor of tail 0xFF → …ffffffff).
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val elemsPerReg = (vlenb.U >> vsew).asUInt
  val vlBase = (uopIdx * elemsPerReg).asUInt
  val vlRemain = Mux(vl <= vlBase, 0.U,
    Mux((vl - vlBase) >= elemsPerReg, elemsPerReg, vl - vlBase))
  val vlRemainBytes = vlRemain << vsew
  val max = VRedOpcodes.isVredmax(opcode.op)

  val vredsum_vs = VRedOpcodes.isVredsum(opcode.op)
  val vredmax_vs = VRedOpcodes.isVredmax(opcode.op) && srcTypeVs2(2).asBool
  val vredmaxu_vs = VRedOpcodes.isVredmax(opcode.op) && !srcTypeVs2(2).asBool
  val vredmin_vs = VRedOpcodes.isVredmin(opcode.op) && srcTypeVs2(2).asBool
  val vredminu_vs = VRedOpcodes.isVredmin(opcode.op) && !srcTypeVs2(2).asBool
  val vredand_vs = VRedOpcodes.isVredand(opcode.op)
  val vredor_vs = VRedOpcodes.isVredor(opcode.op)
  val vredxor_vs = VRedOpcodes.isVredxor(opcode.op)
  val vwredsum_vs = VRedOpcodes.isVredsum(opcode.op) && srcTypeVs2(2).asBool && (vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U))
  val vwredsumu_vs = VRedOpcodes.isVredsum(opcode.op) && !srcTypeVs2(2).asBool && (vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U))

  val vs2_bytes = vs2.asTypeOf(Vec(vlenb, UInt(8.W)))
  val vs2_masked = Wire(UInt(VLEN.W))
  val vs2m_bytes = Wire(Vec(vlenb, UInt(8.W)))

  // RVV: vl=0 performs no operation and must leave vd undisturbed.
  val vlIsZero = vl === 0.U

  val valid_s1_r = RegNext(InValid)
  val vredand_vs_s1_r = RegEnable(vredand_vs, false.B, InValid)
  val vredor_vs_s1_r = RegEnable(vredor_vs, false.B, InValid)
  val vredxor_vs_s1_r = RegEnable(vredxor_vs, false.B, InValid)
  val vredsum_s1_r = RegEnable(vredsum_vs || vwredsum_vs || vwredsumu_vs, false.B, InValid)
  val vredcomp_s1_r = RegEnable(vredmax_vs || vredmaxu_vs || vredmin_vs || vredminu_vs, false.B, InValid)
  val vs1_s1_r = RegEnable(vs1, 0.U, InValid)
  val vs2m_s1_r = RegEnable(vs2_masked, 0.U, InValid)
  val old_vd_s1_r = RegEnable(old_vd, 0.U, InValid)
  val vsew_s1_r = RegEnable(vsew, 0.U, InValid)
  val signed_s1_r = RegEnable(signed, false.B, InValid)
  val widen_s1_r = RegEnable(widen, false.B, InValid)
  val max_s1_r = RegEnable(max, false.B, InValid)
  val ta_s1_r = RegEnable(ta, false.B, InValid)
  val vlIsZero_s1_r = RegEnable(vlIsZero, false.B, InValid)
  val logicRes_s1 = Wire(UInt(VLEN.W))

  val valid_s2_r = RegNext(valid_s1_r)
  val vsew_s2_r = RegEnable(vsew_s1_r, 0.U, valid_s1_r)
  val widen_s2_r = RegEnable(widen_s1_r, 0.U, valid_s1_r)
  val old_vd_s2_r = RegEnable(old_vd_s1_r, 0.U, valid_s1_r)
  val ta_s2_r = RegEnable(ta_s1_r, false.B, valid_s1_r)
  val vlIsZero_s2_r = RegEnable(vlIsZero_s1_r, false.B, valid_s1_r)
  val sumRes_s2 = Wire(UInt(VLEN.W))
  val compareRes_s2 = Wire(UInt(VLEN.W))
  val vredlogic_s2_r = RegEnable(vredand_vs_s1_r || vredor_vs_s1_r || vredxor_vs_s1_r, false.B, valid_s1_r)
  val vredsum_s2_r = RegEnable(vredsum_s1_r, false.B, valid_s1_r)
  val vredcomp_s2_r = RegEnable(vredcomp_s1_r, false.B, valid_s1_r)

  // generate masked source
  def umax(w: Int) = ~(0.U(w.W))
  def smax(w: Int) = Cat(0.U(1.W), ~(0.U((w - 1).W)))
  def smin(w: Int) = Cat(1.U(1.W), 0.U((w - 1).W))

  val fillValue8 = WireInit(UInt(8.W), 0.U)
  val fillValue16 = WireInit(UInt(16.W), 0.U)
  val fillValue32 = WireInit(UInt(32.W), 0.U)
  val fillValue64 = WireInit(UInt(64.W), 0.U)

  when(InValid) {
    when(vredmax_vs) {
      fillValue8 := smin(8)
      fillValue16 := smin(16)
      fillValue32 := smin(32)
      fillValue64 := smin(64)
    }.elsewhen(vredmin_vs) {
      fillValue8 := smax(8)
      fillValue16 := smax(16)
      fillValue32 := smax(32)
      fillValue64 := smax(64)
    }.elsewhen(vredminu_vs || vredand_vs) {
      fillValue8 := umax(8)
      fillValue16 := umax(16)
      fillValue32 := umax(32)
      fillValue64 := umax(64)
    }
  }

  for (i <- 0 until vlenb) {
    val fillByte = MuxLookup(vsew, 0.U)(Seq(
      VSew.e8 -> fillValue8,
      VSew.e16 -> fillValue16(8 * (i % 2) + 7, 8 * (i % 2)),
      VSew.e32 -> fillValue32(8 * (i % 4) + 7, 8 * (i % 4)),
      VSew.e64 -> fillValue64(8 * (i % 8) + 7, 8 * (i % 8)),
    ))
    val mask = MuxLookup(vsew, 1.U)(Seq(
      VSew.e8 -> vmask(i),
      VSew.e16 -> vmask(i >> 1),
      VSew.e32 -> vmask(i >> 2),
      VSew.e64 -> vmask(i >> 3),
    ))
    vs2m_bytes(i) := Mux((!vm && !mask) || (i.U >= vlRemainBytes.asUInt), fillByte, vs2_bytes(i))
  }
  vs2_masked := Cat(vs2m_bytes.reverse)

  // stage 1
  val vredand = Module(new VRedAND())
  vredand.io.vsew := vsew_s1_r
  vredand.io.v1 := vs2m_s1_r(127, 0)
  vredand.io.v2 := vs1_s1_r
  val vredor = Module(new VRedOR())
  vredor.io.vsew := vsew_s1_r
  vredor.io.v1 := vs2m_s1_r(127, 0)
  vredor.io.v2 := vs1_s1_r
  val vredxor = Module(new VRedXOR())
  vredxor.io.vsew := vsew_s1_r
  vredxor.io.v1 := vs2m_s1_r(127, 0)
  vredxor.io.v2 := vs1_s1_r

  logicRes_s1 := 0.U
  when(vredand_vs_s1_r) {
    logicRes_s1 := Cat(0.U((VLEN - XLEN).W), vredand.io.vout)
  }.elsewhen(vredor_vs_s1_r) {
    logicRes_s1 := Cat(0.U((VLEN - XLEN).W), vredor.io.vout)
  }.elsewhen(vredxor_vs_s1_r) {
    logicRes_s1 := Cat(0.U((VLEN - XLEN).W), vredxor.io.vout)
  }

  val logicRes_s2_r = RegEnable(logicRes_s1, 0.U, valid_s1_r)

  // stage 1 ~ 2
  val vredwsum = Module(new VRedWSUM())
  // Gate the CSA register update on stage-1 valid so idle cycles cannot
  // advance vd_vsew_reg / vd_reg with a stale sew.
  vredwsum.io.valid := valid_s1_r
  vredwsum.io.signed := signed_s1_r
  vredwsum.io.widen := widen_s1_r
  vredwsum.io.vsew := vsew_s1_r
  vredwsum.io.v1 := vs2m_s1_r(127, 0)
  vredwsum.io.v2 := vs1_s1_r

  // VRedWSUM / VRedComp already register internally for 1 cycle. Sample their
  // outputs as wires on stage-2 so they line up with vredsum_s2_r / control.
  // Do NOT RegEnable here, and do NOT drive io.vout from a combo fold — that
  // races stage-2 and is what made vredsum_vs-0 (e8/mf8/vl=2) return 0.
  sumRes_s2 := Cat(0.U((VLEN - XLEN).W), vredwsum.io.vout)

  // stage 1 ~ 2
  val vredcomp = Module(new VRedComp())
  vredcomp.io.signed := signed_s1_r
  vredcomp.io.vsew := vsew_s1_r
  vredcomp.io.max := max_s1_r
  vredcomp.io.v1 := vs2m_s1_r(127, 0)
  vredcomp.io.v2 := vs1_s1_r

  compareRes_s2 := Cat(0.U((VLEN - XLEN).W), vredcomp.io.vout)

  val vd_val = Wire(UInt(128.W))
  vd_val := 0.U
  when(vredlogic_s2_r) {
    vd_val := logicRes_s2_r
  }.elsewhen(vredsum_s2_r) {
    vd_val := sumRes_s2
  }.elsewhen(vredcomp_s2_r) {
    vd_val := compareRes_s2
  }
  // need modify
  val vd_mask = Wire(UInt(128.W))
  vd_mask := MuxLookup(vsew_s2_r + widen_s2_r, 0.U)(Seq(
    VSew.e8 -> Cat(Fill(VLEN - 8, 1.U), 0.U(8.W)),
    VSew.e16 -> Cat(Fill(VLEN - 16, 1.U), 0.U(16.W)),
    VSew.e32 -> Cat(Fill(VLEN - 32, 1.U), 0.U(32.W)),
    VSew.e64 -> Cat(Fill(VLEN - 64, 1.U), 0.U(64.W)),
  ))
  val vd_updated = Mux(ta_s2_r, (vd_mask | vd_val), ((vd_mask & old_vd_s2_r) | vd_val))
  // vl=0: pass old_vd through unchanged (do not apply EEW tu/ta update).
  io.out.vd := Mux(vlIsZero_s2_r, old_vd_s2_r, vd_updated)
  io.out.vxsat := false.B
}

class VRedAND extends Module with ReductionParam {
  val io = IO(new Bundle {
    val vsew = Input(UInt(2.W))
    val v1 = Input(UInt(VLEN.W))
    val v2 = Input(UInt(XLEN.W))
    val vout = Output(UInt(XLEN.W))
  })
  val v1 = io.v1
  val v2 = MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(Fill(XLEN - 8, 1.U), io.v2(7,0)),
    VSew.e16 -> Cat(Fill(XLEN - 16, 1.U), io.v2(15,0)),
    VSew.e32 -> Cat(Fill(XLEN - 32, 1.U), io.v2(31,0)),
    VSew.e64 -> io.v2,
  ))
  val in1 = v1(63, 0) & v2
  val in2 = v1(127, 64)

  val out64 = in1 & in2
  val out32 = out64(31,0) & out64(63,32)
  val out16 = out32(15,0) & out32(31,16)
  val out8 = out16(7,0) & out16(15,8)

  io.vout := MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(0.U((XLEN - 8).W), out8),
    VSew.e16 -> Cat(0.U((XLEN - 16).W), out16),
    VSew.e32 -> Cat(0.U((XLEN - 32).W), out32),
    VSew.e64 -> out64
  ))
}

class VRedOR extends Module with ReductionParam {
  val io = IO(new Bundle {
    val vsew = Input(UInt(2.W))
    val v1 = Input(UInt(VLEN.W))
    val v2 = Input(UInt(XLEN.W))
    val vout = Output(UInt(XLEN.W))
  })
  val v1 = io.v1
  val v2 = MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(Fill(XLEN - 8, 0.U), io.v2(7,0)),
    VSew.e16 -> Cat(Fill(XLEN - 16, 0.U), io.v2(15,0)),
    VSew.e32 -> Cat(Fill(XLEN - 32, 0.U), io.v2(31,0)),
    VSew.e64 -> io.v2,
  ))
  val in1 = v1(63, 0) | v2
  val in2 = v1(127, 64)

  val out64 = in1 | in2
  val out32 = out64(31,0) | out64(63,32)
  val out16 = out32(15,0) | out32(31,16)
  val out8 = out16(7,0) | out16(15,8)

  io.vout := MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(0.U((XLEN - 8).W), out8),
    VSew.e16 -> Cat(0.U((XLEN - 16).W), out16),
    VSew.e32 -> Cat(0.U((XLEN - 32).W), out32),
    VSew.e64 -> out64
  ))
}

class VRedXOR extends Module with ReductionParam {
  val io = IO(new Bundle {
    val vsew = Input(UInt(2.W))
    val v1 = Input(UInt(VLEN.W))
    val v2 = Input(UInt(XLEN.W))
    val vout = Output(UInt(XLEN.W))
  })
  val v1 = io.v1
  val v2 = MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(Fill(XLEN - 8, 0.U), io.v2(7,0)),
    VSew.e16 -> Cat(Fill(XLEN - 16, 0.U), io.v2(15,0)),
    VSew.e32 -> Cat(Fill(XLEN - 32, 0.U), io.v2(31,0)),
    VSew.e64 -> io.v2,
  ))
  val in1 = v1(63, 0) ^ v2
  val in2 = v1(127, 64)

  val out64 = in1 ^ in2
  val out32 = out64(31,0) ^ out64(63,32)
  val out16 = out32(15,0) ^ out32(31,16)
  val out8 = out16(7,0) ^ out16(15,8)

  io.vout := MuxLookup(io.vsew, 0.U)(Seq(
    VSew.e8 -> Cat(0.U((XLEN - 8).W), out8),
    VSew.e16 -> Cat(0.U((XLEN - 16).W), out16),
    VSew.e32 -> Cat(0.U((XLEN - 32).W), out32),
    VSew.e64 -> out64
  ))
}

class VRedComp extends Module {
  val io = IO(new Bundle {
    val max = Input(Bool())
    val signed = Input(Bool())
    val vsew = Input(UInt(2.W))
    val v1 = Input(UInt(128.W))
    val v2 = Input(UInt(64.W))
    val vout = Output(UInt(64.W))
  })
  val max = io.max
  val signed = io.signed
  val v1 = io.v1
  val v2 = io.v2

  val max_reg = RegNext(max)
  val signed_reg = RegNext(signed)
  val vsew_reg = RegNext(io.vsew)

  // sew64 max/min
  val vd_max_sew64 = Wire(UInt(64.W))
  // stage 1
  val compare_3to1_sew64 = Module(new new_compare_3to1(w = 64))
  compare_3to1_sew64.io.a := v1(63, 0)
  compare_3to1_sew64.io.b := v1(127, 64)
  compare_3to1_sew64.io.c := v2
  compare_3to1_sew64.io.max := max
  compare_3to1_sew64.io.signed := signed
  vd_max_sew64 := compare_3to1_sew64.io.d
  // stage 2
  val vd_max_sew64_s2_r = RegNext(vd_max_sew64)

  // sew32 max/min
  // stage 1
  val vd0_max_sew32 = Wire(Vec(2, UInt(32.W)))
  val vd1_max_sew32 = Wire(UInt(32.W))

  val compare_3to1_sew32 = Module(new new_compare_3to1(w = 32))
  compare_3to1_sew32.io.a := v1(31, 0)
  compare_3to1_sew32.io.b := v1(63, 32)
  compare_3to1_sew32.io.c := v1(95, 64)
  compare_3to1_sew32.io.max := max
  compare_3to1_sew32.io.signed := signed
  vd0_max_sew32(0) := compare_3to1_sew32.io.d

  val compare0_2to1_sew32 = Module(new new_compare_2to1(w = 32))
  compare0_2to1_sew32.io.a := v1(127, 96)
  compare0_2to1_sew32.io.b := v2(31, 0)
  compare0_2to1_sew32.io.max := max
  compare0_2to1_sew32.io.signed := signed
  vd0_max_sew32(1) := compare0_2to1_sew32.io.c

  val v32_reg = RegNext(vd0_max_sew32)
  //stage 2
  val compare1_2to1_sew32 = Module(new new_compare_2to1(w = 32))
  compare1_2to1_sew32.io.a := v32_reg(0)
  compare1_2to1_sew32.io.b := v32_reg(1)
  compare1_2to1_sew32.io.max := max_reg
  compare1_2to1_sew32.io.signed := signed_reg
  vd1_max_sew32 := compare1_2to1_sew32.io.c

  // sew16 max/min
  // stage 1
  val vd0_max_sew16 = Wire(Vec(3, UInt(16.W)))
  val vd1_max_sew16 = Wire(UInt(16.W))
  val in0_max_sew16 = Cat(v2(15, 0), v1(127, 0))

  for (i <- 0 until 3) {
    val compare_3to1_sew16 = Module(new new_compare_3to1(w = 16))
    compare_3to1_sew16.io.a := in0_max_sew16(48 * i + 15, 48 * i + 0)
    compare_3to1_sew16.io.b := in0_max_sew16(48 * i + 31, 48 * i + 16)
    compare_3to1_sew16.io.c := in0_max_sew16(48 * i + 47, 48 * i + 32)
    compare_3to1_sew16.io.max := max
    compare_3to1_sew16.io.signed := signed
    vd0_max_sew16(i) := compare_3to1_sew16.io.d
  }
  val v16_reg = RegNext(vd0_max_sew16)
  // stage 2
  val compare1_3to1_sew16 = Module(new new_compare_3to1(w = 16))
  compare1_3to1_sew16.io.a := v16_reg(0)
  compare1_3to1_sew16.io.b := v16_reg(1)
  compare1_3to1_sew16.io.c := v16_reg(2)
  compare1_3to1_sew16.io.max := max_reg
  compare1_3to1_sew16.io.signed := signed_reg
  vd1_max_sew16 := compare1_3to1_sew16.io.d

  // sew8 max/min
  // stage 1
  val vd0_max_sew8 = Wire(Vec(6, UInt(8.W)))
  val vd1_max_sew8 = Wire(Vec(2, UInt(8.W)))
  val vd2_max_sew8 = Wire(UInt(8.W))
  val in2_max_sew8 = Cat(vd1_max_sew8.reverse)

  for (i <- 0 until 5) {
    val compare_3to1_sew8 = Module(new new_compare_3to1(w = 8))
    compare_3to1_sew8.io.a := v1(24 * i + 7, 24 * i + 0)
    compare_3to1_sew8.io.b := v1(24 * i + 15, 24 * i + 8)
    compare_3to1_sew8.io.c := v1(24 * i + 23, 24 * i + 16)
    compare_3to1_sew8.io.max := max
    compare_3to1_sew8.io.signed := signed
    vd0_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare0_2to1_sew8 = Module(new new_compare_2to1(w = 8))
  compare0_2to1_sew8.io.a := v1(127, 120)
  compare0_2to1_sew8.io.b := v2(7, 0)
  compare0_2to1_sew8.io.max := max
  compare0_2to1_sew8.io.signed := signed
  vd0_max_sew8(5) := compare0_2to1_sew8.io.c

  val v8_reg = RegNext(vd0_max_sew8)
  // stage 2
  for (i <- 0 until 2) {
    val compare_3to1_sew8 = Module(new new_compare_3to1(w = 8))
    compare_3to1_sew8.io.a := v8_reg(3 * i)
    compare_3to1_sew8.io.b := v8_reg(3 * i + 1)
    compare_3to1_sew8.io.c := v8_reg(3 * i + 2)
    compare_3to1_sew8.io.max := max_reg
    compare_3to1_sew8.io.signed := signed_reg
    vd1_max_sew8(i) := compare_3to1_sew8.io.d
  }

  val compare1_2to1_sew8 = Module(new new_compare_2to1(w = 8))
  compare1_2to1_sew8.io.a := in2_max_sew8(15, 8)
  compare1_2to1_sew8.io.b := in2_max_sew8(7, 0)
  compare1_2to1_sew8.io.max := max_reg
  compare1_2to1_sew8.io.signed := signed_reg
  vd2_max_sew8 := compare1_2to1_sew8.io.c

  io.vout := vd_max_sew64_s2_r
  when(vsew_reg === 0.U) {
    io.vout := Cat(0.U, vd2_max_sew8)
  }.elsewhen(vsew_reg === 1.U) {
    io.vout := Cat(0.U, vd1_max_sew16)
  }.elsewhen(vsew_reg === 2.U) {
    io.vout := Cat(0.U, vd1_max_sew32)
  }
}
/*
class compare_2to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val c = Output(UInt(w.W))
  })

  // a-b
  val fs_a = Cat(io.signed ^ io.a(w-1), io.a(w-2, 0))
  val fs_b = Cat(io.signed ^ io.b(w-1), io.b(w-2, 0))
  val less = Wire(Bool())

  less := fs_a < fs_b
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
  val fs_a = Cat(io.signed ^ io.a(w-1), io.a(w-2, 0))
  val fs_b = Cat(io.signed ^ io.b(w-1), io.b(w-2, 0))
  val fs_c = Cat(io.signed ^ io.c(w-1), io.c(w-2, 0))
  val less = Wire(Vec(3, Bool()))

  less(2) := fs_a < fs_b
  less(1) := fs_a < fs_c
  less(0) := fs_b < fs_c

  io.d := 0.U
  when((less(2) && less(1) && !io.max) || (!less(2) && !less(1) && io.max)) {
    io.d := io.a
  }.elsewhen((!less(2) && less(0) && !io.max) || (less(2) && !less(0) && io.max)) {
    io.d := io.b
  }.elsewhen((!less(1) && !less(0) && !io.max) || (less(1) && less(0) && io.max)) {
    io.d := io.c
  }
}
*/
class VRedWSUM extends Module {
  val io = IO(new Bundle() {
    val valid = Input(Bool())
    val widen = Input(Bool())
    val signed = Input(Bool())
    val vsew = Input(UInt(2.W))
    val v1 = Input(UInt(128.W))
    val v2 = Input(UInt(64.W))
    val vout = Output(UInt(64.W))
  })
  val vector = io.v1
  val scalar = io.v2
  val signed = io.signed
  val widen = io.widen

  val vd_reg = RegInit(0.U(128.W))
  val vd_vsew_reg_s1 = io.vsew
  val signed_s2_r = RegEnable(io.signed, io.valid)
  val widen_s2_r = RegEnable(io.widen, io.valid)

  // sew64 sum
  val sum_sew64 = Wire(UInt(64.W))
  val carry_sew64 = Wire(UInt(64.W))
  val vd_sew64 = Wire(UInt(64.W))
  // stage 1
  val csa_3to2_sew64 = Module(new CSA3to2(width = 64))
  csa_3to2_sew64.io.in_a := vector(63, 0)
  csa_3to2_sew64.io.in_b := vector(127, 64)
  csa_3to2_sew64.io.in_c := scalar
  sum_sew64 := csa_3to2_sew64.io.out_sum
  carry_sew64 := csa_3to2_sew64.io.out_car
  // stage 2
  vd_sew64 := vd_reg(127, 64) + vd_reg(63, 0)

  // sew32 (widen) sum
  val sum_sew32 = Wire(Vec(2, UInt(64.W)))
  val carry_sew32 = Wire(Vec(2, UInt(64.W)))
  val sum_add_sew32 = Wire(UInt(64.W))
  val vd_sew32 = Wire(UInt(64.W))
  // stage 1
  val csa_3to2_sew32_0 = Module(new CSA3to2(width = 64))
  csa_3to2_sew32_0.io.in_a := Cat(Fill(32, vector(31) & signed), vector(31, 0))
  csa_3to2_sew32_0.io.in_b := Cat(Fill(32, vector(63) & signed), vector(63, 32))
  csa_3to2_sew32_0.io.in_c := Cat(Fill(32, vector(95) & signed), vector(95, 64))
  sum_sew32(0) := csa_3to2_sew32_0.io.out_sum
  carry_sew32(0) := csa_3to2_sew32_0.io.out_car
  sum_add_sew32 := Cat(Fill(32, vector(127) & signed), vector(127, 96)) + scalar(63, 0)

  val csa_3to2_sew32_1 = Module(new CSA3to2(width = 64))
  csa_3to2_sew32_1.io.in_a := sum_sew32(0)
  csa_3to2_sew32_1.io.in_b := carry_sew32(0)
  csa_3to2_sew32_1.io.in_c := sum_add_sew32
  sum_sew32(1) := csa_3to2_sew32_1.io.out_sum
  carry_sew32(1) := csa_3to2_sew32_1.io.out_car
  // stage 2
  vd_sew32 := vd_reg(127, 64) + vd_reg(63, 0)

  // sew16 (widen) sum
  val sum0_sew16 = Wire(Vec(3, UInt(32.W)))
  val carry0_sew16 = Wire(Vec(3, UInt(32.W)))
  val sum1_sew16 = Wire(Vec(2, UInt(32.W)))
  val carry1_sew16 = Wire(Vec(2, UInt(32.W)))
  val sum2_sew16 = Wire(UInt(32.W))
  val carry2_sew16 = Wire(UInt(32.W))
  val vd_sew16 = Wire(UInt(32.W))

  val in0_sew16_vec = Wire(Vec(9, UInt(32.W)))
  val in0_sew16 = Cat(in0_sew16_vec.reverse)
  val in1_sew16 = Cat(Cat(sum0_sew16.reverse), Cat(carry0_sew16.reverse))
  val in2_sew16 = Cat(Cat(sum1_sew16.reverse), Cat(carry1_sew16.reverse))

  // stage 1
  for (i <- 0 until 8) {
    in0_sew16_vec(i) := Cat(Fill(16, vector(16*(i+1)-1) & signed), vector(16*(i+1)-1, 16*i))
  }
  in0_sew16_vec(8) := scalar(31, 0)

  for (i <- 0 until 3) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 32))
    csa_3to2_sew16.io.in_a := in0_sew16(96 * i + 31, 96 * i + 0)
    csa_3to2_sew16.io.in_b := in0_sew16(96 * i + 63, 96 * i + 32)
    csa_3to2_sew16.io.in_c := in0_sew16(96 * i + 95, 96 * i + 64)
    sum0_sew16(i) := csa_3to2_sew16.io.out_sum
    carry0_sew16(i) := csa_3to2_sew16.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew16 = Module(new CSA3to2(width = 32))
    csa_3to2_sew16.io.in_a := in1_sew16(96 * i + 31, 96 * i + 0)
    csa_3to2_sew16.io.in_b := in1_sew16(96 * i + 63, 96 * i + 32)
    csa_3to2_sew16.io.in_c := in1_sew16(96 * i + 95, 96 * i + 64)
    sum1_sew16(i) := csa_3to2_sew16.io.out_sum
    carry1_sew16(i) := csa_3to2_sew16.io.out_car
  }

  val csa_4to2_sew16 = Module(new CSA4to2(width = 32))
  csa_4to2_sew16.io.in_a := in2_sew16(31, 0)
  csa_4to2_sew16.io.in_b := in2_sew16(63, 32)
  csa_4to2_sew16.io.in_c := in2_sew16(95, 64)
  csa_4to2_sew16.io.in_d := in2_sew16(127, 96)
  sum2_sew16 := csa_4to2_sew16.io.out_sum
  carry2_sew16 := csa_4to2_sew16.io.out_car
  // stage 2
  vd_sew16 := vd_reg(63, 32) + vd_reg(31, 0)

  // sew8 (widen) sum
  val sum0_sew8 = Wire(Vec(4, UInt(16.W)))
  val carry0_sew8 = Wire(Vec(4, UInt(16.W)))
  val sum1_sew8 = Wire(Vec(3, UInt(16.W)))
  val carry1_sew8 = Wire(Vec(3, UInt(16.W)))
  val sum2_sew8 = Wire(Vec(2, UInt(16.W)))
  val carry2_sew8 = Wire(Vec(2, UInt(16.W)))
  val sum3_sew8 = Wire(UInt(16.W))
  val carry3_sew8 = Wire(UInt(16.W))
  val vd_sew8 = Wire(UInt(16.W))

  val in0_sew8_vec = Wire(Vec(16, UInt(16.W)))
  val in0_sew8 = Cat(in0_sew8_vec.reverse)
  val in1_sew8 = Cat(scalar(15, 0), Cat(sum0_sew8.reverse), Cat(carry0_sew8.reverse))
  val in2_sew8 = Cat(Cat(sum1_sew8.reverse), Cat(carry1_sew8.reverse))
  val in3_sew8 = Cat(Cat(sum2_sew8.reverse), Cat(carry2_sew8.reverse))
  // stage 1
  for (i <- 0 until 16) {
    in0_sew8_vec(i) := Cat(Fill(8, vector(8*(i+1)-1) & signed), vector(8*(i+1)-1, 8*i))
  }

  for (i <- 0 until 4) {
    val csa_4to2_sew8 = Module(new CSA4to2(width = 16))
    csa_4to2_sew8.io.in_a := in0_sew8(64 * i + 15, 64 * i + 0)
    csa_4to2_sew8.io.in_b := in0_sew8(64 * i + 31, 64 * i + 16)
    csa_4to2_sew8.io.in_c := in0_sew8(64 * i + 47, 64 * i + 32)
    csa_4to2_sew8.io.in_d := in0_sew8(64 * i + 63, 64 * i + 48)
    sum0_sew8(i) := csa_4to2_sew8.io.out_sum
    carry0_sew8(i) := csa_4to2_sew8.io.out_car
  }

  for (i <- 0 until 3) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 16))
    csa_3to2_sew8.io.in_a := in1_sew8(48 * i + 15, 48 * i + 0)
    csa_3to2_sew8.io.in_b := in1_sew8(48 * i + 31, 48 * i + 16)
    csa_3to2_sew8.io.in_c := in1_sew8(48 * i + 47, 48 * i + 32)
    sum1_sew8(i) := csa_3to2_sew8.io.out_sum
    carry1_sew8(i) := csa_3to2_sew8.io.out_car
  }

  for (i <- 0 until 2) {
    val csa_3to2_sew8 = Module(new CSA3to2(width = 16))
    csa_3to2_sew8.io.in_a := in2_sew8(48 * i + 15, 48 * i + 0)
    csa_3to2_sew8.io.in_b := in2_sew8(48 * i + 31, 48 * i + 16)
    csa_3to2_sew8.io.in_c := in2_sew8(48 * i + 47, 48 * i + 32)
    sum2_sew8(i) := csa_3to2_sew8.io.out_sum
    carry2_sew8(i) := csa_3to2_sew8.io.out_car
  }

  val csa_4to2_sew8 = Module(new CSA4to2(width = 16))
  csa_4to2_sew8.io.in_a := in3_sew8(15, 0)
  csa_4to2_sew8.io.in_b := in3_sew8(31, 16)
  csa_4to2_sew8.io.in_c := in3_sew8(47, 32)
  csa_4to2_sew8.io.in_d := in3_sew8(63, 48)
  sum3_sew8 := csa_4to2_sew8.io.out_sum
  carry3_sew8 := csa_4to2_sew8.io.out_car
  // stage 2
  vd_sew8 := vd_reg(31, 16) + vd_reg(15, 0)

  when(vd_vsew_reg_s1 === 0.U) {
    vd_reg := Cat(sum3_sew8, carry3_sew8)
  }.elsewhen(vd_vsew_reg_s1 === 1.U) {
    vd_reg := Cat(sum2_sew16, carry2_sew16)
  }.elsewhen(vd_vsew_reg_s1 === 2.U) {
    vd_reg := Cat(sum_sew32(1), carry_sew32(1))
  }.elsewhen(vd_vsew_reg_s1 === 3.U) {
    vd_reg := Cat(sum_sew64, carry_sew64)
  }

  val vd_vsew_reg = RegEnable(io.vsew, io.valid)
  // Explicit sew64 arm — do not leave e64 as the Wire default.
  val sum_vd = Wire(UInt(64.W))
  sum_vd := MuxLookup(vd_vsew_reg, vd_sew64)(Seq(
    0.U -> Mux(widen_s2_r, vd_sew8, vd_sew8(7, 0)),
    1.U -> Mux(widen_s2_r, vd_sew16, vd_sew16(15, 0)),
    2.U -> Mux(widen_s2_r, vd_sew32, vd_sew32(31, 0)),
    3.U -> vd_sew64,
  ))

  // VRedWSUM is sampled by NewReduction on valid_s1_r. Keep the CSA path's
  // registered sum_vd (1-cycle latency). A combinational fold on io.vout races
  // the parent stage-2 control.
  io.vout := sum_vd
}

class new_compare_2to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val c = Output(UInt(w.W))
  })

  // a-b
  val fs_a = Cat(io.signed ^ io.a(w-1), io.a(w-2, 0))
  val fs_b = Cat(io.signed ^ io.b(w-1), io.b(w-2, 0))
  val less = Wire(Bool())

  less := fs_a < fs_b
  io.c := Mux(less === io.max, io.b, io.a)
}

class new_compare_3to1(w: Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val c = Input(UInt(w.W))
    val max = Input(Bool())
    val signed = Input(Bool())
    val d = Output(UInt(w.W))
  })

  // a-b, a-c, b-c
  val fs_a = Cat(io.signed ^ io.a(w-1), io.a(w-2, 0))
  val fs_b = Cat(io.signed ^ io.b(w-1), io.b(w-2, 0))
  val fs_c = Cat(io.signed ^ io.c(w-1), io.c(w-2, 0))
  val less = Wire(Vec(3, Bool()))

  less(2) := fs_a < fs_b
  less(1) := fs_a < fs_c
  less(0) := fs_b < fs_c

  io.d := 0.U
  when((less(2) && less(1) && !io.max) || (!less(2) && !less(1) && io.max)) {
    io.d := io.a
  }.elsewhen((!less(2) && less(0) && !io.max) || (less(2) && !less(0) && io.max)) {
    io.d := io.b
  }.elsewhen((!less(1) && !less(0) && !io.max) || (less(1) && less(0) && io.max)) {
    io.d := io.c
  }
}


object VerilogNewRed extends App {
  println("Generating the VPU NewReduction hardware")
  emitVerilog(new NewReduction(), Array("--target-dir", "out/new-reduction-smoke"))
}

object VerilogVRedWSUM extends App {
  emitVerilog(new VRedWSUM(), Array("--target-dir", "out/reduction-smoke"))
}
