package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._

class VMask extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN / 64
  val vlenb = VLEN / 8
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = Output(new VIFuOutput)
  })

  val vs1 = io.in.bits.vs1
  val vs2 = VecInit(Seq.tabulate(NLanes)(i => (io.in.bits.vs2) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val old_vd = VecInit(Seq.tabulate(NLanes)(i => (io.in.bits.old_vd) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val vmask = VecInit(Seq.tabulate(NLanes)(i => (io.in.bits.mask) ((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val opcode = io.in.bits.opcode
  val srcTypeVs2 = io.in.bits.srcType(0)
  val srcTypeVs1 = io.in.bits.srcType(1)
  val vdType = io.in.bits.vdType
  val vm = io.in.bits.info.vm
  val ma = io.in.bits.info.ma
  val ta = io.in.bits.info.ta
  val vlmul = io.in.bits.info.vlmul
  val vstart = io.in.bits.info.vstart
  val vl = io.in.bits.info.vl
  val uopIdx = io.in.bits.info.uopIdx
  val fire = io.in.valid

  val vsew = vdType(1, 0)
  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val ele_cnt = vlenb.U / vsew_bytes
  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val vd_mask = (~0.U(VLEN.W))

  val vcpop_m = opcode.isVcpop
  val vfirst_m = opcode.isVfirst
  val vmsbf_m = opcode.isVmsbf
  val vmsif_m = opcode.isVmsif
  val vmsof_m = opcode.isVmsof
  val viota_m = opcode.isViota
  val vid_v = opcode.isVid

  val first = Wire(Vec(NLanes, SInt(xLen.W)))
  val vmfirst = Wire(SInt(xLen.W))
  val vmsbf = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vmsif = VecInit(Seq.tabulate(NLanes)(i => Cat(Cat(vmsbf.reverse)(VLEN - 2, 0), 1.U)((i + 1) * LaneWidth - 1, i * LaneWidth)))
  val vmsof = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vd_vmsbf = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vd_vmsif = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vd_vmsof = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val ohasone = Wire(Vec(NLanes, Bool()))
  val ihasone = Wire(Vec(NLanes, Bool()))
  val nmask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vd_nmask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val sbf_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val sif_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val sof_mask = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vs2m = Wire(Vec(VLEN, UInt(1.W)))

  vlRemain := Mux(vl >= Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1), vl - Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1), 0.U)

  for (i <- 0 until VLEN) {
    vs2m(i) := 0.U
    when(fire) {
      vs2m(i) := Cat(vs2.reverse)(i) & (Cat(vmask.reverse)(i) | vm) & (i.U < vl)
    }
  }

  for (i <- 0 until NLanes) {
    vmsof(i) := (~vmsbf(i)) & vmsif(i)
    nmask(i) := ~(vmask(i) | Fill(LaneWidth, vm))
    vd_nmask(i) := Mux(ma, nmask(i), old_vd(i) & nmask(i))
    sbf_mask(i) := vmsbf(i) & (vmask(i) | Fill(LaneWidth, vm))
    sif_mask(i) := vmsif(i) & (vmask(i) | Fill(LaneWidth, vm))
    sof_mask(i) := vmsof(i) & (vmask(i) | Fill(LaneWidth, vm))
    vd_vmsbf(i) := vd_nmask(i) | sbf_mask(i)
    vd_vmsif(i) := vd_nmask(i) | sif_mask(i)
    vd_vmsof(i) := vd_nmask(i) | sof_mask(i)
  }

  ihasone(0) := false.B
  for (i <- 1 until NLanes) {
    ihasone(i) := ohasone(i - 1)
  }

  val slices = Seq.fill(NLanes)(Module(new VMaskSlice))
  for (i <- 0 until NLanes) {
    slices(i).io.ihasone := ihasone(i)
    slices(i).io.vs2 := vs2m.slice(i * LaneWidth, (i + 1) * LaneWidth)
    first(i) := Mux(slices(i).io.first(LaneWidth - 1).asBool, -1.S, slices(i).io.first + (i * LaneWidth).S)
    vmsbf(i) := slices(i).io.sbf
    ohasone(i) := slices(i).io.ohasone
  }

  vmfirst := -1.S
  for (i <- 0 until NLanes reverse) {
    when(first(i) =/= -1.S) {
      vmfirst := first(i)
    }
  }

  // viota/vid/vcpop
  val vs2m_uop = Cat(vs2m.reverse) >> Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1)
  val one_sum = vs1
  val one_cnt = Wire(Vec(vlenb + 1, UInt(64.W)))
  val vid_vd = Wire(Vec(vlenb, UInt(8.W)))

  for (i <- 0 until vlenb + 1) {
    one_cnt(i) := one_sum
  }

  for (i <- 0 until vlenb) {
    vid_vd(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    when(i.U < ele_cnt) {
      when(vs2m_uop(i).asBool || vid_v) {
        one_cnt(i + 1) := one_cnt(i) + 1.U
      }.otherwise {
        one_cnt(i + 1) := one_cnt(i)
      }
    }
  }

  for (i <- 0 until vlenb) {
    when(vsew === 0.U) {
      vid_vd(i) := one_cnt(i.U / vsew_bytes)(7, 0)
    }.elsewhen(vsew === 1.U) {
      vid_vd(i) := one_cnt(i.U / vsew_bytes)((i % 2 + 1) * 8 - 1, (i % 2) * 8)
    }.elsewhen(vsew === 2.U) {
      vid_vd(i) := one_cnt(i.U / vsew_bytes)((i % 4 + 1) * 8 - 1, (i % 4) * 8)
    }.elsewhen(vsew === 3.U) {
      vid_vd(i) := one_cnt(i.U / vsew_bytes)((i % 8 + 1) * 8 - 1, (i % 8) * 8)
    }
  }

  val vmask_bits = Wire(UInt(VLEN.W))
  val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)

  val vmask_old_vd = Cat(old_vd.reverse) & (~vmask_vd_bits)
  val vmask_ones_vd = vd_mask & (~vmask_vd_bits)
  val vmask_vd = Mux(ma, vmask_ones_vd, vmask_old_vd)

  vmask_bits := Cat(vmask.reverse) >> (uopIdx(5, 1) << vsew_plus1)

  for (i <- 0 until vlenb) {
    vmask_vd_bytes(i) := "hff".U
    when((!vm && !vmask_bits(i.U / vsew_bytes)) || (i.U >= vlRemainBytes)) {
      vmask_vd_bytes(i) := 0.U
    }
  }

  val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd

  val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U - vlRemainBytes)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = vd_mask >> tail_bits
  val tail_old_vd = Cat(old_vd.reverse) & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val vid_tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)
  val vid_tail_mask_vd = Wire(UInt(VLEN.W))

  val vstartRemain = Wire(UInt(7.W))
  vstartRemain := Mux(vid_v, Mux(vstart >= (uopIdx(5, 1) << vsew_plus1), (vstart - (uopIdx(5, 1) << vsew_plus1)), 0.U), 0.U)
  val vstartRemainBytes = vstartRemain << vsew
  val vstart_bytes = Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = vd_mask << vstart_bits
  val vstart_old_vd = Cat(old_vd.reverse) & (~vmask_vstart_bits)

  vid_tail_mask_vd := 0.U
  when((vid_v || viota_m) && fire && !uopIdx(0)) {
    vid_tail_mask_vd := (vid_mask_vd & vmask_tail_bits & vmask_vstart_bits) | vid_tail_vd | vstart_old_vd
  }.elsewhen((((vid_v || viota_m) && uopIdx(0)) || vcpop_m) && fire) {
    vid_tail_mask_vd := one_cnt(ele_cnt)
  }

  val vd = Wire(UInt(VLEN.W))
  val rd = Wire(UInt(xLen.W))
  val tail_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))

  when(vmsbf_m && fire) {
    vd := Cat(vd_vmsbf.reverse)
  }.elsewhen(vmsif_m && fire) {
    vd := Cat(vd_vmsif.reverse)
  }.elsewhen(vmsof_m && fire) {
    vd := Cat(vd_vmsof.reverse)
  }.otherwise {
    vd := vd_reg
  }

  val old_vd_vl_mask = (~0.U(VLEN.W)) << vl
  val vd_vl_mask = (~0.U(VLEN.W)) >> (VLEN.U - vl)

  tail_vd := old_vd_vl_mask | (vd & vd_vl_mask)

  when((vmsbf_m || vmsif_m || vmsof_m) && fire) {
    vd_reg := tail_vd
  }.elsewhen(vfirst_m && fire) {
    vd_reg := Cat(0.U((VLEN - xLen).W), rd)
  }.elsewhen((vid_v || viota_m || vcpop_m) && fire) {
    vd_reg := vid_tail_mask_vd
  }

  rd := 0.U
  when(vfirst_m && fire) {
    rd := vmfirst.asUInt
  }

  io.out.vd := vd_reg
  io.out.vxsat := false.B
}

object VerilogMask extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMask(), Array("--target-dir", "build/vifu"))
}
