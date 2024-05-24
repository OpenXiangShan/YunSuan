package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.util.ZeroExt

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
  val ele_cnt = vlenb.U >> vsew
  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val all_one = (~0.U(VLEN.W))

  val vcpop_m = opcode.isVcpop
  val vfirst_m = opcode.isVfirst
  val vmsbf_m = opcode.isVmsbf
  val vmsif_m = opcode.isVmsif
  val vmsof_m = opcode.isVmsof
  val viota_m = opcode.isViota
  val vid_v = opcode.isVid

  val vmfirst = Wire(UInt(xLen.W))
  val vmsbf = Wire(UInt(VLEN.W))
  val vmsif = Cat(vmsbf(VLEN - 2, 0), 1.U)
  val vmsof = Wire(UInt(VLEN.W))
  val nmask = Wire(UInt(VLEN.W))
  val vd_nmask = Wire(UInt(VLEN.W))
  val vd_mask = Wire(UInt(VLEN.W))
  val vs2m = Wire(Vec(VLEN, UInt(1.W)))

  // to ensure timing requirements delay onecyle
  val fire_reg_s1 = RegNext(fire)
  val vl_reg_s1 = RegEnable(vl, 0.U, fire)
  val vstart_reg_s1 = RegEnable(vstart, 0.U, fire)
  val vm_reg_s1 = RegEnable(vm, false.B, fire)
  val ma_reg_s1 = RegEnable(ma, false.B, fire)
  val ta_reg_s1 = RegEnable(ta, false.B, fire)
  val vsew_reg_s1 = RegEnable(vsew, 0.U, fire)
  val uopIdx_reg_s1 = RegEnable(uopIdx, 0.U, fire)
  val vlRemainBytes_reg_s1 = RegEnable(vlRemainBytes, 0.U, fire)
  val vs1_reg_s1 = RegEnable(vs1, 0.U, fire)
  val ele_cnt_reg_s1 = RegEnable(ele_cnt,0.U,fire)

  val old_vd_reg_s1 = RegEnable(old_vd, 0.U, fire)
  val vmask_reg_s1 = RegEnable(vmask, 0.U, fire)
  val vcpop_m_reg_s1 = RegEnable(vcpop_m, false.B, fire)
  val vfirst_m_reg_s1 = RegEnable(vfirst_m, false.B, fire)
  val vmsbf_m_reg_s1 = RegEnable(vmsbf_m, false.B, fire)
  val vmsif_m_reg_s1 = RegEnable(vmsif_m, false.B, fire)
  val vmsof_m_reg_s1 = RegEnable(vmsof_m, false.B, fire)
  val viota_m_reg_s1 = RegEnable(viota_m, false.B, fire)
  val vid_v_reg_s1 = RegEnable(vid_v, false.B, fire)
  val vs2m_reg_s1 = RegEnable(vs2m, VecInit(Seq.fill(VLEN)(0.U(1.W))),fire)
  //stage 0
  def sbf(data: UInt): UInt = {
    val w = data.getWidth
    val result = Wire(UInt(w.W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = sbf(data(w - 1, w / 2))
      val lo = sbf(data(w / 2 - 1, 0))
      result := Mux(lo(w / 2 - 1), Cat(hi, lo), Cat(0.U((w / 2).W), lo))
    }
    result
  }

  def vfirst(data: UInt): UInt = {
    val w = data.getWidth
    val logW = log2Ceil(w) // 1 -> 0, 2 -> 1, 4 -> 2
    val result = Wire(UInt((logW + 1).W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = vfirst(data(w - 1, w / 2))
      val lo = vfirst(data(w / 2 - 1, 0))
      result := Mux(!lo(logW - 1), Cat(0.U(1.W), lo),
        if (w == 2) Cat(hi(logW - 1), 1.U(1.W)) else Cat(hi(logW - 1), 1.U(1.W), hi(logW - 2, 0)))
    }
    result
  }

  vlRemain := Mux(vl >= Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1), vl - Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1), 0.U)

  for (i <- 0 until VLEN) {
    vs2m(i) := 0.U
    when(fire) {
      vs2m(i) := vs2(i) & (vmask(i) | vm) & (i.U < vl)
    }
  }
  // end stage 0
  // stage 1
  vmsof := ~vmsbf & vmsif
  vmsbf := sbf(Cat(vs2m_reg_s1.reverse))
  vmfirst := Mux(
    vs2m_reg_s1.asUInt.orR,
    ZeroExt(vfirst(Cat(vs2m_reg_s1.reverse)), xLen),
    Fill(xLen, 1.U(1.W))
  )
  // end stage 1
  // stage 0
  // viota/vid/vcpop
  val vs2m_uop = Cat(vs2m.reverse) >> Mux(vcpop_m, uopIdx << vsew_plus1, uopIdx(5, 1) << vsew_plus1)
  val vs2m_uop_vid = Mux(vid_v, Fill(16, vid_v), vs2m_uop(15, 0))
  // end stage0
  // stage_1
  val one_sum = vs1_reg_s1(7, 0)
  val one_cnt = Wire(Vec(vlenb + 1, UInt(8.W)))
  val one_cnt_uop = Wire(Vec(vlenb + 1, UInt(5.W)))
  val one_cnt_uop_sew8 = Wire(Vec(vlenb + 1, UInt(5.W)))
  val one_cnt_uop_sew16 = Wire(Vec(vlenb + 1, UInt(5.W)))
  val one_cnt_uop_sew32 = Wire(Vec(vlenb + 1, UInt(5.W)))
  val one_cnt_uop_sew64 = Wire(Vec(vlenb + 1, UInt(5.W)))
  val vid_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew8 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew16 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew32 = Wire(Vec(vlenb, UInt(8.W)))
  val vid_vd_sew64 = Wire(Vec(vlenb, UInt(8.W)))


  // to ensure timing requirements delay onecyle
  // stage 0 to stage 1
  val vs2m_uop_vid_reg_s1 = RegEnable(vs2m_uop_vid, 0.U, fire)

  // stage 1
  for (i <- 0 until vlenb + 1) {
    one_cnt_uop(i) := 0.U
    one_cnt_uop_sew8(i) := 0.U
    one_cnt_uop_sew16(i) := 0.U
    one_cnt_uop_sew32(i) := 0.U
    one_cnt_uop_sew64(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    one_cnt_uop_sew8(i + 1) := PopCount(vs2m_uop_vid_reg_s1(i, 0))
  }

  for (i <- 0 until vlenb / 2) {
    one_cnt_uop_sew16(i + 1) := PopCount(vs2m_uop_vid_reg_s1(i, 0))
  }

  for (i <- 0 until vlenb / 4) {
    one_cnt_uop_sew32(i + 1) := PopCount(vs2m_uop_vid_reg_s1(i, 0))
  }

  for (i <- 0 until vlenb / 8) {
    one_cnt_uop_sew64(i + 1) := PopCount(vs2m_uop_vid_reg_s1(i, 0))
  }

  for (i <- 0 until vlenb + 1) {
    when(vsew_reg_s1 === 0.U) {
      one_cnt_uop(i) := one_cnt_uop_sew8(i)
    }.elsewhen(vsew_reg_s1 === 1.U) {
      one_cnt_uop(i) := one_cnt_uop_sew16(i)
    }.elsewhen(vsew_reg_s1 === 2.U) {
      one_cnt_uop(i) := one_cnt_uop_sew32(i)
    }.elsewhen(vsew_reg_s1 === 3.U) {
      one_cnt_uop(i) := one_cnt_uop_sew64(i)
    }
  }

  for (i <- 0 until vlenb + 1) {
    one_cnt(i) := one_sum + one_cnt_uop(i)
  }
  // end stage 1

  val tail_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))
  val vd_out = Wire(UInt(VLEN.W))
  // stage 1 to stage 2
  when(vmsbf_m_reg_s1 && fire_reg_s1) {
    vd_reg := vmsbf
  }.elsewhen(vmsif_m_reg_s1 && fire_reg_s1) {
    vd_reg := vmsif
  }.elsewhen(vmsof_m_reg_s1 && fire_reg_s1) {
    vd_reg := vmsof
  }.elsewhen(vfirst_m_reg_s1 && fire_reg_s1) {
    vd_reg := Cat(0.U((VLEN - xLen).W), vmfirst)
  }.elsewhen((vid_v_reg_s1 || viota_m_reg_s1) && fire_reg_s1 && !uopIdx_reg_s1(0)) {
    vd_reg := Cat(one_cnt.reverse)(VLEN - 1, 0)
  }.elsewhen((((vid_v_reg_s1 || viota_m_reg_s1) && uopIdx_reg_s1(0)) || vcpop_m_reg_s1) && fire_reg_s1) {
    vd_reg := one_cnt(ele_cnt_reg_s1)
  }
  //end stage 1 to stage 2

  val vstartRemain = Wire(UInt(7.W))
  // stage 0
  vstartRemain := Mux(vid_v, Mux(vstart >= (uopIdx(5, 1) << vsew_plus1), (vstart - (uopIdx(5, 1) << vsew_plus1)), 0.U), 0.U)
  val vstartRemain_reg_s1 = RegEnable(vstartRemain, 0.U, fire)
  // end stage 0
  // stage 1 to stage 2
  val vl_reg = RegEnable(vl_reg_s1, 0.U, fire_reg_s1)
  val vstart_reg = RegEnable(vstart_reg_s1, 0.U, fire_reg_s1)
  val vm_reg = RegEnable(vm_reg_s1, false.B, fire_reg_s1)
  val ma_reg = RegEnable(ma_reg_s1, false.B, fire_reg_s1)
  val ta_reg = RegEnable(ta_reg_s1, false.B, fire_reg_s1)
  val vsew_reg = RegEnable(vsew_reg_s1, 0.U, fire_reg_s1)
  val uopIdx_reg = RegEnable(uopIdx_reg_s1, 0.U, fire_reg_s1)
  val vlRemainBytes_reg = RegEnable(vlRemainBytes_reg_s1, 0.U, fire_reg_s1)
  val vstartRemain_reg = RegEnable(vstartRemain_reg_s1, 0.U, fire_reg_s1)
  val old_vd_reg = RegEnable(old_vd_reg_s1, 0.U, fire_reg_s1)
  val vmask_reg = RegEnable(vmask_reg_s1, 0.U, fire_reg_s1)
  val reg_vcpop_m = RegEnable(vcpop_m_reg_s1, false.B, fire_reg_s1)
  val reg_vfirst_m = RegEnable(vfirst_m_reg_s1, false.B, fire_reg_s1)
  val reg_vmsbf_m = RegEnable(vmsbf_m_reg_s1, false.B, fire_reg_s1)
  val reg_vmsif_m = RegEnable(vmsif_m_reg_s1, false.B, fire_reg_s1)
  val reg_vmsof_m = RegEnable(vmsof_m_reg_s1, false.B, fire_reg_s1)
  val reg_viota_m = RegEnable(viota_m_reg_s1, false.B, fire_reg_s1)
  val reg_vid_v = RegEnable(vid_v_reg_s1, false.B, fire_reg_s1)
  //end stage 1 to stage 2

  //stage 2
  val vsew_plus1_reg = Wire(UInt(3.W))
  vsew_plus1_reg := Cat(0.U(1.W), ~vsew_reg) + 1.U

  val vmask_bits = Wire(UInt(VLEN.W))
  vmask_bits := vmask_reg >> (uopIdx_reg(5, 1) << vsew_plus1_reg)
  val vmask_vd_bytes = Wire(Vec(vlenb, UInt(8.W)))
  val vmask_vd_bits = Cat(vmask_vd_bytes.reverse)
  val vmask_old_vd = old_vd_reg & (~vmask_vd_bits)
  val vmask_ones_vd = all_one & (~vmask_vd_bits)
  val vmask_vd = Mux(ma_reg, vmask_ones_vd, vmask_old_vd)
  val vid_mask_vd = (Cat(vid_vd.reverse) & vmask_vd_bits) | vmask_vd

  val vstartRemainBytes = vstartRemain_reg << vsew_reg
  val vstart_bytes = Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := all_one << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := all_one >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val vid_tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val vid_tail_mask_vd = Wire(UInt(VLEN.W))
  val vd_reg_byte = VecInit(Seq.tabulate(vlenb)(i => vd_reg((i + 1) * 8 - 1, i * 8)))

  for (i <- 0 until vlenb) {
    vid_vd(i) := 0.U
    vid_vd_sew8(i) := 0.U
    vid_vd_sew16(i) := 0.U
    vid_vd_sew32(i) := 0.U
    vid_vd_sew64(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    vid_vd_sew8(i) := vd_reg_byte(i)
  }

  for (i <- 0 until vlenb) {
    when(i.U % 2.U === 0.U) {
      vid_vd_sew16(i) := vd_reg_byte(i / 2)
    }.otherwise {
      vid_vd_sew16(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(i.U % 4.U === 0.U) {
      vid_vd_sew32(i) := vd_reg_byte(i / 4)
    }.otherwise {
      vid_vd_sew32(i) := 0.U
    }
  }
  for (i <- 0 until vlenb) {
    when(i.U % 8.U === 0.U) {
      vid_vd_sew64(i) := vd_reg_byte(i / 8)
    }.otherwise {
      vid_vd_sew64(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(vsew_reg === 0.U) {
      vid_vd(i) := vid_vd_sew8(i)
    }.elsewhen(vsew_reg === 1.U) {
      vid_vd(i) := vid_vd_sew16(i)
    }.elsewhen(vsew_reg === 2.U) {
      vid_vd(i) := vid_vd_sew32(i)
    }.elsewhen(vsew_reg === 3.U) {
      vid_vd(i) := vid_vd_sew64(i)
    }
  }

  for (i <- 0 until vlenb) {
    vmask_vd_bytes(i) := "hff".U
    when((!vm_reg && !vmask_bits(i.U >> vsew_reg)) || (i.U >= vlRemainBytes_reg)) {
      vmask_vd_bytes(i) := 0.U
    }
  }

  vid_tail_mask_vd := 0.U
  when((reg_vid_v || reg_viota_m) && !uopIdx_reg(0)) {
    vid_tail_mask_vd := (vid_mask_vd & vmask_tail_bits & vmask_vstart_bits) | vid_tail_vd | vstart_old_vd
  }

  nmask := ~(vmask_reg | Fill(VLEN, vm_reg))
  vd_nmask := Mux(ma_reg, nmask, old_vd_reg & nmask)
  vd_mask := vd_reg & (vmask_reg | Fill(VLEN, vm_reg))
  val mask_vd = vd_nmask | vd_mask


  val old_vd_vl_mask = Wire(UInt(VLEN.W))
  val vd_vl_mask = Wire(UInt(VLEN.W))

  old_vd_vl_mask := (~0.U(VLEN.W)) << vl_reg
  vd_vl_mask := (~0.U(VLEN.W)) >> (VLEN.U - vl_reg)

  tail_vd := old_vd_vl_mask | (mask_vd & vd_vl_mask)

  vd_out := vd_reg
  when(vstart_reg >= vl_reg && !reg_vfirst_m && !reg_vcpop_m) {
    vd_out := old_vd_reg
  }.elsewhen(reg_vmsbf_m || reg_vmsif_m || reg_vmsof_m) {
    vd_out := tail_vd
  }.elsewhen((reg_vid_v || reg_viota_m) && !uopIdx_reg(0)) {
    vd_out := vid_tail_mask_vd
  }
  // end stage 2

  io.out.vd := vd_out
  io.out.vxsat := false.B

  dontTouch(vmfirst)
}

object VerilogMask extends App {
  println("Generating the VPU Mask hardware")
  emitVerilog(new VMask(), Array("--target-dir", "build/vifu"))
}
