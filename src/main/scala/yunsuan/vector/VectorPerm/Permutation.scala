package yunsuan.vector.perm

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._

class Permutation extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN / 64
  val vlenb = VLEN / 8
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VPermInput))
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
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val ele_cnt = vlenb.U / vsew_bytes
  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val current_res_boundary = Mux((vlRemainBytes > vlenb.U), vlenb.U, vlRemainBytes)
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val vd_mask = (~0.U(VLEN.W))

  val vcompress = opcode.isVcompress
  val vslideup = opcode.isVslideup
  val vslidedn = opcode.isVslidedown
  val vslide1up = opcode.isVslide1up
  val vslide1dn = opcode.isVslide1down
  val vrgather = opcode.isVrgather
  val vrgather_vx = opcode.isVrgather_vx
  val vmvnr = opcode.isVmvnr
  val vrgather16_sew8 = opcode.isVrgather && (srcTypeVs1 === 1.U) && (srcTypeVs2 === 0.U)
  val vslide = vslideup || vslide1up || vslidedn || vslide1dn

  val base = Wire(UInt(7.W))
  val vmask0 = Mux(vcompress, vs1, vmask)
  val vmask1 = Mux(vcompress, vs1 >> ele_cnt, vmask >> ele_cnt)
  val vmask_uop = Wire(UInt(VLEN.W))
  val vmask_byte_strb = Wire(Vec(vlenb, UInt(1.W)))
  val vs1_bytes = VecInit(Seq.tabulate(vlenb)(i => vs1((i + 1) * 8 - 1, i * 8)))
  val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val vd_reg = RegInit(0.U(VLEN.W))

  vlRemain := vl
  when((vcompress && (uopIdx === 3.U)) ||
    ((vslideup) && ((uopIdx === 1.U) || (uopIdx === 2.U))) ||
    ((vslidedn) && (uopIdx === 2.U)) ||
    (((vrgather && !vrgather16_sew8) || vrgather_vx) && (uopIdx >= 2.U)) ||
    (vrgather16_sew8 && (uopIdx >= 4.U))
  ) {
    vlRemain := Mux(vl >= ele_cnt, vl - ele_cnt, 0.U)
  }.elsewhen(vslide1up) {
    vlRemain := Mux(vl >= ele_cnt*uopIdx, vl - ele_cnt*uopIdx, 0.U)
  }.elsewhen(vslide1dn) {
    vlRemain := Mux(vl >= ele_cnt*uopIdx(5,1), vl - ele_cnt*uopIdx(5,1), 0.U)
  }

  vmask_uop := vmask0
  when((vcompress && uopIdx(1)) ||
    (vslideup && ((uopIdx === 1.U) || (uopIdx === 2.U))) ||
    (vslidedn && (uopIdx === 2.U)) ||
    (((vrgather && !vrgather16_sew8) || vrgather_vx) && (uopIdx >= 2.U)) ||
    (vrgather16_sew8 && (uopIdx >= 4.U))
  ) {
    vmask_uop := vmask1
  }.elsewhen(vslide1up) {
    vmask_uop := vmask >> (ele_cnt * uopIdx)
  }.elsewhen(vslide1dn) {
    vmask_uop := vmask >> (ele_cnt * uopIdx(5,1))
  }

  when((vcompress && (uopIdx === 3.U)) ||
    (vslideup && (uopIdx === 1.U)) ||
    (vslidedn && (uopIdx === 0.U) && vlmul === 1.U)
  ) {
    base := vlenb.U
  }.otherwise {
    base := 0.U
  }

  for (i <- 0 until vlenb) {
    when(i.U < vlRemainBytes) {
      vmask_byte_strb(i) := vmask_uop(i) | (vm & !vcompress)
      when(vsew === 1.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 2) | (vm & !vcompress)
      }.elsewhen(vsew === 2.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 4) | (vm & !vcompress)
      }.elsewhen(vsew === 3.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 8) | (vm & !vcompress)
      }
    }.otherwise {
      vmask_byte_strb(i) := 0.U
    }
  }

  // vrgather/vrgather16
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(64.W)))
  val first_gather = (vlmul >= 4.U) || (vlmul === 0.U) || ((vlmul === 1.U) && (Mux(vrgather16_sew8, uopIdx(1), uopIdx(0)) === 0.U))
  val vs2_bytes_min = Mux((vrgather16_sew8 && uopIdx(1)) || (((vrgather && !vrgather16_sew8) || vrgather_vx) && uopIdx(0)), vlenb.U, 0.U)
  val vs2_bytes_max = Mux((vrgather16_sew8 && uopIdx(1)) || (((vrgather && !vrgather16_sew8) || vrgather_vx) && uopIdx(0)), Cat(vlenb.U, 0.U), vlenb.U)
  val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))

  for (i <- 0 until vlenb) {
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := 0.U
  }

  for (i <- 0 until vlenb / 2) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vx) {
      vrgather_byte_sel(i) := vs1(63, 0)
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && !uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(2.W)) + i.U % 4.U
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(3.W)) +i.U % 8.U
        }
      }.elsewhen(srcTypeVs1(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs1(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  for (i <- (vlenb / 2) until vlenb) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vx) {
      vrgather_byte_sel(i) := vs1(63, 0)
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          vrgather_byte_sel(i) := Cat(vs1(( i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(2.W)) +i.U % 4.U
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          vrgather_byte_sel(i) := Cat(vs1(( i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(3.W)) +i.U % 8.U
        }
      }.elsewhen(srcTypeVs1(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs1(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  when((vrgather || vrgather_vx) && fire) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := Mux(ma, "hff".U, old_vd(( i + 1) * 8 - 1, i * 8))
      when((vrgather_byte_sel(i) >= vs2_bytes_min) && (vrgather_byte_sel(i) < vs2_bytes_max) && vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := vs2_bytes(vrgather_byte_sel(i.U)-vs2_bytes_min)
      }.elsewhen (first_gather && vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := 0.U 
      }.elsewhen (vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := old_vd(i*8+7, i*8)
      }
    }
  }

  // vslideup/vslide1up
  val slide_ele = Mux(vslide1up || vslide1dn, 1.U, vs1(xLen, 0))
  val slide_bytes = slide_ele << vsew
  val vslideup_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1up_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslidedn_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_wo_rs1 = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_rs1 = Wire(UInt(VLEN.W))
  val first_slidedn = vslidedn && (uopIdx === 0.U || uopIdx === 2.U)
  val load_rs1 = (((vlmul >= 4.U) || (vlmul === 0.U)) && (uopIdx === 0.U)) ||
    ((vlmul === 1.U) && (uopIdx === 2.U)) ||
    ((vlmul === 2.U) && (uopIdx === 6.U)) ||
    (uopIdx === 14.U)
  val vslide1dn_vd = Mux((load_rs1 || uopIdx(0)), VecInit(Seq.tabulate(vlenb)(i => vslide1dn_vd_rs1((i + 1) * 8 - 1, i * 8))), vslide1dn_vd_wo_rs1)

  for (i <- 0 until vlenb) { // offset old_vd
    val in_bounds_up = ((base + i.U) >= slide_bytes) && ((base + i.U - slide_bytes) < vlenb.U)
    vslideup_vd(i) := Mux(ma && in_bounds_up, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(in_bounds_up && (vmask_byte_strb(i) === 1.U)) {
      vslideup_vd(i) := vs2_bytes(base + i.U - slide_bytes)
    }
  }

  for (i <- 0 until vlenb) {
    vslidedn_vd(i) := Mux(ma, "hff".U, old_vd(i*8+7, i*8))
    when(vmask_byte_strb(i) === 1.U) {
      when(((i.U + slide_bytes) >= base) && ((i.U + slide_bytes - base) < vlenb.U)) {
        vslidedn_vd(i) := vs2_bytes(i.U + slide_bytes - base)
      }.elsewhen(first_slidedn) {
        vslidedn_vd(i) := 0.U
      }.otherwise {
        vslidedn_vd(i) := old_vd(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1up_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vslide1up && (vmask_byte_strb(i) === 1.U)) {
      when((i.U < vsew_bytes)) {
        vslide1up_vd(i) := vs1_bytes(vlenb.U - vsew_bytes + i.U)
      }.otherwise {
        vslide1up_vd(i) := vs2_bytes(i.U - vsew_bytes)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1dn_vd_wo_rs1(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vslide1dn && !uopIdx(0) && (vmask_byte_strb(i) === 1.U)) {
      when(i.U < (vlenb.U - vsew_bytes)) {
        vslide1dn_vd_wo_rs1(i) := vs2_bytes(vsew_bytes + i.U)
      }.otherwise {
        vslide1dn_vd_wo_rs1(i) := vs1_bytes(i.U + vsew_bytes - vlenb.U)
      }
    }
  }

  vslide1dn_vd_rs1 := old_vd
  when(load_rs1 || uopIdx(0)) {
    when((vlRemainBytes > 0.U) && (vlRemainBytes <= vlenb.U) && (vmask_byte_strb(vlRemainBytes - 1.U).asBool)) {
      vslide1dn_vd_rs1 := (Cat(vslide1dn_vd_wo_rs1.reverse) & (vd_mask >> (VLEN.U - Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))) |
        (vs1 & (vd_mask << Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))
    }
  }

  val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U - vlRemainBytes)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = vd_mask >> tail_bits
  val tail_old_vd = old_vd & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta, tail_ones_vd, tail_old_vd)
  val perm_vd = Wire(Vec(vlenb, UInt(8.W)))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))


  perm_vd := vslideup_vd
  when(vslideup && fire) {
    perm_vd := vslideup_vd
  }.elsewhen(vslide1up && fire) {
    perm_vd := vslide1up_vd
  }.elsewhen(vslidedn && fire) {
    perm_vd := vslidedn_vd
  }.elsewhen(vslide1dn && fire) {
    perm_vd := vslide1dn_vd
  }.elsewhen((vrgather || vrgather_vx) && fire) {
    perm_vd := vrgather_vd
  }

  perm_tail_mask_vd := Cat(perm_vd.reverse)
  when((vslideup && fire && (uopIdx =/= 1.U)) ||
    (vslidedn && fire && (uopIdx =/= 0.U)) ||
    (vslide1up && fire) ||
    (vslide1dn && fire && (uopIdx(0) || load_rs1))) {
    perm_tail_mask_vd := (Cat(perm_vd.reverse) & vmask_tail_bits) | tail_vd
  }

  val in_previous_ones_sum = Wire(UInt(7.W))
  val out_previous_ones_sum = Wire(UInt(7.W))
  val res_idx = Wire(Vec(vlenb, UInt(7.W)))
  val res_valid = Wire(Vec(vlenb, Bool()))
  val current_ones_sum = Wire(Vec(vlenb + 1, UInt(7.W)))

  val cmprs_vd = Wire(Vec(vlenb, UInt(8.W)))

  out_previous_ones_sum := Mux1H(Seq(
    eewVs2.is8 -> PopCount(vs1(15, 0)),
    eewVs2.is16 -> Cat(PopCount(vs1(7, 0)), 0.U(1.W)),
    eewVs2.is32 -> Cat(PopCount(vs1(3, 0)), 0.U(2.W)),
    eewVs2.is64 -> Cat(PopCount(vs1(1, 0)), 0.U(3.W))
  ))

  when(vcompress && fire) {
    when(uopIdx === 1.U) {
      vd_reg := Cat(0.U((VLEN - 7).W), out_previous_ones_sum)
    }.otherwise {
      vd_reg := Cat(cmprs_vd.reverse)
    }
  }.elsewhen(vrgather16_sew8 && fire) {
    when(uopIdx(0)) {
      vd_reg := Cat(perm_tail_mask_vd(VLEN - 1, VLEN / 2), old_vd(VLEN / 2 - 1, 0))
    }.otherwise {
      vd_reg := Cat(old_vd(VLEN - 1, VLEN / 2), perm_tail_mask_vd(VLEN / 2 - 1, 0))
    }
  }.elsewhen((vslide || vrgather || vrgather_vx) && fire) {
    vd_reg := perm_tail_mask_vd
  }.elsewhen(vmvnr && fire) {
    vd_reg := vs2
  }


  in_previous_ones_sum := vmask(6, 0)

  for (i <- 0 until (vlenb + 1)) {
    current_ones_sum(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    cmprs_vd(i) := Mux(ta && (i.U + base >= current_ones_sum(vlenb)), "hff".U, old_vd(i * 8 + 7, i * 8))
    res_idx(i) := 0.U
    res_valid(i) := false.B
  }

  current_ones_sum(0) := in_previous_ones_sum
  for (i <- 0 until vlenb) {
    current_ones_sum(i + 1) := current_ones_sum(i) + vmask_byte_strb(i)
    res_idx(i) := current_ones_sum(i + 1) - base - 1.U
    res_valid(i) := current_ones_sum(i + 1) >= base + 1.U
    when((vmask_byte_strb(i) === 1.U) && res_valid(i) && (res_idx(i) < current_res_boundary)) {
      cmprs_vd(res_idx(i)) := vs2(i * 8 + 7, i * 8)
    }
  }

  io.out.vd := vd_reg
  io.out.vxsat := false.B

}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "build/vifu"))
}


