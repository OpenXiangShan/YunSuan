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
  val vstart = io.in.bits.info.vstart
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
  val ele_cnt = vlenb.U >> vsew

  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
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
  val emul = vlmul(1, 0)
  val evl = Mux1H(Seq.tabulate(4)(i => (emul === i.U) -> (ele_cnt << i.U)))

  val vslideup_vl = Wire(UInt(8.W))
  vlRemain := vslideup_vl
  when((vcompress && uopIdx(1)) ||
    (vslideup && ((uopIdx === 1.U) || (uopIdx === 2.U))) ||
    (vslidedn && (uopIdx === 2.U)) ||
    (((vrgather && !vrgather16_sew8) || vrgather_vx) && (uopIdx >= 2.U)) ||
    (vrgather16_sew8 && (uopIdx >= 4.U))
  ) {
    vlRemain := Mux(vslideup_vl >= ele_cnt, vslideup_vl - ele_cnt, 0.U)
  }.elsewhen(vslide1up) {
    vlRemain := Mux(vl >= (uopIdx << vsew_plus1), vl - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vlRemain := Mux(vl >= (uopIdx(5, 1) << vsew_plus1), vl - (uopIdx(5, 1) << vsew_plus1), 0.U)
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
    vmask_uop := vmask >> (uopIdx << vsew_plus1)
  }.elsewhen(vslide1dn) {
    vmask_uop := vmask >> (uopIdx(5, 1) << vsew_plus1)
  }

  when((vcompress && (uopIdx === 3.U)) ||
    (vslideup && (uopIdx === 1.U)) ||
    (vslidedn && (uopIdx === 0.U) && (vlmul === 1.U))
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
  val vlmax_bytes = Wire(UInt(5.W))
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(64.W)))
  val first_gather = (vlmul >= 4.U) || (vlmul === 0.U) || ((vlmul === 1.U) && (Mux(vrgather16_sew8, uopIdx(1), uopIdx(0)) === 0.U))
  val vs2_bytes_min = Mux((vrgather16_sew8 && uopIdx(1)) || (((vrgather && !vrgather16_sew8) || vrgather_vx) && uopIdx(0)), vlenb.U, 0.U)
  val vs2_bytes_max = Mux((vrgather16_sew8 && uopIdx(1)) || (((vrgather && !vrgather16_sew8) || vrgather_vx) && uopIdx(0)), Cat(vlenb.U, 0.U), vlmax_bytes)
  val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))

  vlmax_bytes := vlenb.U
  when(vlmul === 5.U) {
    vlmax_bytes := (vlenb / 8).U
  }.elsewhen(vlmul === 6.U) {
    vlmax_bytes := (vlenb / 4).U
  }.elsewhen(vlmul === 7.U) {
    vlmax_bytes := (vlenb / 2).U
  }

  for (i <- 0 until vlenb) {
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := 0.U
  }

  for (i <- 0 until vlenb / 2) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vx) {
      vrgather_byte_sel(i) := vs1(63, 0)
      when(srcTypeVs2(1, 0) === 1.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && !uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          when(uopIdx(1).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          when(uopIdx(1) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }
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
      when(srcTypeVs2(1, 0) === 1.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          when(uopIdx(1).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          when(uopIdx(1) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }
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
      vrgather_vd(i) := Mux(ma, "hff".U, old_vd((i + 1) * 8 - 1, i * 8))
      when(vmask_byte_strb(i).asBool) {
        when((vrgather_byte_sel(i) >= vs2_bytes_min) && (vrgather_byte_sel(i) < vs2_bytes_max)) {
          vrgather_vd(i) := vs2_bytes(vrgather_byte_sel(i.U) - vs2_bytes_min)
        }.elsewhen(first_gather) {
          vrgather_vd(i) := 0.U
        }.otherwise {
          vrgather_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
        }
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

  for (i <- 0 until vlenb) {
    vslideup_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vmask_byte_strb(i).asBool) {
      when(((base + i.U) >= slide_bytes) && ((base + i.U - slide_bytes) < vlenb.U)) {
        vslideup_vd(i) := vs2_bytes(base + i.U - slide_bytes)
      }.otherwise {
        vslideup_vd(i) := old_vd(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslidedn_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vmask_byte_strb(i).asBool) {
      when(((i.U + slide_bytes) >= base) && ((i.U + slide_bytes - base) < vlmax_bytes)) {
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

  val rs1_old_vd = Mux(load_rs1, Cat(vslide1dn_vd_wo_rs1.reverse), old_vd)
  vslide1dn_vd_rs1 := Mux(load_rs1, Cat(vslide1dn_vd_wo_rs1.reverse), old_vd)
  when(load_rs1 || uopIdx(0)) {
    when((vlRemainBytes > 0.U) && (vlRemainBytes <= vlenb.U) && (vmask_byte_strb(vlRemainBytes - 1.U).asBool)) {
      vslide1dn_vd_rs1 := (rs1_old_vd & (vd_mask >> (VLEN.U - Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))) |
        (vs1 & (vd_mask << Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))
    }
  }

  val vstartRemain = Wire(UInt(8.W))
  val vstartRemainBytes = vstartRemain << vsew

  val vslideup_vstart = Mux(vslideup & (slide_ele > vstart), Mux(slide_ele > VLEN.U, VLEN.U, slide_ele), vstart)
  vstartRemain := vslideup_vstart
  when((vcompress && (uopIdx === 3.U)) ||
    ((vslideup) && ((uopIdx === 1.U) || (uopIdx === 2.U))) ||
    ((vslidedn) && (uopIdx === 2.U)) ||
    (((vrgather && !vrgather16_sew8) || vrgather_vx) && (uopIdx >= 2.U)) ||
    (vrgather16_sew8 && (uopIdx >= 4.U))
  ) {
    vstartRemain := Mux(vslideup_vstart >= ele_cnt, vslideup_vstart - ele_cnt, 0.U)
  }.elsewhen(vslide1up) {
    vstartRemain := Mux(vstart >= (uopIdx << vsew_plus1), vstart - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vstartRemain := Mux(vstart >= (uopIdx(5, 1) << vsew_plus1), vstart - (uopIdx(5, 1) << vsew_plus1), 0.U)
  }

  val in_previous_ones_sum = Wire(UInt(8.W))
  val out_previous_ones_sum = Wire(UInt(8.W))
  val res_idx = Wire(Vec(vlenb, UInt(7.W)))
  val res_valid = Wire(Vec(vlenb, Bool()))
  val current_ones_sum = Wire(Vec(vlenb, UInt(8.W)))
  val current_uop_ones_sum = Wire(Vec(vlenb, UInt(5.W)))
  val vd_reg = RegInit(0.U(VLEN.W))

  in_previous_ones_sum := vmask(7, 0)
  out_previous_ones_sum := current_uop_ones_sum(vlenb - 1)

  for (i <- 0 until vlenb) {
    current_uop_ones_sum(i) := PopCount(Cat(vmask_byte_strb.reverse)(i, 0))
    current_ones_sum(i) := in_previous_ones_sum + current_uop_ones_sum(i)
  }

  when(vcompress && fire) {
    when(uopIdx === 1.U) {
      vd_reg := Cat(0.U((VLEN - 8).W), out_previous_ones_sum)
    }.otherwise {
      vd_reg := Cat(current_ones_sum.reverse)
    }
  }.elsewhen(vmvnr && fire) {
    vd_reg := vs2
  }.elsewhen(vslideup && fire) {
    vd_reg := Cat(vslideup_vd.reverse)
  }.elsewhen(vslide1up && fire) {
    vd_reg := Cat(vslide1up_vd.reverse)
  }.elsewhen(vslidedn && fire) {
    vd_reg := Cat(vslidedn_vd.reverse)
  }.elsewhen(vslide1dn && fire) {
    vd_reg := Cat(vslide1dn_vd.reverse)
  }.elsewhen((vrgather || vrgather_vx) && !(vrgather16_sew8 && ((vlmul === 0.U) || (vlmul === 1.U))) && fire) {
    vd_reg := Cat(vrgather_vd.reverse)
  }.elsewhen(vrgather16_sew8 && (vlmul === 0.U) || (vlmul === 1.U) && fire) {
    when(uopIdx(0)) {
      vd_reg := Cat(Cat(vrgather_vd.reverse)(VLEN - 1, VLEN / 2), old_vd(VLEN / 2 - 1, 0))
    }.otherwise {
      vd_reg := Cat(old_vd(VLEN - 1, VLEN / 2), Cat(vrgather_vd.reverse)(VLEN / 2 - 1, 0))
    }
  }

  val is_vcompress_reg = RegEnable(vcompress, false.B, fire)
  val is_vslideup_reg = RegEnable(vslideup, false.B, fire)
  val is_vslidedn_reg = RegEnable(vslidedn, false.B, fire)
  val is_vslide1up_reg = RegEnable(vslide1up, false.B, fire)
  val is_vslide1dn_reg = RegEnable(vslide1dn, false.B, fire)
  val is_vrgather_reg = RegEnable(vrgather, false.B, fire)
  val is_vrgather_vx_reg = RegEnable(vrgather_vx, false.B, fire)
  val is_vmvnr_reg = RegEnable(vmvnr, false.B, fire)
  val is_vslide_reg = RegEnable(vslide, false.B, fire)
  val uopIdx_reg = RegEnable(uopIdx, 0.U, fire)
  val load_rs1_reg = RegEnable(load_rs1, false.B, fire)
  val vlRemain_reg = RegEnable(vlRemain, 0.U, fire)
  val vsew_reg = RegEnable(vsew, 0.U, fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val vs2_reg = RegEnable(vs2, 0.U, fire)
  val vmask_byte_strb_reg = RegEnable(Cat(vmask_byte_strb.reverse), 0.U, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vstartRemain_reg = RegEnable(vstartRemain, 0.U, fire)
  val base_reg = RegEnable(base, 0.U, fire)
  val one_reg = RegEnable(in_previous_ones_sum, 0.U, fire)
  val vstart_reg = RegEnable(vstart, 0.U, fire)
  val vl_reg = RegEnable(Mux(vmvnr, evl, vl), 0.U, fire)

  val vlRemainBytes_reg = vlRemain_reg << vsew_reg
  val vstartRemainBytes_reg = vstartRemain_reg << vsew_reg
  val current_res_boundary = Mux(uopIdx_reg === 3.U, (2 * vlenb).U, vlenb.U)

  vslideup_vl := Mux(vslideup & (slide_ele > vl), Mux(slide_ele > VLEN.U, VLEN.U, slide_ele), vl)
  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := Mux(is_vmvnr_reg, vd_mask, vd_mask >> tail_bits)
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(is_vmvnr_reg, 0.U, Mux(ta_reg, tail_ones_vd, tail_old_vd))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))

  val vstart_bytes = Mux(vstartRemainBytes_reg >= vlenb.U, vlenb.U, vstartRemainBytes_reg)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  val cmprs_vd = Wire(Vec(vlenb, UInt(8.W)))
  for (i <- 0 until vlenb) {
    cmprs_vd(i) := Mux(ta_reg && ((i.U >= one_reg) && (uopIdx_reg =/= 3.U) || (uopIdx_reg === 3.U)), "hff".U, old_vd_reg(i * 8 + 7, i * 8))
  }

  for (i <- 0 until vlenb) {
    res_idx(i) := vd_reg(8 * (i + 1) - 1, 8 * i) - base_reg - 1.U
    res_valid(i) := vd_reg(8 * (i + 1) - 1, 8 * i) >= base_reg + 1.U
    when((vmask_byte_strb_reg(i) === 1.U) && res_valid(i) && (res_idx(i) < current_res_boundary)) {
      cmprs_vd(res_idx(i)) := vs2_reg(i * 8 + 7, i * 8)
    }
  }

  perm_tail_mask_vd := vd_reg
  when(is_vslide_reg || is_vrgather_reg || is_vrgather_vx_reg) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  val perm_vd = Wire(UInt(VLEN.W))

  perm_vd := perm_tail_mask_vd
  when(vstart_reg >= vl_reg) {
    perm_vd := old_vd_reg
  }.elsewhen(is_vcompress_reg) {
    when(uopIdx_reg === 1.U) {
      perm_vd := vd_reg
    }.otherwise {
      perm_vd := Cat(cmprs_vd.reverse)
    }
  }

  io.out.vd := perm_vd
  io.out.vxsat := false.B
}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "build/vifu"))
}


