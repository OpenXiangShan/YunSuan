package yunsuan.vector.permfsm

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._
import yunsuan.vector.permfsm.VPermFsmOpcode._

class PermFsm extends Module {
  val VLEN = 128
  val vlenb = VLEN / 8
  val io = IO(new Bundle {
    val in = Input(new VPermFsmInput)
    val out = Output(new VPermFsmOutput)
  })

  val viq0_opcode = io.in.viq0_opcode
  val viq0_sew = io.in.viq0_sew
  val viq0_vs1 = io.in.viq0_vs1
  val viq0_old_vd = io.in.viq0_old_vd
  val viq0_mask = io.in.viq0_mask
  val viq0_vs1_preg_idx = io.in.viq0_vs1_preg_idx
  val viq0_vs2_preg_idx = io.in.viq0_vs2_preg_idx
  val viq0_old_vd_preg_idx = io.in.viq0_old_vd_preg_idx
  val viq0_vd_preg_idx = io.in.viq0_vd_preg_idx
  val viq0_uop_idx = io.in.viq0_uop_idx
  val viq0_vm = io.in.viq0_vm
  val viq0_ta = io.in.viq0_ta
  val viq0_ma = io.in.viq0_ma
  val viq0_vstart = io.in.viq0_vstart
  val viq0_vl = io.in.viq0_vl
  val viq0_lmul_4 = io.in.viq0_lmul_4
  val viq0_uop_vld = io.in.viq0_uop_vld
  val viq0_uop_flush_vld = io.in.viq0_uop_flush_vld
  val viq0_rob_idx = io.in.viq0_rob_idx
  val viq1_opcode = io.in.viq1_opcode
  val viq1_sew = io.in.viq1_sew
  val viq1_vs1 = io.in.viq1_vs1
  val viq1_old_vd = io.in.viq1_old_vd
  val viq1_mask = io.in.viq1_mask
  val viq1_vs1_preg_idx = io.in.viq1_vs1_preg_idx
  val viq1_vs2_preg_idx = io.in.viq1_vs2_preg_idx
  val viq1_old_vd_preg_idx = io.in.viq1_old_vd_preg_idx
  val viq1_vd_preg_idx = io.in.viq1_vd_preg_idx
  val viq1_uop_idx = io.in.viq1_uop_idx
  val viq1_vm = io.in.viq1_vm
  val viq1_ta = io.in.viq1_ta
  val viq1_ma = io.in.viq1_ma
  val viq1_vstart = io.in.viq1_vstart
  val viq1_vl = io.in.viq1_vl
  val viq1_lmul_4 = io.in.viq1_lmul_4
  val viq1_uop_vld = io.in.viq1_uop_vld
  val viq1_uop_flush_vld = io.in.viq1_uop_flush_vld
  val viq1_rob_idx = io.in.viq1_rob_idx
  val block_fsm_rd_vld = io.in.block_fsm_rd_vld
  val vs1 = io.in.fsm_rd_data_0
  val vs2_lo = io.in.fsm_rd_data_1
  val vs2_hi = io.in.fsm_rd_data_2
  val br_flush_vld = io.in.br_flush_vld
  val br_flush_rob_idx = io.in.br_flush_rob_idx
  val rob_flush_vld = io.in.rob_flush_vld
  val rob_commit_vld = io.in.rob_commit_vld
  val rob_commit_rob_idx = io.in.rob_commit_rob_idx

  val vslideup = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVslideup) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVslideup)
  val vslidedn = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVslidedn) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVslidedn)
  val vrgather_vv = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather)
  val vrgather_vx = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather_vx) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather_vx)
  val vrgather16 = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16)
  val vcompress = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVcompress) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVcompress)
  val vrgather16_sew8 = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 0.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 0.U))
  val vrgather16_sew32 = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 2.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 2.U))
  val vrgather16_sew64 = (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 3.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 3.U))
  val vslide = vslideup || vslidedn
  val vrgather = vrgather_vv || vrgather_vx || vrgather16

  val uop_valid = (viq0_uop_vld && !viq0_uop_flush_vld) || (viq1_uop_vld && !viq1_uop_flush_vld)
  val viq01_valid = (viq0_uop_vld && !viq0_uop_flush_vld) && (viq1_uop_vld && !viq1_uop_flush_vld)
  val viq0_valid = viq0_uop_vld && !viq0_uop_flush_vld
  val viq1_valid = viq1_uop_vld && !viq1_uop_flush_vld
  val vlmul = Mux(((viq0_uop_vld && !viq0_uop_flush_vld && !viq0_lmul_4) || (viq1_uop_vld && !viq1_uop_flush_vld && !viq1_lmul_4) || vrgather16_sew8), 7.U, 3.U) //todo
  val uop_cnt = RegInit(0.U(3.W))
  val rec_done = Mux(viq01_valid, (uop_cnt === vlmul - 1.U), (uop_cnt === vlmul)) && uop_valid
  val vs_idx = RegInit(0.U(3.W))
  val rd_sent_idx = RegInit(0.U(3.W))

  val opcode_reg = RegInit(dummy.asTypeOf(new VPermFsmOpcode))
  val vsew_reg = RegInit(0.U(2.W))
  val vm_reg = RegInit(false.B)
  val ta_reg = RegInit(false.B)
  val ma_reg = RegInit(false.B)
  val vstart_reg = RegInit(0.U(7.W))
  val vl_reg = RegInit(0.U(8.W))
  val vlmul_reg = RegInit(0.U(3.W))
  val rob_idx_reg = RegInit(0.U(3.W))
  val lmul_4_reg = RegInit(false.B)
  val fsm_busy = RegInit(false.B)
  val vd_idx = RegInit(0.U(3.W))
  val update_table_cnt = RegInit(0.U(2.W))
  val update_table_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val vrgather_table_sent_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val vrgather_wb_vld_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val rd_sent_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val rd_wb_resent_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val rd_wb_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val cmprs_wb_vld_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val cmprs_rd_vd_sent_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val table_lo_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(4.W))))
  val table_hi_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(4.W))))
  val rd_sent_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(3.W))))
  val calc_done = RegInit(false.B)
  val access_table = RegInit(VecInit(Seq.fill(8)(VecInit(Seq.fill(8)(0.U)))))
  val (table_lo_idx, table_lo) = sof(Cat(access_table(vs_idx).reverse))
  val (table_hi_idx, table_hi) = sof(~table_lo & Cat(access_table(vs_idx).reverse))
  val table_hi_lo = Wire(Vec(8, UInt(1.W)))
  val fsm_rd_vld_0 = RegInit(false.B)
  val fsm_rd_vld_1 = RegInit(false.B)
  val fsm_rd_vld_2 = RegInit(false.B)
  val fsm_rd_vld_3 = RegInit(false.B)
  val fsm_rd_preg_idx_0 = RegInit(0.U(8.W))
  val fsm_rd_preg_idx_1 = RegInit(0.U(8.W))
  val fsm_rd_preg_idx_2 = RegInit(0.U(8.W))
  val fsm_rd_preg_idx_3 = RegInit(0.U(8.W))
  val vs1_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vs2_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val old_vd_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vd_preg_idx = RegInit(VecInit(Seq.fill(8)(0.U(8.W))))
  val vmask_reg = RegInit(0.U(128.W))
  val old_vd = RegInit(0.U(128.W))

  val vs2_lo_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2_lo((i + 1) * 8 - 1, i * 8)))
  val vs2_hi_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2_hi((i + 1) * 8 - 1, i * 8)))
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(64.W)))
  val first_gather = update_table_cnt === 0.U
  val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslideup_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslidedn_vd = Wire(Vec(vlenb, UInt(8.W)))
  val cmprs_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vmask_byte_strb = Wire(Vec(vlenb, UInt(1.W)))
  val vmask_byte_strb_old_vd = Wire(Vec(vlenb, UInt(1.W)))
  val vmask_byte_strb_lmul = Wire(Vec(VLEN, UInt(1.W)))
  val vlRemain = RegInit(0.U(8.W))
  val vlRemainBytes = vlRemain << vsew_reg
  val vd_mask = (~0.U(VLEN.W))
  val sew_shift = Wire(UInt(3.W))
  val cmprs_rd_vlRemain = RegInit(0.U(8.W))
  val cmprs_rd_vlRemainBytes = cmprs_rd_vlRemain << vsew_reg
  val vl_reg_bytes = vl_reg << vsew_reg
  val update_vl_cnt = RegInit(0.U(3.W))

  val vlRemain_reg = RegNext(vlRemain)
  val vlRemainBytes_reg = vlRemain_reg << vsew_reg
  val old_vd_reg = RegNext(old_vd)
  val update_vl_cnt_reg = RegNext(update_vl_cnt) //todo
  val vmask_byte_strb_reg = RegNext(Cat(vmask_byte_strb.reverse))
  val vs2_lo_reg = RegNext(vs2_lo)

  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := vd_mask >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val perm_vd = Wire(UInt(VLEN.W))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))

  val idle :: rec_idx :: rd_vs :: calc_vd :: Nil = Enum(4)
  val perm_state = RegInit(idle)

  val reg_vslideup = opcode_reg.isVslideup
  val reg_vslidedn = opcode_reg.isVslidedn
  val reg_vrgather_vv = opcode_reg.isVrgather
  val reg_vrgather_vx = opcode_reg.isVrgather_vx
  val reg_vrgather16 = opcode_reg.isVrgather16
  val reg_vcompress = opcode_reg.isVcompress
  val reg_vrgather16_sew8 = opcode_reg.isVrgather16 && (vsew_reg === 0.U)
  val reg_vrgather16_sew32 = opcode_reg.isVrgather16 && (vsew_reg === 2.U)
  val reg_vrgather16_sew64 = opcode_reg.isVrgather16 && (vsew_reg === 3.U)
  val reg_vslide = reg_vslideup || reg_vslidedn
  val reg_vrgather = reg_vrgather_vv || reg_vrgather_vx || reg_vrgather16
  val vs1_type = Mux(reg_vrgather16, 1.U, vsew_reg)
  val one_sum = RegInit(0.U(8.W))
  val cmprs_update_vd_idx = Wire(Bool())
  val res_idx = Wire(Vec(vlenb, UInt(8.W)))
  val res_valid = Wire(Vec(vlenb, Bool()))
  val current_ones_sum = Wire(Vec(vlenb, UInt(8.W)))

  val update_table = reg_vrgather && (perm_state === rd_vs) & !block_fsm_rd_vld
  val base = Wire(UInt(7.W))
  val eewVs2 = SewOH(vsew_reg)
  val current_vs_ones_sum = PopCount(Cat(vmask_byte_strb.reverse))
  val current_rd_vs_ones_sum = PopCount(Cat(vmask_byte_strb_old_vd.reverse))

  val total_one_sum = RegInit(0.U(8.W))

  when(br_flush_vld) {
    total_one_sum := 0.U
  }.elsewhen(rec_done) {
    total_one_sum := PopCount(Cat(vmask_byte_strb_lmul.reverse))
  }

  val update_vs_idx = Wire(Bool())
  val update_vl = Mux(reg_vcompress, cmprs_wb_vld_reg(3), Mux(reg_vrgather, vrgather_wb_vld_reg(3), rd_sent_reg(3)))
  val update_vd_idx = Mux(reg_vcompress, cmprs_update_vd_idx, Mux(reg_vrgather, vrgather_wb_vld_reg(4), rd_sent_reg(4)))
  val vslide_ele = Cat(vs1_preg_idx.reverse)
  val vslide_bytes = vslide_ele << vsew_reg
  val src_lo_valid = Mux(reg_vslideup, vslide_bytes(65, 4) + 1.U <= vs_idx, vs_idx + vslide_bytes(65, 4) <= vlmul_reg)
  val src_hi_valid = Mux(reg_vslideup, vslide_bytes(65, 4) <= vs_idx, vs_idx + vslide_bytes(65, 4) + 1.U <= vlmul_reg)
  val rd_sent_src_lo_valid = Mux(reg_vslideup, vslide_bytes(65, 4) + 1.U <= rd_sent_idx, rd_sent_idx + vslide_bytes(65, 4) <= vlmul_reg)
  val rd_sent_src_hi_valid = Mux(reg_vslideup, vslide_bytes(65, 4) <= rd_sent_idx, rd_sent_idx + vslide_bytes(65, 4) + 1.U <= vlmul_reg)
  val src_lo_valid_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val src_hi_valid_reg = RegInit(VecInit(Seq.fill(5)(false.B)))

  val rd_sent = (fsm_rd_vld_0 || fsm_rd_vld_1 || fsm_rd_vld_2 || fsm_rd_vld_3) && !block_fsm_rd_vld
  val wb_vld = Wire(Bool())
  val wb_idx = RegInit(0.U(3.W))
  val vrgather_rd_en = RegInit(false.B)
  val vrgather_wb_en = RegInit(false.B)
  val vrgather_table_sent = vrgather_rd_en && rd_sent
  val vrgather_wb_vld = vrgather_wb_en & rd_sent

  val rd_wb = Wire(Bool())
  val rd_vd_idx = RegInit(0.U(4.W))
  val cmprs_rd_vd = RegInit(false.B)
  val cmprs_rd_vd_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val rd_done = (wb_idx === vlmul_reg) && wb_vld
  val old_vd_idx = Mux(reg_vcompress, rd_vd_idx, Mux(vs_idx === vlmul_reg, vs_idx, vs_idx + 1.U)) //todo
  val rd_one_sum = RegInit(0.U(8.W))
  val cmprs_rd_vd_wb_en = RegInit(false.B)
  val cmprs_rd_vd_sent = cmprs_rd_vd_wb_en && rd_sent
  val cmprs_wb_vld = rd_wb || (cmprs_rd_vd_wb_en && rd_sent) // compress vs2 into vd or read old vd for vd
  val rd_wb_resent = RegInit(false.B)

  calc_done := false.B
  when(br_flush_vld) {
    calc_done := false.B
  }.elsewhen((vd_idx === vlmul_reg) && update_vd_idx) {
    calc_done := true.B
  }

  when(br_flush_vld) {
    rd_wb_resent := false.B
  }.elsewhen(rd_wb) {
    rd_wb_resent := true.B
  }.elsewhen(rd_sent) {
    rd_wb_resent := false.B
  }

  when(br_flush_vld) {
    cmprs_rd_vd_wb_en := false.B
  }.elsewhen(rd_done) {
    cmprs_rd_vd_wb_en := false.B
  }.elsewhen(reg_vcompress && cmprs_rd_vd && rd_sent && !rd_wb) {
    cmprs_rd_vd_wb_en := true.B
  }

  when(br_flush_vld) {
    rd_one_sum := 0.U
  }.elsewhen(rd_done) {
    rd_one_sum := 0.U
  }.elsewhen(reg_vcompress && (perm_state === rd_vs) && rd_sent && !rd_wb_resent && !cmprs_rd_vd) { // current_rd_vs_ones_sum updated with rd_sent
    when((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) {
      rd_one_sum := rd_one_sum + current_rd_vs_ones_sum - vlenb.U
    }.otherwise {
      rd_one_sum := rd_one_sum + current_rd_vs_ones_sum
    }
  }.elsewhen(rd_one_sum >= vlenb.U) {
    rd_one_sum := rd_one_sum - vlenb.U
  }

  rd_wb := false.B
  when(reg_vcompress && ((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) && rd_sent && !rd_wb_resent) {
    rd_wb := true.B
  }

  update_vs_idx := false.B
  when(reg_vrgather && !Cat(table_hi_lo.reverse).orR && (perm_state === rd_vs) && !block_fsm_rd_vld && !rd_done) {
    update_vs_idx := true.B
  }.elsewhen(reg_vcompress && (perm_state === rd_vs) && !cmprs_rd_vd && !rd_wb && !block_fsm_rd_vld) { // if vcompress into vd, then read this vs_idx again
    update_vs_idx := true.B
  }.elsewhen(reg_vslide && (perm_state === rd_vs) && !block_fsm_rd_vld) {
    update_vs_idx := true.B
  }

  when(br_flush_vld) {
    vrgather_rd_en := false.B
  }.elsewhen(update_table) {
    vrgather_rd_en := true.B
  }.elsewhen(rd_sent) {
    vrgather_rd_en := false.B
  }

  when(br_flush_vld) {
    vrgather_wb_en := false.B
  }.elsewhen(update_vs_idx) {
    vrgather_wb_en := true.B
  }.elsewhen(rd_sent) {
    vrgather_wb_en := false.B
  }

  rd_vd_idx := total_one_sum(7, 4)
  when(br_flush_vld) {
    rd_vd_idx := 0.U
  }.elsewhen(block_fsm_rd_vld) {
    rd_vd_idx := rd_vd_idx
  }.elsewhen((rd_vd_idx === (Cat(0.U(1.W), vlmul_reg) + 1.U)) && cmprs_rd_vd && !block_fsm_rd_vld) {
    rd_vd_idx := 0.U
  }.elsewhen(cmprs_rd_vd && !block_fsm_rd_vld) {
    rd_vd_idx := rd_vd_idx + 1.U
  }

  // read remain old_vd after all vs2 compress into vd
  when(br_flush_vld) {
    cmprs_rd_vd := false.B
  }.elsewhen((rd_vd_idx === (Cat(0.U(1.W), vlmul_reg) + 1.U)) && !block_fsm_rd_vld) {
    cmprs_rd_vd := false.B
  }.elsewhen(reg_vcompress && (vs_idx === vlmul_reg) && update_vs_idx) {
    when(rd_vd_idx =/= (Cat(0.U(1.W), vlmul_reg) + 1.U)) {
      cmprs_rd_vd := true.B
    }
  }

  when(br_flush_vld) {
    perm_state := idle
  }.otherwise {
    switch(perm_state) {
      is(idle) {
        when(uop_valid) {
          perm_state := rec_idx
        }
      }

      is(rec_idx) {
        when(rec_done) {
          perm_state := rd_vs
        }
      }

      is(rd_vs) {
        when(rd_done) {
          perm_state := calc_vd
        }
      }

      is(calc_vd) {
        when(calc_done) {
          perm_state := idle
        }
      }
    }
  }

  when(br_flush_vld) {
    fsm_busy := false.B
  }.elsewhen(uop_valid) {
    fsm_busy := true.B
  }.elsewhen(calc_done) {
    fsm_busy := false.B
  }

  def access_table_gen(vs1: UInt, vsew: UInt, uop_idx: UInt, vrgather_vx: Bool, vrgather16: Bool): Seq[UInt] = {
    val sew = SewOH(Mux(vrgather16, 1.U(2.W), vsew))
    val shift = Wire(UInt(3.W))
    val table_idx = Wire(Vec(vlenb, UInt(64.W)))
    val access_table = Wire(Vec(8, UInt(1.W)))
    val vrgather16_sew8 = vrgather16 && (vsew === 0.U)
    val vrgather16_sew32 = vrgather16 && (vsew === 2.U)
    val vrgather16_sew64 = vrgather16 && (vsew === 3.U)
    access_table := VecInit(Seq.fill(8)(0.U))
    shift := Cat(0.U(1.W), ~vsew) + 1.U(3.W)

    for (i <- 0 until vlenb) {
      table_idx(i) := 0.U
    }

    when(vrgather16_sew32) {
      when(uop_idx(0)) {
        for (i <- vlenb / 2 until vlenb) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.otherwise {
        for (i <- 0 until vlenb / 2) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }
    }.elsewhen(vrgather16_sew64) {
      when(uop_idx(1, 0) === 0.U) {
        for (i <- 0 until vlenb / 4) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.elsewhen(uop_idx(1, 0) === 1.U) {
        for (i <- vlenb / 4 until vlenb / 2) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.elsewhen(uop_idx(1, 0) === 2.U) {
        for (i <- vlenb / 2 until 3 * vlenb / 4) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.otherwise {
        for (i <- 3 * vlenb / 4 until vlenb) {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }
    }.otherwise {
      for (i <- 0 until vlenb) {
        when(vrgather_vx) {
          table_idx(i) := vs1(63, 0) >> shift
        }.otherwise {
          table_idx(i) := Mux1H(sew.oneHot, Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))) >> shift
        }
        when(table_idx(i) <= Mux(vrgather16_sew8, 3.U, vlmul)) {
          access_table(table_idx(i)) := 1.U
        }
      }
    }
    access_table
  }

  def sof(table: UInt): (UInt, UInt) = {
    val flag = Wire(Vec(8, UInt(1.W)))
    val sbf = Wire(Vec(8, UInt(1.W)))
    val index = Wire(Vec(8, UInt(4.W)))
    val sif = Cat(Cat(sbf.reverse)(6, 0), 1.U)
    val sof = ~Cat(sbf.reverse) & sif

    for (i <- 0 until 8) {
      if (i == 0) {
        flag(i) := table(i)
        sbf(i) := Mux(table(0).asBool, 0.U, 1.U)
      } else {
        flag(i) := table(i) | flag(i - 1)
        sbf(i) := Mux(flag(i).asBool, 0.U, 1.U)
      }
    }

    for (i <- 0 until 8 reverse) {
      if (i == 7)
        index(i) := Mux(table(i).asBool, i.U, "hF".U)
      else
        index(i) := Mux(table(i).asBool, i.U, index(i + 1))
    }

    (index(0), sof)
  }

  when((!block_fsm_rd_vld && !rd_wb) || br_flush_vld) {
    fsm_rd_vld_0 := false.B
    when((reg_vrgather_vv || reg_vrgather16) && (perm_state === rd_vs) && !rd_done) {
      fsm_rd_vld_0 := true.B
      when(reg_vrgather16_sew32) {
        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2, 1))
      }.elsewhen(reg_vrgather16_sew64) {
        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2))
      }.otherwise {
        fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx)
      }
    }
  }

  val rd_idx_1 = Wire(UInt(3.W))
  val rd_idx_2 = Wire(UInt(3.W))
  val reg_table_lo_idx = RegInit(0.U(4.W))
  val reg_table_hi_idx = RegInit(0.U(4.W))

  when(update_table) {
    reg_table_lo_idx := table_lo_idx
    reg_table_hi_idx := table_hi_idx
  }

  rd_idx_1 := 0.U
  rd_idx_2 := 0.U
  when(reg_vrgather) {
    rd_idx_1 := Mux(reg_vrgather16_sew8, Cat(table_lo_idx(1, 0), 0.U(1.W)), table_lo_idx)
    rd_idx_2 := Mux(reg_vrgather16_sew8, Cat(table_hi_idx(1, 0), 0.U(1.W)), table_hi_idx)
  }.elsewhen(reg_vslideup) {
    rd_idx_1 := vs_idx - vslide_bytes(6, 4) - 1.U
    rd_idx_2 := vs_idx - vslide_bytes(6, 4)
  }.elsewhen(reg_vslidedn) {
    rd_idx_1 := vs_idx + vslide_bytes(6, 4)
    rd_idx_2 := vs_idx + vslide_bytes(6, 4) + 1.U
  }.elsewhen(reg_vcompress) {
    rd_idx_1 := vs_idx
  }

  when((!block_fsm_rd_vld && !rd_wb) || br_flush_vld) {
    fsm_rd_vld_1 := false.B
    when(((reg_vrgather && table_lo.orR) || (reg_vcompress && !cmprs_rd_vd) || (reg_vslide && src_lo_valid)) && (perm_state === rd_vs) && !rd_done) {
      fsm_rd_vld_1 := true.B
      fsm_rd_preg_idx_1 := vs2_preg_idx(rd_idx_1)
    }
  }

  when((!block_fsm_rd_vld && !rd_wb) || br_flush_vld) {
    fsm_rd_vld_2 := false.B
    when(((reg_vrgather && table_hi.orR) || (reg_vslide && src_hi_valid)) && (perm_state === rd_vs) && !rd_done) {
      fsm_rd_vld_2 := true.B
      fsm_rd_preg_idx_2 := vs2_preg_idx(rd_idx_2)
    }
  }

  when((!block_fsm_rd_vld && !rd_wb) || br_flush_vld) {
    fsm_rd_vld_3 := false.B
    when(((!reg_vcompress && (perm_state === rd_vs)) || (reg_vcompress && cmprs_rd_vd)) && !rd_done) {
      fsm_rd_vld_3 := true.B
      fsm_rd_preg_idx_3 := old_vd_preg_idx(old_vd_idx)
    }
  }

  when(viq01_valid) {
    vs1_preg_idx(viq0_uop_idx) := viq0_vs1_preg_idx
    vs2_preg_idx(viq0_uop_idx) := viq0_vs2_preg_idx
    old_vd_preg_idx(viq0_uop_idx) := viq0_old_vd_preg_idx
    vd_preg_idx(viq0_uop_idx) := viq0_vd_preg_idx
    vs1_preg_idx(viq1_uop_idx) := viq1_vs1_preg_idx
    vs2_preg_idx(viq1_uop_idx) := viq1_vs2_preg_idx
    old_vd_preg_idx(viq1_uop_idx) := viq1_old_vd_preg_idx
    vd_preg_idx(viq1_uop_idx) := viq1_vd_preg_idx
    when(vrgather_vx || vslide) {
      for (i <- 0 until 8) {
        vs1_preg_idx(i) := viq0_vs1((i + 1) * 8 - 1, i * 8)
      }
    }
  }.elsewhen(viq0_valid) {
    vs1_preg_idx(viq0_uop_idx) := viq0_vs1_preg_idx
    vs2_preg_idx(viq0_uop_idx) := viq0_vs2_preg_idx
    old_vd_preg_idx(viq0_uop_idx) := viq0_old_vd_preg_idx
    vd_preg_idx(viq0_uop_idx) := viq0_vd_preg_idx
    when(vrgather_vx || vslide) {
      for (i <- 0 until 8) {
        vs1_preg_idx(i) := viq0_vs1((i + 1) * 8 - 1, i * 8)
      }
    }
  }.elsewhen(viq1_valid) {
    vs1_preg_idx(viq1_uop_idx) := viq1_vs1_preg_idx
    vs2_preg_idx(viq1_uop_idx) := viq1_vs2_preg_idx
    old_vd_preg_idx(viq1_uop_idx) := viq1_old_vd_preg_idx
    vd_preg_idx(viq1_uop_idx) := viq1_vd_preg_idx
    when(vrgather_vx || vslide) {
      for (i <- 0 until 8) {
        vs1_preg_idx(i) := viq1_vs1((i + 1) * 8 - 1, i * 8)
      }
    }
  }

  when(br_flush_vld) {
    for (i <- 0 until 8) {
      for (j <- 0 until 8) {
        access_table(i)(j) := 0.U
      }
    }
  }.otherwise {
    when(viq01_valid) {
      access_table(viq0_uop_idx) := access_table_gen(viq0_vs1, viq0_sew, viq0_uop_idx, vrgather_vx, vrgather16)
      access_table(viq1_uop_idx) := access_table_gen(viq1_vs1, viq1_sew, viq1_uop_idx, vrgather_vx, vrgather16)
    }.elsewhen(viq0_valid) {
      access_table(viq0_uop_idx) := access_table_gen(viq0_vs1, viq0_sew, viq0_uop_idx, vrgather_vx, vrgather16)
    }.elsewhen(viq1_valid) {
      access_table(viq1_uop_idx) := access_table_gen(viq1_vs1, viq1_sew, viq1_uop_idx, vrgather_vx, vrgather16)
    }.elsewhen(update_table) {
      access_table(vs_idx) := table_hi_lo
    }
  }

  for (i <- 0 until 8) {
    table_hi_lo(i) := ~table_lo(i) & ~table_hi(i) & access_table(vs_idx)(i)
  }

  when(br_flush_vld) {
    vs_idx := 0.U
  }.elsewhen(rd_done) {
    vs_idx := 0.U
  }.elsewhen(update_vs_idx) {
    when(vs_idx === vlmul_reg) {
      vs_idx := vs_idx
    }.otherwise {
      vs_idx := vs_idx + 1.U
    }
  }

  when(br_flush_vld) {
    opcode_reg.op := dummy
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    opcode_reg := viq0_opcode
    vsew_reg := viq0_sew
    vm_reg := viq0_vm
    ta_reg := viq0_ta
    ma_reg := viq0_ma
    vstart_reg := viq0_vstart
    vl_reg := viq0_vl
    vlmul_reg := vlmul
    rob_idx_reg := viq0_rob_idx
    lmul_4_reg := viq0_lmul_4
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    opcode_reg := viq1_opcode
    vsew_reg := viq1_sew
    vm_reg := viq1_vm
    ta_reg := viq1_ta
    ma_reg := viq1_ma
    vstart_reg := viq1_vstart
    vl_reg := viq1_vl
    vlmul_reg := vlmul
    rob_idx_reg := viq1_rob_idx
    lmul_4_reg := viq1_lmul_4
  }

  when(viq0_valid && (viq0_uop_idx === 0.U)) {
    old_vd := Mux(vcompress, viq0_vs1, viq0_old_vd)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    old_vd := Mux(vcompress, viq1_vs1, viq1_old_vd)
  }.elsewhen(reg_vslide && rd_sent_reg(3)) {
    old_vd := io.in.fsm_rd_data_3
  }.elsewhen(reg_vrgather && vrgather_wb_vld_reg(3)) { //todo
    old_vd := io.in.fsm_rd_data_3
  }.elsewhen(reg_vcompress && rd_sent && !rd_wb) {
    old_vd := old_vd >> (1.U << sew_shift)
  }

  when(br_flush_vld) {
    uop_cnt := 0.U
  }.elsewhen(rec_done) {
    uop_cnt := 0.U
  }.elsewhen(viq01_valid) {
    uop_cnt := uop_cnt + 2.U
  }.elsewhen(viq0_valid) {
    uop_cnt := uop_cnt + 1.U
  }.elsewhen(viq1_valid) {
    uop_cnt := uop_cnt + 1.U
  }

  when(br_flush_vld) {
    rd_sent_idx_reg(4) := 0.U
    rd_sent_idx_reg(3) := 0.U
    rd_sent_idx_reg(2) := 0.U
    rd_sent_idx_reg(1) := 0.U
    rd_sent_idx_reg(0) := 0.U

    table_lo_idx_reg(4) := 0.U
    table_lo_idx_reg(3) := 0.U
    table_lo_idx_reg(2) := 0.U
    table_lo_idx_reg(1) := 0.U
    table_lo_idx_reg(0) := 0.U

    table_hi_idx_reg(4) := 0.U
    table_hi_idx_reg(3) := 0.U
    table_hi_idx_reg(2) := 0.U
    table_hi_idx_reg(1) := 0.U
    table_hi_idx_reg(0) := 0.U

    update_table_reg(4) := false.B
    update_table_reg(3) := false.B
    update_table_reg(2) := false.B
    update_table_reg(1) := false.B
    update_table_reg(0) := false.B

    vrgather_table_sent_reg(4) := false.B
    vrgather_table_sent_reg(3) := false.B
    vrgather_table_sent_reg(2) := false.B
    vrgather_table_sent_reg(1) := false.B
    vrgather_table_sent_reg(0) := false.B

    vrgather_wb_vld_reg(4) := false.B
    vrgather_wb_vld_reg(3) := false.B
    vrgather_wb_vld_reg(2) := false.B
    vrgather_wb_vld_reg(1) := false.B
    vrgather_wb_vld_reg(0) := false.B

    rd_sent_reg(4) := false.B
    rd_sent_reg(3) := false.B
    rd_sent_reg(2) := false.B
    rd_sent_reg(1) := false.B
    rd_sent_reg(0) := false.B

    rd_wb_resent_reg(4) := false.B
    rd_wb_resent_reg(3) := false.B
    rd_wb_resent_reg(2) := false.B
    rd_wb_resent_reg(1) := false.B
    rd_wb_resent_reg(0) := false.B

    rd_wb_reg(4) := false.B
    rd_wb_reg(3) := false.B
    rd_wb_reg(2) := false.B
    rd_wb_reg(1) := false.B
    rd_wb_reg(0) := false.B

    cmprs_wb_vld_reg(4) := false.B
    cmprs_wb_vld_reg(3) := false.B
    cmprs_wb_vld_reg(2) := false.B
    cmprs_wb_vld_reg(1) := false.B
    cmprs_wb_vld_reg(0) := false.B

    cmprs_rd_vd_sent_reg(4) := false.B
    cmprs_rd_vd_sent_reg(3) := false.B
    cmprs_rd_vd_sent_reg(2) := false.B
    cmprs_rd_vd_sent_reg(1) := false.B
    cmprs_rd_vd_sent_reg(0) := false.B

    src_lo_valid_reg(4) := false.B
    src_lo_valid_reg(3) := false.B
    src_lo_valid_reg(2) := false.B
    src_lo_valid_reg(1) := false.B
    src_lo_valid_reg(0) := false.B

    src_hi_valid_reg(4) := false.B
    src_hi_valid_reg(3) := false.B
    src_hi_valid_reg(2) := false.B
    src_hi_valid_reg(1) := false.B
    src_hi_valid_reg(0) := false.B

    cmprs_rd_vd_reg(4) := false.B
    cmprs_rd_vd_reg(3) := false.B
    cmprs_rd_vd_reg(2) := false.B
    cmprs_rd_vd_reg(1) := false.B
    cmprs_rd_vd_reg(0) := false.B
  }.otherwise {
    rd_sent_idx_reg(4) := rd_sent_idx_reg(3)
    rd_sent_idx_reg(3) := rd_sent_idx_reg(2)
    rd_sent_idx_reg(2) := rd_sent_idx_reg(1)
    rd_sent_idx_reg(1) := rd_sent_idx_reg(0)
    rd_sent_idx_reg(0) := rd_sent_idx

    table_lo_idx_reg(4) := table_lo_idx_reg(3)
    table_lo_idx_reg(3) := table_lo_idx_reg(2)
    table_lo_idx_reg(2) := table_lo_idx_reg(1)
    table_lo_idx_reg(1) := table_lo_idx_reg(0)
    table_lo_idx_reg(0) := reg_table_lo_idx

    table_hi_idx_reg(4) := table_hi_idx_reg(3)
    table_hi_idx_reg(3) := table_hi_idx_reg(2)
    table_hi_idx_reg(2) := table_hi_idx_reg(1)
    table_hi_idx_reg(1) := table_hi_idx_reg(0)
    table_hi_idx_reg(0) := reg_table_hi_idx

    update_table_reg(4) := update_table_reg(3)
    update_table_reg(3) := update_table_reg(2)
    update_table_reg(2) := update_table_reg(1)
    update_table_reg(1) := update_table_reg(0)
    update_table_reg(0) := update_table

    vrgather_table_sent_reg(4) := vrgather_table_sent_reg(3)
    vrgather_table_sent_reg(3) := vrgather_table_sent_reg(2)
    vrgather_table_sent_reg(2) := vrgather_table_sent_reg(1)
    vrgather_table_sent_reg(1) := vrgather_table_sent_reg(0)
    vrgather_table_sent_reg(0) := vrgather_table_sent

    vrgather_wb_vld_reg(4) := vrgather_wb_vld_reg(3)
    vrgather_wb_vld_reg(3) := vrgather_wb_vld_reg(2)
    vrgather_wb_vld_reg(2) := vrgather_wb_vld_reg(1)
    vrgather_wb_vld_reg(1) := vrgather_wb_vld_reg(0)
    vrgather_wb_vld_reg(0) := vrgather_wb_vld

    rd_sent_reg(4) := rd_sent_reg(3)
    rd_sent_reg(3) := rd_sent_reg(2)
    rd_sent_reg(2) := rd_sent_reg(1)
    rd_sent_reg(1) := rd_sent_reg(0)
    rd_sent_reg(0) := rd_sent

    rd_wb_resent_reg(4) := rd_wb_resent_reg(3)
    rd_wb_resent_reg(3) := rd_wb_resent_reg(2)
    rd_wb_resent_reg(2) := rd_wb_resent_reg(1)
    rd_wb_resent_reg(1) := rd_wb_resent_reg(0)
    rd_wb_resent_reg(0) := rd_wb_resent

    rd_wb_reg(4) := rd_wb_reg(3)
    rd_wb_reg(3) := rd_wb_reg(2)
    rd_wb_reg(2) := rd_wb_reg(1)
    rd_wb_reg(1) := rd_wb_reg(0)
    rd_wb_reg(0) := rd_wb

    cmprs_wb_vld_reg(4) := cmprs_wb_vld_reg(3)
    cmprs_wb_vld_reg(3) := cmprs_wb_vld_reg(2)
    cmprs_wb_vld_reg(2) := cmprs_wb_vld_reg(1)
    cmprs_wb_vld_reg(1) := cmprs_wb_vld_reg(0)
    cmprs_wb_vld_reg(0) := cmprs_wb_vld

    cmprs_rd_vd_sent_reg(4) := cmprs_rd_vd_sent_reg(3)
    cmprs_rd_vd_sent_reg(3) := cmprs_rd_vd_sent_reg(2)
    cmprs_rd_vd_sent_reg(2) := cmprs_rd_vd_sent_reg(1)
    cmprs_rd_vd_sent_reg(1) := cmprs_rd_vd_sent_reg(0)
    cmprs_rd_vd_sent_reg(0) := cmprs_rd_vd_sent

    src_lo_valid_reg(4) := src_lo_valid_reg(3)
    src_lo_valid_reg(3) := src_lo_valid_reg(2)
    src_lo_valid_reg(2) := src_lo_valid_reg(1)
    src_lo_valid_reg(1) := src_lo_valid_reg(0)
    src_lo_valid_reg(0) := rd_sent_src_lo_valid

    src_hi_valid_reg(4) := src_hi_valid_reg(3)
    src_hi_valid_reg(3) := src_hi_valid_reg(2)
    src_hi_valid_reg(2) := src_hi_valid_reg(1)
    src_hi_valid_reg(1) := src_hi_valid_reg(0)
    src_hi_valid_reg(0) := rd_sent_src_hi_valid

    cmprs_rd_vd_reg(4) := cmprs_rd_vd_reg(3)
    cmprs_rd_vd_reg(3) := cmprs_rd_vd_reg(2)
    cmprs_rd_vd_reg(2) := cmprs_rd_vd_reg(1)
    cmprs_rd_vd_reg(1) := cmprs_rd_vd_reg(0)
    cmprs_rd_vd_reg(0) := cmprs_rd_vd
  }

  val lo_min = Mux(table_lo_idx_reg(3) === "hf".U, "hff".U, Cat(table_lo_idx_reg(3), 0.U(4.W)))
  val lo_max = Mux(table_lo_idx_reg(3) === "hf".U, "hff".U, Cat((table_lo_idx_reg(3) + 1.U), 0.U(4.W)))
  val hi_min = Mux(table_hi_idx_reg(3) === "hf".U, "hff".U, Cat(table_hi_idx_reg(3), 0.U(4.W)))
  val hi_max = Mux(table_hi_idx_reg(3) === "hf".U, "hff".U, Cat((table_hi_idx_reg(3) + 1.U), 0.U(4.W)))

  when(br_flush_vld) {
    update_table_cnt := 0.U
  }.elsewhen(vrgather_table_sent_reg(3) && vrgather_wb_vld_reg(3)) {
    update_table_cnt := 0.U
  }.elsewhen(vrgather_table_sent_reg(3)) {
    update_table_cnt := update_table_cnt + 1.U
  }

  sew_shift := Cat(0.U(1.W), ~vsew_reg) + 1.U
  when(br_flush_vld) {
    vlRemain := 0.U
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    vlRemain := Mux(vslideup && (viq0_vs1 > viq0_vl), Mux(viq0_vs1 > VLEN.U, VLEN.U, viq0_vs1), viq0_vl)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vlRemain := Mux(vslideup && (viq1_vs1 > viq1_vl), Mux(viq1_vs1 > VLEN.U, VLEN.U, viq1_vs1), viq1_vl)
  }.elsewhen(reg_vcompress && rd_sent_reg(3) && !rd_wb_reg(3)) {
    vlRemain := Mux(vlRemain >= (1.U << sew_shift), vlRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(!reg_vrgather16_sew8 && !reg_vcompress && update_vl) {
    vlRemain := Mux(vlRemain >= (1.U << sew_shift), vlRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(reg_vrgather16_sew8 && update_vl && update_vl_cnt(0)) {
    vlRemain := Mux(vlRemain >= (1.U << sew_shift), vlRemain - (1.U << sew_shift), 0.U)
  }

  when(br_flush_vld) {
    cmprs_rd_vlRemain := 0.U
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    cmprs_rd_vlRemain := viq0_vl
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    cmprs_rd_vlRemain := viq1_vl
  }.elsewhen(reg_vcompress && rd_sent && !rd_wb_resent) {
    cmprs_rd_vlRemain := Mux(cmprs_rd_vlRemain >= (1.U << sew_shift), cmprs_rd_vlRemain - (1.U << sew_shift), 0.U)
  }

  when(viq0_valid && (viq0_uop_idx === 0.U)) {
    vmask_reg := Mux(vcompress, viq0_vs1, viq0_mask)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vmask_reg := Mux(vcompress, viq1_vs1, viq1_mask)
  }.elsewhen((reg_vcompress || reg_vslide) && rd_sent_reg(3) && !rd_wb_reg(3)) {
    vmask_reg := vmask_reg >> (1.U << sew_shift)
  }.elsewhen(reg_vrgather && update_vl) {
    when(reg_vrgather16_sew8) {
      when(update_vl_cnt(0)) {
        vmask_reg := vmask_reg >> (1.U << sew_shift)
      }
    }.otherwise {
      vmask_reg := vmask_reg >> (1.U << sew_shift)
    }
  }

  for (i <- 0 until vlenb) {
    when(i.U < vlRemainBytes) {
      vmask_byte_strb(i) := vmask_reg(i) | (vm_reg & !reg_vcompress)
      when(vsew_reg === 1.U(3.W)) {
        vmask_byte_strb(i) := vmask_reg(i / 2) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 2.U(3.W)) {
        vmask_byte_strb(i) := vmask_reg(i / 4) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 3.U(3.W)) {
        vmask_byte_strb(i) := vmask_reg(i / 8) | (vm_reg & !reg_vcompress)
      }
    }.otherwise {
      vmask_byte_strb(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    when(i.U < cmprs_rd_vlRemainBytes) {
      vmask_byte_strb_old_vd(i) := old_vd(i) | (vm_reg & !reg_vcompress)
      when(vsew_reg === 1.U(3.W)) {
        vmask_byte_strb_old_vd(i) := old_vd(i / 2) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 2.U(3.W)) {
        vmask_byte_strb_old_vd(i) := old_vd(i / 4) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 3.U(3.W)) {
        vmask_byte_strb_old_vd(i) := old_vd(i / 8) | (vm_reg & !reg_vcompress)
      }
    }.otherwise {
      vmask_byte_strb_old_vd(i) := 0.U
    }
  }

  for (i <- 0 until VLEN) {
    when(i.U < vl_reg_bytes) {
      vmask_byte_strb_lmul(i) := vmask_reg(i) | (vm_reg & !reg_vcompress)
      when(vsew_reg === 1.U(3.W)) {
        vmask_byte_strb_lmul(i) := vmask_reg(i / 2) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 2.U(3.W)) {
        vmask_byte_strb_lmul(i) := vmask_reg(i / 4) | (vm_reg & !reg_vcompress)
      }.elsewhen(vsew_reg === 3.U(3.W)) {
        vmask_byte_strb_lmul(i) := vmask_reg(i / 8) | (vm_reg & !reg_vcompress)
      }
    }.otherwise {
      vmask_byte_strb_lmul(i) := 0.U
    }
  }

  for (i <- 0 until vlenb) {
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    // vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    vslideup_vd(i) := old_vd(i * 8 + 7, i * 8)
    //vslidedn_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    vslidedn_vd(i) := old_vd(i * 8 + 7, i * 8)
  }

  for (i <- 0 until vlenb / 2) {
    vrgather_byte_sel(i) := 0.U
    when(reg_vrgather_vx) {
      vrgather_byte_sel(i) := Cat(vs1_preg_idx.reverse)
      when(vsew_reg === 1.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(vsew_reg === 2.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(vsew_reg === 3.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(vs1_type === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_type === 1.U) {
        when((vsew_reg === 0.U) && !update_vl_cnt(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(vsew_reg === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(vsew_reg === 2.U) {
          when(update_vl_cnt(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(vsew_reg === 3.U) {
          when(update_vl_cnt(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 3.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
          }
        }
      }.elsewhen(vs1_type === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(vs1_type === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  for (i <- (vlenb / 2) until vlenb) {
    vrgather_byte_sel(i) := 0.U
    when(reg_vrgather_vx) {
      vrgather_byte_sel(i) := Cat(vs1_preg_idx.reverse)
      when(vsew_reg === 1.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(vsew_reg === 2.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(vsew_reg === 3.U) {
        vrgather_byte_sel(i) := Cat(Cat(vs1_preg_idx.reverse), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(vs1_type === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_type === 1.U) {
        when((vsew_reg === 0.U) && update_vl_cnt(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
        }.elsewhen(vsew_reg === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(vsew_reg === 2.U) {
          when(update_vl_cnt(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(vsew_reg === 3.U) {
          when(update_vl_cnt(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(update_vl_cnt(1, 0) === 3.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
          }
        }
      }.elsewhen(vs1_type === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(vs1_type === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  when(reg_vrgather && !reg_vrgather16_sew8 && vrgather_table_sent_reg(3)) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := Mux(first_gather, old_vd((i + 1) * 8 - 1, i * 8), vd_reg((i + 1) * 8 - 1, i * 8))
      when(vmask_byte_strb(i).asBool) {
        when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
          vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
        }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
          vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
        }.elsewhen(first_gather) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
      }
    }
  }.elsewhen(reg_vrgather16_sew8 && vrgather_table_sent_reg(3) && !update_vl_cnt(0)) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := Mux(first_gather, old_vd((i + 1) * 8 - 1, i * 8), vd_reg((i + 1) * 8 - 1, i * 8))
    }

    for (i <- 0 until vlenb / 2) {
      when(vmask_byte_strb(i).asBool) {
        when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
          vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
        }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
          vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
        }.elsewhen(first_gather) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
      }
    }
  }.elsewhen(reg_vrgather16_sew8 && vrgather_table_sent_reg(3) && update_vl_cnt(0)) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := vd_reg((i + 1) * 8 - 1, i * 8)
    }

    for (i <- vlenb / 2 until vlenb) {
      when(vmask_byte_strb(i).asBool) {
        when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max)) {
          vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i) - lo_min)
        }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max)) {
          vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i) - hi_min)
        }.elsewhen(first_gather) {
          vrgather_vd(i) := 0.U
        }
      }.otherwise {
        vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
      }
    }
  }

  // vcompress
  val vd_idx_plus1 = Wire(UInt(4.W))
  vd_idx_plus1 := Cat(0.U(1.W), vd_idx) + 1.U

  when(br_flush_vld) {
    one_sum := 0.U
  }.elsewhen(cmprs_rd_vd_sent_reg(3) || calc_done || !fsm_busy) {
    one_sum := 0.U
  }.elsewhen(rd_sent_reg(3) && !rd_wb_reg(3)) {
    one_sum := one_sum + current_vs_ones_sum
  }

  cmprs_update_vd_idx := cmprs_wb_vld_reg(4)

  when(br_flush_vld) {
    vd_idx := 0.U
  }.elsewhen(update_vd_idx) {
    when(vd_idx === vlmul_reg) {
      vd_idx := 0.U
    }.otherwise {
      vd_idx := vd_idx + 1.U
    }
  }

  base := Cat(update_vl_cnt, 0.U(4.W))

  val current_uop_ones_sum = Wire(Vec(vlenb, UInt(5.W)))

  for (i <- 0 until vlenb) {
    current_uop_ones_sum(i) := 0.U
    current_ones_sum(i) := one_sum
    when(rd_sent_reg(3)) {
      current_uop_ones_sum(i) := PopCount(Cat(vmask_byte_strb.reverse)(i, 0))
      current_ones_sum(i) := one_sum + current_uop_ones_sum(i)
    }
  }

  for (i <- 0 until vlenb) {
    cmprs_vd(i) := vd_reg(i * 8 + 7, i * 8)
    res_idx(i) := 0.U
    res_valid(i) := false.B
  }

  for (i <- 0 until vlenb) {
    when(rd_sent_reg(3)) {
      when(cmprs_rd_vd_sent_reg(3) && (i.U >= one_sum(3, 0))) {
        cmprs_vd(i) := Mux(ta_reg, "hff".U, io.in.fsm_rd_data_3(i * 8 + 7, i * 8))
      }.otherwise {
        res_idx(i) := current_ones_sum(i) - base - 1.U
        res_valid(i) := current_ones_sum(i) >= base + 1.U
        when((vmask_byte_strb(i) === 1.U) && res_valid(i) && (res_idx(i) < vlenb.U)) {
          cmprs_vd(res_idx(i)) := vs2_lo_bytes(i)
        }
      }
    }
  }

  when(br_flush_vld) {
    vd_reg := 0.U
  }.elsewhen(reg_vcompress && rd_sent_reg(3)) {
    vd_reg := Cat(cmprs_vd.reverse)
  }.elsewhen(reg_vslideup && rd_sent_reg(3)) {
    vd_reg := Cat(vslideup_vd.reverse)
  }.elsewhen(reg_vslidedn && rd_sent_reg(3)) {
    vd_reg := Cat(vslidedn_vd.reverse)
  }.elsewhen(reg_vrgather && vrgather_table_sent_reg(3)) {
    vd_reg := Cat(vrgather_vd.reverse)
  }

  val vstartRemain = RegInit(0.U(8.W))
  val vstartRemainBytes = vstartRemain << vsew_reg
  val vstart_bytes = Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  when(br_flush_vld) {
    vstartRemain := 0.U
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    vstartRemain := Mux(vslideup && (viq0_vs1 > viq0_vstart), Mux(viq0_vs1 > VLEN.U, VLEN.U, viq0_vs1), viq0_vstart)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vstartRemain := Mux(vslideup && (viq1_vs1 > viq1_vstart), Mux(viq1_vs1 > VLEN.U, VLEN.U, viq1_vs1), viq1_vstart)
  }.elsewhen(reg_vcompress && rd_sent_reg(4) && !rd_wb_reg(4)) {
    vstartRemain := Mux(vstartRemain >= (1.U << sew_shift), vstartRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(!reg_vrgather16_sew8 && !reg_vcompress && update_vd_idx) {
    vstartRemain := Mux(vstartRemain >= (1.U << sew_shift), vstartRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(reg_vrgather16_sew8 && update_vd_idx && vd_idx(0)) {
    vstartRemain := Mux(vstartRemain >= (1.U << sew_shift), vstartRemain - (1.U << sew_shift), 0.U)
  }

  perm_tail_mask_vd := vd_reg
  when(vstart_reg >= vl_reg) {
    perm_tail_mask_vd := old_vd_reg
  }.elsewhen((reg_vrgather && vrgather_table_sent_reg(4)) || rd_sent_reg(4)) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  perm_vd := perm_tail_mask_vd
  when(reg_vcompress && rd_sent_reg(4) && !rd_wb_resent_reg(4)) {
    perm_vd := vd_reg
  }

  // vslide
  val vslide_offset = vslide_bytes(3, 0)
  // slideoffset, unchange, old_vd
  when(!src_hi_valid_reg(3) && !src_lo_valid_reg(3)) {
    for (i <- 0 until vlenb) {
      vslideup_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
    }
  }.elsewhen(src_hi_valid_reg(3) && !src_lo_valid_reg(3)) { // first old_vd & vs2
    for (i <- 0 until vlenb) {
      when(i.U < vslide_offset) { //old_vd
        vslideup_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
      }.otherwise { // vs2
        when(vmask_byte_strb(i).asBool) {
          vslideup_vd(i) := vs2_hi_bytes(i.U - vslide_offset)
        }.otherwise {
          vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
        }
      }
    }
  }.elsewhen(src_hi_valid_reg(3) && src_lo_valid_reg(3)) { // vs2(i) & vs2(i-1)
    for (i <- 0 until vlenb) {
      when(vmask_byte_strb(i).asBool) {
        when(i.U < vslide_offset) { // vs2(i-1)
          vslideup_vd(i) := vs2_lo_bytes(vlenb.U - vslide_offset + i.U)
        }.otherwise { // vs2(i)
          vslideup_vd(i) := vs2_hi_bytes(i.U - vslide_offset)
        }
      }.otherwise { // MA
        vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
      }
    }
  }

  for (i <- 0 until vlenb) {
    when(vmask_byte_strb(i).asBool) {
      when((i.U < (vlenb.U - vslide_offset)) && src_lo_valid_reg(3)) {
        vslidedn_vd(i) := vs2_lo_bytes(i.U + vslide_offset)
      }.elsewhen((i.U >= (vlenb.U - vslide_offset)) && src_hi_valid_reg(3)) {
        vslidedn_vd(i) := vs2_hi_bytes(i.U + vslide_offset - vlenb.U)
      }.otherwise(
        vslidedn_vd(i) := 0.U
      )
    }.otherwise {
      vslidedn_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    }
  }

  when(br_flush_vld) {
    rd_sent_idx := 0.U
  }.elsewhen(rd_sent && !rd_wb_resent) {
    when(rd_sent_idx === vlmul_reg) {
      rd_sent_idx := 0.U
    }.elsewhen(cmprs_rd_vd) {
      rd_sent_idx := rd_vd_idx
    }.otherwise {
      rd_sent_idx := rd_sent_idx + 1.U
    }
  }

  wb_vld := rd_sent
  when(reg_vcompress) {
    wb_vld := cmprs_wb_vld
  }.elsewhen(reg_vrgather) {
    wb_vld := vrgather_wb_vld
  }


  when(br_flush_vld) {
    wb_idx := 0.U
  }.elsewhen(wb_vld) {
    when(wb_idx === vlmul_reg) {
      wb_idx := 0.U
    }.otherwise {
      wb_idx := wb_idx + 1.U
    }
  }

  when(br_flush_vld) {
    update_vl_cnt := 0.U
  }.elsewhen(update_vl) {
    when(update_vl_cnt === vlmul_reg) {
      update_vl_cnt := 0.U
    }.otherwise {
      update_vl_cnt := update_vl_cnt + 1.U
    }
  }

  io.out.fsm_rd_vld_0 := fsm_rd_vld_0
  io.out.fsm_rd_vld_1 := fsm_rd_vld_1
  io.out.fsm_rd_vld_2 := fsm_rd_vld_2
  io.out.fsm_rd_vld_3 := fsm_rd_vld_3
  io.out.fsm_rd_preg_idx_0 := fsm_rd_preg_idx_0
  io.out.fsm_rd_preg_idx_1 := fsm_rd_preg_idx_1
  io.out.fsm_rd_preg_idx_2 := fsm_rd_preg_idx_2
  io.out.fsm_rd_preg_idx_3 := fsm_rd_preg_idx_3
  io.out.fsm_wb_vld := Mux(reg_vrgather16_sew8, wb_vld && wb_idx(0), wb_vld)
  io.out.fsm_wb_preg_idx := vd_preg_idx(wb_idx)
  io.out.fsm_wb_data := perm_vd
  io.out.fsm_busy := fsm_busy || rd_wb_resent_reg(4)
  io.out.fsm_rob_idx := rob_idx_reg
  io.out.fsm_lmul_4 := lmul_4_reg
}

object VerilogPerFsm extends App {
  println("Generating the VPU Permutation FSM hardware")
  emitVerilog(new PermFsm(), Array("--target-dir", "build/vifu"))
}


