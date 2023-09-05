package yunsuan.vector.permfsm

import chisel3._
import chisel3.util._
import scala.language.postfixOps
import yunsuan.vector._
import yunsuan.vector.permfsm.VPermFsmOpcode._

class PermFsm extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN / 64
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

  val vslideup =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVslideup) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVslideup)
  val vslidedn =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVslidedn) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVslidedn)
  val vrgather_vv =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather)
  val vrgather_vx =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather_vx) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather_vx)
  val vrgather16 =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16)
  val vcompress =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVcompress) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVcompress)
  val vrgather16_sew8 =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 0.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 0.U))
  val vrgather16_sew32 =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 2.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 2.U))
  val vrgather16_sew64 =
    (viq0_uop_vld && !viq0_uop_flush_vld && viq0_opcode.isVrgather16 && (viq0_sew === 3.U)) || (viq1_uop_vld && !viq1_uop_flush_vld && viq1_opcode.isVrgather16 && (viq1_sew === 3.U))
  val vslide = vslideup || vslidedn
  val vrgather = vrgather_vv || vrgather_vx || vrgather16

  val uop_valid = (viq0_uop_vld && !viq0_uop_flush_vld) || (viq1_uop_vld && !viq1_uop_flush_vld)
  val viq01_valid = (viq0_uop_vld && !viq0_uop_flush_vld) && (viq1_uop_vld && !viq1_uop_flush_vld)
  val viq0_valid = viq0_uop_vld && !viq0_uop_flush_vld
  val viq1_valid = viq1_uop_vld && !viq1_uop_flush_vld
  val vlmul = Mux(
    ((viq0_uop_vld && !viq0_uop_flush_vld && !viq0_lmul_4) || (viq1_uop_vld && !viq1_uop_flush_vld && !viq1_lmul_4) || vrgather16_sew8),
    7.U,
    3.U
  )
  val uop_cnt = RegInit(0.U(3.W))
  val rec_done = Mux(viq01_valid, (uop_cnt === vlmul - 1.U), (uop_cnt === vlmul)) && uop_valid
  val vs_idx = RegInit(0.U(3.W))

  val opcode_reg = RegInit(dummy.asTypeOf(new VPermFsmOpcode))
  val vsew_reg = RegInit(0.U(2.W))
  val vm_reg = RegInit(false.B)
  val ta_reg = RegInit(false.B)
  val ma_reg = RegInit(false.B)
  val vstart_reg = RegInit(0.U(7.W))
  val vlmul_reg = RegInit(0.U(3.W))
  val rob_idx_reg = RegInit(0.U(3.W))
  val lmul_4_reg = RegInit(false.B)
  val fsm_busy = RegInit(false.B)
  val vd_idx = RegInit(0.U(3.W))
  val update_table_cnt = RegInit(0.U(2.W))
  val update_table_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val update_vs_idx_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val table_lo_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(4.W))))
  val table_hi_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(4.W))))
  val vs_idx_reg = RegInit(VecInit(Seq.fill(5)(0.U(3.W))))
  val calc_done = (vd_idx === vlmul_reg) && update_vs_idx_reg(4)
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
  val vlRemain = RegInit(0.U(8.W))
  val vlRemainBytes = vlRemain << vsew_reg
  val vd_mask = (~0.U(VLEN.W))
  val sew_shift = Wire(UInt(3.W))

  val tail_bytes = Mux((vlRemainBytes >= vlenb.U), 0.U, vlenb.U - vlRemainBytes)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = vd_mask >> tail_bits
  val tail_old_vd = old_vd & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val perm_vd = Wire(Vec(vlenb, UInt(8.W)))
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))
  val vd_reg = RegInit(0.U(VLEN.W))

  val idle :: rec_idx :: rd_vs :: calc_vd :: Nil = Enum(4)
  val perm_state = RegInit(idle)
  val update_table = (perm_state === rd_vs) & !block_fsm_rd_vld

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
  val res_idx = Wire(Vec(vlenb, UInt(7.W)))
  val res_valid = Wire(Vec(vlenb, Bool()))
  val current_ones_sum = Wire(Vec(vlenb + 1, UInt(8.W)))

  val base = Wire(UInt(7.W))
  val eewVs2 = SewOH(vsew_reg)
  val current_vs_ones_sum = Mux1H(
    Seq(
      eewVs2.is8 -> PopCount(vmask_reg(15, 0)),
      eewVs2.is16 -> Cat(PopCount(vmask_reg(7, 0)), 0.U(1.W)),
      eewVs2.is32 -> Cat(PopCount(vmask_reg(3, 0)), 0.U(2.W)),
      eewVs2.is64 -> Cat(PopCount(vmask_reg(1, 0)), 0.U(3.W))
    )
  )

  val total_one_sum = RegInit(0.U(8.W))

  when(br_flush_vld) {
    total_one_sum := 0.U
  }.elsewhen(rec_done) {
    when(lmul_4_reg) {
      total_one_sum := Mux1H(
        Seq(
          eewVs2.is8 -> PopCount(vmask_reg(63, 0)),
          eewVs2.is16 -> Cat(PopCount(vmask_reg(31, 0)), 0.U(1.W)),
          eewVs2.is32 -> Cat(PopCount(vmask_reg(15, 0)), 0.U(2.W)),
          eewVs2.is64 -> Cat(PopCount(vmask_reg(7, 0)), 0.U(3.W))
        )
      )
    }.otherwise {
      total_one_sum := Mux1H(
        Seq(
          eewVs2.is8 -> PopCount(vmask_reg(127, 0)),
          eewVs2.is16 -> Cat(PopCount(vmask_reg(63, 0)), 0.U(1.W)),
          eewVs2.is32 -> Cat(PopCount(vmask_reg(31, 0)), 0.U(2.W)),
          eewVs2.is64 -> Cat(PopCount(vmask_reg(15, 0)), 0.U(3.W))
        )
      )
    }
  }

  val update_vs_idx = Wire(Bool())
  val update_vd_idx = Mux(reg_vcompress, cmprs_update_vd_idx, update_vs_idx_reg(4))
  val vslide_ele = Cat(vs1_preg_idx.reverse)
  val vslide_bytes = vslide_ele << vsew_reg
  val src_lo_valid = Mux(reg_vslideup, vslide_bytes(65, 4) + 1.U <= vs_idx, vs_idx + vslide_bytes(65, 4) <= vlmul_reg)
  val src_hi_valid = Mux(reg_vslideup, vslide_bytes(65, 4) <= vs_idx, vs_idx + vslide_bytes(65, 4) + 1.U <= vlmul_reg)
  val src_lo_valid_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val src_hi_valid_reg = RegInit(VecInit(Seq.fill(5)(false.B)))

  val rd_vd_idx = RegInit(0.U(4.W))
  val cmprs_rd_vd = RegInit(false.B)
  val cmprs_rd_vd_reg = RegInit(VecInit(Seq.fill(5)(false.B)))
  val cmprs_rd_done = (rd_vd_idx === (vlmul_reg + 1.U)) || ((rd_vd_idx === vlmul_reg) && cmprs_rd_vd)
  val rd_done = Mux(reg_vcompress, cmprs_rd_done, (vs_idx === vlmul_reg) && update_vs_idx)
  val old_vd_idx = Mux(
    reg_vcompress,
    rd_vd_idx,
    Mux(reg_vrgather16_sew8, vs_idx(2, 1) + 1.U, Mux(vs_idx === vlmul_reg, vs_idx, vs_idx + 1.U))
  )
  val rd_one_sum = RegInit(0.U(8.W))
  val rd_wb = Wire(Bool())
  val cmprs_wb_vld = RegNext(rd_wb || cmprs_rd_vd)

  val current_rd_vs_ones_sum = Mux1H(
    Seq(
      eewVs2.is8 -> PopCount(old_vd(15, 0)),
      eewVs2.is16 -> Cat(PopCount(old_vd(7, 0)), 0.U(1.W)),
      eewVs2.is32 -> Cat(PopCount(old_vd(3, 0)), 0.U(2.W)),
      eewVs2.is64 -> Cat(PopCount(old_vd(1, 0)), 0.U(3.W))
    )
  )

  when(br_flush_vld) {
    rd_one_sum := 0.U
  }.elsewhen(rd_done) {
    rd_one_sum := 0.U
  }.elsewhen(reg_vcompress && !cmprs_rd_vd && (perm_state === rd_vs) && !cmprs_wb_vld) {
    when((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) {
      rd_one_sum := rd_one_sum + current_rd_vs_ones_sum - vlenb.U
    }.otherwise {
      rd_one_sum := rd_one_sum + current_rd_vs_ones_sum
    }
  }.elsewhen(rd_one_sum >= vlenb.U) {
    rd_one_sum := rd_one_sum - vlenb.U
  }

  rd_wb := false.B
  when((rd_one_sum + current_rd_vs_ones_sum) >= vlenb.U) {
    rd_wb := true.B
  }

  update_vs_idx := false.B
  when(reg_vrgather && !Cat(table_hi_lo.reverse).orR && (perm_state === rd_vs)) {
    update_vs_idx := true.B
  }.elsewhen(reg_vcompress && !cmprs_rd_vd && (perm_state === rd_vs) && !rd_wb) {
    update_vs_idx := true.B
  }.elsewhen(reg_vslide && (perm_state === rd_vs)) {
    update_vs_idx := true.B
  }

  rd_vd_idx := total_one_sum(7, 4)
  when(br_flush_vld) {
    rd_vd_idx := 0.U
  }.elsewhen((rd_vd_idx === vlmul_reg) && cmprs_rd_vd) {
    rd_vd_idx := 0.U
  }.elsewhen(cmprs_rd_vd) {
    rd_vd_idx := rd_vd_idx + 1.U
  }

  when(br_flush_vld) {
    cmprs_rd_vd := false.B
  }.elsewhen((rd_vd_idx === vlmul_reg) && cmprs_rd_vd) {
    cmprs_rd_vd := false.B
  }.elsewhen((vs_idx === vlmul_reg) && update_vs_idx) {
    when(rd_vd_idx =/= (vlmul_reg + 1.U)) {
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
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.otherwise {
        for (i <- 0 until vlenb / 2) {
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }
    }.elsewhen(vrgather16_sew64) {
      when(uop_idx(1, 0) === 0.U) {
        for (i <- 0 until vlenb / 4) {
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.elsewhen(uop_idx(1, 0) === 1.U) {
        for (i <- vlenb / 4 until vlenb / 2) {
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.elsewhen(uop_idx(1, 0) === 2.U) {
        for (i <- vlenb / 2 until 3 * vlenb / 4) {
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
          when(table_idx(i) <= vlmul) {
            access_table(table_idx(i)) := 1.U
          }
        }
      }.otherwise {
        for (i <- 3 * vlenb / 4 until vlenb) {
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
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
          table_idx(i) := Mux1H(
            sew.oneHot,
            Seq(8, 16, 32, 64).map(n => vs1((i / (n / 8) + 1) * n - 1, i / (n / 8) * n))
          ) >> shift
        }
        when(table_idx(i) <= Mux(vrgather16_sew8, 7.U, vlmul)) {
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

  fsm_rd_vld_0 := false.B
  fsm_rd_vld_1 := false.B
  fsm_rd_vld_2 := false.B
  fsm_rd_vld_3 := false.B
  fsm_rd_preg_idx_0 := 0.U
  fsm_rd_preg_idx_1 := 0.U
  fsm_rd_preg_idx_2 := 0.U
  fsm_rd_preg_idx_3 := 0.U

  when(reg_vrgather && (perm_state === rd_vs)) {
    fsm_rd_vld_0 := true.B
    when(reg_vrgather16_sew32) {
      fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2, 1))
    }.elsewhen(reg_vrgather16_sew64) {
      fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx(2))
    }.otherwise {
      fsm_rd_preg_idx_0 := vs1_preg_idx(vs_idx)
    }
  }

  val rd_idx_1 = Wire(UInt(3.W))
  val rd_idx_2 = Wire(UInt(3.W))
  rd_idx_1 := 0.U
  rd_idx_2 := 0.U
  when(reg_vrgather) {
    rd_idx_1 := table_lo_idx
    rd_idx_2 := table_hi_idx
  }.elsewhen(reg_vslideup) {
    rd_idx_1 := vs_idx - vslide_bytes(6, 4) - 1.U
    rd_idx_2 := vs_idx - vslide_bytes(6, 4)
  }.elsewhen(reg_vslidedn) {
    rd_idx_1 := vs_idx + vslide_bytes(6, 4)
    rd_idx_2 := vs_idx + vslide_bytes(6, 4) + 1.U
  }.elsewhen(reg_vcompress) {
    rd_idx_1 := vs_idx
  }

  when(
    ((reg_vrgather && table_lo.orR) || (reg_vcompress && !cmprs_rd_vd) || (reg_vslide && src_lo_valid)) && (perm_state === rd_vs)
  ) {
    fsm_rd_vld_1 := true.B
    fsm_rd_preg_idx_1 := vs2_preg_idx(rd_idx_1)
  }

  when(((reg_vrgather && table_hi.orR) || (reg_vslide && src_hi_valid)) && (perm_state === rd_vs)) {
    fsm_rd_vld_2 := true.B
    fsm_rd_preg_idx_2 := vs2_preg_idx(rd_idx_2)
  }

  when((!reg_vcompress && (perm_state === rd_vs)) || (reg_vcompress && cmprs_rd_vd)) {
    fsm_rd_vld_3 := true.B
    fsm_rd_preg_idx_3 := old_vd_preg_idx(old_vd_idx)
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
    vs_idx := vs_idx + 1.U
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
    vlmul_reg := vlmul
    rob_idx_reg := viq1_rob_idx
    lmul_4_reg := viq1_lmul_4
  }

  when(viq0_valid && (viq0_uop_idx === 0.U)) {
    old_vd := Mux(vcompress, viq0_vs1, viq0_old_vd)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    old_vd := Mux(vcompress, viq1_vs1, viq1_old_vd)
  }.elsewhen(!reg_vcompress && !reg_vrgather16_sew8 && update_vs_idx_reg(4)) {
    old_vd := io.in.fsm_rd_data_3
  }.elsewhen(!reg_vcompress && reg_vrgather16_sew8 && update_vs_idx_reg(4) && vd_idx(0)) {
    old_vd := io.in.fsm_rd_data_3
  }.elsewhen(reg_vcompress && update_vs_idx) {
    old_vd := old_vd >> (1.U << sew_shift)
  }.elsewhen(reg_vrgather && update_table_reg(4)) {
    old_vd := Cat(vrgather_vd.reverse)
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
    vs_idx_reg(4) := 0.U
    vs_idx_reg(3) := 0.U
    vs_idx_reg(2) := 0.U
    vs_idx_reg(1) := 0.U
    vs_idx_reg(0) := 0.U

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

    update_vs_idx_reg(4) := false.B
    update_vs_idx_reg(3) := false.B
    update_vs_idx_reg(2) := false.B
    update_vs_idx_reg(1) := false.B
    update_vs_idx_reg(0) := false.B

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
    vs_idx_reg(4) := vs_idx_reg(3)
    vs_idx_reg(3) := vs_idx_reg(2)
    vs_idx_reg(2) := vs_idx_reg(1)
    vs_idx_reg(1) := vs_idx_reg(0)
    vs_idx_reg(0) := vs_idx

    table_lo_idx_reg(4) := table_lo_idx_reg(3)
    table_lo_idx_reg(3) := table_lo_idx_reg(2)
    table_lo_idx_reg(2) := table_lo_idx_reg(1)
    table_lo_idx_reg(1) := table_lo_idx_reg(0)
    table_lo_idx_reg(0) := table_lo_idx

    table_hi_idx_reg(4) := table_hi_idx_reg(3)
    table_hi_idx_reg(3) := table_hi_idx_reg(2)
    table_hi_idx_reg(2) := table_hi_idx_reg(1)
    table_hi_idx_reg(1) := table_hi_idx_reg(0)
    table_hi_idx_reg(0) := table_hi_idx

    update_table_reg(4) := update_table_reg(3)
    update_table_reg(3) := update_table_reg(2)
    update_table_reg(2) := update_table_reg(1)
    update_table_reg(1) := update_table_reg(0)
    update_table_reg(0) := update_table

    update_vs_idx_reg(4) := update_vs_idx_reg(3)
    update_vs_idx_reg(3) := update_vs_idx_reg(2)
    update_vs_idx_reg(2) := update_vs_idx_reg(1)
    update_vs_idx_reg(1) := update_vs_idx_reg(0)
    update_vs_idx_reg(0) := update_vs_idx

    src_lo_valid_reg(4) := src_lo_valid_reg(3)
    src_lo_valid_reg(3) := src_lo_valid_reg(2)
    src_lo_valid_reg(2) := src_lo_valid_reg(1)
    src_lo_valid_reg(1) := src_lo_valid_reg(0)
    src_lo_valid_reg(0) := src_lo_valid

    src_hi_valid_reg(4) := src_hi_valid_reg(3)
    src_hi_valid_reg(3) := src_hi_valid_reg(2)
    src_hi_valid_reg(2) := src_hi_valid_reg(1)
    src_hi_valid_reg(1) := src_hi_valid_reg(0)
    src_hi_valid_reg(0) := src_hi_valid

    cmprs_rd_vd_reg(4) := cmprs_rd_vd_reg(3)
    cmprs_rd_vd_reg(3) := cmprs_rd_vd_reg(2)
    cmprs_rd_vd_reg(2) := cmprs_rd_vd_reg(1)
    cmprs_rd_vd_reg(1) := cmprs_rd_vd_reg(0)
    cmprs_rd_vd_reg(0) := cmprs_rd_vd
  }

  val lo_min = Mux(table_lo_idx_reg(4) === "hf".U, "hff".U, Cat(table_lo_idx_reg(4), 0.U(4.W)))
  val lo_max = Mux(table_lo_idx_reg(4) === "hf".U, "hff".U, Cat((table_lo_idx_reg(4) + 1.U), 0.U(4.W)))
  val hi_min = Mux(table_hi_idx_reg(4) === "hf".U, "hff".U, Cat(table_hi_idx_reg(4), 0.U(4.W)))
  val hi_max = Mux(table_hi_idx_reg(4) === "hf".U, "hff".U, Cat((table_hi_idx_reg(4) + 1.U), 0.U(4.W)))

  when(br_flush_vld) {
    update_table_cnt := 0.U
  }.elsewhen(update_table_reg(4) && update_vs_idx_reg(4)) {
    update_table_cnt := 0.U
  }.elsewhen(update_table_reg(4)) {
    update_table_cnt := update_table_cnt + 1.U
  }

  sew_shift := Cat(0.U(1.W), ~vsew_reg) + 1.U
  when(br_flush_vld) {
    vlRemain := 0.U
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    vlRemain := viq0_vl
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vlRemain := viq1_vl
  }.elsewhen(!reg_vrgather16_sew8 && update_vd_idx) {
    vlRemain := Mux(vlRemain >= (1.U << sew_shift), vlRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(reg_vrgather16_sew8 && update_vd_idx && vd_idx(0)) {
    vlRemain := Mux(vlRemain >= (1.U << sew_shift), vlRemain - (1.U << sew_shift), 0.U)
  }

  when(viq0_valid && (viq0_uop_idx === 0.U)) {
    vmask_reg := Mux(vcompress, viq0_vs1, viq0_mask)
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vmask_reg := Mux(vcompress, viq1_vs1, viq1_mask)
  }.elsewhen(update_vs_idx_reg(4)) {
    when(reg_vrgather16_sew8) {
      when(vd_idx(0)) {
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
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    vslideup_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    vslidedn_vd(i) := Mux(ma_reg, "hff".U, old_vd(i * 8 + 7, i * 8))
    cmprs_vd(i) := 0.U
  }

  for (i <- 0 until vlenb / 2) {
    vrgather_byte_sel(i) := 0.U
    when(reg_vrgather_vx) {
      vrgather_byte_sel(i) := Cat(vs1_preg_idx.reverse)
    }.otherwise {
      when(vs1_type === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_type === 1.U) {
        when((vsew_reg === 0.U) && !vd_idx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(vsew_reg === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(vsew_reg === 2.U) {
          when(vs_idx_reg(4)(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(vsew_reg === 3.U) {
          when(vs_idx_reg(4)(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vs_idx_reg(4) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vs_idx_reg(4) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
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
    }.otherwise {
      when(vs1_type === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(vs1_type === 1.U) {
        when((vsew_reg === 0.U) && vd_idx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
        }.elsewhen(vsew_reg === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(vsew_reg === 2.U) {
          when(vs_idx_reg(4)(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(vsew_reg === 3.U) {
          when(vs_idx_reg(4)(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vs_idx_reg(4) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vs_idx_reg(4) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
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

  when(reg_vrgather && update_table_reg(4)) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
      when((vrgather_byte_sel(i) >= lo_min) && (vrgather_byte_sel(i) < lo_max) && vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := vs2_lo_bytes(vrgather_byte_sel(i.U) - lo_min)
      }.elsewhen((vrgather_byte_sel(i) >= hi_min) && (vrgather_byte_sel(i) < hi_max) && vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := vs2_hi_bytes(vrgather_byte_sel(i.U) - hi_min)
      }.elsewhen(first_gather && vmask_byte_strb(i).asBool) {
        vrgather_vd(i) := 0.U
      }
    }
  }

  // vcompress
  val vd_idx_plus1 = Wire(UInt(4.W))
  vd_idx_plus1 := Cat(0.U(1.W), vd_idx) + 1.U

  when(br_flush_vld) {
    one_sum := 0.U
  }.elsewhen(cmprs_rd_vd_reg(4)) {
    one_sum := 0.U
  }.elsewhen(update_vs_idx_reg(4)) {
    one_sum := one_sum + current_vs_ones_sum
  }

  cmprs_update_vd_idx := false.B
  when(((one_sum + current_vs_ones_sum) >= Cat(vd_idx_plus1, 0.U(4.W))) || cmprs_rd_vd_reg(4)) {
    cmprs_update_vd_idx := true.B
  }

  when(br_flush_vld) {
    vd_idx := 0.U
  }.elsewhen(update_vd_idx) {
    when(vd_idx === vlmul_reg) {
      vd_idx := 0.U
    }.otherwise {
      vd_idx := vd_idx + 1.U
    }
  }

  base := Cat(vd_idx, 0.U(4.W))

  for (i <- 0 until (vlenb + 1)) {
    current_ones_sum(i) := 0.U
  }

  for (i <- 0 until vlenb) {
    cmprs_vd(i) := Mux(ta_reg, "hff".U, vd_reg(i * 8 + 7, i * 8))
    res_idx(i) := 0.U
    res_valid(i) := false.B
  }

  current_ones_sum(0) := one_sum
  for (i <- 0 until vlenb) {
    when(cmprs_rd_vd_reg(4) && (i.U >= one_sum(3, 0))) {
      cmprs_vd(i) := io.in.fsm_rd_data_3(i * 8 + 7, i * 8)
    }.otherwise {
      current_ones_sum(i + 1) := current_ones_sum(i) + vmask_byte_strb(i)
      res_idx(i) := current_ones_sum(i + 1) - base - 1.U
      res_valid(i) := current_ones_sum(i + 1) >= base + 1.U
      when((vmask_byte_strb(i) === 1.U) && res_valid(i) && (res_idx(i) < vlenb.U)) {
        cmprs_vd(res_idx(i)) := vs2_lo(i * 8 + 7, i * 8)
      }
    }
  }

  perm_vd := vslideup_vd
  when(reg_vslideup && update_vs_idx_reg(4)) {
    perm_vd := vslideup_vd
  }.elsewhen(reg_vslidedn && update_vs_idx_reg(4)) {
    perm_vd := vslidedn_vd
  }.elsewhen(reg_vrgather && update_vs_idx_reg(4)) {
    perm_vd := vrgather_vd
  }

  val vstartRemain = RegInit(0.U(7.W))
  val vstartRemainBytes = vstartRemain << vsew_reg
  val vstart_bytes = Mux(vstartRemainBytes >= vlenb.U, vlenb.U, vstartRemainBytes)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = vd_mask << vstart_bits
  val vstart_old_vd = old_vd & (~vmask_vstart_bits)

  when(br_flush_vld) {
    vstartRemain := 0.U
  }.elsewhen(viq0_valid && (viq0_uop_idx === 0.U)) {
    vstartRemain := viq0_vstart
  }.elsewhen(viq1_valid && (viq1_uop_idx === 0.U)) {
    vstartRemain := viq1_vstart
  }.elsewhen(!reg_vrgather16_sew8 && update_vd_idx) {
    vstartRemain := Mux(vstartRemain >= (1.U << sew_shift), vstartRemain - (1.U << sew_shift), 0.U)
  }.elsewhen(reg_vrgather16_sew8 && update_vd_idx && vd_idx(0)) {
    vstartRemain := Mux(vstartRemain >= (1.U << sew_shift), vstartRemain - (1.U << sew_shift), 0.U)
  }

  perm_tail_mask_vd := Cat(perm_vd.reverse)
  when(update_vs_idx_reg(4)) {
    perm_tail_mask_vd := (Cat(perm_vd.reverse) & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  when(reg_vcompress) {
    vd_reg := Cat(cmprs_vd.reverse)
  }.elsewhen(reg_vrgather16_sew8 && vd_idx(1).asBool && update_vs_idx_reg(4)) {
    vd_reg := perm_tail_mask_vd
  }.elsewhen(update_vs_idx_reg(4)) {
    vd_reg := perm_tail_mask_vd
  }

  // vslide
  val vslide_offset = vslide_bytes(3, 0)
  for (i <- 0 until vlenb) {
    when(
      (i.U >= vslide_offset) && (i.U < vlenb.U) && vmask_byte_strb(i.U - vslide_offset).asBool && src_lo_valid_reg(4)
    ) {
      vslideup_vd(i.U - vslide_offset) := vs2_lo_bytes(i)
    }.elsewhen((i.U < vslide_offset) && vmask_byte_strb(i.U + vlenb.U - vslide_offset).asBool && src_hi_valid_reg(4)) {
      vslideup_vd(i.U + vlenb.U - vslide_offset) := vs2_hi_bytes(i)
    }
  }

  for (i <- 0 until vlenb) {
    when(vmask_byte_strb(i).asBool) {
      when((i.U < (vlenb.U - vslide_offset)) && src_lo_valid_reg(4)) {
        vslidedn_vd(i) := vs2_lo_bytes(i.U + vslide_offset)
      }.elsewhen((i.U >= (vlenb.U - vslide_offset)) && src_hi_valid_reg(4)) {
        vslidedn_vd(i) := vs2_hi_bytes(i.U + vslide_offset - vlenb.U)
      }.otherwise(
        vslidedn_vd(i) := 0.U
      )
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
  io.out.fsm_wb_vld := Mux(
    reg_vcompress,
    cmprs_wb_vld,
    Mux(reg_vrgather16_sew8, update_vs_idx_reg(0) && vs_idx_reg(0)(0), update_vs_idx_reg(0))
  )
  io.out.fsm_wb_preg_idx := vd_preg_idx(vs_idx_reg(0))
  io.out.fsm_wb_data := vd_reg
  io.out.fsm_busy := fsm_busy
  io.out.fsm_rob_idx := rob_idx_reg
  io.out.fsm_lmul_4 := lmul_4_reg
}

object VerilogPerFsm extends App {
  println("Generating the VPU Permutation FSM hardware")
  emitVerilog(new PermFsm(), Array("--target-dir", "build/vifu"))
}
