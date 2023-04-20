
package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.vector.permfsm.VPermFsmOpcode._

class VPermFsmOpcode extends Bundle {
  val op = UInt(3.W)

  def isVslideup = op === vslideup

  def isVslidedn = op === vslidedn

  def isVrgather = op === vrgather

  def isVrgather_vx = op === vrgather_vx

  def isVrgather16 = op === vrgather16

  def isVcompress = op === vcompress

  def isDummy = op === dummy
}

class VPermFsmInput extends Bundle {
  val viq0_opcode = new VPermFsmOpcode
  val viq0_sew = UInt(2.W)
  val viq0_vs1 = UInt(128.W)
  val viq0_old_vd = UInt(128.W)
  val viq0_mask = UInt(128.W)
  val viq0_vs1_preg_idx = UInt(8.W)
  val viq0_vs2_preg_idx = UInt(8.W)
  val viq0_old_vd_preg_idx = UInt(8.W)
  val viq0_vd_preg_idx = UInt(8.W)
  val viq0_uop_idx = UInt(3.W)
  val viq0_vm = Bool()
  val viq0_ta = Bool()
  val viq0_ma = Bool()
  val viq0_vstart = UInt(7.W)
  val viq0_vl = UInt(8.W)
  val viq0_lmul_4 = Bool()
  val viq0_uop_vld = Bool()
  val viq0_uop_flush_vld = Bool()
  val viq0_rob_idx = UInt(9.W)
  val viq1_opcode = new VPermFsmOpcode
  val viq1_sew = UInt(2.W)
  val viq1_vs1 = UInt(128.W)
  val viq1_old_vd = UInt(128.W)
  val viq1_mask = UInt(128.W)
  val viq1_vs1_preg_idx = UInt(8.W)
  val viq1_vs2_preg_idx = UInt(8.W)
  val viq1_old_vd_preg_idx = UInt(8.W)
  val viq1_vd_preg_idx = UInt(8.W)
  val viq1_uop_idx = UInt(3.W)
  val viq1_vm = Bool()
  val viq1_ta = Bool()
  val viq1_ma = Bool()
  val viq1_vstart = UInt(7.W)
  val viq1_vl = UInt(8.W)
  val viq1_lmul_4 = Bool()
  val viq1_uop_vld = Bool()
  val viq1_uop_flush_vld = Bool()
  val viq1_rob_idx = UInt(9.W)
  val block_fsm_rd_vld = Bool()
  val fsm_rd_data_0 = UInt(128.W)
  val fsm_rd_data_1 = UInt(128.W)
  val fsm_rd_data_2 = UInt(128.W)
  val fsm_rd_data_3 = UInt(128.W)
  val br_flush_vld = Bool()
  val br_flush_rob_idx = UInt(9.W)
  val rob_flush_vld = Bool()
  val rob_commit_vld = Bool()
  val rob_commit_rob_idx = UInt(9.W)

}


class VPermFsmOutput extends Bundle {
  val fsm_rd_vld_0 = Bool()
  val fsm_rd_vld_1 = Bool()
  val fsm_rd_vld_2 = Bool()
  val fsm_rd_vld_3 = Bool()
  val fsm_rd_preg_idx_0 = UInt(8.W)
  val fsm_rd_preg_idx_1 = UInt(8.W)
  val fsm_rd_preg_idx_2 = UInt(8.W)
  val fsm_rd_preg_idx_3 = UInt(8.W)
  val fsm_wb_vld = Bool()
  val fsm_wb_preg_idx = UInt(8.W)
  val fsm_wb_data = UInt(128.W)
  val fsm_busy = Bool()
  val fsm_rob_idx = UInt(9.W)
  val fsm_lmul_4 = Bool()

}
