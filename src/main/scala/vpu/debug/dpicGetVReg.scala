package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class get_vreg_dpic extends BlackBox(Map(
  "VLEN" -> VLEN
)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val enable = Input(Bool())
    val rf_addr_in = Input(UInt(8.W))
    val data_0_in = Input(UInt(VLEN.W))
    val data_1_in = Input(UInt(VLEN.W))
    val data_2_in = Input(UInt(VLEN.W))
    val data_3_in = Input(UInt(VLEN.W))
    val data_4_in = Input(UInt(VLEN.W))
    val data_5_in = Input(UInt(VLEN.W))
    val data_6_in = Input(UInt(VLEN.W))
    val data_7_in = Input(UInt(VLEN.W))
  })
  
  // Add Verilog resource
  addResource("/vsrc/get_vreg_dpic.sv")
}

class DpicGetVReg extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val rfAddr = Input(UInt(8.W))
    val data8Regs = Input(Vec(8, UInt(VLEN.W)))
  })

  val getVRegDpic = Module(new get_vreg_dpic())

  getVRegDpic.io.clk := clock
  getVRegDpic.io.enable := io.enable
  getVRegDpic.io.rf_addr_in := io.rfAddr
  getVRegDpic.io.data_0_in := io.data8Regs(0)
  getVRegDpic.io.data_1_in := io.data8Regs(1)
  getVRegDpic.io.data_2_in := io.data8Regs(2)
  getVRegDpic.io.data_3_in := io.data8Regs(3)
  getVRegDpic.io.data_4_in := io.data8Regs(4)
  getVRegDpic.io.data_5_in := io.data8Regs(5)
  getVRegDpic.io.data_6_in := io.data8Regs(6)
  getVRegDpic.io.data_7_in := io.data8Regs(7)
}