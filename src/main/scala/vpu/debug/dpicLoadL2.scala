package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

// BlackBox for the Verilog load_l2_dpic module
class load_l2_dpic extends BlackBox(Map(
  "VLEN" -> VLEN
)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rst_n = Input(Bool())
    val enable = Input(Bool())
    val paddr = Input(UInt(64.W))
    val load_data = Output(UInt(VLEN.W))
    val load_valid = Output(Bool())
  })
  
  // Add Verilog resource
  addResource("/vsrc/load_l2_dpic.sv")
}

class DpicLoadL2 extends Module {
  val io = IO(new Bundle {
    // Control signals
    val enable = Input(Bool())
    val paddr = Input(UInt(64.W))
    // Output data
    val load_data = ValidIO(new VL2LoadRsp)
  })
  
  // Instantiate the BlackBox
  val loadL2Dpic = Module(new load_l2_dpic())
  
  // Connect the BlackBox IO with the module IO
  loadL2Dpic.io.clk := clock
  loadL2Dpic.io.rst_n := !reset.asBool
  loadL2Dpic.io.enable := io.enable
  loadL2Dpic.io.paddr := io.paddr
//   io.load_data := loadL2Dpic.io.load_data
//   io.load_valid := loadL2Dpic.io.load_valid
  io.load_data.valid := loadL2Dpic.io.load_valid
  io.load_data.bits.data := VecInit(UIntSplit(loadL2Dpic.io.load_data, CachelineBits))
}
