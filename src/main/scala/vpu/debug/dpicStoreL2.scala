package race.vpu.debug

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

// BlackBox for the Verilog store_l2_dpic module
class store_l2_dpic extends BlackBox(Map(
  "VLEN" -> VLEN
)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val enable = Input(Bool())
    val paddr = Input(UInt(64.W))
    val store_data = Input(UInt(VLEN.W))
  })
  
  // Add Verilog resource
  addResource("/vsrc/store_l2_dpic.sv")
}

class DpicStoreL2 extends Module {
  val io = IO(new Bundle {
    // Store request
    val store_req = Flipped(ValidIO(new VL2StoreReq))
  })
  
  // Instantiate the BlackBox
  val storeL2Dpic = Module(new store_l2_dpic())
  
  // Connect the BlackBox IO with the module IO
  storeL2Dpic.io.clk := clock
  storeL2Dpic.io.enable := io.store_req.valid
  
  // 由于VL2StoreReq支持多个端口，这里我们只使用第一个端口
  storeL2Dpic.io.paddr := io.store_req.bits.addr(0)
  
  // 将多个Cacheline的数据合并成一个VLEN宽的数据
  val store_data = Cat(io.store_req.bits.data.reverse)
  storeL2Dpic.io.store_data := store_data
  

} 