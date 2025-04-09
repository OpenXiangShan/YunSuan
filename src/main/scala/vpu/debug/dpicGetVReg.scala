// package race.vpu.debug

// import chisel3._
// import chisel3.util._
// import race.vpu.VParams._
// import race.vpu._

// // BlackBox for the Verilog load_l2_dpic module
// class get_vreg_dpic extends BlackBox(Map(
//   "VLEN" -> VLEN
// )) with HasBlackBoxResource {
//   val io = IO(new Bundle {
//     val clk = Input(Clock())
//     val enable = Input(Bool())
//     val rf_addr_in = Input(UInt(8.W))
//     val data_0_in = Input(UInt(VLEN.W))
//     val data_1_in = Input(UInt(VLEN.W))
//     val data_2_in = Input(UInt(VLEN.W))
//     val data_3_in = Input(UInt(VLEN.W))
//     val data_4_in = Input(UInt(VLEN.W))
//     val data_5_in = Input(UInt(VLEN.W))
//     val data_6_in = Input(UInt(VLEN.W))
//     val data_7_in = Input(UInt(VLEN.W))
//   })
  
//   // Add Verilog resource
//   addResource("/vsrc/get_vreg_dpic.sv")
// }
