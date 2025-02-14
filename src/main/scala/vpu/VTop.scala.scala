package race.vpu

import chisel3._
import chisel3.util._
import VParams._
import race.vpu.ctrl._

class VTop extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Input(new Dispatch_S2V)
    val out = Output(new VCtrl)
  })

  
  val decoder = Module(new VDecode)
  decoder.io.in := io.dispatch_s2v.inst
  io.out := decoder.io.out
}


