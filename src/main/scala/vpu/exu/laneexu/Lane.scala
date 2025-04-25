package race.vpu.exu.laneexu

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._
import race.vpu.exu.laneexu.alu._

class LaneInput extends Bundle {
  val uop = new VUop
  val vs1 = UInt(LaneWidth.W)
  val vs2 = UInt(LaneWidth.W)
  val rs1 = UInt(xLen.W)
//   val prestart = UInt(NByteLane.W)
//   val mask = UInt(NByteLane.W)
//   val tail = UInt(NByteLane.W)
}

class LaneOutput extends Bundle {
  val uop = new VUop
  val vd = UInt(LaneWidth.W)
  val fflags = Vec(LaneWidth/16, UInt(5.W)) // For eew=32, fflags valid pattern is 0101
//   val vxsat = Bool() // Fixed-point accrued saturation flag
}

class Lane extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new LaneInput))
    val out = ValidIO(new LaneOutput)
  })

  val alu = Module(new LaneALU)
  alu.io.in.valid := io.in.valid && io.in.bits.uop.ctrl.alu
  alu.io.in.bits := io.in.bits
  io.out.valid := alu.io.out.valid
  io.out.bits := alu.io.out.bits
}