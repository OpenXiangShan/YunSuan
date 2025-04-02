package race.vpu.exu.laneexu

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._

class LaneExu extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new VExuInput))
    val out = ValidIO(new VExuOutput)
  })

  val vSrcSplit = io.in.bits.vSrc.map(UIntSplit.vlen_splitTo_lanes(_))

  val lanes = Seq.fill(NLanes)(Module(new Lane))
  for (i <- 0 until NLanes) {
    lanes(i).io.in.valid := io.in.valid
    lanes(i).io.in.bits.uop := io.in.bits.uop
    lanes(i).io.in.bits.vs1 := vSrcSplit(0)(i)
    lanes(i).io.in.bits.vs2 := vSrcSplit(1)(i)
    // lanes(i).io.in.bits.oldVd := vSrcSplit(2)(i)
    // lanes(i).io.in.bits.mask := vSrcSplit(3)(i)
    lanes(i).io.in.bits.rs1 := io.in.bits.rs1
  }

  val vd = Cat(lanes.map(_.io.out.bits.vd).reverse)
  val fflags = lanes.map(_.io.out.bits.fflags).reduce(_ | _)
  io.out.bits.vd := vd
  io.out.bits.fflags := fflags
  io.out.bits.uop := lanes(0).io.out.bits.uop
  io.out.valid := lanes(0).io.out.valid

}