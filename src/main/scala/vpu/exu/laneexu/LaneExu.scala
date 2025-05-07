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

  val uop = io.in.bits.uop
  val vSrcSplit = io.in.bits.vSrc.map(UIntSplit.vlen_splitTo_lanes(_))

  val widenVs1 = rearrg_widen(vSrcSplit(0))
  val widenVs2 = rearrg_widen(vSrcSplit(1))

  val vs1Final = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  val vs2Final = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  for (i <- 0 until NLanes) {
    vs1Final(i) := Mux(uop.ctrl.widen || uop.ctrl.widen2 || uop.ctrl.narrow,
                       widenVs1(i), vSrcSplit(0)(i))
    vs2Final(i) := Mux(uop.ctrl.widen,
                       widenVs2(i), vSrcSplit(1)(i))
  }

  val lanes = Seq.fill(NLanes)(Module(new Lane))
  for (i <- 0 until NLanes) {
    lanes(i).io.in.valid := io.in.valid
    lanes(i).io.in.bits.uop := io.in.bits.uop
    lanes(i).io.in.bits.vs1 := vs1Final(i)
    lanes(i).io.in.bits.vs2 := vs2Final(i)
    lanes(i).io.in.bits.vs3 := vSrcSplit(2)(i)
    // lanes(i).io.in.bits.oldVd := vSrcSplit(2)(i)
    // lanes(i).io.in.bits.mask := vSrcSplit(3)(i)
    lanes(i).io.in.bits.rs1 := io.in.bits.rs1
  }

  val vd = lanes.map(_.io.out.bits.vd)
  val vdRearrg = rearrg_narrow(vd)
  val vdFinal = Wire(Vec(NLanes, UInt(LaneWidth.W)))
  for (i <- 0 until NLanes) {
    vdFinal(i) := Mux(lanes(0).io.out.bits.uop.ctrl.narrow, vdRearrg(i), vd(i))
  }
  io.out.bits.vd := vdFinal.asUInt
  
  val lanefflags = lanes.map(_.io.out.bits.fflags.asUInt)
  io.out.bits.fflags := VecInit(UIntSplit(VecInit(lanefflags).asUInt, 5))
  io.out.bits.uop := lanes(0).io.out.bits.uop
  io.out.valid := lanes(0).io.out.valid

  /**
    * Lane data rearrange functions for widen/narrow
    */
  // Widen rearrange for input
  def rearrg_widen(in: Seq[UInt]): Vec[UInt] = {
    //              lane3         lane2        lane1         lane0
    // original:    7   6    |    5   4   |    3   2    |    1   0
    //                       |            |             |
    // rearranged:  7   3    |    6   2   |    5   1    |    4   0
    require(in.head.getWidth == LaneWidth && in.length == NLanes)
    val out_by_lanes = Wire(Vec(NLanes, UInt(LaneWidth.W)))
    for (i <- 0 until NLanes) {
      val out_by_lanes_high_low = Wire(Vec(2, UInt(32.W)))  // widen & ext=2
      for (k <- 0 until 2) {
        out_by_lanes_high_low(k) := in(i/2 + k*NLanes/2)(32*(i%2)+31, 32*(i%2))
      }
      out_by_lanes(i) := out_by_lanes_high_low.asUInt
    }
    out_by_lanes
  }
  // Narrow rearrange for output
  def rearrg_narrow(lanesOut: Seq[UInt]): Vec[UInt] = {
    // Lane index:           3       2       1       0 (sew = destEew = 16)
    // Output of lanes:   FE76     DC54   BA32    9810
    // Rearranged vd:     FEDC     BA98   7654    3210
    val lanesOutRearrg = Wire(Vec(NLanes, UInt(LaneWidth.W)))
    for (i <- 0 until NLanes) {
      if (i < NLanes/2) {
        lanesOutRearrg(i) := Cat(lanesOut(2*i+1)(31, 0), lanesOut(2*i)(31, 0))
      } else {
        lanesOutRearrg(i) := Cat(lanesOut(2*i+1-NLanes)(63, 32), lanesOut(2*i-NLanes)(63, 32))
      }
    }
    lanesOutRearrg
  }
}