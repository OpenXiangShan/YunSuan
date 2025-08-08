/**
  * 13.2 vfadd vfsub vfrsub.vf
  * 13.3 vfwadd vfwsub vfwadd.w vfwsub.w
  * 13.11 vfmin vfmax
  * 13.12 vfsgnj vfsgnjn vfsgnjx
  * 13.13 vmfeq vmfne vmflt vmfle vmfgt vmfge
  * 13.16 vfmv
  */
//TODO: compare output valid only on uopEnd
//TODO: compare output 32b: last 2 bits   16b: last 4 bits (dirty code)
package race.vpu.exu.laneexu.fp

import chisel3._
import chisel3.util._
import race.vpu._
import VParams._
import race.vpu.exu.laneexu._

class LaneFAdd extends Module {
  val io = IO(new Bundle {
    val in = Input(ValidIO(new LaneInput))
    val sewIn = Input(new SewFpOH)
    val out = ValidIO(new LaneOutput)
  })

  val vfadd0 = Module(new FAdd_16_32(3, 3))
  val vfadd1 = Module(new FAdd_16_32(3, 3))

  val uop = io.in.bits.uop
  val (vs1, vs2, vs3) = (io.in.bits.vs1, io.in.bits.vs2, io.in.bits.vs3)
  val rs1 = Mux(io.sewIn.is16, Fill(4, io.in.bits.rs1(15, 0)), Fill(2, io.in.bits.rs1(31, 0)))
  val res_is_16b = io.sewIn.is16 && !uop.ctrl.widen2 && !uop.ctrl.widen
  
  // Widen case:
  //       32         32
  //    ---------  ---------
  //             |
  //             v   (mux by uopIdx(0))
  //         --------- 32
  //    |            |
  //    v            v
  //  ----   ----   ----   ----
  //   16     16     16     16
  def widen_sel(vs: UInt): UInt = {  // vs is 64 bits
    val (fadd1_high, fadd1_low, fadd0_high, fadd0_low) = (WireDefault(vs(63, 48)), vs(47, 32),
                                                          WireDefault(vs(31, 16)), vs(15, 0))
    val vs_32b = Mux(uop.uopIdx(0), vs(63, 32), vs(31, 0))
    when (uop.ctrl.widen) {
      fadd0_high := vs_32b(15, 0)
      fadd1_high := vs_32b(31, 16)
    }
    Cat(fadd1_high, fadd1_low, fadd0_high, fadd0_low)
  }
  
  Seq(vfadd0, vfadd1).foreach { vfadd =>
    vfadd.io.valid_in := io.in.valid
    vfadd.io.is_bf16 := io.sewIn.isBf16
    vfadd.io.is_fp16 := io.sewIn.isFp16
    vfadd.io.is_fp32 := io.sewIn.isFp32
    vfadd.io.is_widen := uop.ctrl.widen || uop.ctrl.widen2
    vfadd.io.a_already_widen := uop.ctrl.widen2
  }

  val vs1_32b, vs2_32b = Wire(Vec(2, UInt(32.W)))
  for (i <- 0 until 2) {
    vs2_32b(i) := Mux(uop.ctrl.widen2, vs2(i*32+31, i*32), widen_sel(vs2)(i*32+31, i*32))
    vs1_32b(i) := Mux(uop.ctrl.vx, rs1, widen_sel(vs1)(i*32+31, i*32))
  }

  val funct6 = uop.ctrl.funct6
  val is16In = io.sewIn.is16
  val isAdd = funct6 === 0.U || funct6(5, 4) === 3.U && !funct6(1)
  val isMinMax = funct6(5, 2) === 1.U
  val isCmp = funct6(5, 3) === 3.U
  val isSub = isMinMax || isCmp ||
              funct6(5, 4) === 0.U && funct6(1) || funct6(5, 4) === 3.U && funct6(1)
  val isRSub = funct6 === "b100111".U
  val isSgn = funct6(5, 3) === 1.U
  val isMove = funct6(5, 2) === "b0101".U && uop.ctrl.vm

  Seq(vfadd0, vfadd1).zipWithIndex.foreach { case (vfadd, i) =>
    vfadd.io.b := inv(vs1_32b(i), isSub, io.sewIn.is16)
    vfadd.io.a := inv(vs2_32b(i), isRSub, io.sewIn.is16)
  }

  val faddOut_32b = Seq(vfadd0.io.res, vfadd1.io.res)
  val faddOut_16b = Seq(faddOut_32b(0)(15, 0), faddOut_32b(0)(31, 16), faddOut_32b(1)(15, 0), faddOut_32b(1)(31, 16))
  //  3,2,1,0 -> 63 47 31 15
  val cmp_gte = Seq(faddOut_16b(0)(15), faddOut_16b(1)(15), faddOut_16b(2)(15), faddOut_16b(3)(15))

  // Output
  val out_valid = vfadd0.io.valid_out
  val out_bits = Wire(new LaneOutput)
  out_bits.fflags := VecInit(Seq.fill(LaneWidth/16)(0.U(5.W)))
  out_bits.uop := RegEnable(RegEnable(uop, io.in.valid), vfadd0.io.valid_S1)
  val funct6_S2 = out_bits.uop.ctrl.funct6
  val vs1_S2 = RegEnable(RegEnable(vs1, io.in.valid), vfadd0.io.valid_S1)
  val vs2_S2 = RegEnable(RegEnable(vs2, io.in.valid), vfadd0.io.valid_S1)
  val rs1_S2 = RegEnable(RegEnable(rs1, io.in.valid), vfadd0.io.valid_S1)
  val vs1_S2_16b = UIntSplit(vs1_S2, 16)
  val vs2_S2_16b = UIntSplit(vs2_S2, 16)
  val res_is_16b_S2 = RegEnable(RegEnable(res_is_16b, io.in.valid), vfadd0.io.valid_S1)
  val isMinMax_S2 = RegEnable(RegEnable(isMinMax, io.in.valid), vfadd0.io.valid_S1)
  val isSgn_S2 = RegEnable(RegEnable(isSgn, io.in.valid), vfadd0.io.valid_S1)
  val isCmp_S2 = RegEnable(RegEnable(isCmp, io.in.valid), vfadd0.io.valid_S1)
  val isMove_S2 = RegEnable(RegEnable(isMove, io.in.valid), vfadd0.io.valid_S1)

  val vd_minmax, vd_sgn = Wire(Vec(4, UInt(16.W)))
  val isMax_S2 = isMinMax_S2 && funct6_S2(1)
  val isMin_S2 = isMinMax_S2 && !funct6_S2(1)
  for (i <- 0 until 4) {
    if (i % 2 == 1) {
      vd_minmax(i) :=  Mux(cmp_gte(i) && isMax_S2 || !cmp_gte(i) && isMin_S2,
                           vs1_S2_16b(i), vs2_S2_16b(i))
      vd_sgn(i) := Mux(funct6_S2(1, 0) === 0.U, vs2_S2_16b(i)(15),
                   Mux(funct6_S2(1, 0) === 1.U, !vs2_S2_16b(i)(15),
                       vs1_S2_16b(i)(15) ^ vs2_S2_16b(i)(15))) ## vs1_S2_16b(i)(14, 0)
    } else {
      when (res_is_16b_S2) {
        vd_minmax(i) :=  Mux(cmp_gte(i) && isMax_S2 || !cmp_gte(i) && isMin_S2,
                             vs1_S2_16b(i), vs2_S2_16b(i))
        vd_sgn(i) := Mux(funct6_S2(1, 0) === 0.U, vs2_S2_16b(i)(15),
                     Mux(funct6_S2(1, 0) === 1.U, !vs2_S2_16b(i)(15),
                         vs1_S2_16b(i)(15) ^ vs2_S2_16b(i)(15))) ## vs1_S2_16b(i)(14, 0)
      }.otherwise {
        vd_minmax(i) := Mux(cmp_gte(i+1) && isMax_S2 || !cmp_gte(i+1) && isMin_S2,
                            vs1_S2_16b(i), vs2_S2_16b(i))
        vd_sgn(i) := vs1_S2_16b(i)
      }
    }
  }

  // Compare output
  val cmp_eq_32b = faddOut_32b.map(x => x.tail(1) === 0.U)
  val cmp_eq_16b = faddOut_16b.map(x => x.tail(1) === 0.U)
  val cmp_ge_32b = Seq(cmp_gte(1), cmp_gte(3))
  val cmp_ge_16b = cmp_gte
  val cmp_ne_32b = cmp_eq_32b.map(!_)
  val cmp_ne_16b = cmp_eq_16b.map(!_)
  val cmp_lt_32b = cmp_ge_32b zip cmp_ne_32b map { case (ge, ne) => !ge && ne }
  val cmp_lt_16b = cmp_ge_16b zip cmp_ne_16b map { case (ge, ne) => !ge && ne }
  val cmp_le_32b = cmp_ge_32b zip cmp_eq_32b map { case (ge, eq) => !ge || eq }
  val cmp_le_16b = cmp_ge_16b zip cmp_eq_16b map { case (ge, eq) => !ge || eq }
  val cmp_gt_32b = cmp_ge_32b zip cmp_ne_32b map { case (ge, ne) => ge && ne }
  val cmp_gt_16b = cmp_ge_16b zip cmp_ne_16b map { case (ge, ne) => ge && ne }

  val (eq, le, lt, ne, gt, ge) = (funct6(2, 0) === 0.U, funct6(2, 0) === 1.U, funct6(2, 0) === 3.U,
                                  funct6(2, 0) === 4.U, funct6(2, 0) === 5.U, funct6(2, 0) === 7.U)
  val vd_cmp_32b = Mux1H(Seq(
    eq -> VecInit(cmp_eq_32b).asUInt,
    le -> VecInit(cmp_le_32b).asUInt,
    lt -> VecInit(cmp_lt_32b).asUInt,
    ne -> VecInit(cmp_ne_32b).asUInt,
    gt -> VecInit(cmp_gt_32b).asUInt,
    ge -> VecInit(cmp_ge_32b).asUInt
  ))
  val vd_cmp_16b = Mux1H(Seq(
    eq -> VecInit(cmp_eq_16b).asUInt,
    le -> VecInit(cmp_le_16b).asUInt,
    lt -> VecInit(cmp_lt_16b).asUInt,
    ne -> VecInit(cmp_ne_16b).asUInt,
    gt -> VecInit(cmp_gt_16b).asUInt,
    ge -> VecInit(cmp_ge_16b).asUInt
  ))
  val vd_cmp = Mux(res_is_16b_S2, vd_cmp_16b.pad(LaneWidth), vd_cmp_32b.pad(LaneWidth))

  out_bits.vd := MuxCase(Cat(vfadd1.io.res, vfadd0.io.res), Seq(
    isMinMax_S2 -> vd_minmax.asUInt,
    isSgn_S2 -> vd_sgn.asUInt,
    isMove_S2 -> rs1_S2,
    isCmp_S2 -> vd_cmp
  ))

  /**
    *  Put a register on the output of FAdd_16_32, since the FAdd_16_32 output rounding has some dealy of combinational logic
    */
  io.out.valid := RegNext(out_valid)
  io.out.bits := RegEnable(out_bits, out_valid)
  
  def inv(fp: UInt, inv_bit: Bool): UInt = {
    Cat(inv_bit ^ fp(fp.getWidth - 1), fp(fp.getWidth - 2, 0))
  }
  def inv(fp32: UInt, inv_bit: Bool, is_16: Bool): UInt = {
    Mux(is_16, 
        Cat(inv(fp32(31, 16), inv_bit), inv(fp32(15, 0), inv_bit)),
        inv(fp32, inv_bit))
  }
}

// object LaneFAdd extends App {
//   println("Generating the LaneFAdd hardware")
//   emitVerilog(new LaneFAdd(), Array("--target-dir", "build/verilog_lane_fadd"))
// }