package yunsuan.vector.vfred

import chisel3._
import chisel3.util._
import yunsuan.vector.{ShiftLeftPriorityWithF32EXPResult, ShiftLeftPriorityWithF64EXPResult}
import yunsuan.vector.Common._

class VFInfo extends Bundle {
  val vm = Bool()
  val ma = Bool()
  val ta = Bool()
  val vlmul = UInt(3.W)
  val vl = UInt(8.W)
  val vstart = UInt(7.W)
  val rm = Frm()
}

class VFRedInput extends Bundle {
  val info = new VFInfo()
  val vsew = VSew()
  val vs1 = UInt(128.W)
  val vs2 = UInt(128.W)
  val old_vd = UInt(128.W)
  val mask = UInt(128.W)
  val widen = Bool() // for sum
  val isMax = Bool() // for comp
}

class VFOutput extends Bundle {
  val vd = UInt(128.W)
  val fflags = UInt(5.W)
  val vxsat = Bool()
}

// TODO: need vl, vstart logic
// FIXME: considering vstart, and vm
class VFRedComp extends Module {
  val VLEN = 128
  val io = IO(new Bundle() {
    val in = Flipped(ValidIO(new VFRedInput))
    val out = ValidIO(new VFOutput)
  })
  val in_valid = io.in.valid

  val vsew = io.in.bits.vsew
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.old_vd
  val mask = io.in.bits.mask
  val isMax = io.in.bits.isMax
  val vm = io.in.bits.info.vm
  val ta = io.in.bits.info.ta
  val vl = io.in.bits.info.vl

  def IsActive(i: Int): Bool = {
    (vm || mask(i).asBool) && (i.U < vl)
  }

  // vector f64
  val scalar64 = vs1(63, 0)
  val vec64 = vs2.asTypeOf(Vec(2, UInt(64.W)))
  val f64_0_0 = Module(new fcomp_3to1(64))
  f64_0_0.io.fpA := vec64(0)
  f64_0_0.io.fpB := vec64(1)
  f64_0_0.io.fpC := scalar64
  f64_0_0.io.activeMask := Cat(true.B, IsActive(1), IsActive(0))
  f64_0_0.io.isMax := isMax
  val f64_res = f64_0_0.io.fpD
  val f64_flags = f64_0_0.io.outFlags

  // vector f32
  val scalar32 = vs1(31, 0)
  val vec32 = vs2.asTypeOf(Vec(4, UInt(32.W)))
  val f32_0_0 = Module(new fcomp_3to1(32))
  f32_0_0.io.fpA := vec32(0)
  f32_0_0.io.fpB := vec32(1)
  f32_0_0.io.fpC := scalar32
  f32_0_0.io.activeMask := Cat(true.B, IsActive(1), IsActive(0))
  f32_0_0.io.isMax := isMax
  val f32_0_1 = Module(new fcomp_2to1(32))
  f32_0_1.io.fpA := vec32(2)
  f32_0_1.io.fpB := vec32(3)
  f32_0_1.io.activeMask := Cat(IsActive(3), IsActive(2))
  f32_0_1.io.isMax := isMax
  val f32_1_0 = Module(new fcomp_2to1(32))
  f32_1_0.io.fpA := f32_0_0.io.fpD
  f32_1_0.io.fpB := f32_0_1.io.fpC
  f32_1_0.io.activeMask := Cat(f32_0_1.io.outActive, f32_0_0.io.outActive)
  f32_1_0.io.isMax := isMax
  val f32_res = f32_1_0.io.fpC
  val f32_flags = f32_0_0.io.outFlags | f32_0_1.io.outFlags | f32_1_0.io.outFlags

  // vector f16
  val scalar16 = vs1(15, 0)
  val vec16 = vs2.asTypeOf(Vec(8, UInt(16.W)))
  val f16_0_0 = Module(new fcomp_3to1(16))
  f16_0_0.io.fpA := vec16(0)
  f16_0_0.io.fpB := vec16(1)
  f16_0_0.io.fpC := scalar16
  f16_0_0.io.activeMask := Cat(true.B, IsActive(1), IsActive(0))
  f16_0_0.io.isMax := isMax
  val f16_0_1 = Module(new fcomp_3to1(16))
  f16_0_1.io.fpA := vec16(2)
  f16_0_1.io.fpB := vec16(3)
  f16_0_1.io.fpC := vec16(4)
  f16_0_1.io.activeMask := Cat(IsActive(4), IsActive(3), IsActive(2))
  f16_0_1.io.isMax := isMax
  val f16_0_2 = Module(new fcomp_3to1(16))
  f16_0_2.io.fpA := vec16(5)
  f16_0_2.io.fpB := vec16(6)
  f16_0_2.io.fpC := vec16(7)
  f16_0_2.io.activeMask := Cat(IsActive(7), IsActive(6), IsActive(5))
  f16_0_2.io.isMax := isMax
  val f16_1_0 = Module(new fcomp_3to1(16))
  f16_1_0.io.fpA := f16_0_0.io.fpD
  f16_1_0.io.fpB := f16_0_1.io.fpD
  f16_1_0.io.fpC := f16_0_2.io.fpD
  f16_1_0.io.activeMask := Cat(f16_0_2.io.outActive, f16_0_1.io.outActive, f16_0_0.io.outActive)
  f16_1_0.io.isMax := isMax
  val f16_res = f16_1_0.io.fpD
  val f16_flags = f16_0_0.io.outFlags | f16_0_1.io.outFlags | f16_0_2.io.outFlags | f16_1_0.io.outFlags

  val old_vd_masked = old_vd | Fill(VLEN, ta)

  val in_valid_r = RegNext(in_valid)

  val active_s1_r = RegEnable(Cat((0 until 8).map(i => IsActive(i)).reverse), in_valid) // for lmul = 1, only use 8 bit
  val vs1_s1_r = RegEnable(vs1, in_valid)
  val vsew_s1_r = RegEnable(vsew, in_valid)
  val old_vd_masked_s1_r = RegEnable(old_vd_masked, in_valid)
  val f64_res_s1_r = RegEnable(f64_res, in_valid)
  val f32_res_s1_r = RegEnable(f32_res, in_valid)
  val f16_res_s1_r = RegEnable(f16_res, in_valid)
  val f64_flags_s1_r = RegEnable(f64_flags, in_valid)
  val f32_flags_s1_r = RegEnable(f32_flags, in_valid)
  val f16_flags_s1_r = RegEnable(f16_flags, in_valid)

  val vd_val = Wire(UInt(128.W)) // copy vs1 when there's no active element.
  vd_val := MuxLookup(vsew_s1_r, 0.U)(Seq(
    VSew.e16 -> Cat(Fill(VLEN - 16, 1.U) & old_vd_masked_s1_r(VLEN - 1, 16), Mux(active_s1_r.orR, f16_res_s1_r, vs1_s1_r(15, 0))),
    VSew.e32 -> Cat(Fill(VLEN - 32, 1.U) & old_vd_masked_s1_r(VLEN - 1, 32), Mux(active_s1_r.orR, f32_res_s1_r, vs1_s1_r(31, 0))),
    VSew.e64 -> Cat(Fill(VLEN - 64, 1.U) & old_vd_masked_s1_r(VLEN - 1, 64), Mux(active_s1_r.orR, f64_res_s1_r, vs1_s1_r(63, 0))),
  ))
  val outFlags = MuxLookup(vsew_s1_r, 0.U)(Seq(
    VSew.e16 -> f16_flags_s1_r,
    VSew.e32 -> f32_flags_s1_r,
    VSew.e64 -> f64_flags_s1_r,
  ))

  io.out.valid := in_valid_r
  io.out.bits.vd := vd_val
  io.out.bits.fflags := Mux(active_s1_r.orR, outFlags, 0.U) // do not set any exception flags when there's no active element.
  io.out.bits.vxsat := false.B
}

class fcomp_2to1(val totalWidth: Int) extends Module with FloatParams {
  val io = IO(new Bundle() {
    val fpA = Input(UInt(totalWidth.W))
    val fpB = Input(UInt(totalWidth.W))
    val activeMask = Input(UInt(2.W)) // 1-bit: B active, 0-bit: A active
    val isMax = Input(Bool())
    val fpC = Output(UInt(totalWidth.W))
    val outActive = Output(Bool())
    val outFlags = Output(UInt(5.W))
  })
  val fpA = io.fpA
  val fpB = io.fpB
  val fpC = io.fpC
  val activeMask = io.activeMask
  val isMax = io.isMax

  val signA = fpA.head(1).asBool
  val signB = fpB.head(1).asBool
  val magA = fpA.tail(1) // magnitude = exponent + fraction
  val magB = fpB.tail(1)
  val expA = magA.head(exponentWidth)
  val expB = magB.head(exponentWidth)
  val fracA = magA.tail(exponentWidth)
  val fracB = magB.tail(exponentWidth)

  val expAIsZero = !expA.orR
  val expBIsZero = !expB.orR
  val expAIsAllOne = expA.andR
  val expBIsAllOne = expB.andR
  val fracAIsZero = !fracA.orR
  val fracBIsZero = !fracB.orR

  val fpAIsActive = activeMask(0)
  val fpBIsActive = activeMask(1)

  val quietBitA = fracA.head(1).asBool
  val quietBitB = fracB.head(1).asBool

  val magAGB = magA > magB
  val diffSignAB = signA ^ signB

  // Same rule as fcomp_3to1: when signs differ, min picks the negative
  // (signA=1) and max picks the positive (signA=0) → selA = signA ^ isMax.
  // Using signB here inverted mixed-sign min (vfredmin e32: vs2 had a
  // negative and a tiny positive; DUT wrote the unsigned-min tiny value).
  val selAFromAB = Mux(diffSignAB, signA ^ isMax, Mux(signA ^ isMax, magAGB, !magAGB))

  val fpAIsNaN = expAIsAllOne & !fracAIsZero
  val fpBIsNaN = expBIsAllOne & !fracBIsZero

  val fpAInvalid = fpAIsNaN | !activeMask(0)
  val fpBInvalid = fpBIsNaN | !activeMask(1)

  io.fpC := cNaN.U
  // when there's only one active element, fpD should be the active one even it is a qNaN
  when((!fpAInvalid & fpBInvalid) | (!fpAInvalid & selAFromAB)) {
    io.fpC := fpA
  }.elsewhen((fpAInvalid & !fpBInvalid) | (!fpBInvalid & !selAFromAB)) {
    io.fpC := fpB
  }

  io.outActive := activeMask.orR

  val flagsNV = (!quietBitA && fpAIsNaN && activeMask(0).asBool) || (!quietBitB && fpBIsNaN && activeMask(1).asBool)
  val flagsDZ = false.B
  val flagsOF = false.B
  val flagsUF = false.B
  val flagsNX = false.B
  io.outFlags := Cat(flagsNV, flagsDZ, flagsOF, flagsUF, flagsNX)
}

class fcomp_3to1(val totalWidth: Int) extends Module with FloatParams {
  val io = IO(new Bundle() {
    val fpA = Input(UInt(totalWidth.W))
    val fpB = Input(UInt(totalWidth.W))
    val fpC = Input(UInt(totalWidth.W))
    val activeMask = Input(UInt(3.W)) // 2-bit: C active, 1-bit: B active, 0-bit: A active
    val isMax = Input(Bool())
    val fpD = Output(UInt(totalWidth.W))
    val outActive = Output(Bool())
    val outFlags = Output(UInt(5.W))
  })
  val fpA = io.fpA
  val fpB = io.fpB
  val fpC = io.fpC
  val activeMask = io.activeMask
  val isMax = io.isMax

  val signA = fpA.head(1).asBool
  val signB = fpB.head(1).asBool
  val signC = fpC.head(1).asBool
  val magA = fpA.tail(1) // magnitude = exponent + fraction
  val magB = fpB.tail(1)
  val magC = fpC.tail(1)
  val expA = magA.head(exponentWidth)
  val expB = magB.head(exponentWidth)
  val expC = magC.head(exponentWidth)
  val fracA = magA.tail(exponentWidth)
  val fracB = magB.tail(exponentWidth)
  val fracC = magC.tail(exponentWidth)

  val expAIsZero = !expA.orR
  val expBIsZero = !expB.orR
  val expCIsZero = !expC.orR
  val expAIsAllOne = expA.andR
  val expBIsAllOne = expB.andR
  val expCIsAllOne = expC.andR
  val fracAIsZero = !fracA.orR
  val fracBIsZero = !fracB.orR
  val fracCIsZero = !fracC.orR

  val fpAIsActive = activeMask(0)
  val fpBIsActive = activeMask(1)
  val fpCIsActive = activeMask(2)

  val quietBitA = fracA.head(1).asBool
  val quietBitB = fracB.head(1).asBool
  val quietBitC = fracC.head(1).asBool

  val magAGB = magA > magB
  val magAGC = magA > magC
  val magBGC = magB > magC
  val diffSignAB = signA ^ signB
  val diffSignAC = signA ^ signC
  val diffSignBC = signB ^ signC

  val selAFromAB = Mux(diffSignAB, signA ^ isMax, Mux(signA ^ isMax, magAGB, !magAGB))
  val selAFromAC = Mux(diffSignAC, signA ^ isMax, Mux(signA ^ isMax, magAGC, !magAGC))
  val selBFromBC = Mux(diffSignBC, signB ^ isMax, Mux(signB ^ isMax, magBGC, !magBGC))

  val fpAIsNaN = expAIsAllOne & !fracAIsZero
  val fpBIsNaN = expBIsAllOne & !fracBIsZero
  val fpCIsNaN = expCIsAllOne & !fracCIsZero

  val fpAInvalid = fpAIsNaN | !activeMask(0)
  val fpBInvalid = fpBIsNaN | !activeMask(1)
  val fpCInvalid = fpCIsNaN | !activeMask(2)

  io.fpD := cNaN.U
  when((!fpAInvalid & fpBInvalid & fpCInvalid) | (!fpAInvalid & fpBInvalid & selAFromAC) | (!fpAInvalid & fpCInvalid & selAFromAB) | (!fpAInvalid & selAFromAB & selAFromAC)) {
    io.fpD := fpA
  }.elsewhen((fpAInvalid & !fpBInvalid & fpCInvalid) | (fpAInvalid & !fpBInvalid & selBFromBC) | (!fpBInvalid & fpCInvalid & !selAFromAB) | (!fpBInvalid & !selAFromAB & selBFromBC)) {
    io.fpD := fpB
  }.elsewhen((fpAInvalid & fpBInvalid & !fpCInvalid) | (fpAInvalid & !fpCInvalid & !selBFromBC) | (fpBInvalid & !fpCInvalid & !selAFromAC) | (!fpCInvalid & !selAFromAC & !selBFromBC)) {
    io.fpD := fpC
  }

  io.outActive := activeMask.orR

  val flagsNV = (!quietBitA && fpAIsNaN && activeMask(0).asBool) || (!quietBitB && fpBIsNaN && activeMask(1).asBool) || (!quietBitC && fpCIsNaN && activeMask(2).asBool)
  val flagsDZ = false.B
  val flagsOF = false.B
  val flagsUF = false.B
  val flagsNX = false.B
  io.outFlags := Cat(flagsNV, flagsDZ, flagsOF, flagsUF, flagsNX)
}

class VFRedSUM() extends Module {
  val VLEN = 128
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val isorder = Input(Bool())
    val in = Flipped(DecoupledIO(new VFRedInput))
    val out = DecoupledIO(new VFOutput)
  })

  val isorder_r = RegEnable(io.isorder, io.in.fire)

  val usum = Module(new VFRedUSum())
  val osum = Module(new VFRedOSum())
  val in_ready = io.in.ready
  in_ready := usum.io.in.ready && osum.io.in.ready
  usum.io.flush := io.flush
  usum.io.in.bits := io.in.bits
  usum.io.in.valid := io.in.valid && !io.isorder
  usum.io.out.ready := io.out.ready
  osum.io.flush := io.flush
  osum.io.in.bits := io.in.bits
  osum.io.in.valid := io.in.valid && io.isorder
  osum.io.out.ready := io.out.ready

  io.out.valid := Mux(isorder_r, osum.io.out.valid, usum.io.out.valid)
  io.out.bits := Mux(isorder_r, osum.io.out.bits, usum.io.out.bits)
}
// for every operation, valid only lasts for one cycle
// do not gen exceptions in widen stage
class VFRedUSum() extends Module {
  val VLEN = 128
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(DecoupledIO(new VFRedInput))
    val out = DecoupledIO(new VFOutput)
  })

  val flush = io.flush

  val vsew = io.in.bits.vsew
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.old_vd
  val mask = io.in.bits.mask
  val widen = io.in.bits.widen
  val vm = io.in.bits.info.vm
  val ta = io.in.bits.info.ta
  val vl = io.in.bits.info.vl
  val rm = io.in.bits.info.rm

  def IsActive(i: Int): Bool = {
    mask(i).asBool && (i.U < vl)
  }
  def SrcWiden(in: UInt): UInt = {
    assert(in.getWidth == 16 || in.getWidth == 32)

    val w = in.getWidth
    val exp_w = if (w == 16) 5 else 8
    val sig_w = if (w == 16) 11 else 24
    val dest_exp_w = if (w == 16) 8 else 11
    val dest_sig_w = if (w == 16) 24 else 53

    val sign = in(w - 1)
    val exp = in(w - 2, sig_w - 1)
    val frac = in(sig_w - 2, 0) // source fraction, width = sig_w - 1

    val expIsZero = !exp.orR
    val expIsAllOne = exp.andR
    val fracIsZero = !frac.orR

    val isZero = expIsZero && fracIsZero
    val isSubnormal = expIsZero && !fracIsZero
    val isInf = expIsAllOne && fracIsZero
    val isNaN = expIsAllOne && !fracIsZero
    val isNormal = !expIsZero && !expIsAllOne

    val lshift = Wire(UInt((sig_w - 1).W))
    val subExp = Wire(UInt(dest_exp_w.W))

    if (w == 16) {
      val u = Module(new ShiftLeftPriorityWithF32EXPResult(
        srcW = sig_w - 1, priorityShiftValueW = sig_w - 1, expW = dest_exp_w))
      u.io.src := frac
      u.io.priority_shiftValue := frac
      lshift := u.io.lshift_result
      subExp := u.io.exp_result
    } else {
      val u = Module(new ShiftLeftPriorityWithF64EXPResult(
        srcW = sig_w - 1, priorityShiftValueW = sig_w - 1, expW = dest_exp_w))
      u.io.src := frac
      u.io.priority_shiftValue := frac
      lshift := u.io.lshift_result
      subExp := u.io.exp_result
    }

    // normal: bias shift (keep original logic)
    val normalExp = {
      val const1 = "b1000".U
      val const0 = "b0111".U
      Mux(exp(exp_w - 1), Cat(const1, exp(exp_w - 2, 0)), Cat(const0, exp(exp_w - 2, 0)))
    }
    val normalMant = Cat(frac, 0.U((dest_sig_w - sig_w).W))

    // subnormal: drop implicit bit after left-normalization, then pad
    val subMant = Cat(Cat(lshift.tail(1), 0.U(1.W)), 0.U((dest_sig_w - sig_w).W))

    // inf/zero
    val zeroExp = 0.U(dest_exp_w.W)
    val zeroMant = 0.U((dest_sig_w - 1).W)
    val infExp = Fill(dest_exp_w, 1.U(1.W))
    val infMant = 0.U((dest_sig_w - 1).W)

    // NaN: keep sNaN/qNaN distinction and move payload
    // quiet bit is the fraction MSB
    val srcQuietBit = frac(sig_w - 2) // 1=qNaN, 0=sNaN
    val srcPayload = frac(sig_w - 3, 0) // payload without the quiet bit

    val dstPayloadW = dest_sig_w - 2
    val srcPayloadW = sig_w - 2
    val dstPayload = Cat(srcPayload, 0.U((dstPayloadW - srcPayloadW).W))
    val nanMant = Cat(srcQuietBit, dstPayload) // keep the s/q classification

    val outExp = MuxCase(0.U(dest_exp_w.W), Seq(
      isZero -> zeroExp,
      isSubnormal -> subExp,
      isNormal -> normalExp,
      isInf -> infExp,
      isNaN -> infExp
    ))

    val outMant = MuxCase(0.U((dest_sig_w - 1).W), Seq(
      isZero -> zeroMant,
      isSubnormal -> subMant,
      isNormal -> normalMant,
      isInf -> infMant,
      isNaN -> nanMant
    ))

    Cat(sign, outExp, outMant)
  }

  val widen_vector = Wire(UInt((2 * VLEN).W))
  widen_vector := Mux(
    vsew === VSew.e16,
    Cat(vs2.asTypeOf(Vec(8, UInt(16.W))).map(x => SrcWiden(x)).reverse),
    Cat(vs2.asTypeOf(Vec(4, UInt(32.W))).map(x => SrcWiden(x)).reverse)
  )

  // TODO:
  val active_mask = Cat(mask.asBools.zipWithIndex.map{case (m, i) => (vm || m) && (i.U < vl)}.reverse)

  val vsew_src = Mux(widen, Mux(vsew === VSew.e16, VSew.e32, VSew.e64), vsew)
  val vsew_src_r = RegEnable(vsew_src, io.in.fire)
  val vsew_r = RegEnable(vsew, io.in.fire)
  val widen_r = RegEnable(widen, io.in.fire)
  val old_vd_r = RegEnable(old_vd, io.in.fire)
  val vl_r = RegEnable(vl, io.in.fire)
  val ta_r = RegEnable(ta, io.in.fire)
  val vm_r = RegEnable(vm, io.in.fire)
  val rm_r = RegEnable(rm, io.in.fire)
  val vs1_r = RegEnable(vs1, io.in.fire)
  val vs2_r = RegEnable(vs2, io.in.fire)
  val active_mask_r = RegEnable(active_mask, io.in.fire)

  val FSM_W = 3
  val FSM_IDLE = (1 << 0).U(FSM_W.W)
  val FSM_SUM = (1 << 1).U(FSM_W.W)
  val FSM_OUT = (1 << 2).U(FSM_W.W)
  val state_r = RegInit(FSM_IDLE)
  val state_next = Wire(UInt(FSM_W.W))

  val LastStageFFlags = Wire(UInt(5.W))
  val LastStageActive = Wire(UInt(8.W))
  val LastStageRes = Wire(UInt(128.W))
  val FFlags_r = Reg(UInt(5.W))
  val Active_r = Reg(UInt(8.W))
  val AdderSrc_r = Reg(UInt(256.W))
  val ValidCnt = RegInit(0.U(4.W))
  val finalSumCycle = state_r === FSM_SUM && ValidCnt === 1.U

  state_r := state_next
  when(flush) {
    state_next := FSM_IDLE
  }.elsewhen(state_r === FSM_IDLE && io.in.fire) {
    state_next := FSM_SUM
  }.elsewhen(finalSumCycle) {
    state_next := Mux(io.out.ready, FSM_IDLE, FSM_OUT)
  }.elsewhen(state_r === FSM_OUT && io.out.fire) {
    state_next := FSM_IDLE
  }.otherwise {
    state_next := state_r
  }

  when(state_r === FSM_SUM) {
    ValidCnt := ValidCnt - 1.U
    when(ValidCnt(0) === 1.U) {
      FFlags_r := FFlags_r | LastStageFFlags
      when(ValidCnt === 3.U) {
        Active_r := Cat(true.B, LastStageActive(0))
        AdderSrc_r := MuxLookup(vsew_src_r, 0.U)(Seq(
          VSew.e16 -> Cat(0.U(112.W), vs1_r(15, 0), LastStageRes(15, 0)),
          VSew.e32 -> Cat(0.U(96.W), vs1_r(31, 0), LastStageRes(31, 0)),
          VSew.e64 -> Cat(0.U(64.W), vs1_r(63, 0), LastStageRes(63, 0)),
        ))
      }.otherwise {
        Active_r := LastStageActive
        AdderSrc_r := Cat(0.U(128.W), LastStageRes)
      }
    }
  }.otherwise {
    FFlags_r := 0.U
    Active_r := active_mask
    AdderSrc_r := Mux(widen, widen_vector, Cat(0.U(128.W), vs2))
    when(vsew === VSew.e64) {
      ValidCnt := 4.U
    }.elsewhen(vsew === VSew.e32) {
      ValidCnt := 6.U
    }.elsewhen(vsew === VSew.e16) {
      ValidCnt := 8.U
    }.otherwise {
      ValidCnt := 8.U
    }
  }

  // f64
  val f64_0 = Module(new FAdder_w(64))
  val f64_1 = Module(new FAdder_w(64))
  val f64_outActive = Wire(UInt(2.W))
  val f64_outFlags = Wire(UInt(5.W))
  val f64_outRes = Wire(UInt(128.W))

  f64_0.io.fpA := AdderSrc_r(63, 0)
  f64_0.io.fpB := AdderSrc_r(127, 64)
  f64_0.io.mask := Active_r(1, 0)
  f64_0.io.rm := rm_r
  f64_1.io.fpA := AdderSrc_r(191, 128)
  f64_1.io.fpB := AdderSrc_r(255, 192)
  f64_1.io.mask := Active_r(3, 2)
  f64_1.io.rm := rm_r

  f64_outActive := Cat(f64_1.io.outActive, f64_0.io.outActive)
  f64_outFlags := f64_0.io.outFlags | f64_1.io.outFlags
  f64_outRes := Cat(f64_1.io.outRes, f64_0.io.outRes)

  // f32
  val f32_0 = Module(new FAdder_w(32))
  val f32_1 = Module(new FAdder_w(32))
  val f32_2 = Module(new FAdder_w(32))
  val f32_3 = Module(new FAdder_w(32))
  val f32_outActive = Wire(UInt(4.W))
  val f32_outFlags = Wire(UInt(5.W))
  val f32_outRes = Wire(UInt(128.W))
  f32_0.io.fpA := AdderSrc_r(31, 0)
  f32_0.io.fpB := AdderSrc_r(63, 32)
  f32_0.io.mask := Active_r(1, 0)
  f32_0.io.rm := rm_r
  f32_1.io.fpA := AdderSrc_r(95, 64)
  f32_1.io.fpB := AdderSrc_r(127, 96)
  f32_1.io.mask := Active_r(3, 2)
  f32_1.io.rm := rm_r
  f32_2.io.fpA := AdderSrc_r(159, 128)
  f32_2.io.fpB := AdderSrc_r(191, 160)
  f32_2.io.mask := Active_r(5, 4)
  f32_2.io.rm := rm_r
  f32_3.io.fpA := AdderSrc_r(223, 192)
  f32_3.io.fpB := AdderSrc_r(255, 224)
  f32_3.io.mask := Active_r(7, 6)
  f32_3.io.rm := rm_r

  f32_outActive := Cat(f32_3.io.outActive, f32_2.io.outActive, f32_1.io.outActive, f32_0.io.outActive)
  f32_outFlags := f32_0.io.outFlags | f32_1.io.outFlags | f32_2.io.outFlags | f32_3.io.outFlags
  f32_outRes := Cat(f32_3.io.outRes, f32_2.io.outRes, f32_1.io.outRes, f32_0.io.outRes)

  // f16
  val f16_0 = Module(new FAdder_w(16))
  val f16_1 = Module(new FAdder_w(16))
  val f16_2 = Module(new FAdder_w(16))
  val f16_3 = Module(new FAdder_w(16))
  val f16_outActive = Wire(UInt(4.W))
  val f16_outFlags = Wire(UInt(5.W))
  val f16_outRes = Wire(UInt(128.W))
  f16_0.io.fpA := AdderSrc_r(15, 0)
  f16_0.io.fpB := AdderSrc_r(31, 16)
  f16_0.io.mask := Active_r(1, 0)
  f16_0.io.rm := rm_r
  f16_1.io.fpA := AdderSrc_r(47, 32)
  f16_1.io.fpB := AdderSrc_r(63, 48)
  f16_1.io.mask := Active_r(3, 2)
  f16_1.io.rm := rm_r
  f16_2.io.fpA := AdderSrc_r(79, 64)
  f16_2.io.fpB := AdderSrc_r(95, 80)
  f16_2.io.mask := Active_r(5, 4)
  f16_2.io.rm := rm_r
  f16_3.io.fpA := AdderSrc_r(111, 96)
  f16_3.io.fpB := AdderSrc_r(127, 112)
  f16_3.io.mask := Active_r(7, 6)
  f16_3.io.rm := rm_r

  f16_outActive := Cat(f16_3.io.outActive, f16_2.io.outActive, f16_1.io.outActive, f16_0.io.outActive)
  f16_outFlags := f16_0.io.outFlags | f16_1.io.outFlags | f16_2.io.outFlags | f16_3.io.outFlags
  f16_outRes := Cat(f16_3.io.outRes, f16_2.io.outRes, f16_1.io.outRes, f16_0.io.outRes)

  LastStageActive := MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> f16_outActive,
    VSew.e32 -> f32_outActive,
    VSew.e64 -> f64_outActive,
  ))
  LastStageFFlags := MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> f16_outFlags,
    VSew.e32 -> f32_outFlags,
    VSew.e64 -> f64_outFlags,
  ))
  LastStageRes := MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> f16_outRes,
    VSew.e32 -> f32_outRes,
    VSew.e64 -> f64_outRes,
  ))

  val result_16 = f16_0.io.outRes
  val result_32 = f32_0.io.outRes
  val result_64 = f64_0.io.outRes
  val result_fflags = FFlags_r | LastStageFFlags
  val old_vd_masked = old_vd_r | Fill(VLEN, ta_r)
  val vd_val = MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> Cat(Fill(VLEN - 16, 1.U) & old_vd_masked(VLEN - 1, 16), Mux(active_mask_r(7, 0).orR, result_16, vs1_r(15, 0))),
    VSew.e32 -> Cat(Fill(VLEN - 32, 1.U) & old_vd_masked(VLEN - 1, 32), Mux(active_mask_r(7, 0).orR, result_32, vs1_r(31, 0))),
    VSew.e64 -> Cat(Fill(VLEN - 64, 1.U) & old_vd_masked(VLEN - 1, 64), Mux(active_mask_r(7, 0).orR, result_64, vs1_r(63, 0))),
  ))

  io.in.ready := state_r === FSM_IDLE

  val out_fflags = Mux(active_mask_r(7, 0).orR, result_fflags, 0.U)
  val out_vd = vd_val
  // Bypass the final result register on the ready-consumer path; only retain
  // the registered output state when Decoupled backpressure needs it.
  val out_fflags_r = RegEnable(out_fflags, finalSumCycle)
  val out_vd_r = RegEnable(out_vd, finalSumCycle)
  io.out.bits.vd := Mux(finalSumCycle, out_vd, out_vd_r)
  io.out.bits.fflags := Mux(finalSumCycle, out_fflags, out_fflags_r)
  io.out.bits.vxsat := false.B
  io.out.valid := (finalSumCycle || state_r === FSM_OUT) && !flush
}

// NOTE: only lmul = 1
class VFRedOSum() extends Module {
  val VLEN = 128
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new VFRedInput))
    val out = Decoupled(new VFOutput)
  })

  val flush = io.flush

  val vsew = io.in.bits.vsew
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.old_vd
  val mask = io.in.bits.mask
  val widen = io.in.bits.widen
  val vm = io.in.bits.info.vm
  val ta = io.in.bits.info.ta
  val vl = io.in.bits.info.vl
  val rm = io.in.bits.info.rm

  def SrcWiden(in: UInt): UInt = {
    assert(in.getWidth == 16 || in.getWidth == 32)

    val w = in.getWidth
    val exp_w = if (w == 16) 5 else 8
    val sig_w = if (w == 16) 11 else 24
    val dest_exp_w = if (w == 16) 8 else 11
    val dest_sig_w = if (w == 16) 24 else 53

    val sign = in(w - 1)
    val exp = in(w - 2, sig_w - 1)
    val frac = in(sig_w - 2, 0) // source fraction, width = sig_w - 1

    val expIsZero = !exp.orR
    val expIsAllOne = exp.andR
    val fracIsZero = !frac.orR

    val isZero = expIsZero && fracIsZero
    val isSubnormal = expIsZero && !fracIsZero
    val isInf = expIsAllOne && fracIsZero
    val isNaN = expIsAllOne && !fracIsZero
    val isNormal = !expIsZero && !expIsAllOne

    val lshift = Wire(UInt((sig_w - 1).W))
    val subExp = Wire(UInt(dest_exp_w.W))

    if (w == 16) {
      val u = Module(new ShiftLeftPriorityWithF32EXPResult(
        srcW = sig_w - 1, priorityShiftValueW = sig_w - 1, expW = dest_exp_w))
      u.io.src := frac
      u.io.priority_shiftValue := frac
      lshift := u.io.lshift_result
      subExp := u.io.exp_result
    } else {
      val u = Module(new ShiftLeftPriorityWithF64EXPResult(
        srcW = sig_w - 1, priorityShiftValueW = sig_w - 1, expW = dest_exp_w))
      u.io.src := frac
      u.io.priority_shiftValue := frac
      lshift := u.io.lshift_result
      subExp := u.io.exp_result
    }

    // normal: bias shift (keep original logic)
    val normalExp = {
      val const1 = "b1000".U
      val const0 = "b0111".U
      Mux(exp(exp_w - 1), Cat(const1, exp(exp_w - 2, 0)), Cat(const0, exp(exp_w - 2, 0)))
    }
    val normalMant = Cat(frac, 0.U((dest_sig_w - sig_w).W))

    // subnormal: drop implicit bit after left-normalization, then pad
    val subMant = Cat(Cat(lshift.tail(1), 0.U(1.W)), 0.U((dest_sig_w - sig_w).W))

    // inf/zero
    val zeroExp = 0.U(dest_exp_w.W)
    val zeroMant = 0.U((dest_sig_w - 1).W)
    val infExp = Fill(dest_exp_w, 1.U(1.W))
    val infMant = 0.U((dest_sig_w - 1).W)

    // NaN: keep sNaN/qNaN distinction and move payload
    // quiet bit is the fraction MSB
    val srcQuietBit = frac(sig_w - 2) // 1=qNaN, 0=sNaN
    val srcPayload = frac(sig_w - 3, 0) // payload without the quiet bit

    val dstPayloadW = dest_sig_w - 2
    val srcPayloadW = sig_w - 2
    val dstPayload = Cat(srcPayload, 0.U((dstPayloadW - srcPayloadW).W))
    val nanMant = Cat(srcQuietBit, dstPayload) // keep the s/q classification

    val outExp = MuxCase(0.U(dest_exp_w.W), Seq(
      isZero -> zeroExp,
      isSubnormal -> subExp,
      isNormal -> normalExp,
      isInf -> infExp,
      isNaN -> infExp
    ))

    val outMant = MuxCase(0.U((dest_sig_w - 1).W), Seq(
      isZero -> zeroMant,
      isSubnormal -> subMant,
      isNormal -> normalMant,
      isInf -> infMant,
      isNaN -> nanMant
    ))

    Cat(sign, outExp, outMant)
  }

  val widen_vector = Wire(UInt((2 * VLEN).W))
  widen_vector := Mux(
    vsew === VSew.e16,
    Cat(vs2.asTypeOf(Vec(8, UInt(16.W))).map(x => SrcWiden(x)).reverse),
    Cat(vs2.asTypeOf(Vec(4, UInt(32.W))).map(x => SrcWiden(x)).reverse)
  )

  // TODO:
  val active_mask = Cat(mask.asBools.zipWithIndex.map{case (m, i) => (vm || m) && (i.U < vl)}.reverse)
  val vsew_plus1 = ~vsew + 1.U

  val vsew_src = Mux(widen, Mux(vsew === VSew.e16, VSew.e32, VSew.e64), vsew)
  val vsew_src_r = RegEnable(vsew_src, io.in.fire)
  val vsew_r = RegEnable(vsew, io.in.fire)
  val widen_r = RegEnable(widen, io.in.fire)
  val old_vd_r = RegEnable(old_vd, io.in.fire)
  val vl_r = RegEnable(vl, io.in.fire)
  val ta_r = RegEnable(ta, io.in.fire)
  val vm_r = RegEnable(vm, io.in.fire)
  val rm_r = RegEnable(rm, io.in.fire)
  val vs1_r = RegEnable(vs1, io.in.fire)
  val vs2_r = RegEnable(vs2, io.in.fire)
  val active_mask_r = RegEnable(active_mask, io.in.fire)

  val FSM_W = 3
  val FSM_IDLE = (1 << 0).U(FSM_W.W)
  val FSM_SUM = (1 << 1).U(FSM_W.W)
  val FSM_OUT = (1 << 2).U(FSM_W.W)
  val state_r = RegInit(FSM_IDLE)
  val state_next = Wire(UInt(FSM_W.W))

  val LastStageFFlags = Wire(UInt(5.W))
  val LastStageRes = Wire(UInt(128.W))
  val FFlags_r = Reg(UInt(5.W))
  val Active_r = Reg(UInt(9.W))
  val AdderSrc_r = Reg(UInt(256.W))
  val BaseVal_r = Reg(UInt(64.W))
  val ValidCnt = RegInit(0.U(5.W))

  state_r := state_next
  when(flush) {
    state_next := FSM_IDLE
  }.elsewhen(state_r === FSM_IDLE && io.in.fire) {
    state_next := FSM_SUM
  }.elsewhen(state_r === FSM_SUM && ValidCnt === 1.U) {
    state_next := FSM_OUT
  }.elsewhen(state_r === FSM_OUT && io.out.fire) {
    state_next := FSM_IDLE
  }.otherwise {
    state_next := state_r
  }

  when(state_r === FSM_SUM) {
    ValidCnt := ValidCnt - 1.U
    when(ValidCnt(0) === 1.U) {
      FFlags_r := FFlags_r | LastStageFFlags
      Active_r := Cat(false.B, Active_r(8, 1))
      AdderSrc_r := MuxLookup(vsew_src_r, 0.U)(Seq(
        VSew.e16 -> Cat(0.U(16.W), AdderSrc_r(255, 16)),
        VSew.e32 -> Cat(0.U(32.W), AdderSrc_r(255, 32)),
        VSew.e64 -> Cat(0.U(64.W), AdderSrc_r(255, 64)),
      ))
      BaseVal_r := LastStageRes
    }
  }.otherwise {
    FFlags_r := 0.U
    Active_r := active_mask
    AdderSrc_r := Mux(widen, widen_vector, Cat(0.U(128.W), vs2))
    BaseVal_r := vs1(63, 0)
    // 2 cycles per add; adds per register = elements at the original sew
    // (widen fp16 still adds 8 fp32 values, fp32 adds 4 fp64), so key on
    // vsew: 4/8/16 for e64/e32/e16. The final add's result is visible
    // combinationally in vd_val/result_fflags on the ValidCnt===1 cycle,
    // so no trailing bubble.
    when(vsew === VSew.e64) {
      ValidCnt := 4.U
    }.elsewhen(vsew === VSew.e32) {
      ValidCnt := 8.U
    }.elsewhen(vsew === VSew.e16) {
      ValidCnt := 16.U
    }.otherwise {
      ValidCnt := 16.U
    }
  }

  dontTouch(ValidCnt)

  // f64
  val f64_0 = Module(new FAdder_w(64))
  val f64_outFlags = Wire(UInt(5.W))
  val f64_outRes = Wire(UInt(64.W))

  f64_0.io.fpA := AdderSrc_r(63, 0)
  f64_0.io.fpB := BaseVal_r
  f64_0.io.mask := Cat(true.B, Active_r(0))
  f64_0.io.rm := rm_r

  f64_outFlags := f64_0.io.outFlags
  f64_outRes := f64_0.io.outRes

  // f32
  val f32_0 = Module(new FAdder_w(32))
  val f32_outFlags = Wire(UInt(5.W))
  val f32_outRes = Wire(UInt(32.W))
  f32_0.io.fpA := AdderSrc_r(31, 0)
  f32_0.io.fpB := BaseVal_r
  f32_0.io.mask := Cat(true.B, Active_r(0))
  f32_0.io.rm := rm_r

  f32_outFlags := f32_0.io.outFlags
  f32_outRes := f32_0.io.outRes

  // f16
  val f16_0 = Module(new FAdder_w(16))
  val f16_outFlags = Wire(UInt(5.W))
  val f16_outRes = Wire(UInt(16.W))
  f16_0.io.fpA := AdderSrc_r(15, 0)
  f16_0.io.fpB := BaseVal_r
  f16_0.io.mask := Cat(true.B, Active_r(0))
  f16_0.io.rm := rm_r

  f16_outFlags := f16_0.io.outFlags
  f16_outRes := f16_0.io.outRes

  LastStageFFlags := MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> f16_outFlags,
    VSew.e32 -> f32_outFlags,
    VSew.e64 -> f64_outFlags,
  ))
  LastStageRes := MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> f16_outRes,
    VSew.e32 -> f32_outRes,
    VSew.e64 -> f64_outRes,
  ))

  val result_16 = f16_0.io.outRes
  val result_32 = f32_0.io.outRes
  val result_64 = f64_0.io.outRes
  val result_fflags = FFlags_r | LastStageFFlags
  val old_vd_masked = old_vd_r | Fill(VLEN, ta)
  val vd_val = MuxLookup(vsew_src_r, 0.U)(Seq(
    VSew.e16 -> Cat(Fill(VLEN - 16, 1.U) & old_vd_masked(VLEN - 1, 16), Mux(active_mask_r(7, 0).orR, result_16, vs1_r(15, 0))),
    VSew.e32 -> Cat(Fill(VLEN - 32, 1.U) & old_vd_masked(VLEN - 1, 32), Mux(active_mask_r(7, 0).orR, result_32, vs1_r(31, 0))),
    VSew.e64 -> Cat(Fill(VLEN - 64, 1.U) & old_vd_masked(VLEN - 1, 64), Mux(active_mask_r(7, 0).orR, result_64, vs1_r(63, 0))),
  ))

  io.in.ready := state_r === FSM_IDLE

  val out_fflags = Mux(active_mask_r(7, 0).orR, result_fflags, 0.U)
  val out_vd = vd_val
  val out_fflags_r = RegEnable(out_fflags, ValidCnt === 1.U)
  val out_vd_r = RegEnable(out_vd, ValidCnt === 1.U)
  io.out.bits.vd := out_vd_r
  io.out.bits.fflags := out_fflags_r
  io.out.bits.vxsat := false.B
  io.out.valid := state_r === FSM_OUT
}

class FAdder_w(w: Int) extends Module {
  val io = IO(new Bundle() {
    val fpA = Input(UInt(w.W))
    val fpB = Input(UInt(w.W))
    val mask = Input(UInt(2.W))
    val rm = Input(UInt(3.W))
    val outActive = Output(Bool())
    val outRes = Output(UInt(w.W))
    val outFlags = Output(UInt(5.W))
  })
  val fpA = io.fpA
  val fpB = io.fpB
  val mask = io.mask
  val rm = io.rm

  val not_masked = mask.andR
  val both_masked = !mask.orR
  val masked_result = Mux(mask(0), fpA, fpB)
  val not_masked_s1_r = RegNext(not_masked)
  val both_masked_s1_r = RegNext(both_masked)
  val masked_result_s1_r = RegNext(masked_result)

  val fadder = Module(new FloatAdder(w))
  fadder.io.fpA := fpA
  fadder.io.fpB := fpB
  fadder.io.rm := rm

  io.outActive := !both_masked_s1_r
  io.outRes := Mux(not_masked_s1_r, fadder.io.outRes, masked_result_s1_r)
  io.outFlags := Mux(not_masked_s1_r, fadder.io.outFlags, Fill(5, false.B)) // Note: when vs1 is sNaN but vs2 all masked
}

class FloatAdder(val totalWidth: Int) extends Module with FloatParams {
  val io = IO(new Bundle() {
    val fpA = Input(UInt(totalWidth.W))
    val fpB = Input(UInt(totalWidth.W))
    val rm  = Input(UInt(rmWidth.W))
    val outRes = Output(UInt(totalWidth.W))
    val outFlags = Output(UInt(flagsWidth.W))
  })
  val FALUS0 = Module(new FALUS0(totalWidth))
  FALUS0.io.fpA := io.fpA
  FALUS0.io.fpB := io.fpB
  FALUS0.io.rm := io.rm
  val FMULS1 = Module(new FALUS1(totalWidth))
  FMULS1.io.fromS0 := RegNext(FALUS0.io.toS1)
  io.outRes := FMULS1.io.outRes
  io.outFlags := FMULS1.io.outFlags
}
