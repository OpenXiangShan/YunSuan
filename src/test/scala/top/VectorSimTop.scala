package yunsuan.top

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.vector._
import yunsuan.scalar.INT2FP
import yunsuan.scalar.FPCVT

trait VSPParameter {
  val VLEN       : Int = 128
  val XLEN       : Int = 64
  val VIA_latency: Int = 0 // TODO: change to 1
  val VIAF_latency: Int = 1 // TODO:
  val VFF_latency: Int = 3 // TODO: check only mul and mul+add, different or not
  val VFD_latency: Int = 99
  val VFA_latency: Int = 1
  val VPERM_latency: Int = 1
  val VID_latency: Int = 99
  val VCVT_latency: Int = 2 // ??
}

object VPUTestFuType { // only use in test, difftest with xs
  def vfa = "b0000_0000".U(8.W)
  def vff = "b0000_0001".U(8.W)
  def vfd = "b0000_0010".U(8.W)
  def via = "b0000_0011".U(8.W)
  def vperm = "b0000_0100".U(8.W)
  def viaf = "b0000_0101".U(8.W)
  def vid = "b0000_0110".U(8.W)
  def vcvt= "b0000_0111".U(8.W)
  def fcvtf2x= "b0000_1000".U(8.W)
  def fcvti2f= "b0000_1001".U(8.W)

  def unknown(typ: UInt) = {
    (typ > 9.U)
  }
}

class VPUTestBundle extends Bundle with VSPParameter
class VPUTestModule extends Module with VSPParameter

class VecInfoBundle extends VPUTestBundle {
  val vstart    = UInt(7.W)     // 0-127
  val vl        = UInt(8.W)     // 0-128
  val vlmul     = UInt(3.W)

  val vm        = Bool()        // 0: masked, 1: unmasked
  val ta        = Bool()        // 0: undisturbed, 1: agnostic
  val ma        = Bool()        // 0: undisturbed, 1: agnostic
}

class VSTInputIO extends VPUTestBundle {
  val src = Vec(4, Vec(VLEN/XLEN, UInt(XLEN.W)))
  val fuType = UInt(5.W)
  val fuOpType = UInt(8.W)
  val sew = UInt(2.W)
  val uop_idx = UInt(6.W)

  val src_widen = Bool()
  val widen = Bool()
  val is_frs1 = Bool()
  val is_frs2 = Bool()

  val rm = UInt(3.W)
  val rm_s = UInt(2.W)

  val vinfo = new VecInfoBundle
}

class VSTOutputIO extends VPUTestBundle {
  val result = Vec(VLEN/XLEN, UInt(XLEN.W))
  val fflags = Vec(VLEN/XLEN, UInt((5*(XLEN/16)).W))
  val vxsat = Bool()
}

class SimTopIO extends VPUTestBundle {
  val in = Flipped(DecoupledIO(Output(new VSTInputIO)))
  val out = DecoupledIO(Output(new VSTOutputIO))
}

class SimTop() extends VPUTestModule {
  val io = IO(new SimTopIO())

  val busy = RegInit(false.B)
  val has_issued = RegInit(false.B)
  val counter = RegInit(0.U(64.W))
  val latency = RegInit(0.U(64.W))

  val in = Reg(new VSTInputIO)
  val out = Reg(new VSTOutputIO)

  io.in.ready := !busy
  io.out.bits := out

  has_issued := busy
  when (io.in.fire) {
    counter := 0.U
    busy := true.B
    in := io.in.bits
    latency := LookupTreeDefault(io.in.bits.fuType, 999.U, List(
      VPUTestFuType.vfa -> VFA_latency.U,
      VPUTestFuType.vff -> VFF_latency.U,
      VPUTestFuType.vfd -> VFD_latency.U,
      VPUTestFuType.via -> VIA_latency.U,
      VPUTestFuType.vperm -> VPERM_latency.U,
      VPUTestFuType.viaf -> VIAF_latency.U,
      VPUTestFuType.vid -> VID_latency.U,
      VPUTestFuType.vcvt -> VCVT_latency.U,
      VPUTestFuType.fcvtf2x -> VCVT_latency.U,
      VPUTestFuType.fcvti2f -> VCVT_latency.U
    )) // fuType --> latency, spec case for div
    assert(!VPUTestFuType.unknown(io.in.bits.fuType))
  }
  when(io.out.fire) {
    busy := false.B
  }
  when (busy) { counter := counter + 1.U }
  val finish_fixLatency = busy && (counter >= latency)
  val finish_uncertain = Wire(Bool())
  val is_uncertain = (in.fuType === VPUTestFuType.vfd) || (in.fuType === VPUTestFuType.vid)

  val (sew, uop_idx, rm, rm_s, fuType, opcode, src_widen, widen, is_frs1, is_frs2) = (
    in.sew, in.uop_idx, in.rm, in.rm_s, in.fuType, in.fuOpType,
    in.src_widen, in.widen, in.is_frs1, in.is_frs2
  )

  val (vstart, vl, vlmul, vm, ta, ma) = (
    in.vinfo.vstart, in.vinfo.vl, in.vinfo.vlmul, in.vinfo.vm, in.vinfo.ta, in.vinfo.ma
  )

  val vfa_result = Wire(new VSTOutputIO)
  val vff_result = Wire(new VSTOutputIO)
  val vfd_result = Reg(new VSTOutputIO)
  val via_result = Wire(new VSTOutputIO)
  val vperm_result = Wire(new VSTOutputIO)
  val viaf_result = Wire(new VSTOutputIO)
  val vfd_result_valid = RegInit(VecInit(Seq.fill(VLEN/XLEN)(false.B)))
  val vid_result = Wire(new VSTOutputIO)
  val vid_result_valid = Wire(Bool())
  val vcvt_result = Wire(new VSTOutputIO)
  val i2f_result = Wire(new VSTOutputIO)
  val fpcvt_result = Wire(new VSTOutputIO)
  when (io.in.fire || io.out.fire) {
    vfd_result_valid.map(_ := false.B)
  }

  finish_uncertain := Mux(in.fuType === VPUTestFuType.vid, vid_result_valid,vfd_result_valid.reduce(_&&_))

  for (i <- 0 until (VLEN / XLEN)) {
    val (src1, src2, src3) = (in.src(0)(i), in.src(1)(i), in.src(2)(i))
    val vfa = Module(new VectorFloatAdder) // result at next cycle
    val vff = Module(new VectorFloatFMA)
    val vfd = Module(new VectorFloatDivider)
    val via = Module(new VectorIntAdder)
    val vcvt = Module(new VectorCvt(XLEN))
    val i2fcvt = Module(new INT2FP(2, XLEN))
    val fpcvt = Module(new FPCVT(XLEN))

    require(vfa.io.fp_a.getWidth == XLEN)
    vfa.io.fire := busy
    vfa.io.fp_a := src1
    vfa.io.fp_b := src2
    //io.widen_a Cat(vs2(95,64),vs2(31,0)) or Cat(vs2(127,96),vs2(63,32))
    //io.widen_b Cat(vs1(95,64),vs1(31,0)) or Cat(vs1(127,96),vs1(63,32))
    vfa.io.widen_a := Cat(in.src(0)(1)(31+i*32,0+i*32),in.src(0)(0)(31+i*32,0+i*32))
    vfa.io.widen_b := Cat(in.src(1)(1)(31+i*32,0+i*32),in.src(1)(0)(31+i*32,0+i*32))
    vfa.io.frs1  := in.src(1)(0) // VS1(63,0)
    vfa.io.fp_b := src2
    // TODO: change mask
    val maskTemp = Cat(src3(48),src3(32),src3(16),src3(0))
    vfa.io.mask := Mux1H(
      Seq(
        (sew === 1.U) -> maskTemp,
        (sew === 2.U) -> Cat(maskTemp(2),maskTemp(0)),
        (sew === 3.U) -> maskTemp(0)
      )
    )
    vfa.io.uop_idx := uop_idx(0)
    // TODO: which module to handle dest's original value
    vfa.io.round_mode := rm
    vfa.io.fp_format := sew
    vfa.io.opb_widening := src_widen
    vfa.io.res_widening := widen
    vfa.io.is_frs1 := is_frs1
    vfa.io.op_code      := opcode
    vfa.io.is_vec       := true.B // TODO: check it
    vfa.io.fp_aIsFpCanonicalNAN := false.B
    vfa.io.fp_bIsFpCanonicalNAN := false.B
    vfa.io.maskForReduction := 0.U
    vfa.io.is_vfwredosum := false.B
    vfa.io.is_fold := 0.U
    vfa.io.vs2_fold := Cat(in.src(0)(1), in.src(0)(0))
    vfa_result.result(i) := vfa.io.fp_result
    vfa_result.fflags(i) := vfa.io.fflags
    vfa_result.vxsat := 0.U // DontCare

    vfd.io.start_valid_i := busy && !has_issued && fuType === VPUTestFuType.vfd
    // io.in.ready := vfd.io.start_ready_o
    vfd.io.flush_i := false.B
    vfd.io.fp_format_i := sew
    vfd.io.opa_i := src1
    vfd.io.opb_i := src2
    vfd.io.frs2_i := in.src(0)(0) // VS2(63,0)
    vfd.io.frs1_i := in.src(1)(0) // VS1(63,0)
    vfd.io.is_frs2_i := is_frs2
    vfd.io.is_frs1_i := is_frs1
    vfd.io.is_sqrt_i := opcode
    vfd.io.rm_i := rm
    vfd.io.is_vec_i := true.B // TODO: check it
    vfd.io.fp_aIsFpCanonicalNAN := false.B
    vfd.io.fp_bIsFpCanonicalNAN := false.B
    vfd.io.finish_ready_i := !vfd_result_valid(i) && busy
    // FIXME: do dual vfd result sync.
    when (vfd.io.finish_valid_o && vfd.io.finish_ready_i) {
      vfd_result_valid(i) := true.B
      vfd_result.result(i) := vfd.io.fpdiv_res_o
      vfd_result.fflags(i) := vfd.io.fflags_o
      vfd_result.vxsat := 0.U // DontCare
    }

    via.io.in_0 := src1
    via.io.in_1 := src2
    via.io.int_format := sew
    via.io.op_code := opcode
    via.io.uop_index := DontCare // TODO: add it
    via.io.rm_s := rm_s
    //via.io.carry_or_borrow_in := MuxLookUp(sew)(0.U, Seq(0.U -> (in.src(3)(0) >> (8 * i))(7, 0), 1.U -> (in.src(3)(0) >> (4 * i))(7, 0), 2.U -> (in.src(3)(0) >> (2 * i))(7, 0), 3.U -> (in.src(3)(0) >> i)(7, 0)))
    when(sew === 0.U) {
      via.io.carry_or_borrow_in := (in.src(3)(0) >> (8 * i))(7, 0)
    }.elsewhen(sew === 1.U) {
      via.io.carry_or_borrow_in := (in.src(3)(0) >> (4 * i))(7, 0)
    }.elsewhen(sew === 2.U) {
      via.io.carry_or_borrow_in := (in.src(3)(0) >> (2 * i))(7, 0)
    }.elsewhen(sew === 3.U) {
      via.io.carry_or_borrow_in := (in.src(3)(0) >> (i))(7, 0)
    }.otherwise {
      via.io.carry_or_borrow_in := 0.U
    }
    via_result.result(i) := via.io.out
    via_result.fflags(i) := 0.U // DontCare
    via_result.vxsat := 0.U // DontCare

    vff.io.fire := busy
    vff.io.fp_a := src1
    vff.io.fp_b := src2
    vff.io.fp_c := src3
    //io.widen_a Cat(vs2(95,64),vs2(31,0)) or Cat(vs2(127,96),vs2(63,32))
    //io.widen_b Cat(vs1(95,64),vs1(31,0)) or Cat(vs1(127,96),vs1(63,32))
    vff.io.widen_a := Cat(in.src(0)(1)(31+i*32,0+i*32),in.src(0)(0)(31+i*32,0+i*32))
    vff.io.widen_b := Cat(in.src(1)(1)(31+i*32,0+i*32),in.src(1)(0)(31+i*32,0+i*32))
    vff.io.uop_idx := uop_idx(0)
    vff.io.frs1  := in.src(1)(0) // VS1(63,0)
    vff.io.round_mode := rm
    vff.io.fp_format := sew
    vff.io.op_code := opcode
    vff.io.is_frs1  := is_frs1
    vff.io.is_vec := true.B // TODO: check it
    vff.io.fp_aIsFpCanonicalNAN := false.B
    vff.io.fp_bIsFpCanonicalNAN := false.B
    vff.io.fp_cIsFpCanonicalNAN := false.B
    vff.io.res_widening := widen
    vff_result.result(i) := vff.io.fp_result
    vff_result.fflags(i) := vff.io.fflags
    vff_result.vxsat := 0.U // DontCare

    // connect vcvt's io
    vcvt.io.fire := busy
    vcvt.io.sew := sew
    vcvt.io.opType := opcode
    vcvt.io.rm := rm
    vcvt.io.src := src1 // 128 bit->vcvt
    vcvt.io.isFpToVecInst := false.B
    vcvt.io.isFround := 0.U
    vcvt.io.isFcvtmod := false.B
    vcvt_result.vxsat := 0.U
    vcvt_result.result(i) := vcvt.io.result
    vcvt_result.fflags(i) := vcvt.io.fflags

    // i2fcvt
    i2fcvt.regEnables(0) := true.B
    i2fcvt.regEnables(1) := true.B
    i2fcvt.io.wflags := busy
    i2fcvt.io.opType := opcode(4,0)
    i2fcvt.io.rm := rm
    i2fcvt.io.rmInst := 7.U
    i2fcvt.io.src := src1
    i2f_result.vxsat := 0.U
    i2f_result.result(i) := i2fcvt.io.result
    i2f_result.fflags(i) := i2fcvt.io.fflags

    //fpcvt
    fpcvt.io.fire := busy
    fpcvt.io.sew := sew
    fpcvt.io.opType := opcode
    fpcvt.io.rm := rm
    fpcvt.io.src := src1
    fpcvt.io.isFpToVecInst := true.B
    fpcvt.io.isFround := 0.U
    fpcvt.io.isFcvtmod := false.B
    fpcvt_result.vxsat := 0.U
    fpcvt_result.result(i) := fpcvt.io.result
    fpcvt_result.fflags(i) := fpcvt.io.fflags
  }

  val vperm = Module(new VPermTop)
  vperm.io.vs1 := Cat(in.src(0)(1), in.src(0)(0))
  vperm.io.vs2 := Cat(in.src(1)(1), in.src(1)(0))
  vperm.io.old_vd := Cat(in.src(2)(1), in.src(2)(0))
  vperm.io.mask := Cat(in.src(3)(1), in.src(3)(0))
  vperm.io.vs1_type := ZeroExt(sew, 4)
  vperm.io.vs2_type := ZeroExt(sew, 4)
  vperm.io.vd_type := ZeroExt(sew, 4)

  vperm.io.opcode := opcode
  vperm.io.uop_idx := uop_idx
  vperm.io.vstart := vstart
  vperm.io.vl := vl
  vperm.io.vlmul := vlmul
  vperm.io.vm := vm
  vperm.io.ta := ta
  vperm.io.ma := ma
  vperm_result.result(0) := vperm.io.res_vd(XLEN-1, 0)
  vperm_result.result(1) := vperm.io.res_vd(VLEN-1, XLEN)
  vperm_result.fflags(0) := 0.U
  vperm_result.fflags(1) := 0.U
  vperm_result.vxsat := 0.U

  val viaf = Module(new VIAluFWrapper)
  viaf.io.in.bits.fuType := in.fuType
  viaf.io.in.bits.fuOpType := opcode
  viaf.io.in.bits.vsew := sew
  viaf.io.in.bits.info.vstart := in.vinfo.vstart
  viaf.io.in.bits.info.vl := in.vinfo.vl
  viaf.io.in.bits.info.vlmul := in.vinfo.vlmul
  viaf.io.in.bits.info.vm := in.vinfo.vm
  viaf.io.in.bits.info.ta := in.vinfo.ta
  viaf.io.in.bits.info.ma := in.vinfo.ma
  viaf.io.in.bits.info.uopIdx := in.uop_idx
  viaf.io.in.bits.info.vxrm := in.rm_s
  viaf.io.in.valid := true.B
  viaf.io.out.ready := true.B
  viaf.io.in.bits.src.zip(in.src).foreach { case (a, b) => a := b.asUInt }
  viaf_result.result.zipWithIndex.foreach { case (rs, i) => rs := viaf.io.out.bits.data(XLEN * (i + 1) - 1, XLEN * i) }
  viaf_result.fflags := 0.U.asTypeOf(viaf_result.fflags.cloneType) // DontCare
  viaf_result.vxsat := viaf.io.out.bits.vxsat
  // vid
  val vid_opcode = opcode
  val vid_sign = vid_opcode(0)
  val vid_rem = vid_opcode(1)
  val vid = Module(new VectorIdiv)
  val src1 = Cat(in.src(0)(1), in.src(0)(0))
  val src2 = Cat(in.src(1)(1), in.src(1)(0))
  vid.io.div_in_valid := busy && !has_issued && fuType === VPUTestFuType.vid
  vid.io.sign := vid_sign
  vid.io.dividend_v := src1
  vid.io.divisor_v := src2
  vid.io.flush := false.B
  vid.io.sew := sew
  vid.io.div_out_ready := busy
  val vid_fflags_0 = LookupTreeDefault(sew, 0.U, List(
    0.U -> vid.io.d_zero(7, 0),
    1.U -> vid.io.d_zero(3, 0),
    2.U -> vid.io.d_zero(1, 0),
    3.U -> vid.io.d_zero(0, 0)
  ))
  val vid_fflags_1 = LookupTreeDefault(sew, 0.U, List(
    0.U -> vid.io.d_zero(15, 8),
    1.U -> vid.io.d_zero(15, 4),
    2.U -> vid.io.d_zero(15, 2),
    3.U -> vid.io.d_zero(15, 1)
  ))
  vid_result_valid := vid.io.div_out_valid
  vid_result.result(0) := Mux(vid_rem, vid.io.div_out_rem_v, vid.io.div_out_q_v)(XLEN - 1, 0)
  vid_result.result(1) := Mux(vid_rem, vid.io.div_out_rem_v, vid.io.div_out_q_v)(VLEN - 1, XLEN)
  vid_result.fflags(0) := ZeroExt(vid_fflags_0, 20)
  vid_result.fflags(1) := ZeroExt(vid_fflags_1, 20)
  vid_result.vxsat := 0.U

  // arbiter
  io.out.valid := Mux(is_uncertain, finish_uncertain, finish_fixLatency)
  io.out.bits := LookupTreeDefault(in.fuType, 0.U.asTypeOf(new VSTOutputIO), List(
    VPUTestFuType.vfa -> vfa_result,
    VPUTestFuType.vff -> vff_result,
    VPUTestFuType.vfd -> vfd_result,
    VPUTestFuType.via -> via_result,
    VPUTestFuType.vperm -> vperm_result,
    VPUTestFuType.viaf -> viaf_result,
    VPUTestFuType.vid -> vid_result,
    VPUTestFuType.vcvt -> vcvt_result,
    VPUTestFuType.fcvtf2x -> fpcvt_result,
    VPUTestFuType.fcvti2f -> i2f_result
  ))
}


object SimTop extends App {
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new SimTop()), FirtoolOption("--lowering-options=explicitBitcast")
  ))
}
