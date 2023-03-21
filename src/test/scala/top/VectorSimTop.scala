package yunsuan.top

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import yunsuan.util._
import yunsuan.vector._

trait VSPParameter {
  val VLEN       : Int = 128
  val XLEN       : Int = 64
  val VIA_latency: Int = 0 // TODO: change to 1
  val VFF_latency: Int = 3 // TODO: check only mul and mul+add, different or not
  val VFD_latency: Int = 99
  val VFA_latency: Int = 1
  val VPERM_latency: Int = 0
}

object VPUTestFuType { // only use in test, difftest with xs
  def vfa = "b0000_0000".U(8.W)
  def vff = "b0000_0001".U(8.W)
  def vfd = "b0000_0010".U(8.W)
  def via = "b0000_0011".U(8.W)
  def vperm = "b0000_0100".U(8.W)

  def unknown(typ: UInt) = {
    (typ > 4.U)
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

  val rm = UInt(3.W)
  val rm_s = UInt(2.W)

  val vinfo = new VecInfoBundle
}

class VSTOutputIO extends VPUTestBundle {
  val result = Vec(VLEN/XLEN, UInt(XLEN.W))
  val fflags = Vec(VLEN/XLEN, UInt((5*(XLEN/16)).W))
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
  when (io.in.fire()) {
    counter := 0.U
    busy := true.B
    in := io.in.bits
    latency := LookupTreeDefault(io.in.bits.fuType, 999.U, List(
      VPUTestFuType.vfa -> VFA_latency.U,
      VPUTestFuType.vff -> VFF_latency.U,
      VPUTestFuType.vfd -> VFD_latency.U,
      VPUTestFuType.via -> VIA_latency.U,
      VPUTestFuType.vperm -> VPERM_latency.U
    )) // fuType --> latency, spec case for div
    assert(!VPUTestFuType.unknown(io.in.bits.fuType))
  }
  when(io.out.fire()) {
    busy := false.B
  }
  when (busy) { counter := counter + 1.U }
  val finish_fixLatency = busy && (counter >= latency)
  val finish_uncertain = Wire(Bool())
  val is_uncertain = (in.fuType === VPUTestFuType.vfd)

  val (sew, uop_idx, rm, rm_s, fuType, opcode, src_widen, widen) = (
    in.sew, in.uop_idx, in.rm, in.rm_s, in.fuType, in.fuOpType,
    in.src_widen, in.widen
  )

  val (vstart, vl, vlmul, vm, ta, ma) = (
    in.vinfo.vstart, in.vinfo.vl, in.vinfo.vlmul, in.vinfo.vm, in.vinfo.ta, in.vinfo.ma
  )

  val vfa_result = Wire(new VSTOutputIO)
  val vff_result = Wire(new VSTOutputIO)
  val vfd_result = Reg(new VSTOutputIO)
  val via_result = Wire(new VSTOutputIO)
  val vperm_result = Wire(new VSTOutputIO)
  val vfd_result_valid = RegInit(VecInit(Seq.fill(VLEN/XLEN)(false.B)))
  when (io.in.fire() || io.out.fire()) {
    vfd_result_valid.map(_ := false.B)
  }

  finish_uncertain := vfd_result_valid.reduce(_||_)

  for (i <- 0 until (VLEN / XLEN)) {
    val (src1, src2, src3) = (in.src(0)(i), in.src(1)(i), in.src(2)(i))
    val vfa = Module(new VectorFloatAdder) // result at next cycle
    val vff = Module(new VectorFloatFMA)
    val vfd = Module(new VectorFloatDivider)
    val via = Module(new VectorIntAdder)

    require(vfa.io.fp_a.getWidth == XLEN)
    vfa.io.fp_a := src1
    vfa.io.fp_b := src2
    // Cat(vs2(95,64),vs2(31,0)) or Cat(vs2(127,96),vs2(63,32))
    vfa.io.widen_a := Cat(in.src(0)(1)(31+i*32,0+i*32),in.src(0)(0)(31+i*32,0+i*32))
    vfa.io.widen_b := Cat(in.src(1)(1)(31+i*32,0+i*32),in.src(1)(0)(31+i*32,0+i*32))
    vfa.io.fp_b := src2
    // TODO: change mask
    vfa.io.mask := Cat(src3(48),src3(32),src3(16),src3(0))
    vfa.io.uop_idx := uop_idx(0)
    // TODO: which module to handle dest's original value
    vfa.io.round_mode := rm
    vfa.io.fp_format := sew
    vfa.io.opb_widening := src_widen
    vfa.io.res_widening := widen
    vfa.io.op_code      := opcode
    vfa.io.is_vec       := true.B // TODO: check it
    vfa_result.result(i) := vfa.io.fp_result
    vfa_result.fflags(i) := vfa.io.fflags

    vfd.io.start_valid_i := busy && !has_issued && fuType === VPUTestFuType.vfd
    // io.in.ready := vfd.io.start_ready_o
    vfd.io.flush_i := false.B
    vfd.io.fp_format_i := sew
    vfd.io.opa_i := src1
    vfd.io.opb_i := src2
    vfd.io.rm_i := rm
    vfd.io.is_vec_i := true.B // TODO: check it
    vfd.io.finish_ready_i := !vfd_result_valid(i) && busy
    // FIXME: do dual vfd result sync.
    when (vfd.io.finish_valid_o && vfd.io.finish_ready_i) {
      vfd_result_valid(i) := true.B
      vfd_result.result(i) := vfd.io.fpdiv_res_o
      vfd_result.fflags(i) := vfd.io.fflags_o
    }

    via.io.in_0 := src1
    via.io.in_1 := src2
    via.io.int_format := sew
    via.io.op_code := opcode
    via.io.uop_index := DontCare // TODO: add it
    via.io.rm_s := rm_s
    //via.io.carry_or_borrow_in := MuxLookUp(sew, 0.U, Seq(0.U -> (in.src(3)(0) >> (8 * i))(7, 0), 1.U -> (in.src(3)(0) >> (4 * i))(7, 0), 2.U -> (in.src(3)(0) >> (2 * i))(7, 0), 3.U -> (in.src(3)(0) >> i)(7, 0)))
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

    vff.io.fp_a := src1
    vff.io.fp_b := src2
    vff.io.fp_c := src3
    vff.io.round_mode := rm
    vff.io.fp_format := sew
    vff.io.is_vec := true.B // TODO: check it
    vff.io.res_widening := widen
    vff_result.result(i) := vff.io.fp_result
    vff_result.fflags(i) := vff.io.fflags
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

  // arbiter
  io.out.valid := Mux(is_uncertain, finish_uncertain, finish_fixLatency)
  io.out.bits := LookupTreeDefault(in.fuType, 0.U.asTypeOf(new VSTOutputIO), List(
    VPUTestFuType.vfa -> vfa_result,
    VPUTestFuType.vff -> vff_result,
    VPUTestFuType.vfd -> vfd_result,
    VPUTestFuType.via -> via_result,
    VPUTestFuType.vperm -> vperm_result
  ))
}


object SimTop extends App {
  override def main(args: Array[String]): Unit = {
    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => new SimTop())
    ))
  }
}
