package yunsuan.vector

import chisel3._
import chisel3.util._
//for example VectorFloatAdder
import yunsuan.{VfaddOpCode, VectorElementFormat}

trait VSPParameter {
  val VLEN      : Int = 2048
  val XLEN       : Int = 64
  val VFA_latency: Int = 1
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

//INPUT
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

//OUTPUT
class VSTOutputIO extends VPUTestBundle {
  val result = Vec(VLEN/XLEN, UInt(XLEN.W))
  val fflags = Vec(VLEN/XLEN, UInt((5*(XLEN/16)).W))
  val vxsat = Bool()
}

class ExmapleIO extends VPUTestBundle {
  val in = Flipped(DecoupledIO(Output(new VSTInputIO)))
  val out = DecoupledIO(Output(new VSTOutputIO))
}

class NewVectorCore() extends VPUTestModule {
  val io = IO(new ExmapleIO())


  // just for example
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
    latency := VFA_latency.U
  }
  
  when(io.out.fire()) {
    busy := false.B
  }
  when (busy) { counter := counter + 1.U }
  val finish_fixLatency = busy && (counter >= latency)

  val (sew, uop_idx, rm, rm_s, fuType, opcode, src_widen, widen, is_frs1, is_frs2) = (
    in.sew, in.uop_idx, in.rm, in.rm_s, in.fuType, in.fuOpType,
    in.src_widen, in.widen, in.is_frs1, in.is_frs2
  )

  val (vstart, vl, vlmul, vm, ta, ma) = (
    in.vinfo.vstart, in.vinfo.vl, in.vinfo.vlmul, in.vinfo.vm, in.vinfo.ta, in.vinfo.ma
  )


  // new vector core, vfa inside
  val vfa_result = Wire(new VSTOutputIO)

  // example io for vfa inside
  for (i <- 0 until (VLEN / XLEN)) {
    // TODO:
    val (src1, src2, src3) = (in.src(0)(i), in.src(1)(i), in.src(2)(i))

    val vfa = Module(new VectorFloatAdder())
    require(vfa.io.fp_a.getWidth == XLEN)
    vfa.io.fire := busy
    vfa.io.fp_a := src1
    vfa.io.fp_b := src2

    vfa.io.widen_a := Cat(in.src(0)(VLEN/XLEN/2+i/2)(31+(i%2)*32,0+(i%2)*32),in.src(0)(i/2)(31+(i%2)*32,0+(i%2)*32))
    vfa.io.widen_b := Cat(in.src(1)(VLEN/XLEN/2+i/2)(31+(i%2)*32,0+(i%2)*32),in.src(1)(i/2)(31+(i%2)*32,0+(i%2)*32))
    
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
  }

  io.out.valid := finish_fixLatency
  io.out.bits := vfa_result

}
