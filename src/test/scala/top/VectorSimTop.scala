package yunsuan.top

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3.util._
import yunsuan.encoding.Opcode.{Latency, Opcode, Opcodes}
import yunsuan.encoding.Opcode.Opcodes.{FMacOpcode, FMiscOpcode, VFDivOpcode, VFMacOpcode}
import yunsuan.fpu.falu.FloatAdderV2
import yunsuan.fpu.fmul.FloatMUL
import yunsuan.fpu.{FloatCompare, FloatFMAV2}
import yunsuan.scalar.Mul
import yunsuan.util._
import yunsuan.vector._
import yunsuan.vector.Common._

trait VSPParameter {
  val VLEN       : Int = 128
  val XLEN       : Int = 64
}

object VPUTestFuType { // only use in test, difftest with xs
  def vfa = "b0000_0000".U(8.W)
  def vff = "b0000_0001".U(8.W)
  def vfd = "b0000_0010".U(8.W)
  def via = "b0000_0011".U(8.W)
  def vperm = "b0000_0100".U(8.W)
  def imul = "b0000_1011".U(8.W)
  def fcmp = "b0000_1100".U(8.W)
  def falu = "b0000_1101".U(8.W)
  def fmul = "b0000_1110".U(8.W)
  def fma = "b0000_1111".U(8.W)

  private def all = Seq(vfa, vff, vfd, via, vperm, imul, fcmp, falu, fmul, fma)

  def unknown(typ: UInt): Bool = !typ.isOneOf(all)
}


// placeholder to let the complie pass
private object VPermTestOpcode extends Opcodes {
  val vslideup    = Value(BitPat("b000000000")).setLatency(1)
  val vslidedown  = Value(BitPat("b000000001")).setLatency(1)
  val vslide1up   = Value(BitPat("b000000010")).setLatency(1)
  val vslide1down = Value(BitPat("b000000011")).setLatency(1)
  val vrgather    = Value(BitPat("b000000100")).setLatency(1)
  val vrgatherrs1 = Value(BitPat("b000000101")).setLatency(1)
  val vcompress   = Value(BitPat("b000000110")).setLatency(1)
}

// placeholder to let the complie pass
private object VIAluTestOpcode extends Opcodes {
  val maxMin     = Value(BitPat("b00?01100?")).setLatency(1)
  val saturating = Value(BitPat("b00?0?0010")).setLatency(1)
  val vnclipu    = Value(BitPat("b110101011")).setLatency(1)
  val vnclip     = Value(BitPat("b111101101")).setLatency(1)
  val other      = Value(BitPat("b?????????")).setLatency(0)
}

// placeholder to let the complie pass
private object IMulTestOpcode extends Opcodes {
  val mul    = Value(BitPat("b000000000")).setLatency(2)
  val mulh   = Value(BitPat("b000000001")).setLatency(2)
  val mulhsu = Value(BitPat("b000000010")).setLatency(2)
  val mulhu  = Value(BitPat("b000000011")).setLatency(2)
  val mulw   = Value(BitPat("b000000100")).setLatency(2)
  val mulw7  = Value(BitPat("b000001100")).setLatency(2)
}

// placeholder to let the complie pass
private object FMulTestOpcode extends Opcodes {
  val fmul = Value(BitPat("b000000000")).setLatency(2)
}

object VPUTestLatency {
  private val latencyWidth = 64

  private case class Result(latency: UInt, uncertain: Bool, valid: Bool)

  private def decode(opcode: UInt, opcodes: Opcodes): Result = {
    val table = opcodes.all.toSeq.sortBy(op => (-op.encode.mask.bitCount, op.encode.value))
    require(table.nonEmpty, s"${opcodes.getClass.getSimpleName} must define at least one opcode")

    val rows = table.map { op =>
      val latency = op.getLat
      require(latency >= Latency.uncertainLitVal(), s"Invalid latency $latency for ${op.getName()}")
      (op.encode === opcode, latency)
    }
    val fixedRows = rows.collect {
      case (hit, latency) if latency != Latency.uncertainLitVal() => hit -> latency.U(latencyWidth.W)
    }

    Result(
      MuxCase(0.U(latencyWidth.W), fixedRows),
      rows.collect { case (hit, latency) if latency == Latency.uncertainLitVal() => hit }.foldLeft(false.B)(_ || _),
      rows.map(_._1).reduce(_ || _)
    )
  }

  def apply(fuType: UInt, opcode: UInt): (UInt, Bool, Bool) = {
    val fmac  = decode(opcode, FMacOpcode)
    val vfmac = decode(opcode, VFMacOpcode)
    val vfdiv = decode(opcode, VFDivOpcode)
    val vialu = decode(opcode, VIAluTestOpcode)
    val vperm = decode(opcode, VPermTestOpcode)
    val imul  = decode(opcode, IMulTestOpcode)
    val fmisc = decode(opcode, FMiscOpcode)
    val fmul  = decode(opcode, FMulTestOpcode)

    val table = Seq(
      VPUTestFuType.vfa   -> fmac,
      VPUTestFuType.vff   -> vfmac,
      VPUTestFuType.vfd   -> vfdiv,
      VPUTestFuType.via   -> vialu,
      VPUTestFuType.vperm -> vperm,
      VPUTestFuType.imul  -> imul,
      VPUTestFuType.fcmp  -> fmisc,
      VPUTestFuType.falu  -> fmac,
      VPUTestFuType.fmul  -> fmul,
      VPUTestFuType.fma   -> vfmac
    )

    val select = table.map { case (typ, result) => (fuType === typ, result) }
    val latency = MuxCase(0.U(latencyWidth.W), select.map { case (hit, result) => hit -> result.latency })
    val uncertain = MuxCase(false.B, select.map { case (hit, result) => hit -> result.uncertain })
    val valid = MuxCase(false.B, select.map { case (hit, result) => hit -> result.valid })
    (latency, uncertain, valid)
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
  val fuOpType = Opcode()
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
  val uncertainLatency = RegInit(false.B)

  val in = Reg(new VSTInputIO)
  val out = Reg(new VSTOutputIO)

  io.in.ready := !busy
  io.out.bits := out

  val (inputLatency, inputUncertainLatency, inputOpcodeValid) =
    VPUTestLatency(io.in.bits.fuType, io.in.bits.fuOpType)

  has_issued := busy
  when (io.in.fire) {
    counter := 0.U
    busy := true.B
    in := io.in.bits
    latency := inputLatency
    uncertainLatency := inputUncertainLatency
    assert(!VPUTestFuType.unknown(io.in.bits.fuType))
    assert(inputOpcodeValid)
  }
  when(io.out.fire) {
    busy := false.B
  }
  when (busy) { counter := counter + 1.U }
  val finish_fixLatency = busy && (counter >= latency)
  val finish_uncertain = Wire(Bool())

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
  val vfd_result_valid = RegInit(VecInit(Seq.fill(VLEN/XLEN)(false.B)))
  val imul_result = Wire(new VSTOutputIO)
  val fcmp_result = Wire(new VSTOutputIO)
  val falu_result = Wire(new VSTOutputIO)
  val fmul_result = Wire(new VSTOutputIO)
  val fma_result = Wire(new VSTOutputIO)
  when (io.in.fire || io.out.fire) {
    vfd_result_valid.map(_ := false.B)
  }

  finish_uncertain := vfd_result_valid.reduce(_&&_)

  for (i <- 0 until (VLEN / XLEN)) {
    val (src1, src2, src3) = (in.src(0)(i), in.src(1)(i), in.src(2)(i))
    val vfa = Module(new VectorFloatAdder) // result at next cycle
    val vff = Module(new VectorFloatFMA)
    val vfd = Module(new VectorFloatDivider)
    val via = Module(new VectorIntAdder)
    val imul = Module(new Mul(XLEN))
    val fcmp = Module(new FloatCompare)
    val falu = Module(new FloatAdderV2)
    val fmul = Module(new FloatMUL)
    val fma = Module(new FloatFMAV2)

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
    vfd.io.is_sqrt_i := false.B // RTL does not support sqrt yet
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

    // mul
    imul.io.in.valid := busy
    imul.io.in.bits.fuOpType := opcode
    imul.io.in.bits.src(0) := src1
    imul.io.in.bits.src(1) := src2
    imul_result.result(i) := imul.io.out
    imul_result.fflags(i) := 0.U
    imul_result.vxsat := 0.U

    // fcmp
    fcmp.io.src0 := src1
    fcmp.io.src1 := src2
    fcmp.io.opCode := opcode
    fcmp_result.vxsat := 0.U
    fcmp_result.result(i) := fcmp.io.result
    fcmp_result.fflags(i) := ZeroExt(fcmp.io.fflags, 20)



    // falu
    falu.io.fire := busy
    falu.io.in.fp_fmt := sew
    falu.io.in.op_code := opcode
    falu.io.in.fp_a := src1
    falu.io.in.fp_b := src2
    falu.io.in.fpAAppend := 0.U
    falu.io.in.round_mode := rm
    falu.io.in.inCtrlFromFMUL := 0.U.asTypeOf(falu.io.in.inCtrlFromFMUL)
    falu.io.in.isSubFromFMUL := false.B
    falu_result.result(i) := falu.io.out.fp_result
    falu_result.fflags(i) := ZeroExt(falu.io.out.fflags, 20)
    falu_result.vxsat := 0.U

    // fmul
    fmul.io.fire := busy
    fmul.io.in.isFMUL := true.B
    fmul.io.in.isNeg := false.B
    fmul.io.in.fp_fmt := sew
    fmul.io.in.fp_a := src1
    fmul.io.in.fp_b := src2
    fmul.io.in.round_mode := rm
    fmul_result.result(i) := fmul.io.out.fp_result
    fmul_result.fflags(i) := ZeroExt(fmul.io.out.fflags, 20)
    fmul_result.vxsat := 0.U

    // fma
    fma.io.in.fire := busy
    fma.io.in.fp_fmt := sew
    fma.io.in.op_code := opcode
    fma.io.in.fp_a := src1
    fma.io.in.fp_b := src2
    fma.io.in.fp_c := src3
    fma.io.in.round_mode := rm
    fma_result.result(i) := fma.io.out.fp_result
    fma_result.fflags(i) := ZeroExt(fma.io.out.fflags, 20)
    fma_result.vxsat := 0.U
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

  // arbiter
  io.out.valid := Mux(uncertainLatency, finish_uncertain, finish_fixLatency)
  io.out.bits := LookupTreeDefault(in.fuType, 0.U.asTypeOf(new VSTOutputIO), List(
    VPUTestFuType.vfa -> vfa_result,
    VPUTestFuType.vff -> vff_result,
    VPUTestFuType.vfd -> vfd_result,
    VPUTestFuType.via -> via_result,
    VPUTestFuType.vperm -> vperm_result,
    VPUTestFuType.imul -> imul_result,
    VPUTestFuType.fcmp -> fcmp_result,
    VPUTestFuType.falu -> falu_result,
    VPUTestFuType.fmul -> fmul_result,
    VPUTestFuType.fma -> fma_result
  ))
}


object SimTop extends App {
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new SimTop()),
    FirtoolOption("--lowering-options=explicitBitcast"),
    FirtoolOption("--default-layer-specialization=enable")
  ))
}
