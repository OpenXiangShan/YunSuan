package yunsuan.top

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._
import yunsuan.util._
import yunsuan.vector.VectorConvert.VectorCvt
import yunsuan.vector._
import yunsuan.scalar.INT2FP
import yunsuan.scalar.FPCVT

trait VSPParameter {
  val VLEN      : Int = 2048
  val XLEN       : Int = 64
  val VIA_latency: Int = 0 // TODO: change to 1
  val VIAF_latency: Int = 1 // TODO:
  val VFF_latency: Int = 3 // TODO: check only mul and mul+add, different or not
  val VFD_latency: Int = 99
  val VFA_latency: Int = 1
  val VPERM_latency: Int = 1
  val VID_latency: Int = 99
  val VCVT_latency: Int = 2 // ??
  val Vrgather_latency: Int = 1
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
  def vrgather = "b0000_1010".U(8.W)
  def NewVectorCore = "b0000_1011".U(8.W)
  def unknown(typ: UInt) = {
    (typ > 11.U)
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

class SimTopIO extends VPUTestBundle {
  val in = Flipped(DecoupledIO(Output(new VSTInputIO)))
  val out = DecoupledIO(Output(new VSTOutputIO))
}

class SimTop() extends VPUTestModule {
  val io = IO(new SimTopIO())

  val vector_core = Module(new NewVectorCore())
  io <> vector_core.io
}


object SimTop extends App {
  (new ChiselStage).execute(args, Seq(
    ChiselGeneratorAnnotation(() => new SimTop())
  ))
}
