
package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.vector.alu.{VAluOpcode, VIntFixpAlu}
import yunsuan.VialuFixType
import yunsuan.util.LookupTree

class VIAluFWrapperIn extends Bundle {
  val fuType = UInt(5.W)
  val fuOpType = UInt(8.W)
  val vsew = UInt(2.W)
  val info = new VIFuInfo()
  val src = Vec(4, UInt(VIFuParam.VLEN.W))
}

class VIAluFWrapperOut extends Bundle {
  val data = UInt(VIFuParam.VLEN.W)
  val vxsat = Bool()
}

class VIAluFWrapperIO extends Bundle {
  val in = Flipped(DecoupledIO(Output(new VIAluFWrapperIn)))
  val out = DecoupledIO(Output(new VIAluFWrapperOut))
}

class VIAluFWrapper extends Module {
  val XLEN = VIFuParam.XLEN
  val VLEN = VIFuParam.VLEN

  val io = IO(new VIAluFWrapperIO)

// rename signal
  val in = io.in.bits
  val out = io.out.bits

// connect VIAluF
  val vIntFixpAlu = Module(new VIntFixpAlu)
  vIntFixpAlu.io.fire := io.in.valid
  vIntFixpAlu.io.in.opcode := VialuFixType.getOpcode(in.fuOpType).asTypeOf(vIntFixpAlu.io.in.opcode.cloneType)
  vIntFixpAlu.io.in.info.vm := in.info.vm
  vIntFixpAlu.io.in.info.ma := in.info.ma
  vIntFixpAlu.io.in.info.ta := in.info.ta
  vIntFixpAlu.io.in.info.vlmul := in.info.vlmul
  vIntFixpAlu.io.in.info.vl := in.info.vl

  vIntFixpAlu.io.in.info.vstart := in.info.vstart // TODO :
  vIntFixpAlu.io.in.info.uopIdx := in.info.uopIdx

  vIntFixpAlu.io.in.info.vxrm := in.info.vxrm
  val srcVdType = Wire(new Bundle{
    val srcType2 = UInt(4.W)
    val srcType1 = UInt(4.W)
    val vdType = UInt(4.W)
  })
  srcVdType := VialuFixType.getSrcVdType(in.fuOpType, in.vsew(1,0)).asTypeOf(srcVdType.cloneType)
  vIntFixpAlu.io.in.srcType(0) := srcVdType.srcType2
  vIntFixpAlu.io.in.srcType(1) := srcVdType.srcType1
  vIntFixpAlu.io.in.vdType := srcVdType.vdType
  val needReverse  = VialuFixType.needReverse(in.fuOpType)
  val needClearMask = VialuFixType.needClearMask(in.fuOpType)
  val vs1 = Mux(needReverse, in.src(1), in.src(0))
  val vs2 = Mux(needReverse, in.src(0), in.src(1))
  val mask = Mux(needClearMask, 0.U, in.src(3))
  vIntFixpAlu.io.in.vs1 := vs1
  vIntFixpAlu.io.in.vs2 := vs2
  vIntFixpAlu.io.in.old_vd := in.src(2)

  val eewVs1 = SewOH(srcVdType.srcType1(1, 0))
  val eewVd = SewOH(srcVdType.vdType(1, 0))
  val uopIdx = in.info.uopIdx
  val narrow = srcVdType.srcType2(1, 0) === 3.U && srcVdType.vdType(1, 0) === 2.U ||
    srcVdType.srcType2(1, 0) === 2.U && srcVdType.vdType(1, 0) === 1.U ||
    srcVdType.srcType2(1, 0) === 1.U && srcVdType.vdType(1, 0) === 0.U
  val eewVm = Mux(srcVdType.vdType === 15.U, eewVs1, eewVd)
  val maskIdx = Mux(narrow, uopIdx >> 1, uopIdx)
  vIntFixpAlu.io.in.mask16b := MaskExtract(mask, maskIdx, eewVm)
  vIntFixpAlu.io.ctrl.narrow := narrow
  vIntFixpAlu.io.ctrl.vstart_gte_vl := in.info.vstart >= in.info.vl

  // connect io
  out.data := vIntFixpAlu.io.out.vd
  out.vxsat := vIntFixpAlu.io.out.vxsat
  io.out.valid := RegNext(io.in.valid)
  io.in.ready := DontCare
}