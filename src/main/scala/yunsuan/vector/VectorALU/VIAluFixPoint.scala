package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.VIAluOpcode
import yunsuan.util.GatedValidRegNext

class VIAluCtrl extends Bundle {
  val valid = Bool()
  val widen = Bool()
  val isSigned = Bool()
  val sel8  = Bool()
  val sel16 = Bool()
  val sel32 = Bool()
  val sel64 = Bool()
  val vm = Bool()
  val isSub = Bool()
  val isCmpEq = Bool()
  val isCmpNe = Bool()
  val isCmpLt = Bool()
  val isCmpLe = Bool()
  val isCmpGt = Bool()
  val isVmsbc = Bool()
  val isAvg = Bool()
  val isMaxMin = Bool()
  val isMax = Bool()
  val isVf2 = Bool()
  val isVf4 = Bool()
  val isVf8 = Bool()
  val isMisc = Bool()
  val isNarrow = Bool()
  val isCtz = Bool()
  val isLeftShiftLogic = Bool()
  val isNClip = Bool()
  val isSat = Bool()
  val widenVs2 = Bool()
  val isAddCarry = Bool()

  val isandResult = Bool()
  val isorResult = Bool()
  val isxorResult = Bool()
  val isvnandResult = Bool()
  val isVandnResult = Bool()
  val isvnorReault = Bool()
  val isVornResult = Bool()
  val isvxnorReault = Bool()
  val isextResult = Bool()
  val isscalResult = Bool()
  val isvroShiftResult = Bool()
  val isvwsllResult = Bool()
  val isleftShiftResult = Bool()
  val isrightShiftResult = Bool()
  val isleadZeroResult = Bool()
  val isbrevResult = Bool()
  val isbrev8Result = Bool()
  val isrev8Result = Bool()
  val isvIAluAddervd = Bool()
  val isoriginAddResult = Bool()
}

class VIAluData(xlen: Int) extends Bundle {
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val vs2Widen = UInt(xlen.W)
  val vs1Widen = UInt(xlen.W)
  val mask = UInt(8.W)
  val vxrm = UInt(2.W)
}


class VIAluFixPointInput(xlen: Int) extends Bundle {
  val ctrl = new VIAluCtrl
  val data = new VIAluData(xlen)
}
class VIAluFixPointOutData (xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen/2).W)
  val addCarryCmpMask = UInt(8.W)
  val vxsat = UInt(8.W)
}

class VIAluFixPoint(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluFixPointInput(xlen))
    val out = Output(Vec(2,new VIAluFixPointOutData(xlen)))
  })

  private val valid = io.in.ctrl.valid
  private val vs2 = io.in.data.vs2
  private val vs1 = io.in.data.vs1
  private val vs2Widen = io.in.data.vs2Widen
  private val vs1Widen = io.in.data.vs1Widen
  private val mask = io.in.data.mask
  private val vxrm = io.in.data.vxrm
  private val sel8  = io.in.ctrl.sel8
  private val sel16 = io.in.ctrl.sel16
  private val sel32 = io.in.ctrl.sel32
  private val sel64 = io.in.ctrl.sel64
  private val widen = io.in.ctrl.widen
  private val vm = io.in.ctrl.vm
  private val isSub = io.in.ctrl.isSub
  private val isCmpEq = io.in.ctrl.isCmpEq
  private val isCmpNe = io.in.ctrl.isCmpNe
  private val isCmpLt = io.in.ctrl.isCmpLt
  private val isCmpLe = io.in.ctrl.isCmpLe
  private val isCmpGt = io.in.ctrl.isCmpGt
  private val isVmsbc = io.in.ctrl.isVmsbc
  private val isAvg = io.in.ctrl.isAvg
  private val isMaxMin = io.in.ctrl.isMaxMin
  private val isMax = io.in.ctrl.isMax
  private val isVf2 = io.in.ctrl.isVf2
  private val isVf4 = io.in.ctrl.isVf4
  private val isVf8 = io.in.ctrl.isVf8
  private val isSigned = io.in.ctrl.isSigned
  private val isMisc = io.in.ctrl.isMisc
  private val isNarrow = io.in.ctrl.isNarrow
  private val isCtz = io.in.ctrl.isCtz
  private val isLeftShiftLogic = io.in.ctrl.isLeftShiftLogic
  private val isNClip = io.in.ctrl.isNClip
  private val isSat = io.in.ctrl.isSat
  private val widenVs2 = io.in.ctrl.widenVs2
  private val isAddCarry = io.in.ctrl.isAddCarry

  private val isoriginAddResult = io.in.ctrl.isoriginAddResult
  private val isandResult = io.in.ctrl.isandResult
  private val isorResult = io.in.ctrl.isorResult
  private val isxorResult = io.in.ctrl.isxorResult
  private val isvnandResult = io.in.ctrl.isvnandResult
  private val isVandnResult = io.in.ctrl.isVandnResult
  private val isvnorReault = io.in.ctrl.isvnorReault
  private val isVornResult = io.in.ctrl.isVornResult
  private val isvxnorReault = io.in.ctrl.isvxnorReault
  private val isextResult = io.in.ctrl.isextResult
  private val isscalResult = io.in.ctrl.isscalResult
  private val isvroShiftResult = io.in.ctrl.isvroShiftResult
  private val isvwsllResult = io.in.ctrl.isvwsllResult
  private val isleftShiftResult = io.in.ctrl.isleftShiftResult
  private val isrightShiftResult = io.in.ctrl.isrightShiftResult
  private val isleadZeroResult = io.in.ctrl.isleadZeroResult
  private val isbrevResult = io.in.ctrl.isbrevResult
  private val isbrev8Result = io.in.ctrl.isbrev8Result
  private val isrev8Result = io.in.ctrl.isrev8Result
  private val isvIAluAddervd = io.in.ctrl.isvIAluAddervd

  val VIAluFixPointLat0 = Module(new VIAluFixPointLat0(xlen))
  VIAluFixPointLat0.io.in.data.vs2 := vs2
  VIAluFixPointLat0.io.in.data.vs1 := vs1
  VIAluFixPointLat0.io.in.data.vs2Widen := vs2Widen
  VIAluFixPointLat0.io.in.data.vs1Widen := vs1Widen
  VIAluFixPointLat0.io.in.data.mask := mask
  VIAluFixPointLat0.io.in.data.vxrm := vxrm
  VIAluFixPointLat0.io.in.data.vm := vm
  VIAluFixPointLat0.io.in.ctrl.isSub := isSub
  VIAluFixPointLat0.io.in.ctrl.isCmpEq := isCmpEq
  VIAluFixPointLat0.io.in.ctrl.isCmpNe := isCmpNe
  VIAluFixPointLat0.io.in.ctrl.isCmpLt := isCmpLt
  VIAluFixPointLat0.io.in.ctrl.isCmpLe := isCmpLe
  VIAluFixPointLat0.io.in.ctrl.isCmpGt := isCmpGt
  VIAluFixPointLat0.io.in.ctrl.isVmsbc := isVmsbc
  VIAluFixPointLat0.io.in.ctrl.isAvg := isAvg
  VIAluFixPointLat0.io.in.ctrl.isMaxMin := isMaxMin
  VIAluFixPointLat0.io.in.ctrl.isMax := isMax
  VIAluFixPointLat0.io.in.ctrl.isVf2 := isVf2
  VIAluFixPointLat0.io.in.ctrl.isVf4 := isVf4
  VIAluFixPointLat0.io.in.ctrl.isVf8 := isVf8
  VIAluFixPointLat0.io.in.ctrl.sel8 := sel8
  VIAluFixPointLat0.io.in.ctrl.sel16 := sel16
  VIAluFixPointLat0.io.in.ctrl.sel32 := sel32
  VIAluFixPointLat0.io.in.ctrl.sel64 := sel64
  VIAluFixPointLat0.io.in.ctrl.isSigned := isSigned
  VIAluFixPointLat0.io.in.ctrl.widen := widen
  VIAluFixPointLat0.io.in.ctrl.widenVs2 := widenVs2
  VIAluFixPointLat0.io.in.ctrl.isAddCarry := isAddCarry
  VIAluFixPointLat0.io.in.ctrl.isMisc := isMisc
  VIAluFixPointLat0.io.in.ctrl.isNarrow := isNarrow
  VIAluFixPointLat0.io.in.ctrl.isCtz := isCtz
  VIAluFixPointLat0.io.in.ctrl.isLeftShiftLogic := isLeftShiftLogic
  VIAluFixPointLat0.io.in.ctrl.isNClip := isNClip
  VIAluFixPointLat0.io.in.ctrl.isSat := isSat

  VIAluFixPointLat0.io.in.ctrl.isoriginAddResult := isoriginAddResult
  VIAluFixPointLat0.io.in.ctrl.isandResult := isandResult
  VIAluFixPointLat0.io.in.ctrl.isorResult := isorResult
  VIAluFixPointLat0.io.in.ctrl.isxorResult := isxorResult
  VIAluFixPointLat0.io.in.ctrl.isvnandResult := isvnandResult
  VIAluFixPointLat0.io.in.ctrl.isVandnResult := isVandnResult
  VIAluFixPointLat0.io.in.ctrl.isvnorReault := isvnorReault
  VIAluFixPointLat0.io.in.ctrl.isVornResult := isVornResult
  VIAluFixPointLat0.io.in.ctrl.isvxnorReault := isvxnorReault
  VIAluFixPointLat0.io.in.ctrl.isextResult := isextResult
  VIAluFixPointLat0.io.in.ctrl.isscalResult := isscalResult
  VIAluFixPointLat0.io.in.ctrl.isvroShiftResult := isvroShiftResult
  VIAluFixPointLat0.io.in.ctrl.isvwsllResult := isvwsllResult
  VIAluFixPointLat0.io.in.ctrl.isleftShiftResult := isleftShiftResult
  VIAluFixPointLat0.io.in.ctrl.isrightShiftResult := isrightShiftResult
  VIAluFixPointLat0.io.in.ctrl.isleadZeroResult := isleadZeroResult
  VIAluFixPointLat0.io.in.ctrl.isbrevResult := isbrevResult
  VIAluFixPointLat0.io.in.ctrl.isbrev8Result := isbrev8Result
  VIAluFixPointLat0.io.in.ctrl.isrev8Result := isrev8Result
  VIAluFixPointLat0.io.in.ctrl.isvIAluAddervd := isvIAluAddervd

  val VIAluFixPointLat1 = Module(new VIAluFixPointLat1(xlen))
  VIAluFixPointLat1.io.in.VIAluFixPointLat0ToLat1 := RegEnable(VIAluFixPointLat0.io.out.VIAluFixPointLat0ToLat1, valid)
  VIAluFixPointLat1.io.in.sel8  := RegEnable(sel8,  valid)
  VIAluFixPointLat1.io.in.sel16 := RegEnable(sel16, valid)
  VIAluFixPointLat1.io.in.sel32 := RegEnable(sel32, valid)
  VIAluFixPointLat1.io.in.sel64 := RegEnable(sel64, valid)
  VIAluFixPointLat1.io.in.vs2 := RegEnable(vs2, valid)
  VIAluFixPointLat1.io.in.vs1 := RegEnable(vs1, valid)
  VIAluFixPointLat1.io.in.isSigned := GatedValidRegNext(isSigned)
  VIAluFixPointLat1.io.in.isMisc := GatedValidRegNext(isMisc)


  val VIAluFixPointLat0Vd = VIAluFixPointLat0.io.out.vd
  val VIAluFixPointLat0NarrowVd = VIAluFixPointLat0.io.out.narrowVd
  val VIAluFixPointLat0AddCarryCmpMask = VIAluFixPointLat0.io.out.addCarryCmpMask
  val VIAluFixPointLat0Vxsat = VIAluFixPointLat0.io.out.vxsat
  val VIAluFixPointLat1vd = VIAluFixPointLat1.io.out.vd
  val VIAluFixPointLat1NarrowVd = VIAluFixPointLat1.io.out.narrowVd
  val VIAluFixPointLat1AddCarryCmpMask = VIAluFixPointLat1.io.out.addCarryCmpMask
  val VIAluFixPointLat1Vxsat = VIAluFixPointLat1.io.out.vxsat

  io.out(0).vd := VIAluFixPointLat0Vd
  io.out(0).narrowVd := VIAluFixPointLat0NarrowVd
  io.out(0).addCarryCmpMask := VIAluFixPointLat0AddCarryCmpMask
  io.out(0).vxsat := VIAluFixPointLat0Vxsat

  io.out(1).vd := VIAluFixPointLat1vd
  io.out(1).narrowVd := VIAluFixPointLat1NarrowVd
  io.out(1).addCarryCmpMask := VIAluFixPointLat1AddCarryCmpMask
  io.out(1).vxsat := VIAluFixPointLat1Vxsat
}
