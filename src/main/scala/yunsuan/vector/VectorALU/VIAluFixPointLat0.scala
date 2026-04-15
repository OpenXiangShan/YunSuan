package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._

class VIAluFixPointLat0ToLat1(xlen: Int) extends Bundle{
  val addCarryCmpMask = UInt(8.W)
  val VIAluAddervxsat = UInt(8.W)
  val isSat = Bool()
  val isMaxMin = Bool()
  val isMax = Bool()
  val ltResult = UInt(8.W)
  val sat = UInt(8.W)
  val originAddResult = UInt(xlen.W)
  val upOverflowUnSign = UInt(8.W)
  val downOverflowSign = UInt(8.W)
  val narrowVd = UInt((xlen / 2).W)
  val VIAluMiscvxsat = UInt(8.W)
  val isNClip = Bool()
  val signBitSel8  = UInt(4.W)
  val signBitSel16 = UInt(2.W)
  val signBitSel32 = Bool()
  val overflowSignSel8 = UInt(4.W)
  val overflowSignSel16 = UInt(2.W)
  val overflowSignSel32 = Bool()
  val upOverflowUnSignSel8 = UInt(4.W)
  val upOverflowUnSignSel16 = UInt(2.W)
  val upOverflowUnSignSel32 = Bool()
  val nclipResultSel8Tmp = UInt((xlen / 2).W)
  val nclipResultSel16Tmp = UInt((xlen / 2).W)
  val nclipResultSel32Tmp = UInt((xlen / 2).W)
}

class VIAluFixPointLat0Ctrl extends Bundle{
  val isSub = Bool()
  val isCmpEq = Bool()
  val isCmpNe = Bool()
  val isCmpLt = Bool()
  val isCmpLe = Bool()
  val isCmpGt = Bool()
  val isVmsbc = Bool()
  val isAvg = Bool()
  val isSat = Bool()
  val isMaxMin = Bool()
  val isMax = Bool()
  val isVf2 = Bool()
  val isVf4 = Bool()
  val isVf8 = Bool()
  val sel8 = Bool()
  val sel16 = Bool()
  val sel32 = Bool()
  val sel64 = Bool()
  val isSigned = Bool()
  val isNClip = Bool()

  val widen = Bool()
  val widenVs2 = Bool()
  val isAddCarry = Bool()
  val isMisc = Bool()
  val isNarrow = Bool()
  val isCtz = Bool()
  val isLeftShiftLogic= Bool()

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

class VIAluFixPointLat0Data(xlen: Int) extends Bundle {
  val vs2 = UInt(xlen.W)
  val vs1 = UInt(xlen.W)
  val vs2Widen = UInt(xlen.W)
  val vs1Widen = UInt(xlen.W)
  val mask = UInt(8.W)
  val vxrm = UInt(2.W)
  val vm = Bool()
}

class VIAluFixPointLat0Input(xlen: Int) extends Bundle {
    val ctrl = new VIAluFixPointLat0Ctrl
    val data = new VIAluFixPointLat0Data(xlen)
}

class VIAluFixPointLat0Output(xlen: Int) extends Bundle {
    val vd = UInt(xlen.W)
    val addCarryCmpMask = UInt(8.W)
    val narrowVd = UInt((xlen / 2).W)
    val vxsat = UInt(8.W)
    val VIAluFixPointLat0ToLat1 = new VIAluFixPointLat0ToLat1(xlen)
}

class VIAluFixPointLat0(xlen: Int = 64) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluFixPointLat0Input(xlen))
    val out = Output(new VIAluFixPointLat0Output(xlen))
    
  })
  
  private val sel8  = io.in.ctrl.sel8
  private val sel16 = io.in.ctrl.sel16
  private val sel32 = io.in.ctrl.sel32
  private val sel64 = io.in.ctrl.sel64

  private val isSub = io.in.ctrl.isSub
  private val isCmpEq = io.in.ctrl.isCmpEq
  private val isCmpNe = io.in.ctrl.isCmpNe
  private val isCmpLt = io.in.ctrl.isCmpLt
  private val isCmpLe = io.in.ctrl.isCmpLe
  private val isCmpGt = io.in.ctrl.isCmpGt
  private val isVmsbc = io.in.ctrl.isVmsbc
  private val isAvg = io.in.ctrl.isAvg
  private val isSat = io.in.ctrl.isSat
  private val isMaxMin = io.in.ctrl.isMaxMin
  private val isMax = io.in.ctrl.isMax
  private val isCtz = io.in.ctrl.isCtz
  private val isLeftShiftLogic = io.in.ctrl.isLeftShiftLogic
  private val isNClip = io.in.ctrl.isNClip

  private val isVf2 = io.in.ctrl.isVf2
  private val isVf4 = io.in.ctrl.isVf4
  private val isVf8 = io.in.ctrl.isVf8

  private val vm = io.in.data.vm
  private val widen = io.in.ctrl.widen
  private val isSigned = io.in.ctrl.isSigned
  private val widenVs2 = io.in.ctrl.widenVs2
  private val isAddCarry = io.in.ctrl.isAddCarry
  private val isMisc = io.in.ctrl.isMisc
  private val isNarrow = io.in.ctrl.isNarrow
  private val vs2 = io.in.data.vs2
  private val vs1 = io.in.data.vs1
  private val vs2Widen = io.in.data.vs2Widen
  private val vs1Widen = io.in.data.vs1Widen
  private val mask = io.in.data.mask
  private val vxrm = io.in.data.vxrm

  //new
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
  


  val vIAluAdder = Module(new VIAluAdder(xlen))
  vIAluAdder.io.in.ctrl.sel8  := sel8
  vIAluAdder.io.in.ctrl.sel16 := sel16
  vIAluAdder.io.in.ctrl.sel32 := sel32
  vIAluAdder.io.in.ctrl.sel64 := sel64
  vIAluAdder.io.in.ctrl.widenVs2 := widenVs2
  vIAluAdder.io.in.ctrl.widen := widen
  vIAluAdder.io.in.ctrl.isSigned := isSigned
  vIAluAdder.io.in.ctrl.isAddCarry := isAddCarry
  vIAluAdder.io.in.ctrl.isSub := isSub
  vIAluAdder.io.in.ctrl.isCmpEq := isCmpEq
  vIAluAdder.io.in.ctrl.isCmpNe := isCmpNe
  vIAluAdder.io.in.ctrl.isCmpLt := isCmpLt
  vIAluAdder.io.in.ctrl.isCmpLe := isCmpLe
  vIAluAdder.io.in.ctrl.isCmpGt := isCmpGt
  vIAluAdder.io.in.ctrl.isVmsbc := isVmsbc
  vIAluAdder.io.in.ctrl.isAvg := isAvg
  vIAluAdder.io.in.ctrl.isSat := isSat
  vIAluAdder.io.in.ctrl.isMaxMin := isMaxMin
  vIAluAdder.io.in.ctrl.isMax := isMax
  vIAluAdder.io.in.data.vs2 := vs2
  vIAluAdder.io.in.data.vs1 := vs1
  vIAluAdder.io.in.data.vs2Widen := vs2Widen
  vIAluAdder.io.in.data.vs1Widen := vs1Widen
  vIAluAdder.io.in.data.vm := vm
  vIAluAdder.io.in.data.vxrm := vxrm
  vIAluAdder.io.in.data.mask := mask

  val vIAluMisc = Module(new VIAluMisc(xlen))
  vIAluMisc.io.in.ctrl.sel8  := sel8
  vIAluMisc.io.in.ctrl.sel16 := sel16
  vIAluMisc.io.in.ctrl.sel32 := sel32
  vIAluMisc.io.in.ctrl.sel64 := sel64
  vIAluMisc.io.in.ctrl.isVf2 := isVf2
  vIAluMisc.io.in.ctrl.isVf4 := isVf4
  vIAluMisc.io.in.ctrl.isVf8 := isVf8
  vIAluMisc.io.in.ctrl.widenVs2 := widenVs2
  vIAluMisc.io.in.ctrl.widen := widen
  vIAluMisc.io.in.ctrl.isNarrow := isNarrow
  vIAluMisc.io.in.ctrl.isSigned := isSigned
  vIAluMisc.io.in.ctrl.isLeftShiftLogic := isLeftShiftLogic
  vIAluMisc.io.in.ctrl.isCtz := isCtz
  vIAluMisc.io.in.data.vs2 := vs2
  vIAluMisc.io.in.data.vs1 := vs1
  vIAluMisc.io.in.data.vs2Widen := vs2Widen
  vIAluMisc.io.in.data.vs1Widen := vs1Widen
  vIAluMisc.io.in.data.vxrm := vxrm

  //vIAluAdder
  private val vIAluAddervd = vIAluAdder.io.out.vd
  private val addCarryCmpMask = vIAluAdder.io.out.addCarryCmpMask
  private val vIAluAddervxsat = vIAluAdder.io.out.vxsat
  private val ltResult = vIAluAdder.io.out.ltResult
  private val sat = vIAluAdder.io.out.sat
  private val originAddResult = vIAluAdder.io.out.originAddResult
  private val upOverflowUnSign = vIAluAdder.io.out.upOverflowUnSign
  private val downOverflowSign = vIAluAdder.io.out.downOverflowSign

  //vIAluMisc
  private val extResult = vIAluMisc.io.out.extResult
  private val andResult = vIAluMisc.io.out.andResult
  private val orResult = vIAluMisc.io.out.orResult
  private val xorResult = vIAluMisc.io.out.xorResult
  private val vnandResult = vIAluMisc.io.out.vnandResult
  private val VandnResult = vIAluMisc.io.out.VandnResult
  private val vnorReault = vIAluMisc.io.out.vnorReault
  private val VornResult = vIAluMisc.io.out.VornResult
  private val vxnorReault = vIAluMisc.io.out.vxnorReault
  private val scalResult = vIAluMisc.io.out.scalResult
  private val vroShiftResult = vIAluMisc.io.out.vroShiftResult
  private val vwsllResult = vIAluMisc.io.out.vwsllResult
  private val leftShiftResult = vIAluMisc.io.out.leftShiftResult
  private val rightShiftResult = vIAluMisc.io.out.rightShiftResult
  private val leadZeroResult = vIAluMisc.io.out.leadZeroResult
  private val brevResult = vIAluMisc.io.out.brevResult
  private val brev8Result = vIAluMisc.io.out.brev8Result
  private val rev8Result = vIAluMisc.io.out.rev8Result

  private val narrowVd = vIAluMisc.io.out.narrowVd
  private val VIAluMiscvxsat = vIAluMisc.io.out.vxsat
  private val popResult = vIAluMisc.io.out.popResult
  private val signBitSel8 = vIAluMisc.io.out.signBitSel8
  private val signBitSel16 = vIAluMisc.io.out.signBitSel16
  private val signBitSel32 = vIAluMisc.io.out.signBitSel32
  private val overflowSignSel8 = vIAluMisc.io.out.overflowSignSel8
  private val overflowSignSel16 = vIAluMisc.io.out.overflowSignSel16
  private val overflowSignSel32 = vIAluMisc.io.out.overflowSignSel32
  private val upOverflowUnSignSel8 = vIAluMisc.io.out.upOverflowUnSignSel8
  private val upOverflowUnSignSel16 = vIAluMisc.io.out.upOverflowUnSignSel16
  private val upOverflowUnSignSel32 = vIAluMisc.io.out.upOverflowUnSignSel32
  private val nclipResultSel8Tmp = vIAluMisc.io.out.nclipResultSel8Tmp
  private val nclipResultSel16Tmp = vIAluMisc.io.out.nclipResultSel16Tmp
  private val nclipResultSel32Tmp = vIAluMisc.io.out.nclipResultSel32Tmp  

  io.out.vd := Mux1H(Seq(
    isandResult -> andResult,
    isorResult -> orResult,
    isxorResult -> xorResult,
    isvnandResult -> vnandResult,
    isVandnResult -> VandnResult,
    isvnorReault -> vnorReault,
    isVornResult -> VornResult,
    isvxnorReault -> vxnorReault,
    isextResult -> extResult,
    isscalResult -> scalResult,
    isvroShiftResult -> vroShiftResult,
    isvwsllResult -> vwsllResult,
    isleftShiftResult -> leftShiftResult,
    isrightShiftResult -> rightShiftResult,
    isleadZeroResult -> leadZeroResult,
    isbrevResult -> brevResult,
    isbrev8Result -> brev8Result,
    isrev8Result -> rev8Result,
    isvIAluAddervd -> vIAluAddervd,
    isoriginAddResult -> originAddResult
  ))
  io.out.vxsat := Mux(isMisc,0.U, vIAluAddervxsat)
  io.out.addCarryCmpMask := addCarryCmpMask
  io.out.narrowVd := narrowVd

  val oneHot = Cat(isandResult,isorResult,isxorResult,isvnandResult,isVandnResult,
                  isvnorReault,isVornResult,isvxnorReault,isextResult,isscalResult,
                  isvroShiftResult,isvwsllResult,isleftShiftResult,isrightShiftResult,
                  isleadZeroResult,isbrevResult,isbrev8Result,isrev8Result,isvIAluAddervd,
                  isoriginAddResult)
  val count = PopCount(oneHot)
  assert(count <= 1.U, "oneHot is not one-hot, has $count bits set")

  //tolat2
  io.out.VIAluFixPointLat0ToLat1.addCarryCmpMask := addCarryCmpMask
  io.out.VIAluFixPointLat0ToLat1.VIAluAddervxsat := vIAluAddervxsat
  io.out.VIAluFixPointLat0ToLat1.isSat := isSat
  io.out.VIAluFixPointLat0ToLat1.isMaxMin := isMaxMin
  io.out.VIAluFixPointLat0ToLat1.isMax := isMax
  io.out.VIAluFixPointLat0ToLat1.ltResult := ltResult
  io.out.VIAluFixPointLat0ToLat1.sat := sat
  io.out.VIAluFixPointLat0ToLat1.originAddResult := originAddResult
  io.out.VIAluFixPointLat0ToLat1.upOverflowUnSign := upOverflowUnSign
  io.out.VIAluFixPointLat0ToLat1.downOverflowSign := downOverflowSign

  io.out.VIAluFixPointLat0ToLat1.narrowVd := narrowVd
  io.out.VIAluFixPointLat0ToLat1.VIAluMiscvxsat := VIAluMiscvxsat
  io.out.VIAluFixPointLat0ToLat1.isNClip := isNClip
  io.out.VIAluFixPointLat0ToLat1.signBitSel8 := signBitSel8
  io.out.VIAluFixPointLat0ToLat1.signBitSel16 := signBitSel16
  io.out.VIAluFixPointLat0ToLat1.signBitSel32 := signBitSel32
  io.out.VIAluFixPointLat0ToLat1.overflowSignSel8 := overflowSignSel8
  io.out.VIAluFixPointLat0ToLat1.overflowSignSel16 := overflowSignSel16
  io.out.VIAluFixPointLat0ToLat1.overflowSignSel32 := overflowSignSel32
  io.out.VIAluFixPointLat0ToLat1.upOverflowUnSignSel8 := upOverflowUnSignSel8
  io.out.VIAluFixPointLat0ToLat1.upOverflowUnSignSel16 := upOverflowUnSignSel16
  io.out.VIAluFixPointLat0ToLat1.upOverflowUnSignSel32 := upOverflowUnSignSel32
  io.out.VIAluFixPointLat0ToLat1.nclipResultSel8Tmp := nclipResultSel8Tmp
  io.out.VIAluFixPointLat0ToLat1.nclipResultSel16Tmp := nclipResultSel16Tmp
  io.out.VIAluFixPointLat0ToLat1.nclipResultSel32Tmp := nclipResultSel32Tmp
}
