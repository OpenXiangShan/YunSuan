package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.VIAluOpcode
import yunsuan.util.GatedValidRegNext

class VIAluInfo extends Bundle {
  val sel8  = Bool()
  val sel16 = Bool()
  val sel32 = Bool()
  val sel64 = Bool()
  val vm = Bool()
}

class ExtInfo extends Bundle {
  val isVf2 = Bool()
  val isVf4 = Bool()
  val isVf8 = Bool()
}

class AdderCtrl extends Bundle {
  val widenVs2 = Bool()
  val isAddCarry = Bool()
}

class MiscCtrl extends Bundle {
  val isExt = ValidIO(new ExtInfo)
  val isMisc = Bool()
  val isNarrow = Bool()
}

class VIAluCtrl extends Bundle {
  val valid = Bool()
  val widen = Bool()
  val isSigned = Bool()
  val adderCtrl = new AdderCtrl
  val miscCtrl = new MiscCtrl
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
  val opcode = new VIAluOpcode
  val info = new VIAluInfo
  val ctrl = new VIAluCtrl
  val data = new VIAluData(xlen)
}

class VIAluFixPointOutput(xlen: Int) extends Bundle {
  val vd = UInt(xlen.W)
  val narrowVd = UInt((xlen/2).W)
  val addCarryCmpMask = UInt(8.W)
  val vxsat = UInt(8.W)
}

class VIAluFixPoint(xlen: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new VIAluFixPointInput(xlen))
    val out = Output(new VIAluFixPointOutput(xlen))
  })
  private val valid = io.in.ctrl.valid
  private val opcode = io.in.opcode
  private val sel8  = io.in.info.sel8
  private val sel16 = io.in.info.sel16
  private val sel32 = io.in.info.sel32
  private val sel64 = io.in.info.sel64
  private val vm = io.in.info.vm
  private val widen = io.in.ctrl.widen
  private val isSigned = io.in.ctrl.isSigned
  private val widenVs2 = io.in.ctrl.adderCtrl.widenVs2
  private val isAddCarry = io.in.ctrl.adderCtrl.isAddCarry
  private val isExt = io.in.ctrl.miscCtrl.isExt
  private val isMisc = io.in.ctrl.miscCtrl.isMisc
  private val isNarrow = io.in.ctrl.miscCtrl.isNarrow
  private val vs2 = io.in.data.vs2
  private val vs1 = io.in.data.vs1
  private val vs2Widen = io.in.data.vs2Widen
  private val vs1Widen = io.in.data.vs1Widen
  private val mask = io.in.data.mask
  private val vxrm = io.in.data.vxrm

  val vIAluAdder = Module(new VIAluAdder(xlen))
  vIAluAdder.io.in.opcode := opcode
  vIAluAdder.io.in.ctrl.sel8  := sel8
  vIAluAdder.io.in.ctrl.sel16 := sel16
  vIAluAdder.io.in.ctrl.sel32 := sel32
  vIAluAdder.io.in.ctrl.sel64 := sel64
  vIAluAdder.io.in.ctrl.widenVs2 := widenVs2
  vIAluAdder.io.in.ctrl.widen := widen
  vIAluAdder.io.in.ctrl.isSigned := isSigned
  vIAluAdder.io.in.ctrl.isAddCarry := isAddCarry
  vIAluAdder.io.in.data.vs2 := vs2
  vIAluAdder.io.in.data.vs1 := vs1
  vIAluAdder.io.in.data.vs2Widen := vs2Widen
  vIAluAdder.io.in.data.vs1Widen := vs1Widen
  vIAluAdder.io.in.data.vm := vm
  vIAluAdder.io.in.data.vxrm := vxrm
  vIAluAdder.io.in.data.mask := mask

  val vIAluMisc = Module(new VIAluMisc(xlen))
  vIAluMisc.io.in.opcode := opcode
  vIAluMisc.io.in.ctrl.sel8  := sel8
  vIAluMisc.io.in.ctrl.sel16 := sel16
  vIAluMisc.io.in.ctrl.sel32 := sel32
  vIAluMisc.io.in.ctrl.sel64 := sel64
  vIAluMisc.io.in.ctrl.isExt := isExt
  vIAluMisc.io.in.ctrl.widenVs2 := widenVs2
  vIAluMisc.io.in.ctrl.widen := widen
  vIAluMisc.io.in.ctrl.isNarrow := isNarrow
  vIAluMisc.io.in.ctrl.isSigned := isSigned
  vIAluMisc.io.in.data.vs2 := vs2
  vIAluMisc.io.in.data.vs1 := vs1
  vIAluMisc.io.in.data.vs2Widen := vs2Widen
  vIAluMisc.io.in.data.vs1Widen := vs1Widen
  vIAluMisc.io.in.data.vxrm := vxrm

  val vIAluFixPointS1 = Module(new VIAluFixPointS1(xlen))
  vIAluFixPointS1.io.in.adderToS1 := RegEnable(vIAluAdder.io.out.toS1, valid)
  vIAluFixPointS1.io.in.miscToS1  := RegEnable(vIAluMisc.io.out.toS1, valid)
  vIAluFixPointS1.io.in.sel8  := RegEnable(sel8,  valid)
  vIAluFixPointS1.io.in.sel16 := RegEnable(sel16, valid)
  vIAluFixPointS1.io.in.sel32 := RegEnable(sel32, valid)
  vIAluFixPointS1.io.in.sel64 := RegEnable(sel64, valid)
  vIAluFixPointS1.io.in.vs2 := RegEnable(vs2, valid)
  vIAluFixPointS1.io.in.vs1 := RegEnable(vs1, valid)
  vIAluFixPointS1.io.in.isSigned := GatedValidRegNext(isSigned)
  vIAluFixPointS1.io.in.isMisc := GatedValidRegNext(isMisc)

  io.out.vd := vIAluFixPointS1.io.out.vd
  io.out.narrowVd := vIAluFixPointS1.io.out.narrowVd
  io.out.addCarryCmpMask := vIAluFixPointS1.io.out.addCarryCmpMask
  io.out.vxsat := vIAluFixPointS1.io.out.vxsat
}
