
package yunsuan.vector

import chisel3._
import chisel3.util._

import yunsuan.util.VecDataSplitModule
import yunsuan.util.{DstMgu, Mgu, Mgtu}
import yunsuan.vector.Common.SewOH
import yunsuan.vector.VectorALU.VIAluFixPoint
import yunsuan.util.Utils.{SplitMask, VecDataToMaskDataVec}
import yunsuan.VialuFixType

class VIAluInfo extends Bundle {
  val vm = Bool()
  val ma = Bool()
  val ta = Bool()
  val vsew = UInt(2.W)
  val vlmul = UInt(3.W)
  val vl = UInt(8.W)
  val uopIdx = UInt(6.W)
  val vxrm = UInt(2.W)
}

class VIAluFixPointWrapperIn(vlen: Int) extends Bundle {
  val src = Vec(4, UInt(vlen.W))
  val info = new VIAluInfo
  val fuOpType = UInt(9.W)
}

class VIAluFixPointWrapperOut(vlen: Int) extends Bundle {
  val data = UInt(vlen.W)
  val vxsat = Bool()
}

class VIAluFixPointWrapperIO(vlen: Int) extends Bundle {
  val in = Flipped(DecoupledIO(Output(new VIAluFixPointWrapperIn(vlen))))
  val out = DecoupledIO(Output(new VIAluFixPointWrapperOut(vlen)))
}
class VIAluFixPointWrapper extends Module {
  val VLEN = 128
  val io = IO(new VIAluFixPointWrapperIO(VLEN))

  val valid = io.in.valid
  io.in.ready := DontCare

  val vIAluFixPointTop = Module(new VIAluFixPointTop(VLEN))
  vIAluFixPointTop.io.valid := valid
  vIAluFixPointTop.io.in <> io.in.bits

  io.out.valid := RegNext(valid)
  io.out.bits.data  := vIAluFixPointTop.io.out.vd
  io.out.bits.vxsat := vIAluFixPointTop.io.out.vxsat
}

class VIAluFixPointTestTopOut(vlen: Int) extends Bundle {
  val vd = UInt(vlen.W)
  val vxsat = Bool()
}

class VIAluFixPointTestTopIO(vlen: Int) extends Bundle {
  val valid = Input(Bool())
  val in = Input(new VIAluFixPointWrapperIn(vlen))
  val out = Output(new VIAluFixPointTestTopOut(vlen))
}

class VIAluFixPointTop(vlen: Int) extends Module {
  val io = IO(new VIAluFixPointTestTopIO(vlen))

  val dataWidth = vlen
  val dataWidthOfDataModule = 64
  val VLEN = 128
  val XLEN = 64
  val numVecModule = dataWidth / dataWidthOfDataModule

  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vs1Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val vIAluFixPoints = Seq.fill(numVecModule)(Module(new VIAluFixPoint(XLEN)))
  private val mgu = Module(new Mgu(dataWidth))
  private val mgtu = Module(new Mgtu(dataWidth))

  val valid = io.valid
  val vs1   = io.in.src(0)
  val vs2   = io.in.src(1)
  val oldVd = io.in.src(2)
  val maskIn = io.in.src(3)
  val vm     = io.in.info.vm
  val ma     = io.in.info.ma
  val ta     = io.in.info.ta
  val vsew   = io.in.info.vsew
  val vlmul  = io.in.info.vlmul
  val vl     = io.in.info.vl
  val vuopIdx = io.in.info.uopIdx
  val vxrm   = io.in.info.vxrm
  val fuOpType = io.in.fuOpType
  val opcode = VialuFixType.getOpcode(fuOpType).asTypeOf(vIAluFixPoints.head.io.in.opcode)

  val widen = (VialuFixType.fmtIsWVW(fuOpType) | VialuFixType.fmtIsVVW(fuOpType)) & VialuFixType.isAddSub(fuOpType)
  val isSigned = VialuFixType.isSigned(fuOpType)
  val widenVs2 = VialuFixType.fmtIsVVW(fuOpType) & VialuFixType.isAddSub(fuOpType)
  val isAddCarry = VialuFixType.isAddCarry(fuOpType)
  val extSeq = Seq(VialuFixType.vzext_vf2, VialuFixType.vzext_vf4, VialuFixType.vzext_vf8, VialuFixType.vsext_vf2, VialuFixType.vsext_vf4, VialuFixType.vsext_vf8)
  val isExt = extSeq.map(_ === fuOpType).reduce(_ || _)
  val isVf2 = VialuFixType.fmtIsVF2(fuOpType) & isExt
  val isVf4 = VialuFixType.fmtIsVF4(fuOpType) & isExt
  val isVf8 = VialuFixType.fmtIsVF8(fuOpType) & isExt
  val isMisc = VialuFixType.isMisc(fuOpType)
  val narrowSeq = Seq(VialuFixType.vnsra_wv, VialuFixType.vnsrl_wv, VialuFixType.vnclip_wv, VialuFixType.vnclipu_wv)
  val isNarrow = narrowSeq.map(_ === fuOpType).reduce(_ || _)
  val dstMaskSeq = Seq(VialuFixType.vmadc_vv, VialuFixType.vmadc_vvm, VialuFixType.vmsbc_vv, VialuFixType.vmsbc_vvm,
    VialuFixType.vmand_mm, VialuFixType.vmnand_mm, VialuFixType.vmandn_mm, VialuFixType.vmxor_mm, VialuFixType.vmor_mm,
    VialuFixType.vmnor_mm, VialuFixType.vmorn_mm, VialuFixType.vmxnor_mm,
    VialuFixType.vmseq_vv, VialuFixType.vmsne_vv, VialuFixType.vmsle_vv, VialuFixType.vmsleu_vv,
    VialuFixType.vmslt_vv, VialuFixType.vmsltu_vv, VialuFixType.vmsgt_vv, VialuFixType.vmsgtu_vv,
  )
  val isDstMask = dstMaskSeq.map(_ === fuOpType).reduce(_ || _)
  val opMaskSeq = Seq(VialuFixType.vmand_mm, VialuFixType.vmnand_mm, VialuFixType.vmandn_mm,
    VialuFixType.vmxor_mm, VialuFixType.vmor_mm, VialuFixType.vmnor_mm, VialuFixType.vmorn_mm, VialuFixType.vmxnor_mm
  )
  val isOpMask = opMaskSeq.map(_ === fuOpType).reduce(_ || _)

  val sel8  = vsew === 0.U
  val sel16 = vsew === 1.U
  val sel32 = vsew === 2.U
  val sel64 = vsew === 3.U

  vs2Split.io.inVecData := vs2
  vs1Split.io.inVecData := vs1

  val vs2Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  val vs1Vec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  val vs2WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))
  val vs1WidenVec: Vec[UInt] = Wire(Vec(numVecModule, UInt(XLEN.W)))

  val allMaskTrue = VecInit(Seq.fill(VLEN)(true.B)).asUInt
  val allMaskFalse = VecInit(Seq.fill(VLEN)(false.B)).asUInt

  val needClearMask = VialuFixType.needClearMask(fuOpType)
  dontTouch(needClearMask)
  val srcMask = Wire(UInt(VLEN.W))
  srcMask := MuxCase(maskIn, Seq(
    needClearMask -> allMaskFalse,
    vm -> allMaskTrue,
  ))

  val maskDataVec = VecDataToMaskDataVec(srcMask, vsew)
  val maskVecGen = Wire(UInt((VLEN / 8).W))
  maskVecGen := SplitMask(maskDataVec(vuopIdx), vsew).asUInt
  val maskVec = Wire(Vec(2, UInt(8.W)))
  maskVec := maskVecGen.asTypeOf(maskVec)

  for (i <- 0 until 2) {
    vs2Vec(i) := vs2Split.io.outVec64b(i)
    vs1Vec(i) := vs1Split.io.outVec64b(i)
    vs2WidenVec(i) := Mux(vuopIdx(0), vs2Split.io.outVec32b(i + 2), vs2Split.io.outVec32b(i))
    vs1WidenVec(i) := Mux(vuopIdx(0), vs1Split.io.outVec32b(i + 2), vs1Split.io.outVec32b(i))
  }


  vIAluFixPoints.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.in.ctrl.valid := valid
      mod.io.in.opcode := opcode
      mod.io.in.info.sel8 := sel8
      mod.io.in.info.sel16 := sel16
      mod.io.in.info.sel32 := sel32
      mod.io.in.info.sel64 := sel64
      mod.io.in.info.vm := vm
      mod.io.in.ctrl.widen := widen
      mod.io.in.ctrl.isSigned := isSigned
      mod.io.in.ctrl.adderCtrl.widenVs2 := widenVs2
      mod.io.in.ctrl.adderCtrl.isAddCarry := isAddCarry
      mod.io.in.ctrl.miscCtrl.isExt.valid := isExt
      mod.io.in.ctrl.miscCtrl.isExt.bits.isVf2 := isVf2
      mod.io.in.ctrl.miscCtrl.isExt.bits.isVf4 := isVf4
      mod.io.in.ctrl.miscCtrl.isExt.bits.isVf8 := isVf8
      mod.io.in.ctrl.miscCtrl.isMisc := isMisc
      mod.io.in.ctrl.miscCtrl.isNarrow := isNarrow
      mod.io.in.data.vs2 := vs2Vec(i)
      mod.io.in.data.vs1 := vs1Vec(i)
      mod.io.in.data.vs2Widen := vs2WidenVec(i)
      mod.io.in.data.vs1Widen := vs1WidenVec(i)
      mod.io.in.data.mask := maskVec(i)
      mod.io.in.data.vxrm := vxrm
  }
  
  val maskToMgu = Mux(isAddCarry, allMaskTrue, srcMask)
  val vdWiden = (VialuFixType.fmtIsVVW(fuOpType) || VialuFixType.fmtIsWVW(fuOpType)) & !isExt & !isDstMask
  val eew = Mux(vdWiden, vsew + 1.U, vsew)
  val vdIdx = Mux(isNarrow, vuopIdx(2, 1), vuopIdx)

  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := ta
  mgu.io.in.info.ma := ma
  mgu.io.in.info.vstart := 0.U
  mgu.io.in.info.vl := vl
  mgu.io.in.info.eew := eew
  mgu.io.in.info.vsew := vsew
  mgu.io.in.info.vdIdx := vdIdx
  mgu.io.in.isIndexedVls := false.B

  val maxUopIdx = VLEN / 8
  val numBytes = maxUopIdx
  
  val activeEn = Wire(UInt(numBytes.W))
  val agnosticEn = Wire(UInt(numBytes.W))
  activeEn := mgu.io.out.activeEn
  agnosticEn := mgu.io.out.agnosticEn

  val byte1s: UInt = (~0.U(8.W)).asUInt
  val agnosticVecByte = Wire(Vec(numBytes, UInt(8.W)))
  val oldVdVecByte = oldVd.asTypeOf(agnosticVecByte)
  for (i <- 0 until numBytes) {
    agnosticVecByte(i) := Mux(agnosticEn(i), byte1s, oldVdVecByte(i))
  }

  val activeEnS1 = RegEnable(activeEn, valid)
  val agnosticVecByteS1 = RegEnable(agnosticVecByte, valid)

  val vlIsZero = !vl.orR
  val vlIsZeroS1 = RegEnable(vlIsZero, valid)

  val vd = Cat(vIAluFixPoints.map(_.io.out.vd).reverse)
  val outNarrow = RegEnable(isNarrow, valid)
  val outVuopIdx0 = RegEnable(vuopIdx(0), valid)
  val narrowVd = Cat(vIAluFixPoints.map(_.io.out.narrowVd).reverse)
  val outOldVd = RegEnable(oldVd, valid)
  val outNarrowVd = Mux(outVuopIdx0,
    Cat(narrowVd, outOldVd(dataWidth / 2 - 1, 0)),
    Cat(outOldVd(dataWidth - 1, dataWidth / 2), narrowVd))
  
  val outVd = Mux(outNarrow, outNarrowVd, vd)

  val resVecByte = Wire(Vec(numBytes, UInt(8.W)))
  val vdVecByte = outVd.asTypeOf(resVecByte)

  for (i <- 0 until numBytes) {
    resVecByte(i) := Mux(activeEnS1(i), vdVecByte(i), agnosticVecByteS1(i))
  }

  val vsewOH = SewOH(vsew).oneHot
  val outVsewOH = RegEnable(vsewOH, valid)

  val addCarryCmpMask = Mux1H(outVsewOH, Seq(8, 4, 2, 1).map(i =>
    Cat(vIAluFixPoints.map(_.io.out.addCarryCmpMask(i - 1, 0)).reverse)  
  ))

  val dstMgu = Module(new DstMgu(dataWidth))
  dstMgu.io.in.valid := valid
  dstMgu.io.in.oldVd := oldVd
  dstMgu.io.in.mask := maskToMgu
  dstMgu.io.in.ma := ma
  dstMgu.io.in.eew := vsew
  dstMgu.io.in.vdIdx := vuopIdx(2, 0)
  dstMgu.io.in.toS1.vd := addCarryCmpMask
  dstMgu.io.in.toS1.oldVdS1 := outOldVd
  dstMgu.io.in.toS1.eewS1 := RegEnable(vsew, valid)
  dstMgu.io.in.toS1.vdIdxS1 := RegEnable(vuopIdx(2, 0), valid)

  private val outDstMask = RegEnable(isDstMask, valid)
  private val outOpMask = RegEnable(isOpMask, valid)

  private val outVxsat = Mux1H(Seq(
    (outNarrow & outVuopIdx0) -> Cat(Cat(vIAluFixPoints.map(_.io.out.vxsat(3, 0)).reverse), 0.U(8.W)),
    (outNarrow & !outVuopIdx0) -> Cat(vIAluFixPoints.map(_.io.out.vxsat(3, 0)).reverse),
    !outNarrow -> Cat(vIAluFixPoints.map(_.io.out.vxsat).reverse),
  ))

  mgtu.io.in.vd := Mux(outOpMask, vd, dstMgu.io.out.vd)
  mgtu.io.in.vl := RegEnable(vl, valid)

  io.out.vd := Mux(vlIsZeroS1, outOldVd, Mux(outDstMask, mgtu.io.out.vd, resVecByte.asUInt))
  io.out.vxsat := Mux(vlIsZeroS1, false.B, (outVxsat & activeEnS1).orR)
}
