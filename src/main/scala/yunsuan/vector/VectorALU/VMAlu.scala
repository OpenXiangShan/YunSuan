package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
class VMAlu extends Module {
    val in = IO(Input(Valid(new VMAlu.In)))
    val out = IO(Output(new VMAlu.Out))

    val VLEN = VMAlu.VLEN
    val XLEN = VMAlu.XLEN
    val vlenb = VLEN / 8

    val fire = in.valid
    val vm = in.bits.data.vm
    val vl = in.bits.data.vl
    val uopIdx = in.bits.data.uopIdx
    val vSrc = in.bits.data.vSrc
    val mask = in.bits.data.mask
    val sel8 = in.bits.ctrl.sel8
    val sel16 = in.bits.ctrl.sel16
    val sel32 = in.bits.ctrl.sel32
    val sel64 = in.bits.ctrl.sel64
    val isVcpopV = in.bits.ctrl.isVcpopV
    val isVmsbf = in.bits.ctrl.isVmsbf
    val isVmsif = in.bits.ctrl.isVmsif
    val isVmsof = in.bits.ctrl.isVmsof
    val isViota = in.bits.ctrl.isViota

    val activeVSrc = Wire(UInt(VLEN.W))
    activeVSrc := Cat((0 until VLEN).reverse.map { i =>
      vSrc(i) & (mask(i) | vm) & (i.U < vl)
    })

    val uopBase = calcVidBase(uopIdx)

    val vidVdSew8 = VecInit.tabulate(vlenb)(i => vidByte(i))
    val vidVdSew16 = VecInit.tabulate(vlenb)(i => if (i % 2 == 0) vidByte(i / 2) else 0.U(8.W))
    val vidVdSew32 = VecInit.tabulate(vlenb)(i => if (i % 4 == 0) vidByte(i / 4) else 0.U(8.W))
    val vidVdSew64 = VecInit.tabulate(vlenb)(i => if (i % 8 == 0) vidByte(i / 8) else 0.U(8.W))
    val vidVd = Mux1H(Seq(
      sel8 -> vidVdSew8,
      sel16 -> vidVdSew16,
      sel32 -> vidVdSew32,
      sel64 -> vidVdSew64
    ))

    out.ex0.vData := Cat(vidVd.reverse)
    out.ex0.iData := 0.U

    val activeVSrcRegS1 = RegEnable(activeVSrc, fire)
    val fireRegS1 = RegNext(fire)
    val sel8RegS1 = RegEnable(sel8, fire)
    val sel16RegS1 = RegEnable(sel16, fire)
    val sel32RegS1 = RegEnable(sel32, fire)
    val sel64RegS1 = RegEnable(sel64, fire)
    val uopBaseRegS1 = RegEnable(uopBase, fire)
    val isVcpopVRegS1 = RegEnable(isVcpopV, fire)
    val isVmsbfRegS1 = RegEnable(isVmsbf, fire)
    val isVmsifRegS1 = RegEnable(isVmsif, fire)
    val isVmsofRegS1 = RegEnable(isVmsof, fire)
    val isViotaRegS1 = RegEnable(isViota, fire)

    val vmsbf = VMAlu.sbf(activeVSrcRegS1)
    val vmsif = Cat(vmsbf(VLEN - 2, 0), 1.U(1.W))
    val vmsof = ~vmsbf & vmsif
    val vfirst = Cat(0.U((VLEN - XLEN).W), Mux(
      activeVSrcRegS1.orR,
      VMAlu.vfirst(activeVSrcRegS1).pad(XLEN),
      Fill(XLEN, 1.U(1.W))
    ))

    val precedingMask = ((1.U((VLEN + 1).W) << uopBaseRegS1) - 1.U)(VLEN - 1, 0)
    val precedingCnt = PopCount(activeVSrcRegS1 & precedingMask)
    val uopWindow = (activeVSrcRegS1 >> uopBaseRegS1)(vlenb - 1, 0)

    val uopPrefixCntSew8 = uopPrefixCount(vlenb)
    val uopPrefixCntSew16 = uopPrefixCount(vlenb / 2)
    val uopPrefixCntSew32 = uopPrefixCount(vlenb / 4)
    val uopPrefixCntSew64 = uopPrefixCount(vlenb / 8)
    val uopPrefixCnt = Mux1H(Seq(
      sel8RegS1 -> uopPrefixCntSew8,
      sel16RegS1 -> uopPrefixCntSew16,
      sel32RegS1 -> uopPrefixCntSew32,
      sel64RegS1 -> uopPrefixCntSew64
    ))

    out.ex1.iData := vfirst
    out.ex1.vData := Mux1H(Seq(
      isVmsbfRegS1 -> vmsbf,
      isVmsifRegS1 -> vmsif,
      isVmsofRegS1 -> vmsof
    ))

    val precedingCntRegS2 = RegEnable(precedingCnt, fireRegS1)
    val uopPrefixCntRegS2 = RegEnable(uopPrefixCnt, fireRegS1)
    val sel8RegS2 = RegEnable(sel8RegS1, fireRegS1)
    val sel16RegS2 = RegEnable(sel16RegS1, fireRegS1)
    val sel32RegS2 = RegEnable(sel32RegS1, fireRegS1)
    val sel64RegS2 = RegEnable(sel64RegS1, fireRegS1)
    val isVcpopVRegS2 = RegEnable(isVcpopVRegS1, fireRegS1)
    val isViotaRegS2 = RegEnable(isViotaRegS1, fireRegS1)
    
    val eleCntRegS2 = Mux1H(Seq(
      sel8RegS2 -> 16.U(5.W),
      sel16RegS2 -> 8.U(5.W),
      sel32RegS2 -> 4.U(5.W),
      sel64RegS2 -> 2.U(5.W)
    ))

    val viotaCnt = VecInit.tabulate(vlenb + 1)(cntSum)
    val vcpopMRes = (precedingCntRegS2 + uopPrefixCntRegS2(eleCntRegS2))(7, 0).pad(VLEN)
    val vcpopVRes = uopPrefixCntRegS2(eleCntRegS2).pad(VLEN)
    val viotaVdSew8 = VecInit.tabulate(vlenb)(i => viotaCnt(i))
    val viotaVdSew16 = VecInit.tabulate(vlenb)(i => if (i % 2 == 0) viotaCnt(i / 2) else 0.U(8.W))
    val viotaVdSew32 = VecInit.tabulate(vlenb)(i => if (i % 4 == 0) viotaCnt(i / 4) else 0.U(8.W))
    val viotaVdSew64 = VecInit.tabulate(vlenb)(i => if (i % 8 == 0) viotaCnt(i / 8) else 0.U(8.W))
    val viotaVd = Mux1H(Seq(
      sel8RegS2 -> viotaVdSew8,
      sel16RegS2 -> viotaVdSew16,
      sel32RegS2 -> viotaVdSew32,
      sel64RegS2 -> viotaVdSew64
    ))

    out.ex2.iData := vcpopMRes
    out.ex2.vData := Mux1H(Seq(
      isVcpopVRegS2 -> vcpopVRes,
      isViotaRegS2 -> Cat(viotaVd.reverse)
    ))
    
    def calcVidBase(idx: UInt): UInt = Mux1H(Seq(
      sel8 -> Cat(idx, 0.U(4.W)).pad(8),
      sel16 -> Cat(idx, 0.U(3.W)).pad(8),
      sel32 -> Cat(idx, 0.U(2.W)).pad(8),
      sel64 -> Cat(idx, 0.U(1.W)).pad(8)
    ))

    def uopPrefixCount(activeCnt: Int): Vec[UInt] = VecInit.tabulate(vlenb + 1) { i =>
      if (i == 0 || i > activeCnt) 0.U(5.W) else PopCount(uopWindow(i - 1, 0))
    }

    def vidByte(idx: Int): UInt = (uopBase + idx.U(8.W))(7, 0)

    def cntSum(idx: Int): UInt = {
      val sum = precedingCntRegS2 + uopPrefixCntRegS2(idx)
      sum(7, 0)
    }

}

object VMAlu{

  def VLEN = 128
  def XLEN = 64
  def vlWidth = log2Ceil(VLEN + 1)
  def uopidxWidth = 3

  class In extends Bundle {
    val data = new Data()
    val ctrl = new Ctrl()
  }

  class Data extends Bundle{
    val vm = Bool()
    val vl = UInt(vlWidth.W)
    val uopIdx = UInt(uopidxWidth.W)

    val vSrc = UInt(VLEN.W)
    val mask = UInt(VLEN.W)
  }

  class Out extends Bundle {
    val ex0 = new OutData
    val ex1 = new OutData
    val ex2 = new OutData
  }
  class OutData extends Bundle {
    val vData = UInt(VLEN.W)
    val iData = UInt(XLEN.W)
  }

  class Ctrl extends Bundle {
    val sel8 = Bool()
    val sel16 = Bool()
    val sel32 = Bool()
    val sel64 = Bool()

    val isVcpopV  = Bool()
    val isVmsbf  = Bool()
    val isVmsif  = Bool()
    val isVmsof  = Bool()
    val isViota  = Bool()
  }

  def sbf(data: UInt): UInt = {
    val w = data.getWidth
    val result = Wire(UInt(w.W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = sbf(data(w - 1, w / 2))
      val lo = sbf(data(w / 2 - 1, 0))
      result := Mux(lo(w / 2 - 1), Cat(hi, lo), Cat(0.U((w / 2).W), lo))
    }
    result
  }

  def vfirst(data: UInt): UInt = {
    val w = data.getWidth
    val logW = log2Ceil(w) // 1 -> 0, 2 -> 1, 4 -> 2
    val result = Wire(UInt((logW + 1).W))
    if (w == 1) {
      result := Mux(data(0), 0.U(1.W), 1.U(1.W))
    } else {
      val hi = vfirst(data(w - 1, w / 2))
      val lo = vfirst(data(w / 2 - 1, 0))
      result := Mux(!lo(logW - 1), Cat(0.U(1.W), lo),
        if (w == 2) Cat(hi(logW - 1), 1.U(1.W)) else Cat(hi(logW - 1), 1.U(1.W), hi(logW - 2, 0)))
    }
    result
  }

}
