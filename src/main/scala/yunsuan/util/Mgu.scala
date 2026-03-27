package yunsuan.util

import chisel3._
import chisel3.util._
import yunsuan.util.Utils.VecDataToMaskDataVec

class Mgu(vlen: Int) extends Module {
  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes)

  val io = IO(new MguIO(vlen))

  val in = io.in
  val info = in.info
  val mask = in.mask
  val isIndexedVls = in.isIndexedVls
  val vdIdx = info.vdIdx
  val vsew = info.vsew
  val eew = info.eew

  private val maskTailGen = Module(new ByteMaskTailGen(vlen))
  
  private val realEw = Mux(isIndexedVls, vsew, eew)
  private val maskDataVec: Vec[UInt] = VecDataToMaskDataVec(mask, realEw)
  protected lazy val maskUsed = maskDataVec(vdIdx)

  maskTailGen.io.in.begin := info.vstart
  maskTailGen.io.in.end := info.vl
  maskTailGen.io.in.vma := info.ma
  maskTailGen.io.in.vta := info.ta
  maskTailGen.io.in.vsew := realEw
  maskTailGen.io.in.maskUsed := maskUsed
  maskTailGen.io.in.vdIdx := vdIdx

  private val activeEn = maskTailGen.io.out.activeEn
  private val agnosticEn = maskTailGen.io.out.agnosticEn

  io.out.activeEn := activeEn
  io.out.agnosticEn := agnosticEn
}

class MguIO(vlen: Int) extends Bundle {
  val in = Input(new MguInputBundle(vlen))
  val out = Output(new MguOutputBundle(vlen))
}

class MguInputBundle(vlen: Int) extends Bundle {
  val mask = UInt(vlen.W)
  val info = new VecInfo
  val isIndexedVls = Bool()
}

class MguOutputBundle(vlen: Int) extends Bundle {
  val activeEn = UInt((vlen / 8).W)
  val agnosticEn = UInt((vlen / 8).W)
}

class VecInfo extends Bundle {
  val ta = Bool()
  val ma = Bool()
  val vstart = UInt(7.W)
  val vl = UInt(8.W)
  val eew = UInt(2.W)
  val vsew = UInt(2.W)
  val vdIdx = UInt(3.W)
}
