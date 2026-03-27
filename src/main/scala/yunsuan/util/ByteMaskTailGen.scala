package yunsuan.util

import chisel3._
import chisel3.util._
import yunsuan.vector.SewOH
import yunsuan.util.LookupTree

import math.pow

class ByteMaskTailGenIO(vlen: Int) extends Bundle {
  private val numBytes = vlen / 8
  private val maxVLMUL = 8
  private val maxVLMAX = 8 * 16 // TODO: parameterize this
  private val elemIdxWidth = log2Up(maxVLMAX + 1)
  println(s"elemIdxWidth: $elemIdxWidth")

  val in = Input(new Bundle {
    val begin = UInt(elemIdxWidth.W)
    val end = UInt(elemIdxWidth.W)
    val vma = Bool()
    val vta = Bool()
    val vsew = UInt(2.W)
    val maskUsed = UInt(numBytes.W)
    val vdIdx = UInt(3.W)
  })
  val out = Output(new Bundle {
    val activeEn   = UInt(numBytes.W)
    val agnosticEn = UInt(numBytes.W)
  })
}

class ByteMaskTailGen(vlen: Int) extends Module {
  require(isPow2(vlen))

  private val numBytes = vlen / 8
  private val byteWidth = log2Up(numBytes) // vlen=128, numBytes=16, byteWidth=log2(16)=4
  private val maxVLMUL = 8
  private val maxVLMAX = 8 * 16 // TODO: parameterize this
  private val elemIdxWidth = log2Up(maxVLMAX + 1)

  println(s"numBytes: ${numBytes}, byteWidth: ${byteWidth}")

  val io = IO(new ByteMaskTailGenIO(vlen))

  private val eewOH = SewOH(io.in.vsew).oneHot

  private val startBytes = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.begin(elemIdxWidth - 1 - x, 0) << x)).asUInt
  private val vlBytes    = Mux1H(eewOH, Seq.tabulate(4)(x => io.in.end(elemIdxWidth - 1 - x, 0) << x)).asUInt
  private val vdIdx      = io.in.vdIdx

  private val prestartEn = UIntToContLow1s(startBytes, maxVLMAX)
  private val bodyEn = UIntToContLow0s(startBytes, maxVLMAX) & UIntToContLow1s(vlBytes, maxVLMAX)
  private val tailEn = UIntToContLow0s(vlBytes, maxVLMAX)
  private val prestartEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> prestartEn((i+1)*numBytes - 1, i*numBytes)))
  private val bodyEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> bodyEn((i+1)*numBytes - 1, i*numBytes)))
  private val tailEnInVd = LookupTree(vdIdx, (0 until maxVLMUL).map(i => i.U -> tailEn((i+1)*numBytes - 1, i*numBytes)))

  private val maskEn = MaskExtractor(vlen)(io.in.maskUsed, io.in.vsew)
  private val maskOffEn = (~maskEn).asUInt
  private val maskAgnosticEn = Mux(io.in.vma, maskOffEn, 0.U) & bodyEnInVd

  private val tailAgnosticEn = Mux(io.in.vta, tailEnInVd, 0.U)

  private val activeEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), bodyEnInVd & maskEn)
  private val agnosticEn = Mux(io.in.begin >= io.in.end, 0.U(numBytes.W), maskAgnosticEn | tailAgnosticEn)

  io.out.activeEn := activeEn
  io.out.agnosticEn := agnosticEn
}
