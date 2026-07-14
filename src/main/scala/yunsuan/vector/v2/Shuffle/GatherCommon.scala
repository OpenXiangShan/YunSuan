package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util.{isPow2, log2Ceil}


object GatherCommon {
  abstract class GatherBundle extends Bundle with Config

  abstract class GatherModule extends Module with Config

  /**
   *
   * @param vlen
   * @param dlen
   * @param xlen
   * @param numUnits The number of all [[GatherRingUnit]] in [[GatherRing]]
   * @param unitIdx The index of this [[GatherRingUnit]] in [[GatherRing]]
   */
  class Param(
    val numPipeIn : Int,
    val numPipeOut: Int,
    val vlen      : Int = 128,
    val dlen      : Int = 128,
    val xlen      : Int = 64,
  ) {
    require(isPow2(dlen) && dlen >= 64)
    require(isPow2(vlen) && vlen >= 128)
  }

  object Param {
    def default: Param = new Param(4, 4)
  }

  trait Config {
    implicit val param: Param

    def XLEN: Int = param.xlen

    def VLEN: Int = param.vlen

    def DLEN: Int = param.dlen

    def VLENB: Int = VLEN / 8

    def DLENB: Int = DLEN / 8

    def MaxLMUL: Int = 8

    def MinDataWidth = 8

    def NumPipeIn = param.numPipeIn

    def NumPipeOut = param.numPipeOut

    def NumElem = DLEN / MinDataWidth

    def IndexWidthInDLEN: Int = log2Ceil(NumElem)

    def IndexWidth: Int = log2Ceil(VLEN * MaxLMUL / MinDataWidth)

    def NumUnits: Int = VLEN / DLEN * MaxLMUL

    def UnitIdx(): UInt = UInt(TableIdxWidth.W)

    def BlockSize(): UInt = UInt((TableIdxWidth + 1).W)

    def TableIdxWidth: Int = log2Ceil(NumUnits)

    def VlWidth: Int = IndexWidth + 1

    def Elem(): UInt = UInt(MinDataWidth.W)

    def Index() = UInt(IndexWidth.W)

    def PipeInIdx() = UInt(log2Ceil(NumPipeIn).W)
  }

  class FakeRobPtr extends Bundle {
    val flag = Bool()
    val value = UInt(8.W)
  }

  object FakeRobPtr {
    def apply() = new FakeRobPtr
  }
}
