package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util.{isPow2, log2Ceil, Mux1H}


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
  class GatherParam(
    val numPipeIn : Int,
    val numPipeOut: Int,
    val vlen      : Int = 128,
    val dlen      : Int = 128,
    val xlen      : Int = 64,
    val debug     : Boolean = false,
  ) {
    require(isPow2(dlen) && dlen >= 64)
    require(isPow2(vlen) && vlen >= 128)
  }

  object GatherParam {
    def default: GatherParam = new GatherParam(4, 4)

    def debug: GatherParam = new GatherParam(4, 4, debug = true)
  }

  trait Config {
    implicit val param: GatherParam

    def XLEN: Int = param.xlen

    def VLEN: Int = param.vlen

    def DLEN: Int = param.dlen

    def VLENB: Int = VLEN / 8

    def DLENB: Int = DLEN / 8

    def MaxLMUL: Int = 8

    def MinDataWidth = 8

    def ResWidth = MinDataWidth

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

  /**
   * Check if the index is greater than or equal to real vlmax for the given LMUL.
   * Used by IndexGen modules to determine fillZeros for out-of-range indices.
   */
  def geRealVlmax(lmul: GatherLmulBundle, index: UInt): Bool = {
    Mux1H(Seq(
      lmul.mf8 -> (index.head(6) =/= 0.U),
      lmul.mf4 -> (index.head(5) =/= 0.U),
      lmul.mf2 -> (index.head(4) =/= 0.U),
      lmul.m1  -> (index.head(3) =/= 0.U),
      lmul.m2  -> (index.head(2) =/= 0.U),
      lmul.m4  -> (index.head(1) =/= 0.U),
      // m8: index never >= vlmax (vlmax covers full index space)
    ))
  }

  class FakeRobPtr extends Bundle {
    val flag = Bool()
    val value = UInt(8.W)
  }

  object FakeRobPtr {
    def apply() = new FakeRobPtr
  }
}
