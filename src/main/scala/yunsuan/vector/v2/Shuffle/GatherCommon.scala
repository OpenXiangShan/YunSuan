package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util.{isPow2, log2Ceil, log2Up, Mux1H}


object GatherCommon {
  abstract class GatherBundle extends Bundle with Config

  abstract class GatherModule extends Module with Config

  /**
   *
   * @param vlen
   * @param dlen
   * @param xlen
   * @param numUnits The number of all [[GatherRingUnit]] in [[GatherRing]]
   */
  class GatherParam(
    val numPipeIn : Int,
    val numPipeOut: Int,
    val vlen      : Int = 128,
    val dlen      : Int = 128,
    val xlen      : Int = 64,
    val debug     : Boolean = false,
    val numUnits  : Int = 16,
    // Number of register stages from issue to entering the exu; the vperm wakeup
    // delay is computed as issue2exu - counter (delay=0 corresponds to wbM3)
    val issue2exu : Int = 3,
  ) {
    require(numPipeIn > 0)
    require(numPipeOut > 0)
    require(isPow2(dlen) && dlen >= 64)
    require(isPow2(vlen) && vlen >= 128)
    require(vlen >= dlen)
    require(isPow2(numUnits) && numUnits >= 8)
    require(issue2exu > 0)
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

    def NumUnits: Int = param.numUnits

    def UnitIdx(): UInt = UInt(TableIdxWidth.W)

    def BlockSize(): UInt = UInt((TableIdxWidth + 1).W)

    /** Real field (table/index) count of a block: == block size for full blocks,
      * 2..size-1 for hole blocks (segment 3/5/6/7), 0 = empty. */
    def BlockFieldNum(): UInt = UInt((TableIdxWidth + 1).W)

    /** One-hot blockSize (bit0..3 = 1/2/4/8) -> block element count (1/2/4/8). */
    def blockSizeCount(blockSize: UInt): UInt = Mux1H(Seq(
      blockSize(0) -> 1.U,
      blockSize(1) -> 2.U,
      blockSize(2) -> 4.U,
      blockSize(3) -> 8.U,
    ))

    def TableIdxWidth: Int = log2Ceil(NumUnits)

    def VlWidth: Int = IndexWidth + 1

    def Elem(): UInt = UInt(MinDataWidth.W)

    def Index() = UInt(IndexWidth.W)

    def PipeInIdxWidth: Int = 1.max(log2Ceil(NumPipeIn))

    def PipeInIdx() = UInt(PipeInIdxWidth.W)

    def UopIdx() = UInt(log2Ceil(VLEN / DLEN * 8).W)

    def WakeDelayWidth: Int = log2Up(param.issue2exu + 1)
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
