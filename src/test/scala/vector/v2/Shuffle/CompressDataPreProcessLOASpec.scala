package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class CompressDataPreProcessLOASpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  val dlenb = dlen / 8
  val splittedParts = 8 * vlen / dlen

  /** Golden: compress active bytes to the front using LOA algorithm */
  def goldenCompressLOA(data: Seq[Int], mask: BigInt): Seq[Option[Int]] = {
    val selected = mask & ((BigInt(1) << numElem) - 1)
    val activeBytes = (0 until numElem).filter(i => ((selected >> i) & 1) == 1)
    val result = Array.fill(numElem)(Option.empty[Int])
    activeBytes.zipWithIndex.foreach { case (srcPos, dstPos) =>
      result(dstPos) = Some(data(srcPos))
    }
    result.toSeq
  }

  def pDP(dut: CompressDataPreProcessLOA, mask: BigInt, data: Seq[Int], groupSel: Int): Unit = {
    dut.in.groupSel.poke(groupSel)
    dut.in.byteMask.poke(mask)
    for (i <- 0 until numElem) {
      dut.in.data(i).poke(data(i))
    }
  }

  def eDP(dut: CompressDataPreProcessLOA, golden: Seq[Option[Int]]): Unit = {
    for (i <- 0 until numElem) {
      golden(i) match {
        case Some(v) =>
          dut.out.data(i).valid.expect(true.B)
          dut.out.data(i).bits.expect(v.U)
        case None =>
          dut.out.data(i).valid.expect(false.B)
      }
    }
  }

  describe("CompressDataPreProcessLOA") {
    it("all bytes active") {
      simulate(new CompressDataPreProcessLOA()(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        val golden = goldenCompressLOA(data, mask)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, golden)
      }
    }
    it("no bytes active") {
      simulate(new CompressDataPreProcessLOA()(GatherParam.default)) { dut =>
        val mask = BigInt(0)
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        val golden = goldenCompressLOA(data, mask)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, golden)
      }
    }
    it("alternating mask") {
      simulate(new CompressDataPreProcessLOA()(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        val golden = goldenCompressLOA(data, mask)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, golden)
      }
    }
    it("single active byte") {
      simulate(new CompressDataPreProcessLOA()(GatherParam.default)) { dut =>
        val mask = BigInt(1) << 7
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        val golden = goldenCompressLOA(data, mask)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, golden)
      }
    }
  }
}
