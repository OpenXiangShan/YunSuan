package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class CompressDataPreProcessLZASpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  /** Golden: same compress result as LOA, just different algorithm */
  def goldenCompress(data: Seq[Int], mask: BigInt): Seq[Option[Int]] = {
    val selected = mask & ((BigInt(1) << numElem) - 1)
    val activeBytes = (0 until numElem).filter(i => ((selected >> i) & 1) == 1)
    val result = Array.fill(numElem)(Option.empty[Int])
    activeBytes.zipWithIndex.foreach { case (srcPos, dstPos) =>
      result(dstPos) = Some(data(srcPos))
    }
    result.toSeq
  }

  def pDP(dut: CompressDataPreProcessLZA, mask: BigInt, data: Seq[Int], groupSel: Int): Unit = {
    dut.in.groupSel.poke(groupSel)
    dut.in.byteMask.poke(mask)
    for (i <- 0 until numElem) {
      dut.in.data(i).poke(data(i))
    }
  }

  def eDP(dut: CompressDataPreProcessLZA, golden: Seq[Option[Int]]): Unit = {
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

  describe("CompressDataPreProcessLZA") {
    it("all bytes active") {
      simulate(new CompressDataPreProcessLZA()(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
    it("no bytes active") {
      simulate(new CompressDataPreProcessLZA()(GatherParam.default)) { dut =>
        pDP(dut, BigInt(0), (0 until numElem).map(i => i & 0xff), groupSel = 0)
        eDP(dut, goldenCompress((0 until numElem).map(i => i & 0xff), BigInt(0)))
      }
    }
    it("alternating mask") {
      simulate(new CompressDataPreProcessLZA()(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
  }
}
