package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class CompressDataPreProcessSpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  /** Golden: compress active bytes to the front */
  def goldenCompress(data: Seq[Int], mask: BigInt): Seq[Option[Int]] = {
    val selected = mask & ((BigInt(1) << numElem) - 1)
    val activeBytes = (0 until numElem).filter(i => ((selected >> i) & 1) == 1)
    val result = Array.fill(numElem)(Option.empty[Int])
    activeBytes.zipWithIndex.foreach { case (srcPos, dstPos) =>
      result(dstPos) = Some(data(srcPos))
    }
    result.toSeq
  }

  def pDP(dut: CompressDataPreProcess, mask: BigInt, data: Seq[Int], groupSel: Int): Unit = {
    dut.in.groupSel.poke(groupSel)
    dut.in.byteMask.poke(mask)
    for (i <- 0 until numElem) {
      dut.in.data(i).poke(data(i))
    }
  }

  def eDP(dut: CompressDataPreProcess, golden: Seq[Option[Int]]): Unit = {
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

  describe("LOA") {
    it("all bytes active") {
      simulate(new CompressDataPreProcess(useLZA = false)(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
    it("no bytes active") {
      simulate(new CompressDataPreProcess(useLZA = false)(GatherParam.default)) { dut =>
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, BigInt(0), data, groupSel = 0)
        eDP(dut, goldenCompress(data, BigInt(0)))
      }
    }
    it("alternating mask") {
      simulate(new CompressDataPreProcess(useLZA = false)(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
    it("single active byte") {
      simulate(new CompressDataPreProcess(useLZA = false)(GatherParam.default)) { dut =>
        val mask = BigInt(1) << 7
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
  }

  describe("LZA") {
    it("all bytes active") {
      simulate(new CompressDataPreProcess(useLZA = true)(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
    it("no bytes active") {
      simulate(new CompressDataPreProcess(useLZA = true)(GatherParam.default)) { dut =>
        val data = (0 until numElem).map(i => i & 0xff)
        pDP(dut, BigInt(0), data, groupSel = 0)
        eDP(dut, goldenCompress(data, BigInt(0)))
      }
    }
    it("alternating mask") {
      simulate(new CompressDataPreProcess(useLZA = true)(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)
        val data = (0 until numElem).map(i => (i * 0x11 + 0xAB) & 0xff)
        pDP(dut, mask, data, groupSel = 0)
        eDP(dut, goldenCompress(data, mask))
      }
    }
  }
}
