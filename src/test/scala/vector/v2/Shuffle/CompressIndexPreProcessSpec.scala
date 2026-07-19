package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class CompressIndexPreProcessSpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  val dlenb = dlen / 8
  val splittedParts = 8 * vlen / dlen

  /** Golden: leadingSums(j) = popcount of first dlenb*(j+1) bytes of mask */
  def goldenLeadingSums(mask: BigInt): Seq[Int] = {
    (0 until splittedParts).map { j =>
      val bits = (dlenb * (j + 1)).min(vlen)
      val window = mask & ((BigInt(1) << bits) - 1)
      window.bitCount
    }
  }

  def pPreProcess(dut: CompressIndexPreProcess, mask: BigInt, groupSel: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.groupSel.poke(groupSel)
    dut.in.byteMask.poke(mask)
  }

  describe("CompressIndexPreProcess") {
    describe("leading sums") {
      it("all zeros") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = BigInt(0)
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 0)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
      it("groupSel=0 all ones") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = (BigInt(1) << vlen) - 1
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 0)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
      it("groupSel=3 identical result") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = (BigInt(1) << vlen) - 1
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 3)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
      it("groupSel=7 identical result") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = (BigInt(1) << vlen) - 1
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 7)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
      it("alternating bytes") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = BigInt("5555", 16)
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 0)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
      it("single bit set") {
        simulate(new CompressIndexPreProcess()(GatherParam.default)) { dut =>
          val mask = BigInt(1) << 5
          val golden = goldenLeadingSums(mask)
          pPreProcess(dut, mask, groupSel = 0)
          for (j <- 0 until splittedParts) {
            dut.out.leadingSums(j).expect(golden(j).U)
          }
        }
      }
    }
  }
}
