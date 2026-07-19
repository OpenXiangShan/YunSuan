package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class UnzipIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with GatherSpecUtils {



  // Golden model: unzip index
  // times = unzipn * sewb
  // index = (i % vlenb) / sewb * times + (i / vlenb) * sewb + i % sewb
  def goldenUnzip(unzipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sewbVal = sewb(sew)
    val times = unzipn * sewbVal
    (0 until numElem).map { elemIdx =>
      val i = uopIdx * numElem + elemIdx
      if (i < unzipn * vlenb) {
        Some((i % vlenb) / sewbVal * times + (i / vlenb) * sewbVal + i % sewbVal)
      } else {
        None
      }
    }
  }


  def pUnzip(dut: UnzipIndexGenDecodeTable, unzipn: Int, sew: Int, uopIdx: Int): Unit = {
    dut.in.zipNum.poke(unzipn - 1)
    pSew(dut.in.sew, sew)
    dut.in.uopIdx.poke(uopIdx)
  }

  describe("UnzipIndexGen") {
    it("unzipn=2, SEW=32, uopIdx=0") {
      simulate(new UnzipIndexGenDecodeTable()(GatherParam.default)) { dut =>
        val golden = goldenUnzip(unzipn = 2, sew = 32, uopIdx = 0)
        pUnzip(dut, unzipn = 2, sew = 32, uopIdx = 0)
        dut.clock.step()
        for (i <- 0 until numElem) {
          golden(i).foreach { idx => dut.out.index(i).bits.expect(idx.U) }
        }
      }
    }
    it("unzipn=4, SEW=16, uopIdx=0") {
      simulate(new UnzipIndexGenDecodeTable()(GatherParam.default)) { dut =>
        val golden = goldenUnzip(unzipn = 4, sew = 16, uopIdx = 0)
        pUnzip(dut, unzipn = 4, sew = 16, uopIdx = 0)
        dut.clock.step()
        for (i <- 0 until numElem) {
          golden(i).foreach { idx => dut.out.index(i).bits.expect(idx.U) }
        }
      }
    }
    it("unzipn=2, SEW=8, uopIdx=0") {
      simulate(new UnzipIndexGenDecodeTable()(GatherParam.default)) { dut =>
        val golden = goldenUnzip(unzipn = 2, sew = 8, uopIdx = 0)
        pUnzip(dut, unzipn = 2, sew = 8, uopIdx = 0)
        dut.clock.step()
        for (i <- 0 until numElem) {
          golden(i).foreach { idx => dut.out.index(i).bits.expect(idx.U) }
        }
      }
    }
    it("unzipn=8, SEW=64, uopIdx=0") {
      simulate(new UnzipIndexGenDecodeTable()(GatherParam.default)) { dut =>
        val golden = goldenUnzip(unzipn = 8, sew = 64, uopIdx = 0)
        pUnzip(dut, unzipn = 8, sew = 64, uopIdx = 0)
        dut.clock.step()
        for (i <- 0 until numElem) {
          golden(i).foreach { idx => dut.out.index(i).bits.expect(idx.U) }
        }
      }
    }
  }
}
