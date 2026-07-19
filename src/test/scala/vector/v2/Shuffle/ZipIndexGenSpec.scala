package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class ZipIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {



  // Golden model: zip index
  // i = uopIdx * numElem + elemIdx
  // loopLength = zipn * sew / 8
  // index = (i / loopLength) * sewb + (i % loopLength) / sewb * vlenb + i % sewb
  def goldenZip(zipn: Int, sew: Int, uopIdx: Int): Seq[Option[Int]] = {
    val sewbVal = sewb(sew)
    val loopLength = zipn * sewbVal
    (0 until numElem).map { elemIdx =>
      val i = uopIdx * numElem + elemIdx
      if (i < zipn * vlenb) {
        val q = i / loopLength
        val r = i % loopLength
        Some(q * sewbVal + r / sewbVal * vlenb + i % sewbVal)
      } else {
        None
      }
    }
  }


  def pZip(dut: ZipIndexGenHandWrite, zipn: Int, sew: Int, uopIdx: Int): Unit = {
    dut.in.zipNum.poke(zipn - 1)  // 0-based: 2->1, 3->2, ..., 8->7
    pSew(dut.in.sew, sew)
    dut.in.uopIdx.poke(uopIdx)
  }

  def eZip(dut: ZipIndexGenHandWrite, golden: Seq[Option[Int]]): Unit = {
    for (i <- 0 until numElem) {
      golden(i) match {
        case Some(idx) =>
          dut.out.index(i).valid.expect(true.B)
          dut.out.index(i).bits.expect(idx.U)
        case None =>
          dut.out.index(i).valid.expect(false.B)
      }
    }
  }

  describe("ZipIndexGenHandWrite") {
    describe("zipn=2") {
      it("SEW=32, uopIdx=0") {
        simulate(new ZipIndexGenHandWrite()(GatherParam.default)) { dut =>
          val golden = goldenZip(zipn = 2, sew = 32, uopIdx = 0)
          pZip(dut, zipn = 2, sew = 32, uopIdx = 0)
          dut.clock.step()
          eZip(dut, golden)
        }
      }
      it("SEW=8, uopIdx=0") {
        simulate(new ZipIndexGenHandWrite()(GatherParam.default)) { dut =>
          val golden = goldenZip(zipn = 2, sew = 8, uopIdx = 0)
          pZip(dut, zipn = 2, sew = 8, uopIdx = 0)
          dut.clock.step()
          eZip(dut, golden)
        }
      }
    }
    describe("zipn=4") {
      it("SEW=16, uopIdx=0") {
        simulate(new ZipIndexGenHandWrite()(GatherParam.default)) { dut =>
          val golden = goldenZip(zipn = 4, sew = 16, uopIdx = 0)
          pZip(dut, zipn = 4, sew = 16, uopIdx = 0)
          dut.clock.step()
          eZip(dut, golden)
        }
      }
    }
    describe("zipn=8") {
      it("SEW=64, uopIdx=0") {
        simulate(new ZipIndexGenHandWrite()(GatherParam.default)) { dut =>
          val golden = goldenZip(zipn = 8, sew = 64, uopIdx = 0)
          pZip(dut, zipn = 8, sew = 64, uopIdx = 0)
          dut.clock.step()
          eZip(dut, golden)
        }
      }
    }
  }

  describe("ZipIndexGenDecodeTable") {
    // BUG: decode input does not include uopIdx — cannot distinguish uop-level indices
    it("zipn=2, SEW=32 — decode lacks uopIdx") {
      simulate(new ZipIndexGenDecodeTable()(GatherParam.default)) { dut =>
        pSew(dut.in.sew, 32)
        dut.in.zipNum.poke(1)  // zipn=2
        dut.in.uopIdx.poke(0)
        dut.clock.step()
        // Expected: HandWrite golden. Actual: only matches uopIdx=0 for all uops.
        val hwGolden = goldenZip(zipn = 2, sew = 32, uopIdx = 0)
        for (i <- 0 until numElem) {
          hwGolden(i).foreach { idx => dut.out.index(i).bits.expect(idx.U) }
        }
      }
    }
  }
}
