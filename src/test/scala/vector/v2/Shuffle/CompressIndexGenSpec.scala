package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class CompressIndexGenSpec extends SpecBase with GatherSpecUtils {

  val dlenb = dlen / 8
  val splittedParts = 8 * vlen / dlen

  /** Compute leading sums: popcount of first dlenb*(j+1) bytes of VLEN-wide mask */
  def leadingSums(mask: BigInt): Seq[Int] = {
    (0 until splittedParts).map { j =>
      val bits = (dlenb * (j + 1)).min(vlen)
      (mask & ((BigInt(1) << bits) - 1)).bitCount
    }
  }

  /** Golden: compress output for given mask and uopIdx */
  def goldenCompress(mask: BigInt, uopIdx: Int): (Seq[(Int, Boolean)], Int) = {
    val sums = leadingSums(mask)
    val total = sums.last
    val results = (0 until numElem).map { i =>
      val globalPos = uopIdx * numElem + i
      val geCount = sums.count(s => globalPos >= s)
      val index = geCount * numElem + i
      val valid = globalPos < total
      (index, valid)
    }
    (results, total)
  }

  def pCompress(dut: CompressIndexGen, mask: BigInt, uopIdx: Int): Unit = {
    dut.in.valid.poke(true)
    dut.in.uopIdx.poke(uopIdx)
    dut.in.byteMask.poke(mask)
    dut.in.vs3.poke(0)
    for (i <- 0 until numElem) {
      dut.in.data(i).poke(i + uopIdx * numElem)
    }
  }

  describe("CompressIndexGen") {
    it("all bytes active") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1
        val (golden, total) = goldenCompress(mask, uopIdx = 0)
        pCompress(dut, mask, uopIdx = 0)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
    it("alternating mask") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)
        val (golden, total) = goldenCompress(mask, uopIdx = 0)
        pCompress(dut, mask, uopIdx = 0)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
    it("no bytes active") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        pCompress(dut, BigInt(0), uopIdx = 0)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          dut.out.index.indexValid(i).expect(false.B)
        }
        dut.out.count1s.expect(0.U)
      }
    }
    it("uopIdx=1, all active in chunk 1") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask = ((BigInt(1) << numElem) - 1) << numElem
        val (golden, total) = goldenCompress(mask, uopIdx = 1)
        pCompress(dut, mask, uopIdx = 1)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
    it("uopIdx=0, mask spanning 2 chunks") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask0 = BigInt("5555", 16)
        val mask1 = BigInt("15", 16) << numElem
        val mask = mask0 | mask1
        val (golden, total) = goldenCompress(mask, uopIdx = 0)
        pCompress(dut, mask, uopIdx = 0)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
    it("uopIdx=2, active in chunk 2") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask = ((BigInt(1) << numElem) - 1) << (2 * numElem)
        val (golden, total) = goldenCompress(mask, uopIdx = 2)
        pCompress(dut, mask, uopIdx = 2)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
    it("uopIdx=3, alternating in chunk 3") {
      simulate(new CompressIndexGen(useLZA = false)(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16) << (3 * numElem)
        val (golden, total) = goldenCompress(mask, uopIdx = 3)
        pCompress(dut, mask, uopIdx = 3)
        dut.clock.step()
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(total.U)
      }
    }
  }
}
