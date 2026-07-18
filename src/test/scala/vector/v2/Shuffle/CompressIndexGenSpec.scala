package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class CompressIndexGenSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val vlen = 128
  val dlen = 128
  val numElem = dlen / 8
  val splittedParts = 8 * vlen / dlen

  // Golden model: compress index
  // For each input byte position, if mask is set, the byte moves to position popcount(mask[0..pos])-1
  // indexValid(pos) = mask[pos]
  // index(pos) = {popcount(mask[0..pos])-1, pos}
  def goldenCompress(mask: BigInt, uopIdx: Int): (Seq[(Int, Boolean)], Int) = {
    val base = uopIdx * numElem
    var count = 0
    val results = (0 until numElem).map { i =>
      val globalPos = base + i
      val bit = (mask >> globalPos) & 1
      if (bit == 1) {
        count += 1
        (globalPos, true)  // (index, indexValid) — index is the original position
      } else {
        (0, false)
      }
    }
    (results, count)
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

  def pOff(dut: CompressIndexGen): Unit = {
    dut.in.valid.poke(false)
  }

  describe("CompressIndexGen") {
    it("all bytes active") {
      simulate(new CompressIndexGen()(GatherParam.default)) { dut =>
        val mask = (BigInt(1) << numElem) - 1  // 0xFFFF
        pCompress(dut, mask, uopIdx = 0)
        dut.clock.step()  // 1-cycle pipeline: in registered -> out
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          dut.out.index.indexValid(i).expect(true.B)
          dut.out.index.index(i).expect(i.U)  // identity mapping when all active
        }
        dut.out.count1s.expect(numElem.U)
      }
    }
    it("alternating mask") {
      simulate(new CompressIndexGen()(GatherParam.default)) { dut =>
        val mask = BigInt("5555", 16)  // 0b0101010101010101
        val (golden, count) = goldenCompress(mask, uopIdx = 0)
        pCompress(dut, mask, uopIdx = 0)
        dut.clock.step()  // 1-cycle pipeline
        dut.out.valid.expect(true.B)
        for (i <- 0 until numElem) {
          val (idx, valid) = golden(i)
          dut.out.index.indexValid(i).expect(valid.B)
          if (valid) dut.out.index.index(i).expect(idx.U)
        }
        dut.out.count1s.expect(count.U)
      }
    }
    it("no bytes active") {
      simulate(new CompressIndexGen()(GatherParam.default)) { dut =>
        pCompress(dut, BigInt(0), uopIdx = 0)
        dut.clock.step()  // 1-cycle pipeline
        dut.out.valid.expect(true.B)  // valid reg stays set for at least 1 cycle
      }
    }
  }
}
