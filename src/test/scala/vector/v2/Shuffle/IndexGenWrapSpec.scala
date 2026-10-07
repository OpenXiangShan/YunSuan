package yunsuan.vector.v2.Shuffle

import chisel3._
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

class IndexGenWrapSpec extends SpecBase with GatherSpecUtils {
  private def defaults(dut: IndexGen): Unit = {
    dut.in.valid.poke(false)
    dut.flush.poke(false)
    val op = dut.in.bits.op
    op.isGather.poke(false)
    op.isGatherEI16.poke(false)
    op.isGatherEI16Rvs3.poke(false)
    op.isSlideUp.poke(false)
    op.isSlide1Up.poke(false)
    op.isSlideDown.poke(false)
    op.isSlide1Down.poke(false)
    op.isZip.poke(false)
    op.isUnzip.poke(false)
    op.isCompress.poke(false)
    dut.in.bits.uopIdx.poke(0)
    pSew(dut.in.bits.sew, 8)
    pSew(dut.in.bits.ei, 16)
    pLmul(dut.in.bits.lmul, 0)
    dut.in.bits.vl.poke(0)
    dut.in.bits.vta.poke(false)
    dut.in.bits.vma.poke(false)
    dut.in.bits.vs1.poke(0)
    dut.in.bits.vs2.poke(0)
    dut.in.bits.vs3.poke(0)
    dut.in.bits.rs1.poke(0)
    dut.in.bits.mask.poke((BigInt(1) << dut.in.bits.mask.getWidth) - 1)
    dut.in.bits.zipNum.poke(0)
  }

  describe("IndexGen EI16 uop wrapping") {
    it("keeps reset and idle invalid with all-zero LMUL, then recovers in one cycle") {
      simulate(new IndexGen()(GatherParam.default)) { dut =>
        defaults(dut)
        dut.reset.poke(true)
        dut.clock.step(2)
        dut.out.valid.expect(false)
        dut.reset.poke(false)
        dut.clock.step(3)
        dut.out.valid.expect(false)

        pLmul(dut.in.bits.lmul, 16)
        dut.in.bits.vl.poke(16)
        dut.in.bits.uopIdx.poke(2)
        dut.in.bits.op.isGatherEI16Rvs3.poke(true)
        dut.in.valid.poke(true)
        dut.out.valid.expect(false)
        dut.clock.step()
        dut.out.valid.expect(true)
        dut.out.isRvs3.expect(true)
        for (i <- 0 until numElem) {
          dut.out.elemFlags(i).expect(ElemFlags.ZERO)
          dut.out.elemData(i).expect(0)
        }
        defaults(dut)
        dut.clock.step(3)
        dut.out.valid.expect(false)
      }
    }

    it("preserves remainder-based tail boundaries and one-cycle timing in the configured 128-bit IndexGen") {
      val dataLength = 128
      val param = GatherParam.default
      simulate(new IndexGen()(param)) { dut =>
        defaults(dut)
        val bytesPerUop = dataLength / 8
        val numUops = param.vlen / dataLength * 8
        val oldBytes = (0 until bytesPerUop).map(i => 0x40 + i)
        val oldVd = oldBytes.zipWithIndex.foldLeft(BigInt(0)) {
          case (acc, (byte, i)) => acc | (BigInt(byte) << (8 * i))
        }
        for {
          lmulFrac <- Seq(1, 2, 4, 8, 16, 32, 64)
          sew <- Seq(8, 16, 32, 64)
          vlmax = lmulFrac * param.vlen / (8 * sew)
          if vlmax > 0
          rvs3 <- Seq(false, true)
          uopIdx <- 0 until numUops
          vl <- Seq(0, 1, dataLength / sew - 1, dataLength / sew, vlmax).distinct.filter(v => v >= 0 && v <= vlmax)
          tailAgnostic <- Seq(false, true)
        } {
          val blockSize = (lmulFrac / 8).max(1)
          // The golden deliberately retains mathematical remainder, not the RTL mask.
          val effectiveIdx = if (sew == 8) uopIdx % blockSize else uopIdx
          val e8vl = vl * (sew / 8)
          pLmul(dut.in.bits.lmul, lmulFrac)
          pSew(dut.in.bits.sew, sew)
          dut.in.bits.op.isGatherEI16.poke(!rvs3)
          dut.in.bits.op.isGatherEI16Rvs3.poke(rvs3)
          dut.in.bits.uopIdx.poke(uopIdx)
          dut.in.bits.vl.poke(vl)
          dut.in.bits.vta.poke(tailAgnostic)
          dut.in.bits.vs3.poke(if (rvs3) oldVd else BigInt(0))
          dut.in.valid.poke(true)
          dut.clock.step()
          dut.out.valid.expect(true)
          dut.out.isRvs3.expect(rvs3)
          dut.out.waitRvs3.expect(!rvs3 && sew == 8)
          for (i <- 0 until bytesPerUop) {
            val tail = effectiveIdx * bytesPerUop + i >= e8vl
            val flags = if (tail) ElemFlags.PREFILL else if (rvs3) ElemFlags.ZERO else ElemFlags.LOOKUP
            val data = if (tail) {
              if (!rvs3 && sew == 8) 0
              else if (tailAgnostic) 0xff
              else if (rvs3) oldBytes(i)
              else 0
            } else if (rvs3) 0 else i % (sew / 8)
            val context = s"lmul=$lmulFrac sew=$sew rvs3=$rvs3 uop=$uopIdx vl=$vl ta=$tailAgnostic byte=$i"
            dut.out.elemFlags(i).expect(flags, context)
            dut.out.elemData(i).expect(data.U, context)
          }
        }
        dut.flush.poke(true)
        dut.out.valid.expect(false)
        dut.in.valid.poke(false)
        dut.clock.step()
        dut.flush.poke(false)
        dut.out.valid.expect(false)
      }
    }

    it("does not wrap ordinary SEW=8 gather uops") {
      simulate(new IndexGen()(GatherParam.default)) { dut =>
        defaults(dut)
        pSew(dut.in.bits.ei, 8)
        dut.in.bits.op.isGather.poke(true)
        dut.in.valid.poke(true)
        val oldVd = BigInt("AABBCCDDEEFF00112233445566778899", 16)
        dut.in.bits.vs3.poke(oldVd)
        for {
          lmulFrac <- Seq(1, 2, 4, 8, 16, 32, 64)
          uopIdx <- 0 until 8
          vl <- Seq(0, 1, lmulFrac * 2).distinct
          tailAgnostic <- Seq(false, true)
        } {
          pLmul(dut.in.bits.lmul, lmulFrac)
          dut.in.bits.uopIdx.poke(uopIdx)
          dut.in.bits.vl.poke(vl)
          dut.in.bits.vta.poke(tailAgnostic)
          dut.clock.step()
          dut.out.valid.expect(true)
          dut.out.waitRvs3.expect(false)
          dut.out.isRvs3.expect(false)
          for (i <- 0 until numElem) {
            val tail = uopIdx * numElem + i >= vl
            val data = if (!tail) BigInt(0)
              else if (tailAgnostic) BigInt(0xff)
              else (oldVd >> (8 * i)) & 0xff
            val context = s"ordinary gather lmul=$lmulFrac uop=$uopIdx vl=$vl byte=$i"
            dut.out.elemFlags(i).expect(if (tail) ElemFlags.PREFILL else ElemFlags.LOOKUP, context)
            dut.out.elemData(i).expect(data.U, context)
          }
        }
      }
    }
  }
}
