package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.vector.v2.Shuffle.GatherCommon.{FakeRobPtr, GatherParam}

class PermUnitSpec extends AnyFunSpec with ChiselSim with Matchers
    with ParallelTestExecution with GatherSpecBase with GatherSpecUtils {

  val NumPipes = 4

  // -- Helpers -----------------------------------------------------------

  def pDefaults(dut: PermUnit[FakeRobPtr], pipe: Int): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(false)
    p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0)
    val o = p.bits.op
    o.isGather.poke(false); o.isGatherEI16.poke(false)
    o.isSlideUp.poke(false); o.isSlide1Up.poke(false)
    o.isSlideDown.poke(false); o.isSlide1Down.poke(false)
    o.isZip.poke(false); o.isUnzip.poke(false); o.isCompress.poke(false)
    p.bits.uopIdx.poke(0)
    pSew(p.bits.sew, 8); pSew(p.bits.ei, 8); pLmul(p.bits.lmul, 8)
    p.bits.vl.poke(16)
    p.bits.vta.poke(false); p.bits.vma.poke(false)
    p.bits.vs1.poke(0); p.bits.vs2.poke(0); p.bits.vs3.poke(0); p.bits.rs1.poke(0)
    p.bits.mask.poke((BigInt(1) << numElem) - 1); p.bits.zipNum.poke(0)
  }

  def bytesToBigInt(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  def setLmulForBlockSize(dut: PermUnit[FakeRobPtr], pipe: Int, blockSize: Int): Unit = {
    val l = dut.in.pipes(pipe).bits.lmul
    l.mf8.poke(false); l.mf4.poke(false); l.mf2.poke(false)
    l.m1.poke(false); l.m2.poke(false); l.m4.poke(false); l.m8.poke(false)
    blockSize match {
      case 1 => l.m1.poke(true)
      case 2 => l.m2.poke(true)
      case 4 => l.m4.poke(true)
      case 8 => l.m8.poke(true)
    }
  }

  def pGather(dut: PermUnit[FakeRobPtr], pipe: Int,
    sew: Int, ei: Int, vs1: BigInt, vs2: Seq[Int], vs3: Seq[Int],
    uopIdx: Int = 0, blockBase: Int = 0, blockSize: Int = 1, placeIdx: Int = 0
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isGather.poke(true)
    p.bits.placeIdx.poke(placeIdx); p.bits.blockBase.poke(blockBase)
    p.bits.uopIdx.poke(uopIdx)
    pSew(p.bits.sew, sew); pSew(p.bits.ei, ei)
    setLmulForBlockSize(dut, pipe, blockSize)
    p.bits.vs1.poke(vs1)
    p.bits.mask.poke((BigInt(1) << numElem) - 1)
    p.bits.vs2.poke(bytesToBigInt(vs2))
    p.bits.vs3.poke(bytesToBigInt(vs3))
  }

  def pSlide1Up(dut: PermUnit[FakeRobPtr], pipe: Int,
    sew: Int, vl: Int, vs2: Seq[Int], vs3: Seq[Int], rs1: BigInt
  ): Unit = {
    val p = dut.in.pipes(pipe)
    p.valid.poke(true)
    p.bits.op.isSlide1Up.poke(true)
    p.bits.placeIdx.poke(0); p.bits.blockBase.poke(0)
    pSew(p.bits.sew, sew); pSew(p.bits.ei, 8); pLmul(p.bits.lmul, 8)
    p.bits.vl.poke(vl)
    p.bits.rs1.poke(rs1)
    p.bits.vs2.poke(bytesToBigInt(vs2))
    p.bits.vs3.poke(bytesToBigInt(vs3))
  }

  def pPipeOff(dut: PermUnit[FakeRobPtr], pipe: Int): Unit = {
    dut.in.pipes(pipe).valid.poke(false)
  }

  def initDut(dut: PermUnit[FakeRobPtr]): Unit = {
    for (p <- 0 until NumPipes) pDefaults(dut, p)
    dut.in.wbReady.foreach(_.poke(false))
    dut.in.flush.foreach(_.poke(false))
    dut.clock.step()
  }

  def eWb(dut: PermUnit[FakeRobPtr], pipe: Int, dest: BigInt): Unit = {
    dut.out.pipes(pipe).valid.expect(true.B, s"pipe $pipe wb.valid")
    dut.out.pipes(pipe).bits.data.expect(dest)
  }

  // -- Utilities ---------------------------------------------------------

  def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))

  def goldenWb(table: Seq[Int], index: Seq[Int], indexValid: Seq[Boolean],
    fillZeros: Seq[Boolean], fillScala: Seq[Boolean],
    prefill: Seq[Int], tableRange: Seq[Int]
  ): BigInt = {
    index.indices.map { i =>
      if (indexValid(i)) {
        if (tableRange.contains(index(i) / 16)) table(index(i)) else index(i)
      } else if (fillZeros(i)) 0
      else if (fillScala(i)) 0
      else prefill(i)
    }.foldRight(BigInt(0))((b, a) => (a << 8) | BigInt(b))
  }

  def goldenGatherIdx(vs1: BigInt, sew: Int, ei: Int, lmulFrac: Int, uopIdx: Int): Seq[(Int, Boolean, Boolean, Boolean)] = {
    val s = sewb(sew); val nIdx = dlen / ei; val vlmax = lmulFrac * vlen / 64
    val res = Array.fill(numElem)((0, false, false, false))
    for (ie <- 0 until nIdx; j <- 0 until s) {
      val raw = ((vs1 >> (ie * ei)) & ((BigInt(1) << ei) - 1)).toInt
      val pos = ie * s + j
      if (pos < numElem) { val idx = raw * s + j; res(pos) = (idx, idx < vlmax, idx >= vlmax, false) }
    }
    res.toSeq
  }

  // -- Tests --------------------------------------------------------------

  describe("PermUnit") {

    describe("LMUL=1 gather") {
      it("identity") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = (0 until 16).toList
          val vs3 = (0xff to 0xf0 by -1).toList // prefill bytes
          val g = goldenGatherIdx(0, 8, 8, 8, 0)
          val golden = goldenWb(vs2, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), vs3, 0 until 1)
          pGather(dut, 0, 8, 8, 0, vs2, vs3)
          dut.clock.step(); pPipeOff(dut, 0)
          dut.clock.step(); dut.clock.step()
          eWb(dut, 0, golden)
        }
      }
      it("blockBase=4") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = (0 until 16).toList; val vs3 = (0xff to 0xf0 by -1).toList
          val g = goldenGatherIdx(0, 8, 8, 8, 0)
          val golden = goldenWb(vs2, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), vs3, 0 until 1)
          pGather(dut, 0, 8, 8, 0, vs2, vs3, blockBase = 4)
          dut.clock.step(); pPipeOff(dut, 0)
          dut.clock.step(); dut.clock.step()
          eWb(dut, 0, golden)
        }
      }
    }

    describe("LMUL=2 gather") {
      it("two pipes") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2Full = (0 until 32).toList
          val vs3 = (0xff to 0xf0 by -1).toList
          val g = goldenGatherIdx(0, 8, 8, 8, 0)
          val golden = goldenWb(vs2Full, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), vs3, 0 until 2)
          pGather(dut, 0, 8, 8, 0, vs2Full.slice(0, 16), vs3, blockSize = 2, placeIdx = 0)
          pGather(dut, 1, 8, 8, 0, vs2Full.slice(16, 32), vs3, blockSize = 2, placeIdx = 1)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 1)
          for (_ <- 0 until 4) dut.clock.step()
          eWb(dut, 0, golden)
        }
      }
    }

    describe("Slide1Up") {
      it("SEW=64 scalar fill") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = (0 until 16).toList; val vs3 = (0xff to 0xf0 by -1).toList
          val rs1 = BigInt("CAFEBABEDEADBEEF", 16)
          val scalarBytes = (0 until 8).map(b => ((rs1 >> (b * 8)) & 0xff).toInt)
          val expected = foldBytes(scalarBytes ++ vs2.take(8))
          pSlide1Up(dut, 0, 64, 2, vs2, vs3, rs1)
          dut.clock.step(); pPipeOff(dut, 0)
          dut.clock.step(); dut.clock.step()
          eWb(dut, 0, expected)
        }
      }
    }

    describe("Parallel") {
      it("two independent LMUL=1 on different units") {
        simulate(new PermUnit[FakeRobPtr](FakeRobPtr())(GatherParam.default)) { dut =>
          initDut(dut)
          val vs2 = (0 until 16).toList; val vs3 = (0xff to 0xf0 by -1).toList
          val g = goldenGatherIdx(0, 8, 8, 8, 0)
          val golden = goldenWb(vs2, g.map(_._1), g.map(_._2), g.map(_._3), g.map(_._4), vs3, 0 until 1)
          pGather(dut, 0, 8, 8, 0, vs2, vs3, blockBase = 0)
          pGather(dut, 2, 8, 8, 0, vs2, vs3, blockBase = 2)
          dut.clock.step(); pPipeOff(dut, 0); pPipeOff(dut, 2)
          dut.clock.step(); dut.clock.step()
          eWb(dut, 0, golden); eWb(dut, 2, golden)
        }
      }
    }
  }
}
