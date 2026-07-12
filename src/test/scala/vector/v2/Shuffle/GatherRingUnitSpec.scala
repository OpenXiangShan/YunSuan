package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class GatherRingUnitSpec extends AnyFunSpec with ChiselSim with Matchers with ParallelTestExecution {

  val vlen        = 128
  val dlen        = 128
  val maxTableNum = 8

  case class Cycle(pokes: Seq[GatherRingUnit => Unit], expects: Seq[GatherRingUnit => Unit])

  def runCycles(dut: GatherRingUnit, init: GatherRingUnit => Unit, cycles: Cycle*): Unit = {
    init(dut)
    for (c <- cycles) {
      c.pokes.foreach(_(dut))
      dut.clock.step()
      c.expects.foreach(_(dut))
    }
  }

  def initUnit: GatherRingUnit => Unit = {
    dut =>

      dut.in.blockConfig.blockBase.poke(0)
      dut.in.blockConfig.blockSize.poke(8)
      dut.in.willFlow.poke(false)
      dut.in.flowIn.valid.poke(false)
      dut.in.flowIn.idx.poke(0)
      dut.in.flowIn.data.foreach(_.poke(0))
      dut.in.flowIn.tag.isFirst.poke(false)
      dut.in.flowIn.tag.passedEntry.poke(false)
      dut.in.placeTable.valid.poke(false)
      dut.in.placeIndex.valid.poke(false)
      dut.out.writeback.ready.poke(false)
      dut.flush.poke(true)
      dut.clock.step()
      dut.flush.poke(false)
      dut.clock.step()
  }

  def eHasTable(v: Boolean): GatherRingUnit => Unit = _.out.status.hasTable.expect(v)

  def eHasIndex(v: Boolean): GatherRingUnit => Unit = _.out.status.hasIndex.expect(v)

  def eWb(valid: Boolean, dest: BigInt = 0): GatherRingUnit => Unit = {
    dut =>

      dut.out.writeback.valid.expect(valid)
      if (valid) dut.out.writeback.bits.dest.expect(dest)
  }

  def pFlush: GatherRingUnit => Unit = _.flush.poke(true)

  def pDrop: GatherRingUnit => Unit = {
    dut =>

      dut.in.willFlow.poke(true)
      dut.in.flowIn.valid.poke(false)
  }

  def pPlace(
    blkSize: Int,
    tblIdx:  Int,
    tbl:     Seq[Int],
    idx:     Seq[Int],
    iValid:  Seq[Boolean],
    prefill: Seq[Int] = Seq.fill(16)(0),
    fScala:  Seq[Boolean] = Seq.fill(16)(false),
    fZeros:  Seq[Boolean] = Seq.fill(16)(false)
  ): GatherRingUnit => Unit = {
    dut =>

      dut.in.blockConfig.blockSize.poke(blkSize)
      dut.in.placeTable.valid.poke(true)
      dut.in.placeIndex.valid.poke(true)
      dut.in.placeTable.bits.blockBase.poke(0)
      dut.in.placeTable.bits.tableIdx.poke(tblIdx)
      tbl.zipWithIndex.foreach {
        case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
      }
      idx.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.index(i).poke(v)
      }
      iValid.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.indexValid(i).poke(v)
      }
      prefill.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.prefillData(i).poke(v)
      }
      fScala.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.fillScala(i).poke(v)
      }
      fZeros.zipWithIndex.foreach {
        case (v, i) => dut.in.placeIndex.bits.fillZeros(i).poke(v)
      }
  }

  def pNoPlace: GatherRingUnit => Unit = {
    dut =>

      dut.in.placeTable.valid.poke(false)
      dut.in.placeIndex.valid.poke(false)
  }

  def pNoFlow: GatherRingUnit => Unit = _.in.willFlow.poke(false)

  def pFlow: GatherRingUnit => Unit = {
    dut =>

      dut.in.willFlow.poke(true)
      dut.in.flowIn.valid.poke(true)
      dut.in.flowIn.idx.poke(0)
      dut.in.flowIn.data.foreach(_.poke(0))
      dut.in.flowIn.tag.isFirst.poke(false)
      dut.in.flowIn.tag.passedEntry.poke(false)
  }

  def pFlowTbl(tblIdx: Int, tbl: Seq[Int]): GatherRingUnit => Unit = {
    dut =>

      dut.in.willFlow.poke(true)
      dut.in.flowIn.valid.poke(true)
      dut.in.flowIn.idx.poke(tblIdx)
      tbl.zipWithIndex.foreach {
        case (v, i) => dut.in.flowIn.data(i).poke(v)
      }
      dut.in.flowIn.tag.isFirst.poke(false)
      dut.in.flowIn.tag.passedEntry.poke(false)
  }

  def pScala(v: BigInt): GatherRingUnit => Unit = _.in.scalaTable.poke(v)

  private val noOp    = Seq.empty[GatherRingUnit => Unit]
  private val H1      = Seq(eHasTable(true))
  private val H0      = Seq(eHasTable(false))
  private val I1      = Seq(eHasIndex(true))
  private val I0      = Seq(eHasIndex(false))
  private val Wb0     = Seq(eWb(false))

  def Wb(exp: BigInt) = Seq(eWb(true, exp))

  // ═══ 测试 1-4: FSM 基础 ══════════════════════════════
  describe("FSM") {
    it("Place sets hasTable/hasIndex") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val e = Seq.fill(16)(0)
          val f = Seq.fill(16)(false)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pNoFlow), H0 ++ I0),
            Cycle(Seq(pPlace(8, 0, e, e, f)), H1 ++ I1 ++ Wb0)
          )
      }
    }
    it("Flow decrements counter") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val e = Seq.fill(16)(0)
          val f = Seq.fill(16)(false)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, e, e, f)), Wb0),
            Cycle(Seq(pNoPlace, pFlow), Wb(0))
          )
      }
    }
    it("Hold keeps state") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val e = Seq.fill(16)(0)
          val f = Seq.fill(16)(false)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(8, 0, e, e, f)), noOp),
            Cycle(Seq(pNoPlace, pNoFlow), H1 ++ I1)
          )
      }
    }
    it("Flush clears") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val e = Seq.fill(16)(0)
          val f = Seq.fill(16)(false)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(8, 0, e, e, f)), H1),
            Cycle(Seq(pNoPlace, pNoFlow, pFlush), H0 ++ I0)
          )
      }
    }
  }

  // ═══ 测试 5-10: LMUL=1 运算 ═════════════════════════
  describe("LMUL=1") {
    it("identity 16 elem") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val exp = BigInt("1F1E1D1C1B1A19181716151413121110", 16)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, 0 until 16, Seq.fill(16)(true))), Wb(exp))
          )
      }
    }
    it("reverse order gather") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          ) // table: 0x10..0x1F
          val idx = (0 until 16).map(
            i => 15 - i
          ) // index: 15,14,...,0
          val bytes = (0 until 16).map(
            i => 0x10 + (15 - i)
          ) // expected: reversed
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, Seq.fill(16)(true))), Wb(exp))
          )
      }
    }
    it("shuffled gather") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          ) // table: 0x10..0x1F
          val perm = Seq(3, 7, 0, 11, 15, 2, 5, 9, 13, 1, 8, 14, 4, 6, 10, 12) // shuffled order
          val bytes = perm.map(
            i => 0x10 + i
          ) // expected: permuted
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, perm, Seq.fill(16)(true))), Wb(exp))
          )
      }
    }
    it("mixed found/notfound") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0xa0 + i
          )
          val idx = (0 until 16).map(
            i => if (i % 2 == 0) i else 16 + i
          )
          val pf = (0 until 16).map(
            i => if (i % 2 == 0) 0 else 16 + i
          )
          val bytes = (0 until 16).map(
            i => if (i % 2 == 0) 0xa0 + i else 16 + i
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, Seq.fill(16)(true), pf)), Wb(exp))
          )
      }
    }
    it("vslide1up SEW=8") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i == 0) 0 else i - 1
          )
          val iv = (0 until 16).map(_ != 0)
          val fs = (0 until 16).map(_ == 0)
          val bytes = (0 until 16).map(
            i => if (i == 0) 0x5a else 0x10 + (i - 1)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
    it("vslide1down SEW=8") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i == 15) 0 else i + 1
          )
          val iv = (0 until 16).map(_ != 15)
          val fs = (0 until 16).map(_ == 15)
          val bytes = (0 until 16).map(
            i => if (i == 15) 0x5a else 0x10 + (i + 1)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
    it("prefillData for out-of-range indices") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i % 4 < 2) i else 100 + i
          )
          val pf = (0 until 16).map(
            i => if (i % 4 >= 2) 0x7e else 0
          )
          val bytes = (0 until 16).map(
            i => if (i % 4 < 2) 0x10 + i else 0x7e
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, Seq.fill(16)(true), pf)), Wb(exp))
          )
      }
    }
    it("oldTable for inactive elements") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>
          // inactive elements (indexValid=false) get oldTable data (undisturbed/agnostic)
          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val ot = (0 until 16).map(
            i => 0xaa + i
          )
          val iv = (0 until 16).map(_ % 4 < 2) // 0,1,4,5,8,9,12,13 active; rest inactive
          val idx = (0 until 16).map(
            i => if (i % 4 < 2) i else 0
          )
          val bytes = (0 until 16).map(
            i => if (i % 4 < 2) 0x10 + i else 0xaa + i
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          initUnit(dut)
          ot.zipWithIndex.foreach {
            case (v, i) => dut.in.oldTable(i).poke(v)
          }
          dut.in.blockConfig.blockSize.poke(1)
          dut.in.placeTable.valid.poke(true)
          dut.in.placeIndex.valid.poke(true)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
          }
          idx.zipWithIndex.foreach {
            case (v, i) => dut.in.placeIndex.bits.index(i).poke(v)
          }
          iv.zipWithIndex.foreach {
            case (v, i) => dut.in.placeIndex.bits.indexValid(i).poke(v)
          }
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.prefillData(i).poke(0)
          )
          dut.in.placeTable.bits.tableIdx.poke(0)
          dut.in.placeTable.bits.blockBase.poke(0)
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.fillScala(i).poke(false)
          )
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.fillZeros(i).poke(false)
          )
          dut.in.willFlow.poke(false)
          dut.in.flowIn.valid.poke(false)
          dut.clock.step()
          dut.out.writeback.bits.dest.expect(exp)
      }
    }
  }

  // ═══ 测试 11-12: SEW=16/32 fillScala ════════════════
  describe("SEW=16 fillScala") {
    val sb = 2
    it("vslide1up") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i < sb) 0 else i - sb
          )
          val iv = (0 until 16).map(_ >= sb)
          val fs = (0 until 16).map(_ < sb)
          val bytes = (0 until 16).map(
            i => if (i < sb) 0x5a else 0x10 + (i - sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
    it("vslide1down") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i >= 16 - sb) 0 else i + sb
          )
          val iv = (0 until 16).map(_ < 16 - sb)
          val fs = (0 until 16).map(_ >= 16 - sb)
          val bytes = (0 until 16).map(
            i => if (i >= 16 - sb) 0x5a else 0x10 + (i + sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
  }
  describe("SEW=32 fillScala") {
    val sb = 4
    it("vslide1up") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i < sb) 0 else i - sb
          )
          val iv = (0 until 16).map(_ >= sb)
          val fs = (0 until 16).map(_ < sb)
          val bytes = (0 until 16).map(
            i => if (i < sb) 0x5a else 0x10 + (i - sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
    it("vslide1down") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => if (i >= 16 - sb) 0 else i + sb
          )
          val iv = (0 until 16).map(_ < 16 - sb)
          val fs = (0 until 16).map(_ >= 16 - sb)
          val bytes = (0 until 16).map(
            i => if (i >= 16 - sb) 0x5a else 0x10 + (i + sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
  }

  // ═══ 测试 13-14: fillZeros ═══════════════════════════
  describe("fillZeros") {
    it("vslideup offset=3") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val fz = (0 until 16).map(_ < 3)
          val idx = (0 until 16).map(
            i => if (i < 3) 0 else i - 3
          )
          val iv = (0 until 16).map(_ >= 3)
          val bytes = (0 until 16).map(
            i => if (i < 3) 0 else 0x10 + (i - 3)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fZeros = fz)), Wb(exp))
          )
      }
    }
    it("vslidedown offset=2") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val fz = (0 until 16).map(_ >= 14)
          val idx = (0 until 16).map(
            i => if (i >= 14) 0 else i + 2
          )
          val iv = (0 until 16).map(_ < 14)
          val bytes = (0 until 16).map(
            i => if (i >= 14) 0 else 0x10 + (i + 2)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fZeros = fz)), Wb(exp))
          )
      }
    }
    it("fillZeros + fillScala mixed") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val sb = 4
          val fz = (0 until 16).map(_ < sb)
          val fs = (0 until 16).map(
            i => i >= sb && i < sb * 2
          )
          val iv = (0 until 16).map(_ >= sb * 2)
          val idx = (0 until 16).map(
            i => if (i >= sb * 2) i - sb * 2 else 0
          )
          val bytes = (0 until 16).map(
            i => if (i < sb) 0 else if (i < sb * 2) 0x5a else 0x10 + (i - sb * 2)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(
              Seq(pPlace(1, 0, t, idx, iv, fScala = fs, fZeros = fz), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))),
              Wb(exp)
            )
          )
      }
    }
  }

  // ═══ 测试 15-16: Multi-Table ═════════════════════════
  describe("Multi-Table") {
    it("LMUL=2 identity") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s0 = (0 until 16).map(
            i => 0x10 + i
          )
          val s1 = (0 until 16).map(
            i => 0xa0 + i
          )
          val idx = (0 until 16).map(
            i => 15 - i
          ) // reverse: all found in seg 0
          val bytes = (0 until 16).map(
            i => 0x10 + (15 - i)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, s0, idx, Seq.fill(16)(true))), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s1)), Wb(exp))
          )
      }
    }
    it("LMUL=4 identity") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s = (0 until 16).map(
            i => 0x10 + i
          )
          val perm = Seq(3, 7, 0, 11, 15, 2, 5, 9, 13, 1, 8, 14, 4, 6, 10, 12) // shuffled
          val bytes = perm.map(
            i => 0x10 + i
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(4, 0, s, perm, Seq.fill(16)(true))), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(2, s)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(3, s)), Wb(exp))
          )
      }
    }
    it("LMUL=2 all indices in seg1") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s0 = (0 until 16).map(
            i => 0x10 + i
          )
          val s1 = (0 until 16).map(
            i => 0xa0 + i
          )
          val shift = 16
          // ALL indices point to seg1 (high=1), none found in seg0
          val idx = (0 until 16).map(
            i => i + shift
          )
          val pf = (0 until 16).map(
            i => i + shift
          )
          val s1Bytes = s1.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, s0, idx, Seq.fill(16)(true), pf)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s1)), Wb(s1Bytes))
          )
      }
    }
    it("LMUL=2 zip interleave") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s0 = (0 until 16).map(
            i => 0x10 + i
          )
          val s1 = (0 until 16).map(
            i => 0xa0 + i
          )
          // zip: even elements from seg0, odd from seg1
          val idx = (0 until 16).map(
            i => if (i % 2 == 0) i / 2 else 16 + i / 2
          )
          val pf = idx // prefill = index, placeholder for odd elements notfound in seg0
          val zipBytes = (0 until 16).map(
            i => if (i % 2 == 0) s0(i / 2) else s1(i / 2)
          )
          val exp = zipBytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, s0, idx, Seq.fill(16)(true), pf)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s1)), Wb(exp))
          )
      }
    }
    it("LMUL=2 consecutive Places override") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s0 = (0 until 16).map(
            i => 0x10 + i
          )
          val s0b = (0 until 16).map(
            i => 0x30 + i
          )
          val s0bBytes = s0b.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, s0, 0 until 16, Seq.fill(16)(true))), Wb0),
            Cycle(Seq(pPlace(2, 0, s0b, 0 until 16, Seq.fill(16)(true))), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s0b)), Wb(s0bBytes))
          )
      }
    }
    it("LMUL=4 distinct segments") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val s0 = (0 until 16).map(
            i => 0x10 + i
          )
          val s1 = (0 until 16).map(
            i => 0xa0 + i
          )
          val s2 = (0 until 16).map(
            i => 0x20 + i
          )
          val s3 = (0 until 16).map(
            i => 0x30 + i
          )
          // 4 elements per segment: elem 0-3→s0, 4-7→s1, 8-11→s2, 12-15→s3
          val idx = (0 until 16).map(
            i => (i % 4) + (i / 4) * 16
          )
          val bytes = (0 until 16).map(
            i =>
              (i / 4) match {
                case 0 => s0(i % 4)
                case 1 => s1(i % 4)
                case 2 => s2(i % 4)
                case 3 => s3(i % 4)
              }
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(
              Seq(
                pPlace(
                  4,
                  0,
                  s0,
                  idx,
                  Seq.fill(16)(true),
                  prefill = idx
                )
              ),
              Wb0
            ),
            Cycle(Seq(pNoPlace, pFlowTbl(1, s1)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(2, s2)), Wb0),
            Cycle(Seq(pNoPlace, pFlowTbl(3, s3)), Wb(exp))
          )
      }
    }
  }

  // ═══ 测试 17-18: Tag ═════════════════════════════════
  describe("Tags") {
    it("tableOut valid after Place") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => 15 - i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, t, idx, Seq.fill(16)(true))), noOp),
            Cycle(Seq(pNoPlace, pNoFlow), Seq(_.out.tableOut.valid.expect(true)))
          )
      }
    }
    it("passedEntry at entry unit after flow") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          val idx = (0 until 16).map(
            i => 15 - i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, t, idx, Seq.fill(16)(true))), noOp),
            Cycle(Seq(pNoPlace, pFlowTbl(1, t)), Seq(_.out.tableOut.tag.passedEntry.expect(true)))
          )
      }
    }
    it("tag.isFirst set on Place") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(2, 0, t, 0 until 16, Seq.fill(16)(true))), noOp),
            Cycle(Seq(pNoPlace, pNoFlow), Seq(_.out.tableOut.tag.isFirst.expect(false)))
          )
      }
    }
    it("flowIn.tag.passedEntry propagates") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          initUnit(dut)
          val t = (0 until 16).map(
            i => 0x10 + i
          )
          dut.in.blockConfig.blockSize.poke(2)
          dut.in.placeTable.valid.poke(true)
          dut.in.placeIndex.valid.poke(true)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
          }
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.index(i).poke(i)
          )
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.indexValid(i).poke(true)
          )
          dut.in.placeTable.bits.tableIdx.poke(0)
          dut.in.placeTable.bits.blockBase.poke(0)
          dut.in.willFlow.poke(false)
          dut.in.flowIn.valid.poke(false)
          dut.clock.step()
          dut.in.placeTable.valid.poke(false)
          dut.in.placeIndex.valid.poke(false)
          dut.in.willFlow.poke(true)
          dut.in.flowIn.valid.poke(true)
          dut.in.flowIn.idx.poke(1)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.flowIn.data(i).poke(v)
          }
          dut.in.flowIn.tag.isFirst.poke(false)
          dut.in.flowIn.tag.passedEntry.poke(true)
          dut.clock.step()
          dut.out.tableOut.tag.passedEntry.expect(true)
      }
    }
    it("flowIn.tag.isFirst propagates") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          initUnit(dut)
          val t = (0 until 16).map(
            i => 0x10 + i
          )
          dut.in.blockConfig.blockSize.poke(2)
          dut.in.placeTable.valid.poke(true)
          dut.in.placeIndex.valid.poke(true)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
          }
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.index(i).poke(i)
          )
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.indexValid(i).poke(true)
          )
          dut.in.placeTable.bits.tableIdx.poke(0)
          dut.in.placeTable.bits.blockBase.poke(0)
          dut.in.willFlow.poke(false)
          dut.in.flowIn.valid.poke(false)
          dut.clock.step()
          dut.in.placeTable.valid.poke(false)
          dut.in.placeIndex.valid.poke(false)
          dut.in.willFlow.poke(true)
          dut.in.flowIn.valid.poke(true)
          dut.in.flowIn.idx.poke(1)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.flowIn.data(i).poke(v)
          }
          dut.in.flowIn.tag.isFirst.poke(true)
          dut.in.flowIn.tag.passedEntry.poke(false)
          dut.clock.step()
          dut.out.tableOut.tag.isFirst.expect(true)
      }
    }
  }

  // ═══ 测试 19: Reuse ═════════════════════════════════
  describe("Reuse") {
    it("second instruction after flush") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t1 = (0 until 16).map(
            i => 0x10 + i
          )
          val e1 = t1.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          val t2 = (0 until 16).map(
            i => 0xa0 + i
          )
          val e2 = t2.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true))), Wb(e1)),
            Cycle(Seq(pNoPlace, pNoFlow, pFlush), noOp),
            Cycle(Seq(pNoPlace, pNoFlow, _.flush.poke(false)), noOp),
            Cycle(Seq(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true))), Wb(e2))
          )
      }
    }
    it("back-to-back without flush") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t1 = (0 until 16).map(
            i => 0x10 + i
          )
          val e1 = t1.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          val t2 = (0 until 16).map(
            i => 0xa0 + i
          )
          val e2 = t2.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t1, 0 until 16, Seq.fill(16)(true))), Wb(e1)),
            Cycle(Seq(pPlace(1, 0, t2, 0 until 16, Seq.fill(16)(true))), Wb(e2))
          )
      }
    }
  }

  // ═══ SEW=64 ══════════════════════════════════════════
  describe("SEW=64") {
    it("identity 2 lanes") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 8).map(
            i => 0x00 + i
          ) ++ (0 until 8).map(
            i => 0x10 + i
          )
          val exp = BigInt("17161514131211100706050403020100", 16)
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, 0 until 16, Seq.fill(16)(true))), Wb(exp))
          )
      }
    }
    it("vslide1up fillScala 8 bytes") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x00 + i
          )
          val sb = 8
          val idx = (0 until 16).map(
            i => if (i < sb) 0 else i - sb
          )
          val iv = (0 until 16).map(_ >= sb)
          val fs = (0 until 16).map(_ < sb)
          val bytes = (0 until 16).map(
            i => if (i < sb) 0x5a else 0x00 + (i - sb)
          )
          val exp = bytes.foldRight(BigInt(0))(
            (b, a) => (a << 8) | BigInt(b)
          )
          runCycles(
            dut,
            initUnit,
            Cycle(Seq(pPlace(1, 0, t, idx, iv, fScala = fs), pScala(BigInt("5A5A5A5A5A5A5A5A", 16))), Wb(exp))
          )
      }
    }
  }

  // ═══ Table Control ═══════════════════════════════════
  describe("Table Control") {
    it("hasTable dropped after invalid flow") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>

          val t = (0 until 16).map(
            i => 0x10 + i
          )
          runCycles(
            dut,
            initUnit,
            Cycle(
              Seq(pPlace(1, 0, t, 0 until 16, Seq.fill(16)(true))),
              H1 ++ Wb(
                t.foldRight(BigInt(0))(
                  (b, a) => (a << 8) | BigInt(b)
                )
              )
            ),
            Cycle(Seq(pNoPlace, pDrop), H0 ++ I1)
          )
      }
    }
  }

  // ═══ Block Config ════════════════════════════════════
  describe("Block Config") {
    it("non-zero blockBase places correctly") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>
          // unitIdx=0, blockBase=2, blockSize=4 => localPos=6, non-entry, non-ring-end
          val t = (0 until 16).map(
            i => 0x10 + i
          )
          initUnit(dut)
          dut.in.blockConfig.blockBase.poke(2)
          dut.in.blockConfig.blockSize.poke(4)
          dut.in.placeTable.valid.poke(true)
          dut.in.placeIndex.valid.poke(true)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
          }
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.index(i).poke(i)
          )
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.indexValid(i).poke(true)
          )
          dut.in.placeTable.bits.tableIdx.poke(0)
          dut.in.placeTable.bits.blockBase.poke(2)
          dut.clock.step()
          dut.out.status.hasTable.expect(true)
          dut.out.status.hasIndex.expect(true)
          dut.out.writeback.valid.expect(false) // counter=3, not done
      }
    }
    it("non-entry unit does not set passedEntry on flow") {
      simulate(
        new GatherRingUnit(
          vlen,
          dlen,
          maxTableNum,
          xlen = 64,
          unitIdx = 0
        )
      ) {
        dut =>
          // unitIdx=0, blockBase=2 => not the entry => passedEntry NOT set during flow
          val t = (0 until 16).map(
            i => 0x10 + i
          )
          initUnit(dut)
          dut.in.blockConfig.blockBase.poke(2)
          dut.in.blockConfig.blockSize.poke(4)
          dut.in.placeTable.valid.poke(true)
          dut.in.placeIndex.valid.poke(true)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.placeTable.bits.table(i).poke(v)
          }
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.index(i).poke(i)
          )
          (0 until 16).foreach(
            i => dut.in.placeIndex.bits.indexValid(i).poke(true)
          )
          dut.in.placeTable.bits.tableIdx.poke(0)
          dut.in.placeTable.bits.blockBase.poke(2)
          dut.clock.step()
          // Flow through this non-entry unit
          dut.in.placeTable.valid.poke(false)
          dut.in.placeIndex.valid.poke(false)
          dut.in.willFlow.poke(true)
          dut.in.flowIn.valid.poke(true)
          dut.in.flowIn.idx.poke(1)
          t.zipWithIndex.foreach {
            case (v, i) => dut.in.flowIn.data(i).poke(v)
          }
          dut.in.flowIn.tag.isFirst.poke(false)
          dut.in.flowIn.tag.passedEntry.poke(false)
          dut.clock.step()
          dut.out.tableOut.tag.passedEntry.expect(false) // should NOT be set (not entry)
      }
    }
  }
}
