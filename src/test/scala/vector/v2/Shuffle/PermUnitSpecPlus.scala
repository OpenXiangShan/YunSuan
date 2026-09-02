package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.simulator.scalatest.HasCliOptions.CliOption
import org.scalatest.Outcome
import org.scalatest.matchers.must.Matchers._
import yunsuan.util.SpecBase
import yunsuan.vector.v2.Shuffle.GatherCommon.GatherParam

import scala.collection.mutable
import scala.util.Random

object PermUnitSchedule {
  final case class DispatchBatch(uopIdx: Seq[Int]) {
    require(uopIdx.nonEmpty)
    def pipes: Seq[Int] = uopIdx.indices
  }

  final case class DispatchSchedule(permutation: Seq[Int], batches: Seq[DispatchBatch], gaps: Seq[Int]) {
    require(batches.nonEmpty)
    require(gaps.size == batches.size - 1)

    def composition: Seq[Int] = batches.map(_.uopIdx.size)
  }

  def factorial(n: Int): Long = {
    require(n >= 0)
    (2 to n).foldLeft(1L)(_ * _)
  }

  def compositions(total: Int, maxBatchSize: Int): Seq[Seq[Int]] = {
    require(total > 0)
    require(maxBatchSize > 0)

    def recur(remaining: Int): Seq[Seq[Int]] = {
      if (remaining == 0) Seq(Seq.empty)
      else {
        for {
          size <- 1 to math.min(remaining, maxBatchSize)
          suffix <- recur(remaining - size)
        } yield size +: suffix
      }
    }

    recur(total)
  }

  def permutationAt(rank: Int, size: Int): Seq[Int] = {
    require(size > 0)
    require(rank >= 0 && rank < factorial(size), s"invalid permutation rank $rank for size $size")
    var remaining = (0 until size).toBuffer
    var quotient = rank.toLong
    (size to 1 by -1).map { width =>
      val divisor = factorial(width - 1)
      val index = (quotient / divisor).toInt
      quotient %= divisor
      remaining.remove(index)
    }
  }

  def rankOf(permutation: Seq[Int]): Int = {
    require(permutation.sorted == (0 until permutation.size))
    var rank = 0L
    var remaining = (0 until permutation.size).toBuffer
    permutation.zipWithIndex.foreach { case (value, index) =>
      val position = remaining.indexOf(value)
      rank += position.toLong * factorial(permutation.size - index - 1)
      remaining.remove(position)
    }
    rank.toInt
  }

  /**
   * A seed is a stable distributed-task identifier, rather than a PRNG input.
   * This makes every m8 seed map to exactly one distinct permutation.
   */
  def m8RankForSeed(seed: Long): Int = {
    val count = factorial(8)
    require(seed >= 0 && seed < count, s"invalid m8 seed $seed; expected 0..${count - 1}")
    seed.toInt
  }

  def countSchedulesPerPermutation(uopCount: Int, maxBatchSize: Int): Long = {
    schedulesFor(0 until uopCount, maxBatchSize).size.toLong
  }

  def schedulesFor(permutation: Seq[Int], maxBatchSize: Int): Iterator[DispatchSchedule] = {
    require(permutation.sorted == (0 until permutation.size))

    compositions(permutation.size, maxBatchSize).iterator.flatMap { composition =>
      val batches = composition.scanLeft(0)(_ + _).sliding(2).map {
        case Seq(start, end) => DispatchBatch(permutation.slice(start, end))
      }.toSeq
      val dispatchedCounts = composition.drop(1).scanLeft(0)(_ + _).tail

      def expand(remainingDispatchedCounts: Seq[Int], priorGapSum: Int): Iterator[Seq[Int]] = remainingDispatchedCounts match {
        case Seq() => Iterator.single(Seq.empty)
        case dispatched +: tail =>
          val gapUpperBound = dispatched + 1 - priorGapSum
          for {
            gap <- (0 until gapUpperBound).iterator
            suffix <- expand(tail, priorGapSum + gap)
          } yield gap +: suffix
      }

      expand(dispatchedCounts, 0).map(DispatchSchedule(permutation, batches, _))
    }
  }

  def defaultM8Ranks(extraRanks: Int = 128, seed: Long = 0L): Seq[Int] = {
    val count = factorial(8).toInt
    require(extraRanks >= 0 && extraRanks <= count - 2)
    val selected = mutable.Set(0, count - 1)
    val random = new Random(seed)
    while (selected.size < extraRanks + 2) selected += random.nextInt(count)
    selected.toSeq.sorted
  }
}

class PermUnitSpecPlus extends SpecBase with GatherSpecUtils {
  import PermUnitSchedule._

  private val numPipes = GatherParam.default.numPipeIn
  private val numUnits = 8
  private val activeConfigMap = new ThreadLocal[org.scalatest.ConfigMap]
  private var simulatedCycles = 0L

  addOption(CliOption.int("permunit.plus.m8.seed", "run one m8 permutation task, indexed by seed 0..40319"))
  addOption(CliOption.int("permunit.plus.m8.rank", "run one m8 permutation rank 0..40319"))
  addOption(CliOption.simple("permunit.plus.m8.rankRange", "run an inclusive m8 rank range start-end", identity))
  addOption(CliOption.simple("permunit.plus.m8.run", "enable the default m8 rank selection", identity))
  addOption(CliOption.int("permunit.plus.schedule.start", "skip this many schedules for each selected rank"))
  addOption(CliOption.int("permunit.plus.schedule.count", "run at most this many schedules for each selected rank"))

  override def withFixture(test: NoArgTest): Outcome = {
    activeConfigMap.set(test.configMap)
    try super.withFixture(test)
    finally activeConfigMap.remove()
  }

  private def taskOption(name: String): Option[String] =
    Option(activeConfigMap.get()).flatMap(_.get(name)).map(_.toString).orElse(sys.props.get(name))

  private def foldBytes(bytes: Seq[Int]): BigInt =
    bytes.foldRight(BigInt(0))((byte, value) => (value << 8) | BigInt(byte))

  private def table(uopCount: Int): Seq[Int] = Seq.tabulate(uopCount * numElem)(identity)

  private def prefill(uopCount: Int): Seq[Int] = Seq.tabulate(uopCount * numElem)(0xff - _)

  private def pMeta(dut: PermUnit[FakePermMeta], pipe: Int, pdest: Int): Unit = {
    dut.in.pipes(pipe).bits.meta.robIdx.poke(0)
    dut.in.pipes(pipe).bits.meta.pdest.poke(pdest)
  }

  private def pOff(dut: PermUnit[FakePermMeta], pipe: Int): Unit = dut.in.pipes(pipe).valid.poke(false)

  private def clearPipes(dut: PermUnit[FakePermMeta]): Unit =
    for (pipe <- 0 until numPipes) pOff(dut, pipe)

  private def pDefaults(dut: PermUnit[FakePermMeta], pipe: Int): Unit = {
    val input = dut.in.pipes(pipe)
    input.valid.poke(false)
    input.bits.placeIdx.poke(0)
    input.bits.blockBase.poke(0)
    input.bits.uopIdx.poke(0)
    input.bits.op.isGather.poke(false)
    input.bits.op.isGatherEI16.poke(false)
    input.bits.op.isSlideUp.poke(false)
    input.bits.op.isSlide1Up.poke(false)
    input.bits.op.isSlideDown.poke(false)
    input.bits.op.isSlide1Down.poke(false)
    input.bits.op.isZip.poke(false)
    input.bits.op.isUnzip.poke(false)
    input.bits.op.isCompress.poke(false)
    pSew(input.bits.sew, 8)
    pSew(input.bits.ei, 8)
    pLmul(input.bits.lmul, 8)
    input.bits.vl.poke(numElem)
    input.bits.vta.poke(false)
    input.bits.vma.poke(false)
    input.bits.vs1.poke(0)
    input.bits.vs2.poke(0)
    input.bits.vs3.poke(0)
    input.bits.rs1.poke(0)
    input.bits.mask.poke((BigInt(1) << numElem) - 1)
    input.bits.zipNum.poke(0)
    pMeta(dut, pipe, 0)
  }

  private def initDut(dut: PermUnit[FakePermMeta]): Unit = {
    for (pipe <- 0 until numPipes) pDefaults(dut, pipe)
    dut.in.flush.foreach(_.poke(false))
    dut.in.wbReady.foreach(_.poke(true))
    dut.in.wakeAck.foreach(_.poke(false))
    stepClock(dut)
  }

  private def stepClock(dut: PermUnit[FakePermMeta]): Unit = {
    dut.clock.step()
    simulatedCycles += 1
  }

  private def flushDut(dut: PermUnit[FakePermMeta]): Unit = {
    clearPipes(dut)
    dut.in.wakeAck.foreach(_.poke(false))
    dut.in.flush.foreach(_.poke(true))
    stepClock(dut)
    dut.in.flush.foreach(_.poke(false))
    stepClock(dut)
  }

  private def pGather(
    dut: PermUnit[FakePermMeta],
    pipe: Int,
    uopIdx: Int,
    placeIdx: Int,
    uopCount: Int,
    vs1: BigInt,
    fullTable: Seq[Int],
    fullPrefill: Seq[Int]
  ): Unit = {
    val lmulFrac = uopCount * 8
    val input = dut.in.pipes(pipe)
    input.valid.poke(true)
    input.bits.op.isGather.poke(true)
    input.bits.placeIdx.poke(placeIdx)
    input.bits.blockBase.poke(0)
    input.bits.uopIdx.poke(uopIdx)
    pSew(input.bits.sew, 8)
    pSew(input.bits.ei, 8)
    pLmul(input.bits.lmul, lmulFrac)
    input.bits.vl.poke(uopCount * numElem)
    input.bits.vta.poke(false)
    input.bits.vma.poke(false)
    input.bits.vs1.poke(vs1)
    input.bits.vs2.poke(foldBytes(fullTable.slice(uopIdx * numElem, (uopIdx + 1) * numElem)))
    input.bits.vs3.poke(foldBytes(fullPrefill.slice(uopIdx * numElem, (uopIdx + 1) * numElem)))
    input.bits.mask.poke((BigInt(1) << numElem) - 1)
    pMeta(dut, pipe, uopIdx)
  }

  private def runSchedule(dut: PermUnit[FakePermMeta], schedule: DispatchSchedule): Unit = {
    val uopCount = schedule.permutation.size
    val fullTable = table(uopCount)
    val fullPrefill = prefill(uopCount)
    val indexByUop = (0 until uopCount).map { uop =>
      uop -> foldBytes((0 until numElem).map(byte => uop * numElem + numElem - 1 - byte))
    }.toMap
    val expected = (0 until uopCount).map { uop =>
      uop -> foldBytes((0 until numElem).map(byte => uop * numElem + numElem - 1 - byte))
    }.toMap
    val seenWb = mutable.Map.empty[Int, BigInt]
    val seenWake = mutable.Set.empty[Int]
    val wbTicks = mutable.Map.empty[Int, Int]
    val wakeTicks = mutable.Map.empty[Int, Int]
    val details = s"perm=${schedule.permutation.mkString("[", ",", "]")} composition=${schedule.composition.mkString("[", ",", "]")} gaps=${schedule.gaps.mkString("[", ",", "]")}"
    var sawActive = false
    var ticksSinceLastDispatch: Option[Int] = None

    print(s"testing $details ... ")

    def observe(): Unit = {
      for (pipe <- 0 until numPipes) {
        dut.in.wakeAck(pipe).poke(false)
        if (dut.out.pipes(pipe).valid.peekBoolean()) {
          val pdest = dut.out.pipes(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
          val data = dut.out.pipes(pipe).bits.data.peekValue().asBigInt
          expected.getOrElse(pdest, fail(s"unknown writeback pdest=$pdest $details"))
          data mustBe expected(pdest)
          seenWb.getOrElseUpdate(pdest, data)
          ticksSinceLastDispatch.foreach(tick => wbTicks.getOrElseUpdate(pdest, tick))
        }
        if (dut.out.wake(pipe).valid.peekBoolean()) {
          val pdest = dut.out.wake(pipe).bits.meta.pdest.peekValue().asBigInt.toInt
          if (!expected.contains(pdest)) fail(s"unknown wake pdest=$pdest $details")
          seenWake += pdest
          ticksSinceLastDispatch.foreach(tick => wakeTicks.getOrElseUpdate(pdest, tick))
          dut.in.wakeAck(pipe).poke(true)
        }
      }
      if (!sawActive && (0 until numUnits).exists(unit => dut.out.unitStatus(unit).hasIndex.peekBoolean())) sawActive = true
    }

    def step(): Unit = {
      observe()
      stepClock(dut)
      dut.in.wakeAck.foreach(_.poke(false))
    }

    schedule.batches.zipWithIndex.foreach { case (batch, batchIndex) =>
      batch.uopIdx.zipWithIndex.foreach { case (uopIdx, placeIdx) =>
        pGather(dut, placeIdx, uopIdx, placeIdx, uopCount, indexByUop(uopIdx), fullTable, fullPrefill)
      }
      step()
      if (batchIndex == schedule.batches.size - 1) ticksSinceLastDispatch = Some(0)
      clearPipes(dut)
      if (batchIndex < schedule.gaps.size) {
        for (_ <- 0 until schedule.gaps(batchIndex)) step()
      }
    }

    // Bound the remaining dispatch pressure at each batch, treating the final
    // undefined gap as zero, then allow an LMUL-sized ring traversal and one
    // writeback state-clear cycle.
    var maxDrainTicks = 0

    for(i <- 0 until schedule.gaps.size) {
      maxDrainTicks += schedule.composition(i) - 1 - schedule.gaps(i)
      maxDrainTicks = math.max(maxDrainTicks, 0)
    }
    maxDrainTicks += uopCount + schedule.composition.last

    var drained = false
    var postDispatchTick = 0
    while (postDispatchTick <= maxDrainTicks && !drained) {
      ticksSinceLastDispatch = Some(postDispatchTick)
      observe()
      val allStateClear = seenWb.size == uopCount && seenWake.size == uopCount &&
        (0 until numUnits).forall { unit =>
          !dut.out.unitStatus(unit).hasTable.peekBoolean() &&
            !dut.out.unitStatus(unit).hasIndexNext.peekBoolean()
        }
      drained = sawActive && seenWb.size == uopCount && seenWake.size == uopCount && allStateClear
      if (!drained && postDispatchTick < maxDrainTicks) {
        stepClock(dut)
        dut.in.wakeAck.foreach(_.poke(false))
        postDispatchTick += 1
      } else if (!drained) {
        postDispatchTick += 1
      }
    }

    withClue(s"schedule did not drain within $maxDrainTicks ticks after the last uop dispatch: $details; wb=${seenWb.keySet}; wbTicks=${wbTicks.toSeq.sortBy(_._1)}; wake=${seenWake.toSeq.sorted}; wakeTicks=${wakeTicks.toSeq.sortBy(_._1)}") {
      sawActive mustBe true
      seenWb.keySet mustBe expected.keySet
      seenWake mustBe expected.keySet
      drained mustBe true
    }
    flushDut(dut)
    println("done")
  }

  private def runRanks(dut: PermUnit[FakePermMeta], uopCount: Int, ranks: Seq[Int]): Unit = {
    val scheduleStart = taskOption("permunit.plus.schedule.start").map(_.toInt).getOrElse(0)
    val scheduleCount = taskOption("permunit.plus.schedule.count").map(_.toInt).getOrElse(Int.MaxValue)
    require(scheduleStart >= 0, "permunit.plus.schedule.start must be non-negative")
    require(scheduleCount >= 0, "permunit.plus.schedule.count must be non-negative")
    ranks.foreach { rank =>
      val permutation = permutationAt(rank, uopCount)
      schedulesFor(permutation, numPipes)
        .drop(scheduleStart)
        .take(scheduleCount)
        .foreach(schedule => runSchedule(dut, schedule))
    }
      println(s"simulated cycles=$simulatedCycles")
  }

  describe("vrgather.vv scheduling") {
    describe("generator") {
      it("counts compositions and schedule variants") {
        compositions(2, numPipes).size mustBe 2
        compositions(4, numPipes).size mustBe 8
        compositions(8, numPipes).size mustBe 108
        countSchedulesPerPermutation(2, numPipes) mustBe 3L
        countSchedulesPerPermutation(4, numPipes) mustBe 45L
        countSchedulesPerPermutation(8, numPipes) mustBe 20377L
        factorial(2) * countSchedulesPerPermutation(2, numPipes) mustBe 6L
        factorial(4) * countSchedulesPerPermutation(4, numPipes) mustBe 1080L
        factorial(8) * countSchedulesPerPermutation(8, numPipes) mustBe 821600640L
      }

      it("round trips permutation ranks") {
        for (size <- Seq(2, 4); rank <- 0 until factorial(size).toInt) {
          rankOf(permutationAt(rank, size)) mustBe rank
        }
        Seq(0, 1, 20160, 40318, 40319).foreach { rank =>
          rankOf(permutationAt(rank, 8)) mustBe rank
        }
      }

      it("maps every m8 task seed to one distinct permutation rank") {
        m8RankForSeed(0L) mustBe 0
        m8RankForSeed(20160L) mustBe 20160
        m8RankForSeed(40319L) mustBe 40319
      }

      it("selects deterministic m8 ranks") {
        val ranks = defaultM8Ranks()
        ranks.size mustBe 130
        ranks.distinct.size mustBe 130
        ranks.head mustBe 0
        ranks.last mustBe 40319
      }
    }

    describe("LMUL=2") {
      it("all permutations, batches, and gaps") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          runRanks(dut, 2, 0 until factorial(2).toInt)
        }
      }
    }

    describe("LMUL=4") {
      it("all permutations, batches, and gaps") {
        simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
          initDut(dut)
          runRanks(dut, 4, 0 until factorial(4).toInt)
        }
      }
    }

    describe("LMUL=8") {
      it("seed-selected permutation") {
        taskOption("permunit.plus.m8.seed") match {
          case Some(value) =>
            val rank = try m8RankForSeed(value.toLong)
            catch {
              case error: NumberFormatException => fail(s"invalid permunit.plus.m8.seed=$value; expected 0..40319", error)
              case error: IllegalArgumentException => fail(error.getMessage, error)
            }
            simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
              initDut(dut)
              runRanks(dut, 8, Seq(rank))
            }
          case None =>
            m8RankForSeed(0L) mustBe 0
        }
      }

      it("configured rank selection") {
        val requested = taskOption("permunit.plus.m8.rank").map(_.toInt)
        val requestedRange = taskOption("permunit.plus.m8.rankRange").map { value =>
          value.split("-", -1).toSeq match {
            case Seq(start, end) => start.toInt to end.toInt
            case _ => fail(s"invalid permunit.plus.m8.rankRange=$value; expected start-end")
          }
        }
        val enabled = taskOption("permunit.plus.m8.run").contains("true")
        val ranks = requested.toSeq ++ requestedRange.toSeq.flatten
        val selected = if (ranks.nonEmpty) ranks.distinct.sorted else defaultM8Ranks()
        all(selected.map(rank => rank >= 0 && rank < factorial(8))) mustBe true
        if (enabled || ranks.nonEmpty) {
          simulate(new PermUnit[FakePermMeta](FakePermMeta())(GatherParam.default)) { dut =>
            initDut(dut)
            runRanks(dut, 8, selected)
          }
        } else {
          selected.size mustBe 130
        }
      }
    }
  }
}
