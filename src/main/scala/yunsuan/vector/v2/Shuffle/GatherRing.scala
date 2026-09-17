package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherRingUnit.{PlaceIndexRequest, PlaceTableRequest, Status, Wake}

class GatherRing[T <: Data](
  metaGen: => T
)(
  implicit val param: GatherParam
) extends GatherModule {
  override def desiredName: String = super.desiredName + s"_vlen${VLEN}b_dlen${DLEN}b"

  val in = IO(Input(new GatherRing.In(metaGen)))
  val out = IO(Output(new GatherRing.Out(metaGen)))

  /*********************** Module Definations *********************************/

  private val units = Seq.tabulate(NumUnits) {
    i => Module(new GatherRingUnit(unitIdx = i, metaGen)(param)).suggestName(s"ringUnit_$i")
  }

  private val controller = Module(new GatherRingController)

  /************************* Reg Definations **********************************/

  val pipeBlockSize = Reg(Vec(NumPipeIn, BlockSize()))
  val pipeTablePos = Reg(Vec(NumPipeIn, UnitIdx()))
  val pipeIndexPos = Reg(Vec(NumPipeIn, UnitIdx()))

  /******************** Connections of controller *****************************/

  val s1HasTable = Mux1H(in.s1pipes.map(_.valid), pipeTablePos.map(pos => UIntToOH(pos, NumUnits))).ensuring(_.getWidth == NumUnits)
  val s1HasIndex = Mux1H(in.s1pipes.map(_.valid), pipeIndexPos.map(pos => UIntToOH(pos, NumUnits))).ensuring(_.getWidth == NumUnits)

  private val s1PlacesGather = VecInit.tabulate(NumUnits) { u =>
    in.s1pipes.zip(pipeIndexPos).map { case (s1, pos) =>
      s1.valid && pos === u.U && !s1.bits.isRvs3
    }.foldLeft(false.B)(_ || _)
  }
  private val s1PlacesRvs3 = VecInit.tabulate(NumUnits) { u =>
    in.s1pipes.zip(pipeIndexPos).map { case (s1, pos) =>
      s1.valid && pos === u.U && s1.bits.isRvs3
    }.foldLeft(false.B)(_ || _)
  }
  private val ei16PairComplete = VecInit.tabulate(NumUnits) { u =>
    (units(u).out.status.gatherPlaced || s1PlacesGather(u)) &&
    (units(u).out.status.rvs3Placed || s1PlacesRvs3(u))
  }
  private val s1IsEi16Pair = VecInit(in.s1pipes.map(s1 => s1.bits.isRvs3 || s1.bits.waitRvs3))

  controller.in.s0pipes  := in.s0pipes
  controller.in.s1pipes.zipWithIndex.foreach { case (s1, i) =>
    // An rvs3 (SEW=8 ei16) places no table (and its index is routed
    // deterministically by uopIdx), so it must not contribute to the controller's
    // s1 occupancy: otherwise the (stale) pipeTablePos would claim a phantom
    // table slot and corrupt the block's table occupancy (s1TableValid / entryTable).
    s1.valid := in.s1pipes(i).valid && !in.s1pipes(i).bits.isRvs3
    s1.bits.indexValid := in.s1pipes(i).valid &&
      (!s1IsEi16Pair(i) || ei16PairComplete(pipeIndexPos(i)))
    s1.bits.tablePos := pipeTablePos(i)
    s1.bits.indexPos := pipeIndexPos(i)
    s1.bits.blockSize := pipeBlockSize(i)
  }
  controller.in.hasTable := units.map(_.out.status.hasTable)
  controller.in.hasIndex := units.map(_.out.status.hasIndex)
  controller.in.hasIndexNext := units.map(_.out.status.hasIndexNext)
  controller.in.blockSize := units.map(_.out.status.blockSize)
  controller.in.counter := units.map(_.out.status.counter)
  controller.in.wbValid := Cat(units.map(_.out.wb.valid).reverse)
  controller.in.wakeValid := Cat(units.map(_.out.wake.valid).reverse)

  // Flow loopback connections (ring topology)
  private val ring2Units = units.grouped(2).toSeq
  private val ring4Units = units.grouped(4).toSeq
  private val ring8Units = units.grouped(8).toSeq
  private val ring2TailUnits = ring2Units.map(_.last)
  private val ring4TailUnits = ring4Units.map(_.last)
  private val ring8TailUnits = ring8Units.map(_.last)

  for (i <- 0 until NumUnits) {

    val prevFlowSource: Option[ValidIO[GatherRingUnit.RingFlow]] = Option.when(i > 0) {
      val prev = units(i - 1)
      // A tail table returns through its size-specific loopback below. Letting the same tail also
      // use the physical next-unit connection crosses into the neighboring block; if that block is
      // flowing too, Mux1H sees both sources and ORs their payloads. This occurred with adjacent
      // seg2 blocks when unit 3 forwarded into unit 4 while unit 5 looped back into unit 4.
      val crossesBlockBoundary =
        (prev.out.status.blockSize & 1.U).orR ||
          ((prev.out.status.blockSize & 2.U).orR && (i % 2 == 0).B) ||
          ((prev.out.status.blockSize & 4.U).orR && (i % 4 == 0).B) ||
          ((prev.out.status.blockSize & 8.U).orR && (i % 8 == 0).B)
      makeValidIO(prev.out.flow.valid && !crossesBlockBoundary, prev.out.flow.bits)
    }

    // A table returning to the block's entry unit via a ring loopback has already
    // passed the entry, so tag the loopback flow with passedEntry=1. The tail
    // unit only drives the loopback while its own passedEntry is still 0
    // (out.flow.valid = hasTable && !isLastUnit once passed), so the looped
    // table's passedEntry would otherwise always read 0 and the entry unit's
    // counter (gated on in.flow.passedEntry) would never decrement (M13/uop0).
    def looped(tail: GatherRingUnit[T]): GatherRingUnit.RingFlow = {
      val bits = Wire(chiselTypeOf(tail.out.flow.bits))
      bits := tail.out.flow.bits
      bits.passedEntry := true.B
      bits
    }
    val ring2FlowSource = Option.when(i % 2 == 0)(makeValidIO(
      ring2TailUnits(i / 2).out.flow.valid && (units(i).out.status.blockSize & 2.U).orR,
      looped(ring2TailUnits(i / 2)))
    )
    val ring4FlowSource = Option.when(i % 4 == 0)(makeValidIO(
      ring4TailUnits(i / 4).out.flow.valid && (units(i).out.status.blockSize & 4.U).orR,
      looped(ring4TailUnits(i / 4)))
    )
    val ring8FlowSource = Option.when(i % 8 == 0)(makeValidIO(
      ring8TailUnits(i / 8).out.flow.valid && (units(i).out.status.blockSize & 8.U).orR,
      looped(ring8TailUnits(i / 8)))
    )

    val flowSources: Seq[ValidIO[GatherRingUnit.RingFlow]] = Seq(
      prevFlowSource,
      ring2FlowSource,
      ring4FlowSource,
      ring8FlowSource,
    ).flatten

    units(i).in.flow := Mux1HValidIO(flowSources)
    when (controller.out.willFlow(i)) {
      assert(PopCount(flowSources.map(_.valid)) <= 1.U,
        s"multiple ring flow sources target GatherRing unit $i")
    }
  }

  for (i <- 0 until NumUnits) {
    units(i).in.willFlow := controller.out.willFlow(i)
    units(i).in.clearTable := controller.out.clearTable(i)
  }

  private val validMatrixPipeUnitTable = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))
  private val validMatrixPipeUnitIndex = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))

  for (i <- 0 until NumPipeIn) {
    // Table is placed only by the gather uop: the paired rvs3 (isRvs3=1) carries
    // old-data, no table, and must not re-place/overwrite the gather's table
    // (same-cycle pair would otherwise create a second table writer to the unit).
    validMatrixPipeUnitTable(i)(pipeTablePos(i)) := in.s1pipes(i).valid && !in.s1pipes(i).bits.isRvs3
    validMatrixPipeUnitIndex(i)(pipeIndexPos(i)) := in.s1pipes(i).valid
  }

  for (u <- 0 until NumUnits) {
    assert(PopCount(validMatrixPipeUnitTable.map(_(u))) <= 1.U,
      s"multiple table writers target GatherRing unit $u")
    // 2 index writers to the same unit are only legal for SEW=8 vrgatherei16:
    // a gather (waitRvs3=1) and its paired rvs3 (isRvs3=1) landing on the same unit.
    val uIdxWriters = validMatrixPipeUnitIndex.map(_(u))
    val uEi16Pair =
      PopCount(uIdxWriters.zip(in.s1pipes.map(_.bits.isRvs3)).map { case (v, r) => v && r }) === 1.U &&
      PopCount(uIdxWriters.zip(in.s1pipes.map(_.bits.waitRvs3)).map { case (v, r) => v && r }) === 1.U
    assert(PopCount(uIdxWriters) <= 1.U || (PopCount(uIdxWriters) === 2.U && uEi16Pair),
      s"more than 1 index writer to GatherRing unit $u is only legal for SEW=8 ei16 gather+rvs3")

    units(u).in.placeTable.valid := Cat(validMatrixPipeUnitTable.map(_(u))).orR
    units(u).in.placeTable.bits  := Mux1H(
      validMatrixPipeUnitTable.map(_(u)),
      in.s1pipes.map(s1 => PlaceTableRequest.fromPipeInS1(s1.bits, u))
    )

    // Same-unit same-cycle index writers (SEW=8 ei16 gather + its rvs3) are merged
    // per-field inline: identical fields (blockBase/blockSize) and complementary
    // flags/data/meta (gather PREFILL-0 & meta-0 | rvs3 old-data & meta) merge by
    // OR; waitRvs3 is ANDed so the rvs3 writer (waitRvs3=0) clears the wait.
    {
      val idxWriters = validMatrixPipeUnitIndex.map(_(u))
      val uIdx = Wire(new PlaceIndexRequest(chiselTypeOf(units(u).in.placeIndex.bits.meta)))
      uIdx.blockBase := (0 until NumPipeIn).map(p => Mux(idxWriters(p), in.s1pipes(p).bits.blockBase, 0.U)).reduce(_ | _)
      uIdx.blockSize := (0 until NumPipeIn).map(p => Mux(idxWriters(p), in.s1pipes(p).bits.blockSize, 0.U)).reduce(_ | _)
      // isRvs3: true only when EVERY writer is an rvs3 (a lone cross-cycle rvs3
      // continuing its gather). A merged gather+rvs3 pair is a complete placement
      // (gather's isRvs3=0 clears the AND), and a lone gather is not an rvs3.
      uIdx.isRvs3    := (0 until NumPipeIn).map(p =>
        Mux(idxWriters(p), in.s1pipes(p).bits.isRvs3, true.B)).reduce(_ && _)
      uIdx.placesGather := (0 until NumPipeIn).map(p =>
        idxWriters(p) && !in.s1pipes(p).bits.isRvs3).reduce(_ || _)
      uIdx.placesRvs3 := (0 until NumPipeIn).map(p =>
        idxWriters(p) && in.s1pipes(p).bits.isRvs3).reduce(_ || _)
      // Wait for a paired rvs3 only if every writer is a waitRvs3 gather: the
      // rvs3 writer (waitRvs3=0) clears the wait via AND.
      uIdx.waitRvs3  := (0 until NumPipeIn).map(p => Mux(idxWriters(p), in.s1pipes(p).bits.waitRvs3, true.B)).reduce(_ && _)
      uIdx.meta      := (0 until NumPipeIn).map(p =>
        Mux(idxWriters(p), in.s1pipes(p).bits.meta.asUInt, 0.U)).reduce(_ | _).asTypeOf(chiselTypeOf(uIdx.meta))
      for (i <- 0 until NumElem) {
        uIdx.elemFlags(i) := (0 until NumPipeIn).map(p =>
          Mux(idxWriters(p), in.s1pipes(p).bits.elemFlags(i), 0.U(2.W))).reduce(_ | _)
        uIdx.elemData(i) := (0 until NumPipeIn).map(p =>
          Mux(idxWriters(p), in.s1pipes(p).bits.elemData(i), 0.U(8.W))).reduce(_ | _)
      }
      units(u).in.placeIndex.valid := Cat(validMatrixPipeUnitIndex.map(_(u))).orR
      units(u).in.placeIndex.bits  := uIdx
    }
  }

  // out.metas: collect meta from each unit's wb
  for (i <- 0 until NumUnits) {
    out.metas(i) := units(i).out.wb.bits.meta
  }

  for (i <- 0 until NumUnits) {
    units(i).in.flush := in.flush(i)
  }

  // unitStatus output
  for (i <- 0 until NumUnits) {
    out.unitStatus(i) := units(i).out.status
  }
  // debug (indexOrRes / indexDone) threaded from units
  out.debug.foreach { dbg =>
    dbg.zipWithIndex.foreach { case (d, i) => d := units(i).out.debug.get }
  }

  /******************** Connections of controller ****************************/

  // No dedicated connection needed — controller reads unit status directly via in.hasTable/in.hasIndex/in.unitDone

  /************************* Reg Updates **************************************/

  for (i <- 0 until NumPipeIn) {
    when (in.s0pipes(i).valid) {
      val isSingle = (in.s0pipes(i).bits.blockSize & 1.U).orR
      pipeBlockSize(i) := in.s0pipes(i).bits.blockSize
      // Only latch a position when the controller actually assigned one for this
      // pipe. An rvs3 (SEW=8 ei16) places no table, so the controller's
      // pipeTablePos.valid is 0 and the register must not capture an uncontrolled
      // value. For blockSize=1 the position is the block base itself.
      when (isSingle || controller.out.pipeTablePos(i).valid) {
        pipeTablePos(i) := Mux(isSingle,
          in.s0pipes(i).bits.blockBase,
          controller.out.pipeTablePos(i).bits,
        )
      }
      when (isSingle || controller.out.pipeIndexPos(i).valid) {
        pipeIndexPos(i) := Mux(isSingle,
          in.s0pipes(i).bits.blockBase,
          controller.out.pipeIndexPos(i).bits,
        )
      }
    }
  }

  /***************************** wb *******************************************/

  // wbReady routing: pipe -> unit via wbSelOH
  for (j <- 0 until NumUnits) {
    units(j).in.wbReady := Mux1H(controller.out.wbSelOH.map(_(j)), in.wbReady)
  }

  for (i <- 0 until NumPipeOut) {
    out.pipes(i).valid := controller.out.wbSelOH(i).orR
    out.pipes(i).bits := Mux1H(controller.out.wbSelOH(i), units.map(_.out.wb.bits))
  }

  /************************* vperm wakeup *************************************/

  // Wakeup requests are arbitrated in the controller (mirror of writeback); route the
  // per-unit allTableValid gate and the per-pipe acks back to the selected units.
  for (i <- 0 until NumUnits) {
    units(i).in.allTableValid := controller.out.unitAllTableValid(i)
    units(i).in.wakeAck := VecInit(controller.out.wakeSelOH.zip(in.wakeAck).map {
      case (selOH, ack) => selOH(i) && ack
    }).asUInt.orR
    units(i).in.blockedByEqCounter := controller.out.blockedByEqCounter(i)
  }

  for (p <- 0 until NumPipeOut) {
    out.wake(p).valid := controller.out.wakeSelOH(p).orR
    out.wake(p).bits := Mux1H(controller.out.wakeSelOH(p), units.map(_.out.wake.bits))
  }
}

object GatherRing {
  class In[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    // Generated by out.meta and flush from rob
    val flush = Vec(NumUnits, Bool())
    val s0pipes = Vec(NumPipeIn, ValidIO(new PipeInS0))
    val s1pipes = Vec(NumPipeIn, ValidIO(new PipeInS1(metaGen)))
    val wbReady = Vec(NumPipeOut, Bool())
    // VPerm wakeup acks (generated externally, e.g. gated by wb busy table), one per pipe
    val wakeAck = Vec(NumPipeOut, Bool())
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    // writeback
    val pipes = Vec(NumPipeOut, ValidIO(new PipeOut(metaGen)))
    val metas = Vec(NumUnits, metaGen)
    val unitStatus = Vec(NumUnits, new Status)
    // arbitrated vperm wakeup requests (mirror of pipes)
    val wake = Vec(NumPipeOut, ValidIO(new GatherRingUnit.Wake(metaGen)))
    val debug = Option.when(param.debug)(Vec(NumUnits, new GatherRingUnit.Debug))
  }

  class PipeInS0(implicit val param: GatherParam) extends GatherBundle {
    /**
     * The index of placement in block
     */
    val placeIdx = PipeInIdx()

    /**
     * The start index of the block in [[GatherRing]]
     */
    val blockBase   = UnitIdx()

    /**
     * The size of the block in [[GatherRing]]
     */
    val blockSize   = BlockSize()

    // SEW=8 vrgatherei16: index routing is deterministic (blockBase | placeIdx,
    // placeIdx = uopIdx % blockSize), bypassing the entryIndex free-slot search.
    // Index slots of a freshly allocated block are always clean, so skipping the
    // search cannot collide; it guarantees gather i and rvs3 i land on the same unit.
    val isEi16E8    = Bool()

    // SEW=8 vrgatherei16: 1 = this uop is the rvs3 (old-data) half of a gather
    // pair; it places an index (PREFILL) but no table, so the block controller
    // must not generate a tablePos for it.
    val isRvs3      = Bool()

    // uop index within the instruction; the block controller uses it for the
    // SEW=8 ei16 index routing (indexPos = uopIdx % blockSize).
    val uopIdx      = UopIdx()

  }

  class PipeInS1[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val placeIdx  = PipeInIdx()
    val blockBase = UnitIdx()
    val blockSize = BlockSize()

    // Per-element merged: 2b flags + 8b data (index or prefill)
    val elemFlags = Vec(NumElem, ElemFlags())
    val elemData  = Vec(NumElem, UInt(8.W))

    // SEW=8 vrgatherei16: 1 = this uop is the rvs3 (old-data) uop of a gather
    val isRvs3   = Bool()
    // 1 = this gather uop has paired rvs3 uops (PREFILL slots wait for old-data)
    val waitRvs3 = Bool()

    val tableIdx   = UnitIdx()
    val table      = Vec(NumElem, Elem())
    val meta       = metaGen
  }

  class PipeOut[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val data = UInt(DLEN.W)
    val meta = metaGen
  }

  def main(args: Array[String]): Unit = {
    import _root_.circt.stage._

    val targetDir = "build/vector"

    println(s"Generating the GatherRing hardware in $targetDir")

    val firtoolOpts = Array(
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array(
        "--target",
        "systemverilog",
        "--target-dir",
        targetDir,
      ) ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(
        () => new GatherRing(FakeRobPtr())(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
