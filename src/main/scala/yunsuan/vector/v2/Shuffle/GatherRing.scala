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

  controller.in.s0pipes  := in.s0pipes
  controller.in.s1pipes.zipWithIndex.foreach { case (s1, i) =>
    s1.valid := in.s1pipes(i).valid
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

    val prevFlowSource: Option[ValidIO[GatherRingUnit.RingFlow]] = Option.when(i > 0)(units(i - 1).out.flow)

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
  }

  for (i <- 0 until NumUnits) {
    units(i).in.willFlow := controller.out.willFlow(i)
    units(i).in.clearTable := controller.out.clearTable(i)
  }

  private val validMatrixPipeUnitTable = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))
  private val validMatrixPipeUnitIndex = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))

  for (i <- 0 until NumPipeIn) {
    validMatrixPipeUnitTable(i)(pipeTablePos(i)) := in.s1pipes(i).valid
    validMatrixPipeUnitIndex(i)(pipeIndexPos(i)) := in.s1pipes(i).valid
  }

  for (u <- 0 until NumUnits) {
    assert(PopCount(validMatrixPipeUnitTable.map(_(u))) <= 1.U,
      s"multiple table writers target GatherRing unit $u")
    assert(PopCount(validMatrixPipeUnitIndex.map(_(u))) <= 1.U,
      s"multiple index writers target GatherRing unit $u")

    units(u).in.placeTable.valid := Cat(validMatrixPipeUnitTable.map(_(u))).orR
    units(u).in.placeTable.bits  := Mux1H(
      validMatrixPipeUnitTable.map(_(u)),
      in.s1pipes.map(s1 => PlaceTableRequest.fromPipeInS1(s1.bits, u))
    )
    units(u).in.placeIndex.valid := Cat(validMatrixPipeUnitIndex.map(_(u))).orR
    units(u).in.placeIndex.bits  := Mux1H(
      validMatrixPipeUnitIndex.map(_(u)),
      in.s1pipes.map(s1 => PlaceIndexRequest.fromPipeInS1(s1.bits))
    )
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
      pipeTablePos(i) := Mux(isSingle,
        in.s0pipes(i).bits.blockBase,
        controller.out.pipeTablePos(i).bits,
      )
      pipeIndexPos(i) := Mux(isSingle,
        in.s0pipes(i).bits.blockBase,
        controller.out.pipeIndexPos(i).bits,
      )
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

  }

  class PipeInS1[T <: Data](metaGen: => T)(implicit val param: GatherParam) extends GatherBundle {
    val placeIdx  = PipeInIdx()
    val blockBase = UnitIdx()
    val blockSize = BlockSize()

    // Per-element merged: 2b flags + 8b data (index or prefill)
    val elemFlags = Vec(NumElem, ElemFlags())
    val elemData  = Vec(NumElem, UInt(8.W))

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
