package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._
import yunsuan.vector.v2.Shuffle.GatherRing.RingCtrl
import yunsuan.vector.v2.Shuffle.GatherRingUnit.{PlaceIndexRequest, PlaceTableRequest, Status}
import chisel3.experimental.BundleLiterals._

class GatherRing[T <: Data](
  metaGen: => T
)(
  implicit val param: Param
) extends GatherModule {
  override def desiredName: String = super.desiredName + s"_vlen${VLEN}b_dlen${DLEN}b"

  val in = IO(Input(new GatherRing.In(metaGen)))
  val out = IO(Output(new GatherRing.Out(metaGen)))

  val unitStatus = out.unitStatus

  /*********************** Module Definations *********************************/

  private val ring2Controllers = Seq.tabulate(NumUnits / 2)(i => Module(new GatherRingBlockController(2, 2 * i)))
  private val ring4Controllers = Seq.tabulate(NumUnits / 4)(i => Module(new GatherRingBlockController(4, 4 * i)))
  private val ring8Controllers = Seq.tabulate(NumUnits / 8)(i => Module(new GatherRingBlockController(8, 8 * i)))

  private val units = Seq.tabulate(NumUnits) {
    i => Module(new GatherRingUnit(unitIdx = i, metaGen)(param)).suggestName(s"ringUnit_$i")
  }

  private val controller = Module(new GatherRingController)

  /************************* Reg Definations **********************************/

  val willFlowVec: Vec[Bool] = Reg(Vec(NumUnits, Bool()))
  val pipeTablePos = Reg(Vec(NumPipeIn, UnitIdx()))
  val pipeIndexPos = Reg(Vec(NumPipeIn, UnitIdx()))

  val willFlowVecNext: Vec[Bool] = Wire(chiselTypeOf(willFlowVec))

  /******************** Connections of ringControllers ************************/

  private val ring2Units: Seq[Seq[GatherRingUnit[T]]] = units.grouped(2).toSeq
  for ((allocator, i) <- ring2Controllers.zipWithIndex) {
    allocator.in.indexValid := Cat(ring2Units(i).map(_.out.status.hasIndex).reverse)
    allocator.in.tableValid := Cat(ring2Units(i).map(_.out.status.hasTable).reverse)
    for (j <- allocator.in.pipePlaceIdx.indices) {
      allocator.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockBase.drop(1) === allocator.blockBase.U.drop(1)
      allocator.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  private val ring4Units = units.grouped(4).toSeq
  for ((allocator, i) <- ring4Controllers.zipWithIndex) {
    allocator.in.indexValid := Cat(ring4Units(i).map(_.out.status.hasIndex).reverse)
    allocator.in.tableValid := Cat(ring4Units(i).map(_.out.status.hasTable).reverse)
    for (j <- allocator.in.pipePlaceIdx.indices) {
      allocator.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockBase.drop(2) === allocator.blockBase.U.drop(2)
      allocator.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  private val ring8Units = units.grouped(8).toSeq
  for ((allocator, i) <- ring8Controllers.zipWithIndex) {
    allocator.in.indexValid := Cat(ring8Units(i).map(_.out.status.hasIndex).reverse)
    allocator.in.tableValid := Cat(ring8Units(i).map(_.out.status.hasTable).reverse)
    for (j <- allocator.in.pipePlaceIdx.indices) {
      // FIXME: if there are more than one ring8
      allocator.in.pipePlaceIdx(j).valid := in.s0pipes(j).valid.ensuring(ring8Units.size == 1)
      allocator.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
    }
  }

  /*********************** Connections of units *******************************/

  private val ring2TailUnits = ring2Units.map(_.last)
  private val ring4TailUnits = ring4Units.map(_.last)
  private val ring8TailUnits = ring8Units.map(_.last)

  for (i <- 0 until NumUnits) {

    val prevFlowSource: Option[ValidIO[GatherRingUnit.RingFlow]] = Option.when(i > 0)(units(i - 1).out.flow)

    val ring2FlowSource = Option.when(i % 2 == 0)(ring2TailUnits(i / 2).out.flow)
    val ring4FlowSource = Option.when(i % 4 == 0)(ring4TailUnits(i / 4).out.flow)
    val ring8FlowSource = Option.when(i % 8 == 0)(ring8TailUnits(i / 8).out.flow)

    val flowSources: Seq[ValidIO[GatherRingUnit.RingFlow]] = Seq(
      prevFlowSource,
      ring2FlowSource,
      ring4FlowSource,
      ring8FlowSource,
    ).flatten

    units(i).in.flow := Mux1HValidIO(flowSources)
  }

  // vectorized signals
  private val hasIndexVec    = VecInit(units.map(_.out.status.hasIndex))
  private val hasTableVec    = VecInit(units.map(_.out.status.hasTable))
  private val isFirstVec     = VecInit(units.map(_.out.status.isFirstTable))
  private val passedEntryVec = VecInit(units.map(_.out.status.passedEntry))

  private val ring2WillFlowVec = Cat(ring2Controllers.map(_.out.willFlow).reverse)
  private val ring4WillFlowVec = Cat(ring4Controllers.map(_.out.willFlow).reverse)
  private val ring8WillFlowVec = Cat(ring8Controllers.map(_.out.willFlow).reverse)
  private val unitWillFlowVec = ring2WillFlowVec | ring4WillFlowVec | ring8WillFlowVec

  private val unitsControllers = units.indices.map(
    i => Seq(
      ring2Controllers(i / 2),
      ring4Controllers(i / 4),
      ring8Controllers(i / 8),
    )
  )

  for (i <- 0 until NumUnits) {
    units(i).in.willFlow := willFlowVec(i)
  }

  private val validMatrixPipeUnitTable = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))
  private val validMatrixPipeUnitIndex = WireInit(VecInit.fill(NumPipeIn, NumUnits)(false.B))

  for (i <- 0 until NumPipeIn) {
    validMatrixPipeUnitTable(pipeTablePos(i)) := in.s1pipes(i).valid
    validMatrixPipeUnitIndex(pipeIndexPos(i)) := in.s1pipes(i).valid
  }

  for (u <- 0 until NumUnits) {
    units(u).in.placeTable.valid := Cat(validMatrixPipeUnitTable.map(_(u))).orR
    units(u).in.placeTable.bits  := Mux1H(validMatrixPipeUnitTable.map(_(u)), in.s1pipes)
    units(u).in.placeIndex.valid := Cat(validMatrixPipeUnitIndex.map(_(u))).orR
    units(u).in.placeIndex.bits  := Mux1H(validMatrixPipeUnitIndex.map(_(u)), in.s1pipes)
  }

  for (i <- 0 until NumUnits) {
    units(i).in.flush := in.flush(i)
  }

  /******************** Connections of ringControllers ************************/

  controller.in.unitDone := Cat(units.map(_.out.status.done))

  /************************* Reg Updates **************************************/

  for (i <- 0 until NumPipeIn) {
    when (in.s0pipes(i).valid) {
      pipeTablePos(i) := Mux1HValidIO((ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.tablePos(i))).bits
      pipeIndexPos(i) := Mux1HValidIO((ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.indexPos(i))).bits
    }
  }

  // TODO: gate it
  willFlowVec := willFlowVecNext

  /***************************** wb *******************************************/

  for (i <- 0 until NumPipeOut) {
    out.pipes(i).valid := controller.out.wbSelOH(i).orR
    out.pipes(i).bits := Mux1H(controller.out.wbSelOH(i), units.map(_.out.wb.bits))
  }
}

object GatherRing {
  class In[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    // Generated by out.meta and flush from rob
    val flush = Vec(NumUnits, Bool())
    val s0pipes = Vec(NumPipeIn, ValidIO(new PipeInS0))
    val s1pipes = Vec(NumPipeIn, ValidIO(new PipeInS1(metaGen)))
    val wbReady = Vec(NumPipeOut, Bool())
  }

  class Out[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    // writeback
    val pipes = Vec(NumPipeOut, ValidIO(new PipeOut(metaGen)))
    val metas = Vec(NumUnits, metaGen)
    val unitStatus = Vec(NumUnits, new Status)
  }

  class PipeInS0(implicit val param: Param) extends GatherBundle {
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

  class PipeInS1[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
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

    /**
     * Precalculated index vector
     */
    val index       = Vec(NumElem, Index())

    /**
     * If the index need to do gather
     */
    val indexValid  = Vec(NumElem, Bool())

    /**
     * The result need to be filled with zero, when the index of vrgather is greater than vlmax
     */
    val fillZeros   = Vec(NumElem, Bool())

    /**
     * The result need to be filled with scalar value(x[rs1]), when the executed uop is vslide1up/down
     */
    val fillScala   = Vec(NumElem, Bool())

    /**
     * The result need to be filled with inactive/tail data
     */
    val prefillData = Vec(NumElem, Elem())

    /**
     * The scalar data x[rs1] or f[fs1]
     */
    val scalarData = UInt(XLEN.W)

    /**
     * the index of this table in v[vs2]
     */
    val tableIdx = UnitIdx()

    /**
     * The data of table
     */
    val table = Vec(NumElem, Elem())

    /**
     * The meta data
     */
    val meta = metaGen
  }

  class PipeOut[T <: Data](metaGen: => T)(implicit val param: Param) extends GatherBundle {
    val data = UInt(DLEN.W)
    val meta = metaGen
  }

  class RingCtrl(size: Int) extends Bundle {
    val tableValid = UInt(size.W)
    val indexValid = UInt(size.W)

    def allIndexValid: Bool = indexValid.andR
    def indexCnt: UInt = PopCount(indexValid)
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
        () => new GatherRing(FakeRobPtr())(Param.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
