package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.v2.Shuffle.GatherCommon._
import yunsuan.vector.Common._


class GatherRingController(implicit val param: GatherParam) extends GatherModule {
  val in = IO(Input(new GatherRingController.In))
  val out = IO(Output(new GatherRingController.Out))

  // ── Instantiate block controllers ────────────────────
  private val ring2Controllers = Seq.tabulate(NumUnits / 2)(i => Module(new GatherRingBlockController(2, 2 * i)))
  private val ring4Controllers = Seq.tabulate(NumUnits / 4)(i => Module(new GatherRingBlockController(4, 4 * i)))
  private val ring8Controllers = Seq.tabulate(NumUnits / 8)(i => Module(new GatherRingBlockController(8, 8 * i)))

  private val s1PipesTableValid = in.s1pipes.map(x => UIntToOH(x.bits.tablePos, NumUnits))
  private val s1PipesIndexValid = in.s1pipes.map(x => UIntToOH(x.bits.indexPos, NumUnits))

  // ── Wire block controllers ───────────────────────────

  private val ring2UnitMask = Cat(in.blockSize.map(x => (x & 2.U).orR).reverse)
  private val ring2S1TableValid = Mux1H(in.s1pipes.map(x => x.valid && (x.bits.blockSize & 2.U).orR), s1PipesTableValid)
  private val ring2S1IndexValid = Mux1H(in.s1pipes.map(x => x.bits.indexValid && (x.bits.blockSize & 2.U).orR), s1PipesIndexValid)

  for ((bc, i) <- ring2Controllers.zipWithIndex) {
    val base = 2 * i
    bc.in.unitTableValid := (in.hasTable.asUInt & ring2UnitMask)(base + 1, base)
    bc.in.unitIndexValid := (in.hasIndex.asUInt & ring2UnitMask)(base + 1, base)
    bc.in.s1TableValid := ring2S1TableValid(base + 1, base)
    bc.in.s1IndexValid := ring2S1IndexValid(base + 1, base)
    bc.in.indexValidNext := in.hasIndexNext.asUInt(base + 1, base)
    bc.in.counter := VecInit(in.counter.slice(base, base + 2))
    // ring2 never hosts hole blocks (segment 3/5/6/7 round up to size 4/8)
    bc.in.blockFieldNum := 2.U
    for (j <- 0 until NumPipeIn) {
      // blockSize(1)=m2 gates ring2: only active for LMUL=2, not 4 or 8
      bc.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockSize(1) &&
        in.s0pipes(j).bits.blockBase.drop(1) === bc.blockBase.U(UnitIdx().getWidth.W).drop(1)
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
      bc.in.pipeIsEi16E8(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isEi16E8
      bc.in.pipeIsRvs3(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isRvs3
      bc.in.pipeUopIdx(j) := in.s0pipes(j).bits.uopIdx
    }
  }

  // ── ring4 hole state (segment 3 in a size-4 block) ─────────────────────
  // Per-block resident fieldNum, gated by aliveness: a drained/empty block
  // reports 0, which also retires the forcing (D5 lifecycle). Qualified with
  // the resident block actually being size-4 (one-hot bit2) so a ring4 bc
  // never forces for blocks of other sizes sharing its units' history.
  private val ring4FieldNum = Seq.tabulate(NumUnits / 4) { i =>
    val base = 4 * i
    val alive = (base until base + 4).map(u =>
      in.hasTable(u) || in.hasIndex(u) || in.counter(u) =/= 0.U
    ).reduce(_ || _)
    Mux(alive && in.blockSize(base)(2), in.blockFieldNum(base), 0.U)
  }
  // Index holes = HIGH units f..size-1 (passive: table storage/forward only)
  private val ring4IndexHoleBits = Cat(Seq.tabulate(NumUnits / 4) { i =>
    val f = ring4FieldNum(i)
    Mux(f >= 2.U && f < 4.U, (Fill(4, 1.B) << f)(3, 0), 0.U(4.W))
  }.reverse)

  private val ring4UnitMask = Cat(in.blockSize.map(x => (x & 4.U).orR).reverse) | ring4IndexHoleBits
  private val s1Ring4TableValid = Mux1H(in.s1pipes.map(x => x.valid && (x.bits.blockSize & 4.U).orR), s1PipesTableValid)
  private val s1Ring4IndexValid = Mux1H(in.s1pipes.map(x => x.bits.indexValid && (x.bits.blockSize & 4.U).orR), s1PipesIndexValid)

  for ((bc, i) <- ring4Controllers.zipWithIndex) {
    val base = 4 * i

    bc.in.unitTableValid := (in.hasTable.asUInt & ring4UnitMask)(base + 3, base)
    // D4: index holes are forced index-valid so the block's index-completion,
    // clearTable and willFlow criteria treat them as done/flowing.
    bc.in.unitIndexValid := ((in.hasIndex.asUInt | ring4IndexHoleBits) & ring4UnitMask)(base + 3, base)
    bc.in.s1TableValid := s1Ring4TableValid(base + 3, base)
    bc.in.s1IndexValid := s1Ring4IndexValid(base + 3, base)
    bc.in.indexValidNext := in.hasIndexNext.asUInt(base + 3, base)
    bc.in.counter := VecInit(in.counter.slice(base, base + 4))
    bc.in.blockFieldNum := ring4FieldNum(i)(log2Up(4 + 1) - 1, 0)
    for (j <- 0 until NumPipeIn) {
      // blockSize(2) gates ring4: only active for LMUL=4
      bc.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockSize(2) &&
        in.s0pipes(j).bits.blockBase.drop(2) === bc.blockBase.U(UnitIdx().getWidth.W).drop(2)
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
      bc.in.pipeIsEi16E8(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isEi16E8
      bc.in.pipeIsRvs3(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isRvs3
      bc.in.pipeUopIdx(j) := in.s0pipes(j).bits.uopIdx
    }
  }

  // ── ring8 hole state (segment 5/6/7 in a size-8 block) ──────────────────
  private val ring8FieldNum = Seq.tabulate(NumUnits / 8) { i =>
    val base = 8 * i
    val alive = (base until base + 8).map(u =>
      in.hasTable(u) || in.hasIndex(u) || in.counter(u) =/= 0.U
    ).reduce(_ || _)
    Mux(alive && in.blockSize(base)(3), in.blockFieldNum(base), 0.U)
  }
  private val ring8IndexHoleBits = Cat(Seq.tabulate(NumUnits / 8) { i =>
    val f = ring8FieldNum(i)
    Mux(f >= 2.U && f < 8.U, (Fill(8, 1.B) << f)(7, 0), 0.U(8.W))
  }.reverse)

  private val ring8UnitMask = Cat(in.blockSize.map(x => (x & 8.U).orR).reverse) | ring8IndexHoleBits
  private val s1Ring8TableValid = Mux1H(in.s1pipes.map(x => x.valid && (x.bits.blockSize & 8.U).orR), s1PipesTableValid)
  private val s1Ring8IndexValid = Mux1H(in.s1pipes.map(x => x.bits.indexValid && (x.bits.blockSize & 8.U).orR), s1PipesIndexValid)

  for ((bc, i) <- ring8Controllers.zipWithIndex) {
    val base = 8 * i
    bc.in.unitTableValid := (in.hasTable.asUInt & ring8UnitMask)(base + 7, base)
    bc.in.unitIndexValid := ((in.hasIndex.asUInt | ring8IndexHoleBits) & ring8UnitMask)(base + 7, base)
    bc.in.s1TableValid := s1Ring8TableValid(base + 7, base)
    bc.in.s1IndexValid := s1Ring8IndexValid(base + 7, base)
    bc.in.indexValidNext := in.hasIndexNext.asUInt(base + 7, base)
    bc.in.counter := VecInit(in.counter.slice(base, base + 8))
    bc.in.blockFieldNum := ring8FieldNum(i)
    for (j <- 0 until NumPipeIn) {
      // blockSize(3) gates ring8: only active for LMUL=8. With multiple
      // ring8 blocks (NumUnits > 8), also select the block containing the
      // uop's blockBase; otherwise every ring8 controller claims the same
      // pipe and placement is duplicated into the last unit of the block.
      val ring8BlockSelected = if (NumUnits == 8) {
        true.B
      } else {
        in.s0pipes(j).bits.blockBase.drop(3) === bc.blockBase.U(UnitIdx().getWidth.W).drop(3)
      }
      bc.in.pipePlaceIdx(j).valid :=
        in.s0pipes(j).valid && in.s0pipes(j).bits.blockSize(3) &&
        ring8BlockSelected
      bc.in.pipePlaceIdx(j).bits := in.s0pipes(j).bits.placeIdx
      bc.in.pipeIsEi16E8(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isEi16E8
      bc.in.pipeIsRvs3(j) := bc.in.pipePlaceIdx(j).valid && in.s0pipes(j).bits.isRvs3
      bc.in.pipeUopIdx(j) := in.s0pipes(j).bits.uopIdx
    }
  }

  // ── Aggregate block controller outputs ───────────────
  for (i <- 0 until NumPipeIn) {
    out.pipeTablePos(i) := Mux1HValidIO(
      (ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.tablePos(i))
    )
    out.pipeIndexPos(i) := Mux1HValidIO(
      (ring2Controllers ++ ring4Controllers ++ ring8Controllers).map(_.out.indexPos(i))
    )
    // Debug: bitmap of which controller type is active for this pipe
    out.dbgPipeCtrlValid(i) := Cat(
      ring8Controllers.map(_.in.pipePlaceIdx(i).valid).reduce(_ || _),
      ring4Controllers.map(_.in.pipePlaceIdx(i).valid).reduce(_ || _),
      ring2Controllers.map(_.in.pipePlaceIdx(i).valid).reduce(_ || _),
    )
  }

  private val ring2WillFlowVec = Cat(ring2Controllers.map(_.out.willFlow).reverse)
  private val ring4WillFlowVec = Cat(ring4Controllers.map(_.out.willFlow).reverse)
  private val ring8WillFlowVec = Cat(ring8Controllers.map(_.out.willFlow).reverse)
  // Mask each ring's willFlow by its active units before ORing (same reasoning
  // as clearTable): only the ring matching the unit's blockSize should drive it.
  out.willFlow :=
    (ring2WillFlowVec & ring2UnitMask) |
    (ring4WillFlowVec & ring4UnitMask) |
    (ring8WillFlowVec & ring8UnitMask)

  private val ring2ClearTable = Cat(ring2Controllers.map(_.out.clearTable).reverse)
  private val ring4ClearTable = Cat(ring4Controllers.map(_.out.clearTable).reverse)
  private val ring8ClearTable = Cat(ring8Controllers.map(_.out.clearTable).reverse)
  // Mask each ring's clearTable by its active units before ORing: inactive
  // rings (e.g. ring2/ring8 for an m4 block) would otherwise leak spurious
  // always-1 bits (empty-set last unit) and clear tables prematurely.
  out.clearTable :=
    (ring2ClearTable & ring2UnitMask) |
    (ring4ClearTable & ring4UnitMask) |
    (ring8ClearTable & ring8UnitMask)

  // Per-unit "all tables of the corresponding ring block are valid" gate for the vperm wakeup.
  // Aggregated like willFlow (OR across ring2/4/8); the active controller is the one matching
  // the current blockSize, the inactive ones do not conflict.
  private val ring2AllTableValidVec = Cat(ring2Controllers.map(_.out.allTableValid).reverse)
  private val ring4AllTableValidVec = Cat(ring4Controllers.map(_.out.allTableValid).reverse)
  private val ring8AllTableValidVec = Cat(ring8Controllers.map(_.out.allTableValid).reverse)
  // blockSize=1 (m1/fractional) has no ring: the single table is valid as soon as it is
  // placed, so the vperm wake is gated directly on the unit's hasTable.
  private val singleUnitMask = Cat(in.blockSize.map(_ === 1.U).reverse)
  out.unitAllTableValid := VecInit((
    ring2AllTableValidVec & ring2UnitMask |
    ring4AllTableValidVec & ring4UnitMask |
    ring8AllTableValidVec & ring8UnitMask |
    in.hasTable.asUInt & singleUnitMask
  ).asBools)

  // ── Writeback arbitration (interleaved) ──────────────
  // Pipe p serves units where unitIdx % NumPipeOut == p
  private val pipeGroupSize = Seq.tabulate(NumPipeOut) { p =>
    (NumUnits - p + NumPipeOut - 1) / NumPipeOut
  }

  private val pipeUnitDone = Seq.tabulate(NumPipeOut) { p =>
    Cat(Seq.tabulate(pipeGroupSize(p)) { k =>
      in.wbValid(p + k * NumPipeOut)
    }.reverse)
  }

  private val localSelOH = pipeUnitDone.map(oh => PriorityEncoderOH(oh))

  out.wbSelOH := Seq.tabulate(NumPipeOut) { p =>
    val ohBits = localSelOH(p).asBools
    VecInit.tabulate(NumUnits) { u =>
      if (u % NumPipeOut == p) {
        val k = u / NumPipeOut
        ohBits(k)
      } else {
        false.B
      }
    }.asUInt
  }

  // ── VPerm wakeup arbitration (interleaved, mirror of writeback) ──
  private val pipeWakeValid = Seq.tabulate(NumPipeOut) { p =>
    Cat(Seq.tabulate(pipeGroupSize(p)) { k =>
      in.wakeValid(p + k * NumPipeOut)
    }.reverse)
  }

  private val localWakeSelOH = pipeWakeValid.map(oh => PriorityEncoderOH(oh))

  out.wakeSelOH := Seq.tabulate(NumPipeOut) { p =>
    val ohBits = localWakeSelOH(p).asBools
    VecInit.tabulate(NumUnits) { u =>
      if (u % NumPipeOut == p) {
        val k = u / NumPipeOut
        ohBits(k)
      } else {
        false.B
      }
    }.asUInt
  }


}

object GatherRingController {
  class In(implicit val param: GatherParam) extends GatherBundle {
    val s0pipes  = Vec(NumPipeIn, ValidIO(new GatherRing.PipeInS0))
    val s1pipes  = Vec(NumPipeIn, ValidIO(new InPipe1))
    val hasTable = Vec(NumUnits, Bool())
    val hasIndex = Vec(NumUnits, Bool())
    val hasIndexNext = Vec(NumUnits, Bool())
    val blockSize = Vec(NumUnits, BlockSize())
    val wbValid = UInt(NumUnits.W)
    val wakeValid = UInt(NumUnits.W)
    val counter = Vec(NumUnits, UInt(log2Up(NumUnits + 1).W))
    // Per-unit resident block field count (GatherRingUnit.status.blockFieldNum);
    // valid while the unit's block is alive, 0 when empty.
    val blockFieldNum = Vec(NumUnits, BlockFieldNum())
  }

  class InPipe1(implicit val param: GatherParam) extends GatherBundle {
    val indexValid = Bool()
    val tablePos = UnitIdx()
    val indexPos = UnitIdx()
    val blockSize = BlockSize()
  }

  class Out(implicit val param: GatherParam) extends GatherBundle {
    val pipeTablePos = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val pipeIndexPos = Vec(NumPipeIn, ValidIO(UnitIdx()))
    val willFlow     = UInt(NumUnits.W)
    val clearTable   = UInt(NumUnits.W)
    val wbSelOH      = Vec(NumPipeOut, UInt(NumUnits.W))
    val unitAllTableValid = Vec(NumUnits, Bool())
    val wakeSelOH      = Vec(NumPipeOut, UInt(NumUnits.W))
    // Debug: per-pipe controller valid bitmap [ring8, ring4, ring2]
    val dbgPipeCtrlValid = Vec(NumPipeIn, UInt(3.W))
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
        () => new GatherRingController()(GatherParam.default)
      ) +: firtoolAnno
    )

    println("done")
  }
}
