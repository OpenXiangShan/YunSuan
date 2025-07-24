package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption

import scala.language.reflectiveCalls

class GatherArray(
  val vlen: Int,
  val dlen: Int,
  xlen: Int
) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b"

  val ResWidth = MinDataWidth

  val flushVec = IO(Input(Vec(MaxTableNum, Bool())))

  val in = IO(Flipped(ValidIO(new Bundle {
    val info = new GatherInfoBundle(vlen, dlen)
    val op = new GatherArrayOpBundle
    val uopIdx = UInt(TableIdxWidth.W)

    // used as vs1 of vrgather
    val vs1 = UInt(dlen.W)
    val vs2 = UInt(dlen.W)
    val vs3 = UInt(dlen.W)
    val srcMask = UInt(vlen.W)
    val vl = UInt(VlWidth.W)
    val rs1 = UInt(xlen.W)
  })))

  val out = IO(DecoupledIO(new Bundle {
    val dest = UInt(dlen.W)
  }))

  private val inTable = in.bits.vs2.splitToVecN(NumElem)
  private val info = in.bits.info
  private val vs1 = in.bits.vs1
  private val vs2 = in.bits.vs2
  private val vs3 = in.bits.vs3
  private val rs1 = in.bits.rs1
  private val op: GatherArrayOpBundle = in.bits.op
  private val uopIdx = in.bits.uopIdx
  private val vl = in.bits.vl

  private val gatherUnits = Seq.fill(MaxTableNum)(Module(new GatherUnit(vlen = vlen, dlen = dlen, maxTableNum = MaxTableNum)))
  private val compressIndexGen = Module(new CompressIndexGen(vlen = vlen, dlen = dlen))
  private val gatherIndexGen = Module(new GatherIndexGen(vlen = vlen, dlen = dlen, MinDataWidth = MinDataWidth))
  private val slideUpIndexGen = Module(new SlideUpIndexGen(vlen = vlen, dlen = dlen))
  private val slideDownIndexGen = Module(new SlideDownIndexGen(vlen = vlen, dlen = dlen))

  private val gatherModTable = Mux(
    compressIndexGen.out.valid,
    compressIndexGen.out.data,
    inTable,
  )

  private val gatherModUopIdx = Mux(
    compressIndexGen.out.valid,
    compressIndexGen.out.uopIdx,
    in.bits.uopIdx,
  )

  private val usedVl = Mux(
    compressIndexGen.out.valid,
    compressIndexGen.out.count1s,
    in.bits.vl,
  )

  // each min size data 1 bit mask
  private val maskIsTrue = MaskSelectAndDupEI16(vlen, dlen)(in.bits.srcMask, in.bits.uopIdx, in.bits.info.sew, op.vrgatherei16)
  private val iIsLtVl = VlCompareBitVecModule(vlen, dlen, MinDataWidth)(
    usedVl,
    info.sew,
    uopIdx,
    (i, vl) => i < vl,
    "LessThanVl",
  )

  private val gatherModOldTable = Mux(
    compressIndexGen.out.valid,
    compressIndexGen.out.vs3.splitToVecByWidth(MinDataWidth),
    vs3.splitToVecByWidth(MinDataWidth),
  )

  private val elemActive = Wire(Vec(NumElem, Bool()))
  private val elemInactive = Wire(Vec(NumElem, Bool()))
  private val elemTail = Wire(Vec(NumElem, Bool()))

  for (i <- 0 until NumElem) {
    elemActive(i)   := iIsLtVl(i) && (maskIsTrue(i) || op.compress)
    elemInactive(i) := iIsLtVl(i) && !maskIsTrue(i) && !op.compress
    elemTail(i)     := !iIsLtVl(i)
  }

  for (i <- gatherUnits.indices) {
    gatherUnits(i).flush := flushVec(i)
    gatherUnits(i).in.ctrl.valid := in.valid && !op.compress && in.bits.uopIdx === i.U
    gatherUnits(i).in.ctrl.bits.isEntryUnit := (i == 0).B // Todo: support more entry unit
    gatherUnits(i).in.ctrl.bits.info := in.bits.info
    gatherUnits(i).in.ctrl.bits.half := false.B

    gatherUnits(i).in.compress.valid := compressIndexGen.out.validVec(i)
    gatherUnits(i).in.compress.bits := compressIndexGen.out.index

    gatherUnits(i).in.gatherei16  := gatherIndexGen.out.gatherei16
    gatherUnits(i).in.gather      := gatherIndexGen.out.gather
    gatherUnits(i).in.slideUp     := slideUpIndexGen.out.slideup
    gatherUnits(i).in.slide1Up    := slideUpIndexGen.out.slide1up
    gatherUnits(i).in.slideDown   := slideDownIndexGen.out.slideDown
    gatherUnits(i).in.slide1Down  := slideDownIndexGen.out.slide1Down

    gatherUnits(i).in.oldTable.dataVec := gatherModOldTable

    gatherUnits(i).in.scalaTable := rs1

    gatherUnits(i).in.elemActive := elemActive
    gatherUnits(i).in.elemInactive := elemInactive
    gatherUnits(i).in.elemTail := elemTail

    gatherUnits(i).out.ready := out.ready
  }

  gatherUnits.head.in.table.valid := in.valid && !op.compress || compressIndexGen.out.valid
  gatherUnits.head.in.table.bits.dataVec := gatherModTable
  gatherUnits.head.in.table.bits.idx := gatherModUopIdx

  for (i <- gatherUnits.indices.drop(1)) {
    gatherUnits(i).in.table := gatherUnits(i - 1).toNextUnit
  }

  compressIndexGen.in.valid := in.valid && op.compress
  compressIndexGen.in.uopIdx := uopIdx
  compressIndexGen.in.byteMask := in.bits.srcMask
  compressIndexGen.in.data := inTable
  compressIndexGen.in.vs3 := vs3

  gatherIndexGen.in.valid := in.valid && (op.vrgather_v || op.vrgatherei16)
  gatherIndexGen.in.bits.isGatherEI16 := op.vrgatherei16
  gatherIndexGen.in.bits.uopIdx := uopIdx
  gatherIndexGen.in.bits.sew := info.sew
  gatherIndexGen.in.bits.ei.e64 := op.vrgather_v && info.sew.e64
  gatherIndexGen.in.bits.ei.e32 := op.vrgather_v && info.sew.e32
  gatherIndexGen.in.bits.ei.e16 := op.vrgather_v && info.sew.e16 || op.vrgatherei16
  gatherIndexGen.in.bits.ei.e8  := op.vrgather_v && info.sew.e8
  gatherIndexGen.in.bits.lmul := info.lmul
  gatherIndexGen.in.bits.vs1 := vs1
  gatherIndexGen.in.bits.mask := maskIsTrue

  slideUpIndexGen.in.valid := in.valid && (op.slideup || op.slide1up)
  slideUpIndexGen.in.bits.isSlide1Up := op.slide1up
  slideUpIndexGen.in.bits.tableIdx := uopIdx
  slideUpIndexGen.in.bits.sew := info.sew
  slideUpIndexGen.in.bits.offsetOverflow := rs1.drop(IndexWidth) =/= 0.U
  slideUpIndexGen.in.bits.offset := rs1.take(IndexWidth)
  slideUpIndexGen.in.bits.vl := in.bits.vl

  slideDownIndexGen.in.valid := in.valid && (op.slidedown || op.slide1down)
  slideDownIndexGen.in.bits.isSlide1Down := op.slide1down
  slideDownIndexGen.in.bits.tableIdx := uopIdx
  slideDownIndexGen.in.bits.lmul := info.lmul
  slideDownIndexGen.in.bits.sew := info.sew
  slideDownIndexGen.in.bits.offsetOverflow := Mux(op.slide1down, false.B, rs1.drop(IndexWidth) =/= 0.U)
  slideDownIndexGen.in.bits.offset := Mux(op.slide1down, 1.U, rs1.take(IndexWidth))
  slideDownIndexGen.in.bits.vl := in.bits.vl

  out.valid := Cat(gatherUnits.map(_.out.valid)).orR
  out.bits.dest := Mux1H(gatherUnits.map(x => x.out.valid -> x.out.bits.dest))
}

class GatherArrayOpBundle extends Bundle {
  val vrgather_v = Bool()
  val vrgatherei16 = Bool()
  val slideup = Bool()
  val slidedown = Bool()
  val slide1up = Bool()
  val slide1down = Bool()
  val compress = Bool()
}

object GatherArrayMain extends App {
  println("Generating the GatherArray hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherArray(vlen = 128, dlen = 128, xlen = 64)) +: firtoolAnno
  )

  println("done")
}
