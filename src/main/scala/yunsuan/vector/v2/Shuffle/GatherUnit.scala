package yunsuan.vector.v2.Shuffle

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

class GatherUnit(
  val vlen: Int,
  val dlen: Int,
  maxTableNum: Int,
  xlen: Int = 64,
) extends Module with GatherConfig {
  require(isPow2(dlen) && dlen >= 64)
  require(isPow2(vlen) && vlen >= 64)
  require(isPow2(maxTableNum))

  override def desiredName: String = super.desiredName + s"_vlen${vlen}b_dlen${dlen}b_table${maxTableNum}"

  val vlWidth = log2Ceil(vlen + 1)
  val ResWidth = MinDataWidth
  val SplittedParts = 8 * vlen / dlen

  val flush = IO(Input(Bool()))

  val in = IO(Input(new Bundle {
    val ctrl = Flipped(ValidIO(new Bundle {
      val isEntryUnit = Bool()

      val info = new GatherInfoBundle(vlen, dlen)


      // used by vrgatherei16 e8
      // only lower half table will be used
      val half = Bool()
    }))

    val table = Input(ValidIO(new Bundle {
      val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
      val idx = UInt(TableIdxWidth.W)
    }))

    val oldTable = Input(new Bundle {
      val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
    })

    val scalaTable = Input(UInt(xlen.W))

    val compress    = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val gather      = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val gatherei16  = Input(ValidIO(new Bundle{}))
    val slideUp     = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slideDown   = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slide1Up    = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))
    val slide1Down  = Input(ValidIO(new GatherIndexBundle(NumElem, IndexWidth)))

    val elemActive    = Input(Vec(NumElem, Bool()))
    val elemInactive  = Input(Vec(NumElem, Bool()))
    val elemTail      = Input(Vec(NumElem, Bool()))
  }))

  // out.valid means this uop can writeback
  // out.ready means this uop's writeback is handled
  // valid will not rely on ready
  val out = IO(DecoupledIO(new Bundle {
    val dest = UInt(dlen.W)
  }))

  val toNextUnit = IO(Output(ValidIO(new Bundle {
    val dataVec = Vec(NumElem, UInt(MinDataWidth.W))
    val idx = UInt(TableIdxWidth.W)
  })))

  private val ma = in.ctrl.bits.info.ma
  private val ta = in.ctrl.bits.info.ta
  private val info = in.ctrl.bits.info
  private val inTableIdx = in.table.bits.idx
  private val inTable: Vec[UInt] = in.table.bits.dataVec
  private val inScalaTable: Vec[UInt] = in.scalaTable.splitToVecByWidth(MinDataWidth)

  private val isGatherOp = in.gather.valid
  private val isCompressOp = in.compress.valid
  private val isSlideUpOp = in.slideUp.valid
  private val isSlideDownOp = in.slideDown.valid
  private val isSlide1UpOp = in.slide1Up.valid
  private val isSlide1DownOp = in.slide1Down.valid

  private val (elemActive, elemInactive, elemTail) = (in.elemActive, in.elemInactive, in.elemTail)

  private val table = Reg(Vec(NumElem, UInt(MinDataWidth.W)))
  private val indexOrRes = Reg(Vec(NumElem, UInt(IndexWidth.max(ResWidth).W)))
  private val indexView = WireInit(VecInit(indexOrRes.map(_.take(IndexWidth))))
  private val resView = WireInit(VecInit(indexOrRes.map(_.take(ResWidth))))

  private val active = RegInit(false.B)
  private val counter = RegInit(0.U(log2Up(maxTableNum).W))
  private val indexDone = Reg(Vec(NumElem, Bool()))
  private val tableIdx = Reg(UInt(TableIdxWidth.W))
  private val multiUop = RegInit(false.B)

  private val gatherMod = Module(new GatherBase(vlen = vlen, dlen = dlen, maxTableNum))

  private val acceptFirstUop = !active && in.ctrl.valid
  private val acceptNonFirstUop = active && in.ctrl.valid

  when (flush) {
    active := false.B
  }.elsewhen (acceptFirstUop) {
    active := true.B
    counter := Mux1H(Seq(
      info.lmul.m8  -> (1.max(maxTableNum /  1) - 1).U,
      info.lmul.m4  -> (1.max(maxTableNum /  2) - 1).U,
      info.lmul.m2  -> (1.max(maxTableNum /  4) - 1).U,
      info.lmul.m1  -> (1.max(maxTableNum /  8) - 1).U,
      info.lmul.mf2 -> (1.max(maxTableNum / 16) - 1).U,
      info.lmul.mf4 -> (1.max(maxTableNum / 32) - 1).U,
      info.lmul.mf8 -> (1.max(maxTableNum / 64) - 1).U,
    ))
  }.elsewhen (acceptNonFirstUop) {
    counter := counter - 1.U
  }.elsewhen (active && counter === 0.U && out.ready) {
    active := false.B
  }.otherwise {
    // keep
  }

  private val gatherModIndexValid = Wire(Vec(NumElem, Bool()))
  for (i <- gatherModIndexValid.indices) {
    gatherModIndexValid(i) :=
      Mux(
        acceptFirstUop,
        Mux1H(Seq(
          in.gather.valid    -> true.B,
          in.slideUp.valid   -> true.B,
          in.slideDown.valid -> true.B,
          in.compress.valid  -> true.B,
        )),
        !indexDone(i),
      )
  }

  private val gatherModIndex: GatherIndexBundle = Mux1HValidIO(Seq(
    in.gather,
    in.slideUp,
    in.slideDown,
    in.compress,
    in.slide1Up,
    in.slide1Down,
  )).bits

  gatherMod.in.tableIdx := inTableIdx
  gatherMod.in.table := inTable

  for (i <- gatherMod.in.indexValid.indices) {
    gatherMod.in.indexValid(i) := Mux(
      acceptFirstUop,
      gatherModIndex.indexValid(i),
      !indexDone(i),
    )
    gatherMod.in.index(i) := Mux(
      acceptFirstUop,
      gatherModIndex.index(i),
      indexView(i),
    )
  }

  gatherMod.in.lmul := info.lmul

  when (in.ctrl.valid) {
    multiUop := ((vlen / dlen) match {
      case 1 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4 || lmul.mf2 || lmul.m1) }
      case 2 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4 || lmul.mf2) }
      case 4 => info.lmul match { case lmul => !(lmul.mf8 || lmul.mf4) }
      case 8 => info.lmul match { case lmul => !(lmul.mf8) }
      case _ => false.B
    })
  }

  when (in.table.valid) {
    tableIdx := in.table.bits.idx
    table    := in.table.bits.dataVec
  }

  private val fillScalaFromIndexGen = Mux1HValidIO(Seq(
    in.slide1Up,
    in.slide1Down,
  )).bits.fillScala
  private val fillZerosFromIndexGen = Mux1HValidIO(Seq(
    in.gather,
    in.slideDown,
  )).bits.fillZeros

  private val fill1s  = Wire(Vec(NumElem, Bool()))
  private val fillVs3 = Wire(Vec(NumElem, Bool()))
  private val fillIdx = Wire(Vec(NumElem, Bool()))
  private val fillRes = Wire(Vec(NumElem, Bool()))
  private val fillScala = Wire(Vec(NumElem, Bool()))
  private val fillZeros = Wire(Vec(NumElem, Bool()))

  private val scalaTable = Wire(inScalaTable.cloneType)
  for (i <- scalaTable.indices) {
    scalaTable(i) := Mux1H(Seq(
      info.sew.e8  -> inScalaTable(i % (8  / MinDataWidth)),
      info.sew.e16 -> inScalaTable(i % (16 / MinDataWidth)),
      info.sew.e32 -> inScalaTable(i % (32 / MinDataWidth)),
      info.sew.e64 -> inScalaTable(i % (64 / MinDataWidth)),
    ))
  }

  for (i <- 0 until NumElem) {
    fill1s(i)    := acceptFirstUop && (elemInactive(i) &&  ma || elemTail(i) &&  ta)
    fillVs3(i)   := acceptFirstUop && (elemInactive(i) && !ma || elemTail(i) && !ta)
    fillIdx(i)   := acceptFirstUop && gatherMod.out.notfound(i)
    fillRes(i)   :=                   elemActive(i) && gatherMod.out.resultValid(i)
    fillScala(i) := acceptFirstUop && elemActive(i) && fillScalaFromIndexGen(i)
    fillZeros(i) := acceptFirstUop && elemActive(i) && fillZerosFromIndexGen(i)
    when (fillRes(i) || acceptFirstUop) {
      indexOrRes(i) := Mux1H(Seq(
        fill1s(i)    -> Fill(MinDataWidth, 1.U(1.W)),
        fillVs3(i)   -> in.oldTable.dataVec(i),
        fillIdx(i)   -> gatherModIndex.index(i),
        fillRes(i)   -> gatherMod.out.result(i),
        fillScala(i) -> scalaTable(i % (xlen / MinDataWidth)),
        fillZeros(i) -> 0.U
      ))
    }

    indexDone(i) := gatherMod.out.resultValid(i)
  }

  // don't rely on out.ready
  out.valid     := active && counter === 0.U
  out.bits.dest := resView.asUInt

  // Need to transport table for non-first uop and
  toNextUnit.valid := in.table.valid && acceptNonFirstUop && multiUop
  toNextUnit.bits.dataVec := table
  toNextUnit.bits.idx := tableIdx
}

/**
 * This module is used to produce bit vector that is compared to vl.
 * This module is full configurable that support different vlen, dlen and MinDataWidth
 * @param vlen
 * @param dlen
 * @param MinDataWidth the min width of data. if we support int4, MinDataWidth should be 4
 */
class VlCompareBitVecModule(
  val vlen    : Int,
  val dlen    : Int,
  MinDataWidth: Int,
  compFunc    : (UInt, UInt) => Bool,
  postFixName : String = "",
) extends Module with GatherConfig {
  override def desiredName: String = super.desiredName + postFixName

  private val SplittedParts = 8 * vlen / dlen

  val in = IO(Input(new Bundle {
    val vl = UInt(VlWidth.W)
    val sew = new GatherSewBundle
    val uopIdx = UInt(log2Ceil(SplittedParts).W)
  }))
  val out = IO(Output(new Bundle {
    val cmpTrue = Vec(NumElem, Bool())
  }))

  val e8Bias  = log2Ceil(8  / MinDataWidth)
  val e16Bias = log2Ceil(16 / MinDataWidth)
  val e32Bias = log2Ceil(32 / MinDataWidth)
  val e64Bias = log2Ceil(64 / MinDataWidth)

  val indices = out.cmpTrue.indices.map(i => Mux1H(Seq(
    in.sew.e8  -> (0.U( e8Bias.W) ## in.uopIdx ## (i >>  e8Bias).U((IndexWidthInDLEN -  e8Bias).W)),
    in.sew.e16 -> (0.U(e16Bias.W) ## in.uopIdx ## (i >> e16Bias).U((IndexWidthInDLEN - e16Bias).W)),
    in.sew.e32 -> (0.U(e32Bias.W) ## in.uopIdx ## (i >> e32Bias).U((IndexWidthInDLEN - e32Bias).W)),
    in.sew.e64 -> (0.U(e64Bias.W) ## in.uopIdx ## (i >> e64Bias).U((IndexWidthInDLEN - e64Bias).W)),
  )))

  for (i <- out.cmpTrue.indices) {
    out.cmpTrue(i) := compFunc(indices(i), in.vl)
  }

//  dontTouch(out)
//  dontTouch(in)
}

object VlCompareBitVecModule {
  def apply(
    vlen        : Int,
    dlen        : Int,
    MinDataWidth: Int,
  )(
    vl      : UInt,
    sew     : GatherSewBundle,
    uopIdx  : UInt,
    compFunc: (UInt, UInt) => Bool,
    postFixName: String = "",
  ): Vec[Bool] = {
    val mod = Module(new VlCompareBitVecModule(vlen, dlen, MinDataWidth, compFunc, postFixName))
    mod.in.vl := vl
    mod.in.sew := sew
    mod.in.uopIdx := uopIdx
    mod.out.cmpTrue
  }
}

class GatherMaskGenModule(
  vlen: Int,
  dlen: Int,
  MinDataWidth: Int = 8
) extends Module {
  val NumElem = dlen / MinDataWidth

  val in = IO(Input(new Bundle {
    val e64ei64 = Bool()
    val e32ei32 = Bool()
    val e16ei16 = Bool()
    val e8ei8   = Bool()
    val e64ei16 = Bool()
    val e32ei16 = Bool()
    val e8ei16  = Bool()
    val mask = UInt((dlen / MinDataWidth).W)
    val uopIdx = UInt(log2Ceil(vlen / dlen * 8).W)
  }))
  val out = IO(Output(new Bundle {
    val mask = UInt(NumElem.W)
  }))

  private val maskVf1 = in.mask
  private val maskVf2 = in.mask.toVf2Vec
  private val maskVf4 = in.mask.toVf4Vec
  private val usedMaskVf2 = (maskVf2)(in.uopIdx.take(1))
  private val usedMaskVf4 = (maskVf4)(in.uopIdx.take(2))

  private val maskVec = Wire(Vec(NumElem, Bool()))

  for (i <- maskVec.indices) {
    maskVec(i) := Mux1H(Seq(
      (in.e64ei64 || in.e32ei32 || in.e16ei16 || in.e8ei8) -> maskVf1(i),
      (in.e32ei16 || in.e8ei16) -> usedMaskVf2(i / 2),
      (in.e64ei16) -> usedMaskVf4(i / 4),
    ))
  }

  out.mask := maskVec.asUInt
}

class MaskSelectAndDupModule(val vlen: Int, val dlen: Int, useEI16: Boolean = false) extends Module with GatherConfig {
  override def desiredName: String = super.desiredName + (if (useEI16) "EI16" else "")

  val in = IO(Input(new Bundle {
    val mask = UInt(dlen.W)
    val uopIdx = UInt(log2Ceil(vlen / dlen * MaxLMUL).W)
    val sew = new GatherSewBundle
    val ei16 = Option.when(useEI16)(Bool())
  }))
  val out = IO(Output(new Bundle {
    val dupMask = UInt((dlen / MinDataWidth).W)
  }))

  private val sew = in.sew

  val e8Mask  = in.mask.splitToVecByWidth(vlen /  8).takeAsVec(8)(in.uopIdx.tail(log2Ceil(vlen / dlen)))
  val e16Mask = in.mask.splitToVecByWidth(vlen / 16).takeAsVec(8)(in.uopIdx.tail(log2Ceil(vlen / dlen)))
  val e32Mask = in.mask.splitToVecByWidth(vlen / 32).takeAsVec(8)(in.uopIdx.tail(log2Ceil(vlen / dlen)))
  val e64Mask = in.mask.splitToVecByWidth(vlen / 64).takeAsVec(8)(in.uopIdx.tail(log2Ceil(vlen / dlen)))

  val dupMaskVec = Wire(Vec(dlen / MinDataWidth, Bool()))
  for (i <- dupMaskVec.indices) {
    if (!useEI16) {
      dupMaskVec(i) := Mux1H(Seq(
        sew.e8  -> e8Mask (i / (8  / MinDataWidth)),
        sew.e16 -> e16Mask(i / (16 / MinDataWidth)),
        sew.e32 -> e32Mask(i / (32 / MinDataWidth)),
        sew.e64 -> e64Mask(i / (64 / MinDataWidth)),
      ))
    } else {
      val ei16 = in.ei16.get
      dupMaskVec(i) := Mux1H(Seq(
        (sew.e8  && !ei16) -> e8Mask (i / (8  / MinDataWidth)),
        (sew.e8  &&  ei16) -> e8Mask (i / (16 / MinDataWidth)),
        (sew.e16         ) -> e16Mask(i / (16 / MinDataWidth)),
        (sew.e32         ) -> e32Mask(i / (32 / MinDataWidth)),
        (sew.e64         ) -> e64Mask(i / (64 / MinDataWidth)),
      ))
    }
  }
  out.dupMask := dupMaskVec.asUInt
}

object MaskSelectAndDup {
  def apply(vlen: Int, dlen: Int)(mask: UInt, uopIdx: UInt, sew: GatherSewBundle): UInt = {
    val mod = Module(new MaskSelectAndDupModule(vlen, dlen))
    mod.in.mask := mask
    mod.in.uopIdx := uopIdx
    mod.in.sew := sew
    mod.out.dupMask
  }
}

object MaskSelectAndDupEI16 {
  def apply(vlen: Int, dlen: Int)(mask: UInt, uopIdx: UInt, sew: GatherSewBundle, ei16: Bool): UInt = {
    val mod = Module(new MaskSelectAndDupModule(vlen, dlen, useEI16 = true))
    mod.in.mask := mask
    mod.in.uopIdx := uopIdx
    mod.in.sew := sew
    mod.in.ei16.get := ei16
    mod.out.dupMask
  }
}

class MaskDupModule(val vlen: Int, val dlen: Int) extends Module with GatherConfig {
  val in = IO(Input(new Bundle {
    val mask = UInt((dlen / MinDataWidth).W)
    val uopIdx = UInt(log2Ceil(vlen / dlen * MaxLMUL).W)
    val sew = new GatherSewBundle
  }))
  val out = IO(Output(new Bundle {
    val dupMask = UInt((dlen / MinDataWidth).W)
  }))

  val e8MaskDup  = in.mask.splitToVecN( 8 / MinDataWidth)(in.uopIdx.take(0)).bitDup( 8 / MinDataWidth)
  val e16MaskDup = in.mask.splitToVecN(16 / MinDataWidth)(in.uopIdx.take(1)).bitDup(16 / MinDataWidth)
  val e32MaskDup = in.mask.splitToVecN(32 / MinDataWidth)(in.uopIdx.take(2)).bitDup(32 / MinDataWidth)
  val e64MaskDup = in.mask.splitToVecN(64 / MinDataWidth)(in.uopIdx.take(3)).bitDup(64 / MinDataWidth)

  out.dupMask := Mux1H(Seq(
    in.sew.e8  ->  e8MaskDup,
    in.sew.e16 -> e16MaskDup,
    in.sew.e32 -> e32MaskDup,
    in.sew.e64 -> e64MaskDup,
  ))
}

object MaskDup {
  def apply(vlen: Int, dlen: Int)(mask: UInt, uopIdx: UInt, sew: GatherSewBundle): UInt = {
    val maskDupMod = Module(new MaskDupModule(vlen, dlen))
    maskDupMod.in.mask := mask
    maskDupMod.in.uopIdx := uopIdx
    maskDupMod.in.sew := sew
    maskDupMod.out.dupMask
  }
}

class GatherInfoBundle(val vlen: Int, val dlen: Int) extends Bundle with GatherConfig {
  val lmul = new GatherLmulBundle
  val sew = new GatherSewBundle
  val uopIdx = UInt(log2Ceil(vlen / dlen * 8).W)
  // 1 -> ma, 0 -> mu
  val ma = Bool()
  // 1 -> ta, 0 -> tu
  val ta = Bool()
}

class GatherLmulBundle extends Bundle {
  val mf8 = Bool()
  val mf4 = Bool()
  val mf2 = Bool()
  val m1  = Bool()
  val m2  = Bool()
  val m4  = Bool()
  val m8  = Bool()
}

class GatherSewBundle extends Bundle {
  val e64 = Bool()
  val e32 = Bool()
  val e16 = Bool()
  val e8 = Bool()

  def toOH: UInt = Cat(e64, e32, e16, e8)
}

object MaskSelectAndDupModuleMain extends App {
  println("Generating the MaskSelectAndDupModule hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new MaskSelectAndDupModule(vlen = 128, dlen = 128, useEI16 = false)
    ) +: firtoolAnno
  )

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new MaskSelectAndDupModule(vlen = 128, dlen = 128, useEI16 = true)
    ) +: firtoolAnno
  )
  println("done")
}

object GatherMaskGenModuleMain extends App {
  println("Generating the GatherMaskGenModule hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new GatherMaskGenModule(vlen = 128, dlen = 128, MinDataWidth = 8)
    ) +: firtoolAnno
  )

  println("done")
}

object VlCompareBitVecModuleMain extends App {
  println("Generating the VlCompareBitVecModule hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new VlCompareBitVecModule(vlen = 128, dlen = 128, MinDataWidth = 8, (i, vl) => i < vl, "LessThanVl")
    ) +: firtoolAnno
  )

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(
      () => new VlCompareBitVecModule(vlen = 128, dlen = 128, MinDataWidth = 8, (i, vl) => i === (vl - 1.U), "EqualVlM1")
    ) +: firtoolAnno
  )

  println("done")
}

object GatherUnitMain extends App {
  println("Generating the GatherUnit hardware")

  val firtoolOpts = Array(
    "--target", "systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new GatherUnit(vlen = 128, dlen = 128, maxTableNum = 8)) +: firtoolAnno
  )

  println("done")
}