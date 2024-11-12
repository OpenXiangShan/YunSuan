package yunsuan.vector.VectorShuffle

import circt.stage._
import chisel3._
import chisel3.util._
import chisel3.stage._
import yunsuan.util.{LiteralCat, RegNextWithEnable}
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._
import yunsuan.util.Pipeline._

import scala.collection.immutable.SeqMap
import scala.language.{existentials, implicitConversions, postfixOps}

object VectorShuffleEncode {
  object Type {
    var VCOMPRESS = "b0000".U(4.W)
    var VRGATHER  = "b11".U(2.W)
    var VSLIDE    = "b100".U(3.W)
    var VSLIDE1   = "b101".U(3.W)

    val typeCounter: collection.mutable.Map[UInt, Int] = collection.mutable.Map.from(Set(
      VCOMPRESS, VRGATHER, VSLIDE, VSLIDE1
    ).map(x => x -> 0))

    def is(enum: Type.type => UInt)(uint: UInt): Bool = {
      uint.head(enum(this).getWidth) === enum(this)
    }

    lazy val width: Int = getUIntMaxWidthOfObject(this)
  }

  val vcompress     : UInt = Encode(Type.VCOMPRESS)

  // vrgather.v[xi]
  val vrgather_xi   : UInt = Encode(Type.VRGATHER, "b00".U(2.W))

  val vrgather_v    : UInt = Encode(Type.VRGATHER, "b01".U(2.W))
  val vrgatherei16  : UInt = Encode(Type.VRGATHER, "b11".U(2.W))

  val vslideup_xi   : UInt = Encode(Type.VSLIDE,   "b0".U(1.W))
  val vslidedown_xi : UInt = Encode(Type.VSLIDE,   "b1".U(1.W))

  val vslide1up_xi  : UInt = Encode(Type.VSLIDE1,  "b0".U(1.W))
  val vslide1down_xi: UInt = Encode(Type.VSLIDE1,  "b1".U(1.W))

  protected def Encode(dataVec: UInt*): UInt = {
    val t = dataVec.head
    Type.typeCounter.update(t, Type.typeCounter(t) + 1)
    LiteralCat(dataVec: _*)
  }

  def is(enum: Type.type => UInt)(uint: UInt): Bool = Type.is(enum)(uint)

  lazy val width: Int = getUIntWidthOfObject(this)
}

class VectorShuffle(val vlen: Int) extends Module with VectorConfig {
  val io = IO(new Bundle {
    // flush all valid register
    val flush = Input(Bool())
    val in = Input(ValidIO(new Bundle {
      val uopIdx = UInt(3.W)
      val src1 = UInt(vlen.W)
      val src2 = UInt(vlen.W)
      val srcMask = UInt(vlen.W)
      val vl = UInt(VlWidth.W)
      val eew = UInt(2.W)
      val lmul = VLmul()
      val op = UInt(VectorShuffleEncode.width.W)
    }))
    val out = Decoupled(new Bundle {
      val dest = UInt(vlen.W)
      val vdIdx = VdIdx
    })
  })

  val flush = io.flush
  val inFire  = io.in.fire
  val outFire = io.out.fire
  val i_uopIdx = io.in.bits.uopIdx
  val i_lmul   = io.in.bits.lmul
  val i_op     = io.in.bits.op

  val isVCompress = VectorShuffleEncode.Type.is(_.VCOMPRESS)(io.in.bits.op) && inFire
  val isVRgather  = VectorShuffleEncode.Type.is(_.VRGATHER) (io.in.bits.op) && inFire
  val isVSlide    = VectorShuffleEncode.Type.is(_.VSLIDE)   (io.in.bits.op) && inFire
  val isVSlide1   = VectorShuffleEncode.Type.is(_.VSLIDE1)  (io.in.bits.op) && inFire

  // hold [0, vlen]
  val i_e8vl       = (io.in.bits.vl << io.in.bits.eew).asUInt.take(VlWidth)

  val i_e8offsetOrigin = (io.in.bits.src1.take(VlWidth) << io.in.bits.eew).asUInt
  // hold [0, vlen * 2 - 1], i_e8offset.head means slide overflow
  val i_e8offset   = Cat(i_e8offsetOrigin.head(i_e8offsetOrigin.getWidth - VstartWidth).orR, i_e8offsetOrigin.take(VstartWidth))
  val i_vregOffset = (i_e8offset >> ElemIdxWidth).asUInt

  val busy = RegInit(false.B)
  val busyNext = WireInit(busy)
  busy := busyNext

  val leftCount = Reg(UInt(4.W)) // hold 0 ~ 8, when leftCount = 0, busy should be false.

  val e8vl        = DataHoldBypass(i_e8vl,       inFire)
  val e8offset    = DataHoldBypass(i_e8offset,   inFire)

  when (flush) {
    busyNext := false.B
  }.elsewhen (inFire) {
    busyNext := true.B
  }.elsewhen (leftCount === 0.U) {
    busyNext := false.B
  }

  when(inFire) {
    leftCount := i_lmul
  }.elsewhen(outFire) {
    leftCount := leftCount - 1.U
  }

  val vectorSlideUp   = Module(new VectorSlideUp(vlen))
  val vectorSlideDown = Module(new VectorSlideDown(vlen))
  val vectorCompress  = Module(new VectorCompress(vlen))

  val modSeq = Seq(vectorSlideUp, vectorSlideDown, vectorCompress)

  val i_modSelect = Wire(new ModSelect(modSeq))
  val modSelect = RegEnable(i_modSelect, !busy && busyNext)
  i_modSelect(vectorSlideUp)   := isVSlide && i_op === VectorShuffleEncode.vslideup_xi
  i_modSelect(vectorSlideDown) := isVSlide && i_op === VectorShuffleEncode.vslidedown_xi
  i_modSelect(vectorCompress)  := isVCompress

  val dataVec = Reg(Vec(8, Vec(vlen / 8, UInt(8.W))))
  val maskVec = Reg(Vec(8, UInt(VLENB.W)))

  val wdataVec: Seq[ValidIO[UInt]] = modSeq
    .map(mod => mod.io.wdataVec.flatten)
    .transpose
    .map(x => Mux1HValidIO(x))

  (dataVec.flatten lazyZip wdataVec).foreach {
    case (data, wdata) => {
      when (wdata.valid) {
        data := wdata.bits
      }
    }
  }

  (maskVec lazyZip io.in.bits.srcMask.toVf8Vec).foreach {
    case (sink, source) =>
      when (!busy && busyNext) {
        sink := source
      }
  }

  val s0 = io.in
  val s1 = Pipe(s0.valid && !flush, s0.bits)
  val s2 = Pipe(s1.valid && !flush, s1.bits)
  val s3 = Pipe(s2.valid && !flush, s2.bits)

  for (mod <- modSeq) {
    mod.io.in.s0.valid := s0.valid && i_modSelect(mod)
    mod.io.in.s1.valid := s1.valid && modSelect(mod)
    mod.io.in.s2.valid := s2.valid && modSelect(mod)
    mod.io.in.s3.valid := s3.valid && modSelect(mod)
    mod.io.in.s0.bits.elements.foreach { case (str, data) => if (s0.bits.elements.contains(str)) data := s0.bits.elements(str)}
    mod.io.in.s1.bits.elements.foreach { case (str, data) => if (s1.bits.elements.contains(str)) data := s1.bits.elements(str)}
    mod.io.in.s2.bits.elements.foreach { case (str, data) => if (s2.bits.elements.contains(str)) data := s2.bits.elements(str)}
    mod.io.in.s3.bits.elements.foreach { case (str, data) => if (s3.bits.elements.contains(str)) data := s3.bits.elements(str)}
    mod.io.rdataVec := dataVec.asTypeOf(mod.io.rdataVec)
    mod.io.shuffleState.e8mask := Mux(!busy && busyNext, io.in.bits.srcMask, maskVec.asUInt)
    mod.io.shuffleState.e8vl := e8vl
    mod.io.shuffleState.e8offset := e8offset

    mod.io.out.ready := io.out.ready
  }

  dontTouch(isVCompress)
  dontTouch(isVRgather)
  dontTouch(isVSlide)
  dontTouch(isVSlide1)

  io.out.valid := vectorSlideUp.io.out.valid
  io.out.bits.dest := Mux1H(modSeq.map { mod =>
    mod.io.out.valid -> mod.io.out.bits.dest
  })
  io.out.bits.vdIdx := Mux1H(modSeq.map { mod =>
    mod.io.out.valid -> mod.io.out.bits.vdIdx
  })

  class ModSelect(seq: Seq[Module]) extends Record {
    override val elements: SeqMap[String, Data] = SeqMap.from(seq.map(mod => mod.desiredName -> Bool()))

    def apply(mod: Module): Bool = {
      this.elements(mod.desiredName).asInstanceOf[Bool]
    }
  }
}

abstract class VectorShuffleBaseModule(val vlen: Int) extends Module with VectorConfig {
  class Input extends Bundle {
    val s0 = ValidIO(new S0)
    val s1 = ValidIO(new S1)
    val s2 = ValidIO(new S2)
    val s3 = ValidIO(new S3)

    class S0 extends Bundle {
      val uopIdx = UInt(3.W)
      val src1 = UInt(vlen.W)
      val src2 = UInt(vlen.W)
      val srcMask = UInt(vlen.W)
      val eew = UInt(2.W)
      val vl = UInt(VlWidth.W)
    }

    class S1 extends Bundle {
      val uopIdx = UInt(3.W)
    }

    class S2 extends Bundle {
      val uopIdx = UInt(3.W)
    }

    class S3 extends Bundle {
      val uopIdx = UInt(3.W)
    }
  }

  class Output extends Bundle {
    val dest = UInt(vlen.W)
    val vdIdx = VdIdx
  }

  val io = IO(new Bundle {
    val in = Input(new Input)
    val out = DecoupledIO(new Output)
    val shuffleState = Input(new Bundle {
      val e8vl     = UInt(VlWidth.W)
      val e8offset = UInt(VlWidth.W) // hold [0, vlmax]
      val e8mask   = UInt(vlen.W)
    })

    val rdataVec = Input(Vec(8, UInt(vlen.W)))
    val wdataVec = Output(Vec(8, Vec(vlen / 8, ValidIO(UInt(8.W)))))
  })

  val s0 = io.in.s0
  val s1 = io.in.s1
  val s2 = io.in.s2
  val s3 = io.in.s3

  val e8vl = io.shuffleState.e8vl
  val e8offset = io.shuffleState.e8offset
  val e8offsetVreg   = e8offset.head(4)
  val e8offsetInVreg = e8offset.tail(4)
  val e8mask = io.shuffleState.e8mask

  val dreg8bMatrix: Vec[Vec[UInt]] = VecInit(io.rdataVec.map(_.to8bitVec))

  val wdataVec = io.wdataVec
}

object VectorShuffleGen extends App {
  println("Generating the VectorShuffle hardware")

  (new ChiselStage).execute(
    Array("--target", "systemverilog", "--target-dir", "build/vector") ++ args,
    Seq(
      ChiselGeneratorAnnotation(() => new VectorShuffle(128))
    )
  )

  println("done")
}
