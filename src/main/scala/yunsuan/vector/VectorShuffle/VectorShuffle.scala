package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.LiteralCat
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common._
import yunsuan.util.Pipeline._

import scala.language.{existentials, postfixOps, implicitConversions}

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
    val in = Input(ValidIO(new Bundle {
      val uopIdx = UInt(3.W)
      val src1 = UInt(vlen.W)
      val src2 = UInt(vlen.W)
      val srcMask = UInt(vlen.W)
      val vl = UInt(VlWidth.W)
      val eew = UInt(2.W)
      val lmul = VLmul()
      val optype = UInt(VectorShuffleEncode.width.W)
    }))
    val out = Decoupled(new Bundle {
      val dest = UInt(vlen.W)
    })
  })

  val inFire  = io.in.fire
  val outFire = io.out.fire
  val i_uopIdx = io.in.bits.uopIdx
  val i_lmul   = io.in.bits.lmul

  val isVCompress = VectorShuffleEncode.Type.is(_.VCOMPRESS)(io.in.bits.optype) && inFire
  val isVRgather  = VectorShuffleEncode.Type.is(_.VRGATHER) (io.in.bits.optype) && inFire
  val isVSlide    = VectorShuffleEncode.Type.is(_.VSLIDE)   (io.in.bits.optype) && inFire
  val isVSlide1   = VectorShuffleEncode.Type.is(_.VSLIDE1)  (io.in.bits.optype) && inFire

  // hold [0, vlen]
  val i_e8vl       = (io.in.bits.vl << io.in.bits.eew).asUInt.take(VlWidth)

  val i_e8offsetOrigin = (io.in.bits.src1.take(VlWidth) << io.in.bits.eew).asUInt
  // hold [0, vlen * 2 - 1], i_e8offset.head means slide overflow
  val i_e8offset   = Cat(i_e8offsetOrigin.head(i_e8offsetOrigin.getWidth - VstartWidth).orR, i_e8offsetOrigin.take(VstartWidth))
  val i_vregOffset = (i_e8offset >> ElemIdxWidth).asUInt

  val busy = RegInit(false.B)
  val leftCount = Reg(UInt(4.W)) // hold 0 ~ 8, when leftCount = 0, busy should be false.

  val e8vl        = DataHoldBypass(i_e8vl,       inFire)
  val e8offset    = DataHoldBypass(i_e8offset,   inFire)
  val vregOffset  = (e8offset >> ElemIdxWidth).asUInt

  when (inFire) {
    busy := true.B
  }.elsewhen (leftCount === 0.U) {
    busy := false.B
  }

  when(inFire) {
    leftCount := i_lmul
  }.elsewhen(outFire) {
    leftCount := leftCount - 1.U
  }

  val vectorSlideUp = Module(new VectorSlideUp(vlen))

  val dataVec    = Reg(Vec(8, Vec(vlen / 8, UInt(8.W))))

  (dataVec.flatten lazyZip vectorSlideUp.io.wdataVec.flatten).foreach {
    case (data, wdata) => {
      when (wdata.valid) {
        data := wdata.bits
      }
    }
  }

  vectorSlideUp.io.in := io.in
  vectorSlideUp.io.rdataVec := dataVec.asTypeOf(vectorSlideUp.io.rdataVec)

  dontTouch(isVCompress)
  dontTouch(isVRgather)
  dontTouch(isVSlide)
  dontTouch(isVSlide1)

  io.out.valid := vectorSlideUp.io.out.valid
  io.out.bits.dest := Mux1H(Seq(
    vectorSlideUp.io.out.valid -> vectorSlideUp.io.out.bits.dest
  ))
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
      val mask = UInt(VLENB.W)
      val allMask = UInt(vlen.W)
      val vl = UInt(VlWidth.W)
      val eew = UInt(2.W)
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
    val out = Output(DecoupledIO(new Output))
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

  val wdataVec = Wire(chiselTypeOf(io.wdataVec))
}

object VectorShuffleGen extends App {
  println("Generating the VectorShuffle hardware")
  emitVerilog(new VectorShuffle(128), Array("--target-dir", "build/vector"))
  println("done")
}
