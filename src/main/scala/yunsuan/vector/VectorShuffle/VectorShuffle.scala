package yunsuan.vector.VectorShuffle

import chisel3._
import chisel3.util._
import yunsuan.util.LiteralCat
import yunsuan.util.Reflect.{getUIntMaxWidthOfObject, getUIntWidthOfObject}
import yunsuan.vector.Common.VectorConfig

import scala.language.{existentials, postfixOps}

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
  val i = IO(Input(ValidIO(new Bundle {
    val uopIdx = UInt(3.W)
    val src1 = UInt(vlen.W)
    val src2 = UInt(vlen.W)
    val srcMask = UInt(vlen.W)
    val vl = UInt(VlWidth.W)
    val eew = UInt(2.W)
    val optype = UInt(VectorShuffleEncode.width.W)
  })))

  val o = IO(Output(ValidIO(new Bundle {
    val dest = UInt(vlen.W)
  })))

  val isVCompress = VectorShuffleEncode.Type.is(_.VCOMPRESS)(i.bits.optype) && i.valid
  val isVRgather  = VectorShuffleEncode.Type.is(_.VRGATHER) (i.bits.optype) && i.valid
  val isVSlide    = VectorShuffleEncode.Type.is(_.VSLIDE)   (i.bits.optype) && i.valid
  val isVSlide1   = VectorShuffleEncode.Type.is(_.VSLIDE1)  (i.bits.optype) && i.valid

  val i_e8vl       = (i.bits.vl   << i.bits.eew).asUInt(VlWidth - 1, 0)
  val i_e8offset   = (i.bits.src1(VlWidth - 1, 0) << i.bits.eew).asUInt(VlWidth - 1, 0)
  val i_vregOffset = (i_e8offset >> ElemIdxWidth).asUInt
  val i_vs         = Wire(Vec(vlen / 8, UInt(8.W)))
  val i_inBypass   = i_vregOffset =/= 0.U
  val i_uopIdx     = i.bits.uopIdx

  val vectorSlideUp = Module(new VectorSlideUp(vlen))

  i_vs := i.bits.src2.asTypeOf(i_vs)

  val reg_e8vl        = RegEnable(i_e8vl,       isVSlide && i.bits.uopIdx === 0.U)
  val reg_e8offset    = RegEnable(i_e8offset,   isVSlide && i.bits.uopIdx === 0.U)
  val reg_vregOffset  = RegEnable(i_vregOffset, isVSlide && i.bits.uopIdx === 0.U)
  val reg_inBypass    = RegEnable(i_inBypass,   isVSlide && i.bits.uopIdx === 0.U)

  val e8vl       = Mux(isVSlide && i.bits.uopIdx === 0.U, i_e8vl,       reg_e8vl)
  val e8offset   = Mux(isVSlide && i.bits.uopIdx === 0.U, i_e8offset,   reg_e8offset)
  val vregOffset = Mux(isVSlide && i.bits.uopIdx === 0.U, i_vregOffset, reg_vregOffset)
  val inBypass   = Mux(isVSlide && i.bits.uopIdx === 0.U, i_inBypass,   reg_inBypass)

  val dataVec    = Reg(Vec(8, Vec(vlen / 8, UInt(8.W))))

  (dataVec.flatten lazyZip vectorSlideUp.io.wdataVec.flatten).foreach {
    case (data, wdata) => {
      when (wdata.valid) {
        data := wdata.bits
      }
    }
  }

  vectorSlideUp.io.in := i
  vectorSlideUp.io.rdataVec := dataVec.asTypeOf(vectorSlideUp.io.rdataVec)

  dontTouch(isVCompress)
  dontTouch(isVRgather)
  dontTouch(isVSlide)
  dontTouch(isVSlide1)

  o.valid := i.valid
  o.bits.dest := Mux1H(Seq(
    vectorSlideUp.io.out.valid -> vectorSlideUp.io.out.bits.dest
  ))
}

object VectorShuffleGen extends App {
  println("Generating the VectorShuffle hardware")
  emitVerilog(new VectorShuffle(128), Array("--target-dir", "build/vector"))
  println("done")
}
