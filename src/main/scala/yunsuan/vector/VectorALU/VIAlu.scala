package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.VIAluOpcode
import yunsuan.vector.Common.Vxrm

class VIAlu extends VIAlu.Module {
  val in = IO(Input(new VIAlu.In))
  val out = IO(Output(new VIAlu.Out))

  private val ex0 = in.ex0
  private val ex1 = in.ex1

  val adder = Module(new Adder)
  val misc = Module(new Misc)
  val fixPoint = Module(new FixPoint)

  adder.in.vxrm := in.vxrm
  adder.in.ctrl := ex0.bits.ctrl.adder
  adder.in.data.vs1 := ex0.bits.data.vs1
  adder.in.data.vs2 := ex0.bits.data.vs2
  adder.in.data.vs1Widen := ex0.bits.data.vs1Widen
  adder.in.data.vs2Widen := ex0.bits.data.vs2Widen
  adder.in.data.mask := ex0.bits.data.mask

  misc.in.vxrm := in.vxrm
  misc.in.ctrl := ex0.bits.ctrl.misc
  misc.in.data.vs1 := ex0.bits.data.vs1
  misc.in.data.vs2 := ex0.bits.data.vs2
  misc.in.data.vs1Widen := ex0.bits.data.vs1Widen
  misc.in.data.vs2Widen := ex0.bits.data.vs2Widen

  fixPoint.in.vxrm := in.vxrm
  fixPoint.in.ctrl := ex1.bits.ctrl.fixPoint
  fixPoint.in.data.vs1 := ex1.bits.data.vs1
  fixPoint.in.data.vs2 := ex1.bits.data.vs2
  fixPoint.in.adderData := RegEnable(adder.out.mid, ex0.valid)
  fixPoint.in.miscData := RegEnable(misc.out.mid, ex0.valid)

  val ex0Vd: UInt = {
    import VIAluOpcode._
    implicit val opcode: UInt = ex0.bits.ctrl.opcode

    Mux1H(Seq(
      Res.isAnd        -> misc.out.res.and,
      Res.isOr         -> misc.out.res.or,
      Res.isXor        -> misc.out.res.xor,
      Res.isNand       -> misc.out.res.vnand,
      Res.isAndn       -> misc.out.res.vandn,
      Res.isNor        -> misc.out.res.vnor,
      Res.isOrn        -> misc.out.res.vorn,
      Res.isXnor       -> misc.out.res.vxnor,
      Res.isExt        -> misc.out.res.ext,
      Res.isScal       -> misc.out.res.scal,
      Res.isVroShift   -> misc.out.res.vroShift,
      Res.isVwsll      -> misc.out.res.vwsll,
      Res.isLeftShift  -> misc.out.res.leftShift,
      Res.isRightShift -> misc.out.res.rightShift,
      Res.isLeadZero   -> misc.out.res.leadZero,
      Res.isBrev       -> misc.out.res.brev,
      Res.isBrev8      -> misc.out.res.brev8,
      Res.isRev8       -> misc.out.res.rev8,
      Res.isAvg        -> adder.out.res.avg,
      Res.isAdder      -> adder.out.res.adder,
    ))
  }

  out.ex0.vd := ex0Vd
  out.ex0.narrowVd := misc.out.res.narrow
  out.ex0.mask := adder.out.res.adcCmpMask
  out.ex0.vxsat := 0.U
  out.ex0.narrowVxsat := 0.U

  out.ex1.vd := fixPoint.out.res.vd
  out.ex1.narrowVd := fixPoint.out.res.narrowVd
  out.ex1.mask := 0.U.asTypeOf(out.ex1.mask)
  out.ex1.vxsat := fixPoint.out.res.vxsat
  out.ex1.narrowVxsat := fixPoint.out.res.narrowVxsat
}

object VIAlu {
  class In extends Bundle {
    val vxrm = Vxrm()
    val ex0 = ValidIO(new InEx0)
    val ex1 = ValidIO(new InEx1)
  }

  class Out extends Bundle {
    val ex0 = new OutData
    val ex1 = new OutData
  }

  class InEx0 extends Bundle {
    val ctrl = new InCtrlEx0
    val data = new InDataEx0
  }

  class InEx1 extends Bundle {
    val ctrl = new InCtrlEx1
    val data = new InDataEx1
  }

  class InCtrlEx0 extends Bundle {
    val opcode = VIAluOpcode()
    val adder = new Adder.InCtrl
    val misc = new Misc.InCtrl
  }

  class InCtrlEx1 extends Bundle {
    val opcode = VIAluOpcode()
    val fixPoint = new FixPoint.InCtrl
  }

  class InDataEx0 extends Bundle {
    val vs2 = UInt(dWidth.W)
    val vs1 = UInt(dWidth.W)
    val vs2Widen = UInt(dWidth.W)
    val vs1Widen = UInt(dWidth.W)
    val mask = UInt((dWidth / 8).W)
  }

  class InDataEx1 extends Bundle {
    val vs2 = UInt(dWidth.W)
    val vs1 = UInt(dWidth.W)
  }

  class OutData extends Bundle {
    val vd = UInt(dWidth.W)
    val narrowVd = UInt((dWidth / 2).W)
    val mask = new MaskGroup
    val vxsat = MaskData()
    val narrowVxsat = NarrowMask()
  }

  class MaskGroup extends Bundle {
    val e8  = UInt(maskWidth.W)
    val e16 = UInt((maskWidth / 2).W)
    val e32 = UInt((maskWidth / 4).W)
    val e64 = UInt((maskWidth / 8).W)

    def toE8Mask(sew1H: UInt) = {
      require(sew1H.getWidth == 4)
      Mux1H(Seq(
        sew1H(0) -> FillInterleaved(1, e8),
        sew1H(1) -> FillInterleaved(2, e16),
        sew1H(2) -> FillInterleaved(4, e32),
        sew1H(3) -> FillInterleaved(8, e64),
      ))
    }
  }

  class NarrowMaskGroup extends Bundle {
    val e8  = Vec(narrowMaskWidth, Bool())
    val e16 = Vec(narrowMaskWidth / 2, Bool())
    val e32 = Vec(narrowMaskWidth / 4, Bool())

    def toE8Mask(sew1H: UInt) = {
      require(3 <= sew1H.getWidth && sew1H.getWidth <= 4)
      Mux1H(Seq(
        sew1H(0) -> FillInterleaved(1, e8),
        sew1H(1) -> FillInterleaved(2, e16),
        sew1H(2) -> FillInterleaved(4, e32),
      ))
    }
  }

  trait Config {
    val dWidth = 64
    val maskWidth = dWidth / 8
    val narrowMaskWidth = dWidth / 2 / 8
  }

  trait Util {
    self: Config =>
    def WidthData(): UInt = UInt(dWidth.W)
    def MaskData(): UInt = UInt(maskWidth.W)
    def NarrowData(): UInt = UInt((dWidth / 2).W)
    def NarrowMask(): UInt = UInt((maskWidth / 2).W)
  }

  class Bundle extends chisel3.Bundle with Config with Util

  class Module extends chisel3.Module with Config with Util
}
