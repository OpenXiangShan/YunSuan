package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._
import yunsuan.vector.Common._
import yunsuan.encoding.Opcode.Opcodes.VIMacOpcode
import yunsuan.vector.VectorALU.VIAlu.MaskGroup

class VIMac64b extends VIMac64b.Module {
  val in  = IO(Input(new VIMac64b.In))
  val out = IO(Output(new VIMac64b.Out))

  private val ex0 = in.ex0

  val stage1 = Module(new VIMac64bStage1)
  stage1.in.info.vxrm     := in.info.vxrm
  stage1.in.info.uopIdx   := in.info.uopIdx
  stage1.in.ctrl.sewIs8   := in.ex0.bits.ctrl.sewIs8
  stage1.in.ctrl.sewIs16  := in.ex0.bits.ctrl.sewIs16
  stage1.in.ctrl.sewIs32  := in.ex0.bits.ctrl.sewIs32
  stage1.in.ctrl.sewIs64  := in.ex0.bits.ctrl.sewIs64
  stage1.in.ctrl.vs1Sign  := in.ex0.bits.ctrl.vs1Sign
  stage1.in.ctrl.vs2Sign  := in.ex0.bits.ctrl.vs2Sign
  stage1.in.ctrl.vdSign   := in.ex0.bits.ctrl.vdSign
  stage1.in.ctrl.isSub    := in.ex0.bits.ctrl.isSub
  stage1.in.ctrl.highHalf := in.ex0.bits.ctrl.highHalf
  stage1.in.ctrl.isMacc   := in.ex0.bits.ctrl.isMacc
  stage1.in.ctrl.widen    := in.ex0.bits.ctrl.widen
  stage1.in.ctrl.isFixP   := in.ex0.bits.ctrl.isFixP
  stage1.in.data.vs1      := in.ex0.bits.data.vs1
  stage1.in.data.vs2      := in.ex0.bits.data.vs2
  stage1.in.data.oldVd    := in.ex0.bits.data.oldVd
  val stage1Out = RegEnable(stage1.out, ex0.valid)

  val stage2 = Module(new VIMac64bStage2)
  stage2.in.info.vxrm                   := stage1Out.info.vxrm
  stage2.in.info.uopIdx                 := stage1Out.info.uopIdx
  stage2.in.ctrl.sewIs8S1               := stage1Out.ctrl.sewIs8S1
  stage2.in.ctrl.sewIs16S1              := stage1Out.ctrl.sewIs16S1
  stage2.in.ctrl.sewIs32S1              := stage1Out.ctrl.sewIs32S1
  stage2.in.ctrl.sewIs64S1              := stage1Out.ctrl.sewIs64S1
  stage2.in.ctrl.highHalfS1             := stage1Out.ctrl.highHalfS1
  stage2.in.ctrl.widenS1                := stage1Out.ctrl.widenS1
  stage2.in.ctrl.isFixPS1               := stage1Out.ctrl.isFixPS1
  stage2.in.data.compStage1ResultsS1    := stage1Out.data.compStage1ResultsS1
  stage2.in.data.wallaceLine34NonFixPS1 := stage1Out.data.wallaceLine34NonFixPS1
  stage2.in.data.wallaceLine34FixPS1    := stage1Out.data.wallaceLine34FixPS1
  val stage2Out = RegEnable(stage2.out, in.ex1Valid)

  val stage3 = Module(new VIMac64bStage3)
  stage3.in.info.vxrm              := stage2Out.info.vxrm
  stage3.in.info.uopIdx            := stage2Out.info.uopIdx
  stage3.in.ctrl.sewIs8S2          := stage2Out.ctrl.sewIs8S2
  stage3.in.ctrl.sewIs16S2         := stage2Out.ctrl.sewIs16S2
  stage3.in.ctrl.sewIs32S2         := stage2Out.ctrl.sewIs32S2
  stage3.in.ctrl.sewIs64S2         := stage2Out.ctrl.sewIs64S2
  stage3.in.ctrl.highHalfS2        := stage2Out.ctrl.highHalfS2
  stage3.in.ctrl.widenS2           := stage2Out.ctrl.widenS2
  stage3.in.ctrl.isFixPS2          := stage2Out.ctrl.isFixPS2
  stage3.in.data.sumFinalNonFixPS2 := stage2Out.data.sumFinalNonFixPS2
  stage3.in.data.sumFinalFixPS2    := stage2Out.data.sumFinalFixPS2

  out.ex0.vd          := 0.U
  out.ex0.narrowVd    := 0.U
  out.ex0.mask        := 0.U.asTypeOf(out.ex0.mask)
  out.ex0.vxsat       := 0.U
  out.ex0.narrowVxsat := 0.U

  out.ex1.vd          := 0.U
  out.ex1.narrowVd    := 0.U
  out.ex1.mask        := 0.U.asTypeOf(out.ex1.mask)
  out.ex1.vxsat       := 0.U
  out.ex1.narrowVxsat := 0.U

  out.ex2.vd          := stage3.out.vd
  out.ex2.narrowVd    := 0.U
  out.ex2.mask        := 0.U.asTypeOf(out.ex2.mask)
  out.ex2.vxsat       := stage3.out.vxsat
  out.ex2.narrowVxsat := 0.U
}

object VIMac64b {
  class In extends Bundle {
    val info = new Info
    val ex0 = ValidIO(new InEx0)
    val ex1Valid = Bool()
  }

  class Out extends Bundle {
    val ex0 = new OutData
    val ex1 = new OutData
    val ex2 = new OutData
  }

  class Info extends Bundle {
    val uopIdx = UInt(6.W)
    val vxrm   = Vxrm()
  }

  class InEx0 extends Bundle {
    val ctrl = new VIMac64bStage1.InCtrl
    val data = new InDataEx0
  }

  class InDataEx0 extends Bundle {
    val vs2 = UInt(dWidth.W)
    val vs1 = UInt(dWidth.W)
    val oldVd = UInt(dWidth.W)
  }

  class OutData extends Bundle {
    val vd = UInt(dWidth.W)
    val narrowVd = UInt((dWidth / 2).W)
    val mask = new MaskGroup
    val vxsat = MaskData()
    val narrowVxsat = NarrowMask()
  }

  trait Config {
    val dWidth = 64
    val maskWidth = dWidth / 8
  }

  trait Util {
    self: Config =>
      def MaskData(): UInt = UInt(maskWidth.W)
      def NarrowMask(): UInt = UInt((maskWidth / 2).W)
  }

  class Bundle extends chisel3.Bundle with Config with Util

  class Module extends chisel3.Module with Config with Util
}
