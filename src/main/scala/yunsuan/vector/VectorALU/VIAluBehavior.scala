package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util.Cat
import yunsuan.encoding.Opcode.VIAluOpcode

/**
  * Stateless lane-core carrier for VIAluFix.  The original VIAluFixPoint
  * module contains the S1 register; this carrier wires the same adder/misc
  * results directly into S1 so the wrapper owns the architectural pipeline
  * register and grouped lowering remains a pure compute operation.
  */
class VIAluFixComputeCarrier(val modelName: String = "vec-int-alu-lane") extends RawModule {
  override def desiredName: String =
    "WolvrixBehaviorComputeGroup_v1_" + modelName.replace('-', '_')

  private val inputWidth = new VIAluFixPointInput(64).getWidth
  val io = IO(new Bundle {
    val in = Input(UInt(inputWidth.W))
    val out = Output(UInt(112.W)) // vd, narrowVd, addCarryCmpMask, vxsat
  })

  private val req = io.in.asTypeOf(new VIAluFixPointInput(64))
  private val (adder, misc, s1) = withClockAndReset(false.B.asClock, false.B) {
    (Module(new VIAluAdder(64)), Module(new VIAluMisc(64)), Module(new VIAluFixPointS1(64)))
  }

  adder.io.in.opcode := req.opcode
  adder.io.in.ctrl.sel8 := req.info.sel8
  adder.io.in.ctrl.sel16 := req.info.sel16
  adder.io.in.ctrl.sel32 := req.info.sel32
  adder.io.in.ctrl.sel64 := req.info.sel64
  adder.io.in.ctrl.widenVs2 := req.ctrl.adderCtrl.widenVs2
  adder.io.in.ctrl.widen := req.ctrl.widen
  adder.io.in.ctrl.isSigned := req.ctrl.isSigned
  adder.io.in.ctrl.isAddCarry := req.ctrl.adderCtrl.isAddCarry
  adder.io.in.data.vs2 := req.data.vs2
  adder.io.in.data.vs1 := req.data.vs1
  adder.io.in.data.vs2Widen := req.data.vs2Widen
  adder.io.in.data.vs1Widen := req.data.vs1Widen
  adder.io.in.data.vm := req.info.vm
  adder.io.in.data.vxrm := req.data.vxrm
  adder.io.in.data.mask := req.data.mask

  misc.io.in.opcode := req.opcode
  misc.io.in.ctrl.sel8 := req.info.sel8
  misc.io.in.ctrl.sel16 := req.info.sel16
  misc.io.in.ctrl.sel32 := req.info.sel32
  misc.io.in.ctrl.sel64 := req.info.sel64
  misc.io.in.ctrl.isExt := req.ctrl.miscCtrl.isExt
  misc.io.in.ctrl.widenVs2 := req.ctrl.adderCtrl.widenVs2
  misc.io.in.ctrl.widen := req.ctrl.widen
  misc.io.in.ctrl.isNarrow := req.ctrl.miscCtrl.isNarrow
  misc.io.in.ctrl.isSigned := req.ctrl.isSigned
  misc.io.in.data.vs2 := req.data.vs2
  misc.io.in.data.vs1 := req.data.vs1
  misc.io.in.data.vs2Widen := req.data.vs2Widen
  misc.io.in.data.vs1Widen := req.data.vs1Widen
  misc.io.in.data.vxrm := req.data.vxrm

  s1.io.in.adderToS1 := adder.io.out.toS1
  s1.io.in.miscToS1 := misc.io.out.toS1
  s1.io.in.sel8 := req.info.sel8
  s1.io.in.sel16 := req.info.sel16
  s1.io.in.sel32 := req.info.sel32
  s1.io.in.sel64 := req.info.sel64
  s1.io.in.vs2 := req.data.vs2
  s1.io.in.vs1 := req.data.vs1
  s1.io.in.isSigned := req.ctrl.isSigned
  s1.io.in.isMisc := req.ctrl.miscCtrl.isMisc
  io.out := Cat(s1.io.out.vxsat, s1.io.out.addCarryCmpMask, s1.io.out.narrowVd, s1.io.out.vd)
}
