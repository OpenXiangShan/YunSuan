
package yunsuan.vectortest

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import yunsuan.vector._
import yunsuan.vectortest.alu.{VIAluWrapper}
import yunsuan.vectortest.perm._
import yunsuan.vectortest.mac.{VIMacWrapper, VIMac64bWrapper, VIMac64bInput, VIMac64bOutput}

case class SrcBundle(vs2: String = "h0",
                     vs1: String = "h0",
                     old_vd: String = "h0",
                     mask: String = "hffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff"
)

case class CtrlBundle(vdType: Int = 0,
                      srcTypeVs2: Int = 0,
                      srcTypeVs1: Int = 0,
                      opcode: Int = 0,
                      vm: Boolean = true,
                      ma: Boolean = false,
                      ta: Boolean = true,
                      vlmul: Int = 0,
                      vl: Int = 16,
                      vstart: Int = 0,
                      uopIdx: Int = 0,
                      vxrm : Int = 0,
)

// Temp
case class IMac64bCtrlBundle(vdType: Int = 7,
                          srcTypeVs2: Int = 7,
                          srcTypeVs1: Int = 7,
                          highHalf: Boolean = false,
                          isMacc: Boolean = false,
                          isSub: Boolean = false,
                          widen: Boolean = false,
                          isFixP: Boolean = false,
                          uopIdx: Int = 0,
)

trait BundleGenHelper {

  def genVIFuInfo(c: CtrlBundle) = {
    (new VIFuInfo).Lit(
      _.vm -> c.vm.B,
      _.ma -> c.ma.B,
      _.ta -> c.ta.B,
      _.vlmul -> c.vlmul.U,
      _.vl -> c.vl.U,
      _.vstart -> c.vstart.U,
      _.uopIdx -> c.uopIdx.U,
      _.vxrm -> c.vxrm.U,
    )
  }

  def genVAluInput(s: SrcBundle, c: CtrlBundle) = {
    (new VIFuInput).Lit(
      _.opcode -> (new VAluOpcode).Lit(_.op -> c.opcode.U),
      _.info -> genVIFuInfo(c),
      _.srcType -> Vec.Lit(c.srcTypeVs2.U(4.W), c.srcTypeVs1.U(4.W)),
      _.vdType -> c.vdType.U,
      _.vs1 -> s.vs1.U(128.W),
      _.vs2 -> s.vs2.U(128.W),
      _.old_vd -> s.old_vd.U(128.W),
      _.mask -> s.mask.U(128.W),
    )
  }

  def genVPermInput(s: SrcBundle, c: CtrlBundle) = {
    (new VPermInput).Lit(
      _.opcode -> (new VPermOpcode).Lit(_.op -> c.opcode.U),
      _.info -> genVIFuInfo(c),
      _.srcType -> Vec.Lit(c.srcTypeVs2.U(4.W), c.srcTypeVs1.U(4.W)),
      _.vdType -> c.vdType.U,
      _.vs1 -> s.vs1.U(128.W),
      _.vs2 -> s.vs2.U(128.W),
      _.old_vd -> s.old_vd.U(128.W),
      _.mask -> s.mask.U(128.W),
    )
  }

  def genVAluOutput(vd: String, vxsat: Boolean = false) = {
    (new VIFuOutput).Lit(
      _.vd -> vd.U(128.W),
      _.vxsat -> vxsat.B
    )
  }

  // Temp
  def genVIMac64bInput(s: SrcBundle, c: IMac64bCtrlBundle) = {
    (new VIMac64bInput).Lit(
      _.info -> (new VIFuInfo).Lit(
                 _.vm -> true.B,
                 _.ma -> true.B,
                 _.ta -> true.B,
                 _.vlmul -> 0.U,
                 _.vl -> 0.U,
                 _.vstart -> 0.U,
                 _.uopIdx -> c.uopIdx.U,
                 _.vxrm -> 0.U),
      _.srcType -> Vec.Lit(c.srcTypeVs2.U(4.W), c.srcTypeVs1.U(4.W)),
      _.vdType -> c.vdType.U,
      _.vs1 -> s.vs1.U(64.W),
      _.vs2 -> s.vs2.U(64.W),
      _.oldVd -> s.old_vd.U(64.W),
      _.highHalf -> c.highHalf.B,
      _.isMacc -> c.isMacc.B,
      _.isSub -> c.isSub.B,
      _.widen -> c.widen.B,
      _.isFixP -> c.isFixP.B,
    )
  }
  // Temp
  def genVIMac64bOutput(vd: String, vxsat: Boolean = false) = {
    (new VIMac64bOutput).Lit(
      _.vd -> vd.U(64.W),
      _.vxsat -> vxsat.B
    )
  }
}

object dataType {
  val u8  = 0
  val u16 = 1
  val u32 = 2
  val u64 = 3
  val s8  = 4
  val s16 = 5
  val s32 = 6
  val s64 = 7
  val f16 = 9
  val f32 = 10
  val f64 = 11
  val mask = 15
}

object TestHarnessAlu {
  def test_init(dut: VIAluWrapper): Unit = {
    dut.clock.setTimeout(2000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}
object TestHarnessIMac {
  def test_init(dut: VIMacWrapper): Unit = {
    dut.clock.setTimeout(20000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
  def test_init_64b(dut: VIMac64bWrapper): Unit = {
    dut.clock.setTimeout(20000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}

object TestHarnessPerm {
  def test_init(dut: VPermWrapper): Unit = {
    dut.clock.setTimeout(20000)
    dut.io.in.initSource()
    dut.io.in.setSourceClock(dut.clock)
    dut.io.out.initSink()
    dut.io.out.setSinkClock(dut.clock)
    dut.io.out.ready.poke(true.B)
  }
}


