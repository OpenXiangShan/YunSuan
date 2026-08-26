
package yunsuan.vectortest

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import yunsuan.vector._
import yunsuan.vectortest.perm._

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

object TestHarnessPerm {
  def test_init(dut: VPermWrapper): Unit = {
    implicit val clock = dut.clock
    dut.clock.setTimeout(20000)
    dut.io.in.initSource()
    dut.io.out.initSink()
    dut.io.out.ready.poke(true.B)
  }
}

object DecoupledDriver {
  import chisel3.util.DecoupledIO
  def drive[T <: Data](
    in:  DecoupledIO[T],
    out: DecoupledIO[T],
    inputs:  Seq[T],
    outputs: Seq[T],
    preSteps: Int = 0
  )(implicit clock: chisel3.Clock): Unit = {
    val inIt  = inputs.iterator
    val outIt = outputs.iterator
    def pokeIn(): Unit = {
      if (inIt.hasNext) {
        in.bits.poke(inIt.next())
        in.valid.poke(true.B)
      } else {
        in.valid.poke(false.B)
      }
    }
    pokeIn()
    clock.step(1)
    for (_ <- 0 until preSteps) { pokeIn(); clock.step(1) }
    while (outIt.hasNext) {
      if (out.valid.peek().litToBoolean) {
        out.bits.expect(outIt.next())
      }
      pokeIn()
      clock.step(1)
    }
    in.valid.poke(false.B)
  }
}
