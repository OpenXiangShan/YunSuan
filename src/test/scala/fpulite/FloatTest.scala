package yunsuan.fpulite
import chisel3._
import chisel3.stage._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.simulator.{VerilatorCFlags, VerilatorFlags}
import yunsuan.fpulite._
// import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
object GenTest extends App {
  val path = """./generated/fpulite/FloatDivider"""
  (new ChiselStage).execute(Array("--emission-options=disableMemRandomization,disableRegisterRandomization",
    "--target-dir", path), Seq(ChiselGeneratorAnnotation(() => new FloatDivider)))
}