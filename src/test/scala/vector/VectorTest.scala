package yunsuan.vector


import chisel3._
import chisel3.stage._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chiseltest.VerilatorBackendAnnotation
import chiseltest.simulator.{VerilatorFlags, VerilatorCFlags}
// import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl.stage.RunFirrtlTransformAnnotation
import firrtl.AnnotationSeq
import firrtl.options.TargetDirAnnotation
object GenTest extends App {
  val path = """./generated/VectorIdiv"""
  (new ChiselStage).execute(Array("--emission-options=disableMemRandomization,disableRegisterRandomization",
    "--target-dir", path), Seq(ChiselGeneratorAnnotation(() => new VectorIdiv)))
}

trait HasTestAnnos {
  var testAnnos: AnnotationSeq = Seq()
}

trait UseVerilatorBackend { this: HasTestAnnos =>
  testAnnos = testAnnos ++ Seq(VerilatorBackendAnnotation)

}

class YunSuanTester  extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {
  behavior of "YunSuan Test"
}

class VFloatAdderTest extends YunSuanTester {

  behavior of "YunSuan VectorFloatAdder"
  it should "pass the syntax" in {
    test(new VectorFloatAdder()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VFloatDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorFloatDivider"
  it should "pass the syntax" in {
    test(new VectorFloatDivider()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VFloatFMATest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorFloatFMA"
  it should "pass the syntax" in {
    test(new VectorFloatFMA()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VIntAdderTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorIntAdder"
  it should "pass the syntax" in {
    test(new VectorIntAdder()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlideUpLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlideUpLookup"
  it should "pass the syntax" in {
    test(new SlideUpLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlide1UpTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlide1Up"
  it should "pass the syntax" in {
    test(new Slide1UpModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlideDownLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlideDownLookup"
  it should "pass the syntax" in {
    test(new SlideDownLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VSlide1DownTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorSlide1Down"
  it should "pass the syntax" in {
    test(new Slide1DownModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VRGatherLookupTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorRegGatherLookup"
  it should "pass the syntax" in {
    test(new VRGatherLookupModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}

class VCompressTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorCompress"
  it should "pass the syntax" in {
    test(new CompressModule()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}
class VIntDividerTest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasTestAnnos {

  behavior of "YunSuan VectorIntDivider"
  it should "pass the syntax" in {
    test(new VectorIdiv()).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      VerilatorFlags(Seq()),
      // WriteVcdAnnotation,
      // TargetDirAnnotation("./build"),
    )) { dut =>
      dut.clock.step(10)
    }
  }
}