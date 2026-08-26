// import Mill dependency
import mill._
import mill.define.Sources
import mill.modules.Util
import mill.scalalib.TestModule.ScalaTest
import scalalib._
// support BSP
import mill.bsp._
import publish._
import scalalib._

object ivys{
  val sv = "2.13.17"
  val chisel3 = mvn"org.chipsalliance::chisel:7.13.0"
  val chisel3Plugin = mvn"org.chipsalliance:::chisel-plugin:7.13.0"
  val chiseltest = mvn"edu.berkeley.cs::chiseltest:6.0.0"
  val scalatest = mvn"org.scalatest::scalatest:3.2.20"
  val sourcecode = mvn"com.lihaoyi::sourcecode:0.4.4"
}

trait YSModule extends ScalaModule with PublishModule {
  // override this to use chisel from source
  def chiselOpt: Option[PublishModule] = None

  override def scalaVersion = ivys.sv

  override def scalacPluginIvyDeps = Agg(ivys.chisel3Plugin)

  override def scalacOptions = Seq("-language:reflectiveCalls", "-deprecation", "-feature")

  override def ivyDeps = (if(chiselOpt.isEmpty) Agg(ivys.chisel3) else Agg.empty[Dep])

  override def moduleDeps = Seq() ++ chiselOpt

  def publishVersion = "0.0.1"

  // TODO: fix this
  def pomSettings = PomSettings(
    description = "YunSuan",
    organization = "",
    url = "https://github.com/OpenXiangShan/YunSuan",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("OpenXiangShan", "YunSuan"),
    developers = Seq.empty
  )
}

trait CommonYunSuan extends YSModule with SbtModule { m =>
  val pwd = os.Path(sys.env("MILL_WORKSPACE_ROOT"))

  override def millSourcePath = pwd

  override def forkArgs = Seq("-Xmx128G", "-Xss256m")

  val resourcesPATH = pwd.toString() + "/src/main/resources"
  val envPATH = sys.env("PATH") + ":" + resourcesPATH
  override def forkEnv = Map("PATH" -> envPATH)

  override def ivyDeps = super.ivyDeps() ++ Seq(
    ivys.chiseltest,
    ivys.sourcecode,
  )

  override def moduleDeps = super.moduleDeps ++ Seq(
  )

  object test extends SbtTests with TestModule.ScalaTest {

    override def forkArgs = m.forkArgs

    override def forkEnv = m.forkEnv

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivys.scalatest
    )
  }
}

object YunSuan extends CommonYunSuan {
}
