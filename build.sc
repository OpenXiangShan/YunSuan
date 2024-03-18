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
  val sv = "2.13.10"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.6.0"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.6.2"
  val chiselCirct = ivy"com.sifive::chisel-circt:0.6.0"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
}

trait YSModule extends ScalaModule with PublishModule {
  // override this to use chisel from source
  def chiselOpt: Option[PublishModule] = None

  override def scalaVersion = ivys.sv

  override def scalacPluginIvyDeps = Agg(ivys.chisel3Plugin)

  override def scalacOptions = Seq("-Xsource:2.13")

  override def ivyDeps = (if(chiselOpt.isEmpty) Agg(ivys.chisel3) else Agg.empty[Dep]) ++ Agg(ivys.chiselCirct)

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
    override def millSourcePath = os.pwd

  override def forkArgs = Seq("-Xmx128G", "-Xss256m")

  val resourcesPATH = os.pwd.toString() + "/src/main/resources"
  val envPATH = sys.env("PATH") + ":" + resourcesPATH
  override def forkEnv = Map("PATH" -> envPATH)

  override def ivyDeps = super.ivyDeps() ++ Seq(ivys.chiseltest)

  override def moduleDeps = super.moduleDeps ++ Seq(
  )

  object test extends SbtModuleTests with TestModule.ScalaTest {

    override def forkArgs = m.forkArgs

    override def forkEnv = m.forkEnv

    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivys.scalatest
    )
  }
}

object YunSuan extends CommonYunSuan {
}
