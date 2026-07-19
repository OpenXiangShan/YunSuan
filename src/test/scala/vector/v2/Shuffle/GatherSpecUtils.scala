package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.fromBooleanToLiteral
import chisel3.simulator.PeekPokeAPI.TestableBool
import chisel3.simulator.scalatest.{ChiselSim, HasCliOptions}
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import svsim.verilator.Backend.CompilationSettings.CoverageSettings
import yunsuan.util.Cli.EmitFst

trait GatherSpecBase extends EmitFst {
  this: AnyFunSpec with Matchers with ChiselSim =>

  override implicit def backendSettingsModifications: svsim.BackendSettingsModifications = {
    case conf: svsim.verilator.Backend.CompilationSettings =>
      conf.withCoverageSettings(new CoverageSettings(line = true, toggle = true, user = true))
    case conf => conf
  }
}

trait GatherSpecUtils {
  val dlen = 128
  val vlen = 128
  val numElem = dlen / 8
  def vlenb: Int = vlen / 8

  def pSew(dut: GatherSewBundle, sew: Int): Unit = {
    dut.e8.poke((sew == 8).B)
    dut.e16.poke((sew == 16).B)
    dut.e32.poke((sew == 32).B)
    dut.e64.poke((sew == 64).B)
  }

  def pLmul(dut: GatherLmulBundle, lmulFrac: Int): Unit = {
    dut.mf8.poke((lmulFrac == 1).B)
    dut.mf4.poke((lmulFrac == 2).B)
    dut.mf2.poke((lmulFrac == 4).B)
    dut.m1.poke((lmulFrac == 8).B)
    dut.m2.poke((lmulFrac == 16).B)
    dut.m4.poke((lmulFrac == 32).B)
    dut.m8.poke((lmulFrac == 64).B)
  }

  def sewShift(sew: Int): Int = sew match {
    case 8 => 0; case 16 => 1; case 32 => 2; case 64 => 3
  }

  def sewb(sew: Int): Int = sew / 8

  def toSewElems(value: BigInt, sew: Int, nElems: Int): Seq[Int] = {
    (0 until nElems).map(i => ((value >> (i * sew)) & ((BigInt(1) << sew) - 1)).toInt)
  }

  def fromSewElems(elems: Seq[Int], sew: Int): BigInt = {
    elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
      acc | (BigInt(v & ((1 << sew) - 1)) << (i * sew))
    }
  }

  def sewTo8b(vs2Sew: Seq[Int], sew: Int): Seq[Int] = {
    val s = sew / 8
    vs2Sew.flatMap(v => (0 until s).map(b => (v >> (b * 8)) & 0xff))
  }

  def eightBToSew(bytes: Seq[Int], sew: Int): Seq[Int] = {
    val s = sew / 8
    bytes.grouped(s).map(g => g.zipWithIndex.foldLeft(0) { case (a, (b, i)) => a | (b << (i * 8)) }).toSeq
  }
}
