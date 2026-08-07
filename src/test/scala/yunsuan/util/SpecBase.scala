package yunsuan.util

import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.ParallelTestExecution
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import yunsuan.util.Cli.{Coverage, DefaultTimescale, EmitFst}

trait SpecBase
  extends AnyFunSpec
    with ChiselSim
    with Matchers
    with ParallelTestExecution
    with Coverage
    with EmitFst
    with DefaultTimescale
