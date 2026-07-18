package yunsuan.util

import chisel3.simulator.scalatest.HasCliOptions
import chisel3.simulator.scalatest.HasCliOptions.CliOption
import org.scalatest.TestSuite
import svsim.Backend.HarnessCompilationFlags.enableFstTracingSupport
import svsim.CommonCompilationSettings.Timescale
import svsim.CommonCompilationSettings.Timescale.{Magnitude, Time}
import svsim.CommonCompilationSettings.VerilogPreprocessorDefine
import svsim.verilator.Backend.CompilationSettings.CoverageSettings

object Cli {

  trait DefaultTimescale extends HasCliOptions { this: TestSuite =>
    override implicit def commonSettingsModifications: svsim.CommonSettingsModifications = {
      (conf: svsim.CommonCompilationSettings) => {
        super.commonSettingsModifications(conf).copy(
          defaultTimescale = Some(Timescale(
            unit = Time(Magnitude.One, Timescale.Unit.ps),
            precision = Time(Magnitude.One, Timescale.Unit.ps),
          ))
        )
      }
    }
  }

  trait Coverage { this: HasCliOptions =>
    addOption(
      CliOption.flag(
        name = "coverage",
        help = "enable Verilator line+toggle+user coverage",
        updateBackendSettings = (options: svsim.Backend.Settings) =>
          options match {
            case c: svsim.verilator.Backend.CompilationSettings =>
              c.withCoverageSettings(new CoverageSettings(line = true, toggle = true, user = true))
            case other => other
          }
      )
    )
  }

  trait EmitFst { this: HasCliOptions =>
    addOption(
      CliOption
        .flag(
          name = "emitFst",
          help = "compile with FST waveform support and start dumping waves at time zero",
          updateCommonSettings = (options) => {
            options.copy(
              verilogPreprocessorDefines =
                options.verilogPreprocessorDefines :+ VerilogPreprocessorDefine(enableFstTracingSupport),
              simulationSettings = options.simulationSettings.copy(
                enableWavesAtTimeZero = true
              )
            )
          },
          updateBackendSettings = (options) =>
            options match {
              case options: svsim.verilator.Backend.CompilationSettings =>
                options.withTraceStyle(
                  options.traceStyle match {
                    case None =>
                      Some(
                        svsim.verilator.Backend.CompilationSettings
                          .TraceStyle(svsim.verilator.Backend.CompilationSettings.TraceKind.Fst())
                      )
                    case alreadySet => alreadySet
                  }
                )
            }
        )
    )
  }
}
