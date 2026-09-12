package yunsuan.encoding.Opcode

import java.io.PrintWriter

/**
 * Generates a C/C++ header containing the execution latency of every Opcode.
 *
 * Latencies are queried through Opcode.getLat, consistent with LatDecoder.
 * OPCODE_LATENCY_UNCERTAIN denotes variable latency.
 *
 * Usage:
 *   mill -i YunSuan.runMain yunsuan.encoding.Opcode.OpcodeLatencyHeaderGen <out.h>
 */
object OpcodeLatencyHeaderGen {
  def main(args: Array[String]): Unit = {
    val outPath = if (args.nonEmpty) args(0) else "build/opcode_latencies.h"
    // Initialize all Opcode objects to ensure that `all` is complete.
    Opcodes.initOpcodes
    val objects = Opcodes.opcodeObjects

    val pw = new PrintWriter(new java.io.File(outPath))
    try {
      pw.println("// Autogen by OpcodeLatencyHeaderGen. Do not modify.")
      pw.println("#ifndef YUNSUAN_OPCODE_LATENCIES_GEN_H")
      pw.println("#define YUNSUAN_OPCODE_LATENCIES_GEN_H")
      pw.println(s"#define OPCODE_LATENCY_UNCERTAIN (${Latency.uncertainLitVal()})")
      for (obj <- objects) {
        val prefix = obj.getClass.getSimpleName.stripSuffix("$").toUpperCase
        for (op <- obj.all.sortBy(_.encode.value)) {
          val cName = prefix + "_" + op.name.toUpperCase + "_LATENCY"
          pw.println(s"#define $cName (${op.getLat})")
        }
      }
      pw.println("#endif // YUNSUAN_OPCODE_LATENCIES_GEN_H")
    } finally {
      pw.close()
    }
    println(s"Latency file generated to $outPath")
  }
}
