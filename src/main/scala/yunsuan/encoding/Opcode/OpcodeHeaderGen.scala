package yunsuan.encoding.Opcode

import java.io.PrintWriter

/**
 * Generates a C/C++ header containing all opcode encodings.
 *
 * The encodings are defined once in Opcodes.scala; the C++ testbench (csrc)
 * includes the generated header so that the RTL and the golden models always
 * agree on the same encodings.
 *
 * Usage:
 *   mill -i YunSuan.runMain yunsuan.encoding.Opcode.OpcodeHeaderGen <out.h>
 */
object OpcodeHeaderGen {
  def main(args: Array[String]): Unit = {
    val outPath = if (args.nonEmpty) args(0) else "build/opcodes.h"
    // Make sure every opcode object is initialized so that `all` is complete.
    Opcodes.initOpcodes
    val objects = Opcodes.opcodeObjects

    val pw = new PrintWriter(new java.io.File(outPath))
    try {
      pw.println("// Auto-generated from Opcodes.scala by OpcodeHeaderGen. DO NOT EDIT.")
      pw.println("#ifndef __OPCODES_GEN_H")
      pw.println("#define __OPCODES_GEN_H")
      for (obj <- objects) {
        val prefix = obj.getClass.getSimpleName.stripSuffix("$").toUpperCase
        for (op <- obj.all.sortBy(_.encode.value)) {
          val cName = prefix + "_" + op.name.toUpperCase
          val v = op.encode.value
          pw.println(s"#define $cName (0x${v.toString(16)})")
        }
      }
      pw.println("#endif // __OPCODES_GEN_H")
    } finally {
      pw.close()
    }
    println(s"Generated $outPath")
  }
}
