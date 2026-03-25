package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._
import yunsuan.encoding.Opcode.Opcodes.FCvtOpcode
import yunsuan.vector.Common._

object Bundles {
  def CommonConnect(sink: Bundle, source: Bundle, valid: Bool) = {
    sink := RegEnable(source, valid)
  }

  class Special extends Bundle {
    val expNotZero = Bool()
    val expIsOnes = Bool()
    val fracNotZero = Bool()
    val isInf = Bool()
    val isZero = Bool()
    val isSubnormal = Bool()
    val isnormal = Bool()
    val isNaN = Bool()
    val isSNaN = Bool()
    val isQNaN = Bool()
  }
}

