package yunsuan.encoding

import chisel3._

private[yunsuan] object VecFp {
  class Vs1Type {
    def width = 4

    def apply() = UInt(width.W)

    private def vec_reg = "b10_00".U.litValue
    private def fp_reg  = "b11_00".U.litValue

    private def b8  = "b00_00".U.litValue
    private def b16 = "b00_01".U.litValue
    private def b32 = "b00_10".U.litValue
    private def b64 = "b00_11".U.litValue

    // normal int
    def vf16 : UInt = (vec_reg | b16).U(width.W)
    def vf32 : UInt = (vec_reg | b32).U(width.W)
    def vf64 : UInt = (vec_reg | b64).U(width.W)
    def fp16 : UInt = ( fp_reg | b16).U(width.W)
    def fp32 : UInt = ( fp_reg | b32).U(width.W)
    def fp64 : UInt = ( fp_reg | b64).U(width.W)
  }

  class Vs2Type {
    def width = 4

    def apply() = UInt(width.W)

    private def vec_reg = "b10_00".U.litValue
    private def fp_reg  = "b11_00".U.litValue

    private def b8  = "b00_00".U.litValue
    private def b16 = "b00_01".U.litValue
    private def b32 = "b00_10".U.litValue
    private def b64 = "b00_11".U.litValue

    // normal int
    def vf16 : UInt = (vec_reg | b16).U(width.W)
    def vf32 : UInt = (vec_reg | b32).U(width.W)
    def vf64 : UInt = (vec_reg | b64).U(width.W)
    def fp16 : UInt = ( fp_reg | b16).U(width.W)
    def fp32 : UInt = ( fp_reg | b32).U(width.W)
    def fp64 : UInt = ( fp_reg | b64).U(width.W)
  }
}

