package yunsuan.encoding

import chisel3._

private[yunsuan] object VecInt {
  class Vs1Type {
    def width = 4

    def apply() = UInt(width.W)

    private def uint = "b00_00".U.litValue
    private def sint = "b01_00".U.litValue

    private def b8  = "b00_00".U.litValue
    private def b16 = "b00_01".U.litValue
    private def b32 = "b00_10".U.litValue
    private def b64 = "b00_11".U.litValue

    // normal int
    def u8  : UInt = (uint | b8 ).U(width.W)
    def u16 : UInt = (uint | b16).U(width.W)
    def u32 : UInt = (uint | b32).U(width.W)
    def u64 : UInt = (uint | b64).U(width.W)
    def s8  : UInt = (sint | b8 ).U(width.W)
    def s16 : UInt = (sint | b16).U(width.W)
    def s32 : UInt = (sint | b32).U(width.W)
    def s64 : UInt = (sint | b64).U(width.W)

    // mask
    def mask : UInt = "b1111".U(width.W)
  }

  class Vs2Type {
    def width = 4

    def apply() = UInt(width.W)

    private def uint = "b00_00".U.litValue
    private def sint = "b01_00".U.litValue

    private def b8  = "b00_00".U.litValue
    private def b16 = "b00_01".U.litValue
    private def b32 = "b00_10".U.litValue
    private def b64 = "b00_11".U.litValue

    // normal int
    def u8  : UInt = (uint | b8 ).U(width.W)
    def u16 : UInt = (uint | b16).U(width.W)
    def u32 : UInt = (uint | b32).U(width.W)
    def u64 : UInt = (uint | b64).U(width.W)
    def s8  : UInt = (sint | b8 ).U(width.W)
    def s16 : UInt = (sint | b16).U(width.W)
    def s32 : UInt = (sint | b32).U(width.W)
    def s64 : UInt = (sint | b64).U(width.W)

    // mask
    def mask : UInt = "b1111".U(width.W)
  }
}


