
import chisel3._

package object yunsuan {
  def OpTypeWidth: Int = 8

  object OpType {
    def apply() = UInt(OpTypeWidth.W)
  }

  object VipuType {
    def dummy         = "b1111".U(OpTypeWidth.W) // exu not implemented
    def add           = "b0000".U(OpTypeWidth.W) // src1 + src2
    def sub           = "b0010".U(OpTypeWidth.W) // src1 - src2
    def addCarry      = "b0001".U(OpTypeWidth.W) // src1 + src2 + carry
    def subBorrow     = "b0011".U(OpTypeWidth.W) // src1 + borrow - src2
    // TODO: other op and method
  }

  object VfpuType {
    def dummy         = "b1111".U(OpTypeWidth.W) // exu not implemented
  }

  object VectorElementFormat {
    def width = 2
    def b = "b00".U(width.W)
    def h = "b01".U(width.W)
    def w = "b10".U(width.W)
    def d = "b11".U(width.W)

    def apply() = UInt(width.W)
  }
}
