
import chisel3._

package object yunsuan {
  def OpTypeWidth: Int = 8

  object OpType {
    def apply() = UInt(OpTypeWidth.W)
  }

  object VipuType {
    def dummy                          = "b00001111".U(OpTypeWidth.W) // exu not implemented
    def add                            = "b00000000".U(OpTypeWidth.W) // src1 + src2 //vadd vmadc
    def sub                            = "b00000010".U(OpTypeWidth.W) // src1 - src2 //vsub vrsub vmsbc
    def addCarry                       = "b00000001".U(OpTypeWidth.W) // src1 + src2 + carry //vadc vmadc
    def subBorrow                      = "b00000011".U(OpTypeWidth.W) // src1 + borrow - src2 //vsbc vmsbc
    val unsignedWideningAdd            = "b00000100".U(OpTypeWidth.W) //vwaddu
    val unsignedWideningsub            = "b00000101".U(OpTypeWidth.W) //vwsubu
    val signedWideningAdd              = "b00000110".U(OpTypeWidth.W) //vwadd
    val signedWideningSub              = "b00000111".U(OpTypeWidth.W) //vwsub
    val unsignedWideningAddIn0Widening = "b00001000".U(OpTypeWidth.W) //vwaddu
    val unsignedWideningSubIn0Widening = "b00001001".U(OpTypeWidth.W) //vwsubu
    val signedWideningAddIn0Widening   = "b00001010".U(OpTypeWidth.W) //vwadd
    val signedWideningSubIn0Widening   = "b00001011".U(OpTypeWidth.W) //vwsub
    val maxUnsigned                    = "b00001100".U(OpTypeWidth.W) //vmaxu
    val minUnsigned                    = "b00001101".U(OpTypeWidth.W) //vminu
    val maxSigned                      = "b00001110".U(OpTypeWidth.W) //vmax
    val minSigned                      = "b00001111".U(OpTypeWidth.W) //vmin
    val equal                          = "b00010000".U(OpTypeWidth.W) //vmseq
    val notEqual                       = "b00010001".U(OpTypeWidth.W) //vmsne
    val lessThanUnsigned               = "b00010010".U(OpTypeWidth.W) //vmsltu
    val lessThanSigned                 = "b00010011".U(OpTypeWidth.W) //vmslt
    val lessThanOrEqualUnsigned        = "b00010100".U(OpTypeWidth.W) //vmsleu
    val lessThanOrEqualSigned          = "b00010101".U(OpTypeWidth.W) //vmsle
    val greaterThanUnsigned            = "b00010110".U(OpTypeWidth.W) //vmsgtu
    val greaterThanSigned              = "b00010111".U(OpTypeWidth.W) //vmsgt
    val greaterThanOrEqualUnsigned     = "b00011000".U(OpTypeWidth.W) //vmsgeu
    val greaterThanOrEqualSigned       = "b00011001".U(OpTypeWidth.W) //vmsge
    // TODO: other op and method
  }

  object VfpuType {
    def dummy         = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def fadd          = "b10000000".U(OpTypeWidth.W) // src1 + src2
    def fmin          = "b10000001".U(OpTypeWidth.W) // fmin(src1,src2)
    def fmax          = "b10000010".U(OpTypeWidth.W) // fmax(src1,src2)
    def feq           = "b10000011".U(OpTypeWidth.W) // feq(src1,src2)
    def fne           = "b10000100".U(OpTypeWidth.W) // fne(src1,src2)
    def flt           = "b10000101".U(OpTypeWidth.W) // flt(src1,src2)
    def fle           = "b10000110".U(OpTypeWidth.W) // fle(src1,src2)
    def fgt           = "b10000111".U(OpTypeWidth.W) // fgt(src1,src2)
    def fge           = "b10001000".U(OpTypeWidth.W) // fge(src1,src2)
    def fsub          = "b10001001".U(OpTypeWidth.W) // src1 - src2
    def fmacc         = "b00001010".U(OpTypeWidth.W) // vd = +(src1 * src2) + vd
    def fdiv          = "b00001011".U(OpTypeWidth.W) // vd = src2 / src1
    
    def isVfalu(vfpuType: UInt) = vfpuType(7) & !vfpuType(6)
  }

  object VppuType {
    def dummy = "b11111111".U(OpTypeWidth.W) // exu not implemented
    def f2s   = "b10000000".U(OpTypeWidth.W) // vd[0] = f[rs1] (vs2=0)
    def vslide1up = "b10000001".U(OpTypeWidth.W) // vd[0]=f[rs1], vd[i+1] = vs2[i]
  }

  object VectorElementFormat {
    def width = 2
    def b = "b00".U(width.W)
    def h = "b01".U(width.W)  // f16
    def w = "b10".U(width.W)  // f32
    def d = "b11".U(width.W)  // f64

    def apply() = UInt(width.W)
  }
}
