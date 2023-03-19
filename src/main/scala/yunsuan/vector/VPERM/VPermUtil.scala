package yunsuan.vector.vpermutil

import chisel3._
import chisel3.util._
import yunsuan.util._

trait VPermParameter {
    val VLEN : Int = 128
    val XLEN : Int = 64
}

class VPermBundle extends Bundle with VPermParameter
class VPermModule extends Module with VPermParameter

// vialu io
class VPermIO extends VPermBundle {
    val vs1       = Input(UInt(VLEN.W))
    val vs1_type  = Input(UInt(4.W))
    val vs2       = Input(UInt(VLEN.W))
    val vs2_type  = Input(UInt(4.W))
    val old_vd    = Input(UInt(VLEN.W))
    val vd_type   = Input(UInt(4.W))

    val opcode    = Input(UInt(6.W))
    val uop_idx   = Input(UInt(6.W))

    val mask      = Input(UInt(VLEN.W))
    val vm        = Input(Bool())         // 0: masked, 1: unmasked
    val ta        = Input(Bool())         // 0: undisturbed, 1: agnostic
    val ma        = Input(Bool())         // 0: undisturbed, 1: agnostic

    val vstart    = Input(UInt(7.W))      // 0-127
    val vl        = Input(UInt(8.W))      // 0-128
    val vlmul     = Input(UInt(3.W))

    val res_vd    = Output(UInt(VLEN.W))
}

object VPermType {
    def VPermTypeWidth: Int = 4

    def vslideup     = "b0000".U(VPermTypeWidth.W) // Slideup
    def vslidedown   = "b0001".U(VPermTypeWidth.W) // Slidedown
    def vslide1up    = "b0010".U(VPermTypeWidth.W) // Slide1up
    def vslide1down  = "b0011".U(VPermTypeWidth.W) // Slide1down
    def vrgather     = "b0100".U(VPermTypeWidth.W) // Register Gather
    def vrgatherrs1  = "b0101".U(VPermTypeWidth.W) // Register Gather, index is from rs1
    def vcompress    = "b0110".U(VPermTypeWidth.W) // Compress
    def vwregmov     = "b0111".U(VPermTypeWidth.W) // Whole Register Move
}

object VFormat{
    def VFormatTable = List(
        "b00".U -> (8.U,  16.U, 4.U),
        "b01".U -> (16.U, 8.U,  3.U),
        "b10".U -> (32.U, 4.U,  2.U),
        "b11".U -> (64.U, 2.U,  1.U)
    )
}

// select a certen length part of the mask from begin bit
// assume begin is 
//  16*i i=0 to 7 for sew=8
//   8*i i=0 to 7 for sew=16
//   4*i i=0 to 7 for sew=32
//   2*i i=0 to 7 for sew=64
object SelectMaskN{
  def apply(mask: UInt, n: Int, begin: UInt) = {
    val sels_bits = Wire(Vec(20, UInt(n.W)))

    for (i <- 0 until 20) {
        if (i <= 7) { // 2*j j=0 to 7 j=i
            sels_bits(i) := mask(2*i + n-1, 2*i) & Fill(n, (begin === (2*i).U).asUInt)
        }
        else if (8 <= i && i <= 11) { // 4*j j=4 to 7 j=i-8+4
            sels_bits(i) := mask(4*(i-8+4) + n-1, 4*(i-8+4)) & Fill(n, (begin === (4*(i-8+4)).U).asUInt)
        }
        else if (12 <= i && i <= 15) { // 8*j j=4 to 7 j=i-12+4
            sels_bits(i) := mask(8*(i-12+4) + n-1, 8*(i-12+4)) & Fill(n, (begin === (8*(i-12+4)).U).asUInt)
        }
        else { // 16*j j=4 to 7 j=i-16+4
            sels_bits(i) := mask(16*(i-16+4) + n-1, 16*(i-16+4)) & Fill(n, (begin === (16*(i-16+4)).U).asUInt)
        }
    }
    ParallelOR(sels_bits)
  }
}
