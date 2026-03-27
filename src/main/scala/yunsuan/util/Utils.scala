package yunsuan.util

import chisel3._
import chisel3.util._
import chisel3.util.BitPat.bitPatToUInt

object Utils {
  def VecDataToMaskDataVec(vecData: UInt, vsew: UInt): Vec[UInt] = {
    val maskWidth = vecData.getWidth / 8
    val maskDataVec = Wire(Vec(8, UInt(maskWidth.W)))
    require(8 * maskWidth == vecData.getWidth, "can not split this vector data into mask data vec")
    for ((maskData, i) <- maskDataVec.zipWithIndex) {
      maskData := Mux1H(Seq(
        (vsew === 0.U) -> vecData((i + 1) * maskWidth     - 1, i * maskWidth    ),
        (vsew === 1.U) -> vecData((i + 1) * maskWidth / 2 - 1, i * maskWidth / 2),
        (vsew === 2.U) -> vecData((i + 1) * maskWidth / 4 - 1, i * maskWidth / 4),
        (vsew === 3.U) -> vecData((i + 1) * maskWidth / 8 - 1, i * maskWidth / 8),
      ))
    }
    maskDataVec
  }

  def NOnes(n: Int): UInt = bitPatToUInt(BitPat.Y(n))

  def NZeros(n: Int): UInt = bitPatToUInt(BitPat.N(n))

  def SplitMask(maskIn: UInt, vsew: UInt): Vec[UInt] = {
    val maskWidth = maskIn.getWidth
    val maskDataVec = Wire(Vec(maskWidth / 8, UInt(8.W)))
    for ((maskData, i) <- maskDataVec.zipWithIndex) {
      maskData := Mux1H(Seq(
        (vsew === 0.U) -> maskIn(i * 8 + 7, i * 8),
        (vsew === 1.U) -> maskIn(i * 4 + 3, i * 4),
        (vsew === 2.U) -> maskIn(i * 2 + 1, i * 2),
        (vsew === 3.U) -> maskIn(i),
      ))
    }
    maskDataVec
  }
}
