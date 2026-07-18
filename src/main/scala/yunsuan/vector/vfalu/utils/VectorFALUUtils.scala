package yunsuan.vector.vfalu.utils

import chisel3._
import chisel3.util._
import yunsuan.fpu.FloatParams

import scala.math.pow
import scala.reflect.runtime.universe._
import yunsuan.vector.vfmul.utils.VFParas

//object FpALUUtils extends VFParas {
//  private val fpWidthList = List(16, 32, 64)
//
//  private def srcWidthCheck(src: UInt): Unit = {
//    val width = src.getWidth
//    require(fpWidthList.contains(width), s"Unsupported float-point width: $width. Supported widths are: ${fpWidthList.mkString(", ")}")
//  }
//
//  def fpNeg(fpElem: UInt): UInt = {
//    srcWidthCheck(fpElem)
//
//    Cat(~fpElem.head(1), fpElem.tail(1))
//  }
//
//  def split2Vec(src: UInt, elemNum: Int): Vec[UInt] = {
//    srcWidthCheck(src)
//    require(Seq(2, 4, 8).contains(elemNum), s"Unsupported split number for 64-bits UInt: $elemNum")
//    val elemWidth = src.getWidth / elemNum
//
//    VecInit((0 until elemNum).map(i => src(elemWidth * (i + 1) - 1, elemWidth * i)))
//  }
//}
