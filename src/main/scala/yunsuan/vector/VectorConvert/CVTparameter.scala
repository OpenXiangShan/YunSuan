package yunsuan.vector.VectorConvert

import chisel3._
import chisel3.util._

object RoundingModle{
  def RNE: UInt = 0.U(3.W)
  def RTZ: UInt = 1.U(3.W)
  def RDN: UInt = 2.U(3.W)
  def RUP: UInt = 3.U(3.W)
  def RMM: UInt = 4.U(3.W)
  def RTO: UInt = 6.U(3.W)
}

trait FloatFormat{
  def signWidth : Int
  def expWidth  : Int
  def fracWidth : Int
  def bias      : Int
  def width =  signWidth + expWidth + fracWidth
  def precision = fracWidth + 1
  def maxExp = (BigInt(1) << expWidth) - 2
  def minExp = 1
  def froundMaxExp = fracWidth + bias
}

object f16 extends FloatFormat {
  def signWidth = 1
  def expWidth = 5
  def fracWidth = 10
  def bias = 15
}

object f32 extends FloatFormat{
  def signWidth = 1
  def expWidth = 8
  def fracWidth = 23
  def bias = 127
}

object f64 extends FloatFormat{
  def signWidth = 1
  def expWidth = 11
  def fracWidth = 52
  def bias = 1023
}

//object f128 extends FloatFormat{
//  def signWidth = 1
//  def expWidth = 15
//  def fracWidth = 112
//  def bias = 16383
//}

object fpParam {
  val fpMap = Seq(f16, f32, f64)
  val biasDeltaMap = Seq(f32.bias - f16.bias, f64.bias - f32.bias, f64.bias - f16.bias)
}

