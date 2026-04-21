package yunsuan.fpu.fmul.utils

import chisel3._
import yunsuan.fpu.{CSA3to2, CSA4to2}

import scala.collection.mutable.ListBuffer

class CSAnTo2Opt1CSA4to2(n: Int, width: Int = 106) extends Module{
  override def desiredName = (this.getClass.getName + s"_$n").split("\\.").last
  val io = IO(new Bundle() {
    val in      = Input(Vec(n, UInt(width.W)))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
  })
  val in = ListBuffer[UInt]()
  io.in.foreach(a => in += a)
  val in_next = ListBuffer[UInt]()
  var n_next = n
  var CSA4to2Num = if(n_next == 8 || n_next == 4) n_next / 4 else 0
  var CSA3to2Num = if(n_next == 8 || n_next == 4) 0 else n_next / 3
  var remainder = n_next - CSA4to2Num * 4 - CSA3to2Num * 3
  var level = 0
  var is_piped = false
  while (CSA4to2Num != 0 || CSA3to2Num != 0){
    level = level + 1
    in_next.remove(0, in_next.length)
    for (i <- 0 until CSA4to2Num) {
      val U_CSA4to2 = Module(new CSA4to2(width = width))
      U_CSA4to2.io.in_a := in(i * 4 + 0)
      U_CSA4to2.io.in_b := in(i * 4 + 1)
      U_CSA4to2.io.in_c := in(i * 4 + 2)
      U_CSA4to2.io.in_d := in(i * 4 + 3)
      in_next += U_CSA4to2.io.out_sum
      in_next += U_CSA4to2.io.out_car
    }
    for (i <- 0 until CSA3to2Num) {
      val U_CSA3to2 = Module(new CSA3to2(width = width))
      U_CSA3to2.io.in_a := in(i * 3 + 0)
      U_CSA3to2.io.in_b := in(i * 3 + 1)
      U_CSA3to2.io.in_c := in(i * 3 + 2)
      in_next += U_CSA3to2.io.out_sum
      in_next += U_CSA3to2.io.out_car
    }
    if (remainder == 1) in_next += in.last
    if (remainder == 2) {
      in_next += in(in.length-2)
      in_next += in.last
    }
    in.remove(0, in.length)
    in_next.foreach(a => in += a)
    n_next = (CSA4to2Num + CSA3to2Num) * 2 + remainder
    CSA4to2Num = if(n_next == 8 || n_next == 4) n_next / 4 else 0
    CSA3to2Num = if(n_next == 8 || n_next == 4) 0 else n_next / 3
    remainder = n_next - CSA4to2Num * 4 - CSA3to2Num * 3
  }
  io.out_sum := in_next(0)
  io.out_car := in_next(1)
}

class CSAnTo2Opt2CSA4to2(n: Int, width: Int = 106) extends Module{
  override def desiredName = (this.getClass.getName + s"_$n").split("\\.").last
  val io = IO(new Bundle() {
    val in      = Input(Vec(n, UInt(width.W)))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
  })
  val in = ListBuffer[UInt]()
  io.in.take(27).foreach(a => in += a)
  val in_next = ListBuffer[UInt]()
  var n_next = n - 2
  var CSA4to2Num = if(n_next == 8 || n_next == 4) n_next / 4 else 0
  var CSA3to2Num = if(n_next == 8 || n_next == 4) 0 else n_next / 3
  var remainder = n_next - CSA4to2Num * 4 - CSA3to2Num * 3
  var level = 0
  var is_piped = false
  while (CSA4to2Num != 0 || CSA3to2Num != 0){
    level = level + 1
    println(s"level = $level")
    println(s"CSA4to2Num = $CSA4to2Num")
    println(s"CSA3to2Num = $CSA3to2Num")
    println(s"n_this = $n_next")
    in_next.remove(0, in_next.length)
    for (i <- 0 until CSA4to2Num) {
      val U_CSA4to2 = Module(new CSA4to2(width = width))
      U_CSA4to2.io.in_a := in(i * 4 + 0)
      U_CSA4to2.io.in_b := in(i * 4 + 1)
      U_CSA4to2.io.in_c := in(i * 4 + 2)
      U_CSA4to2.io.in_d := in(i * 4 + 3)
      in_next += U_CSA4to2.io.out_sum
      in_next += U_CSA4to2.io.out_car
    }
    for (i <- 0 until CSA3to2Num) {
      val U_CSA3to2 = Module(new CSA3to2(width = width))
      U_CSA3to2.io.in_a := in(i * 3 + 0)
      U_CSA3to2.io.in_b := in(i * 3 + 1)
      U_CSA3to2.io.in_c := in(i * 3 + 2)
      in_next += U_CSA3to2.io.out_sum
      in_next += U_CSA3to2.io.out_car
    }
    if (remainder == 1) in_next += in.last
    if (remainder == 2) {
      in_next += in(in.length-2)
      in_next += in.last
    }
    if (level == 4){
      in_next += io.in(27)
      in_next += io.in(28)
    }
    in.remove(0, in.length)
    in_next.foreach(a => in += a)
    if (level == 4) n_next = (CSA4to2Num + CSA3to2Num) * 2 + remainder + 2
    else n_next = (CSA4to2Num + CSA3to2Num) * 2 + remainder
    println(s"n_next = $n_next")
    println()
    CSA4to2Num = if((level != 3) && (n_next == 8 || n_next == 4)) n_next / 4 else 0
    CSA3to2Num = if((level != 3) && (n_next == 8 || n_next == 4)) 0 else n_next / 3
    remainder = n_next - CSA4to2Num * 4 - CSA3to2Num * 3
  }
  io.out_sum := in_next(0)
  io.out_car := in_next(1)
}


class CSAnTo2OptAll32(n: Int, width: Int = 106) extends Module{
  override def desiredName = (this.getClass.getName + s"_$n").split("\\.").last
  val io = IO(new Bundle() {
    val in      = Input(Vec(n, UInt(width.W)))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
  })
  val in = ListBuffer[UInt]()
  io.in.foreach(a => in += a)
  val in_next = ListBuffer[UInt]()
  var n_next = n
  var CSA3to2Num = n_next / 3
  var remainder = n_next - CSA3to2Num * 3
  var level = 0
  var is_piped = false
  while (CSA3to2Num != 0){
    level = level + 1
    in_next.remove(0, in_next.length)
    for (i <- 0 until CSA3to2Num) {
      val U_CSA3to2 = Module(new CSA3to2(width = width))
      U_CSA3to2.io.in_a := in(i * 3 + 0)
      U_CSA3to2.io.in_b := in(i * 3 + 1)
      U_CSA3to2.io.in_c := in(i * 3 + 2)
      in_next += U_CSA3to2.io.out_sum
      in_next += U_CSA3to2.io.out_car
    }
    if (remainder == 1) in_next += in.last
    if (remainder == 2) {
      in_next += in(in.length-2)
      in_next += in.last
    }
    in.remove(0, in.length)
    in_next.foreach(a => in += a)
    n_next = (CSA3to2Num) * 2 + remainder
    CSA3to2Num = n_next / 3
    remainder = n_next - CSA3to2Num * 3
  }
  io.out_sum := in_next(0)
  io.out_car := in_next(1)
}