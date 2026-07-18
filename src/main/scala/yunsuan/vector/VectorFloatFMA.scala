package yunsuan.vector

import chisel3._
import chisel3.util._
import scala.collection.mutable.ListBuffer
import yunsuan.util._
import yunsuan.encoding.Opcode.Opcodes.VFMacOpcode

private[vector] class CSA3to2(width :Int) extends Module{
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val in_c   = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
    
  })
  io.out_sum := io.in_a ^ io.in_b ^ io.in_c
  io.out_car := Cat( ( (io.in_a & io.in_b) | (io.in_a & io.in_c) | (io.in_b & io.in_c) )(width-2,0),0.U)
  
  
  
}
private[vector] class CSA4to2(width :Int) extends Module{
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val in_c   = Input(UInt(width.W))
    val in_d   = Input(UInt(width.W))
    val out_sum = Output(UInt(width.W))
    val out_car = Output(UInt(width.W))
    
  })
  val cout_vec  = Wire(Vec(width,UInt(1.W)))
  val sum_vec   = Wire(Vec(width,UInt(1.W)))
  val carry_vec = Wire(Vec(width,UInt(1.W)))
  val cin_0 = 0.U
  for (i <- 0 until width) {
    cout_vec(i) := Mux(io.in_a(i) ^ io.in_b(i), io.in_c(i), io.in_a(i))
    if (i ==0){
      sum_vec(i)   := io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cin_0 , io.in_d(i))
    }
    else {
      sum_vec(i)   :=  io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i) ^ cout_vec(i-1)
      carry_vec(i) := Mux(io.in_a(i) ^ io.in_b(i) ^ io.in_c(i) ^ io.in_d(i), cout_vec(i-1), io.in_d(i))
    }
  }

  val sum_temp_vec   = Wire(Vec(width,UInt(1.W)))
  val carry_temp_vec = Wire(Vec(width,UInt(1.W)))
  carry_temp_vec(0) := 0.U
  sum_temp_vec(0)   := sum_vec(0)
  for (i <- 1 until width) {
    if (i%2==1) {
      carry_temp_vec(i) := sum_vec(i)
      sum_temp_vec(i)   := carry_vec(i-1)
    }
    else {
      carry_temp_vec(i) := carry_vec(i-1)
      sum_temp_vec(i)   := sum_vec(i)
    }
  }


  io.out_sum := sum_temp_vec.asUInt
  io.out_car := carry_temp_vec.asUInt
  
  
  
}

