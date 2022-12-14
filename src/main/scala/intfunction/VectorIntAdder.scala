package intfunction

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

/**
 * 8/16/32/64 bits vector integer add/sub/add with carry/sub with borrow
 * carry or borrow output
 * support vadd/vsub/vrsub/vadc/vmadc/vsbc/vmsbc instruction
 **/
class VectorIntAdder() extends Module {
  val n:Int = 64
  val io = IO(new Bundle {
    val in_0 = Input(UInt(n.W))
    val in_1 = Input(UInt(n.W))
    val int_format = Input(UInt(3.W)) // 0->int8,1->int16,2->int32,3->int64
    val op_code = Input(UInt(2.W)) // 0->add,1->sub,2->carry,3->borrow
    val carry_or_borrow_in = Input(UInt(8.W))
    val carry_or_borrow_out = Output(UInt(8.W))
    val out = Output(UInt(n.W))
  })

  val is_int8 = io.int_format === 0.U
  val is_int16 = io.int_format === 1.U
  val is_int32 = io.int_format === 2.U
  val is_int64 = io.int_format === 3.U

  val is_add = io.op_code === 0.U
  val is_sub = io.op_code === 1.U
  val is_add_carry = io.op_code === 2.U
  val is_sub_borrow = io.op_code === 3.U

  val temp7 = !is_int8
  val temp6 = is_int32 | is_int64
  val temp5 = !is_int8
  val temp4 = is_int64
  val temp3 = !is_int8
  val temp2 = is_int32 | is_int64
  val temp1 = !is_int8
  val temp_1 = 1.U
  val temp_0 = 0.U

  val com1_8 = Cat(Fill(9, temp_0), is_int8, Fill(9, temp_0), is_int8, Fill(9, temp_0), is_int8 ,Fill(9, temp_0), is_int8, Fill(9, temp_0), is_int8 ,Fill(9, temp_0), is_int8, Fill(9, temp_0), is_int8 ,Fill(9, temp_0), is_int8)
  val com1_16 = Cat(Fill(19, temp_0), is_int16 ,Fill(19, temp_0), is_int16, Fill(19, temp_0), is_int16 ,Fill(19, temp_0), is_int16)
  val com1_32 = Cat(Fill(39, temp_0), is_int32 ,Fill(39, temp_0), is_int32)
  val com1_64 = Cat(Fill(79, temp_0), is_int64)
  val com0 = Cat(Fill(80, temp_0))

  val add_carry_temp_8 = Cat(Fill(9, temp_0), io.carry_or_borrow_in(7), Fill(9, temp_0),  io.carry_or_borrow_in(6), Fill(9, temp_0),  io.carry_or_borrow_in(5), Fill(9, temp_0),  io.carry_or_borrow_in(4), Fill(9, temp_0),  io.carry_or_borrow_in(3), Fill(9, temp_0),  io.carry_or_borrow_in(2), Fill(9, temp_0),  io.carry_or_borrow_in(1), Fill(9, temp_0),  io.carry_or_borrow_in(0))
  val add_carry_temp_16 = Cat(Fill(19, temp_0), io.carry_or_borrow_in(3), Fill(19, temp_0), io.carry_or_borrow_in(2), Fill(19, temp_0), io.carry_or_borrow_in(1), Fill(19, temp_0), io.carry_or_borrow_in(0))
  val add_carry_temp_32 = Cat(Fill(39, temp_0), io.carry_or_borrow_in(1) ,Fill(39, temp_0), io.carry_or_borrow_in(0))
  val add_carry_temp_64 = Cat(Fill(79, temp_0), io.carry_or_borrow_in(0))

  val sub_borrow_temp_8 = Cat(Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(7), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(6), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(5), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(4), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(3), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(2), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(1), Fill(2, temp_1), Fill(7, temp_0), io.carry_or_borrow_in(0))
  val sub_borrow_temp_16 = Cat(Fill(2, temp_1), Fill(17, temp_0), io.carry_or_borrow_in(3), Fill(2, temp_1), Fill(17, temp_0), io.carry_or_borrow_in(2), Fill(2, temp_1), Fill(17, temp_0), io.carry_or_borrow_in(1), Fill(2, temp_1), Fill(17, temp_0), io.carry_or_borrow_in(0))
  val sub_borrow_temp_32 = Cat(Fill(2, temp_1), Fill(37, temp_0), io.carry_or_borrow_in(1), Fill(2, temp_1), Fill(37, temp_0), io.carry_or_borrow_in(0))
  val sub_borrow_temp_64 = Cat(Fill(2, temp_1), Fill(77, temp_0), io.carry_or_borrow_in(0))

  val in_0_temp = Cat(Fill(2, temp_0), io.in_0.head(8), Fill(2, temp7), io.in_0(55, 48), Fill(2, temp6), io.in_0(47, 40),
    Fill(2, temp5), io.in_0(39, 32), Fill(2, temp4), io.in_0(31, 24), Fill(2, temp3), io.in_0(23, 16),
    Fill(2, temp2), io.in_0(15, 8), Fill(2, temp1), io.in_0(7, 0))
  val in_1_temp = Cat(Fill(2, temp_0), io.in_1.head(8), Fill(2, temp_0), io.in_1(55, 48), Fill(2, temp_0), io.in_1(47, 40),
    Fill(2, temp_0), io.in_1(39, 32), Fill(2, temp_0), io.in_1(31, 24), Fill(2, temp_0), io.in_1(23, 16),
    Fill(2, temp_0), io.in_1(15, 8), Fill(2, temp_0), io.in_1(7, 0))
  val in_1_temp_n = Cat(Fill(2, temp_0),io.in_1.head(8), Fill(2, temp_1), io.in_1(55, 48), Fill(2, temp_1), io.in_1(47, 40),
    Fill(2, temp_1), io.in_1(39, 32), Fill(2, temp_1), io.in_1(31, 24), Fill(2, temp_1), io.in_1(23, 16),
    Fill(2, temp_1), io.in_1(15, 8), Fill(2, temp_1), io.in_1(7, 0))

  val com1 = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64), Seq(com1_8, com1_16, com1_32, com1_64)
  )
  val add_carry_temp = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64), Seq(add_carry_temp_8, add_carry_temp_16, add_carry_temp_32, add_carry_temp_64)
  )
  val sub_borrow_temp_n =Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64),Seq(sub_borrow_temp_8, sub_borrow_temp_16, sub_borrow_temp_32, sub_borrow_temp_64 )
  )

  val add = in_0_temp + in_1_temp
  val sub = in_0_temp + (~in_1_temp_n).asUInt + com1
  val add_carry =  add + add_carry_temp
  val sub_borrow = sub + (~sub_borrow_temp_n).asUInt + com1

 val out_temp = Mux1H(Seq(is_add, is_sub, is_add_carry, is_sub_borrow),
    Seq(add, sub, add_carry, sub_borrow))
    io.out := Cat(out_temp(77, 70), out_temp(67, 60), out_temp(57, 50), out_temp(47, 40),
    out_temp(37, 30), out_temp(27, 20), out_temp(17, 10), out_temp(7, 0))

  val carry_out = Cat(add_carry(78), add_carry(68), add_carry(58), add_carry(48),
    add_carry(38), add_carry(28), add_carry(18), add_carry(8))
  val borrow_out = Cat(sub_borrow(79), sub_borrow(69), sub_borrow(59), sub_borrow(49),
    sub_borrow(39), sub_borrow(29), sub_borrow(19), sub_borrow(9))

    io.carry_or_borrow_out := Mux(io.op_code === 0.U || io.op_code === 1.U, carry_out, borrow_out)
  }
