package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.{OpType, VipuType0, VectorElementFormat}

/**
 * 8/16/32/64 bits vector integer add/sub/add with carry/sub with borrow
 * carry or borrow output
 * unsigned/signed/ widening add/sub
 * unsigned/signed max/min
 * unsigned/signed compare(equal/not equal/less/less or equal/greater/ greater or equal)
 * support vadd/vsub/vrsub/vadc/vmadc/vsbc/vmsbc instruction
 * support vwaddu/vwsubu/vwadd/vwsub instruction
 * support vmin/vminu/vmax/vmaxu instruction
 * support vmseq/vmsne/vmsltu/vmslt/vmsleu/vmsle/vmsgtu/vmsgt/vmsgeu/vmsge instruction
 **/

class VectorIntAdder() extends Module {
  val n: Int = 64
  val io = IO(new Bundle {
    val in_0 = Input(UInt(n.W))
    val in_1 = Input(UInt(n.W))
    val int_format = Input(VectorElementFormat()) // 0->int8,1->int16,2->int32,3->int64
    val op_code = Input(OpType())
    val uop_index = Input(OpType())
    val rm_s = Input(UInt(2.W))
    val carry_or_borrow_in = Input(UInt(8.W))
    val carry_or_borrow_or_compare_out = Output(UInt(8.W))
    val out = Output(UInt(n.W))
  })

  val is_int8 = io.int_format === 0.U
  val is_int16 = io.int_format === 1.U
  val is_int32 = io.int_format === 2.U
  val is_int64 = io.int_format === 3.U

  val is_add = io.op_code === VipuType0.add  //vadd 
  val is_sub = io.op_code === VipuType0.sub || io.op_code === VipuType0.rsub  //vsub vrsub 
  val is_add_carry = io.op_code === VipuType0.adc || io.op_code === VipuType0.madc || io.op_code === VipuType0.madc0  //vadc vmadc
  val is_sub_borrow = io.op_code === VipuType0.subBorrow  //vsbc vmsbc
  val is_unsigned_widening_add = io.op_code === VipuType0.unsignedWideningAdd  //vwaddu
  val is_unsigned_widening_sub = io.op_code === VipuType0.unsignedWideningsub  //vwsubu
  val is_signed_widening_add = io.op_code === VipuType0.signedWideningAdd  //vwadd
  val is_signed_widening_sub = io.op_code === VipuType0.signedWideningSub  //vwsub
  val is_unsigned_widening_add_in0widening = io.op_code === VipuType0.unsignedWideningAddIn0Widening  //vwaddu
  val is_unsigned_widening_sub_in0widening = io.op_code === VipuType0.unsignedWideningSubIn0Widening  //vwsubu
  val is_signed_widening_add_in0widening = io.op_code === VipuType0.signedWideningAddIn0Widening  //vwadd
  val is_signed_widening_sub_in0widening = io.op_code === VipuType0.signedWideningSubIn0Widening  //vwsub
  val is_max_unsigned = io.op_code === VipuType0.maxUnsigned  //vmaxu
  val is_min_unsigned = io.op_code === VipuType0.minUnsigned  //vminu
  val is_max_signed = io.op_code === VipuType0.maxSigned  //vmax
  val is_min_signed = io.op_code === VipuType0.minSigned  //vmin
  val is_equal = io.op_code === VipuType0.equal  //vmseq
  val is_not_equal = io.op_code === VipuType0.notEqual  //vmsne
  val is_less_than_unsigned = io.op_code === VipuType0.lessThanUnsigned  //vmsltu
  val is_less_than_signed = io.op_code === VipuType0.lessThanSigned  //vmslt
  val is_less_than_or_equal_unsigned = io.op_code === VipuType0.lessThanOrEqualUnsigned  //vmsleu
  val is_less_than_or_equal_signed = io.op_code === VipuType0.lessThanOrEqualSigned  //vmsle
  val is_greater_than_unsigned = io.op_code === VipuType0.greaterThanUnsigned  //vmsgtu
  val is_greater_than_signed = io.op_code === VipuType0.greaterThanSigned  //vmsgt
  val is_greater_than_or_equal_unsigned = io.op_code === VipuType0.greaterThanOrEqualUnsigned  //vmsgeu
  val is_greater_than_or_equal_signed = io.op_code === VipuType0.greaterThanOrEqualSigned  //vmsge
  val is_bitwise_logical_and = io.op_code === VipuType0.bitwiseLogicalAnd //vand
  val is_bitwise_logical_nand = io.op_code === VipuType0.bitwiseLogicalNand  //vnand
  val is_bitwise_logical_andn = io.op_code === VipuType0.bitwiseLogicalAndn  //vandn
  val is_bitwise_logical_or = io.op_code === VipuType0.bitwiseLogicalOr  //vor
  val is_bitwise_logical_nor = io.op_code === VipuType0.bitwiseLogicalNor  //vnor
  val is_bitwise_logical_orn = io.op_code === VipuType0.bitwiseLogicalOrn  //vorn
  val is_bitwise_logical_xor = io.op_code === VipuType0.bitwiseLogicalXor  //vxor
  val is_bitwise_logical_xnor = io.op_code === VipuType0.bitwiseLogicalXnor  //vxnor
  val is_shift_left_logical = io.op_code === VipuType0.shiftLeftLogical  //vsll
  val is_shift_right_logical = io.op_code === VipuType0.shiftRightLogical  //vsrl
  val is_shift_right_arithmetic = io.op_code === VipuType0.shiftRightArithmetic  //vsra
  val is_scaling_shift_right_logical = io.op_code === VipuType0.scalingShiftRightLogical  //vssrl
  val is_scaling_shift_right_arithmetic =io.op_code === VipuType0.scalingShiftRightArithmetic  //vssra

  val is_widening_add = is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_signed_widening_add_in0widening | is_signed_widening_add
  val is_widening_sub = is_unsigned_widening_sub | is_unsigned_widening_sub_in0widening | is_signed_widening_sub_in0widening | is_signed_widening_sub
  val is_basic = is_bitwise_logical_and | is_bitwise_logical_nand | is_bitwise_logical_andn | is_bitwise_logical_or | is_bitwise_logical_nor | is_bitwise_logical_orn |
  is_bitwise_logical_xor | is_bitwise_logical_xnor | is_shift_left_logical | is_shift_right_logical | is_shift_right_arithmetic | is_scaling_shift_right_logical | is_scaling_shift_right_arithmetic

  //8 expend 8
  //8 expend 4
  //8 expand 2
  //16 expend 4
  //16 expend 2
  //32 expand 2

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

  val in_0_temp_unsigned_widening_64_half = Cat(Fill(40, temp_0), in_0_temp(39, 0))
  val in_0_temp_unsigned_widening_32_half = Cat(Fill(20, temp_0), in_0_temp(59, 40),Fill(20, temp_0), in_0_temp(19, 0))
  val in_0_temp_unsigned_widening_16_half = Cat(Fill(10, temp_0), in_0_temp(69, 60),Fill(10, temp_0), in_0_temp(49, 40), Fill(10, temp_0), in_0_temp(29, 20),Fill(10, temp_0), in_0_temp(9, 0))

  val in_1_temp_unsigned_widening_64_half = Cat(Fill(40, temp_0), in_1_temp(39, 0))
  val in_1_temp_unsigned_widening_32_half = Cat(Fill(20, temp_0), in_1_temp(59, 40), Fill(20, temp_0), in_1_temp(19, 0))
  val in_1_temp_unsigned_widening_16_half = Cat(Fill(10, temp_0), in_1_temp(69, 60), Fill(10, temp_0), in_1_temp(49, 40), Fill(10, temp_0), in_1_temp(29, 20), Fill(10, temp_0), in_1_temp(9, 0))

  val in_1_temp_unsigned_widening_64_half_n = Cat(Fill(40, temp_1), in_1_temp_n(39, 0))
  val in_1_temp_unsigned_widening_32_half_n = Cat(Fill(20, temp_1), in_1_temp_n(59, 40), Fill(20, temp_1), in_1_temp_n(19, 0))
  val in_1_temp_unsigned_widening_16_half_n = Cat(Fill(10, temp_1), in_1_temp_n(69, 60), Fill(10, temp_1), in_1_temp_n(49, 40), Fill(10, temp_1), in_1_temp_n(29, 20), Fill(10, temp_1), in_1_temp_n(9, 0))

  val in_0_temp_signed_widening_64_half = Cat(Fill(40, in_0_temp(37)), in_0_temp(39, 0))
  val in_0_temp_signed_widening_32_half = Cat(Fill(20, in_0_temp(57)), in_0_temp(59, 40), Fill(20, in_0_temp(17)), in_0_temp(19, 0))
  val in_0_temp_signed_widening_16_half = Cat(Fill(10, in_0_temp(67)), in_0_temp(69, 60), Fill(10, in_0_temp(47)), in_0_temp(49, 40), Fill(10, in_0_temp(27)), in_0_temp(29, 20), Fill(10, in_0_temp(7)), in_0_temp(9, 0))

  val in_1_temp_signed_widening_64_half = Cat(Fill(40, in_1_temp(37)), in_1_temp(39, 0))
  val in_1_temp_signed_widening_32_half = Cat(Fill(20, in_1_temp(57)), in_1_temp(59, 40), Fill(20, in_1_temp(17)), in_1_temp(19, 0))
  val in_1_temp_signed_widening_16_half = Cat(Fill(10, in_1_temp(67)), in_1_temp(69, 60), Fill(10, in_1_temp(47)), in_1_temp(49, 40), Fill(10, in_1_temp(27)), in_1_temp(29, 20), Fill(10, in_1_temp(7)), in_1_temp(9, 0))

  val in_1_temp_signed_widening_64_half_n = Cat(Fill(40, !in_1_temp_n(37)), in_1_temp_n(39, 0))
  val in_1_temp_signed_widening_32_half_n = Cat(Fill(20, !in_1_temp_n(57)), in_1_temp_n(59, 40), Fill(20, !in_1_temp(17)), in_1_temp_n(19, 0))
  val in_1_temp_signed_widening_16_half_n = Cat(Fill(10, !in_1_temp_n(67)), in_1_temp_n(69, 60), Fill(10, !in_1_temp_n(47)), in_1_temp_n(49, 40), Fill(10, !in_1_temp_n(27)), in_1_temp_n(29, 20), Fill(10, !in_1_temp_n(7)), in_1_temp_n(9, 0))

  val com1 = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64), Seq(com1_8, com1_16, com1_32, com1_64)
  )

  val add_carry_temp = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64), Seq(add_carry_temp_8, add_carry_temp_16, add_carry_temp_32, add_carry_temp_64)
  )
  val sub_borrow_temp_n = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64),Seq(sub_borrow_temp_8, sub_borrow_temp_16, sub_borrow_temp_32, sub_borrow_temp_64 )
  )

  val in_0_temp_widening = Mux1H(
    Seq( is_int16 & (is_unsigned_widening_add | is_unsigned_widening_sub ),
      is_int32 & (is_unsigned_widening_add | is_unsigned_widening_sub ),
      is_int64 & (is_unsigned_widening_add | is_unsigned_widening_sub ),
      is_int16 & (is_signed_widening_add | is_signed_widening_sub),
      is_int32 & (is_signed_widening_add | is_signed_widening_sub),
      is_int64 & (is_signed_widening_add | is_signed_widening_sub),
      is_unsigned_widening_add_in0widening | is_signed_widening_add_in0widening | is_unsigned_widening_sub_in0widening | is_signed_widening_sub_in0widening
    ),
    Seq(in_0_temp_unsigned_widening_16_half,in_0_temp_unsigned_widening_32_half, in_0_temp_unsigned_widening_64_half,
      in_0_temp_signed_widening_16_half,in_0_temp_signed_widening_32_half, in_0_temp_signed_widening_64_half,in_0_temp
    )
  )

  val in_1_temp_widening = Mux1H(
    Seq(is_int16 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int32 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int64 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int16 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening),
      is_int32 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening),
      is_int64 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening)
    ),
    Seq(in_1_temp_unsigned_widening_16_half, in_1_temp_unsigned_widening_32_half, in_1_temp_unsigned_widening_64_half,
      in_1_temp_signed_widening_16_half, in_1_temp_signed_widening_32_half, in_1_temp_signed_widening_64_half,
    )
  )

  val in_1_temp_widening_n = Mux1H(
    Seq(is_int16 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int32 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int64 & (is_unsigned_widening_add | is_unsigned_widening_add_in0widening | is_unsigned_widening_sub | is_unsigned_widening_add_in0widening),
      is_int16 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening),
      is_int32 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening),
      is_int64 & (is_signed_widening_add | is_signed_widening_add_in0widening | is_signed_widening_sub | is_signed_widening_add_in0widening)
    ),
    Seq(in_1_temp_unsigned_widening_16_half_n, in_1_temp_unsigned_widening_32_half_n, in_1_temp_unsigned_widening_64_half_n,
      in_1_temp_signed_widening_16_half_n, in_1_temp_signed_widening_32_half_n, in_1_temp_signed_widening_64_half_n,
    )
  )

  val add = in_0_temp + in_1_temp
  val sub = in_0_temp + (~in_1_temp_n).asUInt + com1
  val add_carry =  add + add_carry_temp
  val sub_borrow = sub + (~sub_borrow_temp_n).asUInt + com1
  val widening_add = in_0_temp_widening + in_1_temp_widening
  val widening_sub = in_0_temp_widening + (~in_1_temp_widening_n).asUInt + com1

  val is_equal_out_64 = Cat(Fill(7, temp_0), Mux(io.in_0(63,0) === io.in_1(63,0), temp_1, temp_0))
  val is_equal_out_32 = Cat(Fill(6, temp_0), Mux(io.in_0(63,32) === io.in_1(63,32), temp_1, temp_0).asUInt, Mux(io.in_0(31,0) === io.in_1(31,0), temp_1, temp_0).asUInt)
  val is_equal_out_16 = Cat(Fill(4, temp_0), Mux(io.in_0(63,48) === io.in_1(63,48), temp_1, temp_0).asUInt, Mux(io.in_0(47,32) === io.in_1(47,32), temp_1, temp_0).asUInt, Mux(io.in_0(31,16) === io.in_1(31,16), temp_1, temp_0).asUInt, Mux(io.in_0(15,0) === io.in_1(15,0), temp_1, temp_0).asUInt)
  val is_equal_out_8 = Cat(Mux(io.in_0(63,56) === io.in_1(63,56), temp_1, temp_0).asUInt, Mux(io.in_0(55,48) === io.in_1(55,48), temp_1, temp_0).asUInt, Mux(io.in_0(47,40) === io.in_1(47,40), temp_1, temp_0).asUInt, Mux(io.in_0(39,32) === io.in_1(39,32), temp_1, temp_0).asUInt, Mux(io.in_0(31,24) === io.in_1(31,24), temp_1, temp_0).asUInt, Mux(io.in_0(23,16) === io.in_1(23,16), temp_1, temp_0).asUInt, Mux(io.in_0(15,8) === io.in_1(15,8), temp_1, temp_0).asUInt, Mux(io.in_0(7,0) === io.in_1(7,0), temp_1, temp_0).asUInt)

  val is_less_than_unsigned_out_64 = Cat(Fill(7, temp_0), Mux(sub(79) === 0.U, temp_0, temp_1).asUInt)
  val is_less_than_unsigned_out_32 = Cat(Fill(6, temp_0), Mux(sub(79) === 0.U, temp_0, temp_1).asUInt, Mux(sub(39) === 0.U, temp_0, temp_1).asUInt)
  val is_less_than_unsigned_out_16 = Cat(Fill(4, temp_0), Mux(sub(79) === 0.U, temp_0, temp_1).asUInt, Mux(sub(59) === 0.U, temp_0, temp_1).asUInt, Mux(sub(39) === 0.U, temp_0, temp_1).asUInt, Mux(sub(19) === 0.U, temp_0, temp_1).asUInt)
  val is_less_than_unsigned_out_8 = Cat(Mux(sub(79) === 0.U, temp_0, temp_1).asUInt, Mux(sub(69) === 0.U, temp_0, temp_1).asUInt, Mux(sub(59) === 0.U, temp_0, temp_1).asUInt, Mux(sub(49) === 0.U, temp_0, temp_1).asUInt, Mux(sub(39) === 0.U, temp_0, temp_1).asUInt, Mux(sub(29) === 0.U, temp_0, temp_1).asUInt, Mux(sub(19) === 0.U, temp_0, temp_1).asUInt, Mux(sub(9) === 0.U, temp_0, temp_1).asUInt)

  val is_less_than_signed_out_64 = Cat(Fill(7, temp_0), Mux((sub(79) === 0.U & sub(78) === 0.U)|(sub(79) === 1.U & sub(78) === 1.U) , temp_0, temp_1).asUInt)
  val is_less_than_signed_out_32 = Cat(Fill(6, temp_0), Mux((sub(79) === 0.U & sub(78) === 0.U)|(sub(79) === 1.U & sub(78) === 1.U) , temp_0, temp_1).asUInt, Mux((sub(39) === 0.U & sub(38) === 0.U)|(sub(39) === 1.U & sub(38) === 1.U) , temp_0, temp_1).asUInt)
  val is_less_than_signed_out_16 = Cat(Fill(4, temp_0), Mux((sub(79) === 0.U & sub(78) === 0.U)|(sub(79) === 1.U & sub(78) === 1.U) , temp_0, temp_1).asUInt, Mux((sub(59) === 0.U & sub(58) === 0.U)|(sub(59) === 1.U & sub(58) === 1.U) , temp_0, temp_1).asUInt, Mux((sub(39) === 0.U & sub(38) === 0.U)|(sub(39) === 1.U & sub(38) === 1.U) , temp_0, temp_1).asUInt, Mux((sub(19) === 0.U & sub(18) === 0.U)|(sub(19) === 1.U & sub(18) === 1.U) , temp_0, temp_1).asUInt)
  val is_less_than_signed_out_8 = Cat(Mux((sub(79) === 0.U & sub(78) === 0.U) | (sub(79) === 1.U & sub(78) === 1.U), temp_0, temp_1).asUInt, Mux((sub(69) === 0.U & sub(68) === 0.U) | (sub(69) === 1.U & sub(68) === 1.U), temp_0, temp_1).asUInt, Mux((sub(59) === 0.U & sub(58) === 0.U) | (sub(59) === 1.U & sub(58) === 1.U), temp_0, temp_1).asUInt, Mux((sub(49) === 0.U & sub(48) === 0.U) | (sub(49) === 1.U & sub(48) === 1.U), temp_0, temp_1).asUInt, Mux((sub(39) === 0.U & sub(38) === 0.U) | (sub(39) === 1.U & sub(38) === 1.U), temp_0, temp_1).asUInt, Mux((sub(29) === 0.U & sub(28) === 0.U) | (sub(29) === 1.U & sub(28) === 1.U), temp_0, temp_1).asUInt, Mux((sub(19) === 0.U & sub(18) === 0.U) | (sub(19) === 1.U & sub(18) === 1.U), temp_0, temp_1).asUInt, Mux((sub(9) === 0.U & sub(8) === 0.U) | (sub(9) === 1.U & sub(8) === 1.U), temp_0, temp_1).asUInt)

  val is_equal_out = Mux1H(Seq(is_int64, is_int32, is_int16, is_int8), Seq(is_equal_out_64, is_equal_out_32, is_equal_out_16, is_equal_out_8))
  val is_not_equal_out = (~is_equal_out).asUInt
  val is_less_than_unsigned_out = Mux1H(Seq(is_int64, is_int32, is_int16, is_int8),Seq(is_less_than_unsigned_out_64,is_less_than_unsigned_out_32, is_less_than_unsigned_out_16,is_less_than_unsigned_out_8))
  val is_less_than_signed_out = Mux1H(Seq(is_int64, is_int32, is_int16, is_int8),Seq(is_less_than_signed_out_64,is_less_than_signed_out_32, is_less_than_signed_out_16,is_less_than_signed_out_8))
  val is_greater_than_unsigned_out = (~is_less_than_unsigned_out).asUInt
  val is_greater_than_signed_out = (~is_less_than_signed_out).asUInt
  val is_less_than_or_equal_unsigned_out = is_equal_out | is_less_than_unsigned_out
  val is_less_than_or_equal_signed_out = is_equal_out | is_less_than_signed_out
  val is_greater_than_or_equal_unsigned_out = is_equal_out | is_greater_than_unsigned_out
  val is_greater_than_or_equal_signed_out = is_equal_out | is_greater_than_signed_out

  val carry_out_with_carry_64 = Cat(Fill(7,temp_0),add_carry(78))
  val carry_out_with_carry_32 = Cat(Fill(6,temp_0),add_carry(78), add_carry(38))
  val carry_out_with_carry_16 = Cat(Fill(4,temp_0),add_carry(78), add_carry(58), add_carry(38), add_carry(18))
  val carry_out_with_carry_8 = Cat(add_carry(78), add_carry(68), add_carry(58), add_carry(48),
    add_carry(38), add_carry(28), add_carry(18), add_carry(8))

  val carry_out_without_carry_64 = Cat(Fill(7, temp_0), add(78))
  val carry_out_without_carry_32 = Cat(Fill(6, temp_0), add(78), add(38))
  val carry_out_without_carry_16 = Cat(Fill(4, temp_0), add(78), add(58), add(38), add(18))
  val carry_out_without_carry_8 = Cat(add(78), add(68), add(58), add(48),
    add(38), add(28), add(18), add(8))

  val borrow_out_with_borrow_64 = Cat(Fill(7, temp_0), sub_borrow(78))
  val borrow_out_with_borrow_32 = Cat(Fill(6, temp_0), sub_borrow(78), sub_borrow(38))
  val borrow_out_with_borrow_16 = Cat(Fill(4, temp_0), sub_borrow(78), sub_borrow(58), sub_borrow(38), sub_borrow(18))
  val borrow_out_with_borrow_8 = Cat(sub_borrow(78), sub_borrow(68), sub_borrow(58), sub_borrow(48),
    sub_borrow(38), sub_borrow(28), sub_borrow(18), sub_borrow(8))

  val borrow_out_without_borrow_64 = Cat(Fill(7, temp_0), sub(78))
  val borrow_out_without_borrow_32 = Cat(Fill(6, temp_0), sub(78), sub(38))
  val borrow_out_without_borrow_16 = Cat(Fill(4, temp_0), sub(78), sub(58), sub(38), sub(18))
  val borrow_out_without_borrow_8 = Cat(sub(78), sub(68), sub(58), sub(48),
    sub(38), sub(28), sub(18), sub(8))

  val carry_out_with_carry = Mux1H(Seq(is_int8, is_int16, is_int32, is_int64),Seq(carry_out_with_carry_8,carry_out_with_carry_16,carry_out_with_carry_32,carry_out_with_carry_64))
  val carry_out_without_carry = Mux1H(Seq(is_int8, is_int16, is_int32, is_int64), Seq(carry_out_without_carry_8, carry_out_without_carry_16, carry_out_without_carry_32, carry_out_without_carry_64))
  val borrow_out_with_borrow = Mux1H(Seq(is_int8, is_int16, is_int32, is_int64), Seq(borrow_out_with_borrow_8, borrow_out_with_borrow_16,borrow_out_with_borrow_32, borrow_out_with_borrow_64))
  val borrow_out_without_borrow = Mux1H(Seq(is_int8, is_int16, is_int32, is_int64), Seq(borrow_out_without_borrow_8, borrow_out_without_borrow_16,borrow_out_without_borrow_32, borrow_out_without_borrow_64))

  val out_max_unsigned_temp_0 = Mux(borrow_out_without_borrow(0) === 0.U , in_0_temp(9,0), in_1_temp(9,0))
  val out_max_unsigned_temp_1 = Mux(borrow_out_without_borrow(1) === 0.U , in_0_temp(19,10), in_1_temp(19,10))
  val out_max_unsigned_temp_2 = Mux(borrow_out_without_borrow(2) === 0.U , in_0_temp(29,20), in_1_temp(29,20))
  val out_max_unsigned_temp_3 = Mux(borrow_out_without_borrow(3) === 0.U , in_0_temp(39,30), in_1_temp(39,30))
  val out_max_unsigned_temp_4 = Mux(borrow_out_without_borrow(4) === 0.U , in_0_temp(49,40), in_1_temp(49,40))
  val out_max_unsigned_temp_5 = Mux(borrow_out_without_borrow(5) === 0.U , in_0_temp(59,50), in_1_temp(59,50))
  val out_max_unsigned_temp_6 = Mux(borrow_out_without_borrow(6) === 0.U , in_0_temp(69,60), in_1_temp(69,60))
  val out_max_unsigned_temp_7 = Mux(borrow_out_without_borrow(7) === 0.U , in_0_temp(79,70), in_1_temp(79,70))

  val out_max_unsigned = Cat(out_max_unsigned_temp_7, out_max_unsigned_temp_6, out_max_unsigned_temp_5, out_max_unsigned_temp_4, out_max_unsigned_temp_3, out_max_unsigned_temp_2, out_max_unsigned_temp_1, out_max_unsigned_temp_0)

  val out_max_signed_temp_0 = Mux((borrow_out_without_borrow(0) ^ carry_out_without_carry(0)) === 0.U, in_0_temp(9, 0), in_1_temp(9, 0))
  val out_max_signed_temp_1 = Mux((borrow_out_without_borrow(1) ^ carry_out_without_carry(1)) === 0.U, in_0_temp(19, 10), in_1_temp(19, 10))
  val out_max_signed_temp_2 = Mux((borrow_out_without_borrow(2) ^ carry_out_without_carry(2)) === 0.U, in_0_temp(29, 20), in_1_temp(29, 20))
  val out_max_signed_temp_3 = Mux((borrow_out_without_borrow(3) ^ carry_out_without_carry(3)) === 0.U, in_0_temp(39, 30), in_1_temp(39, 30))
  val out_max_signed_temp_4 = Mux((borrow_out_without_borrow(4) ^ carry_out_without_carry(4)) === 0.U, in_0_temp(49, 40), in_1_temp(49, 40))
  val out_max_signed_temp_5 = Mux((borrow_out_without_borrow(5) ^ carry_out_without_carry(5)) === 0.U, in_0_temp(59, 50), in_1_temp(59, 50))
  val out_max_signed_temp_6 = Mux((borrow_out_without_borrow(6) ^ carry_out_without_carry(6)) === 0.U, in_0_temp(69, 60), in_1_temp(69, 60))
  val out_max_signed_temp_7 = Mux((borrow_out_without_borrow(7) ^ carry_out_without_carry(7)) === 0.U, in_0_temp(79, 70), in_1_temp(79, 70))

  val out_max_signed = Cat(out_max_signed_temp_7, out_max_signed_temp_6, out_max_signed_temp_5, out_max_signed_temp_4, out_max_signed_temp_3, out_max_signed_temp_2, out_max_signed_temp_1, out_max_signed_temp_0)

  val out_min_unsigned_temp_0 = Mux(borrow_out_without_borrow(0) === 0.U, in_0_temp(9, 0), in_1_temp(9, 0))
  val out_min_unsigned_temp_1 = Mux(borrow_out_without_borrow(1) === 0.U, in_0_temp(19, 10), in_1_temp(19, 10))
  val out_min_unsigned_temp_2 = Mux(borrow_out_without_borrow(2) === 0.U, in_0_temp(29, 20), in_1_temp(29, 20))
  val out_min_unsigned_temp_3 = Mux(borrow_out_without_borrow(3) === 0.U, in_0_temp(39, 30), in_1_temp(39, 30))
  val out_min_unsigned_temp_4 = Mux(borrow_out_without_borrow(4) === 0.U, in_0_temp(49, 40), in_1_temp(49, 40))
  val out_min_unsigned_temp_5 = Mux(borrow_out_without_borrow(5) === 0.U, in_0_temp(59, 50), in_1_temp(59, 50))
  val out_min_unsigned_temp_6 = Mux(borrow_out_without_borrow(6) === 0.U, in_0_temp(69, 60), in_1_temp(69, 60))
  val out_min_unsigned_temp_7 = Mux(borrow_out_without_borrow(7) === 0.U, in_0_temp(79, 70), in_1_temp(79, 70))

  val out_min_unsigned = Cat(out_min_unsigned_temp_7, out_min_unsigned_temp_6, out_min_unsigned_temp_5, out_min_unsigned_temp_4, out_min_unsigned_temp_3, out_min_unsigned_temp_2, out_min_unsigned_temp_1, out_min_unsigned_temp_0)

  val out_min_signed_temp_0 = Mux((borrow_out_without_borrow(0) ^ carry_out_without_carry(0)) === 1.U, in_0_temp(9, 0), in_1_temp(9, 0))
  val out_min_signed_temp_1 = Mux((borrow_out_without_borrow(1) ^ carry_out_without_carry(1)) === 1.U, in_0_temp(19, 10), in_1_temp(19, 10))
  val out_min_signed_temp_2 = Mux((borrow_out_without_borrow(2) ^ carry_out_without_carry(2)) === 1.U, in_0_temp(29, 20), in_1_temp(29, 20))
  val out_min_signed_temp_3 = Mux((borrow_out_without_borrow(3) ^ carry_out_without_carry(3)) === 1.U, in_0_temp(39, 30), in_1_temp(39, 30))
  val out_min_signed_temp_4 = Mux((borrow_out_without_borrow(4) ^ carry_out_without_carry(4)) === 1.U, in_0_temp(49, 40), in_1_temp(49, 40))
  val out_min_signed_temp_5 = Mux((borrow_out_without_borrow(5) ^ carry_out_without_carry(5)) === 1.U, in_0_temp(59, 50), in_1_temp(59, 50))
  val out_min_signed_temp_6 = Mux((borrow_out_without_borrow(6) ^ carry_out_without_carry(6)) === 1.U, in_0_temp(69, 60), in_1_temp(69, 60))
  val out_min_signed_temp_7 = Mux((borrow_out_without_borrow(7) ^ carry_out_without_carry(7)) === 1.U, in_0_temp(79, 70), in_1_temp(79, 70))

  val bitwise_logical_and_out = io.in_0 & io.in_1
  val bitwise_logical_nand_out = ~(io.in_0 & io.in_1)
  val bitwise_logical_andn_out = io.in_0 & (~io.in_1).asUInt
  val bitwise_logical_or_out = io.in_0 | io.in_1
  val bitwise_logical_nor_out = ~(io.in_0 | io.in_1)
  val bitwise_logical_orn_out = io.in_0 | (~io.in_1).asUInt
  val bitwise_logical_xor_out = io.in_0 ^ io.in_1
  val bitwise_logical_xnor_out = ~(io.in_0 ^ io.in_1)

  val is_in1_less_than_64 = io.in_1(63,7) === 0.U
  val shift_left_logical_out_temp1 = Cat(Fill(64, temp_0))
  val shift_left_logical_out_temp2 = io.in_0 << io.in_1(7, 0)
  val shift_left_logical_out = Mux(is_in1_less_than_64, shift_left_logical_out_temp2, shift_left_logical_out_temp1)
  val shift_right_logical_out = io.in_0 >> io.in_1
  val shift_right_arithmetic_out = Cat(io.in_0(63), shift_right_logical_out(62, 0))

  val in_1_scaling_temp_8 = Cat(Fill(56, temp_0), io.in_1(2, 0))
  val in_1_scaling_temp_16 = Cat(Fill(48, temp_0), io.in_1(3, 0))
  val in_1_scaling_temp_32 = Cat(Fill(32, temp_0), io.in_1(4, 0))
  val in_1_scaling_temp_64 = Cat(Fill(32, temp_0), io.in_1(5, 0))
  val scaling_shift_right_logical_out_8 = io.in_0 >> in_1_scaling_temp_8
  val scaling_shift_right_logical_out_16 = io.in_0 >> in_1_scaling_temp_16
  val scaling_shift_right_logical_out_32 = io.in_0 >> in_1_scaling_temp_32
  val scaling_shift_right_logical_out_64 = io.in_0 >> in_1_scaling_temp_64
  val scaling_shift_right_logical_out = Mux1H(
    Seq(is_int8, is_int16, is_int32, is_int64), Seq(scaling_shift_right_logical_out_8, scaling_shift_right_logical_out_16, scaling_shift_right_logical_out_32, scaling_shift_right_logical_out_64)
  )
  val scaling_shift_right_arithmetic_out = Cat(io.in_0(63), scaling_shift_right_logical_out(62, 0))

  val basic_out = Mux1H(
    Seq(is_bitwise_logical_and, is_bitwise_logical_nand, is_bitwise_logical_andn, is_bitwise_logical_or, is_bitwise_logical_nor, is_bitwise_logical_orn, is_bitwise_logical_xor,
      is_bitwise_logical_xnor, is_shift_left_logical, is_shift_right_logical, is_shift_right_arithmetic, is_scaling_shift_right_logical, is_scaling_shift_right_arithmetic),
    Seq(bitwise_logical_and_out, bitwise_logical_nand_out, bitwise_logical_andn_out, bitwise_logical_or_out, bitwise_logical_nor_out, bitwise_logical_orn_out,
      bitwise_logical_xor_out, bitwise_logical_xnor_out, shift_left_logical_out, shift_right_logical_out, shift_right_arithmetic_out)
  )

  val basic_out_temp = Cat(Fill(2, temp_0), basic_out.head(8), Fill(2, temp_0), basic_out(55, 48), Fill(2, temp_0), basic_out(47, 40),
    Fill(2, temp_0), basic_out(39, 32), Fill(2, temp_0), basic_out(31, 24), Fill(2, temp_0), basic_out(23, 16),
    Fill(2, temp_0), basic_out(15, 8), Fill(2, temp_0), basic_out(7, 0))

  val out_min_signed = Cat(out_min_unsigned_temp_7, out_min_unsigned_temp_6, out_min_unsigned_temp_5, out_min_unsigned_temp_4, out_min_unsigned_temp_3, out_min_unsigned_temp_2, out_min_unsigned_temp_1, out_min_unsigned_temp_0)

  val out_temp = Mux1H(Seq(is_add, is_sub, is_add_carry, is_sub_borrow, is_widening_add, is_widening_sub, is_max_unsigned, is_min_unsigned,is_max_signed,is_min_signed, is_basic),
    Seq(add, sub, add_carry, sub_borrow, widening_add, widening_sub, out_max_unsigned, out_min_unsigned, out_max_signed, out_min_signed, basic_out_temp))

  io.out := Cat(out_temp(77, 70), out_temp(67, 60), out_temp(57, 50), out_temp(47, 40),
    out_temp(37, 30), out_temp(27, 20), out_temp(17, 10), out_temp(7, 0))
  io.carry_or_borrow_or_compare_out := Mux1H(Seq(is_add, is_sub, is_add_carry, is_sub_borrow,
    is_equal, is_not_equal, is_less_than_unsigned, is_less_than_signed, is_less_than_or_equal_unsigned,
    is_less_than_or_equal_signed, is_greater_than_unsigned, is_greater_than_signed, is_greater_than_or_equal_unsigned,is_greater_than_or_equal_signed),
    Seq(carry_out_without_carry, borrow_out_without_borrow, carry_out_with_carry, borrow_out_with_borrow, is_equal_out, is_not_equal_out, is_less_than_unsigned_out,
      is_less_than_signed_out, is_less_than_or_equal_unsigned_out,is_less_than_or_equal_signed_out,
      is_greater_than_unsigned_out,is_greater_than_signed_out,is_greater_than_or_equal_unsigned_out, is_greater_than_or_equal_signed_out)
  )

}
