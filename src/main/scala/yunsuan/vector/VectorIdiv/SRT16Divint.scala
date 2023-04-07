/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/



package yunsuan.vector

import chisel3._
import chisel3.util._
import yunsuan.util._
// Integer division module using SRT radix-16 algorithm, supporting 8/16/32/64bit bitwidth
// parameters
// (mk:index) -1: 0 0 :1 1:2 2:3
// (u:index) -2: 0 ...
// (q:index) 2:0   -2 3 need to reverse u index here，because we search as -q for const
// sign (index, k) :(0, -1)  (1, 0) (2, 1) (3, 2)

// v2 Attempting to reduce the number of bits required for the sel module, currently sel 3+5, spec 3+6
// In this file, x+y represents the xbit integer and the ybit fractions
class SRT16Divint(bit_width: Int) extends Module {
  val io = IO(new Bundle() {
    val sign = Input(Bool())
    //val Int_format = Input(UInt(2.W))
    val dividend = Input(UInt(bit_width.W))
    val divisor = Input(UInt(bit_width.W))
    val flush = Input(Bool())
    val d_zero = Output(Bool())
    val sew = Input(UInt(2.W)) // multi bit width
    /*
    'b00: I8
    'b01: I16
    'b10: I32
    'b11: I64
     */
    //val overflow_err = Output(Bool())
    //
    val div_in_valid = Input(Bool())
    val div_ready = Output(Bool())
    val div_out_ready = Input(Bool())
    val div_out_valid = Output(Bool())
    val div_out_q = Output(UInt(bit_width.W))
    val div_out_rem = Output(UInt(bit_width.W))

  })
  val Bypass_width = bit_width << 1 // Bypass, used to store divisions aligned with the advisor (not normalized by left shift) to directly obtain the final remainder, with a maximum bitwidth of twice the bit_ width
  val w_width = bit_width + 6 // 6: 1bit for sign ，2bit for *4/4 shift，  3bit for ralign
  val lzc_w = log2Up(bit_width) // lzc leading zero count
  val early_finish = Wire(Bool()) // handle special case
  val iter_finish = Wire(Bool()) // handle iteration finish
  val idle :: pre_0 :: pre_1 :: iter :: post :: output :: Nil = Enum(6)
  val (oh_idle,oh_pre_0,oh_pre_1, oh_iter, oh_post, oh_output) =
    (UIntToOH(idle,6),UIntToOH(pre_0,6),UIntToOH(pre_1,6),UIntToOH(iter,6),UIntToOH(post,6),UIntToOH(output, 6))
  // handshake
  val stateReg = RegInit((1<<idle.litValue.toInt).U(6.W))
  val in_handshake = io.div_in_valid && io.div_ready
  val out_handshake = io.div_out_valid && io.div_out_ready
  io.div_ready := stateReg(idle)
  io.div_out_valid := stateReg(output)
  //  fsm
  when(stateReg(idle) && in_handshake) {
    stateReg := oh_pre_0
  }.elsewhen(stateReg(pre_0)) {
    stateReg := oh_pre_1
  }.elsewhen(stateReg(pre_1)) {
    stateReg := Mux(early_finish,oh_post,oh_iter)
  }.elsewhen(stateReg(iter)) {
    stateReg := Mux(iter_finish,oh_post, oh_iter)
  }.elsewhen(stateReg(post)) {
    stateReg := oh_output
  }.elsewhen(stateReg(output) & io.div_out_ready) {
    stateReg := oh_idle
  }.elsewhen(io.flush){
    stateReg := oh_idle
  }.otherwise {
    stateReg :=stateReg
  }






  
  // pre_0 stage
  val x = io.dividend // x dividend
  //val x_reg = RegEnable(x,stateReg(pre_0))
  val d = io.divisor // d divisor
  val format = io.sew
  //val format = 3.U(2.W)
  // ext_x ext_d Due to the different bit widths of SEW, it cannot be guaranteed that the access signal is processed according to zero extension or symbol extension, so it needs to be rechecked,
  val ext_x = MuxCase(0.U(64.W), Seq(
    (format === "b00".U)  -> Mux(io.sign, SignExt(x(7,0),64), ZeroExt(x(7,0),64)),
    (format === "b01".U)  -> Mux(io.sign, SignExt(x(15,0),64), ZeroExt(x(15,0),64)),
    (format === "b10".U)  -> Mux(io.sign, SignExt(x(31,0),64), ZeroExt(x(31,0),64)),
    (format === "b11".U)  -> Mux(io.sign, SignExt(x(63,0),64), ZeroExt(x(63,0),64))
  ))
  val ext_d =  MuxCase(0.U(64.W), Seq(
    (format === "b00".U) -> Mux(io.sign, SignExt(d(7, 0), 64), ZeroExt(d(7, 0), 64)),
    (format === "b01".U) -> Mux(io.sign, SignExt(d(15, 0), 64), ZeroExt(d(15, 0), 64)),
    (format === "b10".U) -> Mux(io.sign, SignExt(d(31, 0), 64), ZeroExt(d(31, 0), 64)),
    (format === "b11".U) -> Mux(io.sign, SignExt(d(63, 0), 64), ZeroExt(d(63, 0), 64))
  ))
  // Hard coding of symbol bits under different sews
  val sign_array_x = VecInit(x(7),x(15),x(31),x(63))
  val sign_array_d = VecInit(d(7),d(15),d(31),d(63))



  val x_sign = io.sign && sign_array_x(format)
  //val x_sign = io.sign && x(bit_width - 1)
  val x_sign_reg = RegEnable(x_sign, stateReg(pre_0))
  val d_sign = io.sign && sign_array_d(format)
  //val d_sign = io.sign && d(bit_width - 1)

  // reg use for pre0
  val init_q_A = Wire(UInt(bit_width.W)) //q_A :real q
  val iter_q_A = Wire(UInt(bit_width.W))
  val iter_q_A_reg = RegEnable(iter_q_A, stateReg(pre_0) | stateReg(pre_1) | stateReg(iter)) // pre_0 store norm_part_x iter stage store real q
  val init_q_B = Wire(UInt(bit_width.W)) //q_B :real q - 1
  val iter_q_B = Wire(UInt(bit_width.W))
  val iter_q_B_reg = RegEnable(iter_q_B, stateReg(pre_0) | stateReg(pre_1) | stateReg(iter)) // pre_0 store norm_part_x iter stage store real q -1
//  val norm_x_part = Wire(UInt((bit_width+1).W))
//  val norm_d_part = Wire(UInt((bit_width+1).W))
  val norm_x_part = Wire(UInt((bit_width ).W)) // During normalization processing, a partion of the pre-stage is shifted first,
  val norm_d_part = Wire(UInt((bit_width ).W))

  // w for iter
  val nxt_w = Wire(Vec(2, UInt(w_width.W)))
  val w_iter = Wire(Vec(2, UInt(w_width.W)))
  val w_iter_reg = RegEnable(w_iter, stateReg(pre_0) | stateReg(pre_1) | stateReg(iter)) // pre_0 store lzc_x lzc_d lzc_diff  atthention cannot use ||
  val w_iter_mul16_trunc = Wire(Vec(2, UInt(8.W)))
  val w_iter_mul16_trunc_init = Wire(Vec(2, UInt(8.W)))
  val w_iter_mul16_trunc_iter = Wire(Vec(2, UInt(8.W)))
  val w_iter_mul16_trunc_reg = RegEnable(w_iter_mul16_trunc, stateReg(pre_1) | stateReg(iter)) // store 16*w
  val w_iter_mul64_trunc = Wire(Vec(2, UInt(9.W)))
  val w_iter_mul64_trunc_init = Wire(Vec(2, UInt(9.W)))
  val w_iter_mul64_trunc_iter = Wire(Vec(2, UInt(9.W)))
  val w_iter_mul64_trunc_reg = RegEnable(w_iter_mul64_trunc, stateReg(pre_1) | stateReg(iter))

  val neg_x_q = Wire(UInt(bit_width.W)) // -x or -real q
  val neg_d_q = Wire(UInt(bit_width.W)) // -d or -real q -1
  val abs_x = Wire(UInt(bit_width.W)) // Absolute value of x
  val abs_x_reg = RegEnable(abs_x, stateReg(pre_0))
  val abs_d = Wire(UInt(bit_width.W)) // Absolute value of d
  val abs_d_reg = RegEnable(abs_d, stateReg(pre_0))

//  neg_x_q := -Mux(stateReg(pre_0),x, iter_q_A_reg)
//  neg_d_q := -Mux(stateReg(pre_0), d,  iter_q_B_reg)
  neg_x_q := -Mux(stateReg(pre_0), ext_x, iter_q_A_reg)
  neg_d_q := -Mux(stateReg(pre_0), ext_d, iter_q_B_reg)
//  abs_x := Mux(x_sign, neg_x_q, x)
//  abs_d := Mux(d_sign, neg_d_q, d)
  abs_x := Mux(x_sign, neg_x_q, ext_x)
  abs_d := Mux(d_sign, neg_d_q, ext_d)


  // lzc
  val lzc_x = Wire(UInt(lzc_w.W)) // x leading zero count
  val zero_x = Wire(Bool()) // x all zero
  val lzc_d = Wire(UInt(lzc_w.W)) // d leading zero count
  val zero_d = Wire(Bool()) // d all zero
  val zero_d_reg = RegEnable(zero_d,stateReg(pre_0)) // Registers are used in the post stage to determine whether sign adjust are needed for q (which may be omitted)
  val lzc_pre0_all = Wire(UInt(14.W))
  val d_is_one = Wire(Bool())
  /* lzc_pre0_all :use for w_iter store lzc_x lzc_d lzc_diff from pre_0 to pre_1
      {
      norm_d_part[bit_width]
      norm_x_part[bit_width]
      zero_x,         [13:13]
      lzc_x        ,   [12:11](0,1)
      zero_d,          [10:10]
      d_is_one         [ 9: 9]
      lzc_d        ,   [ 8: 7](0,1)
      lzc_diff_zero_ex [ 6: 0]
      }
     存储在w_iter中，
    */

//  val lzc_x_m = Module(new Lzc_vary(bit_width))
//  val lzc_d_m = Module(new Lzc_vary(bit_width))
//  lzc_x_m.io.X := abs_x
//  lzc_x := lzc_x_m.io.Z
//  zero_x := lzc_x_m.io.V
//  lzc_d_m.io.X := abs_d
//  lzc_d := lzc_d_m.io.Z
//  zero_d := lzc_d_m.io.V
//  val x_to_lzc = Mux(x_sign, Cat(~x(bit_width - 1, 1), 1.U(1.W)), x)
//  val d_to_lzc = Mux(d_sign, Cat(~d(bit_width - 1, 1), 1.U(1.W)), d)
  val x_enc = Wire(UInt((lzc_w+1).W)) 
  val d_enc = Wire(UInt((lzc_w+1).W)) 
  x_enc := PriorityEncoder(abs_x(bit_width-1, 0).asBools().reverse)
  d_enc := PriorityEncoder(abs_d(bit_width-1, 0).asBools().reverse)
//  x_enc := PriorityEncoder(x_to_lzc(bit_width - 1, 0).asBools().reverse)
//  d_enc := PriorityEncoder(d_to_lzc(bit_width - 1, 0).asBools().reverse)
  lzc_x := x_enc(lzc_w - 1, 0)
  zero_x := ~abs_x.orR() // error to check zero  cannot check with lzc_x(lzc_w)
  lzc_d := d_enc(lzc_w - 1, 0)
  zero_d := ~abs_d.orR()
  d_is_one := lzc_d(lzc_w - 1, 0).andR()
  // lzc_diff and pre shifter
  val lzc_diff = Cat(0.U(1.W),lzc_d) - Cat(0.U(1.W),lzc_x) // x d the diffenrence between lzc of x and lzc of d
  val lzc_x_ex = ZeroExt(lzc_x, 6) // Due to the possibility of multiple bit widths, it is necessary to uniformly zero expand lzc for easy storage
  val lzc_d_ex = ZeroExt(lzc_d, 6)
  val lzc_diff_ex = SignExt(lzc_diff, 7) // err 1  sign ext not zero ext for
//  lzc_pre0_all := Cat(norm_d_part(bit_width),norm_x_part(bit_width)SRT16Divint.scala,zero_x,lzc_x_ex(1,0),zero_d,d_is_one,lzc_d_ex(1,0),lzc_diff_ex) // err2 forget to pass d_is_one
  lzc_pre0_all := Cat(zero_x,lzc_x_ex(1,0),zero_d,d_is_one,lzc_d_ex(1,0),lzc_diff_ex)
  norm_x_part := abs_x << Cat(lzc_x_ex(5,2),0.U(2.W)) //
  norm_d_part := abs_d << Cat(lzc_d_ex(5,2),0.U(2.W))

  // q _sign
  val q_sign = Wire(Bool())
  val q_sign_reg = RegEnable(q_sign, stateReg(pre_0))
  q_sign := x_sign ^ d_sign


  // pre_1 stage
//  val norm_x_err_top = Wire(UInt(1.W))
//  val norm_d_err_top = Wire(UInt(1.W))
  val lzc_x_1_0 = Wire(UInt(2.W)) // lzc_x(1,0)
  val zero_x_pre_1 = Wire(Bool())
  val lzc_d_1_0 = Wire(UInt(2.W)) // lzc_d(1,0)
  val zero_d_pre_1 = Wire(Bool())
  //val lzc_diff_err = Wire(UInt((lzc_w+1).W))
//  val x_need_c     = Wire(Bool())
//  val d_need_c     = Wire(Bool())
  val lzc_diff_pre_1 = Wire(UInt((lzc_w+1).W)) // pre_1 stage lzc_diff
//  norm_d_err_top := w_iter_reg(0)(15)
//  norm_x_err_top := w_iter_reg(0)(14)
  zero_x_pre_1 := w_iter_reg(0)(13)
  lzc_x_1_0 := w_iter_reg(0)(12,11)
  zero_d_pre_1 := w_iter_reg(0)(10)
  lzc_d_1_0 := w_iter_reg(0)(8,7)
  lzc_diff_pre_1 := w_iter_reg(0)(6,0)
 // lzc_diff_err := w_iter_reg(0)(6,0)
  //two stage shift for normalization

  val norm_x = iter_q_A_reg << lzc_x_1_0
  val norm_d = iter_q_B_reg << lzc_d_1_0
//  val norm_x_err = iter_q_A_reg << lzc_x_1_0
//  val norm_d_err = iter_q_B_reg << lzc_d_1_0
//  x_need_c := !norm_x_err(bit_width-1)
//  d_need_c := !norm_d_err(bit_width-1)
//  lzc_diff_pre_1 := Mux(x_need_c,Mux(d_need_c, lzc_diff_err, lzc_diff_err + 1.U(bit_width.W)),Mux(d_need_c, lzc_diff_err - 1.U(bit_width.W), lzc_diff_err) )
//  val norm_x = Mux(x_need_c, Cat(norm_x_err_top,norm_x_err(bit_width-1, 1)), norm_x_err)
//  val norm_d = Mux(d_need_c, Cat(norm_d_err_top,norm_d_err(bit_width-1, 1)), norm_d_err)
  //val norm_d_reg = RegEnable(norm_d,stateReg(pre_1))

  // special case
  val x_small = lzc_diff_pre_1(lzc_w) // x smaller then d
  val d_zero = zero_d_reg // d is zero
  val d_one = w_iter_reg(0)(9)
  early_finish := x_small|d_zero|d_one
  val early_finish_q = RegEnable(early_finish, stateReg(pre_1))

  val early_q = Mux(x_small, 0.U,
                Mux(d_zero,Fill(bit_width,1.U),
                  abs_x_reg)) // error when d is one abs special case quotatient
  val early_rem = Mux(x_small|d_zero,abs_x_reg,0.U) // special case rem

  // bypass for not r shift at post stage
  val r_shift_num = Wire(UInt(lzc_w.W)) // bypass x rshift for lzc_diff bit to align with d
  val r_shift_hb = UIntToOH(lzc_diff_pre_1(1,0),4) // bypass x or norm x rshift to align0/1/2/3 and get the correct quotation
  val Bypass_abs_x = Wire(UInt(Bypass_width.W)) // bypass x after rshit
  val Bypass_abs_x_h = Wire(UInt(bit_width.W)) // bypass x msb
  val Bypass_abs_x_l = Wire(UInt(bit_width.W)) // bypass x lsb
  val Bypass_align_x_h = Wire(UInt(bit_width.W)) // bypass x msb after align
  val Bypass_align_x_l = Wire(UInt(bit_width.W)) // bypass x lsb after align
  val Bypass_align_w_init = Wire(Vec(2,UInt(bit_width.W))) // Redundant representation ，bypass_align_x_h
  val Bypass_align_w_l = Wire(UInt(bit_width.W))
  val Bypass_align_w_l_reg = RegEnable(Bypass_align_w_l,stateReg(pre_1)|stateReg(iter))
  r_shift_num := Mux(early_finish, 0.U, lzc_diff_pre_1) // error not lzc_dif
  Bypass_abs_x := Cat(abs_x_reg,0.U(bit_width.W)) >> r_shift_num
  // init with /4
  Bypass_abs_x_h := Bypass_abs_x(Bypass_width - 1, bit_width)
  Bypass_abs_x_l := Bypass_abs_x(bit_width -  1, 0)
  Bypass_align_x_h :=
    Mux(r_shift_hb(0),Cat(0.U(4.W),Bypass_abs_x_h(bit_width - 1, 4)),
      Mux(r_shift_hb(1),Cat(0.U(3.W),Bypass_abs_x_h(bit_width - 1, 3)),
        Mux(r_shift_hb(2),Cat(0.U(2.W),Bypass_abs_x_h(bit_width - 1, 2)),
          Cat(0.U(5.W),Bypass_abs_x_h(bit_width - 1, 5)))))
  Bypass_align_x_l :=
    Mux(r_shift_hb(0), Cat(Bypass_abs_x_h(3,0), Bypass_abs_x_l(bit_width - 1, 4)),
      Mux(r_shift_hb(1), Cat(Bypass_abs_x_h(2,0), Bypass_abs_x_l(bit_width - 1, 3)),
        Mux(r_shift_hb(2), Cat(Bypass_abs_x_h(1,0),Bypass_abs_x_l(bit_width - 1, 2)),
          Cat(Bypass_abs_x_h(4,0), Bypass_abs_x_l(bit_width - 1, 5)))))
  
  Bypass_align_w_init := VecInit(
    Mux(early_finish, early_rem, Bypass_align_x_h),
    0.U
  )
  Bypass_align_w_l := Mux(stateReg(pre_1), Bypass_align_x_l, Bypass_align_w_l_reg << 4)





  // iter num 
  val iter_num = Wire(UInt((lzc_w-2).W))
  val init_iter = Wire(UInt((lzc_w-2).W))
  val iter_num_reg = RegEnable(iter_num, stateReg(pre_1)|stateReg(iter))
  init_iter := (lzc_diff_pre_1 >>2).asUInt + (lzc_diff_pre_1(1,0).andR).asUInt
  iter_num := Mux(stateReg(pre_1),init_iter,iter_num_reg -% 1.U)
  iter_finish := iter_num_reg === 0.U

  // r shift  for align
  val w_align_init = Wire(Vec(2,UInt(w_width.W))) // sum/carry
  w_align_init(0) := Cat(0.U(3.W),// init with dividend / 4 and first bit is sign for 0, decimal is between w_width - 1 and w_width -2
    Mux(r_shift_hb(0),Cat(0.U(2.W),norm_x,0.U(1.W)),
      Mux(r_shift_hb(1),Cat(0.U(1.W),norm_x,0.U(2.W)),
        Mux(r_shift_hb(2),Cat(norm_x,0.U(3.W)),
          Cat(0.U(3.W),norm_x)))))
  w_align_init(1) := 0.U

  // choose the first q
  // search table for q 1 different from other
  val pos_1_lookUpTale_q1 = VecInit(
    "b00100".U(5.W),
    "b00100".U(5.W),
    "b00100".U(5.W),
    "b00100".U(5.W),
    "b00110".U(5.W),
    "b00110".U(5.W),
    "b00110".U(5.W),
    "b00110".U(5.W)
  )
  // *4 to chose 1st quo
  val m_pos1_q1 = pos_1_lookUpTale_q1(norm_d(bit_width - 2, bit_width - 4))
  val pos_2_lookUpTale_q1 = VecInit(
    "b01100".U(5.W),
    "b01110".U(5.W),
    "b10000".U(5.W),
    "b10000".U(5.W),
    "b10010".U(5.W),
    "b10100".U(5.W),
    "b10110".U(5.W),
    "b10110".U(5.W)
  )
  val m_pos2_q1 = pos_2_lookUpTale_q1(norm_d(bit_width - 2, bit_width - 4))

  val w_trunc_1_4 = Wire(UInt(5.W)) // 1+4 for first quo selection
  w_trunc_1_4 := Cat(0.U(1.W), w_align_init(0)(w_width - 4, w_width - 7)) // *4  for trunc err 4
  val cmp_pos_1 = w_trunc_1_4 >= m_pos1_q1
  val cmp_pos_2 = w_trunc_1_4 >= m_pos2_q1
  val q_1 = Cat(0.U(2.W),//-2 -1
    (!cmp_pos_1) & (!cmp_pos_2), //0
    (cmp_pos_1)  & (!cmp_pos_2),// 1
    cmp_pos_2)//2  q_1 the first quo with one-hot representation, pre caculate as the q_j
//  val tmp_q_A = Wire(UInt(bit_width.W))
//  val conv_pre1 = Module(new Conversion(bit_width)).suggestName("conversion for pre1")
//  conv_pre1.io.q_j_1 := q_1
//  conv_pre1.io.pre_q_B := 0.U
//  conv_pre1.io.pre_q_A := 0.U
//  tmp_q_A  := conv_pre1.io.nxt_q_A
  init_q_B := 0.U
  init_q_A := Mux(early_finish, early_q,0.U) // Note assigning 0 here because each time a q is calculated in advance, it is necessary to delay one conversion to convert the unique code to real q

  //selection const
  val neg_Vec_m = Wire(Vec(4,UInt(9.W))) // -mk vec
  val qds_cons = Module(new SRT4qdsCons)
  qds_cons.io.d_trunc_3 := norm_d(bit_width - 2, bit_width - 4)
  neg_Vec_m(0) := qds_cons.io.m_neg_1
  neg_Vec_m(1) := qds_cons.io.m_neg_0
  neg_Vec_m(2) := qds_cons.io.m_pos_1
  neg_Vec_m(3) := qds_cons.io.m_pos_2

  // caculate constant
  val d_ext = Cat(0.U(1.W),norm_d) // norm_d ext 1bit for sign
  val neg_d_ext = -d_ext
  val neg_abs_d = -abs_d_reg
  val iter_cons = Wire(Vec(5,UInt(w_width.W)))  //ud for neg u u= -2 -1 0 1 2
  val iter_cons_reg = RegEnable(iter_cons,stateReg(pre_1))
  val Bypass_cons = Wire(Vec(5,UInt(bit_width.W))) //ud note this d is not normalized
  val Bypass_cons_reg = RegEnable(Bypass_cons,stateReg(pre_1))
  val rud = Wire(Vec(5,UInt(9.W)))  // 4*ud 3+6
  val r2ud = Wire(Vec(5,UInt(9.W))) // 16*ud 3+6
  val d_trunc = Wire(Vec(4,UInt(9.W))) // 4*ud , rud exclude u = 0 (can be omit ?)
  val d_trunc_reg = RegEnable(d_trunc, stateReg(pre_1))
  val sel_cons = Wire(Vec(5,Vec(4,UInt(8.W)))) // 4ud-mk for neg u u = -2 -1 0 1 2  k = -1 0 1 2 3+5 sel block
  val sel_cons_reg = RegEnable(sel_cons,stateReg(pre_1))
  val spec_cons = Wire(Vec(5,Vec(4,UInt(9.W)))) // 16ud-mk 3+6 spec block
  val spec_cons_reg = RegEnable(spec_cons,stateReg(pre_1))

  iter_cons := VecInit(  // 000.xxxx err3 
    Cat(SignExt(neg_d_ext,bit_width+2), 0.U(4.W)),
    Cat(SignExt(neg_d_ext,bit_width+3), 0.U(3.W)),
    0.U(w_width.W),
    Cat(SignExt(d_ext,bit_width+3),0.U(3.W)),
    Cat(SignExt(d_ext,bit_width+2),0.U(4.W))
  )
  Bypass_cons := VecInit(
    Cat(neg_abs_d(bit_width - 2, 0),0.U(1.W)),
    neg_abs_d,
    0.U(bit_width.W),
    abs_d_reg,
    Cat(abs_d_reg(bit_width - 2,0),0.U(1.W))
  )
  // 000xx.xxx
  rud := VecInit(Seq.tabulate(5){i => iter_cons(i)(w_width-3,w_width-11)})
  d_trunc := VecInit(
    iter_cons(0)(w_width-3,w_width-11),
    iter_cons(1)(w_width-3,w_width-11),
    iter_cons(3)(w_width-3,w_width-11),
    iter_cons(4)(w_width-3,w_width-11)
  )
  // 000xxxx.x
  r2ud := VecInit(Seq.tabulate(5){i => iter_cons(i)(w_width - 5, w_width - 13)})

  sel_cons := VecInit(Seq.tabulate(5) { i =>
    VecInit(
      Seq.tabulate(4){j => rud(i)(8,1) + neg_Vec_m(j)(8,1)}
    )
  })
    spec_cons := VecInit(Seq.tabulate(5) { i =>
      VecInit(
        Seq.tabulate(4){j => r2ud(i) + neg_Vec_m(j)}
      )
    })

  // iter stage





  w_iter := VecInit(Seq.tabulate(2){i => Mux(stateReg(pre_0),ZeroExt(lzc_pre0_all,bit_width),Mux(stateReg(pre_1), w_align_init(i),nxt_w(i)))})
  w_iter_mul16_trunc_init := VecInit(Seq.tabulate(2){i => (w_align_init(i) << 2)(w_width - 1, w_width -8)}) // x.xxxx xxx.xxxx   lshift2 xx|xxx.xxx
  w_iter_mul64_trunc_init := VecInit(Seq.tabulate(2){i => (w_align_init(i) << 4)(w_width - 1, w_width -9)}) // error just << 4 not 6 //trunc width err
  w_iter_mul16_trunc := Mux(stateReg(pre_1), w_iter_mul16_trunc_init, w_iter_mul16_trunc_iter)
  w_iter_mul64_trunc := Mux(stateReg(pre_1), w_iter_mul64_trunc_init, w_iter_mul64_trunc_iter)
  // q for iter
  val nxt_q_A = Wire(UInt(bit_width.W))
  val nxt_q_B = Wire(UInt(bit_width.W))
  val nxt_q = Wire(UInt(5.W))
  val iter_q = Wire(UInt(5.W))
  val iter_q_reg = RegEnable(iter_q,stateReg(pre_1)|stateReg(iter)) // this store for pre q with one-hot format (retiming)

  iter_q := Mux(stateReg(pre_1), q_1, nxt_q)
  iter_q_A := Mux(stateReg(pre_0), norm_x_part(bit_width - 1, 0),Mux(stateReg(pre_1),init_q_A, nxt_q_A))
  iter_q_B := Mux(stateReg(pre_0), norm_d_part(bit_width - 1, 0),Mux(stateReg(pre_1),init_q_B, nxt_q_B)) // error pre_0 pre_1 mismatch

  // iterblock
  val IterBlock = Module(new IterBlock_v2(bit_width, w_width))
  IterBlock.io.Sel_cons := sel_cons_reg
  IterBlock.io.Spec_cons := spec_cons_reg
  IterBlock.io.iter_cons := iter_cons_reg
  IterBlock.io.d_trunc := d_trunc_reg
  IterBlock.io.q_j := iter_q_reg
  nxt_q := IterBlock.io.q_j_2
  IterBlock.io.pre_w := w_iter_reg
  IterBlock.io.pre_w_mul16_trunc := w_iter_mul16_trunc_reg
  IterBlock.io.pre_w_mul64_trunc := w_iter_mul64_trunc_reg
  nxt_w := IterBlock.io.nxt_w
  w_iter_mul16_trunc_iter := IterBlock.io.nxt_w_mul16_trunc
  w_iter_mul64_trunc_iter := IterBlock.io.nxt_w_mul64_trunc
  IterBlock.io.pre_q_A := iter_q_A_reg
  IterBlock.io.pre_q_B := iter_q_B_reg
  nxt_q_A := IterBlock.io.nxt_q_A
  nxt_q_B := IterBlock.io.nxt_q_B

  // Bypass 
  val Bypass_nxt_w = Wire(Vec(2,UInt(bit_width.W)))
  val Bypass_iter = Wire(Vec(2, UInt(bit_width.W)))
  val Bypass_iter_reg = RegEnable(Bypass_iter, stateReg(pre_1) |stateReg(iter))
  Bypass_iter := Mux(stateReg(pre_1), Bypass_align_w_init, Bypass_nxt_w)
  IterBlock.io.pre_Bypass_w := Bypass_iter_reg
  Bypass_nxt_w := IterBlock.io.nxt_Bypass_w
  IterBlock.io.Bypass_cons := Bypass_cons_reg
  IterBlock.io.w_Bypass_l := Bypass_align_w_l_reg
  
  // post stage
  val q_A_sign_c = Wire(UInt(bit_width.W)) // q after sign adjust
  val q_B_sign_c = Wire(UInt(bit_width.W)) // q-1 after sign adjust
  val Bypass_final_rem = Wire(UInt(bit_width.W)) // bypass rem after sign adjust
  val Bypass_final_rem_plus_d = Wire(UInt(bit_width.W)) // bypass rem after sign adjust and plus d
  q_A_sign_c := Mux(q_sign_reg && !zero_d_reg, neg_x_q, iter_q_A_reg)
  q_B_sign_c := Mux(q_sign_reg && !zero_d_reg, neg_d_q, iter_q_B_reg)
  when(x_sign_reg ) { // error ?? early finish init with abs_reg
    Bypass_final_rem := (~Bypass_iter_reg(0)).asUInt + (~Bypass_iter_reg(1)).asUInt + 2.U // reverse every bit and add 1 for neg 
    Bypass_final_rem_plus_d := (~Bypass_iter_reg(0)).asUInt + (~Bypass_iter_reg(1)).asUInt + neg_abs_d + 2.U
  } .otherwise {
    Bypass_final_rem := Bypass_iter_reg(0) + Bypass_iter_reg(1)
    Bypass_final_rem_plus_d := Bypass_iter_reg(0) + Bypass_iter_reg(1) + abs_d_reg
  }
  // zero detect
//  val p_sum = Wire(UInt((bit_width-1).W))
//  val p_carry = Wire(UInt((bit_width-1).W))
//  p_sum := Bypass_iter_reg(0)(bit_width - 1, 1) ^ Bypass_iter_reg(1)(bit_width - 1, 1)
//  p_carry := Bypass_iter_reg(0)(bit_width - 2, 0) | Bypass_iter_reg(1)(bit_width - 2, 0)

//  val Bypass_rem_is_zero = p_sum === p_carry
  val Bypass_rem_is_zero = !(Bypass_final_rem.orR()) // error : when ffffffff + 0
  val adjust = Mux(x_sign_reg, (~Bypass_rem_is_zero).asBool & (~Bypass_final_rem(bit_width-1)).asBool, Bypass_final_rem(bit_width-1)) & !early_finish_q // rem need to adjust
  val out_q_final =
      Mux(adjust, q_B_sign_c, q_A_sign_c)
  val out_q_final_reg = RegEnable(out_q_final, stateReg(post))
  val out_rem_final =
      Mux(adjust,Bypass_final_rem_plus_d, Bypass_final_rem)
  val out_rem_final_reg = RegEnable(out_rem_final, stateReg(post))
  io.div_out_q := out_q_final_reg
  io.div_out_rem := out_rem_final_reg
  io.d_zero := zero_d_reg


  // zero detection
//  val ps = Wire(UInt(bit_width.W))
//  val pc = Wire(UInt(bit_width.W))


}

class IterBlock_v2(bit_width: Int, w_width: Int) extends Module {
  val io = IO(new Bundle() {
    val Sel_cons = Vec(5,Vec(4,Input(UInt(8.W)))) // 4ud - mk for selection block
    val Spec_cons = Vec(5, Vec(4, Input(UInt(9.W)))) // 4^2ud -mk
    val iter_cons = Vec(5, Input(UInt(w_width.W)))  // neg ud
    val d_trunc = Vec(4,Input(UInt(9.W))) // 4ud for -2 -1 1 2
    val q_j = Input(UInt(5.W))  //one-hot
    val q_j_2 = Output(UInt(5.W))   //one -hot
    val pre_w = Vec(2,Input(UInt(w_width.W))) // sum/carry // x.xxxxx
    val pre_w_mul16_trunc = Vec(2,Input(UInt(8.W))) //
    val pre_w_mul64_trunc = Vec(2,Input(UInt(9.W)))
    val nxt_w = Vec(2,Output(UInt(w_width.W)))
    val nxt_w_mul16_trunc = Vec(2,Output(UInt(8.W)))
    val nxt_w_mul64_trunc = Vec(2,Output(UInt(9.W)))
    val pre_q_A = Input(UInt(bit_width.W))
    val pre_q_B = Input(UInt(bit_width.W))
    val nxt_q_A = Output(UInt(bit_width.W)) // on-the-fly conversion
    val nxt_q_B = Output(UInt(bit_width.W))

    // Bypass
    val pre_Bypass_w = Vec(2,Input(UInt(bit_width.W)))
    val nxt_Bypass_w = Vec(2,Output(UInt(bit_width.W)))
    val Bypass_cons = Vec(5,Input(UInt(bit_width.W)))
    val w_Bypass_l = Input(UInt(bit_width.W))

  })
  val pre_w_mul4 = Wire(Vec(2,UInt(w_width.W)))
  pre_w_mul4 := VecInit(Seq.tabulate(2){i => io.pre_w(i)}) // xxx.xxxxx
  val negq_d = Wire(UInt(w_width.W))
  negq_d := Mux1H(io.q_j, io.iter_cons.toSeq )
  val w_j = Wire(Vec(2,UInt(w_width.W)))
  val csa = Module(new CSA3_2(w_width)).suggestName("csa_iter_1")
  csa.io.in(0) := pre_w_mul4(0)
  csa.io.in(1) := pre_w_mul4(1)
  csa.io.in(2) := negq_d // xxx.xxxxx
  w_j := VecInit(Seq.tabulate(2){i => csa.io.out(i)}) // xxx.xxxx

  // selection block
  val q_j_1 = Wire(UInt(5.W))
  val selB = Module(new SelBlock_v2(8))
  selB.io.q_j := io.q_j
  selB.io.cons := io.Sel_cons
  selB.io.rem_3_5 := io.pre_w_mul16_trunc
  q_j_1 := selB.io.q_j_1

  // speculative block
  val specB = Module(new SpecBlock_v2)
  specB.io.q_j := io.q_j
  specB.io.q_j_1 := q_j_1
  specB.io.d_trunc := io.d_trunc
  specB.io.cons := io.Spec_cons
  specB.io.rem_3_5 := io.pre_w_mul64_trunc
  io.q_j_2 := specB.io.q_j_2

  val w_mul4 = Wire(Vec(2,UInt(w_width.W)))
  w_mul4(0) := w_j(0)<<2 //xxx.xxxx
  w_mul4(1) := (w_j(1)<<1)(w_width -1, 0) <<2 //xxx.xxxx
  val negq1_d = Wire(UInt(w_width.W))
  negq1_d := Mux1H(q_j_1, io.iter_cons.toSeq)
  val csa_2 = Module(new CSA3_2(w_width)).suggestName("csa_iter_2")
  csa_2.io.in(0) := w_mul4(0)
  csa_2.io.in(1) := w_mul4(1)
  csa_2.io.in(2) := negq1_d // out xxx.xxxx
  val nxt_w_o = Wire(Vec(2,UInt(w_width.W)))
  nxt_w_o(0) := csa_2.io.out(0)
  nxt_w_o(1) := (csa_2.io.out(1) << 1)(w_width - 1, 0)
  io.nxt_w := VecInit(Seq.tabulate(2){i => nxt_w_o(i) << 2}) // xxx.xx after mul 4
  io.nxt_w_mul16_trunc := VecInit(Seq.tabulate(2){i => (nxt_w_o(i) << 4)(w_width-1, w_width-8)}) // xxx.xxxx  xxxx|xxx.xxxneed to check ? // error rshift bit
  io.nxt_w_mul64_trunc := VecInit(Seq.tabulate(2){i => (nxt_w_o(i) << 6)(w_width-1, w_width-9)}) // xxx.xxxx  xxxxxx|xxx.xxxxThe decimal point is still between rem_ w -1 and rem_ w-2
  // to do

  // on-the-fly conversion
  val conv_iter = Module(new Conversion(bit_width)).suggestName("iter_conv")
  val nxt1_q_A = Wire(UInt(bit_width.W)) // on-fly conversion
  val nxt1_q_B = Wire(UInt(bit_width.W))
  conv_iter.io.q_j_1 := io.q_j
  conv_iter.io.pre_q_B := io.pre_q_B
  conv_iter.io.pre_q_A := io.pre_q_A
  nxt1_q_A := conv_iter.io.nxt_q_A
  nxt1_q_B := conv_iter.io.nxt_q_B

  val conv_iter_2 = Module(new Conversion(bit_width)).suggestName("iter_conv_2")
  conv_iter_2.io.q_j_1 := q_j_1
  conv_iter_2.io.pre_q_A := nxt1_q_A
  conv_iter_2.io.pre_q_B := nxt1_q_B
  io.nxt_q_A := conv_iter_2.io.nxt_q_A
  io.nxt_q_B := conv_iter_2.io.nxt_q_B

  //Bypass
  val pre_Bypass_w_mul4 = Wire(Vec(2,UInt(bit_width.W)))
  pre_Bypass_w_mul4 := VecInit(
    Cat(io.pre_Bypass_w(0)(bit_width - 3, 0), io.w_Bypass_l(bit_width - 1, bit_width - 2)),
    io.pre_Bypass_w(1)<<2
  )
  val negq_Bypass_d = Wire(UInt(bit_width.W))
  negq_Bypass_d := Mux1H(io.q_j, io.Bypass_cons.toSeq)
  val Bypass_w_j = Wire(Vec(2,UInt(bit_width.W)))
  val csa_3 = Module(new CSA3_2(bit_width)).suggestName("csa_bypass_1")
  csa_3.io.in(0) := pre_Bypass_w_mul4(0)
  csa_3.io.in(1) := pre_Bypass_w_mul4(1)
  csa_3.io.in(2) := negq_Bypass_d
  Bypass_w_j := VecInit(Seq.tabulate(2){i => csa_3.io.out(i)})

  val Bypass_w_j_mul4 = Wire(Vec(2,UInt(bit_width.W)))
  Bypass_w_j_mul4 := VecInit(
    Cat(Bypass_w_j(0)(bit_width - 3, 0), io.w_Bypass_l(bit_width - 3, bit_width - 4)),
    ((Bypass_w_j(1) << 1)(bit_width - 1, 0) <<2)(bit_width - 1, 0)
  )
  val negq1_Bypass_d = Wire(UInt(bit_width.W))
  negq1_Bypass_d := Mux1H(q_j_1, io.Bypass_cons.toSeq)
  val csa_4 = Module(new CSA3_2(bit_width)).suggestName("csa_bypass_2")
  csa_4.io.in(0) := Bypass_w_j_mul4(0)
  csa_4.io.in(1) := Bypass_w_j_mul4(1)
  csa_4.io.in(2) := negq1_Bypass_d
  io.nxt_Bypass_w(0) := csa_4.io.out(0)
  io.nxt_Bypass_w(1) := csa_4.io.out(1) << 1

}


class SelBlock_v2(iter_w :Int) extends Module {
  val io = IO(new Bundle() {
    val q_j = Input(UInt(5.W))   // attention reverse 2 1 0 -1 -2
    val q_j_1 = Output(UInt(5.W))
    val cons = Vec(5,Vec(4,Input(UInt(iter_w.W))))    // 4ud-mk  for u =  {-2,-1,0,1,2} k = {-1 0 1 2}(after trunc)
    val rem_3_5 = Vec(2,Input(UInt(iter_w.W))) // sum/carry
  })

//  val rem_2_5 = Wire(Vec(2, UInt(7.W)))
//  val rem_3_4 = Wire(Vec(2, UInt(7.W)))
//  rem_2_5 := VecInit(Seq.tabulate(2) { i => io.rem_3_5(i)(6, 0) })
//  rem_3_4 := VecInit(Seq.tabulate(2) { i => io.rem_3_5(i)(7, 1) })
//  val rem_array = Wire(Vec(4,Vec(2,UInt(7.W))))
//  rem_array(0) := rem_2_5  // k = -1
//  rem_array(1) := rem_3_4  // k = 0
//  rem_array(2) := rem_3_4  // k = 1
//  rem_array(3) := rem_2_5  // k = 2
  val rem_array = Wire(Vec(4,Vec(2,UInt(iter_w.W))))
  rem_array := VecInit(Seq.tabulate(4) {i => io.rem_3_5})

  val cons_u = Wire(Vec(4,UInt(iter_w.W)))
  cons_u := Mux1H(io.q_j,io.cons.toSeq) // attention -q_j select for 4ud-mk
//  val cons_u_array = Wire(Vec(4,UInt(7.W)))
//  cons_u_array(0) := cons_u(0)(6,0)
//  cons_u_array(1) := cons_u(1)(7,1)
//  cons_u_array(2) := cons_u(2)(7,1)
//  cons_u_array(3) := cons_u(3)(6,0)
  val sign = Wire(Vec(4,Bool()))
  for (i <-0 until 4) {
//    val csa_sel = Module(new CSA3_2(7)).suggestName(s"csa_sel_${i}")
//    csa_sel.io.in(0) := rem_array(i)(0)
//    csa_sel.io.in(1) := rem_array(i)(1)
//    csa_sel.io.in(2) := cons_u_array(i)
    val csa_sel = Module(new CSA3_2(iter_w)).suggestName(s"csa_sel_${i}")
    csa_sel.io.in(0) := rem_array(i)(0)
    csa_sel.io.in(1) := rem_array(i)(1)
    csa_sel.io.in(2) := cons_u(i)
    sign(i) := (csa_sel.io.out(0) + (csa_sel.io.out(1) << 1))(iter_w -1) // attention csa carry need to left shift 1bit
  }
//  sign = VecInit(Seq.tabulate(4){ i =>
//    val csa_sel = Module(new CSA3_2(7))
//    csa_sel.io.in(0) := rem_array(i)(0)
//    csa_sel.io.in(1) := rem_array(i)(1)
//    csa_sel.io.in(2) := cons_u(i)
//
//    (csa_sel.io.out(0) + (csa_sel.io.out(1) << 1))(6)
//  }) //?
  val SD_sel = Module(new SignDec)
  SD_sel.io.sign := sign
  io.q_j_1 := SD_sel.io.q

}

class SpecBlock_v2 extends Module {
  val io = IO(new Bundle(){
    val q_j = Input(UInt(5.W))
    val cons = Vec(5, Vec(4, Input(UInt(9.W)))) // 4^2ud - mk after trunc
    val d_trunc = Vec(4,Input(UInt(9.W))) // u4d  u for -2 -1 1 2 trunc 3_5
    val q_j_1 = Input(UInt(5.W))
    val rem_3_5 = Vec(2, Input(UInt(9.W))) // attention rem_carry need to left shift before this module
    val q_j_2 = Output(UInt(5.W))
  })
  //val cons_u = Wire(Vec(4,UInt(7.W))) // k for -1 0 1 2
  val temp = Wire(Vec(5,Vec(2,UInt(9.W)))) // u for -2 -1 1 2 0
//  val sign = Wire(Vec(5,Vec(4,Bool())))
  val q_j_2_v = Wire(Vec(5,UInt(5.W)))
//  cons_u := Mux1H(io.q_j,io.cons.toSeq)
  temp(4) := io.rem_3_5  // 注意 0d in fifth place need to adjust
  for (i <-0 until 4) {
    val csa_spec_1 = Module(new CSA3_2(9)).suggestName(s"csa_spec_1_${i}")
    csa_spec_1.io.in(0) := io.rem_3_5(0)
    csa_spec_1.io.in(1) := io.rem_3_5(1)
    csa_spec_1.io.in(2) := io.d_trunc(i)
    temp(i)(0) := csa_spec_1.io.out(0)
    temp(i)(1) := csa_spec_1.io.out(1) << 1 // left shift for csa carry
  }
  val temp_2 = Wire(Vec(5,Vec(2,UInt(9.W))))
  temp_2 := VecInit(
    VecInit(Seq.tabulate(2){i => temp(0)(i)}),
    VecInit(Seq.tabulate(2){i => temp(1)(i)}),
    VecInit(Seq.tabulate(2){i => temp(4)(i)}),
    VecInit(Seq.tabulate(2){i => temp(2)(i)}),
    VecInit(Seq.tabulate(2){i => temp(3)(i)})

  )
  for (i <-0 until 5) {
//    val rem_array = Wire(Vec(4,Vec(2,UInt(7.W))))
//    rem_array(0) := VecInit(Seq.tabulate(2) { k => temp(i)(k)(6,0) }) // k = -1
//    rem_array(1) := VecInit(Seq.tabulate(2) { k => temp(i)(k)(7,1) }) // k = 0
//    rem_array(2) := VecInit(Seq.tabulate(2) { k => temp(i)(k)(7,1) }) // k = 1
//    rem_array(3) := VecInit(Seq.tabulate(2) { k => temp(i)(k)(6,0) }) // k = 2
//    for (j <-0 until 4) {
      val sel_spec = Module(new SelBlock_v2(9)).suggestName(s"sel_spec_${i}")
      sel_spec.io.q_j := io.q_j
      sel_spec.io.cons := io.cons
      sel_spec.io.rem_3_5 := temp_2(i)
//      sign(i) := sel_spec.io.q_j_1
//      val csa_spec_2 = Module(new CSA3_2(7))
//      csa_spec_2.io.in(0) := rem_array(j)(0)
//      csa_spec_2.io.in(1) := rem_array(j)(1)
//      csa_spec_2.io.in(2) := cons_u(j)
//      sign(i)(j) := (csa_spec_2.io.out(0) + (csa_spec_2.io.out(1) << 1))(6)
//    }
//    val SD_spec = Module(new SignDec).suggestName(s"SD_spec_${i}")
//    SD_spec.io.sign := sign(i)
    q_j_2_v(i) := sel_spec.io.q_j_1
  }
  io.q_j_2 := Mux1H(io.q_j_1,q_j_2_v.toSeq)

}
class Conversion(bit_width: Int) extends Module {
  val io = IO(new Bundle(){
    val q_j_1 = Input(UInt(5.W))
    val pre_q_A = Input(UInt(bit_width.W))
    val pre_q_B = Input(UInt(bit_width.W))
    val nxt_q_A = Output(UInt(bit_width.W)) // on-the-fly conversion
    val nxt_q_B = Output(UInt(bit_width.W))
  })
  val tmp_q_A = Wire(UInt((bit_width-2).W))
  val tmp_q_B = Wire(UInt((bit_width-2).W))
  tmp_q_A := (io.pre_q_A << 2)(bit_width - 1, 2)
  tmp_q_B := (io.pre_q_B << 2)(bit_width - 1, 2)
  val new_q_A = VecInit(
    Cat(tmp_q_A, 2.U(2.W)), // q = 2
    Cat(tmp_q_A, 1.U(2.W)),
    Cat(tmp_q_A, 0.U(2.W)),
    Cat(tmp_q_B, 3.U(2.W)),
    Cat(tmp_q_B, 2.U(2.W)), // q = 2
  )
  val new_q_B = VecInit(
    Cat(tmp_q_A, 1.U(2.W)), // q = 2
    Cat(tmp_q_A, 0.U(2.W)),
    Cat(tmp_q_B, 3.U(2.W)),
    Cat(tmp_q_B, 2.U(2.W)),
    Cat(tmp_q_B, 1.U(2.W))
  )

  io.nxt_q_A := Mux1H(io.q_j_1, new_q_A.toSeq)
  io.nxt_q_B := Mux1H(io.q_j_1, new_q_B.toSeq)
}
class Lzc_vary(bit_width:Int) extends Module {
  val io = IO(new Bundle() {
    val X = Input(UInt(bit_width.W))
    val Z = Output(UInt(log2Up(bit_width).W))
    val V = Output(Bool())
  })
  val Z_neg = Wire(Vec(log2Up(bit_width),Bool()))
  val V_neg = Wire(Bool())
  val len = bit_width.U
  val (lzc_64,lzc_32,lzc_16,lzc_8) =
    (64.U,32.U,16.U,8.U)
  //  switch (len) {
  //    is (lzc_64) {
  val lzc = Module(new Lzc(bit_width)).suggestName("lzc_64")
  lzc.io.X := io.X
  Z_neg := lzc.io.Z_neg
  V_neg := lzc.io.V_neg
  //    }
  //    is (lzc_32) {
  //      val lzc = Module(new Lzc32).suggestName("lzc_32")
  //      lzc.io.X := io.X
  //      Z_neg := lzc.io.Z_neg
  //      V_neg := lzc.io.V_neg
  //    }
  //    is (lzc_16) {
  //      val lzc = Module(new Lzc16).suggestName("lzc_32")
  //      lzc.io.X := io.X
  //      Z_neg := lzc.io.Z_neg
  //      V_neg := lzc.io.V_neg
  //    }
  //    is(lzc_8) {
  //      val lzc = Module(new Lzc16).suggestName("lzc_32")
  //      lzc.io.X := io.X
  //      Z_neg := lzc.io.Z_neg
  //      V_neg := lzc.io.V_neg
  //    }
  //  }
  val temp = VecInit(Z_neg.map(!_))
  io.Z := temp.asUInt
  io.V := !V_neg
}

class Lzc(bit_width: Int) extends Module {
  val io = IO(new Bundle() {
    val X = Input(UInt(bit_width.W))
    val Z_neg = Vec(log2Up(bit_width),Output(Bool()))
    val V_neg = Output(Bool())
  })
  val lzc32_1 = Module(new Lzc32)
  val lzc32_2 = Module(new Lzc32)
  val Z_low = Wire(Vec(5, Bool()))
  val Z_high = Wire(Vec(5, Bool()))
  lzc32_1.io.X := io.X(31, 0)
  lzc32_2.io.X := io.X(61, 32)
  Z_low := lzc32_1.io.Z_neg
  Z_high := lzc32_2.io.Z_neg
  val V_low = lzc32_1.io.V_neg
  val V_high = lzc32_2.io.V_neg
  val overlap = Module(new Lzclap(64))
  overlap.io.V_high := V_high
  overlap.io.V_low := V_low
  overlap.io.Z_low := Z_low
  overlap.io.Z_high := Z_high
  //  val temp = VecInit(overlap.io.Z_neg.map(!_))
  /*  temp.asUInt
  //  io.v := !(overlap.io.V_neg)*/
  io.Z_neg := overlap.io.Z_neg
  io.V_neg := overlap.io.V_neg
}
class Lzc8 extends Module {
  val io = IO(new Bundle() {
    val X = Input(UInt(8.W))
    val Z_neg = Vec(3,Output(Bool()))
    val V_neg = Output(Bool())
  })
  val x = io.X
  io.V_neg := !(!(x(7) || x(6)) && !(x(5) || x(4)) && !(x(3) || x(2)) && !(x(1) || x(0)))
  io.Z_neg(2) := !(!(x(7) || x(6)) && !(x(5) || x(4)))
  io.Z_neg(1) := !(!(x(6) || x(5)) && ((x(4) || x(3)) || !(x(2) || x(1))))
  io.Z_neg(0) := !(!(x(7) || (!(x(4)) && x(5) )) && (x(6) || x(4) || !(x(3) || (!(x(2)) && x(1)))))
}

class Lzclap(bit_width: Int) extends Module {
  val io = IO(new Bundle() {
    val Z_low = Vec(log2Up(bit_width)-1, Input(Bool()))
    val Z_high =Vec(log2Up(bit_width)-1, Input(Bool()))
    val V_low = Input(Bool())
    val V_high = Input(Bool())
    val V_neg = Output(Bool())
    val Z_neg = Vec(log2Up(bit_width),Output(Bool()))
  })
  io.Z_neg(log2Up(bit_width)-1) := io.V_high
  io.V_neg := io.V_low || io.V_high
  val V_vec = Wire(Vec(log2Up(bit_width)-1, Bool()))
  val tmp = Wire(Vec(log2Up(bit_width)-1, Bool()))
  for (i <-0 until (log2Up(bit_width)-1)) {
    V_vec(i) := !io.V_high
    tmp(i) := V_vec(i) && io.Z_low(i)
    io.Z_neg(i) := io.Z_high(i) || tmp(i)
  }


  //  val V_vec = VecInit(Seq.fill(log2Up(bit_width)-1)(!io.V_high))
  //
  //  val tmp = (V_vec, io.Z_low).zipped.map(_ && _)
  //  io.Z_neg.+:((io.Z_high, tmp).zipped.map(_ || _))
}
class Lzc16 extends Module {
  val io = IO(new Bundle() {
    val X = Input(UInt(16.W))
    val Z_neg = Vec(4, Output(Bool()))
    val V_neg = Output(Bool())
  })
  val lzc8_1 = Module(new Lzc8)
  val lzc8_2 = Module(new Lzc8)
  val Z_low = Wire(Vec(3, Bool()))
  val Z_high = Wire(Vec(3, Bool()))
  lzc8_1.io.X := io.X(7, 0)
  lzc8_2.io.X := io.X(15, 8)
  Z_low := lzc8_1.io.Z_neg
  Z_high := lzc8_2.io.Z_neg
  val V_low = lzc8_1.io.V_neg
  val V_high = lzc8_2.io.V_neg
  val overlap = Module(new Lzclap(16))
  overlap.io.V_high := V_high
  overlap.io.V_low := V_low
  overlap.io.Z_low := Z_low
  overlap.io.Z_high := Z_high
  io.Z_neg := overlap.io.Z_neg
  io.V_neg := overlap.io.V_neg

}
class Lzc32 extends Module {
  val io = IO(new Bundle() {
    val X = Input(UInt(32.W))
    val Z_neg = Vec(5,Output(Bool()))
    val V_neg = Output(Bool())
  })
  val lzc16_1 = Module(new Lzc16)
  val lzc16_2 = Module(new Lzc16)
  val Z_low = Wire(Vec(4, Bool()))
  val Z_high = Wire(Vec(4, Bool()))
  lzc16_1.io.X := io.X(15, 0)
  lzc16_2.io.X := io.X(31, 16)
  Z_low := lzc16_1.io.Z_neg
  Z_high := lzc16_2.io.Z_neg
  val V_low = lzc16_1.io.V_neg
  val V_high = lzc16_2.io.V_neg
  val overlap = Module(new Lzclap(32))
  overlap.io.V_high := V_high
  overlap.io.V_low := V_low
  overlap.io.Z_low := Z_low
  overlap.io.Z_high := Z_high
  io.Z_neg := overlap.io.Z_neg
  io.V_neg := overlap.io.V_neg

}
class SignDec extends Module{
  val io = IO(new Bundle() {
    val sign = Vec(4,Input(Bool()))
    val q = Output(UInt(5.W))
  })
  val q_i = Wire(Vec(5,Bool())) // reverse for cons selection
  q_i(4) := io.sign(0) && io.sign(1)// u = -2(0)    k = -1(0 -) 0(1 -)
  q_i(3) := !io.sign(0) && io.sign(1)// u = -1(1) k = 0 (1 -) -1( 0 +)  error 弄反了
  q_i(2) := !io.sign(1) && io.sign(2)// u = 0(2) k = 1(2 -) 0(1 +)
  q_i(1) := !io.sign(2) && io.sign(3)// u = 1(3) k = 2(3,-) 1(2,+)
  q_i(0) := !io.sign(2) && !io.sign(3)//u = 2(4) k = 2(3,+) 1(2,+)
  io.q := q_i.asUInt
}

class SRT4qdsCons extends Module {
  val io = IO(new Bundle() {
    val d_trunc_3 = Input(UInt(3.W))
    val m_neg_1 = Output(UInt(9.W))
    val m_neg_0 = Output(UInt(9.W))
    val m_pos_1 = Output(UInt(9.W))
    val m_pos_2 = Output(UInt(9.W))
  })
  var neg_1_lookUpTale = VecInit(
    "b000_110100".U(9.W),
    "b000_111100".U(9.W),
    "b001_000000".U(9.W),
    "b001_001000".U(9.W),
    "b001_001100".U(9.W),
    "b001_010100".U(9.W),
    "b001_011000".U(9.W),
    "b001_100000".U(9.W)
  )
  io.m_neg_1 := neg_1_lookUpTale(io.d_trunc_3)
  var neg_0_lookUpTale = VecInit(
    "b000_010000".U(9.W),
    "b000_011000".U(9.W),
    "b000_011000".U(9.W),
    "b000_011000".U(9.W),
    "b000_100000".U(9.W),
    "b000_100000".U(9.W),
    "b000_100000".U(9.W),
    "b000_100000".U(9.W)
  )
  io.m_neg_0 := neg_0_lookUpTale(io.d_trunc_3)
  var pos_1_lookUpTale = VecInit(
    "b111_110100".U(9.W),
    "b111_110000".U(9.W),
    "b111_110000".U(9.W),
    "b111_110000".U(9.W),
    "b111_101100".U(9.W),
    "b111_101000".U(9.W),
    "b111_101000".U(9.W),
    "b111_101000".U(9.W)
  )
  io.m_pos_1 := pos_1_lookUpTale(io.d_trunc_3)
  var pos_2_lookUpTale = VecInit(
    "b111_010000".U(9.W),
    "b111_001000".U(9.W),
    "b111_000100".U(9.W),
    "b110_111100".U(9.W),
    "b110_111000".U(9.W),
    "b110_110000".U(9.W),
    "b110_101100".U(9.W),
    "b110_100100".U(9.W)
  )
  io.m_pos_2 := pos_2_lookUpTale(io.d_trunc_3)
}
abstract class CarrySaveAdderMToN(m: Int, n: Int)(len: Int) extends Module{
  val io = IO(new Bundle() {
    val in = Input(Vec(m, UInt(len.W)))
    val out = Output(Vec(n, UInt(len.W)))
  })
}
class CSA3_2(len: Int) extends CarrySaveAdderMToN(3, 2)(len){
  val temp = Wire(Vec(len, UInt(2.W)))
  for((t, i) <- temp.zipWithIndex){
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b = a ^ b
    val a_and_b = a & b
    val sum = a_xor_b ^ cin
    val cout = a_and_b | (a_xor_b & cin)
    t := Cat(cout, sum)
  }
  io.out.zipWithIndex.foreach({case(x, i) => x := Cat(temp.reverse map(_(i)))})
}
