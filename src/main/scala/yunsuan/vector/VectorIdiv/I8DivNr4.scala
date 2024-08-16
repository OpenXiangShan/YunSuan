
package yunsuan.vector

import chisel3._
import chisel3.util._
// 8bit div int module using non-restoring division
class I8DivNr4(bit_width: Int=8) extends Module {
  val io = IO(new Bundle() {
    val sign = Input(Bool())
    //val Int_format = Input(UInt(2.W))
    val dividend = Input(UInt(bit_width.W))
    val divisor = Input(UInt(bit_width.W))
    val flush = Input(Bool())
    val d_zero = Output(Bool())
    //val d_zero_err = Output(Bool())
    //val overflow_err = Output(Bool())
    //
    val div_in_valid = Input(Bool())
    val div_ready = Output(Bool())
    val div_out_ready = Input(Bool())
    val div_out_valid = Output(Bool())
    val div_out_q = Output(UInt(bit_width.W))
    val div_out_rem = Output(UInt(bit_width.W))

  })
  val lzc_w = log2Up(bit_width)
  val early_finish = Wire(Bool())
  val iter_finish = Wire(Bool())
  val idle :: pre :: iter :: post :: output :: Nil = Enum(5)
  val (oh_idle, oh_pre, oh_iter, oh_post, oh_output) =
    (UIntToOH(idle, 5), UIntToOH(pre, 5), UIntToOH(iter, 5), UIntToOH(post, 5), UIntToOH(output, 5))
  // handshake
  val stateReg = RegInit((1 << idle.litValue.toInt).U(6.W))
  val in_handshake = io.div_in_valid && io.div_ready // input handshake
  //  val out_handshake = io.div_out_valid && io.div_out_ready
  //  val div_out_valid_v = Mux(stateReg(idle), false.B, stateReg(post))
  //  val div_out_valid_reg = RegEnable(div_out_valid_v, stateReg(post)|stateReg(idle))
  io.div_ready := stateReg(idle)
  io.div_out_valid := stateReg(output)
  // fsm
  when(io.flush) {
    stateReg := oh_idle
  }.elsewhen(stateReg(idle) && in_handshake) {
    stateReg := oh_pre
  }.elsewhen(stateReg(pre)) {
    stateReg := Mux(early_finish, oh_post, oh_iter)
  }.elsewhen(stateReg(iter)) {
    stateReg := Mux(iter_finish, oh_post, oh_iter)
  }.elsewhen(stateReg(post)) {
    stateReg := oh_output
  }.elsewhen(stateReg(output) & io.div_out_ready) {
    stateReg := oh_idle
  }.otherwise {
    stateReg := stateReg
  }

  val init_q = Wire(UInt(bit_width.W))
  val iter_q = Wire(UInt(bit_width.W))
  val nxt_q  = Wire(UInt(bit_width.W))
  val iter_q_reg = RegEnable(iter_q, stateReg(pre)|stateReg(iter))
  val init_w = Wire(UInt((bit_width+1).W))
  val iter_w = Wire(UInt((bit_width+1).W))
  val nxt_w = Wire(UInt((bit_width+1).W))
  val iter_w_reg = RegEnable(iter_w, stateReg(pre)|stateReg(iter)) // msb of rem
  val init_w_low = Wire(UInt(bit_width.W))
  val iter_w_low = Wire(UInt(bit_width.W))
  val nxt_w_low = Wire(UInt(bit_width.W))
  val iter_w_low_reg = RegEnable(iter_w_low,stateReg(pre)|stateReg(iter)) // lsbn of rem :divisor need to left shift n bit，which means we subtract d from msb of rem and each iteration we left shift msb and get the lsb from this reg


  // pre
  val x = io.dividend
  val d = io.divisor
  val x_sign = io.sign && x(bit_width - 1)
  val x_sign_reg = RegEnable(x_sign, stateReg(pre))
  val d_sign = io.sign && d(bit_width - 1)

  val neg_x_q = Wire(UInt(bit_width.W))
  val neg_d = Wire(UInt(bit_width.W))
  val abs_x = Wire(UInt(bit_width.W))
  val abs_d = Wire(UInt(bit_width.W))


  neg_x_q := - Mux(stateReg(pre),x,iter_q_reg)
  neg_d := - d
  abs_x := Mux(x_sign, neg_x_q, x)
  abs_d := Mux(d_sign, neg_d, d)


  // q _sign
  val q_sign = Wire(Bool())
  val q_sign_reg = RegEnable(q_sign, stateReg(pre))
  q_sign := x_sign ^ d_sign



  // special case
  val x_small = abs_x < abs_d
  val x_zero = !(abs_x.orR)
  val d_zero = !(abs_d.orR)
  val zero_d_reg = RegEnable(d_zero,stateReg(pre))
  early_finish := x_small || x_zero || d_zero
  val early_finish_q = RegEnable(early_finish,stateReg(pre))

  //  val d_one =

  val early_q = Mux(x_small, 0.U,
    Mux(d_zero, Fill(bit_width, 1.U)
      ,0.U))
  val early_rem = Mux(x_small | d_zero, abs_x, 0.U)

  // iter_num
  val init_iter_num = Wire(UInt(lzc_w.W))
  val iter_num = Wire(UInt(lzc_w.W))
  val iter_num_reg = RegEnable(iter_num, stateReg(pre)|stateReg(iter))
  init_iter_num := bit_width.U >> 1
  iter_num := Mux(stateReg(pre), init_iter_num, iter_num_reg - 1.U)
  iter_finish := iter_num === 0.U


  // init q
  init_q := Mux(early_finish,early_q,0.U)

  // init_w
  init_w := Mux(early_finish,(Cat(0.U(1.W),early_rem)), 0.U)
  init_w_low := abs_x

  // const generate for d
  val d_ext = Cat(0.U(1.W),abs_d)
  val neg_d_ext = -d_ext
  val const_d = VecInit(  // for {-3,-1,1,3} {j 0 j+1 0, j 0 j+1 1, j 1 j+1 0, j 1 j+1 1} j, j+1 is the sign of rem_j rem_j+1
    neg_d_ext + (neg_d_ext << 1),
    neg_d_ext,
    d_ext,
    d_ext + (d_ext << 1)
  )

  val const_d_reg = RegEnable(const_d, stateReg(pre))


  // iter
  val iter_b = Module(new IterBlockI8Nr4(bit_width))
  iter_b.io.d_cons := const_d_reg
  iter_b.io.pre_w := iter_w_reg
  iter_b.io.pre_w_low := iter_w_low_reg
  iter_b.io.pre_q := iter_q_reg
  nxt_w := iter_b.io.nxt_w
  nxt_w_low := iter_b.io.nxt_w_low
  nxt_q := iter_b.io.nxt_q

  // reg
  iter_w := Mux(stateReg(pre), init_w, nxt_w)
  iter_w_low := Mux(stateReg(pre), init_w_low, nxt_w_low)
  iter_q := Mux(stateReg(pre),init_q, nxt_q)


  // post
  val out_q_final = Wire(UInt(bit_width.W))
  out_q_final := Mux(q_sign_reg && !zero_d_reg, neg_x_q, iter_q_reg)
  val out_q_final_reg = RegEnable(out_q_final,stateReg(post)) 

  val out_rem_is_zero = Wire(Bool())
  out_rem_is_zero := !(iter_w_reg.orR)
  val out_rem_is_pos = Wire(Bool())
  out_rem_is_pos := (!iter_w_reg(bit_width)).asBool
  val out_rem_adjust = Wire(UInt(bit_width.W))
  val out_rem_adjust_plus_D = Wire(UInt(bit_width.W))
  when (x_sign_reg ) {
    out_rem_adjust := -iter_w_reg(bit_width-1, 0)
    out_rem_adjust_plus_D := -iter_w_reg(bit_width-1, 0) + const_d_reg(1)(bit_width-1,0)
  } .otherwise {
    out_rem_adjust := iter_w_reg(bit_width-1, 0)
    out_rem_adjust_plus_D := iter_w_reg(bit_width-1, 0) + const_d_reg(2)(bit_width-1, 0)
  }


  val adjust = Wire(Bool())
  //  adjust := Mux(x_sign_reg , (!out_rem_is_zero) && out_rem_is_pos, !out_rem_is_pos) && !early_finish_q
  adjust := !out_rem_is_pos && !early_finish_q
  val out_rem_final = Wire(UInt(bit_width.W))
  val out_rem_final_reg =RegEnable(out_rem_final, stateReg(post))
  out_rem_final := Mux(adjust, out_rem_adjust_plus_D, out_rem_adjust)

  io.div_out_q := out_q_final_reg
  io.div_out_rem := out_rem_final_reg
  io.d_zero := zero_d_reg
  /* for test
  printf("state one-hot :[%d,%d,%d,%d]\n",stateReg(idle),stateReg(pre),stateReg(iter),stateReg(post))
  when(stateReg(pre)) {
    printf("abs_x %d\n", abs_x)
    printf("abs_d %d\n", abs_d)
    printf("q_sign %b\n", q_sign)
    printf("early_finish %b\n", early_finish)
    printf("early_finish_q %b\n", early_q)
    printf("early_finish_rem %b\n", early_rem)
    printf("iter_finish %b \n", iter_finish)
    printf("const neg3d %b,negd%b, pd %b p3d%b\n", const_d(0), const_d(1),const_d(2),const_d(3))
    printf("init q %d, init w %d , init w_low%d\n", init_q, init_w, init_w_low)
  }
  when(stateReg(iter)) {
    printf("iter_num reg %d\n", iter_num_reg)
    printf("nxt_w %b\n", nxt_w)
    printf("nxt_w_low %b \n", nxt_w_low)
    printf("nxt_q %b \n", nxt_q)
  }
  when(stateReg(post)) {
    printf("div_out_q %b\n", io.div_out_q)
    printf("out_rem_adjust %b\n", out_rem_adjust)
    printf("out_rem_adjust_plus_D %b \n", out_rem_adjust_plus_D)
    printf("adjust %b \n", adjust)
    printf("out_rem_final %b\n", out_rem_final)
    printf("out_rem_final_reg %b \n", out_rem_final_reg)
    printf("div_out_rem %b \n", io.div_out_rem)
    printf("data valid %b \n", io.div_out_valid)
    printf("data ready %b \n", io.div_out_ready)
  }
  */
}
class IterBlockI8Nr4(bit_width: Int) extends Module {
  val io = IO(new Bundle() {
    val d_cons = Vec(4,Input(UInt((bit_width+1).W)))
    val pre_w = Input(UInt((bit_width+1).W))
    val pre_w_low = Input(UInt(bit_width.W))
    val nxt_w = Output(UInt((bit_width+1).W))
    val nxt_w_low = Output(UInt(bit_width.W))
    val pre_q = Input(UInt(bit_width.W))
    val nxt_q = Output(UInt(bit_width.W))
  })
  val pre_w_mul4 = Wire(UInt((bit_width+1).W))
  pre_w_mul4 := Cat(io.pre_w(bit_width - 2, 0), io.pre_w_low(bit_width - 1, bit_width - 2))
  val pre_w_mul2 = Wire(UInt((bit_width+1).W))
  pre_w_mul2 := Cat(io.pre_w(bit_width - 1,0),io.pre_w_low(bit_width - 1))
  io.nxt_w_low := io.pre_w_low << 2

  val cons = Wire(UInt((bit_width+1).W))
  val cons_index = Cat(0.U(1.W),io.pre_w(bit_width))+ 1.U
  cons := io.d_cons(cons_index)  // sign 1 pos dcons(1) sign 0 neg dcons(0)

  val tmp = Wire(UInt((bit_width+1).W))
  tmp := cons + pre_w_mul2

  // speculative Parallel implementation, predicting all possible values before obtaining the previous q,
  val spec_rem = Wire(Vec(4,UInt((bit_width + 1).W)))
  spec_rem := VecInit (
    Seq.tabulate(4) {i => io.d_cons(i) + pre_w_mul4}
  )
  // choose w from spec_rem
  val spec_hb = UIntToOH(Cat(io.pre_w(bit_width),tmp(bit_width)),4)
  io.nxt_w := Mux1H(spec_hb,spec_rem.toSeq)
  io.nxt_q := Cat(io.pre_q(bit_width - 3, 0), ~tmp(bit_width),~io.nxt_w(bit_width))
 // printf("pre_w := %b\n",io.pre_w)
  //printf("tmp := %b\n", tmp)
  //printf("cons := %b  cons_index := %b\n",cons,cons_index)
  //printf("spec_rem := %b %b %b %b \n", spec_rem(0),spec_rem(1),spec_rem(2),spec_rem(3))
  //printf("spec_hb %b\n" ,spec_hb)
//  val tmp_mul2 = Wire(UInt((bit_width+1).W))
//  tmp_mul2 := Cat(tmp(bit_width - 1,0),io.pre_w_low(bit_width - 2))
//  val cons_2 = Wire(UInt((bit_width+1).W))
//  cons_2 := io.d_cons(tmp(bit_width).asUInt)
//  io.nxt_w := tmp_mul2 + cons_2
//  io.nxt_q := Cat(io.pre_q(bit_width - 3, 0), ~tmp(bit_width),~io.nxt_w(bit_width))
}