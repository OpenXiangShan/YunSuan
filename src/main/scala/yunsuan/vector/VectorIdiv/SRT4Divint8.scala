
package yunsuan.vector

import chisel3._
import chisel3.util._
/**
 * 8bit div int module using SRT radix-4
 * parameters:
 * (mk:index) -1: 0 0 :1 1:2 2:3
 * (u:index) -2: 0 ...
 * (q:index) 2:0   -2 3  need to reverse u index here，because we search as -q for const
 * v2 try to reduce the number of registers
 * v3 try to reduce the bitwidth of selection const
 */
class SRT4Divint8(bit_width: Int=8) extends Module {
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
  val w_width  = bit_width + 4 // 4 : 1bit sign， 2bit *4/4 shift，1bit ralign
  val bypass_width = bit_width << 1
  val lzc_w = log2Up(bit_width)
  val early_finish = Wire(Bool())
  val iter_finish = Wire(Bool())
  val idle :: pre :: iter :: post :: output :: Nil = Enum(5)
  val (oh_idle, oh_pre, oh_iter, oh_post, oh_output) =
    (UIntToOH(idle, 5), UIntToOH(pre, 5), UIntToOH(iter, 5), UIntToOH(post, 5), UIntToOH(output, 5))
  // handshake
  val stateReg = RegInit((1 << idle.litValue.toInt).U(6.W))
  val in_handshake = io.div_in_valid && io.div_ready

  io.div_ready := stateReg(idle)
  io.div_out_valid := stateReg(output)
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
  val init_q_A = Wire(UInt(bit_width.W)) //q_A :real q
  val iter_q_A = Wire(UInt(bit_width.W))
  val next_q_A = Wire(UInt(bit_width.W))
  val iter_q_A_reg = RegEnable(iter_q_A, stateReg(pre) | stateReg(iter))
  val init_q_B = Wire(UInt(bit_width.W)) //q_B :real q - 1
  val iter_q_B = Wire(UInt(bit_width.W))
  val next_q_B = Wire(UInt(bit_width.W))
  val iter_q_B_reg = RegEnable(iter_q_B, stateReg(pre) | stateReg(iter))
  val init_q = Wire(UInt(5.W))
  val nxt_q = Wire(UInt(5.W))
  val iter_q = Wire(UInt(5.W))
  val iter_q_reg = RegEnable(iter_q, stateReg(pre) | stateReg(iter)) // q with one-hot style，store pre q

  val w_init = Wire(Vec(2, UInt((w_width+2).W))) // because we donot take bypass to reduce the size，after each iteration w*4，so we need to rshift lzc_d + 2 + ralign，to not rshift 2 bit we extend the result of each iter as not mul 4 wich need extra 2bit for shift
  val w_iter = Wire(Vec(2, UInt((w_width+2).W)))
  val w_next = Wire(Vec(2, UInt((w_width+2).W)))
  val w_iter_reg = RegEnable(w_iter, stateReg(pre) | stateReg(iter))

  // pre_stage
  val x = io.dividend
  //val x_reg = RegEnable(x, stateReg(pre))
  val d = io.divisor
  val x_sign = io.sign && x(bit_width - 1)
  val x_sign_reg = RegEnable(x_sign, stateReg(pre))
  val d_sign = io.sign && d(bit_width - 1)

  val neg_x_q = Wire(UInt(bit_width.W))
  val neg_d_q = Wire(UInt(bit_width.W))
  val abs_x = Wire(UInt(bit_width.W))
  val abs_d = Wire(UInt(bit_width.W))
  neg_x_q := -Mux(stateReg(pre), x, iter_q_A_reg)
  neg_d_q := -Mux(stateReg(pre), d, iter_q_B_reg)
  abs_x := Mux(x_sign, neg_x_q, x)
  abs_d := Mux(d_sign, neg_d_q, d)

  // lzc leading zero count
  val lzc_x = Wire(UInt(lzc_w.W))
  val zero_x = Wire(Bool())
  val lzc_d = Wire(UInt(lzc_w.W))
  val zero_d = Wire(Bool())
  val zero_d_reg = RegEnable(zero_d, stateReg(pre))
  val x_enc = Wire(UInt((lzc_w + 1).W))
  val d_enc = Wire(UInt((lzc_w + 1).W))
  x_enc := PriorityEncoder(abs_x.asBools.reverse)
  d_enc := PriorityEncoder(abs_d.asBools.reverse)
  lzc_x := x_enc(lzc_w - 1, 0)
  zero_x := ~abs_x.orR
  lzc_d := d_enc(lzc_w - 1, 0)
  val lzc_d_reg = RegEnable(lzc_d, stateReg(pre))
  zero_d := ~abs_d.orR
  // q _sign
  val q_sign = Wire(Bool())
  val q_sign_reg = RegEnable(q_sign, stateReg(pre))
  q_sign := x_sign ^ d_sign
  // lzc_diff
  val lzc_diff = Cat(0.U(1.W), lzc_d) - Cat(0.U(1.W), lzc_x)
  val norm_x = Wire(UInt(bit_width.W)) 
  val norm_d = Wire(UInt(bit_width.W))
  norm_x := abs_x << lzc_x
  norm_d := abs_d << lzc_d

  // special case
  val x_small = lzc_diff(lzc_w)
  val d_zero = zero_d
  early_finish := x_small | d_zero
  val early_finish_q = RegEnable(early_finish, stateReg(pre))

  val early_q = Mux(x_small, 0.U,
    Mux(d_zero, Fill(bit_width, 1.U),
      0.U))
  val early_rem = Mux(x_small | d_zero, Cat(abs_x,0.U(2.W)), 0.U)

  // iter_num
  val init_iter_num = Wire(UInt(lzc_w.W))
  val iter_num = Wire(UInt(lzc_w.W))
  val iter_num_reg = RegEnable(iter_num, stateReg(pre) | stateReg(iter))
  init_iter_num := (lzc_diff >> 1).asUInt + (lzc_diff(0)).asUInt
  iter_num := Mux(stateReg(pre), init_iter_num, iter_num_reg - 1.U)
  iter_finish := iter_num_reg === 0.U

  // init_w
  // rshift_num = 1 - (lzc_diff + 1) %2
  w_init(0) := Mux(early_finish, early_rem, Cat(0.U(5.W),
    Mux(lzc_diff(0), Cat(0.U(1.W), norm_x),
      Cat(norm_x,0.U(1.W)))
  ))
  w_init(1) := 0.U


  // init_q
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
  val w_trunc_1_4 = Wire(UInt(5.W))
  w_trunc_1_4 := Cat(0.U(1.W), w_init(0)(w_width - 4, w_width - 7)) // *4 and trunc err 4
  val cmp_pos_1 = w_trunc_1_4 >= m_pos1_q1
  val cmp_pos_2 = w_trunc_1_4 >= m_pos2_q1
  init_q := Cat(0.U(2.W), //-2 -1 attentin the msb of cat opeartion is at the right，so it reverse :vec 0 -2d 1 -d 2 0 3 d 4 2d 
    (!cmp_pos_1) & (!cmp_pos_2), //0
    (cmp_pos_1) & (!cmp_pos_2), // 1
    cmp_pos_2) //2


  init_q_A := Mux(early_finish, early_q,0.U)
  init_q_B := 0.U

  // cons for iter
  val d_ext = Cat(0.U(3.W),norm_d,0.U(1.W))
  val neg_d_ext = -d_ext
  val rud = Wire(Vec(5,UInt(w_width.W)))
  rud := VecInit(
    neg_d_ext << 1,
    neg_d_ext,
    0.U,
    d_ext,
    d_ext << 1
  )
  val const_d_reg = RegEnable(rud, stateReg(pre)) // -2d d 0 d 2d

  // get mk
  val neg_mk = Wire(Vec(4,UInt(6.W))) // - selection const
  val R4_qds = Module(new SRT4qdsCons_i8_v3)
  R4_qds.io.d_trunc_3 := norm_d(bit_width - 2, bit_width -4)
  neg_mk(0) := R4_qds.io.m_neg_1
  neg_mk(1) := R4_qds.io.m_neg_0
  neg_mk(2) := R4_qds.io.m_pos_1
  neg_mk(3) := R4_qds.io.m_pos_2
  val neg_mk_reg = RegEnable(neg_mk, stateReg(pre))
  // iter
  val IterBlock_SR4 = Module(new IterBlockSR4_v3(bit_width, w_width))
  IterBlock_SR4.io.d_cons := const_d_reg
  IterBlock_SR4.io.neg_mk := neg_mk_reg
  IterBlock_SR4.io.pre_w := w_iter_reg
  IterBlock_SR4.io.q_j := iter_q_reg
  IterBlock_SR4.io.pre_q_A := iter_q_A_reg
  IterBlock_SR4.io.pre_q_B := iter_q_B_reg

  w_next := IterBlock_SR4.io.nxt_w
//  w_iter_c := Mux(stateReg(pre),w_iter_c_init,IterBlock_SR4.io.nxt_w)
  nxt_q  := IterBlock_SR4.io.q_j_1
  next_q_A := IterBlock_SR4.io.nxt_q_A
  next_q_B := IterBlock_SR4.io.nxt_q_B

  //reg
  iter_q_A := Mux(stateReg(pre),init_q_A, next_q_A)
  iter_q_B := Mux(stateReg(pre), init_q_B, next_q_B)
  w_iter := Mux(stateReg(pre), w_init, w_next)
  iter_q := Mux(stateReg(pre), init_q, nxt_q)

  // post stage
  val rem_sign = Wire(UInt(w_width.W)) // rem after sign adust
  val rem_sign_plus_d = Wire(UInt(w_width.W)) // rem after sign adust then +/- d
  val rem_adjust = Wire(UInt(w_width.W))
  val q_A_sign_c = Wire(UInt(bit_width.W)) // q after sign adjust
  val q_B_sign_c = Wire(UInt(bit_width.W)) // q-1 after sign adjust
  val rem_correct = Wire(Vec(2,UInt(w_width.W)))
  rem_correct := VecInit(Seq.tabulate(2){i => w_iter_reg(i)(w_width + 1, 2)})
  q_A_sign_c := Mux(q_sign_reg && !zero_d_reg, neg_x_q, iter_q_A_reg)
  q_B_sign_c := Mux(q_sign_reg && !zero_d_reg, neg_d_q, iter_q_B_reg)

  when(x_sign_reg) {
    rem_sign := (~rem_correct(0)).asUInt + (~rem_correct(1)).asUInt + 2.U
    rem_sign_plus_d := (~rem_correct(0)).asUInt + (~rem_correct(1)).asUInt + const_d_reg(1) + 2.U
  } .otherwise {
    rem_sign := rem_correct(0).asUInt +rem_correct(1).asUInt
    rem_sign_plus_d := rem_correct(0).asUInt + rem_correct(1).asUInt + const_d_reg(3)
  }
  val rem_is_zero = !(rem_sign.orR)
  val adjust = Mux(x_sign_reg, (~rem_is_zero).asBool &(~rem_sign(w_width - 1)).asBool, rem_sign(w_width -1)) & !early_finish_q
  val out_q_final =
    Mux(adjust, q_B_sign_c, q_A_sign_c)
  rem_adjust :=
    Mux(adjust, rem_sign_plus_d, rem_sign)
  val out_rem_final =
    Mux(early_finish_q, rem_adjust.asSInt , rem_adjust.asSInt >>(lzc_d_reg+1.U)) // attention arithmetic rshift， because it has been sign adjusted,(can put it in the post stage?)
  val out_q_final_reg = RegEnable(out_q_final, stateReg(post))
  val out_rem_final_reg = RegEnable(out_rem_final, stateReg(post))

  io.div_out_q := out_q_final_reg
  io.div_out_rem := out_rem_final_reg.asUInt
  io.d_zero := zero_d_reg
  /*test
  printf("state one-hot :[%d,%d,%d,%d,%d]  \n", stateReg(idle), stateReg(pre), stateReg(iter), stateReg(post), stateReg(output))
  when(stateReg(pre)) {
    printf("abs_x %d\n", abs_x)
    printf("abs_d %d\n", abs_d)
    printf("q_sign %b\n", q_sign)
    printf("early_finish %b\n", early_finish)
    printf("early_finish_q %b\n", early_q)
    printf("early_finish_rem %b\n", early_rem)
    printf("iter_finish %b \n", iter_finish)
    printf("norm_x %b , norm_d %b\n", norm_x, norm_d)
    printf("const neg2d %b, negd %b, pd %b, p2d %b\n",rud(0), rud(1), rud(3), rud(4))
    printf("lzc_x %b lzc_d %b lzc_diff %b\n",lzc_x, lzc_d,lzc_diff)
    printf("w_trunc_1_4 %b\n", w_trunc_1_4)
    printf("init q %b, init w %b , init q_A%b, init_q_b %b\n", init_q, w_init(0) +  w_init(1), init_q_A, init_q_B)
    printf("neg_mk 0 :%b, 1 :%b, 2: %b, 3: %b\n", neg_mk(0), neg_mk(1), neg_mk(2), neg_mk(3))
  }
  when(stateReg(iter)) {
    printf("iter_num reg %d\n", iter_num_reg)
    printf("pre_w %b pre_q %b\n", w_iter_reg(0) + w_iter_reg(1) , iter_q_reg)
    printf("nxt_w %b\n", w_next(0) + w_next(1))
    printf("nxt_q_A %b \n", next_q_A)
    printf("nxt_q_B %b \n", next_q_B)
    printf("nxt_q %b \n", nxt_q)
  }
  when(stateReg(post)) {
    printf("out_handshake")
    printf("x_sign %b\n", x_sign_reg)
    printf("w_iter %b correct_iter %b neg %biterq_A %b\n", w_iter_reg(0) + w_iter_reg(1), rem_correct(0) + rem_correct(1), (~rem_correct(0)).asUInt + (~rem_correct(1)).asUInt+ 2.U, iter_q_A_reg)
    printf("q_adjust %b div_out_q %b\n", out_q_final, io.div_out_q)
    printf("out_rem_adjust %b\n", rem_sign)
    printf("out_rem_adjust_plus_D %b \n", rem_sign_plus_d)
    printf("adjust %b \n", adjust)
    printf("out_rem_final %b\n", out_rem_final)
    printf("out_rem_final_reg %b \n", out_rem_final_reg)
    printf("div_out_rem %b \n", io.div_out_rem)
    printf("data valid %b \n", io.div_out_valid)
    printf("data ready %b \n", io.div_out_ready)
  }
  when(stateReg(output)) {
//    printf("x_sign %b\n", x_sign_reg)
//    printf("w_iter %b correct_iter %b neg %biterq_A %b\n", w_iter_reg(0) + w_iter_reg(1), rem_correct(0) + rem_correct(1), (~rem_correct(0)).asUInt + (~rem_correct(1)).asUInt + 2.U, iter_q_A_reg)
//    printf("q_adjust %b div_out_q %b\n", out_q_final, io.div_out_q)
//    printf("out_rem_adjust %b\n", rem_sign)
//    printf("out_rem_adjust_plus_D %b \n", rem_sign_plus_d)
//    printf("adjust %b \n", adjust)
//    printf("out_rem_final %b\n", out_rem_final)
//    printf("out_rem_final_reg %b \n", out_rem_final_reg)
    printf("div_out_rem %b \n", io.div_out_rem)
    printf("data valid %b \n", io.div_out_valid)
    printf("data ready %b \n", io.div_out_ready)
  }

//  */
}
class IterBlockSR4_v3(bit_width: Int,w_width: Int) extends Module {
  val io = IO(new Bundle() {
    val d_cons = Vec(5, Input(UInt(w_width.W)))
    val neg_mk = Vec(4, Input(UInt(6.W)))
    val pre_w = Vec(2, Input(UInt((w_width+2).W)))
    val nxt_w = Vec(2, Output(UInt((w_width+2).W)))
    //val nxt_w_mul4 = Vec(2, Output(UInt(w_width.W)))
    val q_j   = Input(UInt(5.W))
    val q_j_1 = Output(UInt(5.W))
    val pre_q_A = Input(UInt(bit_width.W))
    val pre_q_B = Input(UInt(bit_width.W))
    val nxt_q_A = Output(UInt(bit_width.W)) // on-the-fly conversion
    val nxt_q_B = Output(UInt(bit_width.W))
  })
  val pre_w_mul4 = Wire(Vec(2,UInt((w_width).W)))
  pre_w_mul4 := VecInit(Seq.tabulate(2) {i => (io.pre_w(i) << 2)(w_width + 1, 2)})//left shift 2
  val csa_1 = Module(new CSA3_2(w_width)).suggestName(s"IterBlockSR4_csa1")
  val d_cons_sel = Mux1H(io.q_j, io.d_cons.toSeq)
  csa_1.io.in(0) := pre_w_mul4(0)
  csa_1.io.in(1) := pre_w_mul4(1)
  csa_1.io.in(2) := d_cons_sel
  val temp_w = Wire(Vec(2,UInt(w_width.W)))  //xxx.xxxx
  temp_w := VecInit(
    csa_1.io.out(0),
    csa_1.io.out(1)<<1
  )
  val rem_trunc = Wire(Vec(4, Vec(2,UInt(6.W)))) // m_2 (2,4) m_1(3,3) m_0(3,3) m_-1(2,4) (integer，fraction)
  val nxt_w_mul4 = VecInit( // x_xxxxx00 
    Cat(temp_w(0)(w_width - 3, 0), 0.U(2.W)),
    Cat(temp_w(1)(w_width - 3, 0), 0.U(2.W))
  )
  rem_trunc := VecInit(
    VecInit(Seq.tabulate(2){i => nxt_w_mul4(i)(w_width - 2, w_width - 7)}),
    VecInit(Seq.tabulate(2){i => nxt_w_mul4(i)(w_width - 1, w_width - 6)}),
    VecInit(Seq.tabulate(2){i => nxt_w_mul4(i)(w_width - 1, w_width - 6)}),
    VecInit(Seq.tabulate(2){i => nxt_w_mul4(i)(w_width - 2, w_width - 7)})
  )
  val sign = Wire(Vec(4,Bool()))
  sign := VecInit(Seq.tabulate(4){i =>(rem_trunc(i)(0) + rem_trunc(i)(1) + io.neg_mk(i))(5)})

  val SD_iter = Module(new SignDec()).suggestName(s"IterBlockSR4_SD")
  SD_iter.io.sign := sign
  io.q_j_1 := SD_iter.io.q

  val Conv_iter = Module(new Conversion(bit_width)).suggestName(s"IterBlockSR4_Conv") // on-the-fly conversion block
  Conv_iter.io.pre_q_A := io.pre_q_A
  Conv_iter.io.pre_q_B := io.pre_q_B
  Conv_iter.io.q_j_1 := io.q_j
  io.nxt_q_A := Conv_iter.io.nxt_q_A
  io.nxt_q_B := Conv_iter.io.nxt_q_B


  io.nxt_w := VecInit(Seq.tabulate(2){i => Cat(temp_w(i),0.U(2.W))})


}
class SRT4qdsCons_i8_v3 extends Module {
  val io = IO(new Bundle() {
    val d_trunc_3 = Input(UInt(3.W))
    val m_neg_1 = Output(UInt(6.W))
    val m_neg_0 = Output(UInt(6.W))
    val m_pos_1 = Output(UInt(6.W))
    val m_pos_2 = Output(UInt(6.W))
  })
  var neg_1_lookUpTale = VecInit(
    "b00_1101".U(6.W),
    "b00_1111".U(6.W),
    "b01_0000".U(6.W),
    "b01_0010".U(6.W),
    "b01_0011".U(6.W),
    "b01_0101".U(6.W),
    "b01_0110".U(6.W),
    "b01_1000".U(6.W)
  )
  io.m_neg_1 := neg_1_lookUpTale(io.d_trunc_3)
  var neg_0_lookUpTale = VecInit(
    "b000_010".U(6.W),
    "b000_011".U(6.W),
    "b000_011".U(6.W),
    "b000_011".U(6.W),
    "b000_100".U(6.W),
    "b000_100".U(6.W),
    "b000_100".U(6.W),
    "b000_100".U(6.W)
  )
  io.m_neg_0 := neg_0_lookUpTale(io.d_trunc_3)
  var pos_1_lookUpTale = VecInit(
    "b111_110".U(6.W), //4
    "b111_110".U(6.W), //4
    "b111_110".U(6.W), //4
    "b111_110".U(6.W), //4
    "b111_101".U(6.W), //6
    "b111_101".U(6.W), //6
    "b111_101".U(6.W), //6
    "b111_101".U(6.W) //6
  )
  io.m_pos_1 := pos_1_lookUpTale(io.d_trunc_3)
  var pos_2_lookUpTale = VecInit(
    "b11_0100".U(6.W),
    "b11_0010".U(6.W),
    "b11_0001".U(6.W),
    "b10_1111".U(6.W),
    "b10_1110".U(6.W),
    "b10_1100".U(6.W),
    "b10_1011".U(6.W),
    "b10_1001".U(6.W)
  )
  io.m_pos_2 := pos_2_lookUpTale(io.d_trunc_3)
}
