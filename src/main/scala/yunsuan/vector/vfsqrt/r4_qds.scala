package yunsuan.vector.vfsqrt

import chisel3._
import chisel3.util._

class r4_qds extends Module{
  val io = IO(new Bundle {

  })

  val rem_i     = IO(Input(UInt(7.W)))
  val m_neg_1_i = IO(Input(UInt(7.W)))
  val m_neg_0_i = IO(Input(UInt(7.W)))
  val m_pos_1_i = IO(Input(UInt(7.W)))
  val m_pos_2_i = IO(Input(UInt(7.W)))
  val rt_dig_o  = IO(Output(UInt(5.W)))

  val qds_sign = Wire(UInt(4.W))
  qds_sign := Cat(
    (rem_i + m_pos_2_i).head(1),
    (rem_i + m_pos_1_i).head(1),
    (rem_i + m_neg_0_i).head(1),
    (rem_i + m_neg_1_i).head(1)
  )
  rt_dig_o := Cat(
    qds_sign(1,0) === "b11".U(2.W),
    qds_sign(1,0) === "b10".U(2.W),
    qds_sign(2,1) === "b10".U(2.W),
    qds_sign(3,2) === "b10".U(2.W),
    qds_sign(3,2) === "b00".U(2.W)
  )
}


class r4_qds_cg extends Module{
  val io = IO(new Bundle {

  })

  val a0_i = IO(Input(Bool()))
  val a2_i = IO(Input(Bool()))
  val a3_i = IO(Input(Bool()))
  val a4_i = IO(Input(Bool()))
  val m_neg_1_o = IO(Output(UInt(7.W)))
  val m_neg_0_o = IO(Output(UInt(7.W)))
  val m_pos_1_o = IO(Output(UInt(7.W)))
  val m_pos_2_o = IO(Output(UInt(7.W)))

  val a2_a3_a4 = Cat(a2_i,a3_i,a4_i)
  m_neg_1_o := Mux1H(
    Seq(
      (!a0_i & a2_a3_a4 === 0.U(3.W)) -> "b0001_101".U(7.W),
      (!a0_i & a2_a3_a4 === 1.U(3.W)) -> "b0001_110".U(7.W),
      (!a0_i & a2_a3_a4 === 2.U(3.W)) -> "b0010_000".U(7.W),
      (!a0_i & a2_a3_a4 === 3.U(3.W)) -> "b0010_001".U(7.W),
      (!a0_i & a2_a3_a4 === 4.U(3.W)) -> "b0010_010".U(7.W),
      (!a0_i & a2_a3_a4 === 5.U(3.W)) -> "b0010_100".U(7.W),
      (!a0_i & a2_a3_a4 === 6.U(3.W)) -> "b0010_110".U(7.W),
      (!a0_i & a2_a3_a4 === 7.U(3.W)) -> "b0010_111".U(7.W),
      (a0_i                         ) -> "b0010_111".U(7.W)
    )
  )
  m_neg_0_o := Mux1H(
    Seq(
      (!a0_i & a2_a3_a4 === 0.U(3.W)) -> "b0000_100".U(7.W),
      (!a0_i & a2_a3_a4 === 1.U(3.W)) -> "b0000_101".U(7.W),
      (!a0_i & a2_a3_a4 === 2.U(3.W)) -> "b0000_110".U(7.W),
      (!a0_i & a2_a3_a4 === 3.U(3.W)) -> "b0000_110".U(7.W),
      (!a0_i & a2_a3_a4 === 4.U(3.W)) -> "b0000_110".U(7.W),
      (!a0_i & a2_a3_a4 === 5.U(3.W)) -> "b0001_000".U(7.W),
      (!a0_i & a2_a3_a4 === 6.U(3.W)) -> "b0001_000".U(7.W),
      (!a0_i & a2_a3_a4 === 7.U(3.W)) -> "b0001_000".U(7.W),
      (a0_i                         ) -> "b0001_000".U(7.W)
    )
  )
  m_pos_1_o := Mux1H(
    Seq(
      (!a0_i & a2_a3_a4 === 0.U(3.W)) -> "b1111_100".U(7.W),
      (!a0_i & a2_a3_a4 === 1.U(3.W)) -> "b1111_100".U(7.W),
      (!a0_i & a2_a3_a4 === 2.U(3.W)) -> "b1111_100".U(7.W),
      (!a0_i & a2_a3_a4 === 3.U(3.W)) -> "b1111_100".U(7.W),
      (!a0_i & a2_a3_a4 === 4.U(3.W)) -> "b1111_010".U(7.W),
      (!a0_i & a2_a3_a4 === 5.U(3.W)) -> "b1111_010".U(7.W),
      (!a0_i & a2_a3_a4 === 6.U(3.W)) -> "b1111_000".U(7.W),
      (!a0_i & a2_a3_a4 === 7.U(3.W)) -> "b1111_000".U(7.W),
      (a0_i                         ) -> "b1111_000".U(7.W)
    )
  )
  m_pos_2_o := Mux1H(
    Seq(
      (!a0_i & a2_a3_a4 === 0.U(3.W)) -> "b1110_100".U(7.W),
      (!a0_i & a2_a3_a4 === 1.U(3.W)) -> "b1110_010".U(7.W),
      (!a0_i & a2_a3_a4 === 2.U(3.W)) -> "b1110_000".U(7.W),
      (!a0_i & a2_a3_a4 === 3.U(3.W)) -> "b1110_000".U(7.W),
      (!a0_i & a2_a3_a4 === 4.U(3.W)) -> "b1101_110".U(7.W),
      (!a0_i & a2_a3_a4 === 5.U(3.W)) -> "b1101_100".U(7.W),
      (!a0_i & a2_a3_a4 === 6.U(3.W)) -> "b1101_100".U(7.W),
      (!a0_i & a2_a3_a4 === 7.U(3.W)) -> "b1101_010".U(7.W),
      (a0_i                         ) -> "b1101_010".U(7.W)
    )
  )
}


class r4_qds_spec extends Module{
  val io = IO(new Bundle {

  })

  val rem_i                     = IO(Input(UInt(9.W)))
  val sqrt_csa_val_neg_2_msbs_i = IO(Input(UInt(9.W)))
  val sqrt_csa_val_neg_1_msbs_i = IO(Input(UInt(9.W)))
  val sqrt_csa_val_pos_1_msbs_i = IO(Input(UInt(9.W)))
  val sqrt_csa_val_pos_2_msbs_i = IO(Input(UInt(9.W)))
  val m_neg_1_neg_2_i = IO(Input(UInt(7.W)))
  val m_neg_0_neg_2_i = IO(Input(UInt(7.W)))
  val m_pos_1_neg_2_i = IO(Input(UInt(7.W)))
  val m_pos_2_neg_2_i = IO(Input(UInt(7.W)))
  val m_neg_1_neg_1_i = IO(Input(UInt(7.W)))
  val m_neg_0_neg_1_i = IO(Input(UInt(7.W)))
  val m_pos_1_neg_1_i = IO(Input(UInt(7.W)))
  val m_pos_2_neg_1_i = IO(Input(UInt(7.W)))
  val m_neg_1_neg_0_i = IO(Input(UInt(7.W)))
  val m_neg_0_neg_0_i = IO(Input(UInt(7.W)))
  val m_pos_1_neg_0_i = IO(Input(UInt(7.W)))
  val m_pos_2_neg_0_i = IO(Input(UInt(7.W)))
  val m_neg_1_pos_1_i = IO(Input(UInt(7.W)))
  val m_neg_0_pos_1_i = IO(Input(UInt(7.W)))
  val m_pos_1_pos_1_i = IO(Input(UInt(7.W)))
  val m_pos_2_pos_1_i = IO(Input(UInt(7.W)))
  val m_neg_1_pos_2_i = IO(Input(UInt(7.W)))
  val m_neg_0_pos_2_i = IO(Input(UInt(7.W)))
  val m_pos_1_pos_2_i = IO(Input(UInt(7.W)))
  val m_pos_2_pos_2_i = IO(Input(UInt(7.W)))
  val prev_rt_dig_i   = IO(Input(UInt(5.W)))

  val rt_dig_o      = IO(Output(UInt(5.W)))

  val qds_sign = Wire(UInt(4.W))
  val qds_sign_spec = Wire(Vec(5,UInt(4.W)))
  qds_sign_spec(4) := Cat(
    (rem_i + sqrt_csa_val_neg_2_msbs_i + Cat(m_pos_2_neg_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_2_msbs_i + Cat(m_pos_1_neg_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_2_msbs_i + Cat(m_neg_0_neg_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_2_msbs_i + Cat(m_neg_1_neg_2_i, 0.U(2.W))).head(1)
  )
  qds_sign_spec(3) := Cat(
    (rem_i + sqrt_csa_val_neg_1_msbs_i + Cat(m_pos_2_neg_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_1_msbs_i + Cat(m_pos_1_neg_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_1_msbs_i + Cat(m_neg_0_neg_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_neg_1_msbs_i + Cat(m_neg_1_neg_1_i, 0.U(2.W))).head(1)
  )
  qds_sign_spec(2) := Cat(
    (rem_i + Cat(m_pos_2_neg_0_i, 0.U(2.W))).head(1),
    (rem_i + Cat(m_pos_1_neg_0_i, 0.U(2.W))).head(1),
    (rem_i + Cat(m_neg_0_neg_0_i, 0.U(2.W))).head(1),
    (rem_i + Cat(m_neg_1_neg_0_i, 0.U(2.W))).head(1)
  )
  qds_sign_spec(1) := Cat(
    (rem_i + sqrt_csa_val_pos_1_msbs_i + Cat(m_pos_2_pos_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_1_msbs_i + Cat(m_pos_1_pos_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_1_msbs_i + Cat(m_neg_0_pos_1_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_1_msbs_i + Cat(m_neg_1_pos_1_i, 0.U(2.W))).head(1)
  )
  qds_sign_spec(0) := Cat(
    (rem_i + sqrt_csa_val_pos_2_msbs_i + Cat(m_pos_2_pos_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_2_msbs_i + Cat(m_pos_1_pos_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_2_msbs_i + Cat(m_neg_0_pos_2_i, 0.U(2.W))).head(1),
    (rem_i + sqrt_csa_val_pos_2_msbs_i + Cat(m_neg_1_pos_2_i, 0.U(2.W))).head(1)
  )
  qds_sign := Mux1H(
    Seq(
      prev_rt_dig_i(4) -> qds_sign_spec(4),
      prev_rt_dig_i(3) -> qds_sign_spec(3),
      prev_rt_dig_i(2) -> qds_sign_spec(2),
      prev_rt_dig_i(1) -> qds_sign_spec(1),
      prev_rt_dig_i(0) -> qds_sign_spec(0)
    )
  )
  rt_dig_o := Cat(
    qds_sign(1,0) === "b11".U(2.W),
    qds_sign(1,0) === "b10".U(2.W),
    qds_sign(2,1) === "b10".U(2.W),
    qds_sign(3,2) === "b10".U(2.W),
    qds_sign(3,2) === "b00".U(2.W)
  )
}

class lzc(
           WIDTH: Int = 58,
           MODE: UInt = 1.U
         ) extends Module{
  val io = IO(new Bundle {

  })

  val in_i    = IO(Input(UInt(WIDTH.W)))
  val cnt_o   = IO(Output(UInt(log2Ceil(WIDTH).W)))
  val empty_o = IO(Output(UInt(1.W)))
  cnt_o := Mux(empty_o.asBool, 0.U,PriorityEncoder(Mux(MODE.asBool, Reverse(in_i), in_i)))
  empty_o := ~in_i.orR
}
