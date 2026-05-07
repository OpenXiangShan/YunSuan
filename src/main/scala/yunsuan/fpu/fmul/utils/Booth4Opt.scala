package yunsuan.fpu.fmul.utils
import chisel3._
import chisel3.util._

class Booth4Opt(width: Int) extends Module{
  override def desiredName = (this.getClass.getName + s"_$width").split("\\.").last
  val fracWidth = (width - 1)
  val originalPPNum = fracWidth / 2 + 1
  val additionalPPNum = 2
  val outPPNum = originalPPNum + additionalPPNum
  val outPPWidth  = 2 * width
  val io = IO(new Bundle() {
    val in_a   = Input(UInt(width.W))
    val in_b   = Input(UInt(width.W))
    val out_pp = Output(Vec(outPPNum, UInt(outPPWidth.W)))
  })
  val in_a_int = io.in_a.head(1)
  val in_b_int = io.in_b.head(1)
  val in_a_frac = io.in_a.tail(1)
  val in_b_frac = io.in_b.tail(1)
  //get booth encode
  val booth_seq = Wire(Vec(originalPPNum, UInt(3.W)))
  val booth_4bit_onehot = Wire(Vec(originalPPNum, UInt(4.W)))
  val in_b_frac_cat = Cat( Fill(2 - (fracWidth % 2), 0.U), in_b_frac, 0.U)
  for (i <- 0 until originalPPNum) {
    booth_seq(i) := in_b_frac_cat(i * 2 + 2, i * 2)
    booth_4bit_onehot(i) := 0.U
    switch(booth_seq(i)){
      is("b001".U) {booth_4bit_onehot(i) := "b1000".U}
      is("b010".U) {booth_4bit_onehot(i) := "b1000".U}
      is("b011".U) {booth_4bit_onehot(i) := "b0100".U}
      is("b100".U) {booth_4bit_onehot(i) := "b0001".U}
      is("b101".U) {booth_4bit_onehot(i) := "b0010".U}
      is("b110".U) {booth_4bit_onehot(i) := "b0010".U}
    }
  }
  // generate partial products
  val pp_seq = Wire(Vec(originalPPNum, UInt((fracWidth + 1).W)))
  val sign_seq = Wire(Vec(originalPPNum, UInt(1.W)))
  for (i <- 0 until originalPPNum) {
    sign_seq(i) := booth_4bit_onehot(i)(1) | booth_4bit_onehot(i)(0)
    pp_seq(i) := Fill(fracWidth + 1, booth_4bit_onehot(i)(3)) & Cat(0.U, in_a_frac) |
      Fill(fracWidth + 1, booth_4bit_onehot(i)(2)) & Cat(in_a_frac, 0.U) |
      Fill(fracWidth + 1, booth_4bit_onehot(i)(1)) & Cat(1.U, ~in_a_frac) |
      Fill(fracWidth + 1, booth_4bit_onehot(i)(0)) & Cat(~in_a_frac, 1.U)
  }
  val addend_seq = Wire(Vec(originalPPNum, UInt((outPPWidth).W)))
  val outNumBeforeLast = originalPPNum - 2
  val outNumLast = originalPPNum - 1
  for (i <- 0 until originalPPNum) {
    val head_first_one_width = fracWidth - 4 - 2 * (i - 1)
    val tail_zero_width = 2 * (i - 1)
    i match {
      case 0 => addend_seq(i) := Cat(0.U((fracWidth - 4).W), ~sign_seq(i), sign_seq(i), sign_seq(i), pp_seq(0))
      case 1 => addend_seq(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq(i), 0.U, sign_seq(i - 1))
      case `outNumBeforeLast` => addend_seq(i) := Cat(1.U, ~sign_seq(i), pp_seq(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      case `outNumLast` => {
        if (fracWidth % 2 == 0) addend_seq(i) := Cat(1.U, pp_seq(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
        else addend_seq(i) := Cat("b11".U, pp_seq(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
      }
      case _ => addend_seq(i) := Cat(1.U(head_first_one_width.W), ~sign_seq(i), pp_seq(i), 0.U, sign_seq(i - 1), 0.U(tail_zero_width.W))
    }
  }
  val additionalPP_0 = Wire(UInt(outPPWidth.W))
  val additionalPP_1 = Wire(UInt(outPPWidth.W))
  additionalPP_0 := Mux(in_a_int.asBool, Cat(0.U(2.W), in_b_frac, 0.U((width - 1).W)), 0.U)
  additionalPP_1 := Mux(in_b_int.asBool, Cat(0.U(1.W), in_a_int & in_b_int, in_a_frac, 0.U((width - 1).W)), 0.U)
  io.out_pp := addend_seq :+ additionalPP_0 :+ additionalPP_1
}
