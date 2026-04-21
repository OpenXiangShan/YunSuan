package yunsuan.fpu.fmul.utils
import chisel3._
import chisel3.util._
import yunsuan.vector.LZD
import yunsuan.fpu._
import yunsuan.fpu.fmul.utils.Booth4Opt
import yunsuan.fpu.fmul.utils.CSAnTo2Opt1CSA4to2

class FMULS0ToS1Bundle(val totalWidth: Int) extends Bundle with FloatParams {
  val out_sum = UInt((2 * decimalWidth).W)
  val out_car = UInt((2 * decimalWidth).W)
  val needLeftShift = Bool()
  val leftShiftNoOverFlow = Bool()
  val shiftBits = UInt(shiftBitsWidth.W)
  val expc = UInt(exponentWidth.W)
  val sign = Bool()
  val isFMA = Bool()
  val rm = UInt(rmWidth.W)
  val resIsNAN = Bool()
  val resIsZero = Bool()
  val resIsInf = Bool()
  val flagsNV = Bool()
  val flagsOFToFADD = Bool()
}
class FMULS1ToS2Bundle(val totalWidth: Int) extends Bundle with FloatParams {
  val resForRounding = UInt((totalWidth + decimalWidth).W)
  val rm = UInt(rmWidth.W)
  val sticky = Bool()
  val resIsNAN = Bool()
  val resIsZero = Bool()
  val resIsInf = Bool()
  val flagsNV = Bool()
}
class FMULToFADDCtrlBundle(val totalWidth: Int) extends Bundle with FloatParams {
  val isFMA = Bool()
  val rm = UInt(rmWidth.W)
  val sticky = Bool()
  val resIsNaN = Bool()
  val resIsZero = Bool()
  val resIsInf = Bool()
  val flagsNV = Bool()
  val flagsOF = Bool()
}
class FMULS0(val totalWidth: Int, val version: Int = 0) extends Module with FloatParams {
  override def desiredName = (this.getClass.getName + s"_$version").split("\\.").last
  val io = IO(new Bundle() {
    val isFMA             = Input(Bool())
    val fp_a              = Input(UInt(totalWidth.W))
    val fp_b              = Input(UInt(totalWidth.W))
    val fpAisCanonicalNaN = Input(Bool())
    val fpBisCanonicalNaN = Input(Bool())
    val rm                = Input(UInt(rmWidth.W))
    val outToS1           = Output(new FMULS0ToS1Bundle(totalWidth))
  })
  val fp_a_exp = io.fp_a.tail(signWidth).head(exponentWidth)
  val fp_b_exp = io.fp_b.tail(signWidth).head(exponentWidth)
  val fp_a_exp_not_zero = fp_a_exp.orR
  val fp_b_exp_not_zero = fp_b_exp.orR
  val fp_a_exp_is_zero = !fp_a_exp_not_zero
  val fp_b_exp_is_zero = !fp_b_exp_not_zero
  val fp_a_raw_frac = io.fp_a(significandWidth - 1, 0)
  val fp_b_raw_frac = io.fp_b(significandWidth - 1, 0)
  val fp_a_raw_frac_not_zero = fp_a_raw_frac.orR
  val fp_b_raw_frac_not_zero = fp_b_raw_frac.orR
  val fp_a_exp_all_one = fp_a_exp.andR
  val fp_b_exp_all_one = fp_b_exp.andR
  val fp_a_is_canonical_nan = io.fpAisCanonicalNaN
  val fp_b_is_canonical_nan = io.fpBisCanonicalNaN
  val fp_a_is_nan = (fp_a_exp_all_one & fp_a_raw_frac_not_zero) | fp_a_is_canonical_nan
  val fp_b_is_nan = (fp_b_exp_all_one & fp_b_raw_frac_not_zero) | fp_b_is_canonical_nan
  val fp_a_is_snan = !fp_a_is_canonical_nan & fp_a_is_nan & !fp_a_raw_frac.head(1)
  val fp_b_is_snan = !fp_b_is_canonical_nan & fp_b_is_nan & !fp_b_raw_frac.head(1)
  val fp_a_is_zero = !fp_a_is_canonical_nan & fp_a_exp_is_zero & !fp_a_raw_frac_not_zero
  val fp_b_is_zero = !fp_b_is_canonical_nan & fp_b_exp_is_zero & !fp_b_raw_frac_not_zero
  val fp_a_is_inf = !fp_a_is_canonical_nan & fp_a_exp_all_one & !fp_a_raw_frac_not_zero
  val fp_b_is_inf = !fp_b_is_canonical_nan & fp_b_exp_all_one & !fp_b_raw_frac_not_zero
  io.outToS1.flagsNV := fp_a_is_snan | fp_b_is_snan | (fp_a_is_zero & fp_b_is_inf | fp_b_is_zero & fp_a_is_inf)
  io.outToS1.resIsNAN := fp_a_is_nan | fp_b_is_nan | (fp_a_is_zero & fp_b_is_inf | fp_b_is_zero & fp_a_is_inf)
  io.outToS1.resIsZero := !io.outToS1.resIsNAN && (fp_a_is_zero || fp_b_is_zero)
  io.outToS1.resIsInf := !io.outToS1.resIsNAN && (fp_a_is_inf || fp_b_is_inf)
  val fp_a_frac = Cat(fp_a_exp_not_zero, fp_a_raw_frac)
  val fp_b_frac = Cat(fp_b_exp_not_zero, fp_b_raw_frac)
  val booth4Opt = Module(new Booth4Opt(decimalWidth))
  booth4Opt.io.in_a := fp_a_frac
  booth4Opt.io.in_b := fp_b_frac
  val CSAnTo2 = Module(new CSAnTo2Opt1CSA4to2(booth4Opt.io.out_pp.length))
  CSAnTo2.io.in := booth4Opt.io.out_pp
  io.outToS1.out_sum := CSAnTo2.io.out_sum
  io.outToS1.out_car := CSAnTo2.io.out_car
  val fp_a_frac_lzd = Wire(UInt(decimalWidth.U.getWidth.W))
  val fp_b_frac_lzd = Wire(UInt(decimalWidth.U.getWidth.W))
  fp_a_frac_lzd := Mux(fp_a_exp_not_zero, 0.U, LZD(Cat(0.U, fp_a_raw_frac))) // 6 bits
  fp_b_frac_lzd := Mux(fp_b_exp_not_zero, 0.U, LZD(Cat(0.U, fp_b_raw_frac))) // 6 bits
  val expa_signed = Wire(UInt((exponentWidth + 3).W))
  val expb_signed = Wire(UInt((exponentWidth + 3).W))
  val expc_signed = Wire(UInt((exponentWidth + 3).W))
  val expa_normalized = Cat(fp_a_exp(exponentWidth - 1, 1), fp_a_exp(0) || (fp_a_exp === 0.U))
  val expb_normalized = Cat(fp_b_exp(exponentWidth - 1, 1), fp_b_exp(0) || (fp_b_exp === 0.U))
  val exp_bias = Cat(0.U(4.W), exponentBias.U) // 14 bits
  val expc_normalized = expa_normalized +& expb_normalized - exp_bias
  expa_signed := Cat(0.U(3.W), expa_normalized) - fp_a_frac_lzd
  expb_signed := Cat(0.U(3.W), expb_normalized) - fp_b_frac_lzd
  expc_signed := expa_signed + expb_signed - exp_bias // 14 bits
  val expc_signed_is_negtive = expc_signed.head(1).asBool
  val expc_signed_is_overflow = expc_signed(exponentWidth + 1, exponentWidth).orR
  val out_expc = Mux(expc_signed_is_negtive || io.outToS1.resIsZero, 0.U, Mux(expc_signed_is_overflow, Fill(exponentWidth, 1.U), expc_signed(exponentWidth - 1, 0)))
  val lzd_a_b = fp_a_frac_lzd | fp_b_frac_lzd
  val leftShift = Mux(lzd_a_b < expc_normalized, lzd_a_b, expc_normalized - 1.U)
  val needLeftShift = expc_normalized.asSInt > 0.S
  val rightShift = -expc_normalized + 1.U
  val leftShiftBits = leftShift(shiftBitsWidth - 1, 0)
  val rightShiftBits = Mux(rightShift(exponentWidth + 2, shiftBitsWidth).orR, Fill(shiftBitsWidth, 1.U), rightShift(shiftBitsWidth - 1, 0))
  val shiftBits = Mux(needLeftShift, leftShiftBits, rightShiftBits)
  io.outToS1.sign := io.fp_a.head(1) ^ io.fp_b.head(1)
  io.outToS1.isFMA := io.isFMA
  io.outToS1.expc := out_expc
  io.outToS1.needLeftShift := needLeftShift
  io.outToS1.shiftBits := shiftBits
  io.outToS1.rm := io.rm
  io.outToS1.leftShiftNoOverFlow := expc_normalized < lzd_a_b
  io.outToS1.flagsOFToFADD := !io.outToS1.resIsNAN && !expc_signed_is_negtive && expc_signed_is_overflow

  dontTouch(fp_a_exp)
  dontTouch(fp_b_exp)
  dontTouch(fp_a_frac)
  dontTouch(fp_b_frac)
  dontTouch(fp_a_frac_lzd)
  dontTouch(fp_b_frac_lzd)
  dontTouch(expa_signed)
  dontTouch(expb_signed)
  dontTouch(expc_signed)
  dontTouch(leftShift)
  dontTouch(leftShiftBits)
  dontTouch(rightShift)
  dontTouch(rightShiftBits)
  dontTouch(needLeftShift)
  dontTouch(shiftBits)
}
