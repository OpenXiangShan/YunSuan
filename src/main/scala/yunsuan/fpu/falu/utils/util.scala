package yunsuan.fpu.falu.utils.util

import chisel3._
import chisel3.util._
import yunsuan.fpu.FloatParams
import math.pow

import scala.reflect.runtime.universe._

trait AutoDontTouch extends Module {
  // 在构造器中自动标记信号
  {
    val mirror = runtimeMirror(this.getClass.getClassLoader)
    val instanceMirror = mirror.reflect(this)
    // 获取模块类的类型信息
    val moduleType = instanceMirror.symbol.toType
    // 遍历所有成员，筛选出显式定义的 val 字段
    moduleType.members
      .filter(_.isTerm) // 只处理 TermSymbol（字段或方法）
      .map(_.asTerm) // 转换为 TermSymbol
      .filter(_.isVal) // 只处理 val 字段
      .filter(!_.isPrivate) // 排除私有字段
      .filter(!_.isSynthetic) // 排除编译器生成的字段
      .foreach { term =>
        try {
          val field = instanceMirror.reflectField(term)
          field.get match {
            case data: Data => dontTouch(data) // 如果是 Data 类型，标记 dontTouch
            case _ => // 忽略非 Data 类型的字段
          }
        } catch {
          case ex: Exception =>
            println(s"Failed to reflect on field ${term.name}: ${ex.getMessage}")
        }
      }
  }
}

class Adder(val aw:Int, val bw:Int, val cw:Int, val is_sub:Boolean = false) extends Module {
  val io = IO(new Bundle() {
    val a = Input (UInt(aw.W))
    val b = Input (UInt(bw.W))
    val c = Output(UInt(cw.W))
  })
  if (cw > aw) {
    io.c := (if (is_sub) io.a -& io.b else io.a +& io.b)
  }
  else {
    io.c := (if (is_sub) io.a - io.b else io.a + io.b)
  }
}

class FarShiftRightWithMuxInvFirst(val srcW:Int,shiftValueW:Int) extends Module {
  val io = IO(new Bundle() {
    val src        = Input (UInt(srcW.W))
    val shiftValue = Input (UInt(shiftValueW.W))
    val result     = Output(UInt())
    val EOP        = Input(Bool())
  })
  def shiftRightWithMuxInvFirst(srcValue: UInt, shiftValue: UInt, EOP:Bool): UInt = {
    val vecLength  = shiftValue.getWidth + 1
    val res_vec    = Wire(Vec(vecLength,UInt(srcValue.getWidth.W)))
    res_vec(0)    := srcValue
    for (i <- 0 until shiftValue.getWidth) {
      res_vec(i+1) := Mux(shiftValue(i), Cat(Fill(1<<i,EOP),res_vec(i) >> (1<<i)), res_vec(i))
    }
    res_vec(vecLength-1)
  }
  io.result := shiftRightWithMuxInvFirst(io.src,io.shiftValue,io.EOP)
}

class ClosePathAdder(val adderWidth:Int, val adderType:String) extends Module {
  val io = IO(new Bundle() {
    val adder_op0    = Input(UInt(adderWidth.W))
    val adder_op1    = Input(UInt(adderWidth.W))
    val result       = Output(UInt((adderWidth+1).W))
  })
  if (adderType == "CS0" | adderType == "CS1") {
    io.result  :=  Cat(0.U,io.adder_op0) - Cat(0.U,io.adder_op1)
  }
  if (adderType == "CS2" | adderType == "CS3") {
    io.result  :=  Cat(io.adder_op0,0.U) - Cat(0.U,io.adder_op1)
  }
}

class FarPathAdder(val AW:Int) extends Module {
  val io = IO(new Bundle() {
    val A   = Input (UInt(AW.W))
    val B   = Input (UInt(AW.W))
    val result = Output(UInt(AW.W))
  })
  io.result  := io.A + io.B
}

object LZD {
  def apply(in: UInt): UInt = PriorityEncoder(Reverse(Cat(in, 1.U)))
}

object LeftShiftHighFirst {
  def apply[TI <: Bits, TS <: Bits] (in: TI, shift: TS): Bits = {
    doLeftShiftHighFirst(in, shift)
  }
  def doLeftShiftHighFirst[TI <: Bits, TS <: Bits] (in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val inWidth = in.getWidth
    val amount = pow(2, length - 1).intValue
    val shifted = if (inWidth == 1) Mux(shift.head(1).asBool, 0.U, in)
    else Mux(shift.head(1).asBool, Cat(in.tail(amount), Fill(amount, 0.U)), in)
    if (length == 1) shifted else doLeftShiftHighFirst(shifted, shift.tail(1))
  }
}

object RightShiftHighFirst {
  def apply[TI <: Bits, TS <: Bits] (in: TI, shift: TS): Bits = {
    doRightShiftHighFirst(in, shift)
  }
  def doRightShiftHighFirst[TI <: Bits, TS <: Bits] (in: TI, shift: TS): Bits = {
    val length = shift.getWidth
    val inWidth = in.getWidth
    val amount = pow(2, length - 1).intValue
    val shifted = if (inWidth == 1) Mux(shift.head(1).asBool, 0.U, in)
    else if (amount >= inWidth) Mux(shift.head(1).asBool, Fill(inWidth, 0.U), in)
    else Mux(shift.head(1).asBool, Cat(Fill(amount, 0.U), in.head(amount)), in)
    if (length == 1) shifted else doRightShiftHighFirst(shifted, shift.tail(1))
  }
}

object LeftShiftLowFirst {
  def apply(in: UInt, shift: UInt): UInt = {
    doLeftRightLowFirstLogic(in, shift, 0)
  }
  def doLeftRightLowFirstLogic(in: UInt, shift: UInt, idx: Int): UInt = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val shiftValue = shift & (1 << idx).U
    val shifted = (in << shiftValue).asUInt
    if (idx == length - 1) shifted else doLeftRightLowFirstLogic(shifted, shift, idx + 1)
  }
}

object RightShiftLowFirst {
  def apply(in: UInt, shift: UInt): UInt = {
    doRightShiftLowFirst(in, shift, 0)
  }
  def doRightShiftLowFirst(in: UInt, shift: UInt, idx: Int): UInt = {
    val length = shift.getWidth
    val amount = pow(2, length - 1).intValue
    val shiftValue = shift & (1 << idx).U
    val shifted = (in >> shiftValue).asUInt
    if (idx == length - 1) shifted else doRightShiftLowFirst(shifted, shift, idx + 1)
  }
}


class AddEqualDetect(val width:Int) extends Module {
  // detect K == A + B
  val io = IO(new Bundle() {
    val A   = Input(UInt(width.W))
    val B   = Input(UInt(width.W))
    val K   = Input(UInt(width.W))
    val equal = Output(Bool())
  })
  val A = io.A
  val B = io.B
  val K = io.K
  val cRequired = Wire(Vec(width, Bool()))
  val cProduced = Wire(Vec(width, Bool()))
  for (i <- 0 until width){
    cRequired(i) := A(i) ^ B(i) ^ K(i)
    if (i == 0) cProduced(i) := false.B
    else cProduced(i) := (A(i-1) ^ B(i-1)) && !K(i-1) || A(i-1) && B(i-1)
  }
  io.equal := cRequired === cProduced
}

class AddEqualDetectSimp(val width:Int) extends Module {
  // detect K == A + B
  val io = IO(new Bundle() {
    val A   = Input(UInt(width.W))
    val B   = Input(UInt(width.W))
    val K   = Input(UInt(width.W))
    val equal = Output(Bool())
  })
  val A = io.A
  val B = io.B
  val K = io.K
  io.equal := K === A + B
}

object LZA {
  def apply(a: UInt, b: UInt): UInt = lza(a,b)
  def lza(a: UInt, b: UInt): UInt = {
    require(a.getWidth == b.getWidth)
    val n = a.getWidth
    // A>B
    val E = (for (i <- (0 until n).reverse) yield {
      if (i == 0) a(0)^b(0)
      else ( a(i) ^ b(i) ) & ( a(i-1) | (~b(i-1)) )
    }.asUInt).reduce(Cat(_,_))
    LZD(E)
  }
}

object LZASeqGen {
  def apply(a: UInt, b: UInt): UInt = lzaSeq(a,b)
  def lzaSeq(a: UInt, b: UInt): UInt = {
    require(a.getWidth == b.getWidth)
    val n = a.getWidth
    // A>B
    val E = (for (i <- (0 until n).reverse) yield {
      if (i == 0) a(0)^b(0)
      else ( a(i) ^ b(i) ) & ( a(i-1) | (~b(i-1)) )
    }.asUInt).reduce(Cat(_,_))
    E
  }
}

object ExpMaskGen {
  def apply(exp: UInt, maskWidth: Int): UInt = expMaskGen(exp, maskWidth)
  def expMaskGen(exp: UInt, maskWidth: Int): UInt = {
    val mask = (for (i <- (1 until maskWidth + 1)) yield {
      if (i == 1) (exp === 0.U) || (exp === i.U)
      else exp === i.U
    }.asUInt).reduce(Cat(_,_))
    mask
  }
}
object RoundGen extends {val totalWidth: Int = 16} with FloatParams {
  def apply(rm: UInt, guard: Bool, round: Bool, sticky: Bool, sign: Bool): Bool = roundGen(rm: UInt, guard: Bool, round: Bool, sticky: Bool, sign: Bool)
  def roundGen(rm: UInt, guard: Bool, round: Bool, sticky: Bool, sign: Bool): Bool = {
    val isRNE = rm === RNE.U
    val isRTZ = rm === RTZ.U
    val isRDN = rm === RDN.U
    val isRUP = rm === RUP.U
    val isRMM = rm === RMM.U
    val roundUp = isRNE && round && (guard || sticky) ||
      isRDN && sign && (round || sticky) ||
      isRUP && (!sign) && (round || sticky) ||
      isRMM && round
    roundUp
  }
}
class LeadingZeroAnticipator(val w:Int) extends Module {
  val io = IO(new Bundle() {
    val a = Input (UInt(w.W))
    val b = Input (UInt(w.W))
    val lza = Output(UInt(w.U.getWidth.W))
  })
  io.lza := LZA(io.a,io.b)
}

case class LzcPart(z: Vec[Bool], v: Bool)

object LzcPart {
  def merge(a: LzcPart, b: LzcPart): LzcPart = {
    val Z_neg = Wire(Vec(a.z.length + 1, Bool()))
    val V_neg = a.v | b.v

    Z_neg(a.z.length) := b.v

    for (j <- 0 until a.z.length) {
      val temp1 = b.v & b.z(j)
      val temp2 = (!b.v) & a.z(j)
      Z_neg(j) := temp1 | temp2
    }

    LzcPart(Z_neg, V_neg)
  }
}

class lzc_parameters(bitWidth:Int) extends Module {
  val io = IO(new Bundle{
    val X = Input(UInt(bitWidth.W))
    val Z = Output(UInt(log2Up(bitWidth).W))
    val V = Output(Bool())
  })
  require(bitWidth > 0, "Bit width muse be greater than 0.")
  val width = Math.pow(2, log2Up(bitWidth)).toInt
  val x = (io.X.asBools.reverse ++ Seq.fill(width - bitWidth)(true.B)).reverse
  val groups: Seq[Seq[Bool]] = x.grouped(8).toSeq

  val localParts: Seq[LzcPart] = groups.map {group =>
    val zLocal = Wire(Vec(3, Bool()))
    val vLocal = Wire(Bool())
    /**
     * -V = x7 + x6 + x5 + x4 + x3 + x2 + x1 + x0
     * -z2 = x4 + x5 + x6 + x7
     * -z1 = (x2 + x3)(-x4)(-x5) + x6 + x7
     * -z0 = x7 + x5(-x6) + x3(-x4)(-x6) + x1(-x2)(-x4)(-x6)
     */
    zLocal(2) := group(7) | group(6) | group(5) | group(4)
    zLocal(1) := group(7) | group(6) | !group(5) & !group(4) & (group(3) | group(2))
    zLocal(0) := group(7) | (!group(6) & group(5)) | (!group(6) & !group(4) & group(3)) | (!group(6) & !group(4) & !group(2) & group(1))
    vLocal := group.reduce(_|_)

    LzcPart(zLocal, vLocal)
  }

  def mergeAll(parts: Seq[LzcPart]): LzcPart = {
    if (parts.length == 1) parts.head
    else {
      val paired = parts.grouped(2).map {
        case Seq(a, b) => LzcPart.merge(a, b)
        case Seq(a) => a
      }.toSeq
      mergeAll(paired)
    }
  }

  val finalPart = mergeAll(localParts)

  val finalZVec = VecInit(finalPart.z.map(!_))
  val z_out = finalZVec.asUInt
  val v_out = !finalPart.v
  io.Z := z_out
  io.V := v_out
}
class lzd(bitWidth:Int) extends Module {
  val io = IO(new Bundle{
    val in = Input(UInt(bitWidth.W))
    val out = Output(UInt(log2Up(bitWidth).W))
  })
  io.out := LZD(io.in)
}
class PassThrough extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt())
    val out = Output(UInt())
  })
  io.out := io.in
}
object PassThrough {
  def apply(input: UInt): UInt = {
    val passThrough = Module(new PassThrough)
    passThrough.io.in := input
    passThrough.io.out
  }
}
class Add extends Module {
  val io = IO(new Bundle {
    val inA  = Input(UInt())
    val inB  = Input(UInt())
    val out = Output(UInt())
  })
  io.out := io.inA + io.inB
}
object Add {
  def apply(inputA: UInt, inputB: UInt): UInt = {
    val moduleAdd = Module(new Add)
    moduleAdd.io.inA := inputA
    moduleAdd.io.inB := inputB
    moduleAdd.io.out
  }
}