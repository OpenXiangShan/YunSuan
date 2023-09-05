//  package yunsuan.vector.VectorConvert
//
// import chisel3._
// import chisel3.util._
// import yunsuan.vector.VectorConvert.RoundingModle._
// import yunsuan.vector.VectorConvert.utils.leftFirstOne
//
// import scala.math.{max, round}
//
// // convertFromInt：此操作可以将所有支持的有符号和无符号整数格式转换为所有支持的浮点算术格式。
// // 当整数值可以用两种格式表示时，整数值会被精确地从整数格式转换为浮点格式；
// // 如果转换后的值不能在目标格式中精确表示，则根据适用的舍入方向属性确定结果，并产生浮点不精确异常或溢出异常。
// // 有符号整数零的符号在转换中会被保留；无符号整数零被转换为+0
//
//  class Int2FPIO(srcFormat: IntFormat, toFormat: FloatFormat) extends Bundle {
//    val src = Input(UInt(srcFormat.width.W))
//    val rm = Input(UInt(3.W))
//    val result = Output(UInt(toFormat.width.W))
//    val fflags = Output(UInt(5.W))
//  }
//
//
//  class Int2FP(srcFormat: IntFormat, toFormat: FloatFormat) extends Module {
//    val io = IO(new Int2FPIO(srcFormat, toFormat))
//    val src = io.src
//    val rm = io.rm
//
//    val bias = toFormat.bias
//    val expWidth = toFormat.expWidth
//    val fracWidth = toFormat.fracWidth
//    val toWidth = toFormat.width
//
//    val hasSign = srcFormat.hasSign
//    val srcWidth = srcFormat.width
//
//    val widthEffect = max(toWidth, srcWidth) // todo: 思考这里需不需要 采用object的话
//    val fracInter = Wire(UInt(widthEffect.W))
//
//    val exp = Wire(UInt(expWidth.W))
//    val frac = Wire(UInt(fracWidth.W))
//    val fflags = Wire(UInt(3.W))
//    val result = Wire(UInt(toWidth.W))
//    val result1H = Wire(UInt(3.W))
//
//
//    val sign = if (hasSign) src.head(1).asBool else false.B
//    val value = if (hasSign) Mux(sign, (~src.tail(1)).asUInt + 1.U, src.tail(1)) else src //note：注意这里有无问题，todo：前面Int7位，后面UInt8位
//    val firstOne = leftFirstOne(value)
//    val isZero = !value.orR
//
//    exp := firstOne + bias.U
//    fracInter := value << (widthEffect.U - firstOne) //add 1 to drop the highest bit
//
//    //不管结果如何，先进rounder再说
//    val rounder = Module(new RoundingUnit(fracWidth))
//    rounder.io.in := fracInter.head(fracWidth)
//    rounder.io.roundIn := fracInter.tail(fracWidth).head(1)
//    rounder.io.stickyIn := fracInter.tail(fracWidth + 1).orR  //todo 找到准确的那几位来进行orR
//    rounder.io.signIn := sign
//    rounder.io.rm := rm
//    val fracRounded = rounder.io.out
//    val nxRounded = rounder.io.inexact
//    val cout = rounder.io.cout
//    val expRounded = Mux(cout, exp + 1.U, exp)
//    val overflowRounded = Mux(cout, exp > (toFormat.maxExp - 1).U, exp > toFormat.maxExp.U)
//
//    val rmin =
//      io.rm === RTZ || (io.rm === RDN && !sign) || (io.rm === RUP && sign)
//
//    val resultOverflow = Mux(rmin,
//      toFormat.maxExp.U(expWidth.W) ## ~0.U(fracWidth.W), // great FN
//      (toFormat.maxExp + 1).U(expWidth.W) ## 0.U(fracWidth.W) // INF
//    )
//
//    val nv = false.B
//    val dz = false.B
//    val of = overflowRounded
//    val uf = false.B
//    val nx = overflowRounded || nxRounded
//
//    result1H := overflowRounded ## isZero ## (!overflowRounded && !isZero)
//
//    val resultMap = VecInit((-1 to 1).map {
//      case -1 => sign ## resultOverflow // outofRange
//      case 0 => sign ## 0.U(toWidth - 1) // 0 是不是包含在下面一种情况下,不能因为在rounder时没有排除这种情况,全零进rounder出来的1##全零是非零
//      case 1 => sign ## expRounded ## fracRounded // normal
//    })
//
//    result := Mux1H(result1H.asBools.reverse, resultMap)
//    fflags := Cat(nv, dz, of, uf, nx)
//    io.result := result
//    io.fflags := fflags
//  }
//
//  object Int2FP {
//    def apply(srcFormat: IntFormat,
//              toFormat: FloatFormat,
//             )(input: UInt,
//               rm: UInt
//             ): (UInt, UInt) = {
//      val m = Module(new Int2FP(srcFormat, toFormat))
//      m.io.src := input
//      m.io.rm := rm
//      (m.io.result, m.io.fflags)
//    }
//  }
