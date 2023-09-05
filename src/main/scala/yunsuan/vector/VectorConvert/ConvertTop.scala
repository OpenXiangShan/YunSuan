//package yunsuan.vector.VectorConvert
//
//import chisel3._
//import chisel3.util._
//import chisel3.util.experimental.decode._
//import yunsuan.encoding.Opcode.VfCvtOpcode._
//import yunsuan.vector.VectorConvert.RoundingModle._
//
//// todo
////according to uopindex, 1: high64 0:low64
//class VectorCvtTop(vlen :Int, xlen: Int) extends Module{
//  val uop_idx = UInt(6.W)
//  val src = Input(Vec(vlen/xlen, UInt(xlen.W)))
//  val opType = Input(UInt(8.W))
//  val sew = Input(UInt(2.W))
//  val rm = Input(UInt(3.W))
//
//  val result = Output(Vec(vlen/xlen, UInt(xlen.W)))
//  val fflags = Output(Vec(vlen/xlen, UInt((5 * (xlen / 16)).W)))
//  val inIsFp = opType.head(1)
//  val outIsFp = opType.tail(1).head(1).asBool
//
//  val widen = opType(4, 3) // 0->single 1->widen 2->norrow => width of result
//  val isSingle = !widen(1) & !widen(0)
//  val isWiden = !widen(1) & widen(0)
//  val isNorrow = widen(1) & !widen(0)
//
//  val input1H = Wire(UInt(8.W))
//  input1H := chisel3.util.experimental.decode.decoder(
//    widen ## sew ## inIsFp,
//    TruthTable(
//      Seq(
//        BitPat("b00_01_0") -> BitPat("b0000_0100"), // i16
//        BitPat("b00_01_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b00_10_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b00_10_1") -> BitPat("b0010_0000"), // f32
//        BitPat("b00_11_0") -> BitPat("b0100_0000"), // i64
//        BitPat("b00_11_1") -> BitPat("b1000_0000"), // f64
//
//        BitPat("b01_00_0") -> BitPat("b0000_0001"), // i8
//        BitPat("b01_01_0") -> BitPat("b0000_0100"), // i16
//        BitPat("b01_01_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b01_10_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b01_10_1") -> BitPat("b0010_0000"), // f32
//
//        BitPat("b10_00_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b10_01_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b10_01_1") -> BitPat("b0010_0000"), // f32
//        BitPat("b10_10_0") -> BitPat("b0100_0000"), // i64
//        BitPat("b10_10_1") -> BitPat("b1000_0000"), // f64
//      ),
//      BitPat("b0000_0010") //f8, don't exist
//    )
//  )
//
//  // output format i8, f8(don't exist), i16, f16, i32, f32, i64, f64
//  val output1H = Wire(UInt(8.W))
//  output1H := chisel3.util.experimental.decode.decoder(
//    widen ## sew ## outIsFp,
//    TruthTable(
//      Seq(
//        BitPat("b00_01_0") -> BitPat("b0000_0100"), // i16
//        BitPat("b00_01_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b00_10_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b00_10_1") -> BitPat("b0010_0000"), // f32
//        BitPat("b00_11_0") -> BitPat("b0100_0000"), // i64
//        BitPat("b00_11_1") -> BitPat("b1000_0000"), // f64
//
//        BitPat("b01_00_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b01_01_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b01_01_1") -> BitPat("b0010_0000"), // f32
//        BitPat("b01_10_0") -> BitPat("b0100_0000"), // i64
//        BitPat("b01_10_1") -> BitPat("b1000_0000"), // f64
//
//        BitPat("b10_00_0") -> BitPat("b0000_0001"), // i8
//        BitPat("b10_01_0") -> BitPat("b0000_0100"), // i16
//        BitPat("b10_01_1") -> BitPat("b0000_1000"), // f16
//        BitPat("b10_10_0") -> BitPat("b0001_0000"), // i32
//        BitPat("b10_10_1") -> BitPat("b0010_0000"), // f32
//      ),
//      BitPat("b0000_0010") //f8, don't exist
//    )
//  )
//  dontTouch(input1H)
//  dontTouch(output1H)
//
//  val outputWidth1H = VecInit((0 to 3).map(i => output1H(2 * i) | output1H(2 * i + 1))).asUInt
//
//  val in0 = Mux(isWiden,
//    Mux(uop_idx(0), src(1).tail(32), src(0).tail(32)),
//    src(0)
//  )
//
//  val in1 = Mux(isWiden,
//    Mux(uop_idx(0), src(1).head(32), src(0).head(32)),
//    src(1)
//  )
//
//
//  val vectorCvt0 = Module(new VectorCvt(64))
//  vectorCvt0.src := in0
//  vectorCvt0.opType := opType
//  vectorCvt0.sew := sew
//  vectorCvt0.rm := rm
//
//  val vectorCvt1 = Module(new VectorCvt(64))
//  vectorCvt1.src := in1
//  vectorCvt1.opType := opType
//  vectorCvt1.sew := sew
//  vectorCvt1.rm := rm
//
//  result := Mux(isNorrow,
//    vectorCvt1.io.result.tail(32) ## vectorCvt0.io.result.tail(32),
//    vectorCvt1.io.result ## vectorCvt1.io.result)
//
//  fflags := Mux1H(outputWidth1H, Seq( // todo: map between fflags and result
//    vectorCvt1.io.fflags ## vectorCvt0.io.fflags,
//    Mux(isNorrow, vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10), vectorCvt1.io.fflags ## vectorCvt0.io.fflags),
//    Mux(isNorrow, vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0), vectorCvt1.io.fflags.tail(10) ## vectorCvt0.io.fflags.tail(10)),
//    vectorCvt1.io.fflags(4,0) ## vectorCvt0.io.fflags(4,0)
//  ))
//}