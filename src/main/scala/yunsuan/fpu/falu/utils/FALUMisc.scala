package yunsuan.fpu.falu.utils

import chisel3._
import chisel3.util._

class FALUMiscInput extends Bundle {
  val src0                  = UInt(64.W)
  val src1                  = UInt(64.W)
  val isfp16                = Bool()
  val isfp32                = Bool()
  val isfp64                = Bool()
  val src0F16IsCanonicalNaN = Bool()
  val src0F32IsCanonicalNaN = Bool()
  val src1F16IsCanonicalNaN = Bool()
  val src1F32IsCanonicalNaN = Bool()
  val isFmax                = Bool()
  val isFmin                = Bool()
  val isMaxMinM             = Bool()
  val isFsgnj               = Bool()
  val isFsgnjn              = Bool()
  val isFsgnjx              = Bool()
  val isFsgn                = Bool()
}

class FALUMiscOutput extends Bundle {
  val result = Output(UInt(64.W))
  val fflags = Output(UInt(5.W))
}

class FALUMiscIO extends Bundle {
  val in  = Input(new FALUMiscInput())
  val out = Output(new FALUMiscOutput())
}

class FALUMisc() extends Module {
  val io = IO(new FALUMiscIO())
  
  val src0                    = io.in.src0
  val src1                    = io.in.src1
  val isfp16                  = io.in.isfp16
  val isfp32                  = io.in.isfp32
  val isfp64                  = io.in.isfp64
  val src0F16IsCanonicalNaN   = io.in.src0F16IsCanonicalNaN
  val src0F32IsCanonicalNaN   = io.in.src0F32IsCanonicalNaN
  val src1F16IsCanonicalNaN   = io.in.src1F16IsCanonicalNaN
  val src1F32IsCanonicalNaN   = io.in.src1F32IsCanonicalNaN
  val isFmax                  = io.in.isFmax
  val isFmin                  = io.in.isFmin
  val isMaxMinM               = io.in.isMaxMinM
  val isFsgnj                 = io.in.isFsgnj
  val isFsgnjn                = io.in.isFsgnjn
  val isFsgnjx                = io.in.isFsgnjx
  val isFsgn                  = io.in.isFsgn

  val src0F16Sign = src0(15)
  val src0F32Sign = src0(31)
  val src0F64Sign = src0(63)
  val src1F16Sign = src1(15)
  val src1F32Sign = src1(31)
  val src1F64Sign = src1(63)
  val src0Sign = Mux1H(Seq(
    isfp16 -> src0F16Sign,
    isfp32 -> src0F32Sign,
    isfp64 -> src0F64Sign
  ))
  val src1Sign = Mux1H(Seq(
    isfp16 -> src1F16Sign,
    isfp32 -> src1F32Sign,
    isfp64 -> src1F64Sign
  ))

  val src0F16IsNan = src0(14, 10).andR && src0(9, 0).orR
  val src0F32IsNan = src0(30, 23).andR && src0(22, 0).orR
  val src0F64IsNan = src0(62, 52).andR && src0(51, 0).orR
  val src1F16IsNan = src1(14, 10).andR && src1(9, 0).orR
  val src1F32IsNan = src1(30, 23).andR && src1(22, 0).orR
  val src1F64IsNan = src1(62, 52).andR && src1(51, 0).orR
  val src0IsFpCanonicalNAN = Mux1H(Seq(
    isfp16 -> src0F16IsCanonicalNaN,
    isfp32 -> src0F32IsCanonicalNaN
  ))
  val src1IsFpCanonicalNAN = Mux1H(Seq(
    isfp16 -> src1F16IsCanonicalNaN,
    isfp32 -> src1F32IsCanonicalNaN
  ))
  val src0IsNan = Mux1H(Seq(
    isfp16 -> src0F16IsNan,
    isfp32 -> src0F32IsNan,
    isfp64 -> src0F64IsNan
  )) || src0IsFpCanonicalNAN
  val src1IsNan = Mux1H(Seq(
    isfp16 -> src1F16IsNan,
    isfp32 -> src1F32IsNan,
    isfp64 -> src1F64IsNan
  )) || src1IsFpCanonicalNAN
  val src0IsSNan = Mux1H(Seq(
    isfp16 -> !src0(9),
    isfp32 -> !src0(22),
    isfp64 -> !src0(51)
  )) && src0IsNan && !src0IsFpCanonicalNAN
  val src1IsSNan = Mux1H(Seq(
    isfp16 -> !src1(9),
    isfp32 -> !src1(22),
    isfp64 -> !src1(51)
  )) && src1IsNan && !src1IsFpCanonicalNAN

  val src0Abs = Cat(src0(62, 32), isfp32 || src0F32Sign, src0(30, 16), isfp16 || src0F16Sign, src0(14, 0))
  val src1Abs = Cat(src1(62, 32), isfp32 || src1F32Sign, src1(30, 16), isfp16 || src1F16Sign, src1(14, 0))
  val src0AbsLt = src0Abs < src1Abs
  val src0AbsEq = src0Abs === src1Abs
  
  // fmax/fmin/fmaxm/fminm
  val selSrc0ForMin = Mux1H(
    Seq(
    !src0Sign && !src1Sign,
    !src0Sign && src1Sign,
    src0Sign && !src1Sign,
    src0Sign && src1Sign,
    ),
    Seq(
      src0AbsLt || src0AbsEq,
      false.B,
      true.B,
      !src0AbsLt
    )
  )
  val bothSrcNan    =  src0IsNan &&  src1IsNan
  val anySrcNan     =  src0IsNan ||  src1IsNan
  val nonSrcNan     = ~src0IsNan && ~src1IsNan
  val onlySrc0IsNan =  src0IsNan && !src1IsNan
  val onlySrc1IsNan = !src0IsNan &&  src1IsNan
  val resultIsCanonicalNAN = (isMaxMinM && anySrcNan) || (!isMaxMinM && bothSrcNan)
  val resultIsSrc0 = (nonSrcNan && ((isFmax && !selSrc0ForMin) || (isFmin && selSrc0ForMin)))  || !isMaxMinM && onlySrc1IsNan
  val resultIsSrc1 = (nonSrcNan && ((isFmax &&  selSrc0ForMin) || (isFmin && !selSrc0ForMin))) || !isMaxMinM && onlySrc0IsNan
  val resultCanonicalNAN = Mux1H(Seq(
    isfp16 -> Cat(Fill(48, 1.U(1.W)), 0.U(1.W), Fill(6, 1.U(1.W)),  0.U(9.W)),
    isfp32 -> Cat(Fill(32, 1.U(1.W)), 0.U(1.W), Fill(9, 1.U(1.W)),  0.U(22.W)),
    isfp64 -> Cat(                    0.U(1.W), Fill(12, 1.U(1.W)), 0.U(51.W))
  ))
  val resultMaxMin = Mux1H(Seq(
    resultIsCanonicalNAN -> resultCanonicalNAN,
    resultIsSrc0         -> src0,
    resultIsSrc1         -> src1
  ))

  // fsgnj/fsgnjn/fsgnjx
  val fsgnSrc0     = Mux(src0IsFpCanonicalNAN, resultCanonicalNAN, src0)
  val fsgnSrc0Sign = Mux(src0IsFpCanonicalNAN, 0.U(1.W), src0Sign)
  val fsgnSrc1Sign = Mux(src1IsFpCanonicalNAN, 0.U(1.W), src1Sign)
  val fsgnSignNegate = !fsgnSrc1Sign
  val fsgnSignXor    =  fsgnSrc0Sign ^ fsgnSrc1Sign
  val fsgnSign = Mux1H(Seq(
    isFsgnj  -> fsgnSrc1Sign,
    isFsgnjn -> fsgnSignNegate,
    isFsgnjx -> fsgnSignXor
  ))
  val resultFsgn = Mux1H(Seq(
    isfp16 -> Cat(Fill(48, 1.U(1.W)), fsgnSign, fsgnSrc0(14, 0)),
    isfp32 -> Cat(Fill(32, 1.U(1.W)), fsgnSign, fsgnSrc0(30, 0)),
    isfp64 -> Cat(                    fsgnSign, fsgnSrc0(62, 0))
  ))

  io.out.result := Mux(isFsgn, resultFsgn, resultMaxMin)
  io.out.fflags := Mux(isFsgn, 0.U(5.W), Cat(src0IsSNan || src1IsSNan, 0.U(4.W)))
}