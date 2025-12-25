package yunsuan.vector.mac

import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import circt.stage._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._
import yunsuan.encoding.Opcode.VimacOpcode.isMacc
import yunsuan.VidivType.vdiv
import yunsuan.encoding.Opcode.VimacOpcode.isFixP

class VIMac64bStage3 extends Module {
  val io = IO(new Bundle {
    val sumFinalNonFixPS2 = Input(UInt(152.W))
    val sumFinalFixPS2    = Input(UInt(152.W))
    val highHalfS2        = Input(Bool())
    val uopIdxS2          = Input(UInt(6.W))
    val widenS2           = Input(Bool())
    val vxrmS2            = Input(UInt(2.W))
    val isFixPS2          = Input(Bool())
    val sewIs8S2          = Input(Bool())
    val sewIs16S2         = Input(Bool())
    val sewIs32S2         = Input(Bool())
    val sewIs64S2         = Input(Bool())

    val vd    = Output(UInt(64.W))
    val vxsat = Output(UInt(8.W))
  })

  val sumFinalNonFixPS2 = io.sumFinalNonFixPS2
  val sumFinalFixPS2    = io.sumFinalFixPS2
  val highHalfS2        = io.highHalfS2
  val uopIdxS2          = io.uopIdxS2
  val widenS2           = io.widenS2
  val vxrmS2            = io.vxrmS2
  val isFixPS2          = io.isFixPS2
  val sewIs8S2          = io.sewIs8S2
  val sewIs16S2         = io.sewIs16S2
  val sewIs32S2         = io.sewIs32S2
  val sewIs64S2         = io.sewIs64S2
  
  // 9.get non fixed-point vd
  val vdNonFixP = Wire(UInt(64.W))

  val vdNonFixPGen = Module(new vdNonFixPGenerator())
  vdNonFixPGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  vdNonFixPGen.io.highHalf        := highHalfS2
  vdNonFixPGen.io.widen           := widenS2
  vdNonFixPGen.io.uopIdx          := uopIdxS2
  vdNonFixPGen.io.sewIs8          := sewIs8S2
  vdNonFixPGen.io.sewIs16         := sewIs16S2
  vdNonFixPGen.io.sewIs32         := sewIs32S2
  vdNonFixPGen.io.sewIs64         := sewIs64S2
  vdNonFixP := vdNonFixPGen.io.vdNonFixP
  
  // ----------- fixed-point instruction result handling  -----------
  // 10.generate vxsat bits and saturated vd
  val vdSat = Wire(UInt(64.W))
  val vxsat = Wire(UInt(8.W))

  val vxsatGen = Module(new vxsatGenerator())
  vxsatGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  vxsatGen.io.sewIs8          := sewIs8S2
  vxsatGen.io.sewIs16         := sewIs16S2
  vxsatGen.io.sewIs32         := sewIs32S2
  vxsatGen.io.sewIs64         := sewIs64S2
  vdSat := vxsatGen.io.vdSat
  vxsat := vxsatGen.io.vxsat

  // 11.generate rounding increment bits
  val rndIncVec = Wire(UInt(8.W))

  val rndIncVecGen = Module(new rndIncVecGenerator())
  rndIncVecGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  rndIncVecGen.io.vxrm            := vxrmS2
  rndIncVecGen.io.sewIs8          := sewIs8S2
  rndIncVecGen.io.sewIs16         := sewIs16S2
  rndIncVecGen.io.sewIs32         := sewIs32S2
  rndIncVecGen.io.sewIs64         := sewIs64S2
  rndIncVec := rndIncVecGen.io.rndIncVec

  // 12.generate rounding increment vd
  val vdRndInc    = Wire(UInt(64.W))

  val vdRndGen = Module(new vdRndGenerator())
  vdRndGen.io.sumFinalFixP    := sumFinalFixPS2
  vdRndGen.io.sewIs8   := sewIs8S2
  vdRndGen.io.sewIs16  := sewIs16S2
  vdRndGen.io.sewIs32  := sewIs32S2
  vdRndGen.io.sewIs64  := sewIs64S2
  vdRndInc    := vdRndGen.io.vdRndInc

  // 14.choose between rounding increment vd and saturated vd and generate final fixed-point vd
  val vdFixP = Wire(UInt(64.W))
  
  val vdFixPGen = Module(new vdFixPGenerator())
  vdFixPGen.io.vdRndInc    := vdRndInc
  vdFixPGen.io.vdSat       := vdSat
  vdFixPGen.io.rndIncVec   := rndIncVec
  vdFixPGen.io.sewIs8      := sewIs8S2
  vdFixPGen.io.sewIs16     := sewIs16S2
  vdFixPGen.io.sewIs32     := sewIs32S2
  vdFixPGen.io.sewIs64     := sewIs64S2
  vdFixP := vdFixPGen.io.vdFixP

  // 15.generate final output
  val outputMux = Module(new outputSelect())
  outputMux.io.vdNonFixP := vdNonFixP
  outputMux.io.vdFixP    := vdFixP
  outputMux.io.vxsat     := vxsat
  outputMux.io.isFixP    := isFixPS2
  
  // Connect Output
  io.vd    := outputMux.io.vdOut
  io.vxsat := outputMux.io.vxsatOut
}

class vdNonFixPGenerator extends Module {
  val io = IO(new Bundle {
    val sumFinalNonFixP   = Input(UInt(152.W))
    val highHalf          = Input(Bool())
    val widen             = Input(Bool())
    val uopIdx            = Input(UInt(6.W))
    val sewIs8            = Input(Bool())
    val sewIs16           = Input(Bool())
    val sewIs32           = Input(Bool())
    val sewIs64           = Input(Bool())

    val vdNonFixP  = Output(UInt(64.W))
  })

  val sumFinalNonFixP = io.sumFinalNonFixP
  val highHalf        = io.highHalf
  val widen           = io.widen
  val uopIdx          = io.uopIdx
  val sewIs8          = io.sewIs8
  val sewIs16         = io.sewIs16
  val sewIs32         = io.sewIs32
  val sewIs64         = io.sewIs64

  io.vdNonFixP := Mux1H(Seq(
    sewIs64 -> Mux(highHalf, sumFinalNonFixP(127,64), sumFinalNonFixP(63,0)),
    sewIs32 -> Mux(widen, Mux(uopIdx(0), sumFinalNonFixP(139, 76), sumFinalNonFixP(63,0)), Cat(UIntSplit(sumFinalNonFixP, 76).reverse.map(x => Mux(highHalf, x(63,32), x(31,0))))),
    sewIs16 -> Mux(widen, Cat(UIntSplit(Mux(uopIdx(0), sumFinalNonFixP(151, 76), sumFinalNonFixP(75, 0)), 38).reverse.map(x => x(31,0))), Cat(UIntSplit(sumFinalNonFixP, 38).reverse.map(x => Mux(highHalf, x(31,16), x(15,0))))),
    sewIs8  -> Mux(widen, Cat(UIntSplit(Mux(uopIdx(0), sumFinalNonFixP(151, 76), sumFinalNonFixP(75, 0)), 19).reverse.map(x => x(15,0))), Cat(UIntSplit(sumFinalNonFixP, 19).reverse.map(x => Mux(highHalf, x(15,8),  x(7, 0)))))
  ))
}

class vxsatGenerator extends Module {
  val io = IO(new Bundle {
    val sumFinalNonFixP = Input(UInt(152.W))
    val sewIs8          = Input(Bool())
    val sewIs16         = Input(Bool())
    val sewIs32         = Input(Bool())
    val sewIs64         = Input(Bool())

    val vdSat           = Output(UInt(64.W))
    val vxsat           = Output(UInt(8.W))
  })

  val sumFinalNonFixP = io.sumFinalNonFixP
  val sewIs64  = io.sewIs64
  val sewIs32  = io.sewIs32
  val sewIs16  = io.sewIs16
  val sewIs8   = io.sewIs8
  
  val vxsat    = Wire(UInt(8.W))
  val vdSat    = Wire(UInt(64.W))
  
  vxsat := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(sumFinalNonFixP, 19).reverse.map(x => x(15,14) === 1.U(2.W))),
    sewIs16 -> Cat(UIntSplit(sumFinalNonFixP, 38).reverse.map(x => Fill(2, x(31,30) === 1.U(2.W)))),
    sewIs32 -> Cat(UIntSplit(sumFinalNonFixP, 76).reverse.map(x => Fill(4, x(63,62) === 1.U(2.W)))),
    sewIs64 -> Fill(8, sumFinalNonFixP(127,126) === 1.U(2.W))
  ))

  vdSat := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(sumFinalNonFixP, 19).reverse.zip(UIntSplit(vxsat, 1).map(x => x(0)).reverse).map{ case(x, vxsat) => Mux(vxsat, "h7F".U(8.W),        x(14,7))}),
    sewIs16 -> Cat(UIntSplit(sumFinalNonFixP, 38).reverse.zip(UIntSplit(vxsat, 2).map(x => x(0)).reverse).map{ case(x, vxsat) => Mux(vxsat, "h7FFF".U(16.W),     x(30,15))}),
    sewIs32 -> Cat(UIntSplit(sumFinalNonFixP, 76).reverse.zip(UIntSplit(vxsat, 4).map(x => x(0)).reverse).map{ case(x, vxsat) => Mux(vxsat, "h7FFFFFFF".U(32.W), x(62,31))}),
    sewIs64 -> Mux(vxsat(0), "h7FFFFFFFFFFFFFFF".U(64.W), sumFinalNonFixP(126,63))
  ))

  io.vdSat    := vdSat
  io.vxsat    := vxsat
}

class rndIncVecGenerator extends Module {
  val io = IO(new Bundle {
    val sumFinalNonFixP = Input(UInt(152.W))
    val vxrm            = Input(UInt(2.W))
    val sewIs8          = Input(Bool())
    val sewIs16         = Input(Bool())
    val sewIs32         = Input(Bool())
    val sewIs64         = Input(Bool())

    val rndIncVec       = Output(UInt(8.W))
  })

  def rndIncGen(v_d: Bool, v_d_1: Bool, tail: UInt, vxrm: UInt): Bool = {
    Mux1H(Seq((vxrm === 0.U) -> v_d_1,
              (vxrm === 1.U) -> (v_d_1 && (tail =/= 0.U || v_d)),
              (vxrm === 2.U) -> false.B,
              (vxrm === 3.U) -> (!v_d && Cat(v_d_1, tail) =/= 0.U) ))
  }

  val sumFinalNonFixP  = io.sumFinalNonFixP
  val vxrm      = io.vxrm
  val sewIs64   = io.sewIs64
  val sewIs32   = io.sewIs32
  val sewIs16   = io.sewIs16
  val sewIs8    = io.sewIs8
  
  val rndIncVec = Wire(UInt(8.W))
  rndIncVec := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(sumFinalNonFixP, 19).reverse.map(x =>         rndIncGen(x(7),  x(6),  x(5, 0), vxrm))),
    sewIs16 -> Cat(UIntSplit(sumFinalNonFixP, 38).reverse.map(x => Fill(2, rndIncGen(x(15), x(14), x(13, 0), vxrm)))),
    sewIs32 -> Cat(UIntSplit(sumFinalNonFixP, 76).reverse.map(x => Fill(4, rndIncGen(x(31), x(30), x(29, 0), vxrm)))),
    sewIs64 -> Fill(8, rndIncGen(sumFinalNonFixP(63), sumFinalNonFixP(62), sumFinalNonFixP(61,0), vxrm))
  ))

  io.rndIncVec := rndIncVec
}

class vdRndGenerator extends Module {
  val io = IO(new Bundle {
    val sumFinalFixP    = Input(UInt(152.W))
    val sewIs8          = Input(Bool())
    val sewIs16         = Input(Bool())
    val sewIs32         = Input(Bool())
    val sewIs64         = Input(Bool())

    val vdRndInc         = Output(UInt(64.W))
  })
  val sumFinalFixP    = io.sumFinalFixP
  val sewIs64         = io.sewIs64
  val sewIs32         = io.sewIs32
  val sewIs16         = io.sewIs16
  val sewIs8          = io.sewIs8

  val vdRndInc    = Wire(UInt(64.W))

  vdRndInc := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(sumFinalFixP, 19).reverse.map(x => x(14, 7))),
    sewIs16 -> Cat(UIntSplit(sumFinalFixP, 38).reverse.map(x => x(30, 15))),
    sewIs32 -> Cat(UIntSplit(sumFinalFixP, 76).reverse.map(x => x(62, 31))),
    sewIs64 -> sumFinalFixP(126, 63)
  ))
  
  io.vdRndInc    := vdRndInc
}

class vdFixPGenerator extends Module {
  val io = IO(new Bundle {
    val vdRndInc    = Input(UInt(64.W))
    val vdSat       = Input(UInt(64.W))
    val rndIncVec   = Input(UInt(8.W))
    val sewIs8      = Input(Bool())
    val sewIs16     = Input(Bool())
    val sewIs32     = Input(Bool())
    val sewIs64     = Input(Bool())

    val vdFixP   = Output(UInt(64.W))
  })

  val vdRndInc    = io.vdRndInc
  val vdSat       = io.vdSat
  val rndIncVec   = io.rndIncVec
  val sewIs64     = io.sewIs64
  val sewIs32     = io.sewIs32
  val sewIs16     = io.sewIs16
  val sewIs8      = io.sewIs8

  val vdFixP = Wire(UInt(64.W))
  vdFixP := Cat(UIntSplit(vdRndInc, 8).reverse.lazyZip(UIntSplit(vdSat, 8).reverse).lazyZip(UIntSplit(rndIncVec, 1).map(x => x(0)).reverse).map{ case(rndIncData, satData, rndIncFlag) => 
                  Mux(rndIncFlag, rndIncData, satData)
            })

  io.vdFixP := vdFixP
}

class outputSelect extends Module {
  val io = IO(new Bundle {
    val vdNonFixP = Input(UInt(64.W))
    val vdFixP    = Input(UInt(64.W))
    val vxsat     = Input(UInt(8.W))
    val isFixP    = Input(Bool())

    val vdOut     = Output(UInt(64.W))
    val vxsatOut  = Output(UInt(8.W))
  })

  val vdNonFixP = io.vdNonFixP
  val vdFixP    = io.vdFixP
  val vxsat     = io.vxsat
  val isFixP    = io.isFixP

  io.vdOut    := Mux(io.isFixP, io.vdFixP, io.vdNonFixP)
  io.vxsatOut := Mux(io.isFixP, vxsat, 0.U(8.W))
}