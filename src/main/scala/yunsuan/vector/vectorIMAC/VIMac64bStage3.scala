package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._
import yunsuan.vector.Common._

class VIMac64bStage3 extends VIMac64b.Module {
  val in  = IO(Input(new VIMac64bStage3.In))
  val out = IO(Output(new VIMac64bStage3.Out))

  private val sumFinalNonFixPS2 = in.data.sumFinalNonFixPS2
  private val sumFinalFixPS2    = in.data.sumFinalFixPS2
  private val highHalfS2        = in.ctrl.highHalfS2
  private val uopIdxS2          = in.info.uopIdx
  private val widenS2           = in.ctrl.widenS2
  private val vxrmS2            = in.info.vxrm
  private val isFixPS2          = in.ctrl.isFixPS2
  private val sewIs8S2          = in.ctrl.sewIs8S2
  private val sewIs16S2         = in.ctrl.sewIs16S2
  private val sewIs32S2         = in.ctrl.sewIs32S2
  private val sewIs64S2         = in.ctrl.sewIs64S2


  // 9.get non fixed-point vd
  val vdNonFixP = Wire(UInt(64.W))

  val vdNonFixPGen = Module(new VIMac64bStage3.vdNonFixPGenerator())
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

  val vxsatGen = Module(new VIMac64bStage3.vxsatGenerator())
  vxsatGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  vxsatGen.io.sewIs8          := sewIs8S2
  vxsatGen.io.sewIs16         := sewIs16S2
  vxsatGen.io.sewIs32         := sewIs32S2
  vxsatGen.io.sewIs64         := sewIs64S2
  vdSat := vxsatGen.io.vdSat
  vxsat := vxsatGen.io.vxsat

  // 11.generate rounding increment bits
  val rndIncVec = Wire(UInt(8.W))

  val rndIncVecGen = Module(new VIMac64bStage3.rndIncVecGenerator())
  rndIncVecGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  rndIncVecGen.io.vxrm            := vxrmS2
  rndIncVecGen.io.sewIs8          := sewIs8S2
  rndIncVecGen.io.sewIs16         := sewIs16S2
  rndIncVecGen.io.sewIs32         := sewIs32S2
  rndIncVecGen.io.sewIs64         := sewIs64S2
  rndIncVec := rndIncVecGen.io.rndIncVec

  // 12.generate rounding increment vd
  val vdRndInc    = Wire(UInt(64.W))

  val vdRndGen = Module(new VIMac64bStage3.vdRndGenerator())
  vdRndGen.io.sumFinalFixP    := sumFinalFixPS2
  vdRndGen.io.sewIs8   := sewIs8S2
  vdRndGen.io.sewIs16  := sewIs16S2
  vdRndGen.io.sewIs32  := sewIs32S2
  vdRndGen.io.sewIs64  := sewIs64S2
  vdRndInc    := vdRndGen.io.vdRndInc

  // 14.choose between rounding increment vd and saturated vd and generate final fixed-point vd
  val vdFixP = Wire(UInt(64.W))

  val vdFixPGen = Module(new VIMac64bStage3.vdFixPGenerator())
  vdFixPGen.io.vdRndInc    := vdRndInc
  vdFixPGen.io.vdSat       := vdSat
  vdFixPGen.io.rndIncVec   := rndIncVec
  vdFixPGen.io.sewIs8      := sewIs8S2
  vdFixPGen.io.sewIs16     := sewIs16S2
  vdFixPGen.io.sewIs32     := sewIs32S2
  vdFixPGen.io.sewIs64     := sewIs64S2
  vdFixP := vdFixPGen.io.vdFixP

  // 15.generate final output
  val outputMux = Module(new VIMac64bStage3.outputSelect())
  outputMux.io.vdNonFixP := vdNonFixP
  outputMux.io.vdFixP    := vdFixP
  outputMux.io.vxsat     := vxsat
  outputMux.io.isFixP    := isFixPS2

  // Connect Output
  out.vd    := outputMux.io.vdOut
  out.vxsat := outputMux.io.vxsatOut
}


object VIMac64bStage3 {
  class In extends Bundle {
    val info = new Info
    val ctrl = new InCtrl
    val data = new InData
  }

  class InCtrl extends Bundle {
    val highHalfS2 = Bool()
    val widenS2    = Bool()
    val isFixPS2   = Bool()
    val sewIs8S2   = Bool()
    val sewIs16S2  = Bool()
    val sewIs32S2  = Bool()
    val sewIs64S2  = Bool()
  }

  class Info extends Bundle {
    val uopIdx = UInt(6.W)
    val vxrm = Vxrm()
  }

  class InData extends Bundle {
    val sumFinalNonFixPS2 = UInt(152.W)
    val sumFinalFixPS2    = UInt(152.W)
  }

  class Out extends VIMac64b.Bundle {
    val vd    = UInt(dWidth.W)
    val vxsat = MaskData()
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
      val vxrm            = Input(Vxrm())
      val sewIs8          = Input(Bool())
      val sewIs16         = Input(Bool())
      val sewIs32         = Input(Bool())
      val sewIs64         = Input(Bool())

      val rndIncVec       = Output(UInt(8.W))
    })

    def rndIncGen(v_d: Bool, v_d_1: Bool, tail: UInt, vxrm: Vxrm): Bool = {
      Mux1H(Seq(
        vxrm.isRnu -> v_d_1,
        vxrm.isRne -> (v_d_1 && (tail =/= 0.U || v_d)),
        vxrm.isRdn -> false.B,
        vxrm.isRod -> (!v_d && Cat(v_d_1, tail) =/= 0.U),
      ))
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
}
