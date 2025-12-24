package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._

class VIMac64b extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    // val opcode = Input(new VIMacOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val oldVd = Input(UInt(64.W)) 
    val highHalf = Input(Bool())
    val isMacc = Input(Bool()) // (w)macc(nmsac)/madd(nmsub)
    val isSub = Input(Bool())
    val widen = Input(Bool())
    val isFixP = Input(Bool())

    val vd = Output(UInt(64.W))
    val vxsat = Output(UInt(8.W))
  })

  val fire = io.fire
  val vs2 = io.vs2
  val vs1 = io.vs1
  val oldVd = io.oldVd
  val vs2_is_signed = io.srcType(0)(2) // vs2 & vd should be signed numbers for vmadd/vnmsub
  val vs1_is_signed = io.srcType(1)(2)
  val vd_is_signed  = io.vdType(2)
  val eewVs2 = SewOH(io.srcType(0)(1, 0))
  
  val sew = eewVs2
  val sewIs64 = sew.is64
  val sewIs32 = sew.is32
  val sewIs16 = sew.is16
  val sewIs8 = sew.is8

  val isSub    = io.isSub
  val highHalf = io.highHalf
  val widen    = io.widen
  val isFixP   = io.isFixP
  val uopIdx   = io.info.uopIdx
  val vxrm     = io.info.vxrm
  
  // Start of First Pipeline Stage
  //-----------------------------------------------------------------------------
  // 1.vs1 booth encoding
  val vs1BoothPos  = Wire(Vec(32, Bool()))
  val vs1BoothNeg  = Wire(Vec(32, Bool()))
  val vs1BoothDoZ  = Wire(Vec(32, Bool()))
  val vs1BoothNonZ = Wire(Vec(32, Bool()))

  val vs1BoothEncode = Module(new vs1Booth())
  vs1BoothEncode.io.vs1     := vs1
  vs1BoothEncode.io.sewIs8  := sewIs8
  vs1BoothEncode.io.sewIs16 := sewIs16
  vs1BoothEncode.io.sewIs32 := sewIs32
  vs1BoothEncode.io.sewIs64 := sewIs64
  vs1BoothPos  := vs1BoothEncode.io.positive
  vs1BoothNeg  := vs1BoothEncode.io.negative
  vs1BoothDoZ  := vs1BoothEncode.io.doubleOrZero
  vs1BoothNonZ := vs1BoothEncode.io.nonZero
  
  // 2.generate vs2 signed bits
  val sgnVs2 = Wire(Vec(8, UInt(1.W)))
  
  val vs2SignGen = Module(new vs2SgnGenerator())
  vs2SignGen.io.vs2            := vs2
  vs2SignGen.io.vs2_is_signed  := vs2_is_signed
  vs2SignGen.io.sewIs8         := sewIs8
  vs2SignGen.io.sewIs16        := sewIs16
  vs2SignGen.io.sewIs32        := sewIs32
  vs2SignGen.io.sewIs64        := sewIs64
  sgnVs2 := vs2SignGen.io.sgnVs2
 
  // 3.generate signed and carry bits for partial products
  val partProdCin = Wire(Vec(32, UInt(1.W)))
  val partProdSgn = Wire(Vec(32, UInt(1.W)))

  val ppSgnAndCBGen = Module(new partProdSgnAndCarryBitGen())
  ppSgnAndCBGen.io.isSub            := isSub
  ppSgnAndCBGen.io.vs2_is_signed    := vs2_is_signed
  ppSgnAndCBGen.io.sewIs8           := sewIs8
  ppSgnAndCBGen.io.sewIs16          := sewIs16
  ppSgnAndCBGen.io.sewIs32          := sewIs32
  ppSgnAndCBGen.io.sewIs64          := sewIs64
  ppSgnAndCBGen.io.sgnVs2           := sgnVs2
  ppSgnAndCBGen.io.vs1BoothPos      := vs1BoothPos
  ppSgnAndCBGen.io.vs1BoothNeg      := vs1BoothNeg
  ppSgnAndCBGen.io.vs1BoothNonZ     := vs1BoothNonZ
  partProdCin := ppSgnAndCBGen.io.partProdCin
  partProdSgn := ppSgnAndCBGen.io.partProdSgn

  // 4.generate partial product
  val partProd = Wire(Vec(32, UInt(68.W)))
  
  val partProdGen = Module(new partProdGenerator())
  partProdGen.io.partProdCin    := partProdCin
  partProdGen.io.partProdSgn    := partProdSgn
  partProdGen.io.vs1BoothDoZ    := vs1BoothDoZ
  partProdGen.io.vs1BoothNonZ   := vs1BoothNonZ
  partProdGen.io.sewIs8         := sewIs8
  partProdGen.io.sewIs16        := sewIs16
  partProdGen.io.sewIs32        := sewIs32
  partProdGen.io.sewIs64        := sewIs64
  partProdGen.io.vs2            := vs2
  partProd := partProdGen.io.partProd

  
  // 5.generate wallace tree
  val wallaceTree = Wire(Vec(33, UInt(152.W)))
  val wallaceLine34NonFixP = Wire(UInt(152.W))
  val wallaceLine34FixP    = Wire(UInt(152.W))

  val wallaceTreeGen = Module(new wallaceTreeGenerator())
  wallaceTreeGen.io.partProd      := partProd
  wallaceTreeGen.io.vs1           := vs1
  wallaceTreeGen.io.vs2           := vs2
  wallaceTreeGen.io.oldVd         := oldVd
  wallaceTreeGen.io.partProdCin   := partProdCin
  wallaceTreeGen.io.vs1_is_signed := vs1_is_signed
  wallaceTreeGen.io.vs2_is_signed := vs2_is_signed
  wallaceTreeGen.io.vd_is_signed  := vd_is_signed
  wallaceTreeGen.io.widen         := widen
  wallaceTreeGen.io.isMacc        := io.isMacc
  wallaceTreeGen.io.sewIs8        := sewIs8
  wallaceTreeGen.io.sewIs16       := sewIs16
  wallaceTreeGen.io.sewIs32       := sewIs32
  wallaceTreeGen.io.sewIs64       := sewIs64
  wallaceTree          := wallaceTreeGen.io.wallaceTree
  wallaceLine34NonFixP := wallaceTreeGen.io.wallaceLine34NonFixP
  wallaceLine34FixP    := wallaceTreeGen.io.wallaceLine34FixP

  // 6.wallace compress stage 1
  val compStage1Results = Wire(Vec(7, UInt(152.W)))
  
  val wallace3to2CompStage1 = Module(new wallace3to2CompressorStage1())
  wallace3to2CompStage1.io.wallaceTree := wallaceTree
  compStage1Results := wallace3to2CompStage1.io.compStage1Results

  // End of First Pipeline Stage
  //-----------------------------------------------------------------------------
  val fireS1                 = GatedValidRegNext(fire)
  val compStage1ResultsS1    = RegEnable(compStage1Results, fire)
  val wallaceLine34NonFixPS1 = RegEnable(wallaceLine34NonFixP, fire)
  val wallaceLine34FixPS1    = RegEnable(wallaceLine34FixP, fire)
  val highHalfS1             = RegEnable(highHalf, fire)
  val uopIdxS1               = RegEnable(uopIdx, fire)
  val widenS1                = RegEnable(widen, fire)
  val vxrmS1                 = RegEnable(vxrm, fire)
  val isFixPS1               = RegEnable(isFixP, fire)
  val sewIs8S1               = RegEnable(sewIs8, fire)
  val sewIs16S1              = RegEnable(sewIs16, fire)
  val sewIs32S1              = RegEnable(sewIs32, fire)
  val sewIs64S1              = RegEnable(sewIs64, fire)
  //-----------------------------------------------------------------------------
  // Start of Second Pipeline Stage
  
  // 2 sets of sums and couts are generated at this stage for non fixed-point and fixed-point increment calculation respectively
  // allowing fixed-point adder chain in Stage 3 to be removed and timing improved

  // 7.wallace compress stage 2
  val sum34to2NonFixP  = Wire(UInt(152.W))
  val cout34to2NonFixP = Wire(UInt(152.W))
  val sum34to2FixP     = Wire(UInt(152.W))
  val cout34to2FixP    = Wire(UInt(152.W))

  val wallace3to2CompStage2 = Module(new wallace3to2CompressorStage2())
  wallace3to2CompStage2.io.compStage1Results    := compStage1ResultsS1
  wallace3to2CompStage2.io.wallaceLine34NonFixP := wallaceLine34NonFixPS1
  wallace3to2CompStage2.io.wallaceLine34FixP    := wallaceLine34FixPS1
  sum34to2NonFixP  := wallace3to2CompStage2.io.sum34to2NonFixP
  cout34to2NonFixP := wallace3to2CompStage2.io.cout34to2NonFixP
  sum34to2FixP     := wallace3to2CompStage2.io.sum34to2FixP
  cout34to2FixP    := wallace3to2CompStage2.io.cout34to2FixP

  // 8.get final sum
  val sumFinalNonFixP  = Wire(UInt(152.W))
  val sumFinalFixP     = Wire(UInt(152.W))
  
  val finalSumAdderNonFixP = Module(new fullAdder152b())
  finalSumAdderNonFixP.io.sum34to2  := sum34to2NonFixP
  finalSumAdderNonFixP.io.cout34to2 := cout34to2NonFixP
  sumFinalNonFixP := finalSumAdderNonFixP.io.sumFinal

  val finalSumAdderFixP = Module(new fullAdder152b())
  finalSumAdderFixP.io.sum34to2  := sum34to2FixP
  finalSumAdderFixP.io.cout34to2 := cout34to2FixP
  sumFinalFixP := finalSumAdderFixP.io.sumFinal

  // End of Second Pipeline Stage
  //-----------------------------------------------------------------------------
  val sumFinalNonFixPS2 = RegEnable(sumFinalNonFixP, fireS1)
  val sumFinalFixPS2    = RegEnable(sumFinalFixP, fireS1)
  val highHalfS2        = RegEnable(highHalfS1, fireS1)
  val uopIdxS2          = RegEnable(uopIdxS1, fireS1)
  val widenS2           = RegEnable(widenS1, fireS1)
  val vxrmS2            = RegEnable(vxrmS1, fireS1)
  val isFixPS2          = RegEnable(isFixPS1, fireS1)
  val sewIs8S2          = RegEnable(sewIs8S1, fireS1)
  val sewIs16S2         = RegEnable(sewIs16S1, fireS1)
  val sewIs32S2         = RegEnable(sewIs32S1, fireS1)
  val sewIs64S2         = RegEnable(sewIs64S1, fireS1)
  //-----------------------------------------------------------------------------
  // Start of Third Pipeline Stage

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
  
  // ----- fixed-point instruction result handling -----
  // 10.get vxsat bits and decide whether fixed-point vd should be saturated
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

  // 11.get rounding increment flag bits which are used as MUX control signals in vdFixPGenerator later
  val rndIncVec = Wire(UInt(8.W))

  val rndIncVecGen = Module(new rndIncVecGenerator())
  rndIncVecGen.io.sumFinalNonFixP := sumFinalNonFixPS2
  rndIncVecGen.io.vxrm            := vxrmS2
  rndIncVecGen.io.sewIs8          := sewIs8S2
  rndIncVecGen.io.sewIs16         := sewIs16S2
  rndIncVecGen.io.sewIs32         := sewIs32S2
  rndIncVecGen.io.sewIs64         := sewIs64S2
  rndIncVec := rndIncVecGen.io.rndIncVec

  // 12.get rounding vd
  val vdRndInc    = Wire(UInt(64.W))

  val vdRndGen = Module(new vdRndGenerator())
  vdRndGen.io.sumFinalFixP := sumFinalFixPS2
  vdRndGen.io.sewIs8   := sewIs8S2
  vdRndGen.io.sewIs16  := sewIs16S2
  vdRndGen.io.sewIs32  := sewIs32S2
  vdRndGen.io.sewIs64  := sewIs64S2
  vdRndInc := vdRndGen.io.vdRndInc

  // 14.decide whether fixed-point vd should be rounded and generate fixed-point vd
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

  // 15.choose between fixed-point vd and non fixed-point vd and generate final output
  val outputMux = Module(new outputSelect())
  outputMux.io.vdNonFixP := vdNonFixP
  outputMux.io.vdFixP    := vdFixP
  outputMux.io.vxsat     := vxsat
  outputMux.io.isFixP    := isFixPS2
  
  // Connect Output
  io.vd    := outputMux.io.vdOut
  io.vxsat := outputMux.io.vxsatOut
}

class vs1Booth extends Module {
  val io = IO(new Bundle {
    val sewIs8  = Input(Bool())
    val sewIs16 = Input(Bool())
    val sewIs32 = Input(Bool())
    val sewIs64 = Input(Bool())
    val vs1     = Input(UInt(64.W))

    val positive     = Output(Vec(32, Bool()))
    val negative     = Output(Vec(32, Bool()))
    val doubleOrZero = Output(Vec(32, Bool()))
    val nonZero      = Output(Vec(32, Bool()))
  })
  
  val vs1 = io.vs1
  val sewIs64 = io.sewIs64
  val sewIs32 = io.sewIs32
  val sewIs16 = io.sewIs16
  val sewIs8 =  io.sewIs8
  
  val vs1Booth3b = Wire(Vec(32,UInt(3.W)))
  vs1Booth3b(0) := Cat(vs1(1,0), 0.U(1.W))
  for (i <- 1 until 32) {
    if (i % 4 != 0) {
      vs1Booth3b(i) := vs1(2*i+1, 2*i-1)
    } else if (i == 16) {
      vs1Booth3b(i) := Cat(vs1(2*i+1, 2*i), sewIs64 & vs1(2*i-1))
    } else if (i % 8 == 0) {
      vs1Booth3b(i) := Cat(vs1(2*i+1, 2*i), (sewIs32 | sewIs64) & vs1(2*i-1))
    } else {
      vs1Booth3b(i) := Cat(vs1(2*i+1, 2*i), ~sewIs8 & vs1(2*i-1))
    }
  }

  val vs1Booth = Seq.tabulate(32)(i => new boothEncode(vs1Booth3b(i)))

  io.positive     := vs1Booth.map(_.positive)
  io.negative     := vs1Booth.map(_.negative)
  io.doubleOrZero := vs1Booth.map(_.doubleOrZero)
  io.nonZero      := vs1Booth.map(_.nonZero)
}

class boothEncode(d: UInt) {
    val positive = (d(1) | d(0)) & ~d(2)
    val negative = (~d(1) | ~d(0)) & d(2)
    val doubleOrZero = ~(d(1) ^ d(0))
    val nonZero = positive | negative
}

class vs2SgnGenerator extends Module {
  val io = IO(new Bundle {
    val vs2_is_signed = Input(Bool())
    val sewIs8        = Input(Bool())
    val sewIs16       = Input(Bool())
    val sewIs32       = Input(Bool())
    val sewIs64       = Input(Bool())
    val vs2           = Input(UInt(64.W))

    val sgnVs2  = Output(Vec(8, UInt(1.W)))
  })
  
  val vs2_is_signed = io.vs2_is_signed
  val vs2     = io.vs2
  val sewIs64 = io.sewIs64
  val sewIs32 = io.sewIs32
  val sewIs16 = io.sewIs16
  val sewIs8  = io.sewIs8

  for(i <- 0 until 8) {
    val sgnSew64 = 63
    val sgnSew32 = 32*(i/4)+31
    val sgnSew16 = 16*(i/2)+15
    val sgnSew8  = 8*i+7
    io.sgnVs2(i) := (vs2_is_signed) & (sewIs64 & vs2(sgnSew64) | sewIs32 & vs2(sgnSew32) | sewIs16 & vs2(sgnSew16) | sewIs8 & vs2(sgnSew8))
  }
}

class partProdSgnAndCarryBitGen extends Module {
  val io = IO(new Bundle {
    val isSub         = Input(Bool())
    val vs2_is_signed = Input(Bool())
    val sewIs8        = Input(Bool())
    val sewIs16       = Input(Bool())
    val sewIs32       = Input(Bool())
    val sewIs64       = Input(Bool())
    val sgnVs2        = Input(Vec(8,  UInt(1.W)))
    val vs1BoothPos   = Input(Vec(32, Bool()))
    val vs1BoothNeg   = Input(Vec(32, Bool()))
    val vs1BoothNonZ  = Input(Vec(32, Bool()))

    val partProdSgn  = Output(Vec(32, UInt(1.W)))
    val partProdCin  = Output(Vec(32, UInt(1.W)))
  })

  val isSub         = io.isSub
  val vs2_is_signed = io.vs2_is_signed
  val sewIs64       = io.sewIs64
  val sewIs32       = io.sewIs32
  val sewIs16       = io.sewIs16
  val sewIs8        = io.sewIs8
  val sgnVs2        = io.sgnVs2
  val vs1BoothPos   = io.vs1BoothPos
  val vs1BoothNeg   = io.vs1BoothNeg
  val vs1BoothNonZ  = io.vs1BoothNonZ
  
  val partProdCin   = Wire(Vec(32, UInt(1.W)))
  for (i <- 0 until 32) {
    partProdCin(i) := vs1BoothNeg(i) & ~isSub | vs1BoothPos(i) & isSub
  }
  io.partProdCin := partProdCin

  for (i <- 0 until 32) {
    val sgnVs2Index = i/4
    io.partProdSgn(i) := (partProdCin(i) ^ sgnVs2(sgnVs2Index)) & vs1BoothNonZ(i)
  }
}

class partProdGenerator extends Module {
   val io = IO(new Bundle {
    val partProdCin   = Input(Vec(32, UInt(1.W)))
    val partProdSgn   = Input(Vec(32, UInt(1.W)))
    val vs1BoothDoZ   = Input(Vec(32, Bool()))
    val vs1BoothNonZ  = Input(Vec(32, Bool()))
    val sewIs8        = Input(Bool())
    val sewIs16       = Input(Bool())
    val sewIs32       = Input(Bool())
    val sewIs64       = Input(Bool())
    val vs2           = Input(UInt(64.W))

    val partProd = Output(Vec(32, UInt(68.W)))
  })

  val partProdCin   = io.partProdCin
  val partProdSgn   = io.partProdSgn
  val vs1BoothDoZ   = io.vs1BoothDoZ
  val vs1BoothNonZ  = io.vs1BoothNonZ
  val sewIs64       = io.sewIs64
  val sewIs32       = io.sewIs32
  val sewIs16       = io.sewIs16
  val sewIs8        = io.sewIs8
  val vs2           = io.vs2

  for(i <- 0 until 32) {
    val sew8IndexL  = (i/4)*8
    val sew8IndexH  = (i/4)*8   + 7
    val sew16IndexL = (i/8)*16
    val sew16IndexH = (i/8)*16  + 15
    val sew32IndexL = (i/16)*32
    val sew32IndexH = (i/16)*32 + 31
    val sew64IndexL = 0
    val sew64IndexH = 63
    if (i == 0) { 
      io.partProd(i) := Mux1H(Seq(
        sewIs8  -> Cat(0.U(56.W), ~partProdSgn(i), partProdSgn(i), partProdSgn(i), Fill(9,  vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL),  partProdCin(i)), Cat(partProdSgn(i), Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL )))),
        sewIs16 -> Cat(0.U(48.W), ~partProdSgn(i), partProdSgn(i), partProdSgn(i), Fill(17, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL)))),
        sewIs32 -> Cat(0.U(32.W), ~partProdSgn(i), partProdSgn(i), partProdSgn(i), Fill(33, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL)))),
        sewIs64 -> Cat(           ~partProdSgn(i), partProdSgn(i), partProdSgn(i), Fill(65, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL))))                    
      ))
    } else if (i == 16) {
      io.partProd(i) := Mux1H(Seq(
        sewIs8  -> Cat(0.U(56.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(9,  vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL),  partProdCin(i)), Cat(partProdSgn(i), Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL )))),
        sewIs16 -> Cat(0.U(48.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(17, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL)))),
        sewIs32 -> Cat(0.U(32.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(33, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL)))),
        sewIs64 -> Cat(           0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(65, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL))))                    
      ))
    } else if (i % 8 == 0) {
      io.partProd(i) := Mux1H(Seq(
        sewIs8  -> Cat(0.U(56.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(9,  vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL),  partProdCin(i)), Cat(partProdSgn(i), Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL )))),
        sewIs16 -> Cat(0.U(48.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(17, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL)))),
        sewIs32 -> Cat(0.U(32.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(33, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL)))),
        sewIs64 -> Cat(           0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(65, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL))))                    
      ))
    } else if (i % 4 == 0) {
       io.partProd(i) := Mux1H(Seq(
        sewIs8  -> Cat(0.U(56.W), ~partProdSgn(i), partProdSgn(i),  partProdSgn(i), Fill(9,  vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL),  partProdCin(i)), Cat(partProdSgn(i), Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL )))),
        sewIs16 -> Cat(0.U(48.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(17, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL)))),
        sewIs32 -> Cat(0.U(32.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(33, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL)))),
        sewIs64 -> Cat(           0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(65, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL))))                    
      ))
    } else {
      io.partProd(i) := Mux1H(Seq(
        sewIs8  -> Cat(0.U(56.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(9,  vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL),  partProdCin(i)), Cat(partProdSgn(i), Fill(8,  partProdCin(i)) ^ vs2(sew8IndexH,  sew8IndexL )))),
        sewIs16 -> Cat(0.U(48.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(17, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(16, partProdCin(i)) ^ vs2(sew16IndexH, sew16IndexL)))),
        sewIs32 -> Cat(0.U(32.W), 0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(33, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(32, partProdCin(i)) ^ vs2(sew32IndexH, sew32IndexL)))),
        sewIs64 -> Cat(           0.U(1.W),        1.U(1.W),       ~partProdSgn(i), Fill(65, vs1BoothNonZ(i)) & Mux(vs1BoothDoZ(i), Cat(Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL), partProdCin(i)), Cat(partProdSgn(i), Fill(64, partProdCin(i)) ^ vs2(sew64IndexH, sew64IndexL))))                    
      ))
    }
  }
}

class wallaceTreeGenerator extends Module {
  val io = IO(new Bundle {
    val partProd      = Input(Vec(32, UInt(68.W)))
    val vs1           = Input(UInt(64.W))
    val vs2           = Input(UInt(64.W))
    val oldVd         = Input(UInt(64.W))
    val partProdCin   = Input(Vec(32, UInt(1.W)))
    val vs1_is_signed = Input(Bool())
    val vs2_is_signed = Input(Bool())
    val vd_is_signed  = Input(Bool())
    val widen         = Input(Bool())
    val isMacc        = Input(Bool())
    val sewIs8        = Input(Bool())
    val sewIs16       = Input(Bool())
    val sewIs32       = Input(Bool())
    val sewIs64       = Input(Bool())

    val wallaceTree          = Output(Vec(33, UInt(152.W)))
    val wallaceLine34NonFixP = Output(UInt(152.W))
    val wallaceLine34FixP    = Output(UInt(152.W))
  })

  def wallaceTreeGen(i: Int, wallaceLine: UInt, partProd: UInt, partProdCin: UInt, sewIs8: Bool, sewIs16: Bool, sewIs32: Bool, sewIs64: Bool): Unit = {
    val elementIndex8b  = i/4
    val elementIndex16b = i/8
    val elementIndex32b = i/16
    val shift8b  = i % 4
    val shift16b = i % 8
    val shift32b = i % 16
    val shift64b = i
    if (i == 0) {
        wallaceTree(i) := Cat(0.U(84.W), partProd)
    } else if (i == 16) {
        wallaceLine := Mux1H(Seq(
          sewIs8  -> Cat(0.U((152 - elementIndex8b*19  - shift8b*2  - 12).W), partProd(11, 0),                        0.U((elementIndex8b*19  + shift8b*2).W)),
          sewIs16 -> Cat(0.U((152 - elementIndex16b*38 - shift16b*2 - 20).W), partProd(19, 0),                        0.U((elementIndex16b*38 + shift16b*2).W)),
          sewIs32 -> Cat(0.U((152 - elementIndex32b*76 - shift32b*2 - 36).W), partProd(35, 0),                        0.U((elementIndex32b*76 + shift32b*2).W)),
          sewIs64 -> Cat(0.U((152                      - shift64b*2 - 68).W), partProd,        0.U(1.W), partProdCin, 0.U((shift64b*2 - 2).W))
      ))
    } else if (i % 8 == 0) {
      wallaceLine := Mux1H(Seq(
          sewIs8  -> Cat(0.U((152 - elementIndex8b*19  - shift8b*2  - 12).W), partProd(11, 0),                        0.U((elementIndex8b*19  + shift8b*2).W)),
          sewIs16 -> Cat(0.U((152 - elementIndex16b*38 - shift16b*2 - 20).W), partProd(19, 0),                        0.U((elementIndex16b*38 + shift16b*2).W)),
          sewIs32 -> Cat(0.U((152 - elementIndex32b*76 - shift32b*2 - 36).W), partProd(35, 0), 0.U(1.W), partProdCin, 0.U((elementIndex32b*76 + shift32b*2 - 2).W)),
          sewIs64 -> Cat(0.U((152                      - shift64b*2 - 68).W), partProd,        0.U(1.W), partProdCin, 0.U((shift64b*2 - 2).W))
      ))
    } else if (i % 4 == 0) {
      wallaceLine := Mux1H(Seq(
          sewIs8  -> Cat(0.U((152 - elementIndex8b*19  - shift8b*2  - 12).W), partProd(11, 0),                        0.U((elementIndex8b*19  + shift8b*2).W)),
          sewIs16 -> Cat(0.U((152 - elementIndex16b*38 - shift16b*2 - 20).W), partProd(19, 0), 0.U(1.W), partProdCin, 0.U((elementIndex16b*38 + shift16b*2 - 2).W)),
          sewIs32 -> Cat(0.U((152 - elementIndex32b*76 - shift32b*2 - 36).W), partProd(35, 0), 0.U(1.W), partProdCin, 0.U((elementIndex32b*76 + shift32b*2 - 2).W)),
          sewIs64 -> Cat(0.U((152                      - shift64b*2 - 68).W), partProd,        0.U(1.W), partProdCin, 0.U((shift64b*2 - 2).W))
      ))
    } else {
      wallaceLine := Mux1H(Seq(
          sewIs8  -> Cat(0.U((152 - elementIndex8b*19  - shift8b*2  - 12).W), partProd(11, 0), 0.U(1.W), partProdCin, 0.U((elementIndex8b*19  + shift8b*2  - 2).W)),
          sewIs16 -> Cat(0.U((152 - elementIndex16b*38 - shift16b*2 - 20).W), partProd(19, 0), 0.U(1.W), partProdCin, 0.U((elementIndex16b*38 + shift16b*2 - 2).W)),
          sewIs32 -> Cat(0.U((152 - elementIndex32b*76 - shift32b*2 - 36).W), partProd(35, 0), 0.U(1.W), partProdCin, 0.U((elementIndex32b*76 + shift32b*2 - 2).W)),
          sewIs64 -> Cat(0.U((152                      - shift64b*2 - 68).W), partProd,        0.U(1.W), partProdCin, 0.U((shift64b*2 - 2).W))
      ))
    }
  }

  val partProd      = io.partProd
  val vs1           = io.vs1
  val vs2           = io.vs2
  val oldVd         = io.oldVd
  val partProdCin   = io.partProdCin
  val vs1_is_signed = io.vs1_is_signed
  val vs2_is_signed = io.vs2_is_signed
  val vd_is_signed  = io.vd_is_signed
  val widen         = io.widen
  val isMacc        = io.isMacc
  val sewIs8        = io.sewIs8
  val sewIs16       = io.sewIs16
  val sewIs32       = io.sewIs32
  val sewIs64       = io.sewIs64

  val wallaceTree = Wire(Vec(33, UInt(152.W)))
  val wallaceLine34NonFixP = Wire(UInt(152.W))
  val wallaceLine34FixP    = Wire(UInt(152.W))
  wallaceTreeGen(0, wallaceTree(0), partProd(0), 0.U(1.W), sewIs8, sewIs16, sewIs32, sewIs64)
  for (i <- 1 until 32) {
    wallaceTreeGen(i, wallaceTree(i), partProd(i), partProdCin(i-1), sewIs8, sewIs16, sewIs32, sewIs64)
  }

  wallaceTree(32) := Mux(io.isMacc, Mux1H(Seq(
    sewIs8  -> Mux(io.widen, Cat(UIntSplit(Cat(oldVd, oldVd), 16).reverse.map(x => Cat(0.U(2.W),  BitsExtend(x, 17, vd_is_signed)))), Cat(UIntSplit(oldVd, 8 ).map(x => Cat(0.U(2.W),  BitsExtend(x, 17, vd_is_signed))).reverse)),
    sewIs16 -> Mux(io.widen, Cat(UIntSplit(Cat(oldVd, oldVd), 32).reverse.map(x => Cat(0.U(5.W),  BitsExtend(x, 33, vd_is_signed)))), Cat(UIntSplit(oldVd, 16).map(x => Cat(0.U(5.W),  BitsExtend(x, 33, vd_is_signed))).reverse)),
    sewIs32 -> Mux(io.widen, Cat(UIntSplit(Cat(oldVd, oldVd), 64).reverse.map(x => Cat(0.U(11.W), BitsExtend(x, 65, vd_is_signed)))), Cat(UIntSplit(oldVd, 32).map(x => Cat(0.U(11.W), BitsExtend(x, 65, vd_is_signed))).reverse)),
    sewIs64 -> Cat(0.U(23.W), BitsExtend(oldVd, 129, vd_is_signed))
  )), 0.U)

  wallaceLine34NonFixP := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(vs2, 8 ).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(2.W),  Mux(~vs1_is_signed & vs1(63 - 8*index),  BitsExtend(x,   9 , vs2_is_signed), 0.U(9.W)) , 0.U(1.W), partProdCin(31 - 4*index),  0.U(6.W))}),
    sewIs16 -> Cat(UIntSplit(vs2, 16).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(5.W),  Mux(~vs1_is_signed & vs1(63 - 16*index), BitsExtend(x,   17, vs2_is_signed), 0.U(17.W)), 0.U(1.W), partProdCin(31 - 8*index),  0.U(14.W))}),
    sewIs32 -> Cat(UIntSplit(vs2, 32).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(11.W), Mux(~vs1_is_signed & vs1(63 - 32*index), BitsExtend(x,   33, vs2_is_signed), 0.U(33.W)), 0.U(1.W), partProdCin(31 - 16*index), 0.U(30.W))}),
    sewIs64 ->                                                                    Cat(0.U(23.W), Mux(~vs1_is_signed & vs1(63),            BitsExtend(vs2, 65, vs2_is_signed), 0.U(35.W)), 0.U(1.W), partProdCin(31)           , 0.U(62.W))
  ))

  wallaceLine34FixP := Mux1H(Seq(
    sewIs8  -> Cat(UIntSplit(vs2, 8 ).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(2.W),  Mux(~vs1_is_signed & vs1(63 - 8*index),  BitsExtend(x,   9 , vs2_is_signed), 0.U(9.W)) , 1.U(1.W), partProdCin(31 - 4*index),  0.U(6.W))}),
    sewIs16 -> Cat(UIntSplit(vs2, 16).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(5.W),  Mux(~vs1_is_signed & vs1(63 - 16*index), BitsExtend(x,   17, vs2_is_signed), 0.U(17.W)), 1.U(1.W), partProdCin(31 - 8*index),  0.U(14.W))}),
    sewIs32 -> Cat(UIntSplit(vs2, 32).reverse.zipWithIndex.map{ case(x, index) => Cat(0.U(11.W), Mux(~vs1_is_signed & vs1(63 - 32*index), BitsExtend(x,   33, vs2_is_signed), 0.U(33.W)), 1.U(1.W), partProdCin(31 - 16*index), 0.U(30.W))}),
    sewIs64 ->                                                                    Cat(0.U(23.W), Mux(~vs1_is_signed & vs1(63),            BitsExtend(vs2, 65, vs2_is_signed), 0.U(35.W)), 1.U(1.W), partProdCin(31)           , 0.U(62.W))
  ))

  io.wallaceTree := wallaceTree
  io.wallaceLine34NonFixP := wallaceLine34NonFixP
  io.wallaceLine34FixP    := wallaceLine34FixP
}

class wallace3to2CompressorStage1 extends Module with wallace3to2Compressor {
  val io = IO(new Bundle {
    val wallaceTree = Input(Vec(33, UInt(152.W)))

    val compStage1Results = Output(Vec(7, UInt(152.W)))
  })

  val wallaceTree = io.wallaceTree
  val result = wallaceCompress(7, wallaceTree)
  io.compStage1Results := result
}

class wallace3to2CompressorStage2 extends Module with wallace3to2Compressor {
  val io = IO(new Bundle {
    val compStage1Results    = Input(Vec(7, UInt(152.W)))
    val wallaceLine34NonFixP = Input(UInt(152.W))
    val wallaceLine34FixP    = Input(UInt(152.W))

    val sum34to2NonFixP  = Output(UInt(152.W))
    val cout34to2NonFixP = Output(UInt(152.W))
    val sum34to2FixP     = Output(UInt(152.W))
    val cout34to2FixP    = Output(UInt(152.W))
  })

  val compStage1Results    = io.compStage1Results
  val wallaceLine34NonFixP = io.wallaceLine34NonFixP
  val wallaceLine34FixP    = io.wallaceLine34FixP
  
  val interCompressResult = wallaceCompress(5, compStage1Results)

  val nonFixPCompressIn = interCompressResult :+ wallaceLine34NonFixP
  val fixPCompressIn    = interCompressResult :+ wallaceLine34FixP
  val nonFixPResult = wallaceCompress(2, nonFixPCompressIn)
  val fixPResult    = wallaceCompress(2, fixPCompressIn)
  
  io.sum34to2NonFixP  := nonFixPResult(0)
  io.cout34to2NonFixP := nonFixPResult(1)
  io.sum34to2FixP     := fixPResult(0)
  io.cout34to2FixP    := fixPResult(1)
}

trait wallace3to2Compressor {
  def compressor3to2(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val sum  = Wire(UInt(152.W))
    val cout = Wire(UInt(152.W))
    sum  := a ^ b ^ c
    cout := (a & b) | (b & c) | (c & a)
    (sum, cout)
  }

  def wallaceCompress(outNum: Int, compressIn: Seq[UInt]): Seq[UInt] = {
    if (compressIn.size == outNum) {
      compressIn
    }
    else {
      val compressGroupNum = compressIn.size / 3
      val sum =  Wire(Vec(compressGroupNum, UInt(152.W)))
      val cout = Wire(Vec(compressGroupNum, UInt(152.W)))
      for(i <- 0 until compressGroupNum) {
        sum(i)  := compressor3to2(compressIn(3*i), compressIn(3*i+1), compressIn(3*i+2))._1
        cout(i) := Cat(compressor3to2(compressIn(3*i), compressIn(3*i+1), compressIn(3*i+2))._2(150,0), 0.U(1.W))
      }
      wallaceCompress(outNum, sum ++ cout ++ compressIn.drop(3*compressGroupNum))
    }
  }
}

class fullAdder152b extends Module {
  val io = IO(new Bundle {
    val sum34to2  = Input(UInt(152.W))
    val cout34to2 = Input(UInt(152.W))

    val sumFinal  = Output(UInt(152.W))
  })

  io.sumFinal := io.sum34to2 + io.cout34to2
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
    sewIs32 -> Mux(widen, Mux(uopIdx(0), sumFinalNonFixP(140, 77), sumFinalNonFixP(63,0)), Cat(UIntSplit(sumFinalNonFixP, 76).reverse.map(x => Mux(highHalf, x(63,32), x(31,0))))),
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
