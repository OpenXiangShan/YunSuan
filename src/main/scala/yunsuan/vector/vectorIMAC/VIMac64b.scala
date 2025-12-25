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
  val isMacc   = io.isMacc
  val widen    = io.widen
  val isFixP   = io.isFixP
  val info     = io.info

  val stage1 = Module(new VIMac64bStage1())
  val stage2 = Module(new VIMac64bStage2())
  val stage3 = Module(new VIMac64bStage3())

  stage1.io.info      := info
  stage1.io.srcType   := io.srcType
  stage1.io.vdType    := io.vdType
  stage1.io.vs1       := vs1
  stage1.io.vs2       := vs2
  stage1.io.oldVd     := oldVd
  stage1.io.highHalf  := highHalf
  stage1.io.isMacc    := isMacc
  stage1.io.isSub     := isSub
  stage1.io.widen     := widen
  stage1.io.isFixP    := isFixP

  val fireS1                 = GatedValidRegNext(fire)
  val compStage1ResultsS1    = RegEnable(stage1.io.compStage1ResultsS1, fire)
  val wallaceLine34NonFixPS1 = RegEnable(stage1.io.wallaceLine34NonFixPS1, fire)
  val wallaceLine34FixPS1    = RegEnable(stage1.io.wallaceLine34FixPS1, fire)
  val highHalfS1             = RegEnable(stage1.io.highHalfS1, fire)
  val uopIdxS1               = RegEnable(stage1.io.uopIdxS1, fire)
  val widenS1                = RegEnable(stage1.io.widenS1, fire)
  val vxrmS1                 = RegEnable(stage1.io.vxrmS1, fire)
  val isFixPS1               = RegEnable(stage1.io.isFixPS1, fire)
  val sewIs8S1               = RegEnable(stage1.io.sewIs8S1, fire)
  val sewIs16S1              = RegEnable(stage1.io.sewIs16S1, fire)
  val sewIs32S1              = RegEnable(stage1.io.sewIs32S1, fire)
  val sewIs64S1              = RegEnable(stage1.io.sewIs64S1, fire)

  stage2.io.compStage1ResultsS1    := compStage1ResultsS1
  stage2.io.wallaceLine34NonFixPS1 := wallaceLine34NonFixPS1
  stage2.io.wallaceLine34FixPS1    := wallaceLine34FixPS1
  stage2.io.highHalfS1             := highHalfS1
  stage2.io.uopIdxS1               := uopIdxS1
  stage2.io.widenS1                := widenS1
  stage2.io.vxrmS1                 := vxrmS1
  stage2.io.isFixPS1               := isFixPS1
  stage2.io.sewIs8S1               := sewIs8S1
  stage2.io.sewIs16S1              := sewIs16S1
  stage2.io.sewIs32S1              := sewIs32S1
  stage2.io.sewIs64S1              := sewIs64S1

  val sumFinalNonFixPS2 = RegEnable(stage2.io.sumFinalNonFixPS2, fireS1)
  val sumFinalFixPS2    = RegEnable(stage2.io.sumFinalFixPS2, fireS1)
  val highHalfS2        = RegEnable(stage2.io.highHalfS2, fireS1)
  val uopIdxS2          = RegEnable(stage2.io.uopIdxS2, fireS1)
  val widenS2           = RegEnable(stage2.io.widenS2, fireS1)
  val vxrmS2            = RegEnable(stage2.io.vxrmS2, fireS1)
  val isFixPS2          = RegEnable(stage2.io.isFixPS2, fireS1)
  val sewIs8S2          = RegEnable(stage2.io.sewIs8S2, fireS1)
  val sewIs16S2         = RegEnable(stage2.io.sewIs16S2, fireS1)
  val sewIs32S2         = RegEnable(stage2.io.sewIs32S2, fireS1)
  val sewIs64S2         = RegEnable(stage2.io.sewIs64S2, fireS1)

  stage3.io.sumFinalNonFixPS2 := sumFinalNonFixPS2
  stage3.io.sumFinalFixPS2    := sumFinalFixPS2
  stage3.io.highHalfS2        := highHalfS2
  stage3.io.uopIdxS2          := uopIdxS2
  stage3.io.widenS2           := widenS2
  stage3.io.vxrmS2            := vxrmS2
  stage3.io.isFixPS2          := isFixPS2
  stage3.io.sewIs8S2          := sewIs8S2
  stage3.io.sewIs16S2         := sewIs16S2
  stage3.io.sewIs32S2         := sewIs32S2
  stage3.io.sewIs64S2         := sewIs64S2

  io.vd    := stage3.io.vd
  io.vxsat := stage3.io.vxsat
}