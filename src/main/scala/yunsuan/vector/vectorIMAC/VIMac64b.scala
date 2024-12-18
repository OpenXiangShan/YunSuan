package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._
import yunsuan.vector.Common._

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

  val fireS1    = GatedValidRegNext(fire)

  val stage1 = Module(new VIMac64bStage1())
  stage1.io.in.info      := info
  stage1.io.in.srcType   := io.srcType
  stage1.io.in.vdType    := io.vdType
  stage1.io.in.vs1       := vs1
  stage1.io.in.vs2       := vs2
  stage1.io.in.oldVd     := oldVd
  stage1.io.in.highHalf  := highHalf
  stage1.io.in.isMacc    := isMacc
  stage1.io.in.isSub     := isSub
  stage1.io.in.widen     := widen
  stage1.io.in.isFixP    := isFixP
  val stage1Out = RegEnable(stage1.io.out, fire)

  val stage2 = Module(new VIMac64bStage2())
  stage2.io.in := stage1Out
  val stage2Out = RegEnable(stage2.io.out, fireS1)

  val stage3 = Module(new VIMac64bStage3())
  stage3.io.in := stage2Out

  io.vd    := stage3.io.out.vd
  io.vxsat := stage3.io.out.vxsat
}