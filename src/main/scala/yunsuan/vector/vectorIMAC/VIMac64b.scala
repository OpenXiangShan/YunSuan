package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._
import yunsuan.vector.Common._

class VIMac64b extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val info = Input(new VIFuInfo)
    //In VImMacType instructions, vs1 and vs2 share the same SEW
    val sew = Input(UInt(2.W))
    val vs1Sign = Input(Bool())
    val vs2Sign = Input(Bool())
    val vdSign = Input(Bool())
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
  val fireS1    = GatedValidRegNext(fire)

  val stage1 = Module(new VIMac64bStage1())
  stage1.io.in.info      := io.info
  stage1.io.in.vs1Sign   := io.vs1Sign
  stage1.io.in.vs2Sign   := io.vs2Sign
  stage1.io.in.vdSign    := io.vdSign
  stage1.io.in.sew       := io.sew
  stage1.io.in.vs1       := io.vs1
  stage1.io.in.vs2       := io.vs2
  stage1.io.in.oldVd     := io.oldVd
  stage1.io.in.highHalf  := io.highHalf
  stage1.io.in.isMacc    := io.isMacc
  stage1.io.in.isSub     := io.isSub
  stage1.io.in.widen     := io.widen
  stage1.io.in.isFixP    := io.isFixP
  val stage1Out = RegEnable(stage1.io.out, fire)

  val stage2 = Module(new VIMac64bStage2())
  stage2.io.in := stage1Out
  val stage2Out = RegEnable(stage2.io.out, fireS1)

  val stage3 = Module(new VIMac64bStage3())
  stage3.io.in := stage2Out

  io.vd    := stage3.io.out.vd
  io.vxsat := stage3.io.out.vxsat
}