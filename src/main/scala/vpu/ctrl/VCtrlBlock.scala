package race.vpu

import chisel3._
import chisel3.util._
import VParams._
import race.vpu.ctrl._
import _root_.vpu.utils.PipeConnect

class VCtrlBlock extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Flipped(DecoupledIO(new Dispatch_S2V))
    val toExu = ValidIO(new VExuInput)
    // val fromExu = 
    val out = Decoupled(new ExpdOutput)
  })

  val decoder = Module(new VDecode)
  decoder.io.in := io.dispatch_s2v.bits.inst

  val infoClac = Module(new VInfoCalc)
  infoClac.io.ctrl := decoder.io.out
  infoClac.io.csr := io.dispatch_s2v.bits.vcsr
  
  val viqInput = DecoupledIO(new VIQInput)
  io.dispatch_s2v.ready := viqInput.ready
  viqInput.valid := io.dispatch_s2v.valid
  viqInput.bits.mop.ctrl := decoder.io.out
  viqInput.bits.mop.csr := io.dispatch_s2v.bits.vcsr
  viqInput.bits.mop.robIdx := io.dispatch_s2v.bits.robIdx
  viqInput.bits.mop.veewVd := infoClac.io.infoAll.veewVd
  viqInput.bits.mop.emulVd := infoClac.io.infoAll.emulVd
  viqInput.bits.mop.emulVs2 := infoClac.io.infoAll.emulVs2
  viqInput.bits.rs.rs1 := io.dispatch_s2v.bits.rs1
  viqInput.bits.rs.rs2 := io.dispatch_s2v.bits.rs2

  val viq = Module(new VIQ)
  PipeConnect(viqInput, viq.io.in, false.B, moduleName = Some("decodePipeIqModule"))
  
  val expdInfo = Module(new ExpdLen)
  expdInfo.io.in := viq.io.in.bits.mop
  viq.io.in_expdInfo := expdInfo.io.out

  val expander = Module(new Expander)
  expander.io.in <> viq.io.out

  val busyTable = Module(new VBusyTable)
  busyTable.io.readReq := expander.io.readBusyTable.req
  expander.io.readBusyTable.resp := busyTable.io.readResp
  busyTable.io.setReq.valid := expander.io.out.fire && expander.io.out.bits.uop.ldestValUop
  busyTable.io.setReq.bits.addr := expander.io.out.bits.uop.ldestUop
  busyTable.io.setReq.bits.robIdx := expander.io.out.bits.uop.robIdx

  val vrf = Module(new VRF(4, nVRFWritePorts))
  vrf.io.raddr(0) := expander.io.out.bits.uop.lsrcUop(0)
  vrf.io.raddr(1) := expander.io.out.bits.uop.lsrcUop(1)
  vrf.io.raddr(2) := expander.io.out.bits.uop.ldestUop
  vrf.io.raddr(3) := 0.U //mask
  
  expander.io.out.ready := true.B
  val exuInputReg = Reg(new VExuInput)
  when (expander.io.out.fire) {
    exuInputReg.vuop := expander.io.out.bits.uop
    exuInputReg.rs1 := expander.io.out.bits.rs1
    exuInputReg.vSrc := vrf.io.rdata
  }
  io.toExu.valid := expander.io.out.fire

  io.out <> expander.io.out
}


