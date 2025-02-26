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
    val fromExu = Input(Vec(nVRFWritePortsExu, ValidIO(new VExuOutput)))
    val fromLSU = Input(ValidIO(new Bundle { //TODO: correct the io.fromLSU
      val uop = new VUop
      val vd = UInt(VLEN.W)
    }))
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

  val scoreboard = Module(new VScoreboard)
  scoreboard.io.readReq := expander.io.readScoreboard.req
  expander.io.readScoreboard.resp := scoreboard.io.readResp
  scoreboard.io.setReq.valid := expander.io.out.fire
  scoreboard.io.setReq.bits := expander.io.out.bits.uop
  scoreboard.io.wb zip io.fromExu foreach { case (a, b) =>
    a.valid := b.valid
    a.bits := b.bits.uop
  }

  val vrf = Module(new VRF(4, nVRFWritePorts))
  vrf.io.raddr(0) := expander.io.out.bits.uop.lsrcUop(0)
  vrf.io.raddr(1) := expander.io.out.bits.uop.lsrcUop(1)
  vrf.io.raddr(2) := expander.io.out.bits.uop.ldestUop
  vrf.io.raddr(3) := 0.U //mask
  for (i <- 0 until nVRFWritePortsExu) {
    vrf.io.wen(i) := io.fromExu(i).valid && io.fromExu(i).bits.uop.ldestValUop
    vrf.io.waddr(i) := io.fromExu(i).bits.uop.ldestUop
    vrf.io.wdata(i) := io.fromExu(i).bits.vd
  }
  vrf.io.wen(nVRFWritePorts - 1) := io.fromLSU.valid && io.fromLSU.bits.uop.ldestValUop
  vrf.io.waddr(nVRFWritePorts - 1) := io.fromLSU.bits.uop.ldestUop
  vrf.io.wdata(nVRFWritePorts - 1) := io.fromLSU.bits.vd
  
  expander.io.out.ready := true.B
  val exuInputReg = Reg(new VExuInput)
  when (expander.io.out.fire) {
    exuInputReg.uop := expander.io.out.bits.uop
    exuInputReg.rs1 := expander.io.out.bits.rs1
    exuInputReg.vSrc := vrf.io.rdata
  }
  io.toExu.valid := expander.io.out.fire
}


