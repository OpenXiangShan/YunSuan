package race.vpu

import chisel3._
import chisel3.util._
import VParams._
import race.vpu.ctrl._

class FromCtrlToDebugRob extends Bundle {
  val issuedUopRobIdx = ValidIO(new RobPtr)
  val wbExu = ValidIO(new Bundle {
    val uop = new VUop
    val vd = UInt(VLEN.W)
  })
  val wbLoad = ValidIO(new Bundle {
    val uop = new VUop
    val vd = UInt(VLEN.W)
  })
  val wbStore = ValidIO(new Bundle {
    val uop = new VUop
  })
}


class VCtrlBlock extends Module {
  val io = IO(new Bundle {
    val dispatch_s2v = Flipped(DecoupledIO(new Dispatch_S2V))
    val toExu = ValidIO(new VExuInput)
    val fromExu = Input(Vec(nVRFWritePortsExu, ValidIO(new VExuOutput)))
    val lsu = new Bundle {
      val loadReq = DecoupledIO(new VLsuLoadReq)
      val loadWb = Input(ValidIO(new VLsuLoadWb))
      val storeReq = DecoupledIO(new VLsuStoreReq)
      val storeAck = Input(ValidIO(new VLsuStoreAck))
    }
    val debugRob = Option.when(debugMode)(Output(new FromCtrlToDebugRob))
  })

  val decoder = Module(new VDecode)
  decoder.io.in := io.dispatch_s2v.bits.inst

  val infoCalc = Module(new VInfoCalc)
  infoCalc.io.ctrl := decoder.io.out
  infoCalc.io.csr := io.dispatch_s2v.bits.vcsr
  
  val viqInput = Wire(DecoupledIO(new VIQInput))
  io.dispatch_s2v.ready := viqInput.ready
  viqInput.valid := io.dispatch_s2v.valid
  viqInput.bits.mop.ctrl := decoder.io.out
  viqInput.bits.mop.csr := io.dispatch_s2v.bits.vcsr
  viqInput.bits.mop.robIdx := io.dispatch_s2v.bits.robIdx
  viqInput.bits.mop.veewVd := infoCalc.io.infoAll.veewVd
  viqInput.bits.mop.emulVd := infoCalc.io.infoAll.emulVd
  viqInput.bits.mop.emulVs2 := infoCalc.io.infoAll.emulVs2
  viqInput.bits.rs.rs1 := io.dispatch_s2v.bits.rs1
  viqInput.bits.rs.rs2 := io.dispatch_s2v.bits.rs2

  val viq = Module(new VIQ)
  viq.io.flush := false.B
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
  scoreboard.io.wb.dropRight(1) zip io.fromExu foreach { case (a, b) =>
    a.valid := b.valid
    a.bits := b.bits.uop
  }
  scoreboard.io.wb.last.valid := io.lsu.loadWb.valid
  scoreboard.io.wb.last.bits := io.lsu.loadWb.bits.uop

  val writebackExu = Module(new WritebackExu)
  writebackExu.io.in.uop := io.fromExu(0).bits.uop

  val vrf = if (!debugMode) Module(new VRF(4, nVRFWritePorts)) else
                            Module(new VRF(4 + 2, nVRFWritePorts))
  vrf.io.raddr(0) := expander.io.out.bits.uop.lsrcUop(0)
  vrf.io.raddr(1) := expander.io.out.bits.uop.lsrcUop(1)
  vrf.io.raddr(2) := expander.io.out.bits.uop.ldestUop
  vrf.io.raddr(3) := 0.U //mask
  for (i <- 0 until nVRFWritePortsExu) {
    vrf.io.wen(i) := io.fromExu(i).valid && io.fromExu(i).bits.uop.ldestValUop
    vrf.io.waddr(i) := io.fromExu(i).bits.uop.ldestUop
    vrf.io.wdata(i) := io.fromExu(i).bits.vd
    vrf.io.wmask(i) := writebackExu.io.maskWb
  }
  vrf.io.wen(nVRFWritePorts - 1) := io.lsu.loadWb.valid && io.lsu.loadWb.bits.uop.ldestValUop
  vrf.io.waddr(nVRFWritePorts - 1) := io.lsu.loadWb.bits.uop.ldestUop
  vrf.io.wdata(nVRFWritePorts - 1) := io.lsu.loadWb.bits.vd
  vrf.io.wmask(nVRFWritePorts - 1) := ~0.U(VLEN.W) // Temp

  /**
    * Expander output goes to EXU, LSU_load, or LSU_store
    *   Note: the EXU and LSU_store input are registered, but the LSU_load input is not.
    *         The reason is that EXU and STORE input need RF read data.
    *                              +-------------+
    *                              |  Expander   |
    *                              +-------------+
    *                                    |
    *                       +-----------++-----------+
    *                       |                       |
    *                     RF read                   |
    *                       |                       |
    *                       v                       v
    *                +------------+         +-----------+
    *                | exuInputReg |        |   Load    |
    *                +------------+         +-----------+
    *                       |              
    *                +------+------+        
    *                |             |        
    *                v             v        
    *         +-----------+  +-----------+  
    *         |    EXU    |  |   Store   |  
    *         +-----------+  +-----------+  
    * 
    *  Valid/ready should be carefully handled here.
    *    (1) We first generate the ready signal "readyExuStore" that is from EXU/STORE to "exuInputReg"
    *        and "exuInputRegOutValid" which is the out-valid of exuInputReg
    *    (2) Then we generate the ready signal "expander.io.out.ready" that is from exuInputReg/Load to expander
    * */

  /**
    *  (1) We first generate the ready signal "readyExuStore" that is from EXU/STORE to "exuInputReg"
    *      and "exuInputRegOutValid" which is the out-valid of exuInputReg
    */
  val expdrOutValid = expander.io.out.valid
  val expdrOutIsExu = expander.io.out.bits.uop.ctrl.arith
  val expdrOutIsStore = expander.io.out.bits.uop.ctrl.store
  val expdrOutIsLoad = expander.io.out.bits.uop.ctrl.load
  // Register the input for the EXU. 
  // STORE also uses vs3 and uop
  val exuInputReg = Reg(new VExuInput)
  val paddrBaseStoreReg = Reg(UInt(XLEN.W))
  val ldstCtrlStoreReg = Reg(new LdstCtrl)
  val exuInputRegOutValid = RegInit(false.B)
  val readyExuStore = Wire(Bool())
  val fire_exuInputReg = expdrOutValid && (expdrOutIsExu || expdrOutIsStore) && readyExuStore
  when (fire_exuInputReg) {
    exuInputReg.uop := expander.io.out.bits.uop
    exuInputReg.vSrc(2) := vrf.io.rdata(2)  // vs3 for store
  }
  when (fire_exuInputReg && expdrOutIsExu) {
    exuInputReg.rs1 := expander.io.out.bits.rs1
    exuInputReg.vSrc(0) := vrf.io.rdata(0)
    exuInputReg.vSrc(1) := vrf.io.rdata(1)
    exuInputReg.vSrc(3) := vrf.io.rdata(3)
  }
  when (fire_exuInputReg && expdrOutIsStore) {
    paddrBaseStoreReg := expander.io.out.bits.rs1 // FIXME: use the real paddr_base from expander
    ldstCtrlStoreReg := 0.U.asTypeOf(new LdstCtrl) // FIXME: use the real ldstCtrl from expander
  }
  // Combine the ready from EXU and LSU_store
  val readyExu = true.B // Exu has no ready signal for expander
  readyExuStore := !exuInputRegOutValid || (exuInputReg.uop.ctrl.arith || io.lsu.storeReq.fire)
  // exuInputRegOutValid
  when (fire_exuInputReg) {
    exuInputRegOutValid := true.B
  }.elsewhen (readyExuStore) {
    exuInputRegOutValid := false.B
  }
  // Exu and LSU_store input valid
  io.toExu.valid := exuInputRegOutValid && exuInputReg.uop.ctrl.arith
  io.lsu.storeReq.valid := exuInputRegOutValid && exuInputReg.uop.ctrl.store
  // Exu and LSU_store input data
  io.toExu.bits.uop := exuInputReg.uop
  io.toExu.bits.vSrc := exuInputReg.vSrc
  io.toExu.bits.rs1 := exuInputReg.rs1
  io.lsu.storeReq.bits.uop := exuInputReg.uop
  io.lsu.storeReq.bits.vs3 := exuInputReg.vSrc(3)
  io.lsu.storeReq.bits.paddr := paddrBaseStoreReg
  io.lsu.storeReq.bits.ldstCtrl := ldstCtrlStoreReg

  /**
    *  (2) Then we generate the ready signal "expander.io.out.ready" that is from exuInputReg/Load to expander
    */
  expander.io.out.ready := io.lsu.loadReq.ready && expdrOutIsLoad || readyExuStore && (expdrOutIsExu || expdrOutIsStore)
  io.lsu.loadReq.valid := expdrOutValid && expdrOutIsLoad
  io.lsu.loadReq.bits.uop := expander.io.out.bits.uop
  io.lsu.loadReq.bits.ldstCtrl := 0.U.asTypeOf(new LdstCtrl) // FIXME: use the real ldstCtrl from expander
  io.lsu.loadReq.bits.paddr := expander.io.out.bits.rs1 // FIXME: use the real paddr_base from expander

  /**
    * Debug
    */
  if (debugMode) {
    io.debugRob.get.issuedUopRobIdx.valid := expander.io.out.fire && expander.io.out.bits.uop.uopEnd
    io.debugRob.get.issuedUopRobIdx.bits := expander.io.out.bits.uop.robIdx
    io.debugRob.get.wbLoad.valid := RegNext(io.lsu.loadWb.valid)
    io.debugRob.get.wbLoad.bits.uop := RegNext(io.lsu.loadWb.bits.uop)
    vrf.io.raddr(5) := RegNext(io.lsu.loadWb.bits.uop.ldestUop)
    io.debugRob.get.wbLoad.bits.vd := vrf.io.rdata(5)
    io.debugRob.get.wbExu.valid := RegNext(io.fromExu(0).valid)
    io.debugRob.get.wbExu.bits.uop := RegNext(io.fromExu(0).bits.uop)
    vrf.io.raddr(4) := RegNext(io.fromExu(0).bits.uop.ldestUop)
    io.debugRob.get.wbExu.bits.vd := vrf.io.rdata(4)
    io.debugRob.get.wbStore.valid := RegNext(io.lsu.storeReq.fire)
    io.debugRob.get.wbStore.bits.uop := RegNext(io.lsu.storeReq.bits.uop)
  }
}

object VerilogVCtrl extends App {
  println("Generating the Vector Ctrl hardware")
  emitVerilog(new VCtrlBlock(), Array("--target-dir", "build/verilog_vctrl"))
}