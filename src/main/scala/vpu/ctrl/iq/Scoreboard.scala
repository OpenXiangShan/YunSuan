package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class VScoreboard extends Module {
  val io = IO(new Bundle {
    // From expander internal
    val readReq = Input(new VUop)
    // Stall signal, to expander internal
    val readResp = Output(Bool())
    // From expander output register, one cycle later than readReq
    val setReq = Input(ValidIO(new VUop))
    // RF writeback ports
    val wb = Input(Vec(nVRFWritePorts, ValidIO(new VUop)))
  })

  val table = RegInit(0.U(32.W)) // busy table
  val zero = false  // If the x0 is constant zero

  def reqVecToMask(enVec: Seq[Bool], addrVec: Seq[UInt]): UInt = {
    enVec zip addrVec map {case (en, addr) => Mux(en, UIntToOH(addr), 0.U)} reduce {_|_}
  }
  def set(enVec: Seq[Bool], addrVec: Seq[UInt]): Unit = {
    val updated = table | reqVecToMask(enVec, addrVec)
    table := { if (!zero) updated else { updated & "hFFFF_FFFE".U } }
  }
  def clear(enVec: Seq[Bool], addrVec: Seq[UInt]): Unit = {
    table := table & (~reqVecToMask(enVec, addrVec))
  }
  def read(addr: UInt): Bool = {
    val addrOH = Seq.tabulate(32)(addr === _.U)
    Mux1H(addrOH, table.asBools)
  }

  val setReqAddr = Wire(ValidIO(UInt(5.W)))
  setReqAddr.valid := io.setReq.valid && io.setReq.bits.ldestValUop
  setReqAddr.bits := io.setReq.bits.ldestUop

  // Set busy table
  set(Seq(setReqAddr.valid), Seq(setReqAddr.bits))
  
  val wbAddrs = Wire(Vec(nVRFWritePorts, ValidIO(UInt(5.W))))
  wbAddrs.zip(io.wb).foreach { case (a, b) =>
    a.valid := b.valid && b.bits.ldestValUop
    a.bits := b.bits.ldestUop
  }
  // RF writeback ports
  clear(wbAddrs.map(_.valid), wbAddrs.map(_.bits))
  
  val readReqAddrs = Wire(Vec(4, ValidIO(UInt(5.W))))
  readReqAddrs(0).valid := io.readReq.lsrcValUop(0)
  readReqAddrs(1).valid := io.readReq.lsrcValUop(1)
  readReqAddrs(2).valid := io.readReq.ldestValUop || io.readReq.lsrcValUop(2) // dest or 3rd operand
  readReqAddrs(3).valid := io.readReq.lmaskValUop // mask
  readReqAddrs(0).bits := io.readReq.lsrcUop(0)
  readReqAddrs(1).bits := io.readReq.lsrcUop(1)
  readReqAddrs(2).bits := io.readReq.ldestUop // dest or 3rd operand
  readReqAddrs(3).bits := 0.U // mask

  /**
    * RAW and WAW hazards (io.readReq are from expander internal)
    */
  val readReqBusy = readReqAddrs.map(x => read(x.bits) && x.valid)  // 0, 1, 2:dest 3:mask
  val raw_waw = readReqBusy.reduce(_ || _)

  /**
    * RAW and WAW hazards generated from (read, set) pair: (set -> read bypass)
    */
  //  1) Table read and set ports (read is one clock cycle younger than set)
  //  2) Two uops from different instructions
  val read_set_hazard = readReqAddrs.map(x => x.valid && x.bits === setReqAddr.bits).reduce(_||_) && setReqAddr.valid
  val different_instrns = io.readReq.robIdx =/= io.setReq.bits.robIdx
  val read_set_hazard_final = read_set_hazard && different_instrns
  
  /**
    * Structural Hazard
    *   (1) Write-back port Conflict
    *         Note: only consider EXU wb ports
    */
  // FU delay table for each RF write address
  val delay = Reg(Vec(32, UInt(bMaxFuDelay.W)))
  // TODO: resolve from uop. Note: don't forget - 1.U
  val setFuDelay = 2.U - 1.U
  // Set delay table
  val setReqAddrOH = UIntToOH(setReqAddr.bits)
  for (i <- 0 until 32) {
    when (setReqAddrOH(i) && setReqAddr.valid) {
      delay(i) := setFuDelay
    }.elsewhen (delay(i) =/= 0.U) {
      delay(i) := delay(i) - 1.U
    }
  }
  // Write port for each RF write address
  val wrPort = Reg(Vec(32, UInt(bNVRFWritePortsExu.W)))
  val setWrPort = Wire(UInt(bNVRFWritePortsExu.W))
  setWrPort := 0.U  // Temp. Only works for nVRFWritePortsExu == 1
  // Set wrPort table
  when (setReqAddr.valid) {
    wrPort(setReqAddr.bits) := setWrPort
  }
  
  // TODO: resolve from io.readReq uop. This is the delay from read req (inside expander)
  val readFuDelay = 2.U
  val strucHazd_wrPort_normal = delay(readReqAddrs(2).bits) === readFuDelay && readReqBusy(2)
  val strucHazd_wrPort_read_set = setFuDelay === readFuDelay && different_instrns &&
                                  readReqAddrs(2).valid && setReqAddr.valid
  val strucHazd_wrPort_final = strucHazd_wrPort_normal || strucHazd_wrPort_read_set                                  

  /**
    * Structural Hazard
    *   (2) Long Latency (Div)
    *         Note: only one long-latency uop is allowed being executed
    */
  // Long-latency uop record register
  val llReg = RegInit(false.B)
  // Set LL reg
  when (setReqAddr.valid) {
    llReg := io.setReq.bits.ctrl.div
  }.elsewhen (io.wb(nVRFWritePortsExu - 1).valid && io.wb(nVRFWritePortsExu - 1).bits.ctrl.div) { //TODO: check if the last port is div
    llReg := false.B
  }
  val strucHazd_ll_normal = llReg && io.readReq.ctrl.div
  val strucHazd_ll_read_set = io.setReq.valid && io.setReq.bits.ctrl.div && io.readReq.ctrl.div
  val strucHazd_ll_final = strucHazd_ll_normal || strucHazd_ll_read_set

  io.readResp := raw_waw || read_set_hazard_final || strucHazd_wrPort_final || strucHazd_ll_final
}
