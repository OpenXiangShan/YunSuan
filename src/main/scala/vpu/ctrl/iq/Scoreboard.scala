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
  
  // RF writeback ports
  clear(io.wb.map(x => x.valid && x.bits.ldestValUop), io.wb.map(_.bits.ldestUop))
  
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
  val raw_waw = readReqAddrs.map(x => read(x.bits) && x.valid).reduce(_ || _)

  // RAW and WAW hazards generated from bypass network:
  //  1) Table read and set ports (bypass)
  //  2) Two uops from different instructions
  val read_set_hazard = readReqAddrs.map(x => x.valid && x.bits === setReqAddr.bits).reduce(_||_) && setReqAddr.valid
  val different_instrns = io.readReq.robIdx =/= io.setReq.bits.robIdx
  val read_set_hazard_final = read_set_hazard && different_instrns
  io.readResp := raw_waw || read_set_hazard_final
}
