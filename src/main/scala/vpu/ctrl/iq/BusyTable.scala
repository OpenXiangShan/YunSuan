package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._

class ReadReqBusyTable extends Bundle {
  val addr = Vec(4, ValidIO(UInt(5.W)))
  val robIdx = new RobPtr
}

class SetReqBusyTable extends Bundle {
  val addr = UInt(5.W)
  val robIdx = new RobPtr
}

class VBusyTable extends Module {
  val io = IO(new Bundle {
    // From expander internal
    val readReq = Input(new ReadReqBusyTable)
    // Stall signal, to expander internal
    val readResp = Output(Bool())
    // From expander output register, one cycle later than readReq
    val setReq = Input(ValidIO(new SetReqBusyTable))
    // RF writeback ports
    val wb = Input(Vec(nVRFWritePorts, ValidIO(UInt(5.W))))
  })

  val table = RegInit(0.U(32.W))
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

  set(Seq(io.setReq.valid), Seq(io.setReq.bits.addr))
  // -- Rewrite set for raw_waw_bypass resue
//   val setMask = reqVecToMask(Seq(io.setReq.valid), Seq(io.setReq.bits.addr))
//   val updated = table | setMask
//   table := { if (!zero) updated else { updated & "hFFFF_FFFE".U } }

  // RF writeback ports
  clear(io.wb.map(_.valid), io.wb.map(_.bits))

  /**
    * RAW and WAW hazards (io.readReq are from expander internal)
    */
  val raw_waw = io.readReq.addr.map(x => read(x.bits) && x.valid).reduce(_ || _)
  // -- Rewrite raw_waw for raw_waw_bypass resue
//   val readMasks = Wire(Vec(4, UInt(32.W)))
//   readMasks := io.readReq.addr.map(x => UIntToOH(x.bits))
//   val readValids = io.readReq.addr.map(_.valid)
//   val raw_waw = (readMasks zip readValids).map(
//       { case (mask, v) => Mux1H(mask, table.asBools) && v }
//     ).reduce(_ || _)

  // RAW and WAW hazards generated from bypass network:
  //  1) Table read and set ports (bypass)
  //  2) Two uops from different instructions
  val read_set_hazard = io.readReq.addr.map(x => x.valid && x.bits === io.setReq.bits.addr).reduce(_||_) && io.setReq.valid
  val different_instrns = io.readReq.robIdx =/= io.setReq.bits.robIdx
  val read_set_hazard_final = read_set_hazard && different_instrns
  io.readResp := raw_waw || read_set_hazard_final
}