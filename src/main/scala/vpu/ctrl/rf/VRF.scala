package race.vpu.ctrl

import chisel3._
import chisel3.util._
import race.vpu.VParams._
import race.vpu._
import utility.{ParallelMux}

class subVRFReadPort(regLen: Int) extends Bundle {
  val addrOH = Input(Vec(32, Bool()))
  val data = Output(UInt(regLen.W))
}
class subVRFWritePort(regLen: Int) extends Bundle {
  val wen = Input(Bool())
  val addrOH = Input(Vec(32, Bool()))
  val data = Input(UInt(regLen.W))
  val wmask = Input(UInt(regLen.W))
}

// A portion of vector RF (only 64-bit)
class subVRegFile(numRead: Int, numWrite: Int, regLen: Int) extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, new subVRFReadPort(regLen))
    val write = Vec(numWrite, new subVRFWritePort(regLen))
    val bypassHits = Input(Vec(numRead, Vec(numWrite, Bool())))
  })

  val rf = Reg(Vec(32, UInt(regLen.W)))
  for (i <- 0 until numRead) {
    // rd.data := ParallelMux(rd.addrOH zip rf)
    val rd_rf_data = ParallelMux(io.read(i).addrOH zip rf)
    // Write-read bypass
    io.read(i).data := Mux(!io.bypassHits(i).reduce(_ || _), 
                   rd_rf_data,
                   Mux1H(io.bypassHits(i), io.write.map(x => (x.data & x.wmask) | (rd_rf_data & ~x.wmask)))
    )
  }
  for (i <- 0 until 32) {
    val wrHit = io.write.map(x => x.wen && x.addrOH(i))
    when (wrHit.reduce(_ || _)) {
      // rf(i) := Mux1H(wrHit, io.write.map(_.data))
      rf(i) := Mux1H(wrHit, io.write.map(x =>
                     (x.data & x.wmask) | (rf(i) & ~x.wmask)))
    }
  }
}

class VRFReadPort(regLen: Int) extends Bundle {
  val addr = Input(UInt(5.W))
  val data = Output(Vec(NLanes, UInt(regLen.W)))
}
class VRFWritePort(regLen: Int) extends Bundle {
  val wen = Input(Bool())
  val addr = Input(UInt(5.W))
  val data = Input(Vec(NLanes, UInt(regLen.W)))
  val wmask = Input(Vec(NLanes, UInt(regLen.W)))
}

class VRegFile(numRead: Int, numWrite: Int) extends Module {
  val io = IO(new Bundle {
    val read = Vec(numRead, new VRFReadPort(LaneWidth))
    val write = Vec(numWrite, new VRFWritePort(LaneWidth))
  })
  
  val subRFs = Seq.fill(NLanes)(Module(new subVRegFile(numRead, numWrite, LaneWidth)))
  val allAddrs = VecInit.tabulate(32)(_.U(5.W))
  val rdOneHot = Seq.tabulate(numRead)(i => allAddrs.map(_ === io.read(i).addr))
  val wrOneHot = Seq.tabulate(numWrite)(i => allAddrs.map(_ === io.write(i).addr))
  for (laneIdx <- 0 until NLanes) {
    for (i <- 0 until numRead) {
      subRFs(laneIdx).io.read(i).addrOH := rdOneHot(i)
      io.read(i).data(laneIdx) := subRFs(laneIdx).io.read(i).data
    }
    for (i <- 0 until numWrite) {
      subRFs(laneIdx).io.write(i).wen := io.write(i).wen
      subRFs(laneIdx).io.write(i).addrOH := wrOneHot(i)
      subRFs(laneIdx).io.write(i).data := io.write(i).data(laneIdx)
      subRFs(laneIdx).io.write(i).wmask := io.write(i).wmask(laneIdx)
    }
  }
  val bypassHits = Wire(Vec(numRead, Vec(numWrite, Bool())))
  for (i <- 0 until numRead) {
    for (j <- 0 until numWrite) {
      bypassHits(i)(j) := io.read(i).addr === io.write(j).addr && io.write(j).wen
    }
  }
  subRFs.foreach(_.io.bypassHits := bypassHits) 
}

class VRF(numRead: Int, numWrite: Int) extends Module {
  val io = IO(new Bundle {
    val raddr = Input(Vec(numRead, UInt(5.W)))
    val rdata = Output(Vec(numRead, UInt(VLEN.W)))
    val wen = Input(Vec(numWrite, Bool()))
    val waddr = Input(Vec(numWrite, UInt(5.W)))
    val wdata = Input(Vec(numWrite, UInt(VLEN.W)))
    // Masks of VRF write.  bit 1: write RF, bit 0: undisturbed
    val wmask = Input(Vec(numWrite, UInt(VLEN.W)))
  })

  val rf = Module(new VRegFile(numRead, numWrite))
  rf.io.read zip io.raddr foreach { case (x, y) => x.addr := y }
  io.rdata zip rf.io.read foreach { case (x, y) => x := y.data.asUInt }
  rf.io.write zip io.wen zip io.waddr zip io.wdata zip io.wmask foreach { case ((((x, y), z), d), m) =>
    x.wen := y
    x.addr := z
    x.data := VecInit(UIntSplit.vlen_splitTo_lanes(d))
    x.wmask := VecInit(UIntSplit.vlen_splitTo_lanes(m))
  }
}