package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._

class VIMac extends Module {
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VIFuInput))
    val out = ValidIO(new VIFuOutput)
  })

  // Latency of MAC is 2 cycles plus
  io.out.valid := RegNext(RegNext(io.in.valid))

  val opcode = io.in.bits.opcode
  val srcTypeVs1 = io.in.bits.srcType(1)
  val srcTypeVs2 = io.in.bits.srcType(0)
  val vdType = io.in.bits.vdType
  val eewVd = SewOH(vdType(1, 0))
  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val oldVd = io.in.bits.old_vd
  val mask = io.in.bits.mask
  val uopIdx = io.in.bits.info.uopIdx
  val vstart_gte_vl = io.in.bits.info.vstart >= io.in.bits.info.vl
  val widen = srcTypeVs1(1, 0) =/= vdType(1, 0)

  val vIMac64bs = Seq.fill(2)(Module(new VIMac64b))
  for (i <- 0 until 2) {
    vIMac64bs(i).io.fire := io.in.valid
    vIMac64bs(i).io.info := io.in.bits.info
    vIMac64bs(i).io.srcType := io.in.bits.srcType
    vIMac64bs(i).io.vdType := io.in.bits.vdType
    vIMac64bs(i).io.highHalf := opcode.highHalf
    vIMac64bs(i).io.isMacc := opcode.isMacc
    vIMac64bs(i).io.isSub := opcode.isSub
    vIMac64bs(i).io.widen := widen
    vIMac64bs(i).io.isFixP := opcode.isFixP
    vIMac64bs(i).io.vs1 := Mux(widen, Cat(UIntSplit(vs1, 32)(i+2), UIntSplit(vs1, 32)(i)),
                               UIntSplit(vs1, 64)(i))
    vIMac64bs(i).io.vs2 := Mux(opcode.overWriteMultiplicand, UIntSplit(oldVd, 64)(i),
                           Mux(widen, Cat(UIntSplit(vs2, 32)(i+2), UIntSplit(vs2, 32)(i)),
                               UIntSplit(vs2, 64)(i)))
    vIMac64bs(i).io.oldVd := Mux(opcode.overWriteMultiplicand, UIntSplit(vs2, 64)(i),
                                 UIntSplit(oldVd, 64)(i))
  }

  /**
   * Output stage
   */
  val eewVdS2 = RegNext(RegNext(eewVd))
  val oldVdS2 = Wire(UInt(128.W))
  oldVdS2 := RegNext(RegNext(oldVd))
  val taS2 = RegNext(RegNext(io.in.bits.info.ta))
  val maS2 = RegNext(RegNext(io.in.bits.info.ma))
  val vmS2 = RegNext(RegNext(io.in.bits.info.vm))
  // Output tail/prestart/mask handling
  //---- Tail gen ----
  val tail = TailGen(io.in.bits.info.vl, uopIdx, eewVd)
  val tailS2 = RegNext(RegNext(tail))
  //---- Prestart gen ----
  val prestart = PrestartGen(io.in.bits.info.vstart, uopIdx, eewVd)
  val prestartS2 = RegNext(RegNext(prestart))
  //---- vstart >= vl ----
  val vstart_gte_vl_S2 = RegNext(RegNext(vstart_gte_vl))

  val tailReorg = MaskReorg.splash(tailS2, eewVdS2)
  val prestartReorg = MaskReorg.splash(prestartS2, eewVdS2)
  val mask16bS2 = RegNext(RegNext(MaskExtract(mask, uopIdx, eewVd)))
  val mask16bReorg = MaskReorg.splash(mask16bS2, eewVdS2)
  val updateType = Wire(Vec(16, UInt(2.W))) // 00: keep result  10: old_vd  11: write 1s
  for (i <- 0 until 16) {
    when (prestartReorg(i) || vstart_gte_vl_S2) {
      updateType(i) := 2.U
    }.elsewhen (tailReorg(i)) {
      updateType(i) := Mux(taS2, 3.U, 2.U)
    }.elsewhen (!vmS2 && !mask16bReorg(i)) {
      updateType(i) := Mux(maS2, 3.U, 2.U)
    }.otherwise {
      updateType(i) := 0.U
    }
  }
  // finalResult = result & bitsKeep | bitsReplace   (all are 128 bits)
  val bitsKeep = Cat(updateType.map(x => Mux(x(1), 0.U(8.W), ~0.U(8.W))).reverse)
  val bitsReplace = Cat(updateType.zipWithIndex.map({case (x, i) => 
        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(oldVdS2, 8)(i)))}).reverse)

  val vdResult = Cat(vIMac64bs(1).io.vd, vIMac64bs(0).io.vd)
  io.out.bits.vd := vdResult & bitsKeep | bitsReplace
  io.out.bits.vxsat := (Cat(vIMac64bs.map(_.io.vxsat).reverse) &
                   Cat(updateType.map(_(1) === false.B).reverse)).orR
}

object VerilogVIMac extends App {
  println("Generating the VIMac hardware")
  emitVerilog(new VIMac(), Array("--target-dir=build/vimac"))
}
