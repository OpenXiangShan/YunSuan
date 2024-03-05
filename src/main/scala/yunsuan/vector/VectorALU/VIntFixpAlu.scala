/**
  * Integer and fixed-point (except mult and div)
  *   
  * Perform below instructions:
  *     11.1  vadd, ...
  *     11.2  vwadd, ...
  *     11.3  vzext, ...
  *     11.4  vadc, vmadc, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.8  vmseq, vmsltu, ...
  *     11.9  vminu, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     12.1  vsadd, ...
  *     12.2  vaadd, ...
  *     12.4  vssrl, ...
  *     12.5  vnclip, ...
  */

package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._
import yunsuan.vector.VIFuParam._

class VIntFixpDecode extends Bundle {
  val sub = Bool()
  val misc = Bool()
  val cmp = Bool()
}

class VIntFixpAlu64b extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val opcode = Input(new VAluOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2_adder = Input(UInt(64.W))
    val vs2_misc = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val oldVd = Input(UInt(8.W))
    val narrow = Input(UInt(8.W))
    val isSub = Input(Bool())  // subtract
    val isMisc = Input(Bool())
    val isFixp = Input(Bool())
    val widen = Input(Bool())
    val widen_vs2 = Input(Bool())

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val cmpOut = Output(UInt(8.W))
    val vxsat = Output(UInt(8.W))
  })

  val fire = io.fire
  val srcTypeVs2 = io.srcType(0)

  val vIntAdder64b = Module(new VIntAdder64b)
  vIntAdder64b.io.opcode := io.opcode
  vIntAdder64b.io.info := io.info
  vIntAdder64b.io.srcType := io.srcType
  vIntAdder64b.io.vdType := io.vdType
  vIntAdder64b.io.vs1 := io.vs1
  vIntAdder64b.io.vs2 := io.vs2_adder
  vIntAdder64b.io.vmask := io.vmask
  vIntAdder64b.io.oldVd := io.oldVd
  vIntAdder64b.io.isSub := io.isSub
  vIntAdder64b.io.widen := io.widen
  vIntAdder64b.io.widen_vs2 := io.widen_vs2

  val vIntMisc64b = Module(new VIntMisc64b)
  vIntMisc64b.io.opcode := io.opcode
  vIntMisc64b.io.info := io.info
  vIntMisc64b.io.srcType := io.srcType
  vIntMisc64b.io.vdType := io.vdType
  vIntMisc64b.io.vs1 := io.vs1
  vIntMisc64b.io.vs2 := io.vs2_misc
  vIntMisc64b.io.vmask := io.vmask
  vIntMisc64b.io.narrow := io.narrow

  val vdAdderS1 = RegEnable(vIntAdder64b.io.vd, fire)
  val vdMiscS1 = RegEnable(vIntMisc64b.io.vd, fire)
  val isMiscS1 = RegEnable(io.isMisc, fire)
  val narrowVdMiscS1 = RegEnable(vIntMisc64b.io.narrowVd, fire)
  val cmpOutS1 = RegEnable(vIntAdder64b.io.cmpOut, fire)
  val isFixpS1 = RegEnable(io.isFixp, fire)

  val vFixPoint64b = Module(new VFixPoint64b)
  vFixPoint64b.io.opcode := RegEnable(io.opcode, fire)
  vFixPoint64b.io.info := RegEnable(io.info, fire)
  vFixPoint64b.io.sew := RegEnable(SewOH(io.vdType(1, 0)), fire)
  vFixPoint64b.io.isSub := RegEnable(io.isSub, fire)
  vFixPoint64b.io.isSigned := RegEnable(srcTypeVs2(3, 2) === 1.U, fire)
  vFixPoint64b.io.isNClip := RegEnable(io.opcode.isScalingShift && io.vdType(1,0) =/= srcTypeVs2(1,0), fire)
  vFixPoint64b.io.fromAdder := RegEnable(vIntAdder64b.io.toFixP, fire)
  vFixPoint64b.io.fromMisc := RegEnable(vIntMisc64b.io.toFixP, fire)

  io.vd := Mux(isFixpS1, vFixPoint64b.io.vd, Mux(isMiscS1, vdMiscS1, vdAdderS1))
  io.narrowVd := Mux(isFixpS1, vFixPoint64b.io.narrowVd, narrowVdMiscS1)
  io.cmpOut := cmpOutS1
  io.vxsat := vFixPoint64b.io.vxsat
}


class VIntFixpAlu extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val in = Input(new Bundle {
      val opcode = Input(new VAluOpcode)
      val info = new VIFuInfo
      val srcType = Vec(2, UInt(4.W))
      val vdType  = UInt(4.W)
      val vs1 = UInt(128.W)
      val vs2 = UInt(128.W)
      val old_vd = UInt(128.W)
      val mask16b = UInt(16.W)
    })
    val ctrl = Input(new Bundle {
      val narrow = Input(Bool())
      val vstart_gte_vl = Input(Bool())
    })
    val out = Output(new VIFuOutput)
  })

  val fire = io.fire
  val opcode = io.in.opcode
  val vs1 = io.in.vs1
  val vs2 = io.in.vs2
  val srcTypeVs2 = io.in.srcType(0)
  val srcTypeVs1 = io.in.srcType(1)
  val vdType = io.in.vdType
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val narrow = io.ctrl.narrow
  val uopIdx = io.in.info.uopIdx
  val vl = io.in.info.vl
  val vstart = io.in.info.vstart
  val signed = srcTypeVs2(3, 2) === 1.U
  val widen = opcode.isAddSub && srcTypeVs1(1, 0) =/= vdType(1, 0)
  val widen_vs2 = opcode.isAddSub && srcTypeVs2(1, 0) =/= vdType(1, 0)

  val truthTable = TruthTable(VIntFixpTable.table, VIntFixpTable.default)
  val decoderOut = decoder(QMCMinimizer, Cat(opcode.op), truthTable)
  val vIntFixpDecode = decoderOut.asTypeOf(new VIntFixpDecode)

  val isFixp = Mux(vIntFixpDecode.misc, opcode.isScalingShift, opcode.isSatAdd || opcode.isAvgAdd)

  //------- Two 64b modules form one 128b unit ------
  val vIntFixpAlu64bs = Seq.fill(2)(Module(new VIntFixpAlu64b))
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.fire := fire
    vIntFixpAlu64bs(i).io.opcode := opcode
    vIntFixpAlu64bs(i).io.info := io.in.info
    vIntFixpAlu64bs(i).io.srcType := io.in.srcType
    vIntFixpAlu64bs(i).io.vdType := io.in.vdType
    vIntFixpAlu64bs(i).io.narrow := narrow
    vIntFixpAlu64bs(i).io.isSub := vIntFixpDecode.sub
    vIntFixpAlu64bs(i).io.isMisc := vIntFixpDecode.misc
    vIntFixpAlu64bs(i).io.isFixp := isFixp
    vIntFixpAlu64bs(i).io.widen := widen
    vIntFixpAlu64bs(i).io.widen_vs2 := widen_vs2
  }
  //------- Input extension (widen & extension instrucitons) ----
  val vd_sub_srcType = vdType(1, 0) - srcTypeVs2(1, 0)
  // Extension instructions
  val vf2 = vd_sub_srcType === 1.U && opcode.isVext
  val vf4 = vd_sub_srcType === 2.U && opcode.isVext
  val vf8 = vd_sub_srcType === 3.U && opcode.isVext
  // Rearrange vs2
  when (widen_vs2) {
    vIntFixpAlu64bs(0).io.vs2_adder := Cat(vs2(95, 64), vs2(31, 0))
    vIntFixpAlu64bs(1).io.vs2_adder := Cat(vs2(127, 96), vs2(63, 32))
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs2_adder := vs2(63, 0)
    vIntFixpAlu64bs(1).io.vs2_adder := vs2(127, 64)
  }
  when (vf2) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(vs2(95, 64), vs2(31, 0))
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(vs2(127, 96), vs2(63, 32))
  }.elsewhen (vf4) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(vs2(111, 96), vs2(79, 64), vs2(47, 32), vs2(15, 0))
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(vs2(127, 112), vs2(95, 80), vs2(63, 48), vs2(31, 16))
  }.elsewhen (vf8) {
    vIntFixpAlu64bs(0).io.vs2_misc := Cat(Seq.tabulate(8)(i => vs2(16*i+7, 16*i)).reverse)
    vIntFixpAlu64bs(1).io.vs2_misc := Cat(Seq.tabulate(8)(i => vs2(16*i+15, 16*i+8)).reverse)
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs2_misc := vs2(63, 0)
    vIntFixpAlu64bs(1).io.vs2_misc := vs2(127, 64)
  }
  // Rearrange vs1 (need concern the case of narrow)
  when (widen || narrow) {
    vIntFixpAlu64bs(0).io.vs1 := Cat(vs1(95, 64), vs1(31, 0))
    vIntFixpAlu64bs(1).io.vs1 := Cat(vs1(127, 96), vs1(63, 32))
  }.otherwise {
    vIntFixpAlu64bs(0).io.vs1 := vs1(63, 0)
    vIntFixpAlu64bs(1).io.vs1 := vs1(127, 64)
  }

  //---- Input mask extraction ----
  val eewVd_is_1b = vdType === 15.U
  for (i <- 0 until 2) {
    vIntFixpAlu64bs(i).io.vmask := 
      mask16_to_2x8(io.in.mask16b, Mux(eewVd_is_1b, eewVs1, eewVd))(i)
    vIntFixpAlu64bs(i).io.oldVd := // only for compare instrution
      mask16_to_2x8(MaskExtract(io.in.old_vd, io.in.info.uopIdx, eewVs1), eewVs1)(i)
  }

  /**
   * Output stage
   */
  val uopIdxS1 = RegEnable(uopIdx, fire)
  val opcodeS1 = RegEnable(opcode, fire)
  val old_vd_S1 = Wire(UInt(128.W))
  old_vd_S1 := RegEnable(io.in.old_vd, fire)
  val eewVs1S1 = RegEnable(eewVs1, fire)
  val eewVdS1 = RegEnable(eewVd, fire)
  val narrowS1 = RegEnable(narrow, fire)
  val cmpFlagS1 = RegEnable(vIntFixpDecode.cmp, fire) // Compare and carry-out
  val vmS1 = RegEnable(io.in.info.vm, fire)
  val taS1 = RegEnable(io.in.info.ta, fire)
  val maS1 = RegEnable(io.in.info.ma, fire)
  val mask16bS1 = RegEnable(io.in.mask16b, fire)
  val vl_S1 = RegEnable(vl, fire)
  val vstartS1 = RegEnable(vstart, fire)
  //---- Narrowing vd rearrangement ----
  val catNarrowVd = Cat(vIntFixpAlu64bs(1).io.narrowVd, vIntFixpAlu64bs(0).io.narrowVd)
  val vdOfNarrow = Mux(uopIdxS1(0), Cat(catNarrowVd, old_vd_S1(63, 0)),
                       Cat(old_vd_S1(127, 64), catNarrowVd))
  //---- Compare/carry-out vd rearrangement ----
  val cmpOuts = vIntFixpAlu64bs.map(_.io.cmpOut)
  val cmpOut128b = Mux1H(eewVs1S1.oneHot, Seq(8,4,2,1).map(
                    k => Cat(0.U((128-2*k).W), cmpOuts(1)(k-1,0), cmpOuts(0)(k-1,0))))
  val cmpOutOff128b = Mux1H(eewVs1S1.oneHot, Seq(8,4,2,1).map(
                    k => Cat(0.U((128-2*k).W), ~0.U((2*k).W))))
  val shiftCmpOut = Wire(UInt(7.W))
  shiftCmpOut := Mux1H(eewVs1S1.oneHot, Seq(4,3,2,1).map(i => uopIdxS1(2, 0) << i))
  val cmpOutKeep = Wire(UInt(128.W))
  cmpOutKeep := cmpOut128b << shiftCmpOut
  val cmpOutOff = Wire(UInt(128.W))
  cmpOutOff := ~(cmpOutOff128b << shiftCmpOut)
  val cmpOutResult = old_vd_S1 & cmpOutOff | cmpOutKeep // Compare and carry-out

  /**
   * Output tail/prestart/mask handling for eewVd >= 8
   */
  //---- Tail gen ----
  // val tail = TailGen(vl, uopIdx, Mux(narrow, eewVs2, eewVd))
  val tail = TailGen(Mux(opcode.isVmvsx, 1.U, vl), uopIdx, eewVd, narrow)
  val tailS1 = RegEnable(tail, fire)
  //---- Prestart gen ----
  // val prestart = PrestartGen(vstart, uopIdx, Mux(narrow, eewVs2, eewVd))
  val prestart = PrestartGen(vstart, uopIdx, eewVd, narrow)
  val prestartS1 = RegEnable(prestart, fire)
  //---- vstart >= vl ----
  val vstart_gte_vl_S1 = RegEnable(io.ctrl.vstart_gte_vl, fire)

  val tailReorg = MaskReorg.splash(tailS1, eewVdS1)
  val prestartReorg = MaskReorg.splash(prestartS1, eewVdS1)
  val mask16bReorg = MaskReorg.splash(mask16bS1, eewVdS1)
  val updateType = Wire(Vec(16, UInt(2.W))) // 00: keep result  10: old_vd  11: write 1s
  for (i <- 0 until 16) {
    when (prestartReorg(i) || vstart_gte_vl_S1) {
      updateType(i) := 2.U
    }.elsewhen (tailReorg(i)) {
      updateType(i) := Mux(taS1, 3.U, 2.U)
    }.elsewhen (opcodeS1.isAddWithCarry || opcodeS1.isVmerge) {
      updateType(i) := 0.U
    }.elsewhen (!vmS1 && !mask16bReorg(i)) {
      updateType(i) := Mux(maS1, 3.U, 2.U)
    }.otherwise {
      updateType(i) := 0.U
    }
  }
  // finalResult = result & bitsKeep | bitsReplace   (all are 128 bits)
  val bitsKeep = Cat(updateType.map(x => Mux(x(1), 0.U(8.W), ~0.U(8.W))).reverse)
  val bitsReplace = Cat(updateType.zipWithIndex.map({case (x, i) => 
        Mux(!x(1), 0.U(8.W), Mux(x(0), ~0.U(8.W), UIntSplit(old_vd_S1, 8)(i)))}).reverse)

  /**
   * Output tail/prestart/mask handling for eewVd == 1
   */
  val tail_1b_temp = UIntToCont0s(vl_S1(wVL-2, 0), wVL-1)
  require(tail_1b_temp.getWidth == 128)
  val tail_1b = Mux(vl_S1 === 128.U, 0.U(128.W), tail_1b_temp)
  val prestart_1b = UIntToCont1s(vstartS1, wVSTART)
  require(prestart_1b.getWidth == 128)
  val bitsKeep_1b = ~(prestart_1b | tail_1b)
  val bitsReplace_1b = Mux(vstart_gte_vl_S1, old_vd_S1, 
                       prestart_1b & old_vd_S1 | tail_1b)

  val bitsKeepFinal = Mux(RegEnable(eewVd_is_1b, fire), bitsKeep_1b, bitsKeep)
  val bitsReplaceFinal = Mux(RegEnable(eewVd_is_1b, fire), bitsReplace_1b, bitsReplace)

  val vdResult = Mux(narrowS1, vdOfNarrow, 
              Mux(cmpFlagS1, cmpOutResult, Cat(vIntFixpAlu64bs.map(_.io.vd).reverse)))
  io.out.vd := vdResult & bitsKeepFinal | bitsReplaceFinal
  when (!narrowS1) {
    io.out.vxsat := (Cat(vIntFixpAlu64bs.map(_.io.vxsat).reverse) &
                     Cat(updateType.map(_(1) === false.B).reverse)).orR
  }.otherwise {
    io.out.vxsat := (Cat(vIntFixpAlu64bs.map(_.io.vxsat(VLENB/4 - 1, 0)).reverse) &
                     Mux(uopIdxS1(0), Cat(updateType.drop(VLENB/2).map(_(1) === false.B).reverse),
                                      Cat(updateType.take(VLENB/2).map(_(1) === false.B).reverse))
                     ).orR
  }


  //---- Some methods ----
  def mask16_to_2x8(maskIn: UInt, sew: SewOH): Seq[UInt] = {
    require(maskIn.getWidth == 16)
    val result16 = Mux1H(Seq(
      sew.is8  -> maskIn,
      sew.is16 -> Cat(0.U(4.W), maskIn(7, 4), 0.U(4.W), maskIn(3, 0)),
      sew.is32 -> Cat(0.U(6.W), maskIn(3, 2), 0.U(6.W), maskIn(1, 0)),
      sew.is64 -> Cat(0.U(7.W), maskIn(1), 0.U(7.W), maskIn(0)),
    ))
    Seq(result16(7, 0), result16(15, 8))
  }
}