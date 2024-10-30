
/**
  * Perform below instructions:
  *     11.3  vzext, ...
  *     11.5  vand, ...
  *     11.6  vsll, ...
  *     11.7  vnsrl, ...
  *     11.15 vmerge
  *     11.16 vmv.v.
  *     Part of 12.4
  *     Part of 12.5
  *     15.1  vmand.mm, ...
  *     16.1  vmv.s.x (only this one) (only supports XLEN=64)
  */
package yunsuan.vector.alu

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.vector.alu.VAluOpcode._
import yunsuan.vector.VectorConvert.util.CLZ

class VIntMisc64b extends Module {
  val io = IO(new Bundle {
    val opcode = Input(new VAluOpcode)
    val info = Input(new VIFuInfo)
    val srcType = Input(Vec(2, UInt(4.W)))
    val vdType  = Input(UInt(4.W))
    val vs1 = Input(UInt(64.W))
    val vs2 = Input(UInt(64.W))
    val vmask = Input(UInt(8.W))
    val narrow = Input(Bool())

    val vd = Output(UInt(64.W))
    val narrowVd = Output(UInt(32.W))
    val toFixP = Output(new MiscToFixP)
  })

  val opcode = io.opcode
  val srcTypeVs2 = io.srcType(0)
  val srcTypeVs1 = io.srcType(1)
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVd = SewOH(io.vdType(1, 0))
  val vs1 = io.vs1
  val vs2 = io.vs2
  val vm = io.info.vm
  val signed = srcTypeVs2(3, 2) === 1.U
  val vd_sub_srcType = io.vdType(1, 0) - srcTypeVs2(1, 0)
  val expdIdx = io.info.uopIdx(2, 0)

  //---- Extension instructions ----
  val extSign = signed  // 0: z   1: s
  val vf2 = vd_sub_srcType === 1.U
  val vf4 = vd_sub_srcType === 2.U
  val expdIdxOH_vf2 = Seq.tabulate(2)(i => expdIdx(0) === i.U)
  val expdIdxOH_vf4 = Seq.tabulate(4)(i => expdIdx(1,0) === i.U)
  val expdIdxOH_vf8 = Seq.tabulate(8)(i => expdIdx === i.U)
  val extResult = Wire(UInt(64.W))
  when (vf2) {  // sew = 16/32/64
    extResult := Mux(eewVd.is16, BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 8),
                 Mux(eewVd.is32, BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 16),
                               BitsExtend.vector(Mux1H(expdIdxOH_vf2.take(2), UIntSplit(vs2, 32)), 64, extSign, 32)))
  }.elsewhen (vf4) {  // sew = 32/64
    extResult := Mux(eewVd.is32, BitsExtend.vector(Mux1H(expdIdxOH_vf4.take(4), UIntSplit(vs2, 16)), 64, extSign, 8),
                               BitsExtend.vector(Mux1H(expdIdxOH_vf4.take(4), UIntSplit(vs2, 16)), 64, extSign, 16))
  }.otherwise {  //vf8  sew = 64
    extResult := BitsExtend(Mux1H(expdIdxOH_vf8, UIntSplit(vs2, 8)), 64, extSign)
  }

  //---- Bitwise Logical instructions ----
  val bitLogical = Mux1H(Seq(
    (opcode.isVand) -> (vs2 & vs1),
    (opcode.isVnand) -> ~(vs2 & vs1),
    (opcode.isVandn) -> (vs2 & ~vs1),
    (opcode.isVxor) -> (vs2 ^ vs1),
    (opcode.isVor) -> (vs2 | vs1),
    (opcode.isVnor) -> ~(vs2 | vs1),
    (opcode.isVorn) -> (vs2 | ~vs1),
    (opcode.isVxnor) -> ~(vs2 ^ vs1),
  ))

  /**
    * Shift: vsll, vsrl, vsra, vnsrl, vnsra (vssrl, vssra and vnclipu/vnclip)
    */
  val signed_shift = opcode.isSignedShift
  def shiftOnce(n: Int, data: UInt): (UInt, Bool, Bool) = { // n: number of shift bits
    val len = data.getWidth
    require(len > n)
    val rnd_high = data(n-1)
    val rnd_tail = if (n == 1) true.B else {data(n-2, 0) === 0.U}
    //             result of bit-shift                     v[d-1]   v[d-2:0] == 0
    (Cat(Fill(n, data(len-1) && signed_shift), data(len-1, n)), rnd_high, rnd_tail)
  }
  // Shift amount is dynamic
  def dynamicShift(shiftAmount: UInt, data: UInt): (UInt, Bool, Bool) = {
    val width = shiftAmount.getWidth
    val shiftSeq = Seq(1, 2, 4, 8, 16, 32)
    // (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(data) {
    //   case (data, (bit, n)) => Mux(bit, shiftOnce(n, data), data)
    // }
    val dataInit = (data, false.B, true.B)
    (shiftAmount.asBools).zip(shiftSeq.take(width)).foldLeft(dataInit) {
      case ((data, rnd_high, rnd_tail), (bit, n)) => {
        val shiftOnceResult = shiftOnce(n, data)
        val data_update = Mux(bit, shiftOnceResult._1, data)
        val rnd_high_update = Mux(bit, shiftOnceResult._2, rnd_high)
        val rnd_tail_update = Mux(bit, shiftOnceResult._3 && rnd_tail && !rnd_high, rnd_tail)
        (data_update, rnd_high_update, rnd_tail_update)
      }
    }
  }
  // Shift SEW bits data (one element)
  def shiftOneElement(shiftAmount: UInt, data: UInt, sew: Int): (UInt, Bool, Bool) = {
    sew match {
      case 8  => dynamicShift(shiftAmount(2, 0), data)
      case 16 => dynamicShift(shiftAmount(3, 0), data)
      case 32 => dynamicShift(shiftAmount(4, 0), data)
      case 64 => {  // For sew=64, the uimm should perform zero-extending
        // val shiftAmount_uimm = Cat(Mux(uop.ctrl.vi, false.B, shiftAmount(5)), shiftAmount(4, 0))
        dynamicShift(shiftAmount(5, 0), data)
      }
    }
  }

  // Handle shift left
  val leftShift = opcode.isLeftShift
  val vs2_reverse = Cat(vs2.asBools) // bit reverse
  val vs2_adjust = Mux(leftShift, vs2_reverse, vs2)
  val vs1_revsByElem = MuxCase(vs1, Seq(  // reverse vs1 by element when left-shift
    (leftShift && eewVd.is32) -> Cat(UIntSplit(vs1, 32)),
    (leftShift && eewVd.is16) -> Cat(UIntSplit(vs1, 16)),
    (leftShift && eewVd.is8)  -> Cat(UIntSplit(vs1, 8))
  ))
  // Handle narrow instruction
  val vs1Split = UIntSplit(vs1_revsByElem, 8)
  val vs1_adjust = Wire(UInt(64.W))
  // val narrow = is_shift && srcTypeVs2(1, 0) =/= io.vdType(1, 0)
  val narrow = io.narrow
  when (narrow && !expdIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(3), 0.U(8.W), vs1Split(2), 0.U(8.W), vs1Split(1), 0.U(8.W), vs1Split(0))
  }.elsewhen (narrow && expdIdx(0)) {
    vs1_adjust := Cat(0.U(8.W), vs1Split(7), 0.U(8.W), vs1Split(6), 0.U(8.W), vs1Split(5), 0.U(8.W), vs1Split(4))
  }.otherwise {
    vs1_adjust := vs1_revsByElem
  }
  
  def shift(sew: Int): Seq[(UInt, Bool, Bool)] = {
    UIntSplit(vs2_adjust, sew) zip UIntSplit(vs1_adjust, sew) map {case (vs2, vs1) => shiftOneElement(vs1, vs2, sew)}
  }
  // val shiftOut = MuxCase(shift(64), Seq(
  //   Mux(uop.ctrl.narrow, sew.is16, sew.is32) -> shift(32),
  //   Mux(uop.ctrl.narrow, sew.is8,  sew.is16) -> shift(16),
  //   Mux(uop.ctrl.narrow, false.B,  sew.is8)  -> shift(8)
  // ))

  val (shift64, shift32, shift16, shift8) = (shift(64), shift(32),shift(16), shift(8))
  val shiftOut = Wire(UInt(64.W))
  // Different SEW cases
  when (Mux(narrow, eewVd.is16, eewVd.is32)) {
    shiftOut := Cat(shift32.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(Fill(4, shift32(1)._2), Fill(4, shift32(0)._2))
    io.toFixP.rnd_tail := Cat(Fill(4, shift32(1)._3), Fill(4, shift32(0)._3))
  }.elsewhen (Mux(narrow, eewVd.is8,  eewVd.is16)) {
    shiftOut := Cat(shift16.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(Fill(2, shift16(3)._2), Fill(2, shift16(2)._2), Fill(2, shift16(1)._2), Fill(2, shift16(0)._2))
    io.toFixP.rnd_tail := Cat(Fill(2, shift16(3)._3), Fill(2, shift16(2)._3), Fill(2, shift16(1)._3), Fill(2, shift16(0)._3))
  }.elsewhen (Mux(narrow, false.B,  eewVd.is8)) {
    shiftOut := Cat(shift8.map(_._1).reverse)
    io.toFixP.rnd_high := Cat(shift8.map(_._2).reverse)
    io.toFixP.rnd_tail := Cat(shift8.map(_._3).reverse)
  }.otherwise {
    shiftOut := shift64(0)._1
    io.toFixP.rnd_high := Fill(8, shift64(0)._2)
    io.toFixP.rnd_tail := Fill(8, shift64(0)._3)
  }
  io.toFixP.shiftOut := shiftOut
  
  val shiftResult = Mux(leftShift, Cat(shiftOut.asBools), shiftOut)
  io.narrowVd := Mux1H(Seq(
    eewVd.is32 -> shiftOut(31, 0),
    eewVd.is16 -> Cat(shiftOut(47, 32), shiftOut(15, 0)),
    eewVd.is8  -> Cat(shiftOut(55, 48), shiftOut(39, 32), shiftOut(23, 16), shiftOut(7, 0))
  ))
  
  /**
    * Integer Merge/Move, vmv.s.x
    */
  // Adjust vmask. E.g., if sew==32: 000000ab -> aaaabbbb   
  val vmask_adjust = Mux1H(eewVd.oneHot, Seq(1, 2, 4, 8).map(k => 
    Cat(Seq.tabulate(8/k)(i => Fill(k, io.vmask(i))).reverse)
  ))
  val mergeResult = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    mergeResult(i) := Mux(vmask_adjust(i), vs1(8*i+7, 8*i), vs2(8*i+7, 8*i))
  }
  val mergeMove = Mux(vm || opcode.isVmvsx, vs1, mergeResult.asUInt)

  /**
   * Zvbb vbrev.v vbrev8.v vrev8.v
   */
  val revResult = Wire(UInt(64.W))
  val brevResult_8      = Wire(Vec(8, UInt(8.W)))
  val brevResult_8_tmp  = Wire(Vec(8, UInt(8.W)))
  val brevResult_16     = Wire(Vec(4, UInt(16.W)))
  val brevResult_16_tmp = Wire(Vec(4, UInt(16.W)))
  val brevResult_32     = Wire(Vec(2, UInt(32.W)))
  val brevResult_32_tmp = Wire(Vec(2, UInt(32.W)))
  val brevResult_64     = Wire(Vec(1, UInt(64.W)))
  val brevResult_64_tmp = Wire(Vec(1, UInt(64.W)))
  brevResult_8_tmp  := vs2.asTypeOf(brevResult_8_tmp)
  brevResult_16_tmp := vs2.asTypeOf(brevResult_16_tmp)
  brevResult_32_tmp := vs2.asTypeOf(brevResult_32_tmp)
  brevResult_64_tmp := vs2.asTypeOf(brevResult_64_tmp)

  for (i <- 0 until 8) {
    brevResult_8(i) := VecInit(brevResult_8_tmp(i).asBools.reverse).asUInt
  }
  for (i <- 0 until 4) {
    brevResult_16(i) := VecInit(brevResult_16_tmp(i).asBools.reverse).asUInt
  }
  for (i <- 0 until 2) {
    brevResult_32(i) := VecInit(brevResult_32_tmp(i).asBools.reverse).asUInt
  }
  for (i <- 0 until 1) {
    brevResult_64(i) := VecInit(brevResult_64_tmp(i).asBools.reverse).asUInt
  }

  val brev8Result = Wire(Vec(8, UInt(8.W)))
  val brev8Result_tmp = Wire(Vec(8, UInt(8.W)))
  brev8Result_tmp := vs2.asTypeOf(brev8Result_tmp)
  for (i <- 0 until 8) {
    brev8Result(i) := VecInit(brev8Result_tmp(i).asBools.reverse).asUInt
  }

  val rev8Result_16     = Wire(Vec(4, Vec(2, UInt(8.W))))
  val rev8Result_16_tmp = Wire(Vec(4, Vec(2, UInt(8.W))))
  val rev8Result_32     = Wire(Vec(2, Vec(4, UInt(8.W))))
  val rev8Result_32_tmp = Wire(Vec(2, Vec(4, UInt(8.W))))
  val rev8Result_64     = Wire(Vec(8, UInt(8.W)))
  val rev8Result_64_tmp = Wire(Vec(8, UInt(8.W)))
  rev8Result_16_tmp := vs2.asTypeOf(rev8Result_16_tmp)
  rev8Result_32_tmp := vs2.asTypeOf(rev8Result_32_tmp)
  rev8Result_64_tmp := vs2.asTypeOf(rev8Result_64_tmp)

  for (i <- 0 until 4) {
    for (j <- 0 until 2) {
      rev8Result_16(i)(1-j) := rev8Result_16_tmp(i)(j)
    }
  }
  for (i <- 0 until 2) {
    for (j <- 0 until 4) {
      rev8Result_32(i)(3-j) := rev8Result_32_tmp(i)(j)
    }
  }
  for (i <- 0 until 8) {
    rev8Result_64(7-i) := rev8Result_64_tmp(i)
  }

  revResult := Mux1H(
    Seq(
      (opcode.op === vbrev) && eewVd.is8,
      (opcode.op === vbrev) && eewVd.is16,
      (opcode.op === vbrev) && eewVd.is32,
      (opcode.op === vbrev) && eewVd.is64,
      opcode.op === vbrev8,
      (opcode.op === vrev8) && eewVd.is8,
      (opcode.op === vrev8) && eewVd.is16,
      (opcode.op === vrev8) && eewVd.is32,
      (opcode.op === vrev8) && eewVd.is64,
    ),
    Seq(
      brevResult_8.asUInt,
      brevResult_16.asUInt,
      brevResult_32.asUInt,
      brevResult_64.asUInt,
      brev8Result.asUInt,
      vs2,
      rev8Result_16.asUInt,
      rev8Result_32.asUInt,
      rev8Result_64.asUInt,
    )
  )

  /**
   * vclz.v
   * vctz.v
   * vcpop.v
   */
  val countResult = Wire(UInt(64.W))
  val countResult_8  = Wire(Vec(4, UInt(8.W)))
  val countResult_16 = Wire(Vec(2, UInt(16.W)))
  val countResult_32 = Wire(UInt(32.W))
  val countResult_64 = Wire(UInt(64.W))
  val pop_8  = Wire(Vec(8, UInt(8.W)))
  val pop_16 = Wire(Vec(4, UInt(5.W)))
  val pop_32 = Wire(Vec(2, UInt(6.W)))
  val pop_64 = Wire(Vec(1, UInt(7.W)))
  val cnt8  = Wire(Vec(8, UInt(8.W)))
  val cnt16 = Wire(Vec(4, UInt(16.W)))
  val cnt32 = Wire(Vec(2, UInt(32.W)))
  val cnt64 = Wire(Vec(1, UInt(64.W)))

  pop_8 := vs2.asTypeOf(pop_8)

  for (i <- 0 until 4) {
    countResult_8(i) := Mux(opcode.isClz, vs2(8*i+7, 8*i), VecInit(vs2(8*i+7, 8*i).asBools.reverse).asUInt)
  }
  for (i <- 0 until 2) {
    countResult_16(i) := Mux1H(
      Seq(
        eewVd.is8,
        eewVd.is16,
      ),
      Seq(
        Mux(opcode.isClz, vs2(8*i+7+32,8*i+32) << 8, VecInit(vs2(8*i+7+32,8*i+32).asBools.reverse).asUInt << 8) | (1.U << 7),
        Mux(opcode.isClz, vs2(16*i+15,16*i), VecInit(vs2(16*i+15,16*i).asBools.reverse).asUInt),
      )
    )
  }

  countResult_32 := Mux1H(
    Seq(
      eewVd.is8,
      eewVd.is16,
      eewVd.is32,
    ),
    Seq(
      Mux(opcode.isClz, vs2(55, 48) << 24, VecInit(vs2(55, 48).asBools.reverse).asUInt << 24) | (1.U << 23),
      Mux(opcode.isClz, vs2(47, 32) << 16, VecInit(vs2(47, 32).asBools.reverse).asUInt << 16) | (1.U << 15),
      Mux(opcode.isClz, vs2(31, 0), VecInit(vs2(31, 0).asBools.reverse).asUInt),
    )
  )
  countResult_64 := Mux1H(
    Seq(
      eewVd.is8,
      eewVd.is16,
      eewVd.is32,
      eewVd.is64,
    ),
    Seq(
      Mux(opcode.isClz, vs2(63, 56) << 56, VecInit(vs2(63, 56).asBools.reverse).asUInt << 56) | (1.U << 55),
      Mux(opcode.isClz, vs2(63, 48) << 48, VecInit(vs2(63, 48).asBools.reverse).asUInt << 48) | (1.U << 47),
      Mux(opcode.isClz, vs2(63, 32) << 32, VecInit(vs2(63, 32).asBools.reverse).asUInt << 32) | (1.U << 31),
      Mux(opcode.isClz, vs2, VecInit(vs2.asBools.reverse).asUInt),
    )
  )

  val cnt16_0_tmp = CLZ(countResult_16(0))
  val cnt16_1_tmp = CLZ(countResult_16(1))
  val cnt32_tmp = CLZ(countResult_32)
  val cnt64_tmp = CLZ(countResult_64)

  for (i <- 0 until 4) {
    cnt8(i) := Mux(opcode.isVcpop, PopCount(pop_8(i)), CLZ(countResult_8(i)))
  }
  cnt8(4)  := Mux(opcode.isVcpop, PopCount(pop_8(4)), cnt16_0_tmp)
  cnt8(5)  := Mux(opcode.isVcpop, PopCount(pop_8(5)), cnt16_1_tmp)
  cnt8(6)  := Mux(opcode.isVcpop, PopCount(pop_8(6)), cnt32_tmp)
  cnt8(7)  := Mux(opcode.isVcpop, PopCount(pop_8(7)), cnt64_tmp)

  pop_16(0) := cnt8(0) +& cnt8(1)
  pop_16(1) := cnt8(2) +& cnt8(3)
  pop_16(2) := cnt8(4) +& cnt8(5)
  pop_16(3) := cnt8(6) +& cnt8(7)

  pop_32(0) := pop_16(0) +& pop_16(1)
  pop_32(1) := pop_16(2) +& pop_16(3)

  pop_64(0) := pop_32(0) +& pop_32(1)

  cnt16(0) := Mux(opcode.isVcpop, pop_16(0), cnt16_0_tmp)
  cnt16(1) := Mux(opcode.isVcpop, pop_16(1), cnt16_1_tmp)
  cnt16(2) := Mux(opcode.isVcpop, pop_16(2), cnt32_tmp)
  cnt16(3) := Mux(opcode.isVcpop, pop_16(3), cnt64_tmp)
  cnt32(0) := Mux(opcode.isVcpop, pop_32(0), cnt32_tmp)
  cnt32(1) := Mux(opcode.isVcpop, pop_32(1), cnt64_tmp)
  cnt64(0) := Mux(opcode.isVcpop, pop_64(0), cnt64_tmp)

  countResult := Mux1H(
    Seq(
      opcode.isVCount && eewVd.is8,
      opcode.isVCount && eewVd.is16,
      opcode.isVCount && eewVd.is32,
      opcode.isVCount && eewVd.is64,
    ),
    Seq(
      cnt8.asUInt,
      cnt16.asUInt,
      cnt32.asUInt,
      cnt64.asUInt,
    )
  )

  val vroResult = Wire(UInt(64.W))
  val vroResult_8  = Wire(Vec(8, UInt(8.W)))
  val vroResult_16 = Wire(Vec(4, UInt(16.W)))
  val vroResult_32 = Wire(Vec(2, UInt(32.W)))
  val vroResult_64 = Wire(Vec(1, UInt(64.W)))
  vroResult_8  := vs2.asTypeOf(vroResult_8)
  vroResult_16 := vs2.asTypeOf(vroResult_16)
  vroResult_32 := vs2.asTypeOf(vroResult_32)
  vroResult_64 := vs2.asTypeOf(vroResult_64)
  val vroShift8  = Wire(Vec(8, UInt(3.W)))
  val vroShift16 = Wire(Vec(4, UInt(4.W)))
  val vroShift32 = Wire(Vec(2, UInt(5.W)))
  val vroShift64 = Wire(Vec(1, UInt(6.W)))
  val vroShift8_neg  = Wire(Vec(8, UInt(3.W)))
  val vroShift16_neg = Wire(Vec(4, UInt(4.W)))
  val vroShift32_neg = Wire(Vec(2, UInt(5.W)))
  val vroShift64_neg = Wire(Vec(1, UInt(6.W)))

  for (i <- 0 until 8) {
    vroShift8(i)     :=   vs1(8*i+2, 8*i)
    vroShift8_neg(i) := (~vs1(8*i+2, 8*i)).asUInt + 1.U
  }
  for (i <- 0 until 4) {
    vroShift16(i)     :=   vs1(16*i+3, 16*i)
    vroShift16_neg(i) := (~vs1(16*i+3, 16*i)).asUInt + 1.U
  }
  for (i <- 0 until 2) {
    vroShift32(i)     :=   vs1(32*i+4, 32*i)
    vroShift32_neg(i) := (~vs1(32*i+4, 32*i)).asUInt + 1.U
  }
  for (i <- 0 until 1) {
    vroShift64(i)     :=   vs1(64*i+5, 64*i)
    vroShift64_neg(i) := (~vs1(64*i+5, 64*i)).asUInt + 1.U
  }

  val vroResult_8_tmp  = Wire(Vec(8, UInt(8.W)))
  val vroResult_16_tmp = Wire(Vec(4, UInt(16.W)))
  val vroResult_32_tmp = Wire(Vec(2, UInt(32.W)))
  val vroResult_64_tmp = Wire(Vec(1, UInt(64.W)))

  // vs2 << vs1 is equal to (vs2.reverse >> vs1).reverse
  for (i <- 0 until 8) {
    vroResult_8_tmp(i) := Mux1H(
      Seq(
        opcode.isVrol,
        opcode.isVror,
      ),
      Seq(
        VecInit(shiftOneElement(vroShift8(i), VecInit(vroResult_8(i).asBools.reverse).asUInt, 8)._1.asBools.reverse).asUInt | shiftOneElement(vroShift8_neg(i), vroResult_8(i), 8)._1,
        VecInit(shiftOneElement(vroShift8_neg(i), VecInit(vroResult_8(i).asBools.reverse).asUInt, 8)._1.asBools.reverse).asUInt | shiftOneElement(vroShift8(i), vroResult_8(i), 8)._1,
      )
    )
  }
  for (i <- 0 until 4) {
    vroResult_16_tmp(i) := Mux1H(
      Seq(
        opcode.isVrol,
        opcode.isVror,
      ),
      Seq(
        VecInit(shiftOneElement(vroShift16(i), VecInit(vroResult_16(i).asBools.reverse).asUInt, 16)._1.asBools.reverse).asUInt | shiftOneElement(vroShift16_neg(i), vroResult_16(i), 16)._1,
        VecInit(shiftOneElement(vroShift16_neg(i), VecInit(vroResult_16(i).asBools.reverse).asUInt, 16)._1.asBools.reverse).asUInt | shiftOneElement(vroShift16(i), vroResult_16(i), 16)._1,
      )
    )
  }
  for (i <- 0 until 2) {
    vroResult_32_tmp(i) := Mux1H(
      Seq(
        opcode.isVrol,
        opcode.isVror,
      ),
      Seq(
        VecInit(shiftOneElement(vroShift32(i), VecInit(vroResult_32(i).asBools.reverse).asUInt, 32)._1.asBools.reverse).asUInt | shiftOneElement(vroShift32_neg(i), vroResult_32(i), 32)._1,
        VecInit(shiftOneElement(vroShift32_neg(i), VecInit(vroResult_32(i).asBools.reverse).asUInt, 32)._1.asBools.reverse).asUInt | shiftOneElement(vroShift32(i), vroResult_32(i), 32)._1,
      )
    )
  }
  for (i <- 0 until 1) {
    vroResult_64_tmp(i) := Mux1H(
      Seq(
        opcode.isVrol,
        opcode.isVror,
      ),
      Seq(
        VecInit(shiftOneElement(vroShift64(i), VecInit(vroResult_64(i).asBools.reverse).asUInt, 64)._1.asBools.reverse).asUInt | shiftOneElement(vroShift64_neg(i), vroResult_64(i), 64)._1,
        VecInit(shiftOneElement(vroShift64_neg(i), VecInit(vroResult_64(i).asBools.reverse).asUInt, 64)._1.asBools.reverse).asUInt | shiftOneElement(vroShift64(i), vroResult_64(i), 64)._1,
      )
    )
  }
  vroResult := Mux1H(
    Seq(
      eewVd.is8,
      eewVd.is16,
      eewVd.is32,
      eewVd.is64,
    ),
    Seq(
      vroResult_8_tmp.asUInt,
      vroResult_16_tmp.asUInt,
      vroResult_32_tmp.asUInt,
      vroResult_64_tmp.asUInt,
    )
  )

  /**
   * vwsll.vv vwsll.vx vwsll.vi
   * select vs2,vs1 according to uopidx
   *
   * sew=32, eewVd=64
   *                      Fu1                                                                                                                   Fu0
   *                 vs2[3]/vs2[1]                                                                                                        vs2[2]/vs2[0]
   * uopidx=0 vd = zeroExt(vs2[1]) shift log2(vs1[1](5,0))                                                             vd = zeroExt(vs2[0]) shift log2(vs1[0](5,0))
   * uopidx=1 vd = zeroExt(vs2[3]) shift log2(vs1[3](5,0))                                                             vd = zeroExt(vs2[2]) shift log2(vs1[2](5,0))
   * 
   * sew=16, eewVd=32
   *                              Fu1                                                                                                         Fu0
   *                      vs2[7],vs2[5]/vs2[3],vs2[1]                                                                               vs2[6],vs2[4]/vs2[2],vs2[0]
   * uopidx=0 vd = Cat(zeroExt(vs2[3]) shift log2(vs1[3](4,0)), zeroExt(vs2[1]) shift log2(vs1[1](4,0)))            vd = Cat(zeroExt(vs2[2]) shift log2(vs1[2](4,0)), zeroExt(vs2[0]) shift log2(vs1[0](4,0)))
   * uopidx=1 vd = Cat(zeroExt(vs2[7]) shift log2(vs1[7](4,0)), zeroExt(vs2[5]) shift log2(vs1[5](4,0)))            vd = Cat(zeroExt(vs2[6]) shift log2(vs1[6](4,0)), zeroExt(vs2[4]) shift log2(vs1[4](4,0)))
   * 
   * sew=8, eewVd=16
   *                              Fu1                                                                                                        Fu0
   *        vs2[15],vs2[13],vs2[11],vs2[9]/vs2[7],vs2[5],vs2[3],vs2[1]                                                  vs2[14],vs2[12],vs2[10],vs2[8]/vs2[6],vs2[4],vs2[2],vs2[0]
   * uopidx=0         vd = Cat(zeroExt(vs2[7]) shift log2(vs1[7](3,0)) ... )                                        vd = Cat(zeroExt(vs2[6]) shift log2(vs1[6](3,0)) ... )
   * uopidx=1         vd = Cat(zeroExt(vs2[15]) shift log2(vs1[15](3,0)) ... )                                      vd = Cat(zeroExt(vs2[14]) shift log2(vs1[14](3,0)) ... )
   */
  val wsllResult = Wire(UInt(64.W))
  val vs1_tmp_8  = Wire(Vec(4, UInt(8.W)))
  val vs1_tmp_16 = Wire(Vec(2, UInt(16.W)))
  val vs1_tmp_32 = Wire(Vec(1, UInt(32.W)))
  val vs2_tmp_8  = Wire(Vec(4, UInt(8.W)))
  val vs2_tmp_16 = Wire(Vec(2, UInt(16.W)))
  val vs2_tmp_32 = Wire(Vec(1, UInt(32.W)))
  val uopIdx = io.info.uopIdx
  val selectHigh32 = uopIdx(0).asBool
  val vs1_tmp = Wire(UInt(32.W))
  val vs2_tmp = Wire(UInt(32.W))
  vs1_tmp := Mux(selectHigh32, vs1(63, 32) ,vs1(31, 0))
  vs2_tmp := Mux(selectHigh32, vs2(63, 32) ,vs2(31, 0))
  vs1_tmp_8  := vs1_tmp.asTypeOf(vs1_tmp_8)
  vs1_tmp_16 := vs1_tmp.asTypeOf(vs1_tmp_16)
  vs1_tmp_32 := vs1_tmp.asTypeOf(vs1_tmp_32)
  vs2_tmp_8  := vs2_tmp.asTypeOf(vs2_tmp_8)
  vs2_tmp_16 := vs2_tmp.asTypeOf(vs2_tmp_16)
  vs2_tmp_32 := vs2_tmp.asTypeOf(vs2_tmp_32)

  val wsllResult_8_tmp  = Wire(Vec(4, UInt(16.W)))
  val wsllResult_16_tmp = Wire(Vec(2, UInt(32.W)))
  val wsllResult_32_tmp = Wire(Vec(1, UInt(64.W)))
  for (i <- 0 until 4) {
    wsllResult_8_tmp(i) := VecInit(shiftOneElement(vs1_tmp_8(i)(3, 0), VecInit(Cat(Fill(8, 0.U), vs2_tmp_8(i)).asBools.reverse).asUInt, 16)._1.asBools.reverse).asUInt
  }
  for (i <- 0 until 2) {
    wsllResult_16_tmp(i) := VecInit(shiftOneElement(vs1_tmp_16(i)(4, 0), VecInit(Cat(Fill(16, 0.U), vs2_tmp_16(i)).asBools.reverse).asUInt, 32)._1.asBools.reverse).asUInt
  }
  for (i <- 0 until 1) {
    wsllResult_32_tmp(i) := VecInit(shiftOneElement(vs1_tmp_32(i)(5, 0), VecInit(Cat(Fill(32, 0.U), vs2_tmp_32(i)).asBools.reverse).asUInt, 64)._1.asBools.reverse).asUInt
  }
  wsllResult := Mux1H(
    Seq(
      opcode.isVwsll && eewVd.is16,
      opcode.isVwsll && eewVd.is32,
      opcode.isVwsll && eewVd.is64,
    ),
    Seq(
      wsllResult_8_tmp.asUInt,
      wsllResult_16_tmp.asUInt,
      wsllResult_32_tmp.asUInt,
    )
  )

  // Output arbiter
  io.vd := Mux1H(
    Seq(
      opcode.isShift,
      opcode.isVext,
      opcode.isBitLogical,
      opcode.isVmergeMove,
      opcode.isVrev,
      opcode.isVCount,
      opcode.isVro,
      opcode.isVwsll,
    ),
    Seq(
      shiftResult,
      extResult,
      bitLogical,
      mergeMove,
      revResult,
      countResult,
      vroResult,
      wsllResult,
    )
  )
}
