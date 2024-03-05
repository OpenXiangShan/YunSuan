package yunsuan.vector.perm

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import scala.language.{existentials, postfixOps}
import yunsuan.vector._
import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}

class slideupVs2VdTable() extends Module {
  // convert uop index of slide instruction to offset of vs2 and vd
  val src = IO(Input(UInt(8.W)))
  val outOffsetVs2 = IO(Output(UInt(3.W)))
  val outOffsetVd = IO(Output(UInt(3.W)))
  def compute_vs2_vd(lmul:Int, uopIdx:Int): (Int, Int) = {
    for (i <- 0 until lmul) {
      var prev = i * (i + 1) / 2
      for (j <- 0 until i + 1) {
        if (uopIdx == prev + j) {
          return (i - j, i)
        }
      }
    }
    return (0, 0)
  }
  var combLmulUopIdx : Seq[(Int, Int, Int, Int)] = Seq()
  for (lmul <- 0 until 4) {
    for (uopIdx <- 0 until 36) {
      var offset = compute_vs2_vd(1 << lmul, uopIdx)
      var offsetVs2 = offset._1
      var offsetVd = offset._2
      combLmulUopIdx :+= (lmul, uopIdx, offsetVs2, offsetVd)
    }
  }
  val out = decoder(QMCMinimizer, src, TruthTable(combLmulUopIdx.map {
    case (lmul, uopIdx, offsetVs2, offsetVd) =>
      (BitPat((lmul << 6 | uopIdx).U(8.W)), BitPat((offsetVs2 << 3 | offsetVd).U(6.W)))
  }, BitPat.N(6)))
  outOffsetVs2 := out(5, 3)
  outOffsetVd := out(2, 0)
}

class slidednVs2VdTable() extends Module {
  // convert uop index of slide instruction to offset of vs2 and vd
  val src = IO(Input(UInt(8.W)))
  val outOffsetVs2 = IO(Output(UInt(3.W)))
  val outOffsetVd = IO(Output(UInt(3.W)))
  val outIsFirst = IO(Output(Bool()))
  def compute_vs2_vd(lmul:Int, uopIdx:Int): (Int, Int, Int) = {
    var uopNum = lmul * (lmul + 1) / 2
    for (i <- 0 until lmul) {
      var prev = lmul * i - i * (i - 1) / 2
      for (j <- 0 until lmul - i) {
        if (uopIdx == prev + lmul - i - j - 1) {
          return (j, i, if (j == lmul - i - 1) 1 else 0)
        }
      }
    }
    return (0, 0, 0)
  }
  var combLmulUopIdx : Seq[(Int, Int, Int, Int, Int)] = Seq()
  for (lmul <- 0 until 4) {
    for (uopIdx <- 0 until 36) {
      var offset = compute_vs2_vd(1 << lmul, uopIdx)
      var offsetVs2 = offset._1
      var offsetVd = offset._2
      var isFirst = offset._3
      combLmulUopIdx :+= (lmul, uopIdx, offsetVs2, offsetVd, isFirst)
    }
  }
  val out = decoder(QMCMinimizer, src, TruthTable(combLmulUopIdx.map {
    case (lmul, uopIdx, offsetVs2, offsetVd, isFirst) =>
      (BitPat((lmul << 6 | uopIdx).U(8.W)), BitPat((isFirst << 6 | offsetVs2 << 3 | offsetVd).U(7.W)))
  }, BitPat.N(7)))
  outIsFirst := out(6).asBool
  outOffsetVs2 := out(5, 3)
  outOffsetVd := out(2, 0)
}

class Permutation extends Module {
  val VLEN = 128
  val xLen = 64
  val LaneWidth = 64
  val NLanes = VLEN / 64
  val vlenb = VLEN / 8
  val vlenbWidth = log2Ceil(vlenb)
  val io = IO(new Bundle {
    val in = Flipped(ValidIO(new VPermInput))
    val out = Output(new VIFuOutput)
  })

  val vs1 = io.in.bits.vs1
  val vs2 = io.in.bits.vs2
  val old_vd = io.in.bits.old_vd
  val vmask = io.in.bits.mask
  val opcode = io.in.bits.opcode
  val srcTypeVs2 = io.in.bits.srcType(0)
  val srcTypeVs1 = io.in.bits.srcType(1)
  val vdType = io.in.bits.vdType
  val vm = io.in.bits.info.vm
  val ma = io.in.bits.info.ma
  val ta = io.in.bits.info.ta
  val vlmul = io.in.bits.info.vlmul
  val lmul = Mux(vlmul > 4.U, 0.U, vlmul)
  val vstart = io.in.bits.info.vstart
  val vl = io.in.bits.info.vl
  val uopIdx = io.in.bits.info.uopIdx
  val fire = io.in.valid

  val vsew = srcTypeVs2(1, 0)
  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val ele_cnt = vlenb.U >> vsew

  val vlRemain = Wire(UInt(8.W))
  val vlRemainBytes = vlRemain << vsew
  val eewVs1 = SewOH(srcTypeVs1(1, 0))
  val eewVs2 = SewOH(srcTypeVs2(1, 0))
  val eewVd = SewOH(vdType(1, 0))
  val vd_mask = (~0.U(VLEN.W))

  val vcompress = opcode.isVcompress
  val vslideup = opcode.isVslideup
  val vslidedn = opcode.isVslidedown
  val vslide1up = opcode.isVslide1up
  val vslide1dn = opcode.isVslide1down
  val vrgather = opcode.isVrgather
  val vrgather_vx = opcode.isVrgather_vx
  val vmvnr = opcode.isVmvnr
  val vrgather16_sew8 = opcode.isVrgather && (srcTypeVs1 === 1.U) && (srcTypeVs2 === 0.U)
  val vslide = vslideup || vslide1up || vslidedn || vslide1dn

  val vlRemain_vcompress = Wire(UInt(8.W))
  val elements = Wire(UInt(5.W))
  val base_ele = Wire(UInt(5.W))
  val mask_start_idx = Wire(UInt(7.W))
  val mask_selected = Wire(UInt(16.W))
  val ones_sum_base = Wire(UInt(8.W))
  val ones_sum = WireInit(VecInit(Seq.fill(vlenb + 1)(0.U(8.W))))
  val compressed_vs2_en = WireInit(VecInit(Seq.fill(vlenb)(0.U(16.W))))
  val compressed_vs2 = WireInit(VecInit(Seq.fill(vlenb)(0.U(VLEN.W))))
  val compressed_vs2_masked = WireInit(VecInit(Seq.fill(vlenb)(0.U(VLEN.W))))
  val compressed_res = Wire(UInt(VLEN.W))
  val compressed_res_8 = WireInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val compressed_res_16 = WireInit(VecInit(Seq.fill(8)(0.U(16.W))))
  val compressed_res_32 = WireInit(VecInit(Seq.fill(4)(0.U(32.W))))
  val compressed_res_64 = WireInit(VecInit(Seq.fill(2)(0.U(64.W))))
  val select_compressed_vs2 = Wire(UInt(vlenb.W))
  val compressed_vs2_merged = Wire(UInt(VLEN.W))

  val vlRemain1H = VecInit(Seq(
    uopIdx === 0.U  || uopIdx === 1.U,
    uopIdx === 2.U  || uopIdx === 3.U  || uopIdx === 4.U,
    uopIdx === 5.U  || uopIdx === 6.U  || uopIdx === 7.U  || uopIdx === 8.U,
    uopIdx === 9.U  || uopIdx === 10.U || uopIdx === 11.U || uopIdx === 12.U || uopIdx === 13.U,
    uopIdx === 14.U || uopIdx === 15.U || uopIdx === 16.U || uopIdx === 17.U || uopIdx === 18.U || uopIdx === 19.U,
    uopIdx === 20.U || uopIdx === 21.U || uopIdx === 22.U || uopIdx === 23.U || uopIdx === 24.U || uopIdx === 25.U || uopIdx === 26.U,
    uopIdx === 27.U || uopIdx === 28.U || uopIdx === 29.U || uopIdx === 30.U || uopIdx === 31.U || uopIdx === 32.U || uopIdx === 33.U || uopIdx === 34.U,
    uopIdx === 35.U || uopIdx === 36.U || uopIdx === 37.U || uopIdx === 38.U || uopIdx === 39.U || uopIdx === 40.U || uopIdx === 41.U || uopIdx === 42.U
  )).asUInt
  val vlRemainOut = Seq(
    Mux(vl > ele_cnt.asUInt, ele_cnt.asUInt, vl),
    Mux(vl > (ele_cnt.asUInt << 1.U), ele_cnt.asUInt, Mux(vl >  ele_cnt.asUInt        , vl -  ele_cnt.asUInt        , 0.U)),
    Mux(vl > (ele_cnt.asUInt *  3.U), ele_cnt.asUInt, Mux(vl > (ele_cnt.asUInt << 1.U), vl - (ele_cnt.asUInt << 1.U), 0.U)),
    Mux(vl > (ele_cnt.asUInt << 2.U), ele_cnt.asUInt, Mux(vl > (ele_cnt.asUInt  * 3.U), vl - (ele_cnt.asUInt *  3.U), 0.U)),
    Mux(vl > (ele_cnt.asUInt *  5.U), ele_cnt.asUInt, Mux(vl > (ele_cnt.asUInt << 2.U), vl - (ele_cnt.asUInt << 2.U), 0.U)),
    Mux(vl > (ele_cnt.asUInt *  6.U), ele_cnt.asUInt, Mux(vl > (ele_cnt.asUInt *  5.U), vl - (ele_cnt.asUInt *  5.U), 0.U)),
    Mux(vl > (ele_cnt.asUInt *  7.U), ele_cnt.asUInt, Mux(vl > (ele_cnt.asUInt *  6.U), vl - (ele_cnt.asUInt *  6.U), 0.U)),
    Mux(vl > (ele_cnt.asUInt *  7.U), vl - (ele_cnt.asUInt * 7.U), 0.U)
  )
  vlRemain_vcompress := Mux1H(vlRemain1H, vlRemainOut)

  when(vlmul === 5.U) {
    elements := Mux(vl < Cat(ele_cnt >> 3.U), vl, ele_cnt >> 3.U)
  }.elsewhen(vlmul === 6.U) {
    elements := Mux(vl < Cat(ele_cnt >> 2.U), vl, ele_cnt >> 2.U)
  }.elsewhen(vlmul === 7.U) {
    elements := Mux(vl < Cat(ele_cnt >> 1.U), vl, ele_cnt >> 1.U)
  }.elsewhen(vlmul === 1.U) {
    elements := Mux(vl < Cat(ele_cnt << 1.U), vlRemain_vcompress, ele_cnt)
  }.elsewhen(vlmul === 2.U) {
    elements := Mux(vl < Cat(ele_cnt << 2.U), vlRemain_vcompress, ele_cnt)
  }.elsewhen(vlmul === 3.U) {
    elements := Mux(vl < Cat(ele_cnt << 3.U), vlRemain_vcompress, ele_cnt)
  }.otherwise {
    elements := Mux(vl < Cat(ele_cnt), vl, ele_cnt)
  }

  when(vlmul === 5.U) {
    base_ele := ele_cnt >> 3.U
  }.elsewhen(vlmul === 6.U) {
    base_ele := ele_cnt >> 2.U
  }.elsewhen(vlmul === 7.U) {
    base_ele := ele_cnt >> 1.U
  }.otherwise {
    base_ele := ele_cnt
  }

  val eNum = Mux1H(UIntToOH(vsew), Seq(16, 8, 4, 2).map(num => num.U))
  val maskUopIdx1H = Seq(
    uopIdx >= 0.U  && uopIdx <= 1.U,
    uopIdx >= 2.U  && uopIdx <= 4.U,
    uopIdx >= 5.U  && uopIdx <= 8.U,
    uopIdx >= 9.U  && uopIdx <= 13.U,
    uopIdx >= 14.U && uopIdx <= 19.U,
    uopIdx >= 20.U && uopIdx <= 26.U,
    uopIdx >= 27.U && uopIdx <= 34.U,
    uopIdx >= 35.U && uopIdx <= 42.U
  )
  val startIdx1H = Mux1H(maskUopIdx1H, Seq.tabulate(8){num => num.U})
  mask_start_idx := eNum * startIdx1H
  val maskPart = vmask >> mask_start_idx
  mask_selected := Mux1H(UIntToOH(vsew), Seq(16, 8, 4, 2).map(num => maskPart(num - 1 ,0)))

  val baseUopIdx1H = Seq(
    uopIdx === 0.U  || uopIdx === 2.U  || uopIdx === 5.U  || uopIdx === 9.U  || uopIdx === 14.U || uopIdx === 20.U || uopIdx === 27.U || uopIdx === 35.U,
    uopIdx === 3.U  || uopIdx === 6.U  || uopIdx === 10.U || uopIdx === 15.U || uopIdx === 21.U || uopIdx === 28.U || uopIdx === 36.U,
    uopIdx === 7.U  || uopIdx === 11.U || uopIdx === 16.U || uopIdx === 22.U || uopIdx === 29.U || uopIdx === 37.U,
    uopIdx === 12.U || uopIdx === 17.U || uopIdx === 23.U || uopIdx === 30.U || uopIdx === 38.U,
    uopIdx === 18.U || uopIdx === 24.U || uopIdx === 31.U || uopIdx === 39.U,
    uopIdx === 25.U || uopIdx === 32.U || uopIdx === 40.U,
    uopIdx === 33.U || uopIdx === 41.U,
    uopIdx === 42.U
  )
  ones_sum_base := Mux1H(baseUopIdx1H, Seq.tabulate(8){num => num.U * base_ele})
  ones_sum(0) := vmask(7, 0)

  for (i <- 1 to vlenb) {
    when(i.U <= elements) {
      ones_sum(i) := ones_sum(i - 1) + vs1(mask_start_idx + i.U - 1.U)
    }
  }

  dontTouch(vlRemain1H)
  dontTouch(vlRemain_vcompress)
  dontTouch(elements)
  dontTouch(base_ele)
  dontTouch(eNum)
  dontTouch(mask_start_idx)
  dontTouch(mask_selected)
  dontTouch(Cat(ones_sum.reverse))

  when(vsew === 0.U) {
    for (i <- 0 until 16) {
      when(i.U < elements) {
        compressed_vs2_en(i) := (vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) << (ones_sum(i) - ones_sum_base)(3, 0)
        compressed_vs2(i) := vs2(8 * i + 7, 8 * i) << ((ones_sum(i) - ones_sum_base)(3, 0) << 3.U)
        compressed_vs2_masked(i) := Fill(VLEN, vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) & compressed_vs2(i)
      }
    }
  }.elsewhen(vsew === 1.U) {
    for (i <- 0 until 8) {
      when(i.U < elements) {
        compressed_vs2_en(i) := (vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) << (ones_sum(i) - ones_sum_base)(3, 0)
        compressed_vs2(i) := vs2(16 * i + 15, 16 * i) << ((ones_sum(i) - ones_sum_base)(3, 0) << 4.U)
        compressed_vs2_masked(i) := Fill(VLEN, vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) & compressed_vs2(i)
      }
    }
  }.elsewhen(vsew === 2.U) {
    for (i <- 0 until 4) {
      when(i.U < elements) {
        compressed_vs2_en(i) := (vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) << (ones_sum(i) - ones_sum_base)(3, 0)
        compressed_vs2(i) := vs2(32 * i + 31, 32 * i) << ((ones_sum(i) - ones_sum_base)(3, 0) << 5.U)
        compressed_vs2_masked(i) := Fill(VLEN, vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) & compressed_vs2(i)
      }
    }
  }.otherwise {
    for (i <- 0 until 2) {
      when(i.U < elements) {
        compressed_vs2_en(i) := (vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) << (ones_sum(i) - ones_sum_base)(3, 0)
        compressed_vs2(i) := vs2(64 * i + 63, 64 * i) << ((ones_sum(i) - ones_sum_base)(3, 0) << 6.U)
        compressed_vs2_masked(i) := Fill(VLEN, vs1(mask_start_idx + i.U) & (0.U <= (ones_sum(i) - ones_sum_base)) & ((ones_sum(i) - ones_sum_base) < ele_cnt)) & compressed_vs2(i)
      }
    }
  }

  select_compressed_vs2 := compressed_vs2_en.reduce(_ | _)
  compressed_vs2_merged := compressed_vs2_masked.reduce(_ | _)

  when(vsew === 0.U) {
    for (i <- 0 until 16) {
      compressed_res_8(i):= Mux(select_compressed_vs2(i), compressed_vs2_merged(8 * i + 7, 8 * i), old_vd(8 * i + 7, 8 * i))
    }
  }.elsewhen(vsew === 1.U) {
    for (i <- 0 until 8) {
      compressed_res_16(i) := Mux(select_compressed_vs2(i), compressed_vs2_merged(16 * i + 15, 16 * i), old_vd(16 * i + 15, 16 * i))
    }
  }.elsewhen(vsew === 2.U) {
    for (i <- 0 until 4) {
      compressed_res_32(i) := Mux(select_compressed_vs2(i), compressed_vs2_merged(32 * i + 31, 32 * i), old_vd(32 * i + 31, 32 * i))
    }
  }.otherwise {
    for (i <- 0 until 2) {
      compressed_res_64(i) := Mux(select_compressed_vs2(i), compressed_vs2_merged(64 * i + 63, 64 * i), old_vd(64 * i + 63, 64 * i))
    }
  }

  when(vsew === 0.U) {
    compressed_res := Cat(compressed_res_8.reverse)
  }.elsewhen(vsew === 1.U) {
    compressed_res := Cat(compressed_res_16.reverse)
  }.elsewhen(vsew === 2.U) {
    compressed_res := Cat(compressed_res_32.reverse)
  }.otherwise {
    compressed_res := Cat(compressed_res_64.reverse)
  }

  dontTouch(select_compressed_vs2)
  dontTouch(Cat(compressed_vs2_en.reverse))
  dontTouch(Cat(compressed_vs2_masked.reverse))
  dontTouch(Cat(compressed_vs2.reverse))
  dontTouch(compressed_vs2_merged)
  dontTouch(compressed_res)

  val base = Wire(UInt(7.W))
  val vmask0 = vmask
  val vmask_uop = Wire(UInt(VLEN.W))
  val vmask_byte_strb = Wire(Vec(vlenb, UInt(1.W)))
  val vs1_bytes = VecInit(Seq.tabulate(vlenb)(i => vs1((i + 1) * 8 - 1, i * 8)))
  val vs2_bytes = VecInit(Seq.tabulate(vlenb)(i => vs2((i + 1) * 8 - 1, i * 8)))
  val emul = vlmul(1, 0)
  val evl = Mux1H(Seq.tabulate(4)(i => (emul === i.U) -> (ele_cnt << i.U)))

  val vslideupOffset = Module(new slideupVs2VdTable)
  vslideupOffset.src := Cat(lmul, uopIdx)
  val vslideupVs2Id = vslideupOffset.outOffsetVs2
  val vslideupVd2Id = vslideupOffset.outOffsetVd

  val vslidednOffset = Module(new slidednVs2VdTable)
  vslidednOffset.src := Cat(lmul, uopIdx)
  val vslidednVs2Id = vslidednOffset.outOffsetVs2
  val vslidednVd2Id = vslidednOffset.outOffsetVd

  val vrgatherVdId = Mux1H(Seq(
    (vlmul === 0.U) -> 0.U,
    (vlmul === 1.U) -> uopIdx(1),
    (vlmul === 2.U) -> uopIdx(3, 2),
    (vlmul === 3.U) -> uopIdx(5, 3),
  ))

  val vrgatherVs2Id = Mux1H(Seq(
    (vlmul === 0.U) -> 0.U,
    (vlmul === 1.U) -> uopIdx(0),
    (vlmul === 2.U) -> uopIdx(1, 0),
    (vlmul === 3.U) -> uopIdx(2, 0),
  ))

  val vrgather16_sew8VdId = Mux1H(Seq(
    (vlmul === 0.U) -> 0.U,
    (vlmul === 1.U) -> uopIdx(2),
    (vlmul === 2.U) -> uopIdx(4, 3),
  ))

  val vrgather16_sew8Vs2Id = Mux1H(Seq(
    (vlmul === 0.U) -> 0.U,
    (vlmul === 1.U) -> uopIdx(1),
    (vlmul === 2.U) -> uopIdx(2, 1),
  ))

  val vdId = Mux1H(Seq(
    ((vrgather && !vrgather16_sew8) || vrgather_vx) -> vrgatherVdId,
    vrgather16_sew8 -> vrgather16_sew8VdId,
    (vslideup) -> vslideupVd2Id,
    (vslidedn) -> vslidednVd2Id,
  ))
  val vs2Id = Mux1H(Seq(
    ((vrgather && !vrgather16_sew8) || vrgather_vx) -> vrgatherVs2Id,
    vrgather16_sew8 -> vrgather16_sew8Vs2Id,
    (vslideup) -> vslideupVs2Id,
    (vslidedn) -> vslidednVs2Id,
  ))

  dontTouch(vdId)
  dontTouch(vs2Id)

  val vslideup_vl = Wire(UInt(8.W))
  vlRemain := vslideup_vl
  when(vslide1up) {
    vlRemain := Mux(vl >= (uopIdx << vsew_plus1), vl - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vlRemain := Mux(vl >= (uopIdx(5, 1) << vsew_plus1), vl - (uopIdx(5, 1) << vsew_plus1), 0.U)
  }.otherwise {
    vlRemain := Mux1H(Seq.tabulate(8)(i => (vdId === i.U) -> (if (i == 0) vslideup_vl else Mux(vslideup_vl >= (ele_cnt * i.U), vslideup_vl - (ele_cnt * i.U), 0.U))))
  }

  vmask_uop := vmask0
  when(vslide1up) {
    vmask_uop := vmask >> (uopIdx << vsew_plus1)
  }.elsewhen(vslide1dn) {
    vmask_uop := vmask >> (uopIdx(5, 1) << vsew_plus1)
  }.otherwise {
    vmask_uop := Mux1H(Seq.tabulate(8)(i => (vdId === i.U) -> (vmask >> (ele_cnt * i.U))))
  }

  base := Mux1H(Seq.tabulate(8)(i => (vs2Id === i.U) -> (vlenb * i).U))

  for (i <- 0 until vlenb) {
    when(i.U < vlRemainBytes) {
      vmask_byte_strb(i) := vmask_uop(i) | vm
      when(vsew === 1.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 2) | vm
      }.elsewhen(vsew === 2.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 4) | vm
      }.elsewhen(vsew === 3.U(3.W)) {
        vmask_byte_strb(i) := vmask_uop(i / 8) | vm
      }
    }.otherwise {
      vmask_byte_strb(i) := 0.U
    }
  }

  // vrgather/vrgather16
  val vlmax_bytes = Wire(UInt(5.W))
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(64.W)))
  val first_gather = (vlmul >= 4.U) || vs2Id === 0.U
  val vs2_bytes_min = Mux1H(Seq.tabulate(8)(i => (vs2Id === i.U) -> (vlenb * i).U))
  val vs2_bytes_max = Mux1H(Seq(
    (vs2Id === 0.U) -> vlmax_bytes,
  ) ++ (1 until 8).map(i => (vs2Id === i.U) -> (vlenb * (i + 1)).U))
  
  dontTouch(vs2_bytes_min)
  dontTouch(vs2_bytes_max)
  val vrgather_vd = Wire(Vec(vlenb, UInt(8.W)))

  vlmax_bytes := vlenb.U
  when(vlmul === 5.U) {
    vlmax_bytes := (vlenb / 8).U
  }.elsewhen(vlmul === 6.U) {
    vlmax_bytes := (vlenb / 4).U
  }.elsewhen(vlmul === 7.U) {
    vlmax_bytes := (vlenb / 2).U
  }

  for (i <- 0 until vlenb) {
    vrgather_byte_sel(i) := 0.U
    vrgather_vd(i) := 0.U
  }

  for (i <- 0 until vlenb / 2) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vx) {
      vrgather_byte_sel(i) := vs1(63, 0)
      when(srcTypeVs2(1, 0) === 1.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && !uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1) * 16 - 1, i * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          when(vdId(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          when(vdId(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vdId(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vdId(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
          }
        }
      }.elsewhen(srcTypeVs1(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs1(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  for (i <- (vlenb / 2) until vlenb) {
    vrgather_byte_sel(i) := 0.U
    when(vrgather_vx) {
      vrgather_byte_sel(i) := vs1(63, 0)
      when(srcTypeVs2(1, 0) === 1.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(1.W)) + i.U % 2.U
      }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1(63, 0), 0.U(3.W)) + i.U % 8.U
      }
    }.otherwise {
      when(srcTypeVs1(1, 0) === 0.U) {
        vrgather_byte_sel(i) := vs1((i + 1) * 8 - 1, i * 8)
      }.elsewhen(srcTypeVs1(1, 0) === 1.U) {
        when((srcTypeVs2(1, 0) === 0.U) && uopIdx(0)) {
          vrgather_byte_sel(i) := vs1((i + 1 - vlenb / 2) * 16 - 1, (i - vlenb / 2) * 16)
        }.elsewhen(srcTypeVs2(1, 0) === 1.U) {
          vrgather_byte_sel(i) := Cat(vs1((i / 2 + 1) * 16 - 1, i / 2 * 16), 0.U(1.W)) + i.U % 2.U
        }.elsewhen(srcTypeVs2(1, 0) === 2.U) {
          when(vdId(0).asBool) {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1 + 4) * 16 - 1, (i / 4 + 4) * 16), 0.U(2.W)) + i.U % 4.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 16 - 1, i / 4 * 16), 0.U(2.W)) + i.U % 4.U
          }
        }.elsewhen(srcTypeVs2(1, 0) === 3.U) {
          when(vdId(1, 0) === 0.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 16 - 1, (i / 8) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vdId(1, 0) === 1.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 2) * 16 - 1, (i / 8 + 2) * 16), 0.U(3.W)) + i.U % 8.U
          }.elsewhen(vdId(1, 0) === 2.U) {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 4) * 16 - 1, (i / 8 + 4) * 16), 0.U(3.W)) + i.U % 8.U
          }.otherwise {
            vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1 + 6) * 16 - 1, (i / 8 + 6) * 16), 0.U(3.W)) + i.U % 8.U
          }
        }
      }.elsewhen(srcTypeVs1(1, 0) === 2.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 4 + 1) * 32 - 1, i / 4 * 32), 0.U(2.W)) + i.U % 4.U
      }.elsewhen(srcTypeVs1(1, 0) === 3.U) {
        vrgather_byte_sel(i) := Cat(vs1((i / 8 + 1) * 64 - 1, i / 8 * 64), 0.U(3.W)) + i.U % 8.U
      }
    }
  }

  when((vrgather || vrgather_vx) && fire) {
    for (i <- 0 until vlenb) {
      vrgather_vd(i) := Mux(ma, "hff".U, old_vd((i + 1) * 8 - 1, i * 8))
      when(vmask_byte_strb(i).asBool) {
        when((vrgather_byte_sel(i) >= vs2_bytes_min) && (vrgather_byte_sel(i) < vs2_bytes_max)) {
          vrgather_vd(i) := vs2_bytes((vrgather_byte_sel(i) - vs2_bytes_min)(vlenbWidth - 1, 0))
        }.elsewhen(first_gather) {
          vrgather_vd(i) := 0.U
        }.otherwise {
          vrgather_vd(i) := old_vd((i + 1) * 8 - 1, i * 8)
        }
      }
    }
  }

  // vslideup/vslide1up
  val slide_ele = Mux(vslide1up || vslide1dn, 1.U, vs1(xLen, 0))
  val slide_bytes = slide_ele << vsew
  val vslideup_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1up_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslidedn_vd = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_wo_rs1 = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_rs1 = Wire(UInt(VLEN.W))
  val first_slidedn = vslidedn && vslidednOffset.outIsFirst
  val load_rs1 = (((vlmul >= 4.U) || (vlmul === 0.U)) && (uopIdx === 0.U)) ||
    ((vlmul === 1.U) && (uopIdx === 2.U)) ||
    ((vlmul === 2.U) && (uopIdx === 6.U)) ||
    (uopIdx === 14.U)
  val vslide1dn_vd = Mux((load_rs1 || uopIdx(0)), VecInit(Seq.tabulate(vlenb)(i => vslide1dn_vd_rs1((i + 1) * 8 - 1, i * 8))), vslide1dn_vd_wo_rs1)
  dontTouch(base)

  for (i <- 0 until vlenb) {
    vslideup_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vmask_byte_strb(i).asBool) {
      when(((base +& i.U) >= slide_bytes) && ((base +& i.U - slide_bytes) < vlmax_bytes)) {
        vslideup_vd(i) := vs2_bytes((base +& i.U - slide_bytes)(vlenbWidth - 1, 0))
      }.otherwise {
        vslideup_vd(i) := old_vd(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslidedn_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vmask_byte_strb(i).asBool) {
      when(((i.U +& slide_bytes) >= base) && ((i.U +& slide_bytes - base) < vlmax_bytes)) {
        vslidedn_vd(i) := vs2_bytes((i.U +& slide_bytes - base)(vlenbWidth - 1, 0))
      }.elsewhen(first_slidedn) {
        vslidedn_vd(i) := 0.U
      }.otherwise {
        vslidedn_vd(i) := old_vd(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1up_vd(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vslide1up && (vmask_byte_strb(i) === 1.U)) {
      when((i.U < vsew_bytes)) {
        vslide1up_vd(i) := vs1_bytes((vlenb.U - vsew_bytes + i.U)(vlenbWidth - 1, 0))
      }.otherwise {
        vslide1up_vd(i) := vs2_bytes(i.U - vsew_bytes)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1dn_vd_wo_rs1(i) := Mux(ma, "hff".U, old_vd(i * 8 + 7, i * 8))
    when(vslide1dn && !uopIdx(0) && (vmask_byte_strb(i) === 1.U)) {
      when(i.U < (vlenb.U - vsew_bytes)) {
        vslide1dn_vd_wo_rs1(i) := vs2_bytes(vsew_bytes + i.U)
      }.otherwise {
        vslide1dn_vd_wo_rs1(i) := vs1_bytes(i.U + vsew_bytes - vlenb.U)
      }
    }
  }

  val rs1_old_vd = Mux(load_rs1, Cat(vslide1dn_vd_wo_rs1.reverse), old_vd)
  vslide1dn_vd_rs1 := Mux(load_rs1, Cat(vslide1dn_vd_wo_rs1.reverse), old_vd)
  when(load_rs1 || uopIdx(0)) {
    when((vlRemainBytes > 0.U) && (vlRemainBytes <= vlenb.U) && (vmask_byte_strb(vlRemainBytes - 1.U).asBool)) {
      vslide1dn_vd_rs1 := (rs1_old_vd & (vd_mask >> (VLEN.U - Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))) |
        (vs1 & (vd_mask << Cat((vlRemainBytes - vsew_bytes), 0.U(3.W))))
    }
  }

  val vstartRemain = Wire(UInt(8.W))
  val vstartRemainBytes = vstartRemain << vsew

  val vslideup_vstart = Mux(vslideup & (slide_ele > vstart), Mux(slide_ele > VLEN.U, VLEN.U, slide_ele), vstart)
  vstartRemain := vslideup_vstart
  when(vslide1up) {
    vstartRemain := Mux(vstart >= (uopIdx << vsew_plus1), vstart - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vstartRemain := Mux(vstart >= (uopIdx(5, 1) << vsew_plus1), vstart - (uopIdx(5, 1) << vsew_plus1), 0.U)
  }.otherwise {
    vstartRemain := Mux1H(Seq.tabulate(8)(i => (vdId === i.U) -> (if (i == 0) vslideup_vstart else Mux(vslideup_vstart >= (ele_cnt * i.U), vslideup_vstart - (ele_cnt * i.U), 0.U))))
  }

  val vd_reg = RegInit(0.U(VLEN.W))

  when(vmvnr && fire) {
    vd_reg := vs2
  }.elsewhen(vslideup && fire) {
    vd_reg := Cat(vslideup_vd.reverse)
  }.elsewhen(vslide1up && fire) {
    vd_reg := Cat(vslide1up_vd.reverse)
  }.elsewhen(vslidedn && fire) {
    vd_reg := Cat(vslidedn_vd.reverse)
  }.elsewhen(vslide1dn && fire) {
    vd_reg := Cat(vslide1dn_vd.reverse)
  }.elsewhen((vrgather || vrgather_vx) && !(vrgather16_sew8) && fire) {
    vd_reg := Cat(vrgather_vd.reverse)
  }.elsewhen(vrgather16_sew8 && fire) {
    when(uopIdx(0)) {
      vd_reg := Cat(Cat(vrgather_vd.reverse)(VLEN - 1, VLEN / 2), old_vd(VLEN / 2 - 1, 0))
    }.otherwise {
      vd_reg := Cat(old_vd(VLEN - 1, VLEN / 2), Cat(vrgather_vd.reverse)(VLEN / 2 - 1, 0))
    }
  }

  val is_vcompress_reg = RegEnable(vcompress, false.B, fire)
  val is_vslideup_reg = RegEnable(vslideup, false.B, fire)
  val is_vslidedn_reg = RegEnable(vslidedn, false.B, fire)
  val is_vslide1up_reg = RegEnable(vslide1up, false.B, fire)
  val is_vslide1dn_reg = RegEnable(vslide1dn, false.B, fire)
  val is_vrgather_reg = RegEnable(vrgather, false.B, fire)
  val is_vrgather_vx_reg = RegEnable(vrgather_vx, false.B, fire)
  val is_vmvnr_reg = RegEnable(vmvnr, false.B, fire)
  val is_vslide_reg = RegEnable(vslide, false.B, fire)
  val uopIdx_reg = RegEnable(uopIdx, 0.U, fire)
  val load_rs1_reg = RegEnable(load_rs1, false.B, fire)
  val vlRemain_reg = RegEnable(vlRemain, 0.U, fire)
  val vsew_reg = RegEnable(vsew, 0.U, fire)
  val old_vd_reg = RegEnable(old_vd, 0.U, fire)
  val ta_reg = RegEnable(ta, false.B, fire)
  val vstartRemain_reg = RegEnable(vstartRemain, 0.U, fire)
  val vstart_reg = RegEnable(vstart, 0.U, fire)
  val vl_reg = RegEnable(Mux(vmvnr, evl, vl), 0.U, fire)
  val ones_sum_base_reg = RegEnable(ones_sum_base, 0.U, fire)
  val mask_selected_reg = RegEnable(mask_selected, 0.U, fire)
  val compressed_res_reg = RegEnable(compressed_res, 0.U, fire)
  val ones_sum_reg = RegEnable(ones_sum(elements), 0.U, fire)

  val vlRemainBytes_reg = vlRemain_reg << vsew_reg
  val vstartRemainBytes_reg = vstartRemain_reg << vsew_reg

  vslideup_vl := Mux(vslideup & (slide_ele > vl), Mux(slide_ele > VLEN.U, VLEN.U, slide_ele), vl)
  val tail_bytes = Mux((vlRemainBytes_reg >= vlenb.U), 0.U, vlenb.U - vlRemainBytes_reg)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := vd_mask >> tail_bits
  val tail_old_vd = old_vd_reg & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg, tail_ones_vd, tail_old_vd)
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))

  val vstart_bytes = Mux(vstartRemainBytes_reg >= vlenb.U, vlenb.U, vstartRemainBytes_reg)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd_reg & (~vmask_vstart_bits)

  val cmprs_vd = Wire(UInt(VLEN.W))
  val cmprs_vd_8  = WireInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val cmprs_vd_16 = WireInit(VecInit(Seq.fill(8)(0.U(16.W))))
  val cmprs_vd_32 = WireInit(VecInit(Seq.fill(4)(0.U(32.W))))
  val cmprs_vd_64 = WireInit(VecInit(Seq.fill(2)(0.U(64.W))))
  val res_agnostic_8  = WireInit(VecInit(Seq.fill(16)(false.B)))
  val res_agnostic_16 = WireInit(VecInit(Seq.fill(8)(false.B)))
  val res_agnostic_32 = WireInit(VecInit(Seq.fill(4)(false.B)))
  val res_agnostic_64 = WireInit(VecInit(Seq.fill(2)(false.B)))

  when(vsew_reg === 0.U) {
    for (i <- 0 until 16) {
      res_agnostic_8(i) := ((ones_sum_base_reg + i.U >= ones_sum_reg) & ta_reg)
      when(res_agnostic_8(i)) {
        cmprs_vd_8(i) := Fill(8, 1.U)
      }.otherwise {
        cmprs_vd_8(i) := compressed_res_reg(8 * i + 7, 8 * i)
      }
    }
  }.elsewhen(vsew_reg === 1.U) {
    for (i <- 0 until 8) {
      res_agnostic_16(i) := ((ones_sum_base_reg + i.U >= ones_sum_reg) & ta_reg)
      when(res_agnostic_16(i)) {
        cmprs_vd_16(i) := Fill(16, 1.U)
      }.otherwise {
        cmprs_vd_16(i) := compressed_res_reg(16 * i + 15, 16 * i)
      }
    }
  }.elsewhen(vsew_reg === 2.U) {
    for (i <- 0 until 4) {
      res_agnostic_32(i) := ((ones_sum_base_reg + i.U >= ones_sum_reg) & ta_reg)
      when(res_agnostic_32(i)) {
        cmprs_vd_32(i) := Fill(32, 1.U)
      }.otherwise {
        cmprs_vd_32(i) := compressed_res_reg(32 * i + 31, 32 * i)
      }
    }
  }.otherwise {
    for (i <- 0 until 2) {
      res_agnostic_64(i) := ((ones_sum_base_reg + i.U >= ones_sum_reg) & ta_reg)
      when(res_agnostic_64(i)) {
        cmprs_vd_64(i) := Fill(64, 1.U)
      }.otherwise {
        cmprs_vd_64(i) := compressed_res_reg(64 * i + 63, 64 * i)
      }
    }
  }

  dontTouch(Cat(cmprs_vd_8.reverse))
  dontTouch(Cat(cmprs_vd_16.reverse))
  dontTouch(Cat(cmprs_vd_32.reverse))
  dontTouch(Cat(cmprs_vd_64.reverse))

  when(vsew_reg === 0.U) {
    cmprs_vd := Cat(cmprs_vd_8.reverse)
  }.elsewhen(vsew_reg === 1.U) {
    cmprs_vd := Cat(cmprs_vd_16.reverse)
  }.elsewhen(vsew_reg === 2.U) {
    cmprs_vd := Cat(cmprs_vd_32.reverse)
  }.otherwise {
    cmprs_vd := Cat(cmprs_vd_64.reverse)
  }

  perm_tail_mask_vd := vd_reg
  when(is_vslide_reg || is_vrgather_reg || is_vrgather_vx_reg) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits & vmask_vstart_bits) | tail_vd | vstart_old_vd
  }

  val perm_vd = Wire(UInt(VLEN.W))

  perm_vd := perm_tail_mask_vd
  when(vstart_reg >= vl_reg) {
    perm_vd := old_vd_reg
  }.elsewhen(is_vcompress_reg) {
    when(uopIdx_reg === 1.U || uopIdx_reg === 4.U || uopIdx_reg === 8.U || uopIdx_reg === 13.U || uopIdx_reg === 19.U ||
      uopIdx_reg === 26.U || uopIdx_reg === 34.U) {
      perm_vd := ones_sum_reg
    }.otherwise {
      perm_vd := cmprs_vd
    }
  }

  io.out.vd := perm_vd
  io.out.vxsat := false.B
}

object VerilogPer extends App {
  println("Generating the VPU CrossLane hardware")
  emitVerilog(new Permutation(), Array("--target-dir", "build/vifu"))
}


