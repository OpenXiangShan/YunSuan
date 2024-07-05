package yunsuan.vector.perm

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.TruthTable
import scala.language.{existentials, postfixOps}
import yunsuan.vector._
import chisel3.util.experimental.decode.{QMCMinimizer, TruthTable, decoder}
import yunsuan.util.GatedValidRegNext

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
  val fire_reg0 = GatedValidRegNext(fire)

  val vsew = srcTypeVs2(1, 0)
  val vsew_plus1 = Wire(UInt(3.W))
  vsew_plus1 := Cat(0.U(1.W), ~vsew) + 1.U
  val widen = vdType(1, 0) === (srcTypeVs2(1, 0) + 1.U)
  val vsew_bytes = 1.U << vsew
  val vsew_bits = 8.U << vsew
  val ele_cnt = vlenb.U >> vsew

  val vlRemain = Wire(UInt(8.W))
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

  /**
   * vcompress
   *
   * cycle0
   * vlRemain_vcompress (mux8)
   * elements (mux7)
   * mask_start_idx(mux8, mul)
   * ones_sum_base(mux8, mul)
   * ones_sum(mask_start_idx, acc sum)
   *
   * cycle1
   * compressed_vs2_en(mask_start_idx)
   * compressed_vs2
   * compressed_vs2_masked(compressed_vs2)
   * select_compressed_vs2(compressed_vs2_en, or)
   * compressed_vs2_merged(compressed_vs2_masked, or)
   * compressed_res_8
   * compressed_res
   *
   * cycle2
   * res_agnostic_8
   * cmprd_vd_8
   * cmprd_vd(mux4)
   *
   */

  /**
   * vcompress cycle0
   * -----begin-----
    */
  val vlRemain_vcompress = Wire(UInt(8.W))
  val elements = Wire(UInt(5.W))
  val base_ele = Wire(UInt(5.W))
  val mask_start_idx = Wire(UInt(7.W))
  val ones_sum_base = Wire(UInt(8.W))

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

  val eNum = Mux1H(UIntToOH(vsew), Seq(4, 3, 2, 1).map(num => num.U))
  mask_start_idx := 1.U << eNum

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

  val in_previous_ones_sum = Wire(UInt(8.W))
  val out_previous_ones_sum = Wire(UInt(8.W))
  val current_uop_ones_sum = WireInit(VecInit(Seq.fill(vlenb)(0.U(5.W))))
  val current_ones_sum = WireInit(VecInit(Seq.fill(vlenb)(0.U(8.W))))

  val vs1_mask = Wire(Vec(vlenb, UInt(1.W)))
  for (i <- 0 until vlenb) {
    vs1_mask(i) := vs1(i)
  }

  in_previous_ones_sum := Mux(uopIdx === 0.U || uopIdx === 1.U, 0.U, vs1(127, 120))
  out_previous_ones_sum := current_uop_ones_sum(elements - 1.U)

  for (i <- 0 until vlenb) {
    when(i.U < elements) {
      current_uop_ones_sum(i) := PopCount(Cat(vs1_mask.reverse)(i, 0))
    }
    when(i.U < elements - 1.U) {
      current_ones_sum(i) := in_previous_ones_sum + current_uop_ones_sum(i)
    }
  }

  /**
   * vcompress cycle0
   * ----end-----
   */


  /**
   * vcompress cycle1
   * -----begin-----
   */
  val vsew_reg0 = RegEnable(vsew, 0.U, fire)
  val elements_reg0 = RegEnable(elements, 0.U, fire)
  val vs1_reg0 = RegEnable(vs1, 0.U, fire)
  val vs1_next_reg0 = RegEnable(vs1 >> mask_start_idx, 0.U, fire)
  val vs2_reg0 = RegEnable(vs2, 0.U, fire)
  val old_vd_reg0 = RegEnable(old_vd, 0.U, fire)
  val current_ones_sum_reg0 = RegEnable(current_ones_sum, fire)
  val in_previous_ones_sum_reg0 = RegEnable(in_previous_ones_sum, fire)
  val out_previous_ones_sum_reg0 = RegEnable(out_previous_ones_sum, fire)
  val ones_sum_base_reg0 = RegEnable(ones_sum_base, fire)
  val ele_cnt_reg0 = RegEnable(ele_cnt, fire)

  val ones_sum_reg0 = WireInit(VecInit(Seq.fill(vlenb)(0.U(8.W))))
  ones_sum_reg0(0) := in_previous_ones_sum_reg0
  for (i <- 1 until vlenb) {
    when(i.U < elements_reg0) {
      ones_sum_reg0(i) := current_ones_sum_reg0(i-1)
    }
  }

  val ones_sum_eles_reg0 = Wire(UInt(8.W))
  ones_sum_eles_reg0 := in_previous_ones_sum_reg0 + out_previous_ones_sum_reg0

  val compressed_vs2_en_reg0 = WireInit(VecInit(Seq.fill(vlenb)(0.U(16.W))))
  val compressed_vs2_reg0 = WireInit(VecInit(Seq.fill(vlenb)(0.U(VLEN.W))))
  val compressed_vs2_masked_reg0 = WireInit(VecInit(Seq.fill(vlenb)(0.U(VLEN.W))))
  val compressed_res_reg0 = Wire(UInt(VLEN.W))
  val compressed_res_8_reg0 = WireInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val compressed_res_16_reg0 = WireInit(VecInit(Seq.fill(8)(0.U(16.W))))
  val compressed_res_32_reg0 = WireInit(VecInit(Seq.fill(4)(0.U(32.W))))
  val compressed_res_64_reg0 = WireInit(VecInit(Seq.fill(2)(0.U(64.W))))
  val select_compressed_vs2_reg0 = Wire(UInt(vlenb.W))
  val compressed_vs2_merged_reg0 = Wire(UInt(VLEN.W))

  when(vsew_reg0 === 0.U) {
    for (i <- 0 until 16) {
      when(i.U < elements_reg0) {
        compressed_vs2_en_reg0(i) := (vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) << (ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0)
        compressed_vs2_reg0(i) := vs2_reg0(8 * i + 7, 8 * i) << ((ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0) << 3.U)
        compressed_vs2_masked_reg0(i) := Fill(VLEN, vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) & compressed_vs2_reg0(i)
      }
    }
  }.elsewhen(vsew_reg0 === 1.U) {
    for (i <- 0 until 8) {
      when(i.U < elements_reg0) {
        compressed_vs2_en_reg0(i) := (vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) << (ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0)
        compressed_vs2_reg0(i) := vs2_reg0(16 * i + 15, 16 * i) << ((ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0) << 4.U)
        compressed_vs2_masked_reg0(i) := Fill(VLEN, vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) & compressed_vs2_reg0(i)
      }
    }
  }.elsewhen(vsew_reg0 === 2.U) {
    for (i <- 0 until 4) {
      when(i.U < elements_reg0) {
        compressed_vs2_en_reg0(i) := (vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) << (ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0)
        compressed_vs2_reg0(i) := vs2_reg0(32 * i + 31, 32 * i) << ((ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0) << 5.U)
        compressed_vs2_masked_reg0(i) := Fill(VLEN, vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) & compressed_vs2_reg0(i)
      }
    }
  }.otherwise {
    for (i <- 0 until 2) {
      when(i.U < elements_reg0) {
        compressed_vs2_en_reg0(i) := (vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) << (ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0)
        compressed_vs2_reg0(i) := vs2_reg0(64 * i + 63, 64 * i) << ((ones_sum_reg0(i) - ones_sum_base_reg0)(3, 0) << 6.U)
        compressed_vs2_masked_reg0(i) := Fill(VLEN, vs1_reg0(i) & (0.U <= (ones_sum_reg0(i) - ones_sum_base_reg0)) & ((ones_sum_reg0(i) - ones_sum_base_reg0) < ele_cnt_reg0)) & compressed_vs2_reg0(i)
      }
    }
  }

  select_compressed_vs2_reg0 := compressed_vs2_en_reg0.reduce(_ | _)
  compressed_vs2_merged_reg0 := compressed_vs2_masked_reg0.reduce(_ | _)

  when(vsew_reg0 === 0.U) {
    for (i <- 0 until 16) {
      compressed_res_8_reg0(i):= Mux(select_compressed_vs2_reg0(i), compressed_vs2_merged_reg0(8 * i + 7, 8 * i), old_vd_reg0(8 * i + 7, 8 * i))
    }
  }.elsewhen(vsew_reg0 === 1.U) {
    for (i <- 0 until 8) {
      compressed_res_16_reg0(i) := Mux(select_compressed_vs2_reg0(i), compressed_vs2_merged_reg0(16 * i + 15, 16 * i), old_vd_reg0(16 * i + 15, 16 * i))
    }
  }.elsewhen(vsew_reg0 === 2.U) {
    for (i <- 0 until 4) {
      compressed_res_32_reg0(i) := Mux(select_compressed_vs2_reg0(i), compressed_vs2_merged_reg0(32 * i + 31, 32 * i), old_vd_reg0(32 * i + 31, 32 * i))
    }
  }.otherwise {
    for (i <- 0 until 2) {
      compressed_res_64_reg0(i) := Mux(select_compressed_vs2_reg0(i), compressed_vs2_merged_reg0(64 * i + 63, 64 * i), old_vd_reg0(64 * i + 63, 64 * i))
    }
  }

  when(vsew_reg0 === 0.U) {
    compressed_res_reg0 := Cat(compressed_res_8_reg0.reverse)
  }.elsewhen(vsew_reg0 === 1.U) {
    compressed_res_reg0 := Cat(compressed_res_16_reg0.reverse)
  }.elsewhen(vsew_reg0 === 2.U) {
    compressed_res_reg0 := Cat(compressed_res_32_reg0.reverse)
  }.otherwise {
    compressed_res_reg0 := Cat(compressed_res_64_reg0.reverse)
  }

//  dontTouch(select_compressed_vs2)
//  dontTouch(Cat(compressed_vs2_en.reverse))
//  dontTouch(Cat(compressed_vs2_masked.reverse))
//  dontTouch(Cat(compressed_vs2.reverse))
//  dontTouch(compressed_vs2_merged)
//  dontTouch(compressed_res)
  /**
   * vcompress cycle1
   * -----end------
   */

  /**
   * vslideup,vslidedn
   *
   * cycle0
   * vlRemain
   * base
   * vmask_byte_strb
   * vrgather_byte_sel
   *
   * clcye1
   * vrgather_vd
   * vslide1dn_vd
   * vslideup_vd
   * vslide1up_vd
   * vstartRemain
   *
   * cycle2
   * vd_reg
   *
   */

  val base = Wire(UInt(7.W))
  val vmask0 = vmask
  val vmask_uop = Wire(UInt(VLEN.W))
  val vmask_byte_strb_reg0 = Wire(Vec(vlenb, UInt(1.W)))
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

  // dontTouch(vdId)
  // dontTouch(vs2Id)

  vlRemain := vl
  when(vslide1up) {
    vlRemain := Mux(vl >= (uopIdx << vsew_plus1), vl - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vlRemain := Mux(vl >= (uopIdx(5, 1) << vsew_plus1), vl - (uopIdx(5, 1) << vsew_plus1), 0.U)
  }.otherwise {
    vlRemain := Mux1H(Seq.tabulate(8)(i => (vdId === i.U) -> (if (i == 0) vl else Mux(vl >= (ele_cnt * i.U), vl - (ele_cnt * i.U), 0.U))))
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
  // dontTouch(base)

  val vlRemain_reg0 = RegEnable(vlRemain, 0.U, fire)
  val vmask_uop_reg0 = RegEnable(vmask_uop, fire)
  val vm_reg0 = RegEnable(vm, fire)

  val vlRemainBytes_reg0 = vlRemain_reg0 << vsew_reg0

  for (i <- 0 until vlenb) {
    when(i.U < vlRemainBytes_reg0) {
      vmask_byte_strb_reg0(i) := vmask_uop_reg0(i) | vm_reg0
      when(vsew_reg0 === 1.U(3.W)) {
        vmask_byte_strb_reg0(i) := vmask_uop_reg0(i / 2) | vm_reg0
      }.elsewhen(vsew_reg0 === 2.U(3.W)) {
        vmask_byte_strb_reg0(i) := vmask_uop_reg0(i / 4) | vm_reg0
      }.elsewhen(vsew_reg0 === 3.U(3.W)) {
        vmask_byte_strb_reg0(i) := vmask_uop_reg0(i / 8) | vm_reg0
      }
    }.otherwise {
      vmask_byte_strb_reg0(i) := 0.U
    }
  }

  // vrgather/vrgather16
  val vlmax_bytes = Wire(UInt(5.W))
  // we may append at most 3bits to vrgather_byte_sel, so the width of vrgather_byte_sel is 64+3 = 67
  val vrgather_byte_sel = Wire(Vec(vlenb, UInt(67.W)))
  val first_gather = (vlmul >= 4.U) || vs2Id === 0.U
  val vs2_bytes_min = Mux1H(Seq.tabulate(8)(i => (vs2Id === i.U) -> (vlenb * i).U))
  val vs2_bytes_max = Mux1H(Seq(
    (vs2Id === 0.U) -> vlmax_bytes,
  ) ++ (1 until 8).map(i => (vs2Id === i.U) -> (vlenb * (i + 1)).U))
  
  // dontTouch(vs2_bytes_min)
  // dontTouch(vs2_bytes_max)
  val vrgather_vd_reg0 = Wire(Vec(vlenb, UInt(8.W)))

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
    vrgather_vd_reg0(i) := 0.U
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

  val base_reg0 = RegEnable(base, fire)
  val vrgather_byte_sel_reg0 = RegEnable(vrgather_byte_sel, fire)
  val is_vrgather_reg0 = RegEnable(vrgather, false.B, fire)
  val is_vrgather_vx_reg0 = RegEnable(vrgather_vx, false.B, fire)
  val ma_reg0 = RegEnable(ma, fire)
  val vs2_bytes_min_reg0 = RegEnable(vs2_bytes_min, fire)
  val vs2_bytes_max_reg0 = RegEnable(vs2_bytes_max, fire)
  val vs2_bytes_reg0 = RegEnable(vs2_bytes, fire)
  val first_gather_reg0 = RegEnable(first_gather, fire)

  when((is_vrgather_reg0 || is_vrgather_vx_reg0) && fire_reg0) {
    for (i <- 0 until vlenb) {
      vrgather_vd_reg0(i) := Mux(ma_reg0, "hff".U, old_vd_reg0((i + 1) * 8 - 1, i * 8))
      when(vmask_byte_strb_reg0(i).asBool) {
        when((vrgather_byte_sel_reg0(i) >= vs2_bytes_min_reg0) && (vrgather_byte_sel_reg0(i) < vs2_bytes_max_reg0)) {
          vrgather_vd_reg0(i) := vs2_bytes_reg0((vrgather_byte_sel_reg0(i) - vs2_bytes_min_reg0)(vlenbWidth - 1, 0))
        }.elsewhen(first_gather_reg0) {
          vrgather_vd_reg0(i) := 0.U
        }.otherwise {
          vrgather_vd_reg0(i) := old_vd_reg0((i + 1) * 8 - 1, i * 8)
        }
      }
    }
  }

  // vslideup/vslide1up
  val slide_ele = Mux(vslide1up || vslide1dn, 1.U, vs1(xLen - 1, 0))
  val slide_bytes = slide_ele << vsew
  val vslideup_vd_reg0 = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1up_vd_reg0 = Wire(Vec(vlenb, UInt(8.W)))
  val vslidedn_vd_reg0 = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_wo_rs1_reg0 = Wire(Vec(vlenb, UInt(8.W)))
  val vslide1dn_vd_rs1_reg0 = Wire(UInt(VLEN.W))
  val first_slidedn = vslidedn && vslidednOffset.outIsFirst
  val load_rs1 = (((vlmul >= 4.U) || (vlmul === 0.U)) && (uopIdx === 0.U)) ||
    ((vlmul === 1.U) && (uopIdx === 2.U)) ||
    ((vlmul === 2.U) && (uopIdx === 6.U)) ||
    (uopIdx === 14.U)

  val uopIdx_reg0 = RegEnable(uopIdx, 0.U, fire)
  val load_rs1_reg0 = RegEnable(load_rs1, fire)
  val slide_bytes_reg0 = RegEnable(slide_bytes, fire)
  val vlmax_bytes_reg0 = RegEnable(vlmax_bytes, fire)
  val first_slidedn_reg0 = RegEnable(first_slidedn, fire)
  val is_vslide1up_reg0 = RegEnable(vslide1up, false.B, fire)
  val is_vslide1dn_reg0 = RegEnable(vslide1dn, false.B, fire)
  val vsew_bytes_reg0 = RegEnable(vsew_bytes, fire)
  val vs1_bytes_reg0 = RegEnable(vs1_bytes, fire)

  for (i <- 0 until vlenb) {
    vslideup_vd_reg0(i) := Mux(ma_reg0, "hff".U, old_vd_reg0(i * 8 + 7, i * 8))
    when(vmask_byte_strb_reg0(i).asBool) {
      when(((base_reg0 +& i.U) >= slide_bytes_reg0) && ((base_reg0 +& i.U - slide_bytes_reg0) < vlmax_bytes_reg0)) {
        vslideup_vd_reg0(i) := vs2_bytes_reg0((base_reg0 +& i.U - slide_bytes_reg0)(vlenbWidth - 1, 0))
      }.otherwise {
        vslideup_vd_reg0(i) := old_vd_reg0(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslidedn_vd_reg0(i) := Mux(ma_reg0, "hff".U, old_vd_reg0(i * 8 + 7, i * 8))
    when(vmask_byte_strb_reg0(i).asBool) {
      when(((i.U +& slide_bytes_reg0) >= base_reg0) && ((i.U +& slide_bytes_reg0 - base_reg0) < vlmax_bytes_reg0)) {
        vslidedn_vd_reg0(i) := vs2_bytes_reg0((i.U +& slide_bytes_reg0 - base_reg0)(vlenbWidth - 1, 0))
      }.elsewhen(first_slidedn_reg0) {
        vslidedn_vd_reg0(i) := 0.U
      }.otherwise {
        vslidedn_vd_reg0(i) := old_vd_reg0(i * 8 + 7, i * 8)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1up_vd_reg0(i) := Mux(ma_reg0, "hff".U, old_vd_reg0(i * 8 + 7, i * 8))
    when(is_vslide1up_reg0 && (vmask_byte_strb_reg0(i) === 1.U)) {
      when((i.U < vsew_bytes_reg0)) {
        vslide1up_vd_reg0(i) := vs1_bytes_reg0((vlenb.U - vsew_bytes_reg0 + i.U)(vlenbWidth - 1, 0))
      }.otherwise {
        vslide1up_vd_reg0(i) := vs2_bytes_reg0(i.U - vsew_bytes_reg0)
      }
    }
  }

  for (i <- 0 until vlenb) {
    vslide1dn_vd_wo_rs1_reg0(i) := Mux(ma_reg0, "hff".U, old_vd_reg0(i * 8 + 7, i * 8))
    when(is_vslide1dn_reg0 && !uopIdx_reg0(0) && (vmask_byte_strb_reg0(i) === 1.U)) {
      when(i.U < (vlenb.U - vsew_bytes_reg0)) {
        vslide1dn_vd_wo_rs1_reg0(i) := vs2_bytes_reg0(vsew_bytes_reg0 + i.U)
      }.otherwise {
        vslide1dn_vd_wo_rs1_reg0(i) := vs1_bytes_reg0(i.U + vsew_bytes_reg0 - vlenb.U)
      }
    }
  }

  val rs1_old_vd_reg0 = Mux(load_rs1_reg0, Cat(vslide1dn_vd_wo_rs1_reg0.reverse), old_vd_reg0)
  vslide1dn_vd_rs1_reg0 := Mux(load_rs1_reg0, Cat(vslide1dn_vd_wo_rs1_reg0.reverse), old_vd_reg0)
  when(load_rs1_reg0 || uopIdx_reg0(0)) {
    when((vlRemainBytes_reg0 > 0.U) && (vlRemainBytes_reg0 <= vlenb.U) && (vmask_byte_strb_reg0(vlRemainBytes_reg0 - 1.U).asBool)) {
      vslide1dn_vd_rs1_reg0 := (rs1_old_vd_reg0 & (vd_mask >> (VLEN.U - Cat((vlRemainBytes_reg0 - vsew_bytes_reg0), 0.U(3.W))))) |
        (vs1_reg0 & (vd_mask << Cat((vlRemainBytes_reg0 - vsew_bytes_reg0), 0.U(3.W))))
    }
  }

  val vslide1dn_vd_reg0 = Mux(load_rs1_reg0 || uopIdx_reg0(0), VecInit(Seq.tabulate(vlenb)(i => vslide1dn_vd_rs1_reg0((i + 1) * 8 - 1, i * 8))), vslide1dn_vd_wo_rs1_reg0)

  val vstartRemain = Wire(UInt(8.W))

  val vslideup_vstart = Mux(vslideup & (slide_ele > vstart), Mux(slide_ele > VLEN.U, VLEN.U, slide_ele), vstart)
  vstartRemain := vslideup_vstart
  when(vslide1up) {
    vstartRemain := Mux(vstart >= (uopIdx << vsew_plus1), vstart - (uopIdx << vsew_plus1), 0.U)
  }.elsewhen(vslide1dn) {
    vstartRemain := Mux(vstart >= (uopIdx(5, 1) << vsew_plus1), vstart - (uopIdx(5, 1) << vsew_plus1), 0.U)
  }.otherwise {
    vstartRemain := Mux1H(Seq.tabulate(8)(i => (vdId === i.U) -> (if (i == 0) vslideup_vstart else Mux(vslideup_vstart >= (ele_cnt * i.U), vslideup_vstart - (ele_cnt * i.U), 0.U))))
  }

  val is_vcompress_reg0 = RegEnable(vcompress, false.B, fire)
  val is_vslideup_reg0 = RegEnable(vslideup, false.B, fire)
  val is_vslidedn_reg0 = RegEnable(vslidedn, false.B, fire)
  val is_vrgather16_sew8_reg0 = RegEnable(vrgather16_sew8, false.B, fire)
  val is_vmvnr_reg0 = RegEnable(vmvnr, false.B, fire)
  val is_vslide_reg0 = RegEnable(vslide, false.B, fire)
  val ta_reg0 = RegEnable(ta, false.B, fire)
  val vstartRemain_reg0 = RegEnable(vstartRemain, 0.U, fire)
  val vstart_reg0 = RegEnable(vstart, 0.U, fire)
  val vl_reg0 = RegEnable(Mux(vmvnr, evl, vl), 0.U, fire)

  val vd_reg = RegInit(0.U(VLEN.W))

  when(is_vmvnr_reg0 && fire_reg0) {
    vd_reg := vs2_reg0
  }.elsewhen(is_vslideup_reg0 && fire_reg0) {
    vd_reg := Cat(vslideup_vd_reg0.reverse)
  }.elsewhen(is_vslide1up_reg0 && fire_reg0) {
    vd_reg := Cat(vslide1up_vd_reg0.reverse)
  }.elsewhen(is_vslidedn_reg0 && fire_reg0) {
    vd_reg := Cat(vslidedn_vd_reg0.reverse)
  }.elsewhen(is_vslide1dn_reg0 && fire_reg0) {
    vd_reg := Cat(vslide1dn_vd_reg0.reverse)
  }.elsewhen((is_vrgather_reg0 || is_vrgather_vx_reg0) && !(is_vrgather16_sew8_reg0) && fire_reg0) {
    vd_reg := Cat(vrgather_vd_reg0.reverse)
  }.elsewhen(is_vrgather16_sew8_reg0 && fire_reg0) {
    when(uopIdx_reg0(0)) {
      vd_reg := Cat(Cat(vrgather_vd_reg0.reverse)(VLEN - 1, VLEN / 2), old_vd_reg0(VLEN / 2 - 1, 0))
    }.otherwise {
      vd_reg := Cat(old_vd_reg0(VLEN - 1, VLEN / 2), Cat(vrgather_vd_reg0.reverse)(VLEN / 2 - 1, 0))
    }
  }

  val is_vcompress_reg1 = RegEnable(is_vcompress_reg0, fire_reg0)
  val is_vrgather_reg1 = RegEnable(is_vrgather_reg0, fire_reg0)
  val is_vrgather_vx_reg1 = RegEnable(is_vrgather_vx_reg0, fire_reg0)
  val is_vslide_reg1 = RegEnable(is_vslide_reg0, fire_reg0)
  val uopIdx_reg1 = RegEnable(uopIdx_reg0, fire_reg0)
  val old_vd_reg1 = RegEnable(old_vd_reg0, fire_reg0)
  val ta_reg1 = RegEnable(ta_reg0, fire_reg0)
  val vstart_reg1 = RegEnable(vstart_reg0, fire_reg0)
  val vl_reg1 = RegEnable(vl_reg0, fire_reg0)
  val vsew_reg1 = RegEnable(vsew_reg0, fire_reg0)
  val ones_sum_eles_reg1 = RegEnable(ones_sum_eles_reg0, fire_reg0)
  val ones_sum_base_reg1 = RegEnable(ones_sum_base_reg0, fire_reg0)
  val compressed_res_reg1 = RegEnable(compressed_res_reg0, fire_reg0)
  val vs1_next_reg1 = RegEnable(vs1_next_reg0, fire_reg0)

  val vstartRemainBytes_reg0 = vstartRemain_reg0 << vsew_reg0

  val tail_bytes = Mux(vlRemainBytes_reg0 >= vlenb.U, 0.U, vlenb.U - vlRemainBytes_reg0)
  val tail_bits = Cat(tail_bytes, 0.U(3.W))
  val vmask_tail_bits = Wire(UInt(VLEN.W))
  vmask_tail_bits := vd_mask >> tail_bits
  val tail_old_vd = old_vd_reg0 & (~vmask_tail_bits)
  val tail_ones_vd = ~vmask_tail_bits
  val tail_vd = Mux(ta_reg0, tail_ones_vd, tail_old_vd)
  val perm_tail_mask_vd = Wire(UInt(VLEN.W))

  val vstart_bytes = Mux(vstartRemainBytes_reg0 >= vlenb.U, vlenb.U, vstartRemainBytes_reg0)
  val vstart_bits = Cat(vstart_bytes, 0.U(3.W))
  val vmask_vstart_bits = Wire(UInt(VLEN.W))
  vmask_vstart_bits := vd_mask << vstart_bits
  val vstart_old_vd = old_vd_reg0 & (~vmask_vstart_bits)

  val vmask_tail_bits_reg1 = RegEnable(vmask_tail_bits, fire_reg0)
  val vmask_vstart_bits_reg1 = RegEnable(vmask_vstart_bits, fire_reg0)
  val tail_vd_reg1 = RegEnable(tail_vd, fire_reg0)
  val vstart_old_vd_reg1 = RegEnable(vstart_old_vd, fire_reg0)

  /**
   * vcompress cycle2
   * ----begin-----
    */
  val cmprs_vd = Wire(UInt(VLEN.W))
  val cmprs_vd_8  = WireInit(VecInit(Seq.fill(16)(0.U(8.W))))
  val cmprs_vd_16 = WireInit(VecInit(Seq.fill(8)(0.U(16.W))))
  val cmprs_vd_32 = WireInit(VecInit(Seq.fill(4)(0.U(32.W))))
  val cmprs_vd_64 = WireInit(VecInit(Seq.fill(2)(0.U(64.W))))
  val res_agnostic_8  = WireInit(VecInit(Seq.fill(16)(false.B)))
  val res_agnostic_16 = WireInit(VecInit(Seq.fill(8)(false.B)))
  val res_agnostic_32 = WireInit(VecInit(Seq.fill(4)(false.B)))
  val res_agnostic_64 = WireInit(VecInit(Seq.fill(2)(false.B)))

  when(vsew_reg1 === 0.U) {
    for (i <- 0 until 16) {
      res_agnostic_8(i) := ((ones_sum_base_reg1 + i.U >= ones_sum_eles_reg1) & ta_reg1)
      when(res_agnostic_8(i)) {
        cmprs_vd_8(i) := Fill(8, 1.U)
      }.otherwise {
        cmprs_vd_8(i) := compressed_res_reg1(8 * i + 7, 8 * i)
      }
    }
  }.elsewhen(vsew_reg1 === 1.U) {
    for (i <- 0 until 8) {
      res_agnostic_16(i) := ((ones_sum_base_reg1 + i.U >= ones_sum_eles_reg1) & ta_reg1)
      when(res_agnostic_16(i)) {
        cmprs_vd_16(i) := Fill(16, 1.U)
      }.otherwise {
        cmprs_vd_16(i) := compressed_res_reg1(16 * i + 15, 16 * i)
      }
    }
  }.elsewhen(vsew_reg1 === 2.U) {
    for (i <- 0 until 4) {
      res_agnostic_32(i) := ((ones_sum_base_reg1 + i.U >= ones_sum_eles_reg1) & ta_reg1)
      when(res_agnostic_32(i)) {
        cmprs_vd_32(i) := Fill(32, 1.U)
      }.otherwise {
        cmprs_vd_32(i) := compressed_res_reg1(32 * i + 31, 32 * i)
      }
    }
  }.otherwise {
    for (i <- 0 until 2) {
      res_agnostic_64(i) := ((ones_sum_base_reg1 + i.U >= ones_sum_eles_reg1) & ta_reg1)
      when(res_agnostic_64(i)) {
        cmprs_vd_64(i) := Fill(64, 1.U)
      }.otherwise {
        cmprs_vd_64(i) := compressed_res_reg1(64 * i + 63, 64 * i)
      }
    }
  }

  // dontTouch(Cat(cmprs_vd_8.reverse))
  // dontTouch(Cat(cmprs_vd_16.reverse))
  // dontTouch(Cat(cmprs_vd_32.reverse))
  // dontTouch(Cat(cmprs_vd_64.reverse))

  when(vsew_reg1 === 0.U) {
    cmprs_vd := Cat(cmprs_vd_8.reverse)
  }.elsewhen(vsew_reg1 === 1.U) {
    cmprs_vd := Cat(cmprs_vd_16.reverse)
  }.elsewhen(vsew_reg1 === 2.U) {
    cmprs_vd := Cat(cmprs_vd_32.reverse)
  }.otherwise {
    cmprs_vd := Cat(cmprs_vd_64.reverse)
  }

  /**
   * vcompress cycle2
   * -----end----
   */

  perm_tail_mask_vd := vd_reg
  when(is_vslide_reg1 || is_vrgather_reg1 || is_vrgather_vx_reg1) {
    perm_tail_mask_vd := (vd_reg & vmask_tail_bits_reg1 & vmask_vstart_bits_reg1) | tail_vd_reg1 | vstart_old_vd_reg1
  }

  val perm_vd = Wire(UInt(VLEN.W))

  perm_vd := perm_tail_mask_vd
  when(vstart_reg1 >= vl_reg1) {
    perm_vd := old_vd_reg1
  }.elsewhen(is_vcompress_reg1) {
    when(uopIdx_reg1 === 1.U || uopIdx_reg1 === 4.U || uopIdx_reg1 === 8.U || uopIdx_reg1 === 13.U || uopIdx_reg1 === 19.U ||
      uopIdx_reg1 === 26.U || uopIdx_reg1 === 34.U) {
      perm_vd := Cat(ones_sum_eles_reg1, vs1_next_reg1(119, 0))  // reuse vs1
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


