package yunsuan.vector.VectorALU

import chisel3._
import chisel3.util._
import yunsuan.vector.Common.Vxrm
import yunsuan.vector.VectorALU.VIAlu.MaskGroup

class Adder extends VIAlu.Module {
  val in = IO(Input(new Adder.In))
  val out = IO(Output(new Adder.Out))

  private val sel8 = in.ctrl.sel8
  private val sel16 = in.ctrl.sel16
  private val sel32 = in.ctrl.sel32
  private val sel64 = in.ctrl.sel64
  private val widenVs2 = in.ctrl.widenVs2
  private val widen = in.ctrl.widen
  private val isSigned = in.ctrl.isSigned
  private val isAddCarry = in.ctrl.isAddCarry
  private val isSub = in.ctrl.isSub
  private val isCmpEq = in.ctrl.isCmpEq
  private val isCmpNe = in.ctrl.isCmpNe
  private val isCmpLt = in.ctrl.isCmpLt
  private val isCmpLe = in.ctrl.isCmpLe
  private val isCmpGt = in.ctrl.isCmpGt
  private val isVmsbc = in.ctrl.isVmsbc
  private val isAvg = in.ctrl.isAvg
  private val vm = in.ctrl.vm
  private val rm = in.vxrm

  private val vs2 = in.data.vs2
  private val vs1 = in.data.vs1
  private val vs2Widen = in.data.vs2Widen
  private val vs1Widen = in.data.vs1Widen
  private val mask = in.data.mask

  private val vs2Vec = Wire(Vec(8, UInt(8.W)))
  private val vs1Vec = Wire(Vec(8, UInt(8.W)))

  vs2Vec(0) := Mux(widenVs2, vs2Widen(7, 0), vs2(7, 0))
  vs2Vec(1) := Mux(widenVs2, Mux(sel8, Fill(8, isSigned & vs2Widen(7)), vs2Widen(15, 8)), vs2(15, 8))
  vs2Vec(2) := Mux(widenVs2, Mux1H(Seq(
    sel8  -> vs2Widen(15, 8),
    sel16 -> Fill(8, isSigned & vs2Widen(15)),
    sel32 -> vs2Widen(23, 16),
  )), vs2(23, 16))
  vs2Vec(3) := Mux(widenVs2, Mux(sel32, vs2Widen(31, 24), Fill(8, isSigned & vs2Widen(15))), vs2(31, 24))
  vs2Vec(4) := Mux(widenVs2, Mux(sel32, Fill(8, isSigned & vs2Widen(31)), vs2Widen(23, 16)), vs2(39, 32))
  vs2Vec(5) := Mux(widenVs2, Mux1H(Seq(
    sel8  -> Fill(8, isSigned & vs2Widen(23)),
    sel16 -> vs2Widen(31, 24),
    sel32 -> Fill(8, isSigned & vs2Widen(31)),
  )), vs2(47, 40))
  vs2Vec(6) := Mux(widenVs2, Mux(sel8, vs2Widen(31, 24), Fill(8, isSigned & vs2Widen(31))), vs2(55, 48))
  vs2Vec(7) := Mux(widenVs2, Fill(8, isSigned & vs2Widen(31)), vs2(63, 56))

  vs1Vec(0) := Mux(widen, vs1Widen(7, 0), vs1(7, 0))
  vs1Vec(1) := Mux(widen, Mux(sel8, Fill(8, isSigned & vs1Widen(7)), vs1Widen(15, 8)), vs1(15, 8))
  vs1Vec(2) := Mux(widen, Mux1H(Seq(
    sel8  -> vs1Widen(15, 8),
    sel16 -> Fill(8, isSigned & vs1Widen(15)),
    sel32 -> vs1Widen(23, 16),
  )), vs1(23, 16))
  vs1Vec(3) := Mux(widen, Mux(sel32, vs1Widen(31, 24), Fill(8, isSigned & vs1Widen(15))), vs1(31, 24))
  vs1Vec(4) := Mux(widen, Mux(sel32, Fill(8, isSigned & vs1Widen(31)), vs1Widen(23, 16)), vs1(39, 32))
  vs1Vec(5) := Mux(widen, Mux1H(Seq(
    sel8 -> Fill(8, isSigned & vs1Widen(23)),
    sel16 -> vs1Widen(31, 24),
    sel32 -> Fill(8, isSigned & vs1Widen(31)),
  )), vs1(47, 40))
  vs1Vec(6) := Mux(widen, Mux(sel8, vs1Widen(31, 24), Fill(8, isSigned & vs1Widen(31))), vs1(55, 48))
  vs1Vec(7) := Mux(widen, Fill(8, isSigned & vs1Widen(31)), vs1(63, 56))

  private val srcbInv = Wire(Vec(8, UInt(8.W)))
  for (i <- 0 until 8) {
    srcbInv(i) := Mux(isSub, ~vs1Vec(i), vs1Vec(i))
  }

  private val cIn = Wire(Vec(8, Bool()))
  cIn(0) := Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(0)), isSub)
  cIn(1) := sel8 & Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(1)), isSub & !widenVs2 & !widen)
  cIn(2) := Mux1H(Seq(
    sel8  -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(2)), isSub),
    sel16 -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(1)), isSub & !widenVs2 & !widen),
  ))
  cIn(3) := sel8 & Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(3)), isSub & !widenVs2 & !widen)
  cIn(4) := Mux1H(Seq(
    sel8  -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(4)), isSub),
    sel16 -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(2)), isSub),
    sel32 -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(1)), isSub & !widenVs2 & !widen),
  ))
  cIn(5) := sel8 & Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(5)), isSub & !widenVs2 & !widen)
  cIn(6) := Mux1H(Seq(
    sel8  -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(6)), isSub),
    sel16 -> Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(3)), isSub & !widenVs2 & !widen),
  ))
  cIn(7) := sel8 & Mux(isAddCarry, Mux(vm, isSub, isSub ^ mask(7)), isSub & !widenVs2 & !widen)

  private val cInPlus2 = Wire(Vec(8, Bool()))
  cInPlus2(0) := 1.U
  cInPlus2(1) := sel8
  cInPlus2(2) := sel8 | sel16
  cInPlus2(3) := sel8
  cInPlus2(4) := !sel64
  cInPlus2(5) := sel8
  cInPlus2(6) := sel8 | sel16
  cInPlus2(7) := sel8

  private val srca = Wire(Vec(8, UInt(8.W)))
  private val srcb = Wire(Vec(8, UInt(8.W)))
  private val srcaPre = Wire(Vec(8, Bool()))
  private val srcbPre = Wire(Vec(8, Bool()))
  private val srccPre = Wire(Vec(8, Bool()))
  private val addSrc1 = Wire(UInt(72.W))
  private val addSrc2 = Wire(UInt(72.W))
  private val addSrc3 = Wire(UInt(72.W))
  private val addPlus2Src1 = Wire(UInt(80.W))
  private val addPlus2Src2 = Wire(UInt(80.W))
  private val addPlus2Src3 = Wire(UInt(80.W))

  private val addResult = Wire(UInt(72.W))
  private val addPlus2Result = Wire(UInt(80.W))
  private val vs2VecAll1sVs1VecAll1s = Wire(Vec(7, Bool()))
  private val vs2VecAll1sVs1VecAll0s = Wire(Vec(7, Bool()))

  for (i <- 0 until 8) {
    srca(i) := vs2Vec(i)
    srcb(i) := srcbInv(i)
  }
  vs2VecAll1sVs1VecAll1s(0) := vs2Vec(0).andR & vs1Vec(0).andR
  vs2VecAll1sVs1VecAll1s(1) := vs2Vec(1).andR & vs1Vec(1).andR
  vs2VecAll1sVs1VecAll1s(2) := vs2Vec(2).andR & vs1Vec(2).andR
  vs2VecAll1sVs1VecAll1s(3) := vs2Vec(3).andR & vs1Vec(3).andR
  vs2VecAll1sVs1VecAll1s(4) := vs2Vec(4).andR & vs1Vec(4).andR
  vs2VecAll1sVs1VecAll1s(5) := vs2Vec(5).andR & vs1Vec(5).andR
  vs2VecAll1sVs1VecAll1s(6) := vs2Vec(6).andR & vs1Vec(6).andR

  vs2VecAll1sVs1VecAll0s(0) := false.B
  vs2VecAll1sVs1VecAll0s(1) := vs2Vec(1).andR & !vs1Vec(1).orR
  vs2VecAll1sVs1VecAll0s(2) := vs2Vec(2).andR & !vs1Vec(2).orR
  vs2VecAll1sVs1VecAll0s(3) := vs2Vec(3).andR & !vs1Vec(3).orR
  vs2VecAll1sVs1VecAll0s(4) := vs2Vec(4).andR & !vs1Vec(4).orR
  vs2VecAll1sVs1VecAll0s(5) := vs2Vec(5).andR & !vs1Vec(5).orR
  vs2VecAll1sVs1VecAll0s(6) := vs2Vec(6).andR & !vs1Vec(6).orR

  private val carry2 = Wire(Vec(4, Bool()))
  carry2(0) := vs2Vec(0).head(7).andR & !vs1Vec(0).orR & (vs2Vec(0)(0) | !vs1Vec(0)(0))
  carry2(1) := vs2Vec(2).head(7).andR & !vs1Vec(2).orR & (vs2Vec(2)(0) | !vs1Vec(2)(0))
  carry2(2) := vs2Vec(4).head(7).andR & !vs1Vec(4).orR & (vs2Vec(4)(0) | !vs1Vec(4)(0))
  carry2(3) := vs2Vec(6).head(7).andR & !vs1Vec(6).orR & (vs2Vec(6)(0) | !vs1Vec(6)(0))

  srcaPre(0) := sel8 & widenVs2 | ~sel8
  srcaPre(1) := sel16 & widenVs2 | sel32 | sel64
  srcaPre(2) := sel8 & widenVs2 | ~sel8
  srcaPre(3) := sel32 & widenVs2 | sel64
  srcaPre(4) := sel8 & widenVs2 | ~sel8
  srcaPre(5) := sel16 & widenVs2 | sel32 | sel64
  srcaPre(6) := sel8 & widenVs2 | ~sel8
  srcaPre(7) := 0.U

  private val onlyWiden = !widenVs2 & widen
  srcbPre(0) := onlyWiden & sel8
  srcbPre(1) := onlyWiden & sel16
  srcbPre(2) := onlyWiden & sel8
  srcbPre(3) := onlyWiden & sel32
  srcbPre(4) := onlyWiden & sel8
  srcbPre(5) := onlyWiden & sel16
  srcbPre(6) := onlyWiden & sel8
  srcbPre(7) := 0.U

  srccPre(0) := !sel8 & Mux(isSub, carry2(0), vs2VecAll1sVs1VecAll1s(0))
  srccPre(1) := (sel32 | sel64) & Mux(isSub,
    vs2VecAll1sVs1VecAll0s(1) & carry2(0),
    vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))
  srccPre(2) := Mux1H(Seq(
    sel16 -> Mux(isSub, carry2(1), vs2VecAll1sVs1VecAll1s(2)),
    (sel32 | sel64) -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(2) & vs2VecAll1sVs1VecAll0s(1) & carry2(0),
      vs2VecAll1sVs1VecAll1s(2) & vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))))
  srccPre(3) := sel64 & Mux(isSub,
    vs2VecAll1sVs1VecAll0s(3) & vs2VecAll1sVs1VecAll0s(2) & vs2VecAll1sVs1VecAll0s(1) & carry2(0),
    vs2VecAll1sVs1VecAll1s(3) & vs2VecAll1sVs1VecAll1s(2) & vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))
  srccPre(4) := Mux1H(Seq(
    (sel16 | sel32) -> Mux(isSub, carry2(2), vs2VecAll1sVs1VecAll1s(4)),
    sel64 -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(4) & vs2VecAll1sVs1VecAll0s(3) & vs2VecAll1sVs1VecAll0s(2) &
        vs2VecAll1sVs1VecAll0s(1) & carry2(0),
      vs2VecAll1sVs1VecAll1s(4) & vs2VecAll1sVs1VecAll1s(3) & vs2VecAll1sVs1VecAll1s(2) &
        vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))))
  srccPre(5) := Mux1H(Seq(
    sel32 -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(5) & carry2(2),
      vs2VecAll1sVs1VecAll1s(5) & vs2VecAll1sVs1VecAll1s(4)),
    sel64 -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(5) & vs2VecAll1sVs1VecAll0s(4) & vs2VecAll1sVs1VecAll0s(3) &
        vs2VecAll1sVs1VecAll0s(2) & vs2VecAll1sVs1VecAll0s(1) & carry2(0),
      vs2VecAll1sVs1VecAll1s(5) & vs2VecAll1sVs1VecAll1s(4) & vs2VecAll1sVs1VecAll1s(3) &
        vs2VecAll1sVs1VecAll1s(2) & vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))))
  srccPre(6) := Mux1H(Seq(
    sel16 -> Mux(isSub, carry2(3), vs2VecAll1sVs1VecAll1s(6)),
    sel32 -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(6) & vs2VecAll1sVs1VecAll0s(5) & carry2(2),
      vs2VecAll1sVs1VecAll1s(6) & vs2VecAll1sVs1VecAll1s(5) & vs2VecAll1sVs1VecAll1s(4)),
    sel64 -> Mux(isSub,
      vs2VecAll1sVs1VecAll0s(6) & vs2VecAll1sVs1VecAll0s(5) & vs2VecAll1sVs1VecAll0s(4) &
        vs2VecAll1sVs1VecAll0s(3) & vs2VecAll1sVs1VecAll0s(2) & vs2VecAll1sVs1VecAll0s(1) & carry2(0),
      vs2VecAll1sVs1VecAll1s(6) & vs2VecAll1sVs1VecAll1s(5) & vs2VecAll1sVs1VecAll1s(4) &
        vs2VecAll1sVs1VecAll1s(3) & vs2VecAll1sVs1VecAll1s(2) & vs2VecAll1sVs1VecAll1s(1) & vs2VecAll1sVs1VecAll1s(0))))
  srccPre(7) := 0.U

  addSrc1 := Cat(VecInit(srcaPre.zip(srca).map { case (pre, a) =>
    Cat(pre, a)
  }).reverse)

  addSrc2 := Cat(VecInit(srcbPre.zip(srcb).map { case (pre, b) =>
    Cat(pre, b)
  }).reverse)

  addSrc3 := Cat(VecInit(cIn.map(in =>
    Cat(0.U(8.W), in)
  )).reverse)

  addPlus2Src1 := Cat(VecInit(srcaPre.zip(srca).map { case (pre, a) =>
    Cat(pre, pre, a)
  }).reverse)

  addPlus2Src2 := Cat(VecInit(srcbPre.zip(srcb).map { case (pre, b) =>
    Cat(pre, pre, b)
  }).reverse)

  addPlus2Src3 := Cat(VecInit(srccPre.zip(cInPlus2).map { case (pre, in) =>
    Cat(pre, pre, 0.U(6.W), in, in & isSub)
  }).reverse)

  private val l0S = Wire(UInt(72.W))  // [71:0]
  private val l0C = Wire(UInt(72.W))  // [72:1]

  l0S := addSrc1 ^ addSrc2 ^ addSrc3
  l0C := addSrc1 & addSrc2 | addSrc2 & addSrc3 | addSrc1 & addSrc3

  addResult := l0S + Cat(l0C.tail(1), 0.U)

  private val originResult = Wire(Vec(8, UInt(8.W)))
  originResult(0) := addResult(7,  0)
  originResult(1) := addResult(16, 9)
  originResult(2) := addResult(25, 18)
  originResult(3) := addResult(34, 27)
  originResult(4) := addResult(43, 36)
  originResult(5) := addResult(52, 45)
  originResult(6) := addResult(61, 54)
  originResult(7) := addResult(70, 63)

  private val originAddResult = Wire(UInt(dWidth.W))
  originAddResult := originResult.asUInt

  private val l0SPlus2 = Wire(UInt(80.W))
  private val l0CPlus2 = Wire(UInt(80.W))

  l0SPlus2 := addPlus2Src1 ^ addPlus2Src2 ^ addPlus2Src3
  l0CPlus2 := addPlus2Src1 & addPlus2Src2 | addPlus2Src2 & addPlus2Src3 | addPlus2Src1 & addPlus2Src3

  addPlus2Result := l0SPlus2 + Cat(l0CPlus2.tail(1), 0.U)

  private val avgAddPlus2Vec = Wire(Vec(8, UInt(8.W)))
  private val avgCoutPlus2 = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    avgAddPlus2Vec(i) := addPlus2Result(i * 10 + 7, i * 10)
    avgCoutPlus2(i) := addPlus2Result(i * 10 + 8)
  }

  private val avgAddPlus2Result = Wire(UInt(dWidth.W))
  avgAddPlus2Result := avgAddPlus2Vec.asUInt

  private val cout = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    cout(i) := addResult(i * 9 + 8)
  }
  private val coutResult = Wire(UInt(8.W))
  coutResult := cout.asUInt

  // compare
  private val ltVec = Wire(Vec(8, Bool()))
  private val eqVec = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    ltVec(i) := Mux(isSigned, srca(i)(7) ^ srcb(i)(7) ^ cout(i), ~cout(i))
    eqVec(i) := vs2(i * 8 + 7, i * 8) === vs1(i * 8 + 7, i * 8)
  }

  private val eqResult = Wire(UInt(8.W))
  eqResult := Mux1H(Seq(
    sel8 -> eqVec.asUInt,
    sel16 -> Cat(Fill(2, eqVec(7) & eqVec(6)), Fill(2, eqVec(5) & eqVec(4)),
      Fill(2, eqVec(3) & eqVec(2)), Fill(2, eqVec(1) & eqVec(0))),
    sel32 -> Cat(Fill(4, eqVec(7) & eqVec(6) & eqVec(5) & eqVec(4)),
      Fill(4, eqVec(3) & eqVec(2) & eqVec(1) & eqVec(0))),
    sel64 -> Fill(8, eqVec(7) & eqVec(6) & eqVec(5) & eqVec(4) & eqVec(3) & eqVec(2) & eqVec(1) & eqVec(0))
  ))

  private val ltResult = Wire(UInt(8.W))
  ltResult := ltVec.asUInt

  private val cmpResult = Wire(UInt(8.W))
  cmpResult := Mux1H(Seq(
    isCmpEq -> eqResult,
    isCmpNe -> ~eqResult,
    isCmpLt -> ltResult,
    isCmpLe -> (eqResult | ltResult),
    isCmpGt -> ~(eqResult | ltResult),
  ))

  private val carryCoutCmp = Wire(UInt(8.W))
  carryCoutCmp := Mux(isAddCarry, Mux(isVmsbc, (~coutResult).asUInt, coutResult), cmpResult)

  private val addCarryCmpMask = Wire(new MaskGroup)

  addCarryCmpMask.e8 := carryCoutCmp
  addCarryCmpMask.e16 := Cat(carryCoutCmp(7), carryCoutCmp(5), carryCoutCmp(3), carryCoutCmp(1))
  addCarryCmpMask.e32 := Cat(carryCoutCmp(7), carryCoutCmp(3))
  addCarryCmpMask.e64 := carryCoutCmp(7)

  // max/min
  def selectPos(in: UInt, i: Int): Bool = {
    Mux1H(Seq(
      sel8  -> in(i),
      sel16 -> in((i / 2) * 2 + 1),
      sel32 -> in((i / 4) * 4 + 3),
      sel64 -> in(7),
    ))
  }

  // saturating add/sub
  private val vs2Head = Wire(Vec(8, Bool()))
  private val vs1Head = Wire(Vec(8, Bool()))
  private val vdHead  = Wire(Vec(8, Bool()))
  private val coutSat = Wire(Vec(8, Bool()))

  for (i <- 0 until 8) {
    vs2Head(i) := vs2(i * 8 + 7)
    vs1Head(i) := vs1(i * 8 + 7)
    vdHead(i) := originResult(i)(7)
    coutSat(i) := Mux(
      isSigned,
      Mux(
        isSub,
        !vs2Head(i) & vs1Head(i) & vdHead(i) | vs2Head(i) & !vs1Head(i) & !vdHead(i),
        vs2Head(i) & vs1Head(i) & !vdHead(i) | !vs2Head(i) & !vs1Head(i) & vdHead(i)
      ),
      cout(i) ^ isSub
    )
  }

  private val sat = Wire(MaskData())
  sat := Mux1H(Seq(
    sel8 -> coutSat.asUInt,
    sel16 -> FillInterleaved(2, Cat(coutSat(7), coutSat(5), coutSat(3), coutSat(1))),
    sel32 -> FillInterleaved(4, Cat(coutSat(7), coutSat(3))),
    sel64 -> FillInterleaved(8, coutSat(7)),
  ))
  private val upOverflowUnSign = Wire(Vec(8, Bool()))
  private val downOverflowSign = Wire(Vec(8, Bool()))

  for (i <- 0 until 8) {
    upOverflowUnSign(i) := selectPos(coutResult, i)
    downOverflowSign(i) := selectPos(vs2Head.asUInt, i)
  }

  // Averaging add/sub
  private val avgShiftOneSel8  = Wire(Vec(8, UInt(8.W)))
  private val avgShiftOneSel16 = Wire(Vec(4, UInt(16.W)))
  private val avgShiftOneSel32 = Wire(Vec(2, UInt(32.W)))
  private val avgShiftOneSel64 = Wire(UInt(64.W))
  private val avgShiftOnePlus1Sel8 = Wire(Vec(8, UInt(8.W)))
  private val avgShiftOnePlus1Sel16 = Wire(Vec(4, UInt(16.W)))
  private val avgShiftOnePlus1Sel32 = Wire(Vec(2, UInt(32.W)))
  private val avgShiftOnePlus1Sel64 = Wire(UInt(64.W))
  private val avgShiftOneRoundSel8  = Wire(Vec(8, Bool()))
  private val avgShiftOneRoundSel16 = Wire(Vec(4, Bool()))
  private val avgShiftOneRoundSel32 = Wire(Vec(2, Bool()))
  private val avgShiftOneRoundSel64 = Wire(Bool())

  for (i <- 0 until 8) {
    val signBaseSel8 = isSub ^ Mux(isSigned, vs2Head(i) ^ vs1Head(i), false.B)
    val signSel8 = signBaseSel8 ^ cout(i)
    val signPlus2Sel8 = signBaseSel8 ^ avgCoutPlus2(i)
    avgShiftOneSel8(i) := Cat(signSel8, originAddResult(i * 8 + 7, i * 8 + 1))
    avgShiftOnePlus1Sel8(i) := Cat(signPlus2Sel8, avgAddPlus2Result(i * 8 + 7, i * 8 + 1))
    avgShiftOneRoundSel8(i) := originResult(i)(0)
  }
  for (i <- 0 until 4) {
    val signBaseSel16 = isSub ^ Mux(isSigned, vs2Head(i * 2 + 1) ^ vs1Head(i * 2 + 1), false.B)
    val signSel16 = signBaseSel16 ^ cout(i * 2 + 1)
    val signPlus2Sel16 = signBaseSel16 ^ avgCoutPlus2(i * 2 + 1)
    avgShiftOneSel16(i) := Cat(signSel16, originAddResult(i * 16 + 15, i * 16 + 1))
    avgShiftOnePlus1Sel16(i) := Cat(signPlus2Sel16, avgAddPlus2Result(i * 16 + 15, i * 16 + 1))
    avgShiftOneRoundSel16(i) := originResult(i * 2)(0)
  }
  for (i <- 0 until 2) {
    val signBaseSel32 = isSub ^ Mux(isSigned, vs2Head(i * 4 + 3) ^ vs1Head(i * 4 + 3), false.B)
    val signSel32 = signBaseSel32 ^ cout(i * 4 + 3)
    val signPlus2Sel32 = signBaseSel32 ^ avgCoutPlus2(i * 4 + 3)
    avgShiftOneSel32(i) := Cat(signSel32, originAddResult(i * 32 + 31, i * 32 + 1))
    avgShiftOnePlus1Sel32(i) := Cat(signPlus2Sel32, avgAddPlus2Result(i * 32 + 31, i * 32 + 1))
    avgShiftOneRoundSel32(i) := originResult(i * 4)(0)
  }
  private val signBaseSel64 = isSub ^ Mux(isSigned, vs2Head(7) ^ vs1Head(7), false.B)
  private val signSel64 = signBaseSel64 ^ cout(7)
  private val signPlus2Sel64 = signBaseSel64 ^ avgCoutPlus2(7)
  avgShiftOneSel64 := Cat(signSel64, originAddResult.head(63))
  avgShiftOnePlus1Sel64 := Cat(signPlus2Sel64, avgAddPlus2Result.head(63))
  avgShiftOneRoundSel64 := originResult(0)(0)

  def roundMode(in: UInt, inPlus1: UInt, rIn: UInt): UInt = {
    val (g, r) = (in(0), rIn(0))
    val roundUp = Mux1H(Seq(
      rm.isRnu -> r,
      rm.isRne -> (r & g),
      rm.isRdn -> false.B,
      rm.isRod -> (!g & r),
    ))
    Mux(roundUp, inPlus1, in)
  }
  private val avgResultSel8  = Wire(Vec(8, UInt(8.W)))
  private val avgResultSel16 = Wire(Vec(4, UInt(16.W)))
  private val avgResultSel32 = Wire(Vec(2, UInt(32.W)))
  private val avgResultSel64 = Wire(UInt(dWidth.W))
  for (i <- 0 until 8) {
    avgResultSel8(i) := roundMode(avgShiftOneSel8(i), avgShiftOnePlus1Sel8(i), avgShiftOneRoundSel8(i))
  }
  for (i <- 0 until 4) {
    avgResultSel16(i) := roundMode(avgShiftOneSel16(i), avgShiftOnePlus1Sel16(i), avgShiftOneRoundSel16(i))
  }
  for (i <- 0 until 2) {
    avgResultSel32(i) := roundMode(avgShiftOneSel32(i), avgShiftOnePlus1Sel32(i), avgShiftOneRoundSel32(i))
  }
  avgResultSel64 := roundMode(avgShiftOneSel64, avgShiftOnePlus1Sel64, avgShiftOneRoundSel64)

  private val vs2Element8IsAll1s = Wire(Vec(8, Bool()))
  private val vs2Element8ExcludeHighBitIsAll1s = Wire(Vec(8, Bool()))
  private val vs1Element8IsAll0s = Wire(Vec(8, Bool()))
  private val vs1Element8ExcludeHighBitIsAll0s = Wire(Vec(8, Bool()))
  for (i <- 0 until 8) {
    vs2Element8IsAll1s(i) := vs2(i * 8 + 7, i * 8).andR
    vs2Element8ExcludeHighBitIsAll1s(i) := !vs2(i * 8 + 7) & vs2(i * 8 + 6, i * 8).andR
    vs1Element8IsAll0s(i) := !vs1(i * 8 + 7, i * 8).orR
    vs1Element8ExcludeHighBitIsAll0s(i) := vs1(i * 8 + 7) & !vs1(i * 8 + 6, i * 8).orR
  }

  private val maxSubMinSel8  = Wire(Vec(8, Bool()))
  private val maxSubMinSel16 = Wire(Vec(4, Bool()))
  private val maxSubMinSel32 = Wire(Vec(2, Bool()))
  private val maxSubMinSel64 = Wire(Bool())
  private val maxSubMin = Wire(Bool())
  for (i <- 0 until 8) {
    maxSubMinSel8(i) := vs2Element8ExcludeHighBitIsAll1s(i) & vs1Element8ExcludeHighBitIsAll0s(i)
  }
  for (i <- 0 until 4) {
    maxSubMinSel16(i) := vs2Element8ExcludeHighBitIsAll1s(i * 2 + 1) & vs2Element8IsAll1s(i * 2) &
                         vs1Element8ExcludeHighBitIsAll0s(i * 2 + 1) & vs1Element8IsAll0s(i * 2)
  }
  for (i <- 0 until 2) {
    maxSubMinSel32(i) := vs2Element8ExcludeHighBitIsAll1s(i * 4 + 3) & vs2Element8IsAll1s.asUInt(i * 4 + 2, i * 4).andR &
                         vs1Element8ExcludeHighBitIsAll0s(i * 4 + 3) & vs1Element8IsAll0s.asUInt(i * 4 + 2, i * 4).andR
  }
  maxSubMinSel64 := vs2Element8ExcludeHighBitIsAll1s(7) & vs2Element8IsAll1s.asUInt(6, 0).andR &
                    vs1Element8ExcludeHighBitIsAll0s(7) & vs1Element8IsAll0s.asUInt(6, 0).andR

  maxSubMin := Mux1H(Seq(
    sel8  -> maxSubMinSel8.asUInt.orR,
    sel16 -> maxSubMinSel16.asUInt.orR,
    sel32 -> maxSubMinSel32.asUInt.orR,
    sel64 -> maxSubMinSel64,
  ))

  private val avgResult = Wire(UInt(dWidth.W))
  avgResult := Mux1H(Seq(
    sel8  -> avgResultSel8.asUInt,
    sel16 -> avgResultSel16.asUInt,
    sel32 -> avgResultSel32.asUInt,
    sel64 -> avgResultSel64,
  ))

  out.res.avg := avgResult
  out.res.adcCmpMask := addCarryCmpMask
  out.res.adder := originAddResult

  out.mid.adder := originAddResult
  out.mid.lt := ltResult
  out.mid.vxsatAdder := sat
  out.mid.upOverflowUnSign := upOverflowUnSign.asUInt
  out.mid.downOverflowSign := downOverflowSign.asUInt

  dontTouch(avgResultSel8)
  dontTouch(avgResultSel16)
  dontTouch(avgResultSel32)
  dontTouch(avgResultSel64)
  dontTouch(avgShiftOneSel8)
  dontTouch(avgShiftOneSel16)
  dontTouch(avgShiftOneSel32)
  dontTouch(avgShiftOneSel64)
  dontTouch(avgShiftOneRoundSel8)
  dontTouch(avgShiftOneRoundSel16)
  dontTouch(avgShiftOneRoundSel32)
  dontTouch(avgShiftOneRoundSel64)
  dontTouch(originResult)

  dontTouch(vs2Head)
  dontTouch(vs1Head)
  dontTouch(vdHead)
  dontTouch(coutSat)
  dontTouch(originAddResult)

  dontTouch(sel8)
  dontTouch(sel16)
  dontTouch(sel32)
  dontTouch(sel64)
  dontTouch(isSub)
  dontTouch(srcbInv)
  dontTouch(cIn)
  dontTouch(srca)
  dontTouch(srcb)
  dontTouch(addSrc1)
  dontTouch(addSrc2)
  dontTouch(addSrc3)
  dontTouch(addPlus2Src1)
  dontTouch(addPlus2Src2)
  dontTouch(addPlus2Src3)
  dontTouch(addResult)
  dontTouch(avgAddPlus2Vec)
//  dontTouch(vsew)
  dontTouch(l0S)
  dontTouch(l0C)
  dontTouch(cout)
  dontTouch(addCarryCmpMask)
  dontTouch(srcaPre)
  dontTouch(ltVec)
  dontTouch(isCmpEq)
  dontTouch(isCmpNe)
  dontTouch(isCmpLt)
  dontTouch(isCmpLe)
  dontTouch(isCmpGt)
  dontTouch(eqResult)
  dontTouch(ltResult)
  dontTouch(cmpResult)
  dontTouch(avgAddPlus2Result)
  dontTouch(avgShiftOnePlus1Sel8)
  dontTouch(avgShiftOnePlus1Sel16)
  dontTouch(avgShiftOnePlus1Sel32)
  dontTouch(avgShiftOnePlus1Sel64)
  dontTouch(addPlus2Result)
  dontTouch(cInPlus2)
}

object Adder extends VIAlu.Config {
  class In extends VIAlu.Bundle {
    val vxrm = Vxrm()
    val ctrl = new InCtrl
    val data = new InData
  }

  class Out extends VIAlu.Bundle {
    val res = new OutRes
    val mid = new OutMidRes
  }

  class InCtrl extends VIAlu.Bundle {
    val sel8 = Bool()
    val sel16 = Bool()
    val sel32 = Bool()
    val sel64 = Bool()
    val widenVs2 = Bool()
    val widen = Bool()
    val isSigned = Bool()
    val isAddCarry = Bool()
    val isSub = Bool()
    val isCmpEq = Bool()
    val isCmpNe = Bool()
    val isCmpLt = Bool()
    val isCmpLe = Bool()
    val isCmpGt = Bool()
    val isVmsbc = Bool()
    val isAvg = Bool()
    val vm = Bool()
  }

  class InData extends VIAlu.Bundle {
    val vs2 = UInt(dWidth.W)
    val vs1 = UInt(dWidth.W)
    val vs2Widen = UInt(dWidth.W)
    val vs1Widen = UInt(dWidth.W)
    // Todo: use e8Mask instead
    val mask = UInt(maskWidth.W)
  }

  class OutRes extends VIAlu.Bundle {
    val avg = WidthData()
    val adder = WidthData()
    val adcCmpMask = new MaskGroup
  }

  class OutMidRes extends VIAlu.Bundle {
    val adder = WidthData()
    val lt = MaskData()
    val vxsatAdder = MaskData()
    val upOverflowUnSign = MaskData()
    val downOverflowSign = MaskData()
  }
}
