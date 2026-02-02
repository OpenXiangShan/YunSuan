package yunsuan.vector.v2.Crypto

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * connection for vsha512c[lh]:
 * pipe0.vs1: VS1[1:0]: kw1, kw0
 * pipe0.vs2: VS2[1:0]: E, F
 * pipe0.vs3: VS3[1:0]: G, H
 * pipe1.vs1: VS1[3:2]: kw3, kw2
 * pipe1.vs2: VS2[3:2]: A, B
 * pipe1.vs3: VS3[3:2]: C, D
 */
class CryptoIterationCtrl extends Module {
  import CryptoIteration._

  val EGW = 256
  val EGS = 8
  val EEW = 32
  val DLEN = 128
  val in = IO(Input(new Bundle {
    val pipe0 = new InPipe
    val pipe1 = new InPipe
    val clear = Bool()
  }))

  val toVSha256c = IO(Output(new VSha256c.In))
  val fromVSha256c = IO(Input(new Bundle {
    val iterOut = new VSha256c.IterOut
    val out = new VSha256c.Out
  }))

  val toVSha512c = IO(Output(new VSha512c.In))
  val fromVSha512c = IO(Input(new Bundle {
    val iterOut = new VSha512c.IterOut
    val out = new VSha512c.Out
  }))

  val toVSm3c = IO(Output(new VSm3c.In))
  val fromVSm3c = IO(Input(new Bundle {
    val iterOut = new VSm3c.IterOut
    val out = new VSm3c.Out
  }))

  val out = IO(Output(new Bundle {
    val pipe0 = new OutPipe
    val pipe1 = new OutPipe
  }))

  private val pipe0 = in.pipe0
  private val pipe1 = in.pipe1

  private val pipe0ValidReg, pipe1ValidReg = RegInit(false.B)
  private val vs1Regs = Reg(Vec(8, UInt(32.W)))
  private val vs2Regs = Reg(Vec(8, UInt(32.W)))
  private val vs3Regs = Reg(Vec(8, UInt(32.W)))
  private val tmpRegs = Reg(Vec(4, UInt(32.W)))
  private val vs1RegsE64View = vs1Regs.splitToVecByWidth(64)
  private val vs2RegsE64View = vs2Regs.splitToVecByWidth(64)
  private val vs3RegsE64View = vs3Regs.splitToVecByWidth(64)
  private val vs1RegsE32View = vs1Regs.splitToVecByWidth(32)
  private val vs2RegsE32View = vs2Regs.splitToVecByWidth(32)
  private val vs3RegsE32View = vs3Regs.splitToVecByWidth(32)

  private val vs1RegsWenWdataE32Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS)(mutable.ArrayBuffer())
  private val vs2RegsWenWdataE32Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS)(mutable.ArrayBuffer())
  private val vs3RegsWenWdataE32Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS)(mutable.ArrayBuffer())
  private val tmpRegsWenWdataE32Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(4)(mutable.ArrayBuffer())
  private val vs1RegsWenWdataE64Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS / 2)(mutable.ArrayBuffer())
  private val vs2RegsWenWdataE64Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS / 2)(mutable.ArrayBuffer())
  private val vs3RegsWenWdataE64Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(EGS / 2)(mutable.ArrayBuffer())
  private val tmpRegsWenWdataE64Vec: mutable.Seq[ArrayBuffer[(Bool, UInt)]] = mutable.Seq.fill(2)(mutable.ArrayBuffer())

  when(in.clear) {
    pipe0ValidReg := false.B
    pipe1ValidReg := false.B
  }.otherwise {
    when (pipe0.valid && !pipe1.valid) {
      pipe0ValidReg := true.B
    }.elsewhen (pipe0ValidReg && pipe1.valid) {
      pipe0ValidReg := false.B
    }
    when (!pipe0.valid && pipe1.valid) {
      pipe1ValidReg := true.B
    }.elsewhen (pipe1ValidReg && pipe0.valid) {
      pipe1ValidReg := false.B
    }
  }

  /**
   *  vsha2c[lh] for SHA256
   */
  val vsha256cStages = RegInit(VecInit.fill(VSha256c.NumStage)(false.B))

  if (true) {
    val vsha256cl = RegEnable(pipe0.uop.vsha2cl, pipe0.valid)

    vsha256cStages zip (in.pipe0.valid +: vsha256cStages.dropRight(1)) foreach {
      case (reg, next) =>
        when (reg || next) {
          reg := next
        }
    }

    // Update Regs when receiving uop
    if (true) {
      val h :: g :: d :: c :: Nil = pipe0.vs3.splitToVec(4, 32, "pipe0Vs3E32").toList
      val f :: e :: b :: a :: Nil = pipe0.vs2.splitToVec(4, 32, "pipe0Vs2E32").toList
      val kw0 :: kw1 :: kw2 :: kw3 :: Nil = pipe0.vs1.splitToVec(4, 32, "pipe0Vs1E32").toList
      val pipe0ValidVSha256c = pipe0.valid && pipe0.uop.vsha256c
      vs3RegsWenWdataE32Vec(0) += pipe0ValidVSha256c -> h
      vs3RegsWenWdataE32Vec(2) += pipe0ValidVSha256c -> g
      vs3RegsWenWdataE32Vec(4) += pipe0ValidVSha256c -> d
      vs3RegsWenWdataE32Vec(6) += pipe0ValidVSha256c -> c
      vs2RegsWenWdataE32Vec(0) += pipe0ValidVSha256c -> f
      vs2RegsWenWdataE32Vec(2) += pipe0ValidVSha256c -> e
      vs2RegsWenWdataE32Vec(4) += pipe0ValidVSha256c -> b
      vs2RegsWenWdataE32Vec(6) += pipe0ValidVSha256c -> a
      vs1RegsWenWdataE32Vec(0) += pipe0ValidVSha256c -> kw0
      vs1RegsWenWdataE32Vec(2) += pipe0ValidVSha256c -> kw1
      vs1RegsWenWdataE32Vec(4) += pipe0ValidVSha256c -> kw2
      vs1RegsWenWdataE32Vec(6) += pipe0ValidVSha256c -> kw3
    }

    // Provide source data
    if (true) {
      // only use lower 32bit
      val kw0 :: kw1 :: kw2 :: kw3 :: Nil = vs1RegsE64View.toList.map(_.take(32))
      val f :: e :: b :: a :: Nil = vs2RegsE64View.toList.map(_.take(32))
      val h :: g :: d :: c :: Nil = vs3RegsE64View.toList.map(_.take(32))
      toVSha256c.stages := vsha256cStages
      toVSha256c.abcdefgh := Cat(Seq(a, b, c, d, e, f, g, h)).ensuring(_.getWidth == EEW * 8)
      val kw1kw0 = kw1 ## kw0
      val kw3kw2 = kw3 ## kw2
      toVSha256c.kw1kw0 := Mux(vsha256cl, kw1kw0, kw3kw2)
    }

    //  Iterate Update
    //    vs2: {_, a, _, b, _, e, _, f}
    //    vs3: {_, c, _, d, _, g, _, h}
    if (true) {
      val h :: g :: f :: e :: d :: c :: b :: a :: Nil = fromVSha256c.iterOut.abcdefgh.splitToVec(8, EEW).toList
      val vsha256cStage0or1 = Cat(Seq(0, 1).map(vsha256cStages.apply)).orR
      vs2RegsWenWdataE32Vec(0) += vsha256cStage0or1 -> f
      vs2RegsWenWdataE32Vec(2) += vsha256cStage0or1 -> e
      vs2RegsWenWdataE32Vec(4) += vsha256cStage0or1 -> b
      vs2RegsWenWdataE32Vec(6) += vsha256cStage0or1 -> a
      vs3RegsWenWdataE32Vec(0) += vsha256cStage0or1 -> h
      vs3RegsWenWdataE32Vec(2) += vsha256cStage0or1 -> g
      vs3RegsWenWdataE32Vec(4) += vsha256cStage0or1 -> d
      vs3RegsWenWdataE32Vec(6) += vsha256cStage0or1 -> c
    }
  }
  val vsha256cPipe0Vd = fromVSha256c.out.ab ## fromVSha256c.out.ef

  /**
   *  vsha2c[lh] for SHA512
   */
  val vsha512cNumStage = 4
  val vsha512cStages = RegInit(VecInit.fill(vsha512cNumStage)(false.B))
  if (true) {
    val vsha512cl = RegEnable(pipe0.uop.vsha2cl, pipe0.valid)

    for (i <- 0 until vsha512cNumStage) {
      if (i == 0) {
        vsha512cStages(0) := (pipe0ValidReg || pipe0.valid) && (pipe1ValidReg || pipe1.valid)
      } else {
        vsha512cStages(i) := vsha512cStages(i - 1)
      }
    }

    // Update Regs when receiving uop
    if (true) {
      val h :: g :: Nil = pipe0.vs3.splitToVecN(2, "pipe0Vs3E64").toList
      val d :: c :: Nil = pipe1.vs3.splitToVecN(2, "pipe1Vs3E64").toList
      val f :: e :: Nil = pipe0.vs2.splitToVecN(2, "pipe0Vs2E64").toList
      val b :: a :: Nil = pipe1.vs2.splitToVecN(2, "pipe1Vs2E64").toList
      val kw0 :: kw1 :: Nil = pipe0.vs1.splitToVecN(2, "pipe0Vs1E64").toList
      val kw2 :: kw3 :: Nil = pipe1.vs1.splitToVecN(2, "pipe1Vs1E64").toList
      val pipe0ValidVSha512c = pipe0.valid && pipe0.uop.vsha512c
      val pipe1ValidVSha512c = pipe1.valid && pipe1.uop.vsha512c
      vs3RegsWenWdataE64Vec(0) += pipe0ValidVSha512c -> h
      vs3RegsWenWdataE64Vec(1) += pipe0ValidVSha512c -> g
      vs3RegsWenWdataE64Vec(2) += pipe1ValidVSha512c -> d
      vs3RegsWenWdataE64Vec(3) += pipe1ValidVSha512c -> c
      vs2RegsWenWdataE64Vec(0) += pipe0ValidVSha512c -> f
      vs2RegsWenWdataE64Vec(1) += pipe0ValidVSha512c -> e
      vs2RegsWenWdataE64Vec(2) += pipe1ValidVSha512c -> b
      vs2RegsWenWdataE64Vec(3) += pipe1ValidVSha512c -> a
      vs1RegsWenWdataE64Vec(0) += pipe0ValidVSha512c -> kw0
      vs1RegsWenWdataE64Vec(1) += pipe0ValidVSha512c -> kw1
      vs1RegsWenWdataE64Vec(2) += pipe1ValidVSha512c -> kw2
      vs1RegsWenWdataE64Vec(3) += pipe1ValidVSha512c -> kw3
    }

    // Provide source data
    if (true) {
      val kw0 :: kw1 :: kw2 :: kw3 :: Nil = vs1RegsE64View.toList
      val f :: e :: b :: a :: Nil = vs2RegsE64View.toList
      val h :: g :: d :: c :: Nil = vs3RegsE64View.toList
      toVSha512c.stages := vsha512cStages
      toVSha512c.abcdefgh := Cat(Seq(a, b, c, d, e, f, g, h))
      val kw1kw0 = kw1 ## kw0
      val kw3kw2 = kw3 ## kw2
      toVSha512c.kw1kw0 := Mux(vsha512cl, kw1kw0, kw3kw2)

      val t1 :: t2 :: Nil = tmpRegs.splitToVecByWidth(64).toList
      toVSha512c.t2t1 := Cat(Seq(t2, t1))
    }

    //  Iterate Update
    //    tmp: {t2, t1}
    if (true) {
      val t1 :: t2 :: Nil = fromVSha512c.iterOut.t2t1.splitToVecN(2).toList
      val vsha512cStage0or2 = Cat(Seq(0, 2).map(vsha512cStages.apply)).orR
      tmpRegsWenWdataE64Vec(0) += vsha512cStage0or2 -> t1
      tmpRegsWenWdataE64Vec(1) += vsha512cStage0or2 -> t2
    }

    //  Iterate Update
    //    vs2: {a, b, e, f}
    //    vs3: {c, d, g, h}
    if (true) {
      val h :: g :: f :: e :: d :: c :: b :: a :: Nil = fromVSha512c.iterOut.abcdefgh.splitToVecN(8).toList
      val vsha512cStage1or3 = Cat(Seq(1, 3).map(vsha512cStages.apply)).orR
      vs2RegsWenWdataE64Vec(0) += vsha512cStage1or3 -> f
      vs2RegsWenWdataE64Vec(1) += vsha512cStage1or3 -> e
      vs2RegsWenWdataE64Vec(2) += vsha512cStage1or3 -> b
      vs2RegsWenWdataE64Vec(3) += vsha512cStage1or3 -> a
      vs3RegsWenWdataE64Vec(0) += vsha512cStage1or3 -> h
      vs3RegsWenWdataE64Vec(1) += vsha512cStage1or3 -> g
      vs3RegsWenWdataE64Vec(2) += vsha512cStage1or3 -> d
      vs3RegsWenWdataE64Vec(3) += vsha512cStage1or3 -> c
    }
  }
  val vsha512cPipe0Vd = fromVSha512c.out.ef
  val vsha512cPipe1Vd = fromVSha512c.out.ab

  /**
   *  vsm3c
   */
  val vsm3cNumStage = 4
  val vsm3cStages = RegInit(VecInit.fill(vsm3cNumStage)(false.B))
  if (true) {
    // Todo: optimize Reg jle15. Merge it with other uops' regs.
    val jle15 = RegEnable(pipe0.uimm <= 7.U, pipe0.valid)

    // Update iteration stages
    // Todo: merge it with other uops' stage regs
    for (i <- 0 until vsm3cNumStage) {
      if (i == 0) {
        vsm3cStages(0) := (pipe0ValidReg || pipe0.valid) && (pipe1ValidReg || pipe1.valid)
      } else {
        vsm3cStages(i) := vsm3cStages(i - 1)
      }
    }

    // Update Regs when receiving uop
    if (true) {
      val w0 :: w1 :: _ :: _ :: Nil = pipe0.vs2.splitToVecByWidth(32, "pipe0Vs2E32").toList
      val w4 :: w5 :: _ :: _ :: Nil = pipe1.vs2.splitToVecByWidth(32, "pipe1Vs2E32").toList
      val a :: b :: c :: d :: Nil = pipe0.vs3.splitToVecByWidth(32, "pipe0Vs3E32").toList
      val e :: f :: g :: h :: Nil = pipe1.vs3.splitToVecByWidth(32, "pipe1Vs3E32").toList
      // Todo: optimize it. use only one rotateLeft, since tjrolj1 will be used at stages(2).
      val uimm = pipe0.uimm
      val j = Cat(uimm, 0.U(1.W))
      val jp1 = Cat(uimm, 1.U(1.W))
      val tjrolj = VSm3c.Tj(j).rotateLeft(j)
      val tjrolj1 = VSm3c.Tj(jp1).rotateLeft(jp1)
      val pipe0ValidVSm3c = pipe0.valid && pipe0.uop.vsm3c
      val pipe1ValidVSm3c = pipe1.valid && pipe1.uop.vsm3c

      vs1RegsWenWdataE32Vec(0) += pipe0ValidVSm3c -> tjrolj
      vs1RegsWenWdataE32Vec(1) += pipe0ValidVSm3c -> tjrolj1

      vs2RegsWenWdataE32Vec(0) += pipe0ValidVSm3c -> w0
      vs2RegsWenWdataE32Vec(1) += pipe0ValidVSm3c -> w1
      vs2RegsWenWdataE32Vec(4) += pipe1ValidVSm3c -> w4
      vs2RegsWenWdataE32Vec(5) += pipe1ValidVSm3c -> w5

      vs3RegsWenWdataE32Vec(0) += pipe0ValidVSm3c -> a
      vs3RegsWenWdataE32Vec(1) += pipe0ValidVSm3c -> b
      vs3RegsWenWdataE32Vec(2) += pipe0ValidVSm3c -> c
      vs3RegsWenWdataE32Vec(3) += pipe0ValidVSm3c -> d
      vs3RegsWenWdataE32Vec(4) += pipe1ValidVSm3c -> e
      vs3RegsWenWdataE32Vec(5) += pipe1ValidVSm3c -> f
      vs3RegsWenWdataE32Vec(6) += pipe1ValidVSm3c -> g
      vs3RegsWenWdataE32Vec(7) += pipe1ValidVSm3c -> h
    }

    // tjrolj: Tj(j) <<< j
    // tjrolj1: Tj(j|1) <<< (j|1)
    val w0 :: w1 :: tjrolj :: tjrolj1 :: w4 :: w5 :: _ :: _ :: Nil = vs2RegsE32View.toList
    val a :: b :: c :: d :: e :: f :: g :: h :: Nil = vs3RegsE32View.toList
    val ss2 :: ss1 :: ffjX0D :: ggjW0H :: Nil = tmpRegs.toList

    toVSm3c.stages := vsm3cStages

    toVSm3c.hgfedcba := Cat(Seq(h, g, f, e, d, c, b, a))
    toVSm3c.w5w4w1w0 := Cat(Seq(w5, w4, w1, w0))
    toVSm3c.tjrolj := Seq(tjrolj, tjrolj1)
    toVSm3c.jle15 := jle15

    toVSm3c.GGjW0HFFjX0D := Cat(Seq(ggjW0H, ffjX0D))
    toVSm3c.ss1ss2 := Cat(Seq(ss1, ss2))

    //  Iterate Update for stage0
    //    tmp: {_, _, _, _, GGjW0H, FFjX0D, ss1, ss2}
    if (true) {
      val vsm3cStage0or2 = Cat(Seq(0, 2).map(vsm3cStages.apply)).orR
      val ffjX0D :: ggjW0H :: Nil = fromVSm3c.iterOut.GGjW0HFFjX0D.splitToVecN(2).toList
      val ss2 :: ss1 :: Nil = fromVSm3c.iterOut.ss1ss2.splitToVecN(2).toList
      tmpRegsWenWdataE32Vec(0) += vsm3cStage0or2 -> ggjW0H
      tmpRegsWenWdataE32Vec(1) += vsm3cStage0or2 -> ffjX0D
      tmpRegsWenWdataE32Vec(2) += vsm3cStage0or2 -> ss1
      tmpRegsWenWdataE32Vec(3) += vsm3cStage0or2 -> ss2
    }

    //  Iterate Update for stage1 and stage3
    //    vs3: {h, g1, f, e1, d, c1, b, a1}
    if (true) {
      val a1 :: b0 :: c1 :: d0 :: e1 :: f0 :: g1 :: h0 :: Nil = fromVSm3c.iterOut.h0g1f0e1d0c1b0a1.splitToVecN(8).toList
      val vsm3cStage1or3 = Cat(Seq(1, 3).map(vsm3cStages.apply)).orR
      vs3RegsWenWdataE32Vec(0) += vsm3cStage1or3 -> a1
      vs3RegsWenWdataE32Vec(1) += vsm3cStage1or3 -> b0
      vs3RegsWenWdataE32Vec(2) += vsm3cStage1or3 -> c1
      vs3RegsWenWdataE32Vec(3) += vsm3cStage1or3 -> d0
      vs3RegsWenWdataE32Vec(4) += vsm3cStage1or3 -> e1
      vs3RegsWenWdataE32Vec(5) += vsm3cStage1or3 -> f0
      vs3RegsWenWdataE32Vec(6) += vsm3cStage1or3 -> g1
      vs3RegsWenWdataE32Vec(7) += vsm3cStage1or3 -> h0
    }
  }
  val vsm3cPipe0Vd = fromVSm3c.out.c1c2a1a2
  val vsm3cPipe1Vd = fromVSm3c.out.g1g2e1e2

  connectVsRegW(vs1Regs, vs1RegsWenWdataE64Vec.toSeq, vs1RegsWenWdataE32Vec.toSeq)
  connectVsRegW(vs2Regs, vs2RegsWenWdataE64Vec.toSeq, vs2RegsWenWdataE32Vec.toSeq)
  connectVsRegW(vs3Regs, vs3RegsWenWdataE64Vec.toSeq, vs3RegsWenWdataE32Vec.toSeq)
  connectVsRegW(tmpRegs, tmpRegsWenWdataE64Vec.toSeq, tmpRegsWenWdataE32Vec.toSeq)

  out.pipe0.valid := vsha512cStages(3) || vsm3cStages(3)
  out.pipe1.valid := vsha512cStages(3) || vsm3cStages(3)
  out.pipe0.vd := Mux1H(Seq(
    vsha256cStages(1) -> vsha256cPipe0Vd,
    vsha512cStages(3) -> vsha512cPipe0Vd,
    vsm3cStages(3) -> vsm3cPipe0Vd,
  ))
  out.pipe1.vd := Mux1H(Seq(
    vsha512cStages(3) -> vsha512cPipe1Vd,
    vsm3cStages(3) -> vsm3cPipe1Vd,
  ))

  def connectVsRegW(
    regs: Vec[UInt],
    e64Ws: Seq[ArrayBuffer[(Bool, UInt)]],
    e32Ws: Seq[ArrayBuffer[(Bool, UInt)]],
  ): Unit = {
    // convert e64Ws(i/2)(j) to e32Ws(i)(j)
    // e32Ws2(i)(j): e32Ws2(i)'s j-th write source

    val e32Ws2: Seq[ArrayBuffer[(Bool, UInt)]] = Seq.fill(e32Ws.size)(ArrayBuffer())
    for ((seq, i) <- e64Ws.zipWithIndex) {
      for ((wen, wdata) <- seq) {
        e32Ws2(i * 2) += wen -> wdata.take(32)
        e32Ws2(i * 2 + 1) += wen -> wdata.drop(32)
      }
    }

    for ((reg: UInt, i: Int) <- regs.zipWithIndex) {
      val e64wen = Cat(e64Ws(i / 2).map{ case (wen, wdata) => wen }.toSeq).orR
      val e32wen = Cat(e32Ws(i).map{ case (wen, wdata) => wen }.toSeq).orR

      when (e64wen || e32wen) {
        reg := Mux1H(e32Ws(i) ++ e32Ws2(i))
      }
    }
  }
}

object CryptoIterationCtrl {
  def main(args: Array[String]): Unit = {
    println("Generating the CryptoEGW256Ctrl hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new CryptoIterationCtrl()) +: firtoolAnno
    )

    println("done")
  }
}
