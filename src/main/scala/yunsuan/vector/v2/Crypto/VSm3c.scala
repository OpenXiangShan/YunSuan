package yunsuan.vector.v2.Crypto

import _root_.circt.stage._
import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

import scala.language.implicitConversions

class VSm3c extends Module {
  import VSm3c._


  val in = IO(Input(new In))
  val toCtrl = IO(Output(new IterOut))
  val out = IO(Output(new Out))

  val stages = in.stages
  val a :: b :: c :: d :: e :: f :: g :: h :: Nil = in.hgfedcba.splitToVecN(8).toList
  val w0 :: w1 :: w4 :: w5 :: Nil = in.w5w4w1w0.splitToVecN(4).map(_.rev8).toList
  val tjrolj = in.tjrolj
  val jle15 = in.jle15

  val hgfedcba = Cat(Seq(h, g, f, e, d, c, b, a))

  val roundSS1 = Module(new RoundSS1)
  val roundGECA = Module(new RoundGECA)

  roundSS1.hgfedcba := hgfedcba
  roundSS1.w5w4 := Mux1H(Seq(
    stages(0) -> w4,
    stages(2) -> w5,
  ))
  roundSS1.w1w0 := Mux1H(Seq(
    stages(0) -> w0,
    stages(2) -> w1,
  ))
  roundSS1.jle15 := jle15
  roundSS1.tjrolj := Mux1H(Seq(
    stages(0) -> tjrolj(0),
    stages(2) -> tjrolj(1),
  ))

  {
    val ggjW0H :: ffjX0D :: ss1 :: ss2 :: Nil = roundSS1.out.toList
    toCtrl.GGjW0HFFjX0D := Cat(Seq(ggjW0H, ffjX0D))
    toCtrl.ss1ss2 := Cat(Seq(ss1, ss2))
  }

  roundGECA match {
    case roundGECA =>
      val ffjX0D :: ggjW0H :: Nil = in.GGjW0HFFjX0D.splitToVecN(2).toList
      val ss2 :: ss1 :: Nil = in.ss1ss2.splitToVecN(2).toList

      roundGECA.GGjW0 := ggjW0H
      roundGECA.FFjX0 := ffjX0D
      roundGECA.ss1   := ss1
      roundGECA.ss2   := ss2
      roundGECA.fb    := b :: f :: Nil
  }

  // stage1 or stage3
  {
    val a1 :: c1 :: e1 :: g1 :: Nil = roundGECA.geca.toList
    toCtrl.h0g1f0e1d0c1b0a1 := Cat(Seq(h, g1, f, e1, d, c1, b, a1))
  }

  val a2 :: c2 :: e2 :: g2 :: Nil = roundGECA.geca.toList
  val a1 :: _ :: c1 :: _ :: e1 :: _ :: g1 :: _ :: Nil = in.hgfedcba.splitToVecN(8).toList

  out.g1g2e1e2 := Cat(Seq(g1, g2, e1, e2))
  out.c1c2a1a2 := Cat(Seq(c1, c2, a1, a2))


  def FF1(x: UInt, y: UInt, z: UInt): UInt = x ^ y ^ z
  def FF2(x: UInt, y: UInt, z: UInt): UInt = (x & y) | (x & z) | (y & z)
  def FFj(x: UInt, y: UInt, z: UInt, jle15: Bool): UInt = Mux(jle15, FF1(x, y, z), FF2(x, y, z))

  def GG1(x: UInt, y: UInt, z: UInt): UInt = x ^ y ^ z
  def GG2(x: UInt, y: UInt, z: UInt): UInt = (x & y) | (~x).asUInt & z
  def GGj(x: UInt, y: UInt, z: UInt, jle15: Bool): UInt = Mux(jle15, GG1(x, y, z), GG2(x, y, z))

  def P0(x: UInt): UInt = x ^ x.rotateLeft(9) ^ x.rotateLeft(17)

  class RoundSS1 extends Module {
    val hgfedcba = IO(Input(UInt((8 * EEW).W)))
    val w1w0, w5w4 = IO(Input(UInt(EEW.W)))
    val jle15 = IO(Input(Bool()))
    val tjrolj = IO(Input(UInt(EEW.W)))

    val GGjW0, FFjX0 = IO(Output(UInt(EEW.W)))
    val ss1, ss2 = IO(Output(UInt(EEW.W)))

    val a :: b :: c :: d :: e :: f :: g :: h :: Nil = hgfedcba.splitToVecN(8).map(x => dontTouch(WireInit(x))).toList

    val x1x0 = w5w4 ^ w1w0
    ss1 := (a.rotateLeft(12) + e + tjrolj).rotateLeft(7)
    ss2 := ss1 ^ a.rotateLeft(12)
    GGjW0 := w1w0 + h + GGj(e, f, g, jle15)
    FFjX0 := x1x0 + d + FFj(a, b, c, jle15)

    def out: Seq[UInt] = Seq(GGjW0, FFjX0, ss1, ss2)
  }

  class RoundGECA extends Module {
    val GGjW0, FFjX0 = IO(Input(UInt(EEW.W)))
    val ss1, ss2 = IO(Input(UInt(EEW.W)))
    val fb = IO(Input(Vec(2, UInt(EEW.W))))

    val geca = IO(Output(Vec(4, UInt(EEW.W))))

    val b :: f :: Nil = fb.toList

    val tt1 = FFjX0 + ss2
    val tt2 = GGjW0 + ss1

    val g = f.rotateLeft(19)
    val e = P0(tt2)
    val c = b.rotateLeft(9)
    val a = tt1

    geca := VecInit(a :: c :: e :: g :: Nil)
  }
}

object VSm3c {
  def main(args: Array[String]): Unit = {
    println("Generating the VSm3c hardware")

    val firtoolOpts = Array(
      "--target=systemverilog",
      "-O=release",
      "--disable-annotation-unknown",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
    )
    val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

    (new ChiselStage).execute(
      Array("--target-dir", "build/vector") ++ args,
      chisel3.stage.ChiselGeneratorAnnotation(() => new VSm3c()) +: firtoolAnno
    )

    println("done")
  }

  def Tj(j: UInt) = {
    Mux(j <= 15.U, "h79CC4519".U, "h7A879D8A".U)
  }

  val EGW: Int = 256
  val EGS: Int = 8
  val EEW: Int = 32

  class In extends Bundle {
    val stages = Vec(4, Bool())
    // stage0 and stage2
    val hgfedcba = UInt((8 * EEW).W)
    val w5w4w1w0 = UInt((4 * EEW).W)
    // Tj(j).rotateLeft(j)
    // Tj(j | 1).rotateLeft(j | 1)
    val tjrolj = Vec(2, UInt(EEW.W))
    val jle15 = Bool()

    // stage1 and stage3
    val GGjW0HFFjX0D = UInt((2 * EEW).W)
    val ss1ss2 = UInt((2 * EEW).W)
  }

  class Out extends Bundle {
    val g1g2e1e2 = UInt((4 * EEW).W)
    val c1c2a1a2 = UInt((4 * EEW).W)
  }

  class IterOut extends Bundle {
    // stage0 and stage2
    val GGjW0HFFjX0D = UInt((2 * EEW).W)
    val ss1ss2 = UInt((2 * EEW).W)
    // stage1 and stage3
    val h0g1f0e1d0c1b0a1 = UInt((8 * EEW).W)
  }
}
