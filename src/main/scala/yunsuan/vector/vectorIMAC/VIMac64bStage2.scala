package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._

class VIMac64bStage2 extends Module {
  val io = IO(new Bundle {
    val compStage1ResultsS1    = Input(Vec(7, UInt(152.W)))
    val wallaceLine34NonFixPS1 = Input(UInt(152.W))
    val wallaceLine34FixPS1    = Input(UInt(152.W))
    val highHalfS1             = Input(Bool())
    val uopIdxS1               = Input(UInt(6.W))
    val widenS1                = Input(Bool())
    val vxrmS1                 = Input(UInt(2.W))
    val isFixPS1               = Input(Bool())
    val sewIs8S1               = Input(Bool())
    val sewIs16S1              = Input(Bool())
    val sewIs32S1              = Input(Bool())
    val sewIs64S1              = Input(Bool())

    val sumFinalNonFixPS2  = Output(UInt(152.W))
    val sumFinalFixPS2     = Output(UInt(152.W))
    val highHalfS2         = Output(Bool())
    val uopIdxS2           = Output(UInt(6.W))
    val widenS2            = Output(Bool())
    val vxrmS2             = Output(UInt(2.W))
    val isFixPS2           = Output(Bool())
    val sewIs8S2           = Output(Bool())
    val sewIs16S2          = Output(Bool())
    val sewIs32S2          = Output(Bool())
    val sewIs64S2          = Output(Bool())
  })
  
  val compStage1ResultsS1    = io.compStage1ResultsS1
  val wallaceLine34NonFixPS1 = io.wallaceLine34NonFixPS1
  val wallaceLine34FixPS1    = io.wallaceLine34FixPS1
  val highHalfS1             = io.highHalfS1
  val uopIdxS1               = io.uopIdxS1
  val widenS1                = io.widenS1
  val vxrmS1                 = io.vxrmS1
  val isFixPS1               = io.isFixPS1
  val sewIs8S1               = io.sewIs8S1
  val sewIs16S1              = io.sewIs16S1
  val sewIs32S1              = io.sewIs32S1
  val sewIs64S1              = io.sewIs64S1

  // Process 7 and 8 produce 2 sets of sums and couts and use 2 152-bit full adders for 
  // 1) non fixed-point operations and fixed-point operation without rounding increment
  // 2) fixed-point operation with rounding increment
  // respectively so chain adders used to handle rounding increment in Stage 3 could be
  // removed. Since these 2 sets of compressor and 152-bit full adders work in parallel,
  // the lacency could be hidden
  
  // 7.wallace compress stage 2
  val sum34to2NonFixP  = Wire(UInt(152.W))
  val cout34to2NonFixP = Wire(UInt(152.W))
  val sum34to2FixP     = Wire(UInt(152.W))
  val cout34to2FixP    = Wire(UInt(152.W))

  val wallace3to2CompStage2 = Module(new wallace3to2CompressorStage2())
  wallace3to2CompStage2.io.compStage1Results := compStage1ResultsS1
  wallace3to2CompStage2.io.wallaceLine34NonFixP := wallaceLine34NonFixPS1
  wallace3to2CompStage2.io.wallaceLine34FixP    := wallaceLine34FixPS1
  sum34to2NonFixP  := wallace3to2CompStage2.io.sum34to2NonFixP
  cout34to2NonFixP := wallace3to2CompStage2.io.cout34to2NonFixP
  sum34to2FixP     := wallace3to2CompStage2.io.sum34to2FixP
  cout34to2FixP    := wallace3to2CompStage2.io.cout34to2FixP

  // 8.generate 2 sets of final sums
  val sumFinalNonFixP  = Wire(UInt(152.W))
  val sumFinalFixP     = Wire(UInt(152.W))

  val finalSumAdderNonFixP = Module(new fullAdder152b())
  finalSumAdderNonFixP.io.sum34to2  := sum34to2NonFixP
  finalSumAdderNonFixP.io.cout34to2 := cout34to2NonFixP
  sumFinalNonFixP                   := finalSumAdderNonFixP.io.sumFinal

  val finalSumAdderFixP = Module(new fullAdder152b())
  finalSumAdderFixP.io.sum34to2  := sum34to2FixP
  finalSumAdderFixP.io.cout34to2 := cout34to2FixP
  sumFinalFixP                   := finalSumAdderFixP.io.sumFinal

  io.sumFinalFixPS2     := sumFinalFixP
  io.sumFinalNonFixPS2  := sumFinalNonFixP
  io.highHalfS2         := highHalfS1
  io.uopIdxS2           := uopIdxS1
  io.widenS2            := widenS1
  io.vxrmS2             := vxrmS1
  io.isFixPS2           := isFixPS1
  io.sewIs8S2           := sewIs8S1
  io.sewIs16S2          := sewIs16S1
  io.sewIs32S2          := sewIs32S1
  io.sewIs64S2          := sewIs64S1
}

class wallace3to2CompressorStage2 extends Module with wallace3to2Compressor {
  val io = IO(new Bundle {
    val compStage1Results    = Input(Vec(7, UInt(152.W)))
    val wallaceLine34NonFixP = Input(UInt(152.W))
    val wallaceLine34FixP    = Input(UInt(152.W))

    val sum34to2NonFixP  = Output(UInt(152.W))
    val cout34to2NonFixP = Output(UInt(152.W))
    val sum34to2FixP     = Output(UInt(152.W))
    val cout34to2FixP    = Output(UInt(152.W))
  })

  val compStage1Results    = io.compStage1Results
  val wallaceLine34NonFixP = io.wallaceLine34NonFixP
  val wallaceLine34FixP    = io.wallaceLine34FixP
  
  val interCompressResult = wallaceCompress(5, compStage1Results)

  val nonFixPCompressIn = interCompressResult :+ wallaceLine34NonFixP
  val fixPCompressIn    = interCompressResult :+ wallaceLine34FixP
  val nonFixPResult = wallaceCompress(2, nonFixPCompressIn)
  val fixPResult    = wallaceCompress(2, fixPCompressIn)
  
  io.sum34to2NonFixP  := nonFixPResult(0)
  io.cout34to2NonFixP := nonFixPResult(1)
  io.sum34to2FixP     := fixPResult(0)
  io.cout34to2FixP    := fixPResult(1)
}

trait wallace3to2Compressor {
  def compressor3to2(a: UInt, b: UInt, c: UInt): (UInt, UInt) = {
    val sum  = Wire(UInt(152.W))
    val cout = Wire(UInt(152.W))
    sum  := a ^ b ^ c
    cout := (a & b) | (b & c) | (c & a)
    (sum, cout)
  }

  def wallaceCompress(outNum: Int, compressIn: Seq[UInt]): Seq[UInt] = {
    if (compressIn.size == outNum) {
      compressIn
    }
    else {
      val compressGroupNum = compressIn.size / 3
      val sum =  Wire(Vec(compressGroupNum, UInt(152.W)))
      val cout = Wire(Vec(compressGroupNum, UInt(152.W)))
      for(i <- 0 until compressGroupNum) {
        sum(i)  := compressor3to2(compressIn(3*i), compressIn(3*i+1), compressIn(3*i+2))._1
        cout(i) := Cat(compressor3to2(compressIn(3*i), compressIn(3*i+1), compressIn(3*i+2))._2(150,0), 0.U(1.W))
      }
      wallaceCompress(outNum, sum ++ cout ++ compressIn.drop(3*compressGroupNum))
    }
  }
}

class fullAdder152b extends Module {
  val io = IO(new Bundle {
    val sum34to2  = Input(UInt(152.W))
    val cout34to2 = Input(UInt(152.W))

    val sumFinal  = Output(UInt(152.W))
  })

  io.sumFinal := io.sum34to2 + io.cout34to2
}