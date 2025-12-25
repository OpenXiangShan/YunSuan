package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.vector._
import yunsuan.util._

class VIMac64bStage2Input extends Bundle {
  val compStage1ResultsS1    = Vec(7, UInt(152.W))
  val wallaceLine34NonFixPS1 = UInt(152.W)
  val wallaceLine34FixPS1    = UInt(152.W)
  val highHalfS1             = Bool()
  val uopIdxS1               = UInt(6.W)
  val widenS1                = Bool()
  val vxrmS1                 = UInt(2.W)
  val isFixPS1               = Bool()
  val sewIs8S1               = Bool()
  val sewIs16S1              = Bool()
  val sewIs32S1              = Bool()
  val sewIs64S1              = Bool()
}

class VIMac64bStage2Output extends Bundle {
  val sumFinalNonFixPS2  = UInt(152.W)
  val sumFinalFixPS2     = UInt(152.W)
  val highHalfS2         = Bool()
  val uopIdxS2           = UInt(6.W)
  val widenS2            = Bool()
  val vxrmS2             = UInt(2.W)
  val isFixPS2           = Bool()
  val sewIs8S2           = Bool()
  val sewIs16S2          = Bool()
  val sewIs32S2          = Bool()
  val sewIs64S2          = Bool()
}

class VIMac64bStage2 extends Module {
  val io = IO(new Bundle {
    val in  = Input(new VIMac64bStage2Input)
    val out = Output(new VIMac64bStage2Output)
  })

  val compStage1ResultsS1    = io.in.compStage1ResultsS1
  val wallaceLine34NonFixPS1 = io.in.wallaceLine34NonFixPS1
  val wallaceLine34FixPS1    = io.in.wallaceLine34FixPS1
  val highHalfS1             = io.in.highHalfS1
  val uopIdxS1               = io.in.uopIdxS1
  val widenS1                = io.in.widenS1
  val vxrmS1                 = io.in.vxrmS1
  val isFixPS1               = io.in.isFixPS1
  val sewIs8S1               = io.in.sewIs8S1
  val sewIs16S1              = io.in.sewIs16S1
  val sewIs32S1              = io.in.sewIs32S1
  val sewIs64S1              = io.in.sewIs64S1

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

  io.out.sumFinalFixPS2     := sumFinalFixP
  io.out.sumFinalNonFixPS2  := sumFinalNonFixP
  io.out.highHalfS2         := highHalfS1
  io.out.uopIdxS2           := uopIdxS1
  io.out.widenS2            := widenS1
  io.out.vxrmS2             := vxrmS1
  io.out.isFixPS2           := isFixPS1
  io.out.sewIs8S2           := sewIs8S1
  io.out.sewIs16S2          := sewIs16S1
  io.out.sewIs32S2          := sewIs32S1
  io.out.sewIs64S2          := sewIs64S1
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