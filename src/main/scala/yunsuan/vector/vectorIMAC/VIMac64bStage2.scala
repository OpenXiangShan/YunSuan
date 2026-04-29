package yunsuan.vector.mac

import chisel3._
import chisel3.util._
import yunsuan.util._
import yunsuan.vector._
import yunsuan.vector.Common._

class VIMac64bStage2 extends VIMac64b.Module {
  val in  = IO(Input(new VIMac64bStage2.In))
  val out = IO(Output(new VIMac64bStage2.Out))

  private val vxrmS1   = in.info.vxrm
  private val uopIdxS1 = in.info.uopIdx

  private val sewIs8S1    = in.ctrl.sewIs8S1
  private val sewIs16S1   = in.ctrl.sewIs16S1
  private val sewIs32S1   = in.ctrl.sewIs32S1
  private val sewIs64S1   = in.ctrl.sewIs64S1

  private val highHalfS1 = in.ctrl.highHalfS1
  private val widenS1    = in.ctrl.widenS1
  private val isFixPS1   = in.ctrl.isFixPS1

  val compStage1ResultsS1    = in.data.compStage1ResultsS1
  val wallaceLine34NonFixPS1 = in.data.wallaceLine34NonFixPS1
  val wallaceLine34FixPS1    = in.data.wallaceLine34FixPS1

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

  val wallace3to2CompStage2 = Module(new VIMac64bStage2.wallace3to2CompressorStage2())
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

  val finalSumAdderNonFixP = Module(new VIMac64bStage2.fullAdder152b())
  finalSumAdderNonFixP.io.sum34to2  := sum34to2NonFixP
  finalSumAdderNonFixP.io.cout34to2 := cout34to2NonFixP
  sumFinalNonFixP                   := finalSumAdderNonFixP.io.sumFinal

  val finalSumAdderFixP = Module(new VIMac64bStage2.fullAdder152b())
  finalSumAdderFixP.io.sum34to2  := sum34to2FixP
  finalSumAdderFixP.io.cout34to2 := cout34to2FixP
  sumFinalFixP                   := finalSumAdderFixP.io.sumFinal

  out.info.vxrm       := vxrmS1
  out.info.uopIdx     := uopIdxS1
  out.ctrl.sewIs8S2   := sewIs8S1
  out.ctrl.sewIs16S2  := sewIs16S1
  out.ctrl.sewIs32S2  := sewIs32S1
  out.ctrl.sewIs64S2  := sewIs64S1
  out.ctrl.highHalfS2 := highHalfS1
  out.ctrl.widenS2    := widenS1
  out.ctrl.isFixPS2   := isFixPS1

  out.data.sumFinalFixPS2     := sumFinalFixP
  out.data.sumFinalNonFixPS2  := sumFinalNonFixP
}

object VIMac64bStage2 {
  class In extends Bundle {
    val info = new Info
    val ctrl = new InCtrl
    val data = new InData
  }

  class Out extends Bundle {
    val info = new Info
    val ctrl = new OutCtrl
    val data = new OutData
  }

  class Info extends Bundle {
    val uopIdx = UInt(6.W)
    val vxrm = Vxrm()
  }

  class InCtrl extends Bundle {
    val sewIs8S1   = Bool()
    val sewIs16S1  = Bool()
    val sewIs32S1  = Bool()
    val sewIs64S1  = Bool()
    val highHalfS1 = Bool()
    val widenS1    = Bool()
    val isFixPS1   = Bool()
  }

  class InData extends Bundle {
    val compStage1ResultsS1    = Vec(7, UInt(152.W))
    val wallaceLine34NonFixPS1 = UInt(152.W)
    val wallaceLine34FixPS1    = UInt(152.W)
  }

  class OutCtrl extends Bundle {
    val sewIs8S2   = Bool()
    val sewIs16S2  = Bool()
    val sewIs32S2  = Bool()
    val sewIs64S2  = Bool()
    val highHalfS2 = Bool()
    val widenS2    = Bool()
    val isFixPS2   = Bool()
  }

  class OutData extends Bundle {
    val sumFinalNonFixPS2 = UInt(152.W)
    val sumFinalFixPS2    = UInt(152.W)
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
}
