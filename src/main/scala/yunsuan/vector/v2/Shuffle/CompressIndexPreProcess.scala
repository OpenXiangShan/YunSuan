package yunsuan.vector.v2.Shuffle

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._
import _root_.circt.stage.FirtoolOption
import yunsuan.vector.VectorShuffle.RotateUp

class CompressIndexGen(vlen: Int, dlen: Int) extends Module {
  require(isPow2(vlen) && vlen >= 64)
  require(isPow2(dlen) && dlen >= 64)
  require(vlen >= dlen)

  override def desiredName: String = super.desiredName + s"_${vlen}b"

  private val dlenb = dlen / 8
  private val vlenb = vlen / 8
  private val dlenbBits = log2Ceil(dlenb)
  private val MinDataWidth = 8
  private val NumElem = dlen / MinDataWidth
  private val IndexWidth = log2Ceil(vlen)
  private val SplittedParts = 8 * vlen / dlen
  private val vlWidth = log2Ceil(vlen + 1)

  val in = IO(Input(new Bundle {
    val valid = Bool()
    val uopIdx = UInt(log2Ceil(SplittedParts).W)
    val byteMask = UInt(vlen.W)
    val data = Vec(NumElem, UInt(MinDataWidth.W))
    val vs3 = UInt(dlen.W)
  }))

  val out = IO(Output(new Bundle {
    val valid = Bool()
    val validVec = Vec(SplittedParts, Bool())
    val uopIdx = UInt(log2Ceil(SplittedParts).W)
    val data = Vec(NumElem, UInt(MinDataWidth.W))
    val vs3 = UInt(dlen.W)
    val index = new GatherIndexBundle(NumElem, IndexWidth)
    val count1s = UInt(vlWidth.W)
  }))

  val indexPreProcess = Module(new CompressIndexPreProcess(vlen = vlen, dlen = dlen))
  val dataPreProcess = Module(new CompressDataPreProcess(vlen = vlen, dlen = dlen))

  private val vs3Reg = RegEnable(in.vs3, in.valid)

  private val leadingSums = indexPreProcess.out.leadingSums

  private val validRegVec = RegInit(VecInit.fill(SplittedParts)(false.B))

  for (i <- validRegVec.indices) {
    when (in.valid && in.uopIdx === i.U || validRegVec(i)) {
      validRegVec(i) := in.valid
    }
  }

  private val preProcessedDataReg = RegEnable(dataPreProcess.out.data, in.valid)
  private val uopIdxReg = RegEnable(in.uopIdx, in.valid)
  private val leadingSumsReg = RegEnable(leadingSums, in.valid)

  private val lzaForDataRotateLZA = VecInit((0.U +: leadingSumsReg).toSeq)

  indexPreProcess.in.valid := in.valid
  indexPreProcess.in.groupSel := in.uopIdx
  indexPreProcess.in.byteMask := in.byteMask

  dataPreProcess.in.groupSel := in.uopIdx
  dataPreProcess.in.byteMask := in.byteMask
  dataPreProcess.in.data := in.data

  val geLeadingSumsVec = Wire(Vec(NumElem, Vec(SplittedParts, Bool())))

  for (i <- geLeadingSumsVec.indices) {
    for (j <- geLeadingSumsVec.head.indices) {
      geLeadingSumsVec(i)(j) := Cat(in.uopIdx, i.U(dlenbBits.W)).suggestName(s"index$i") >= leadingSumsReg(j)
    }
  }

  // The count of index greater than or equal to leading sum
  private val geLeadingSumsSum: Seq[UInt] = geLeadingSumsVec.map(x => PopCount(x.asUInt))

  out.validVec := validRegVec
  out.uopIdx := uopIdxReg

  for (i <- out.index.index.indices) {
    out.index.indexValid(i) := geLeadingSumsVec(i).last // if index < popcount(mask)
    out.index.index(i) := Cat(geLeadingSumsSum(i), i.U(dlenbBits.W))
    out.index.fillZeros(i) := false.B
    out.index.fillScala(i) := false.B
  }
  out.valid := validRegVec.asUInt.orR

  out.data := RotateUp(VecInit(preProcessedDataReg.map(_.bits)), lzaForDataRotateLZA(uopIdxReg))
  out.vs3 := vs3Reg
  out.count1s := leadingSumsReg.last
}

class CompressIndexPreProcess(vlen: Int, dlen: Int) extends Module {
  require(isPow2(vlen) && vlen >= 64)
  require(isPow2(dlen) && dlen >= 64)
  require(vlen >= dlen)

  override def desiredName: String = super.desiredName + s"_${vlen}b"

  private val dlenb = dlen / 8
  private val vlenb = vlen / 8
  private val dlenbBits = log2Ceil(dlenb)
  private val MinDataWidth = 8
  private val NumElem = vlen / MinDataWidth
  private val IndexWidth = log2Ceil(vlen)
  private val SplittedParts = 8 * vlen / dlen

  val in = IO(Input(new Bundle {
    val valid = Bool()
    val groupSel = UInt(log2Ceil(SplittedParts).W)
    val byteMask = UInt(vlen.W)
  }))
  val out = IO(Output(new Bundle {
    // when dlen=128, dlenb=16, dlenbBits=4
    // leadingSums(0).getWidth will be 5 = log2Floor(0) + 4 + 1
    // leadingSums(0...n).getWidth will be 5, 6, 6, 7, 7, 7, 7, 8, ...
    val leadingSums = MixedVec(Seq.tabulate(SplittedParts)(i => UInt((log2Floor(i + 1) + dlenbBits + 1).W)))
  }))

  private val leadingSums = out.leadingSums

  for (i <- leadingSums.indices) {
    leadingSums(i) := PopCount(in.byteMask.take(dlenb * (i + 1)))
  }

  private val leadingSumOHs = IO(Output(Vec(SplittedParts, Vec(dlenb, Bool()))))

  for (i <- leadingSumOHs.indices) {
    for (j <- leadingSumOHs.head.indices) {
      leadingSumOHs(i)(j) := PopCount(in.byteMask.take(dlenb * (i + 1))).take(dlenbBits) === j.U
    }
  }
}

class CompressDataPreProcess(
  vlen: Int,
  dlen: Int,
) extends Module {
  private val dlenb = dlen / 8
  private val vlenb = vlen / 8
  private val dlenbBits = log2Ceil(dlenb)
  private val MinDataWidth = 8
  private val NumElem = vlen / MinDataWidth
  private val IndexWidth = log2Ceil(vlen)
  private val SplittedParts = 8 * vlen / dlen

  val in = IO(Input(new Bundle {
    val groupSel = UInt(log2Ceil(SplittedParts).W)
    val byteMask = UInt(vlen.W)
    val data = Vec(NumElem, UInt(MinDataWidth.W))
  }))
  val out = IO(Output(new Bundle {
    val data = Vec(NumElem, ValidIO(UInt(MinDataWidth.W)))
  }))

  // 8 Mux 1
  private val selectedByteMask: UInt = in.byteMask.splitToVecN(SplittedParts)(in.groupSel)
  private val compressData = Wire(Vec(NumElem, UInt(MinDataWidth.W)))
  private val compressDataValid = Wire(Vec(NumElem, Bool()))

  private val selectMatrix = Wire(Vec(NumElem, Vec(NumElem, Bool())))

  // leading one account
  private val loaVec = (0 until NumElem).map(i => PopCount(selectedByteMask.take(i + 1)))
  for (row <- selectMatrix.indices) {
    for (col <- selectMatrix.indices) {
      selectMatrix(row)(col) := (loaVec(col) === (row + 1).U) && selectedByteMask(col)
    }
  }

  for (i <- compressData.indices) {
    compressData(i) := Mux1H(
      selectMatrix(i).drop(i),
      in.data.drop(i),
    )
    compressDataValid(i) := VecInit(selectMatrix(i).drop(i)).asUInt.orR
  }

  for (i <- out.data.indices) {
    out.data(i).valid := compressDataValid(i)
    out.data(i).bits := compressData(i)
  }
}

class CompressDataPreProcess2(
  vlen: Int,
  dlen: Int,
) extends Module {
  private val dlenb = dlen / 8
  private val vlenb = vlen / 8
  private val dlenbBits = log2Ceil(dlenb)
  private val MinDataWidth = 8
  private val NumElem = vlen / MinDataWidth
  private val IndexWidth = log2Ceil(vlen)
  private val SplittedParts = 8 * vlen / dlen

  val in = IO(Input(new Bundle {
    val groupSel = UInt(log2Ceil(SplittedParts).W)
    val byteMask = UInt(vlen.W)
    val data = Vec(NumElem, UInt(MinDataWidth.W))
  }))
  val out = IO(Output(new Bundle {
    val data = Vec(NumElem, ValidIO(UInt(MinDataWidth.W)))
  }))

  // 8 Mux 1
  private val selectedByteMask: UInt = in.byteMask.splitToVecN(SplittedParts)(in.groupSel)
  private val compressData = Wire(Vec(NumElem, UInt(MinDataWidth.W)))
  private val compressDataValid = Wire(Vec(NumElem, Bool()))

  private val selectMatrix = Wire(Vec(NumElem, Vec(NumElem, Bool())))

  // leading zero account
  private val lzaVec = (0 until NumElem).map(i => PopCount(~selectedByteMask.take(i + 1)))
  for (row <- selectMatrix.indices) {
    for (col <- selectMatrix.indices) {
      selectMatrix(row)(col) := (if (col >= row) ((col - row).U === lzaVec(col)) && selectedByteMask(col) else false.B)
    }
  }

  for (i <- compressData.indices) {
    compressData(i) := Mux1H(
      selectMatrix(i).drop(i),
      in.data.drop(i)
    )
    compressDataValid(i) := VecInit(selectMatrix(i).drop(i)).asUInt.orR
  }

  for (i <- out.data.indices) {
    out.data(i).valid := compressDataValid(i)
    out.data(i).bits := compressData(i)
  }
}

object CompressIndexGenMain extends App {
  println("Generating the CompressIndexGen hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new CompressIndexGen(vlen = 128, dlen = 128)) +: firtoolAnno
  )

  println("done")
}

object CompressPreProcessMain extends App {
  println("Generating the CompressPreProcess hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new CompressIndexPreProcess(vlen = 128, dlen = 128)) +: firtoolAnno
  )

  println("done")
}

object CompressDataPreProcessMain extends App {
  println("Generating the CompressPreProcess hardware")

  val firtoolOpts = Array(
    "--target=systemverilog",
    "-O=release",
    "--disable-annotation-unknown",
    "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
  )
  val firtoolAnno = firtoolOpts.map(FirtoolOption.apply).toSeq

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new CompressDataPreProcess(vlen = 128, dlen = 128)) +: firtoolAnno
  )

  (new chisel3.stage.ChiselStage).execute(
    Array("--target-dir", "build/vector") ++ args,
    chisel3.stage.ChiselGeneratorAnnotation(() => new CompressDataPreProcess2(vlen = 128, dlen = 128)) +: firtoolAnno
  )

  println("done")
}
