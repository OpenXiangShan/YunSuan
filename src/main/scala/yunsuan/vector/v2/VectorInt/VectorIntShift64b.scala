package yunsuan.vector.v2.VectorInt

import chisel3._
import chisel3.util._
import yunsuan.vector.Common._

import scala.language.implicitConversions

class VectorIntShift64b extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val sewOH = SewOH()
      val op = new Bundle {
        val isLeft = Bool()
        val isSigned = Bool()
        val isNarrow = Bool() // vs2: w, vs1: v, vd: v
        val isWiden = Bool()  // vs2: v, vs1: v, vd: w
        val isRotate = Bool()
      }
      val vs1 = UInt(64.W)
      val vs2 = UInt(64.W)
      val vs1Widen = UInt(64.W)
      val vs2Widen = UInt(64.W)
    })
    val out = Output(new Bundle {
      val vd = UInt(64.W)
      val narrowVd = UInt(32.W)
      val toFixP = Output(new MiscToFixP)
    })
  })

  private val sewOH = io.in.sewOH
  private val op = io.in.op

  val vshMod = Module(new VshDynamic64b)

  vshMod.io.in match {
    case in =>
      in.data := Mux(op.isWiden, io.in.vs2Widen, io.in.vs2)
      in.eewOH := Mux(op.isNarrow || op.isWiden, io.in.sewOH.toWiden, io.in.sewOH)
      in.signed := io.in.op.isSigned
      in.rotate := io.in.op.isRotate
      in.isLeft := io.in.op.isLeft
      in.shamt := Mux(op.isNarrow || op.isWiden, io.in.vs1Widen, io.in.vs1)
  }

  val res = vshMod.io.out.res
  val narrowE8ResVec  = VecInit(res.to16bitVec.map(_.take(8)))
  val narrowE16ResVec = VecInit(res.to32bitVec.map(_.take(16)))
  val narrowE32ResVec = VecInit(res.to64bitVec.map(_.take(32)))

  io.out.vd := res

  io.out.narrowVd := Mux1H(sewOH.rmE64, Seq(
    narrowE8ResVec.asUInt,
    narrowE16ResVec.asUInt,
    narrowE32ResVec.asUInt,
  ))

  io.out.toFixP.shiftOut := io.out.vd
  io.out.toFixP.rnd_high := vshMod.io.out.high.asUInt
  io.out.toFixP.rnd_tail := vshMod.io.out.tail.asUInt
}

/**
  *
  * @param shamt: should be 1, 2, 4, 8, etc.
  * @param dWidth: should be 8, 16, 32, 64, greater than shamt
  */
class VshrStatic(val shamt: Int, val dWidth: Int) extends Module {
  require(Seq(1,2,4,8,16,32).contains(shamt))
  require(Seq(8,16,32,64).contains(dWidth))
  require(dWidth > shamt)

  override def desiredName: String = s"VSHR_${dWidth}_${shamt}"

  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val shift = Bool()
      // origin data or last level normal shift result
      val data = UInt(dWidth.W)
      val op = new Bundle {
        // set when this module handle the highest bits and need signed shift
        val signed = Bool()
        // set when this module handle the highest bits and need unsigned shift
        val unsigned = Bool()
        // set if op is rotate shift.
        val rotateSelf = Bool()
        // set if this module is not the highest bits and use droppedBits to shift
        val middle = Bool()
      }
      // dropped bits from last level the former location module
      val droppedBits = UInt(shamt.W)

      val high = Bool()
      val tail = Bool()
    })
    val out = Output(new Bundle {
      // result of shift right or rotate shift right
      val res = UInt(dWidth.W)
      // The msb of dropped data by shift
      val high = Bool()
      // If the dropped data except msb is all 0s.
      val tail = Bool()
    })
  })

  private val shift = io.in.shift

  private val op = io.in.op
  private val signed = op.signed
  private val unsigned = op.unsigned
  private val rotateSelf = op.rotateSelf
  private val middle = op.middle

  private val signBit = io.in.data.head(1).asBool
  private val lastHigh = io.in.high
  private val lastTail = io.in.tail

  private val data = Mux1H(Seq(
    signed -> Fill(shamt, signBit && signed),
    unsigned -> Fill(shamt, 0.U),
    rotateSelf -> io.in.data.take(shamt),
    middle -> io.in.droppedBits
  )) ## io.in.data
  require(
    data.getWidth == dWidth + shamt,
    s"The width of data should be ${dWidth + shamt}, but get ${data.getWidth}"
  )

  private val res = Mux(shift, data.head(dWidth), data.take(dWidth))

  private val thisTail = (
    if (shamt == 1) true.B
    else io.in.data.take(shamt - 1) === 0.U
  )

  io.out.res := res
  io.out.high := Mux(shift, io.in.data(shamt - 1), io.in.high)
  io.out.tail := Mux(
    shift,
    thisTail && lastTail && !lastHigh,
    lastTail
  )
}

class VshDynamic64b extends Module {
  val dWidth: Int = 64
  val sWidth: Int = 6

  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val data = UInt(dWidth.W)
      val eewOH = SewOH()
      val signed = Bool()
      val rotate = Bool()
      val isLeft = Bool()
      val shamt = UInt(dWidth.W)
    })
    val out = Output(new Bundle {
      val res = UInt(dWidth.W)
      // The msb of dropped data by shift
      val high = Vec(8, Bool())
      // If the dropped data except msb is all 0s.
      val tail = Vec(8, Bool())
    })
  })

  private val eewOH = io.in.eewOH.oneHot
  private val isE8 = eewOH(0)
  private val isE16 = eewOH(1)
  private val isE32 = eewOH(2)
  private val isE64 = eewOH(3)
  private val signed = io.in.signed
  private val rotate = io.in.rotate
  private val isLeft = io.in.isLeft

  private val data = Mux(isLeft, Cat(io.in.data.asBools), io.in.data)

  // reverse shamt by elements if shift left.
  private val shamtE8Vec  = VecInit(Mux(isLeft, VecInit(io.in.shamt.to8bitVec.reverse),  io.in.shamt.to8bitVec).map(_.take(3)))
  private val shamtE16Vec = VecInit(Mux(isLeft, VecInit(io.in.shamt.to16bitVec.reverse), io.in.shamt.to16bitVec).map(_.take(4)))
  private val shamtE32Vec = VecInit(Mux(isLeft, VecInit(io.in.shamt.to32bitVec.reverse), io.in.shamt.to32bitVec).map(_.take(5)))
  private val shamtE64Vec = VecInit(Mux(isLeft, VecInit(io.in.shamt.to64bitVec.reverse), io.in.shamt.to64bitVec).map(_.take(6)))

  private val dataE8Vec  = data.to8bitVec

  private val shamtSeq: Seq[Seq[UInt]] = Seq(
    Seq.fill(1)(shamtE8Vec .map(_.take(3))).flatten,
    Seq.fill(2)(shamtE16Vec.map(_.take(4))).flatten,
    Seq.fill(4)(shamtE32Vec.map(_.take(5))).flatten,
    Seq.fill(8)(shamtE64Vec.map(_.take(6))).flatten,
  )

  private val shrMods_8_1 = Seq.fill(dWidth / 8)(Module(new VshrStatic(1, 8)))
  private val shrMods_8_2 = Seq.fill(dWidth / 8)(Module(new VshrStatic(2, 8)))
  private val shrMods_8_4 = Seq.fill(dWidth / 8)(Module(new VshrStatic(4, 8)))
  private val shrMods_16_8 = Seq.fill(dWidth / 16)(Module(new VshrStatic(8, 16)))
  private val shrMods_32_16 = Seq.fill(dWidth / 32)(Module(new VshrStatic(16, 32)))
  private val shrMods_64_32 = Seq.fill(dWidth / 64)(Module(new VshrStatic(32, 64)))

  private val shrModIsTopSeq2ForE8: Seq[Seq[Bool]] = {
    val numElem = dWidth / 8
    Seq(
      Seq.fill(numElem)(true.B),
      Seq.tabulate(numElem) { i => ((i + 1) % 2 == 0).B },
      Seq.tabulate(numElem) { i => ((i + 1) % 4 == 0).B },
      Seq.tabulate(numElem) { i => ((i + 1) % 8 == 0).B },
    ).transpose
  }

  private val shrModIsTopSeq2ForE16 = {
    val numElem = dWidth / 16
    Seq(
      Seq.fill(numElem)(true.B),
      Seq.tabulate(numElem) { i => (( i + 1) % 2 == 0).B },
      Seq.tabulate(numElem) { i => (( i + 1) % 4 == 0).B },
    ).transpose
  }

  private val shrModIsTopSeq2ForE32 = {
    val numElem = dWidth / 32
    Seq(
      Seq.fill(numElem)(true.B),
      Seq.tabulate(numElem) { i => ((i + 1) % 2 == 0).B },
    ).transpose
  }

  private val isTopForShrModE8  = VecInit.tabulate(dWidth /  8){ i => Mux1H(eewOH, shrModIsTopSeq2ForE8(i)) }
  private val isTopForShrModE16 = VecInit.tabulate(dWidth / 16){ i => Mux1H(eewOH.drop(1), shrModIsTopSeq2ForE16(i)) }
  private val isTopForShrModE32 = VecInit.tabulate(dWidth / 32){ i => Mux1H(eewOH.drop(2), shrModIsTopSeq2ForE32(i)) }
  private val isTopForShrModE64 = VecInit.tabulate(dWidth / 64){ i => true.B }

  def droppedBitsE8(dataVec: Seq[UInt], shamt: Int, i: Int): UInt = {
    val dataVecGpForE16 = dataVec.grouped(2).toSeq
    val dataVecGpForE32 = dataVec.grouped(4).toSeq
    val dataVecGpForE64 = dataVec.grouped(8).toSeq
    Mux1H(eewOH.drop(1), Seq(
      dataVecGpForE16(i / 2)((i + 1) % 2).take(shamt), // e16
      dataVecGpForE32(i / 4)((i + 1) % 4).take(shamt), // e32
      dataVecGpForE64(i / 8)((i + 1) % 8).take(shamt), // e64
    ))
  }

  def droppedBitsE16(dataVec: Seq[UInt], shamt: Int, i: Int): UInt = {
    val dataVecGpForE32 = dataVec.grouped(4).toSeq
    val dataVecGpForE64 = dataVec.grouped(8).toSeq
    Mux1H(eewOH.drop(2), Seq(
      dataVecGpForE32(i * 2 / 4)((i + 1) * 2 % 4).take(shamt), // e32
      dataVecGpForE64(i * 2 / 8)((i + 1) * 2 % 8).take(shamt), // e64
    ))
  }

  def droppedBitsE32(dataVec: Seq[UInt], shamt: Int, i: Int): UInt = {
    val dataVecGpForE64 = dataVec.grouped(4).toSeq
    dataVecGpForE64(i * 2 / 4)((i + 1) * 2 % 4).take(shamt) // e64
  }

  def droppedBitsE64(dataVec: Seq[UInt], shamt: Int, i: Int): UInt = {
    // This value is never used until 128 bits shift is supported
    0.U
  }

  private val shrModConnectSeq: Seq[(Seq[VshrStatic], (Seq[UInt], Seq[Bool], Seq[Bool]))] = Seq(
    shrMods_8_1 -> (dataE8Vec, Seq.fill(dWidth / 8)(false.B), Seq.fill(dWidth / 8)(true.B)),
    shrMods_8_2 -> (shrMods_8_1.map(x => (x.io.out.res, x.io.out.high, x.io.out.tail)).unzip3),
    shrMods_8_4 -> (shrMods_8_2.map(x => (x.io.out.res, x.io.out.high, x.io.out.tail)).unzip3),
  )

  for (((mods, (lastRes, lastHigh, lastTail)), shiftBit: Int) <- shrModConnectSeq.zipWithIndex) {
    for (i <- mods.indices) {
      val shamt = 1 << shiftBit
      val isTop = isTopForShrModE8(i)

      val in = mods(i).io.in
      in.shift := Mux1H(eewOH, shamtSeq.map(_(i)(shiftBit)))
      in.data := lastRes(i)
      in.op.signed := isTop && signed && !rotate
      in.op.unsigned := isTop && !signed && !rotate
      in.op.rotateSelf := eewOH(0) && rotate // only e8 can rotate itself
      in.op.middle := !isTop && !in.op.rotateSelf
      in.droppedBits := droppedBitsE8(lastRes, shamt, i)
      in.high := lastHigh(i)
      in.tail := lastTail(i)
    }
  }

  private val shrModMergeConnectSeq = Seq(
    shrMods_16_8  -> (shrMods_8_4.map(x => (x.io.out.res, x.io.out.high, x.io.out.tail)).unzip3),
    shrMods_32_16 -> (shrMods_16_8.map(x => (x.io.out.res, x.io.out.high, x.io.out.tail)).unzip3),
    shrMods_64_32 -> (shrMods_32_16.map(x => (x.io.out.res, x.io.out.high, x.io.out.tail)).unzip3),
  )

  private val isTopForMergeSeq = Seq(isTopForShrModE16, isTopForShrModE32, isTopForShrModE64)
  private val droppedBitsFunc: Seq[(Seq[UInt], Int, Int) => UInt] = Seq(droppedBitsE16, droppedBitsE32, droppedBitsE64)
  private val widthMatch = Seq(isE16, isE32, isE64)
  for (((mods, (lastRes, lastHigh, lastTail)), level) <- shrModMergeConnectSeq.zipWithIndex) {
    for ((mod, i) <- mods.zipWithIndex) {
      val shiftBit = log2Ceil(mods.head.shamt)
      val shamt = 1 << shiftBit
      val isTop = isTopForMergeSeq(level)(i)

      val in = mod.io.in
      in.shift := Mux1H(eewOH.drop(1 + level), shamtSeq.drop(1 + level).map(_(i)(shiftBit)))
      in.data := Cat(lastRes(i * 2 + 1), lastRes(i * 2))
      in.op.signed := isTop && signed && !rotate
      in.op.unsigned := isTop && !signed && !rotate
      in.op.rotateSelf := widthMatch(level) && rotate // only e16 can rotate itself
      in.op.middle := !isTop && !in.op.rotateSelf

      in.droppedBits := droppedBitsFunc(level)(lastRes, shamt, i)

      // only lower parts are used
      in.high := lastHigh(i * 2)
      in.tail := lastTail(i * 2)
    }
  }

  private val shrRes = Mux1H(eewOH, Seq(
    Cat(shrMods_8_4.map(_.io.out.res).reverse),
    Cat(shrMods_16_8.map(_.io.out.res).reverse),
    Cat(shrMods_32_16.map(_.io.out.res).reverse),
    Cat(shrMods_64_32.map(_.io.out.res).reverse),
  ))

  io.out.res := Mux(
    isLeft,
    shrRes.bitReverse,
    shrRes
  )

  io.out.high := Mux1H(eewOH, Seq(
    Cat(shrMods_8_4.map(_.io.out.high).reverse),
    Cat(shrMods_16_8.map(_.io.out.high).reverse.map(x => Fill(2, x))),
    Cat(shrMods_32_16.map(_.io.out.high).reverse.map(x => Fill(4, x))),
    Cat(shrMods_64_32.map(_.io.out.high).reverse.map(x => Fill(8, x))),
  )).asBools

  io.out.tail := Mux1H(eewOH, Seq(
    Cat(shrMods_8_4.map(_.io.out.tail).reverse),
    Cat(shrMods_16_8.map(_.io.out.tail).reverse.map(x => Fill(2, x))),
    Cat(shrMods_32_16.map(_.io.out.tail).reverse.map(x => Fill(4, x))),
    Cat(shrMods_64_32.map(_.io.out.tail).reverse.map(x => Fill(8, x))),
  )).asBools

  dontTouch(shamtE8Vec)
  dontTouch(shamtE16Vec)
  dontTouch(shamtE32Vec)
  dontTouch(shamtE64Vec)
  dontTouch(isE8)
  dontTouch(isE16)
  dontTouch(isE32)
  dontTouch(isE64)
  dontTouch(data)
  dontTouch(isTopForShrModE8)
  dontTouch(isTopForShrModE16)
  dontTouch(isTopForShrModE32)
  dontTouch(isTopForShrModE64)
}

object VshrDynamic64bMain extends App {
  println("Generating the VshrDynamic64b hardware")
  emitVerilog(new VshDynamic64b(), Array("--target-dir", "build/vector", "--throw-on-first-error", "--full-stacktrace"))
  println("done")
}

object VectorIntShift64bMain extends App {
  println("Generating the VectorIntShift64b hardware")
  emitVerilog(
    new VectorIntShift64b(), Array("--target-dir", "build/vector", "--throw-on-first-error", "--full-stacktrace"))
  println("done")
}
