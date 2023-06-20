
package yunsuan.vector

import chisel3._
import chisel3.util._

object UIntSplit {
  //Split into elements, e.g., if sew=8, UInt(64.W) => Seq(UInt(8.W) * 8)
  def apply(data: UInt, sew: Int): Seq[UInt] = {
    val w = data.getWidth
    require(w >= sew && w % sew == 0)
    Seq.tabulate(w/sew)(i => data(sew*i+sew-1, sew*i))
  }
}

object BitsExtend {
  def apply(data: UInt, extLen: Int, signed: Bool): UInt = {
    val width = data.getWidth
    require(width < extLen)
    Cat(Fill(extLen - width, data(width-1) && signed), data)
  }
  def vector(data: UInt, extLen: Int, signed: Bool, sew: Int): UInt = { // For extension instrn
    require(data.getWidth % sew == 0)
    val nVec = data.getWidth / sew
    require(extLen % nVec == 0)
    Cat(UIntSplit(data, sew).map(dataSplit => apply(dataSplit, extLen/nVec, signed)).reverse)
  }
}

// Extract 16-bit mask signal from 128-bit v0
object MaskExtract {
  def apply(vmask128b: UInt, uopIdx: UInt, sew: SewOH) = {
    val extracted = Wire(UInt(16.W))
    extracted := Mux1H(Seq.tabulate(8)(uopIdx === _.U),
                 Seq.tabulate(8)(idx => Mux1H(sew.oneHot, Seq(16,8,4,2).map(stride => 
                                              vmask128b((idx+1)*stride-1, idx*stride)))))
    extracted
  }
}

// E.g., 0.U(3.W) => b"1111_11111"  1.U(3.W) => b"1111_1110"  7.U(3.W) => b"1000_0000"
object UIntToCont0s {
  def apply(data: UInt, dw: Int): UInt = {  // dw is width of data
    if (dw == 1) {
      Mux(data === 0.U, 3.U(2.W), 2.U(2.W))
    } else {
      Mux(data(dw-1), Cat(apply(data(dw-2, 0), dw-1), 0.U((1 << (dw-1)).W)),
                      Cat(~0.U((1 << (dw-1)).W), apply(data(dw-2, 0), dw-1)))
    }
  }
}

// E.g., 0.U(3.W) => b"0000_0000"  1.U(3.W) => b"0000_0001"  7.U(3.W) => b"0111_1111"
object UIntToCont1s {
  def apply(data: UInt, dw: Int): UInt = {  // dw is width of data
    if (dw == 1) {
      Mux(data === 0.U, 0.U(2.W), 1.U(2.W))
    } else {
      Mux(data(dw-1), Cat(apply(data(dw-2, 0), dw-1), ~0.U((1 << (dw-1)).W)),
                      Cat(0.U((1 << (dw-1)).W), apply(data(dw-2, 0), dw-1)))
    }
  }
}

// Tail generation: 16 bits. Note: uopIdx < 8
object TailGen {
  def apply(vl: UInt, uopIdx: UInt, eew: SewOH, narrow: Bool = false.B): UInt = {
    val tail = Wire(UInt(16.W))
    // vl - uopIdx * 128/eew
    val nElemRemain = Cat(0.U(1.W), vl) - Mux1H(eew.oneHot, Seq(4,3,2,1).map(x => Cat(Mux(narrow, uopIdx(2,1), uopIdx(2,0)), 0.U(x.W))))
    val maxNElemInOneUop = Mux1H(eew.oneHot, Seq(16.U, 8.U, 4.U, 2.U))
    val vl_width = vl.getWidth
    require(vl_width == 8)
    when (nElemRemain(vl_width)) {
      tail := ~0.U(16.W)
    }.elsewhen (nElemRemain >= maxNElemInOneUop) {
      tail := 0.U
    }.otherwise {
      tail := UIntToCont0s(nElemRemain(3, 0), 4)
    }
    tail
  }
}

// Prestart generation: 16 bits. Note: uopIdx < 8
object PrestartGen {
  def apply(vstart: UInt, uopIdx: UInt, eew: SewOH, narrow: Bool = false.B): UInt = {
    val prestart = Wire(UInt(16.W))
    // vstart - uopIdx * 128/eew
    val nElemRemain = Cat(0.U(1.W), vstart) - Mux1H(eew.oneHot, Seq(4,3,2,1).map(x => Cat(Mux(narrow, uopIdx(2,1), uopIdx(2,0)), 0.U(x.W))))
    val maxNElemInOneUop = Mux1H(eew.oneHot, Seq(16.U, 8.U, 4.U, 2.U))
    val vstart_width = vstart.getWidth
    require(vstart_width == 7)
    when (nElemRemain(vstart_width)) {
      prestart := 0.U
    }.elsewhen (nElemRemain >= maxNElemInOneUop) {
      prestart := ~0.U(16.W)
    }.otherwise {
      prestart := ~(UIntToCont0s(nElemRemain(3, 0), 4))
    }
    prestart
  }
}

// Rearrange mask, tail, or vstart bits  (width: 16 bits)
object MaskReorg {
  // sew = 8: unchanged, sew = 16: 00000000abcdefgh -> aabbccddeeffgghh, ...
  def splash(bits: UInt, sew: SewOH): UInt = {
    Mux1H(sew.oneHot, Seq(1,2,4,8).map(k => Cat(bits(16/k -1, 0).asBools.map(Fill(k, _)).reverse)))
  }
}
