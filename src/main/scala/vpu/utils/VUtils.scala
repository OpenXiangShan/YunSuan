package race.vpu

import chisel3._
import chisel3.util._
import VParams._

object Vlmul_to_lmul {
// vlmul --> LMUL --> max(LMUL, 1)
// Note: set result to 1 if LMUL < 1
  def apply(vlmul: UInt): UInt = {
    val y0 = !vlmul(1) && !vlmul(0) || vlmul(2)
    val y1 = !vlmul(2) && !vlmul(1) && vlmul(0)
    val y2 = !vlmul(2) && vlmul(1) && !vlmul(0)
    val y3 = !vlmul(2) && vlmul(1) && vlmul(0)
    Cat(y3, y2, y1, y0)
  }
}

object UIntSplit {
  // Split a UInt(VLEN.W) into lanes
  def vlen_splitTo_lanes(data: UInt): Seq[UInt] = {
    Seq.tabulate(NLanes)(i => data(LaneWidth*i+LaneWidth-1, LaneWidth*i))
  }

  def apply(data: UInt, width: Int): Seq[UInt] = {
    Seq.tabulate(data.getWidth / width)(i => data(width*i+width-1, width*i))
  }
}

object BitsExtend {
  def apply(data: UInt, extLen: Int, signed: Bool): UInt = {
    val width = data.getWidth
    require(width < extLen)
    Cat(Fill(extLen - width, data(width - 1) && signed), data)
  }

  def vector(data: UInt, extLen: Int, signed: Bool, sew: Int): UInt = { // For extension instrn
    require(data.getWidth % sew == 0)
    val nVec = data.getWidth / sew
    require(extLen % nVec == 0)
    Cat(UIntSplit(data, sew).map(dataSplit => apply(dataSplit, extLen / nVec, signed)).reverse)
  }
}

// Helper object containing the logic to find the lowest one-hot bit
object FindLowestOneHot {
  // Recursive function to find the lowest set bit and return it as one-hot
  // Assumes input width is a power of 2
  def findLowestOHPow2(in: UInt): UInt = {
    val width = in.getWidth
    if (width == 1) {
      in // Base case: width is 1, return directly
    } else {
      val half = width / 2
      val lower = in(half - 1, 0)       // Lower half of the input
      val upper = in(width - 1, half)   // Upper half of the input

      // Recursively find the lowest OH in each half
      val lowerOH = findLowestOHPow2(lower)
      val upperOH = findLowestOHPow2(upper)

      // If the lower half has any bit set (i.e., is not zero), use its one-hot result.
      // Otherwise, use the one-hot result from the upper half, shifted appropriately (via concatenation).
      Mux(lower =/= 0.U,
          Cat(0.U(half.W), lowerOH), // Pad upper bits with 0
          Cat(upperOH, 0.U(half.W))  // Pad lower bits with 0
      )
    }
  }

  // Public function handling non-power-of-2 widths by padding
  def apply(in: UInt): UInt = {
    val originalWidth = in.getWidth
    if (originalWidth == 0) {
       0.U(0.W) // Handle zero width case
    } else {
      // Calculate the next power-of-2 width
      val paddedWidth = 1 << log2Ceil(originalWidth)

      // Pad the input to power-of-2 width if needed
      val paddedIn = if (paddedWidth == originalWidth) {
        in
      } else {
        Cat(0.U((paddedWidth - originalWidth).W), in) // Pad with zeros at MSB
      }

      // Call the core recursive function on the padded input
      val paddedOH = findLowestOHPow2(paddedIn)

      // Truncate the result back to the original width
      paddedOH(originalWidth - 1, 0)
    }
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

// Tail generation: vlenb bits. Note: uopIdx < 8
object TailGen {
  def apply(vl: UInt, uopIdx: UInt, eew: SewOH, narrow: Bool = false.B): UInt = {
    val tail = Wire(UInt(vlenb.W))
    // vl - uopIdx * VLEN/eew
    val nElemRemain = Cat(0.U(1.W), vl) - Mux1H(eew.oneHot, Seq(3,2,1,0).map(_ + log2Up(VLEN/64)).map(x =>
                                                    Cat(Mux(narrow, uopIdx(2,1), uopIdx(2,0)), 0.U(x.W))))
    val maxNElemInOneUop = Mux1H(eew.oneHot, Seq(8,4,2,1).map(x => (x * VLEN/64).U))
    val vl_width = vl.getWidth
    require(vl_width == (log2Up(VLEN) + 1)) //bVL
    when (nElemRemain(vl_width)) {
      tail := ~0.U(vlenb.W)
    }.elsewhen (nElemRemain >= maxNElemInOneUop) {
      tail := 0.U
    }.otherwise {
      tail := UIntToCont0s(nElemRemain(log2Up(vlenb) - 1, 0), log2Up(vlenb))
    }
    tail
  }
}