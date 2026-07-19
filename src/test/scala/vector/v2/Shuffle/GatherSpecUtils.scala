package yunsuan.vector.v2.Shuffle

trait GatherSpecUtils {
  val dlen = 128
  val vlen = 128
  val numElem = dlen / 8

  def sewShift(sew: Int): Int = sew match {
    case 8 => 0; case 16 => 1; case 32 => 2; case 64 => 3
  }

  def sewb(sew: Int): Int = sew / 8

  def toSewElems(value: BigInt, sew: Int, nElems: Int): Seq[Int] = {
    (0 until nElems).map(i => ((value >> (i * sew)) & ((BigInt(1) << sew) - 1)).toInt)
  }

  def fromSewElems(elems: Seq[Int], sew: Int): BigInt = {
    elems.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (v, i)) =>
      acc | (BigInt(v & ((1 << sew) - 1)) << (i * sew))
    }
  }

  def sewTo8b(vs2Sew: Seq[Int], sew: Int): Seq[Int] = {
    val s = sew / 8
    vs2Sew.flatMap(v => (0 until s).map(b => (v >> (b * 8)) & 0xff))
  }

  def eightBToSew(bytes: Seq[Int], sew: Int): Seq[Int] = {
    val s = sew / 8
    bytes.grouped(s).map(g => g.zipWithIndex.foldLeft(0) { case (a, (b, i)) => a | (b << (i * 8)) }).toSeq
  }
}
