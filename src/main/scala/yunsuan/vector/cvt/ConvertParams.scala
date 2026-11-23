package yunsuan.vector.cvt

/** Numeric format IDs, independent of any host instruction encoding. */
sealed abstract class ConvertFormat(val id: Int, val width: Int)
object ConvertFormat {
  case object I8 extends ConvertFormat(0, 8)
  case object U8 extends ConvertFormat(1, 8)
  case object E4M3 extends ConvertFormat(2, 8)
  case object E5M2 extends ConvertFormat(3, 8)
  case object E2M1 extends ConvertFormat(4, 4)
  val integers: Set[ConvertFormat] = Set(I8, U8)
  val floats: Set[ConvertFormat] = Set(E4M3, E5M2, E2M1)
  val all: Set[ConvertFormat] = integers ++ floats

  private[cvt] case class FloatSpec(exp: Int, frac: Int, bias: Int,
    maxFiniteFrac: Int, hasNaN: Boolean, hasInfinity: Boolean = false)
  private[cvt] def floatSpec(format: ConvertFormat): FloatSpec = format match {
    case E4M3 => FloatSpec(4, 3, 7, 6, hasNaN = true)
    case E5M2 => FloatSpec(5, 2, 15, 3, hasNaN = true, hasInfinity = true)
    case E2M1 => FloatSpec(2, 1, 1, 1, hasNaN = false)
    case _ => throw new IllegalArgumentException(s"$format is not a floating format")
  }
}

sealed trait ConvertDirection
object ConvertDirection {
  case object Encode extends ConvertDirection
  case object Decode extends ConvertDirection
  val all: Set[ConvertDirection] = Set(Encode, Decode)
}

sealed trait ConvertScale
object ConvertScale {
  case object Raw extends ConvertScale
  /** Encode divides by 2^(factor-127); decode multiplies by that value.
    * factor=255 follows the existing format-specific NaN-scale contract.
    */
  case object UE8M0 extends ConvertScale
  val all: Set[ConvertScale] = Set(Raw, UE8M0)
}

/** Compile-time capabilities. Disabled families/formats are never instantiated.
  * Integer formats are raw only; scales describe the enabled float modes.
  */
case class ConvertParams(
  formats: Set[ConvertFormat] = ConvertFormat.all,
  directions: Set[ConvertDirection] = ConvertDirection.all,
  scales: Set[ConvertScale] = ConvertScale.all
) {
  require(formats.nonEmpty, "at least one conversion format is required")
  require(directions.nonEmpty, "at least one conversion direction is required")
  require(scales.nonEmpty, "at least one scale mode is required")
  require(scales.contains(ConvertScale.Raw) || (formats & ConvertFormat.integers).isEmpty,
    "integer formats require the Raw scale mode")
  require(!scales.contains(ConvertScale.UE8M0) || (formats & ConvertFormat.floats).nonEmpty,
    "UE8M0 requires at least one floating format")

  val encode: Boolean = directions.contains(ConvertDirection.Encode)
  val decode: Boolean = directions.contains(ConvertDirection.Decode)
  val raw: Boolean = scales.contains(ConvertScale.Raw)
  val scaled: Boolean = scales.contains(ConvertScale.UE8M0)
  val orderedFormats: Seq[ConvertFormat] = formats.toSeq.sortBy(_.id)

  def supports(format: ConvertFormat, direction: ConvertDirection, scale: ConvertScale): Boolean =
    formats.contains(format) && directions.contains(direction) && scales.contains(scale) &&
      (scale == ConvertScale.Raw || ConvertFormat.floats.contains(format))
}
