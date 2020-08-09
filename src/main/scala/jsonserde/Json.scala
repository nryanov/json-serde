package jsonserde

import java.math.{BigDecimal => JBigDecimal}

sealed trait Json

final case class JsonObj(fields: List[(String, Json)]) extends Json

final case class JsonArray(values: Vector[Json]) extends Json

final case class JsonBoolean(value: Boolean) extends Json

final case class JsonString(value: String) extends Json

final case class JNumber(value: JsonNumber) extends Json

case object JsonNull extends Json

sealed abstract class JsonNumber extends Json {
  final def toByte: Option[Byte] = toLong match {
    case Some(value) =>
      val asByte = value.toByte
      if (value == asByte) Some(asByte) else None
    case None => None
  }

  final def toShort: Option[Short] = toLong match {
    case Some(value) =>
      val asShort = value.toShort
      if (value == asShort) Some(asShort) else None
    case None => None
  }

  final def toInt: Option[Int] = toLong match {
    case Some(value) =>
      val asInt = value.toInt
      if (value == asInt) Some(asInt) else None
    case None => None
  }

  def toLong: Option[Long]

  def toFloat: Float

  def toDouble: Double

  def toBigInt: Option[BigInt]

  def toBigDecimal: Option[BigDecimal]

  // https://github.com/circe/circe/blob/master/modules/core/shared/src/main/scala/io/circe/JsonNumber.scala
  private val bigDecimalMinLong: JBigDecimal = new JBigDecimal(Long.MinValue)
  private val bigDecimalMaxLong: JBigDecimal = new JBigDecimal(Long.MaxValue)

  final protected def isWholeNumber(value: JBigDecimal): Boolean =
    value.signum == 0 || value.scale <= 0 || value.stripTrailingZeros.scale <= 0

  final protected def isValidLong(value: JBigDecimal): Boolean =
    isWholeNumber(value) && value.compareTo(bigDecimalMinLong) >= 0 && value.compareTo(bigDecimalMaxLong) <= 0
}

final case class JsonBigDecimal(value: BigDecimal) extends JsonNumber {
  override def toLong: Option[Long] = if (value.isValidLong) Some(value.longValue) else None
  override def toFloat: Float = value.floatValue
  override def toDouble: Double = value.doubleValue
  override def toBigInt: Option[BigInt] = value.toBigIntExact
  override def toBigDecimal: Option[BigDecimal] = Some(value)
}

final case class JsonBigInt(value: BigInt) extends JsonNumber {
  override def toLong: Option[Long] = if (value.isValidLong) Some(value.longValue) else None
  override def toFloat: Float = value.floatValue
  override def toDouble: Double = value.doubleValue
  override def toBigInt: Option[BigInt] = Some(value)
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonByte(value: Byte) extends JsonNumber {
  override def toLong: Option[Long] = Some(value.toLong)
  override def toFloat: Float = value.toFloat
  override def toDouble: Double = value.toDouble
  override def toBigInt: Option[BigInt] = Some(BigInt(value))
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonShort(value: Short) extends JsonNumber {
  override def toLong: Option[Long] = Some(value.toLong)
  override def toFloat: Float = value.toFloat
  override def toDouble: Double = value.toDouble
  override def toBigInt: Option[BigInt] = Some(BigInt(value))
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonInt(value: Int) extends JsonNumber {
  override def toLong: Option[Long] = Some(value.toLong)
  override def toFloat: Float = value.toFloat
  override def toDouble: Double = value.toDouble
  override def toBigInt: Option[BigInt] = Some(BigInt(value))
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonLong(value: Long) extends JsonNumber {
  override def toLong: Option[Long] = Some(value)
  override def toFloat: Float = value.toFloat
  override def toDouble: Double = value.toDouble
  override def toBigInt: Option[BigInt] = Some(BigInt(value))
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonFloat(value: Float) extends JsonNumber {
  override def toLong: Option[Long] = if (isValidLong(new JBigDecimal(value))) Some(value.toLong) else None
  override def toFloat: Float = value
  override def toDouble: Double = value.toDouble
  override def toBigInt: Option[BigInt] = if (isWholeNumber(new JBigDecimal(value))) Some(BigInt(value.toLong)) else None
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}

final case class JsonDouble(value: Double) extends JsonNumber {
  override def toLong: Option[Long] = if (isValidLong(new JBigDecimal(value))) Some(value.toLong) else None
  override def toFloat: Float = value.toFloat
  override def toDouble: Double = value
  override def toBigInt: Option[BigInt] = if (isWholeNumber(new JBigDecimal(value))) Some(BigInt(value.toLong)) else None
  override def toBigDecimal: Option[BigDecimal] = Some(BigDecimal(value))
}
