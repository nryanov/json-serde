package jsonserde

sealed trait Json

final case class JsonObj(fields: List[(String, Json)]) extends Json

object JsonObj {
  final val EMPTY: JsonObj = JsonObj(List.empty)
}

final case class JsonArray(values: Vector[Json]) extends Json

object JsonArray {
  final val EMPTY: JsonArray = JsonArray(Vector.empty)
}

final case class JsonBoolean(value: Boolean) extends Json

final case class JsonString(value: String) extends Json

case object JsonNull extends Json

final case class JsonNumber(value: BigDecimal) extends Json

object JsonNumber {
  def apply(value: BigDecimal): JsonNumber = new JsonNumber(value)
  def apply(value: Byte): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: Short): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: Int): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: Long): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: Float): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: Double): JsonNumber = new JsonNumber(BigDecimal(value))
  def apply(value: BigInt): JsonNumber = new JsonNumber(BigDecimal(value))
}
