package jsonserde

sealed trait Json

final case class JsonObj(fields: Map[String, Json]) extends Json

final case class JsonArray(values: Vector[Json]) extends Json

final case class JsonBoolean(value: Boolean) extends Json

final case class JsonNumber(value: BigDecimal) extends Json

final case class JsonString(value: String) extends Json

case object JsonNull extends Json
