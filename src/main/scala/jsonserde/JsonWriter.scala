package jsonserde

trait JsonWriter[A] { self =>
  def write(value: A): Json

  final def contramap[B](f: B => A): JsonWriter[B] = (value: B) => self.write(f(value))
}

object JsonWriter {
  def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  final implicit val jsonJsonWrite: JsonWriter[Json] = (value: Json) => value

  final implicit val jsonUnitWrite: JsonWriter[Unit] = (_: Unit) => JsonObj(List.empty)

  final implicit val jsonStringWrite: JsonWriter[String] = (value: String) => JsonString(value)

  final implicit val jsonCharWrite: JsonWriter[Char] = (value: Char) => JsonString(value.toString)

  final implicit val jsonBooleanWrite: JsonWriter[Boolean] = (value: Boolean) => JsonBoolean(value)

  final implicit val jsonByteWrite: JsonWriter[Byte] = (value: Byte) => JsonNumber(value)

  final implicit val jsonShortWrite: JsonWriter[Short] = (value: Short) => JsonNumber(value)

  final implicit val jsonIntWrite: JsonWriter[Int] = (value: Int) => JsonNumber(value)

  final implicit val jsonLongWrite: JsonWriter[Long] = (value: Long) => JsonNumber(value)

  final implicit val jsonFloatWrite: JsonWriter[Float] = (value: Float) => JsonNumber(value)

  final implicit val jsonDoubleWrite: JsonWriter[Double] = (value: Double) => JsonNumber(value)

  final implicit val jsonBigIntWrite: JsonWriter[BigInt] = (value: BigInt) => JsonNumber(value)

  final implicit val jsonBigDecimalWrite: JsonWriter[BigDecimal] = (value: BigDecimal) => JsonNumber(value)

  final implicit def jsonArrayJsonWriter[A: JsonWriter]: JsonWriter[Array[A]] = (values: Array[A]) =>
    JsonArray(values.map(implicitly[JsonWriter[A]].write(_)).toVector)

  final implicit def jsonListJsonWriter[A: JsonWriter]: JsonWriter[List[A]] = (values: List[A]) =>
    JsonArray(values.map(implicitly[JsonWriter[A]].write(_)).toVector)

  final implicit def jsonMapJsonWriter[A: JsonWriter]: JsonWriter[Map[String, A]] = (values: Map[String, A]) =>
    JsonObj(values.map {
      case (k, v) => (k, implicitly[JsonWriter[A]].write(v))
    }.toList)
}
