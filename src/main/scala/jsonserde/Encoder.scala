package jsonserde

import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] extends Serializable { self =>
  def encode(value: A): Json

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.encode(f(value))
}

trait ObjectEncoder[A] extends Encoder[A] {
  def encode(value: A): JsonObj
}

object Encoder extends EncoderLowPriorityInstances {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  final implicit val jsonJsonEncoder: Encoder[Json] = (value: Json) => value

  final implicit val jsonUnitEncoder: Encoder[Unit] = (_: Unit) => JsonObj(List.empty)

  final implicit val jsonStringEncoder: Encoder[String] = (value: String) => JsonString(value)

  final implicit val jsonCharEncoder: Encoder[Char] = (value: Char) => JsonString(value.toString)

  final implicit val jsonBooleanEncoder: Encoder[Boolean] = (value: Boolean) => JsonBoolean(value)

  final implicit val jsonByteEncoder: Encoder[Byte] = (value: Byte) => JsonByte(value)

  final implicit val jsonShortEncoder: Encoder[Short] = (value: Short) => JsonShort(value)

  final implicit val jsonIntEncoder: Encoder[Int] = (value: Int) => JsonInt(value)

  final implicit val jsonLongEncoder: Encoder[Long] = (value: Long) => JsonLong(value)

  final implicit val jsonFloatEncoder: Encoder[Float] = (value: Float) => JsonFloat(value)

  final implicit val jsonDoubleEncoder: Encoder[Double] = (value: Double) => JsonDouble(value)

  final implicit val jsonBigIntEncoder: Encoder[BigInt] = (value: BigInt) => JsonBigInt(value)

  final implicit val jsonBigDecimalEncoder: Encoder[BigDecimal] = (value: BigDecimal) => JsonBigDecimal(value)

  final implicit def jsonArrayEncoder[A: Encoder]: Encoder[Array[A]] = (values: Array[A]) =>
    JsonArray(values.map(implicitly[Encoder[A]].encode(_)).toVector)

  final implicit def jsonListEncoder[A: Encoder]: Encoder[List[A]] = (values: List[A]) =>
    JsonArray(values.map(implicitly[Encoder[A]].encode(_)).toVector)

  final implicit def jsonMapEncoder[A: Encoder]: Encoder[Map[String, A]] = (values: Map[String, A]) =>
    JsonObj(values.map {
      case (k, v) => (k, implicitly[Encoder[A]].encode(v))
    }.toList)

  final implicit def jsonOptionEncoder[A: Encoder]: Encoder[Option[A]] = {
    case Some(value) => implicitly[Encoder[A]].encode(value)
    case None        => JsonNull
  }

  final implicit def jsonSomeEncoder[A: Encoder]: Encoder[Some[A]] = (value: Some[A]) => implicitly[Encoder[A]].encode(value.get)

  final implicit val jsonNoneEncoder: Encoder[None.type] = _ => JsonNull

  final implicit def genericEncoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Encoder[H]]
  ): Encoder[A] = (value: A) => hEncoder.value.encode(gen.to(value))
}

trait EncoderLowPriorityInstances {
  final implicit val hnilEncoder: Encoder[HNil] = (_: HNil) => JsonNull

  final implicit def hlistEncoder[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name

    hlist => {
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.value.encode(hlist.tail)

      tail match {
        case JsonObj(fields) => JsonObj(List((fieldName, head)) ::: fields)
        case _               => JsonObj(List((fieldName, head)))
      }
    }
  }
}
