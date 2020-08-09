package jsonserde

import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.reflect.ClassTag
import scala.util.Try

trait Decoder[A] extends Serializable { self =>
  def decode(json: Json): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (json: Json) => self.decode(json).map(f)
}

object Decoder extends DecoderLowPriorityInstances {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  final implicit val jsonJsonDecoder: Decoder[Json] = (json: Json) => Right(json)

  final implicit val jsonUnitDecoder: Decoder[Unit] = {
    case JsonObj(fields) if fields.isEmpty   => Right(())
    case JsonArray(values) if values.isEmpty => Right(())
    case JsonNull                            => Right(())
    case _                                   => Left(new RuntimeException("Unit"))
  }

  final implicit val jsonStringDecoder: Decoder[String] = {
    case JsonString(value) => Right(value)
    case _                 => Left(new RuntimeException("String"))
  }

  final implicit val jsonBooleanDecoder: Decoder[Boolean] = {
    case JsonBoolean(value) => Right(value)
    case _                  => Left(new RuntimeException("Boolean"))
  }

  final implicit val jsonCharDecoder: Decoder[Char] = {
    case JsonString(value) if value.length == 1 => Right(value.charAt(0))
    case _                                      => Left(new RuntimeException("Char"))
  }

  // todo: Add decoder for JsonNumber (?)

  final implicit val jsonByteDecoder: Decoder[Byte] = {
    case JNumber(value)    => value.toByte.toRight(new RuntimeException("Byte"))
    case JsonString(value) => value.toByteOption.toRight(new RuntimeException("Byte"))
    case _                 => Left(new RuntimeException("Byte"))
  }

  final implicit val jsonShortDecoder: Decoder[Short] = {
    case JNumber(value)    => value.toShort.toRight(new RuntimeException("Short"))
    case JsonString(value) => value.toShortOption.toRight(new RuntimeException("Short"))
    case _                 => Left(new RuntimeException("Short"))
  }

  final implicit val jsonIntDecoder: Decoder[Int] = {
    case JNumber(value)    => value.toInt.toRight(new RuntimeException("Int"))
    case JsonString(value) => value.toIntOption.toRight(new RuntimeException("Int"))
    case _                 => Left(new RuntimeException("Int"))
  }

  final implicit val jsonLongDecoder: Decoder[Long] = {
    case JNumber(value)    => value.toLong.toRight(new RuntimeException("Long"))
    case JsonString(value) => value.toLongOption.toRight(new RuntimeException("Long"))
    case _                 => Left(new RuntimeException("Long"))
  }

  final implicit val jsonFloatDecoder: Decoder[Float] = {
    case JNumber(value)    => Right(value.toFloat)
    case JsonNull          => Right(Float.NaN)
    case JsonString(value) => value.toFloatOption.toRight(new RuntimeException("Float"))
    case _                 => Left(new RuntimeException("Float"))
  }

  final implicit val jsonDoubleDecoder: Decoder[Double] = {
    case JNumber(value)    => Right(value.toDouble)
    case JsonNull          => Right(Double.NaN)
    case JsonString(value) => value.toDoubleOption.toRight(new RuntimeException("Double"))
    case _                 => Left(new RuntimeException("Double"))
  }

  final implicit val jsonBigIntDecoder: Decoder[BigInt] = {
    case JNumber(value)    => value.toBigInt.toRight(new RuntimeException("BigInt"))
    case JsonString(value) => Try(BigInt(value)).toEither
    case _                 => Left(new RuntimeException("BigInt"))
  }

  final implicit val jsonBigDecimalDecoder: Decoder[BigDecimal] = {
    case JNumber(value)    => value.toBigDecimal.toRight(new RuntimeException("BigDecimal"))
    case JsonString(value) => Try(BigDecimal(value)).toEither
    case _                 => Left(new RuntimeException("BigDecimal"))
  }

  final implicit def jsonOptionDecoder[A](implicit decoder: Decoder[A]): Decoder[Option[A]] = {
    case JsonNull => Right(None)
    case json: Json =>
      decoder.decode(json) match {
        case Left(value)  => Left(value)
        case Right(value) => Right(Some(value))
      }
  }

  final implicit def jsonArrayDecoder[A: ClassTag](implicit decoder: Decoder[A]): Decoder[Array[A]] = {
    case JsonArray(values) =>
      values
        .map(decoder.decode)
        .foldLeft(Right(Vector()): Either[Throwable, Vector[A]]) { (acc, el) =>
          for {
            a <- acc
            e <- el
          } yield a.appended(e)
        }
        .map(_.toArray)
    case _ => Left(new RuntimeException("Array"))
  }

  final implicit def jsonListDecoder[A: ClassTag](implicit decoder: Decoder[A]): Decoder[List[A]] = jsonArrayDecoder[A].map(_.toList)

  final implicit def jsonMapDecoder[A](implicit decoder: Decoder[A]): Decoder[Map[String, A]] = {
    case JsonObj(value) =>
      value.map {
        case (k, v) => decoder.decode(v).map(r => (k, r))
      }.foldLeft(Right(Map()): Either[Throwable, Map[String, A]]) { (acc, el) =>
        for {
          a <- acc
          e <- el
        } yield a.+(e)
      }
    case _ => Left(new RuntimeException("Map"))
  }

  final implicit def jsonGenericDecoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hDecoder: Lazy[Decoder[H]]
  ): Decoder[A] = json => hDecoder.value.decode(json).map(gen.from)
}

trait DecoderLowPriorityInstances {
  final implicit val hnilDecoder: Decoder[HNil] = _ => Right(HNil)

  final implicit def hlistDecoder[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[Decoder[T]]
  ): Decoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name

    json => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }

          jsonField match {
            case Some(value) =>
              val head = hDecoder.value.decode(value)
              val tail = tDecoder.value.decode(json)

              for {
                h <- head
                t <- tail
              } yield field[K](h) :: t
            case None => Left(new RuntimeException("Non-option field does not exist"))
          }
        case _ => Left(new RuntimeException("Incorrect json"))
      }
    }
  }

}
