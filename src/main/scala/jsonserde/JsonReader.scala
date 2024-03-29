package jsonserde

import scala.reflect.ClassTag
import scala.util.Try

trait JsonReader[A] { self =>
  def read(json: Json): Either[Throwable, A]

  final def map[B](f: A => B): JsonReader[B] = (json: Json) => self.read(json).map(f)
}

object JsonReader {
  def apply[A](implicit reader: JsonReader[A]): JsonReader[A] = reader

  final implicit val jsonJsonRead: JsonReader[Json] = (json: Json) => Right(json)

  final implicit val jsonStringRead: JsonReader[String] = {
    case JsonString(value) => Right(value)
    case _                 => Left(new RuntimeException("String"))
  }

  final implicit val jsonBooleanRead: JsonReader[Boolean] = {
    case JsonBoolean(value) => Right(value)
    case _                  => Left(new RuntimeException("Boolean"))
  }

  final implicit val jsonCharRead: JsonReader[Char] = {
    case JsonString(value) if value.length == 1 => Right(value.charAt(0))
    case _                                      => Left(new RuntimeException("Char"))
  }

  final implicit val jsonByteRead: JsonReader[Byte] = {
    case JsonNumber(value) => Right(value.byteValue)
    case JsonString(value) => value.toByteOption.toRight(new RuntimeException("Byte"))
    case _                 => Left(new RuntimeException("Byte"))
  }

  final implicit val jsonShortRead: JsonReader[Short] = {
    case JsonNumber(value) => Right(value.shortValue)
    case JsonString(value) => value.toShortOption.toRight(new RuntimeException("Short"))
    case _                 => Left(new RuntimeException("Short"))
  }

  final implicit val jsonIntRead: JsonReader[Int] = {
    case JsonNumber(value) => Right(value.intValue)
    case JsonString(value) => value.toIntOption.toRight(new RuntimeException("Int"))
    case _                 => Left(new RuntimeException("Int"))
  }

  final implicit val jsonLongRead: JsonReader[Long] = {
    case JsonNumber(value) => Right(value.longValue)
    case JsonString(value) => value.toLongOption.toRight(new RuntimeException("Long"))
    case _                 => Left(new RuntimeException("Long"))
  }

  final implicit val jsonFloatRead: JsonReader[Float] = {
    case JsonNumber(value) => Right(value.floatValue)
    case JsonNull          => Right(Float.NaN)
    case JsonString(value) => value.toFloatOption.toRight(new RuntimeException("Float"))
    case _                 => Left(new RuntimeException("Float"))
  }

  final implicit val jsonDoubleRead: JsonReader[Double] = {
    case JsonNumber(value) => Right(value.doubleValue)
    case JsonNull          => Right(Double.NaN)
    case JsonString(value) => value.toDoubleOption.toRight(new RuntimeException("Double"))
    case _                 => Left(new RuntimeException("Double"))
  }

  final implicit val jsonBigIntRead: JsonReader[BigInt] = {
    case JsonNumber(value) => Right(value.toBigInt)
    case JsonString(value) => Try(BigInt(value)).toEither
    case _                 => Left(new RuntimeException("BigInt"))
  }

  final implicit val jsonBigDecimalRead: JsonReader[BigDecimal] = {
    case JsonNumber(value) => Right(value)
    case JsonString(value) => Try(BigDecimal(value)).toEither
    case _                 => Left(new RuntimeException("BigDecimal"))
  }

  final implicit def jsonArrayRead[A: ClassTag](implicit reader: JsonReader[A]): JsonReader[Array[A]] = {
    case JsonArray(values) =>
      values
        .map(reader.read)
        .foldLeft(Right(Vector()): Either[Throwable, Vector[A]]) { (acc, el) =>
          for {
            a <- acc
            e <- el
          } yield a.appended(e)
        }
        .map(_.toArray)
    case _ => Left(new RuntimeException("Array"))
  }

  final implicit def jsonListRead[A: ClassTag](implicit reader: JsonReader[A]): JsonReader[List[A]] = jsonArrayRead[A].map(_.toList)

  final implicit def jsonMapRead[A](implicit reader: JsonReader[A]): JsonReader[Map[String, A]] = {
    case JsonObj(value) =>
      value.map {
        case (k, v) => reader.read(v).map(r => (k, r))
      }.foldLeft(Right(Map()): Either[Throwable, Map[String, A]]) { (acc, el) =>
        for {
          a <- acc
          e <- el
        } yield a.+(e)
      }
    case _ => Left(new RuntimeException("Map"))
  }
}
