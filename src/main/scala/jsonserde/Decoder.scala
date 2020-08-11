package jsonserde

import shapeless._
import shapeless.labelled.{FieldType, field}

trait Decoder[A] extends Serializable { self =>
  def decode(json: Json): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (json: Json) => self.decode(json).map(f)
}

object Decoder extends DecoderLowPriorityInstances {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  implicit def fromJsonReader[A](implicit reader: JsonReader[A]): Decoder[A] = {
    case null     => Left(new RuntimeException("Non nullable field does not exist"))
    case JsonNull => Left(new RuntimeException("Non nullable field is null"))
    case json     => reader.read(json)
  }

  implicit def fromJsonReaderOption[A](implicit reader: JsonReader[A]): Decoder[Option[A]] = {
    case null     => Right(None)
    case JsonNull => Right(None)
    case json     => reader.read(json).map(Some(_))
  }
}

trait DecoderLowPriorityInstances extends DecoderLowestPriorityInstances {
  final implicit def jsonGenericDecoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hDecoder: Lazy[Decoder[H]]
  ): Decoder[A] = json => hDecoder.value.decode(json).map(gen.from)

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
          val head: Either[Throwable, H] = hDecoder.value.decode(jsonField.orNull)
          val tail: Either[Throwable, T] = tDecoder.value.decode(json)

          for {
            h <- head
            t <- tail
          } yield field[K](h) :: t
        case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
      }
    }
  }
}

trait DecoderLowestPriorityInstances {
  final implicit def jsonOptionGenericDecoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hDecoder: Lazy[Decoder[Option[H]]]
  ): Decoder[Option[A]] = {
    case JsonNull => Right(None)
    case json     => hDecoder.value.decode(json).map(_.map(gen.from))
  }

  final implicit val hnilOptionDecoder: Decoder[Option[HNil]] = _ => Right(Some(HNil))

  implicit def hlistOptionDecoder1[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[Option[H]]],
    tDecoder: Lazy[Decoder[Option[T]]],
    N: H <:!< Option[α] forSome { type α }
  ): Decoder[Option[FieldType[K, H] :: T]] = {
    val fieldName = witness.value.name

    json => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }
          val head: Either[Throwable, Option[H]] = hDecoder.value.decode(jsonField.orNull)
          val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json)
          head.flatMap(headOption => tail.map(tailOption => tailOption.flatMap(tt => headOption.map(field[K](_) :: tt))))
        case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
      }
    }
  }

  final implicit def hlistOptionDecoder2[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[Option[H]]],
    tDecoder: Lazy[Decoder[Option[T]]]
  ): Decoder[Option[FieldType[K, Option[H]] :: T]] = {
    val fieldName = witness.value.name

    json => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }
          val head: Either[Throwable, Option[H]] = hDecoder.value.decode(jsonField.orNull)
          val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json)
          head.flatMap(headOption => tail.map(_.map(field[K](headOption) :: _)))
        case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
      }
    }
  }
}
