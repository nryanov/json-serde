package jsonserde

import shapeless._
import shapeless.labelled.{FieldType, field}

trait Decoder[A] extends Serializable { self =>
  def decode(json: Json): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (json: Json) => self.decode(json).map(f)
}

private[jsonserde] trait DecoderWithDefaults[A, B] extends Serializable {
  def decode(json: Json, defaults: B): Either[Throwable, A]
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

  implicit def fromJsonReaderWithDefault[A](implicit reader: JsonReader[A]): DecoderWithDefaults[A, Option[A]] = {
    case (null, _)     => Left(new RuntimeException("Non nullable field does not exist"))
    case (JsonNull, _) => Left(new RuntimeException("Non nullable field is null"))
    case (json, _)     => reader.read(json)
  }

  implicit def fromJsonReaderOptionWithDefault1[A](implicit reader: JsonReader[A]): DecoderWithDefaults[Option[A], Option[Option[A]]] = {
    case (null, default)     => Right(default.flatten)
    case (JsonNull, default) => Right(default.flatten)
    case (json, _)           => reader.read(json).map(Some(_))
  }

  implicit def fromJsonReaderOptionWithDefault2[A](implicit reader: JsonReader[A]): DecoderWithDefaults[Option[A], Option[A]] = {
    case (null, default)     => Right(default)
    case (JsonNull, default) => Right(default)
    case (json, _)           => reader.read(json).map(Some(_))
  }
}

trait DecoderLowPriorityInstances extends DecoderLowestPriorityInstances {
  final implicit def jsonGenericDecoder[A, H <: HList, HD <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    defaults: Default.AsOptions.Aux[A, HD],
    hDecoder: Lazy[DecoderWithDefaults[H, HD]]
  ): Decoder[A] = json => hDecoder.value.decode(json, defaults()).map(gen.from)

  final implicit val hnilDecoder: DecoderWithDefaults[HNil, HNil] = (_, _) => Right(HNil)

  final implicit def hlistDecoder[K <: Symbol, H, T <: HList, TD <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[DecoderWithDefaults[H, Option[H]]],
    tDecoder: Lazy[DecoderWithDefaults[T, TD]]
  ): DecoderWithDefaults[FieldType[K, H] :: T, Option[H] :: TD] = {
    val fieldName = witness.value.name

    (json, defaults) => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }
          val head: Either[Throwable, H] = hDecoder.value.decode(jsonField.orNull, defaults.head)
          val tail: Either[Throwable, T] = tDecoder.value.decode(json, defaults.tail)

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
  final implicit def jsonOptionGenericDecoder[A, H <: HList, HD <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    defaults: Default.AsOptions.Aux[A, HD],
    hDecoder: Lazy[DecoderWithDefaults[Option[H], HD]]
  ): Decoder[Option[A]] = {
    case JsonNull => Right(None)
    case json     => hDecoder.value.decode(json, defaults()).map(_.map(gen.from))
  }

  final implicit val hnilOptionDecoder: DecoderWithDefaults[Option[HNil], HNil] = (_, _) => Right(Some(HNil))

  implicit def hlistOptionDecoder1[K <: Symbol, H, T <: HList, TD <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[DecoderWithDefaults[Option[H], Option[H]]],
    tDecoder: Lazy[DecoderWithDefaults[Option[T], TD]],
    N: H <:!< Option[α] forSome { type α }
  ): DecoderWithDefaults[Option[FieldType[K, H] :: T], Option[H] :: TD] = {
    val fieldName = witness.value.name

    (json, defaults) => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }

          val head: Either[Throwable, Option[H]] = hDecoder.value.decode(jsonField.orNull, defaults.head)
          val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json, defaults.tail)
          head.flatMap(headOption => tail.map(tailOption => tailOption.flatMap(tt => headOption.map(field[K](_) :: tt))))
        case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
      }
    }
  }

  final implicit def hlistOptionDecoder2[K <: Symbol, H, T <: HList, TD <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[DecoderWithDefaults[Option[H], Option[Option[H]]]],
    tDecoder: Lazy[DecoderWithDefaults[Option[T], TD]]
  ): DecoderWithDefaults[Option[FieldType[K, Option[H]] :: T], Option[Option[H]] :: TD] = {
    val fieldName = witness.value.name

    (json, defaults) => {
      json match {
        case JsonObj(fields) =>
          val jsonField = fields.collectFirst {
            case (str, json) if str == fieldName => json
          }

          val head: Either[Throwable, Option[H]] = hDecoder.value.decode(jsonField.orNull, defaults.head)
          val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json, defaults.tail)
          head.flatMap(headOption => tail.map(_.map(field[K](headOption) :: _)))
        case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
      }
    }
  }
}
