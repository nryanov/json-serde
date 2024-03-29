package jsonserde

import shapeless._
import shapeless.labelled.{FieldType, field}

trait Decoder[A] extends Serializable { self =>
  def decode(json: Json): Either[Throwable, A]

  final def map[B](f: A => B): Decoder[B] = (json: Json) => self.decode(json).map(f)
}

private[jsonserde] trait DecoderWithMeta[A, B, C] extends Serializable {
  def decode(json: Json, defaults: B, fieldNames: C): Either[Throwable, A]
}

object Decoder extends DecoderLowPriorityInstances {
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder

  final implicit val unitReader: Decoder[Unit] = {
    case JsonObj(fields) if fields.isEmpty   => Right(())
    case JsonArray(values) if values.isEmpty => Right(())
    case JsonNull                            => Right(())
    case _                                   => Left(new RuntimeException("Unit"))
  }

  implicit def fromJsonReader[A](implicit reader: JsonReader[A]): Decoder[A] = {
    case null     => Left(new RuntimeException("Non nullable field does not exist"))
    case JsonNull => Left(new RuntimeException("Non nullable field is null"))
    case json     => reader.read(json)
  }

  implicit def optionalDecoder[A](implicit decoder: Decoder[A]): Decoder[Option[A]] = {
    case null     => Right(None)
    case JsonNull => Right(None)
    case json     => decoder.decode(json).map(Some(_))
  }

}

trait DecoderLowPriorityInstances {
  final implicit def jsonGenericFamilyDecoder[A, H <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, H],
    hDecoder: Lazy[Decoder[H]],
    notOption: A <:!< Option[Z] forSome { type Z }
  ): Decoder[A] = json => hDecoder.value.decode(json).map(gen.from)

  final implicit val cnilDecoder: Decoder[CNil] = _ => Left(new RuntimeException("Impossible"))

  final implicit def coproductGenericDecoder[K <: Symbol, H, T <: Coproduct](
    implicit hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[Decoder[T]]
  ): Decoder[FieldType[K, H] :+: T] = json => {
    hDecoder.value.decode(json).map(r => Inl(field[K](r))).orElse(tDecoder.value.decode(json).map(Inr(_)))
  }

  final implicit def jsonGenericDecoder[A, H <: HList, HD <: HList, FH <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    defaults: Default.AsOptions.Aux[A, HD],
    annotations: Annotations.Aux[FieldName, A, FH],
    hDecoder: Lazy[DecoderWithMeta[H, HD, FH]]
  ): Decoder[A] = json => hDecoder.value.decode(json, defaults(), annotations()).map(gen.from)

  final implicit val hnilDecoder: DecoderWithMeta[HNil, HNil, HNil] = (_, _, _) => Right(HNil)

  final implicit def hlistDecoder[K <: Symbol, H, T <: HList, TD <: HList, FH <: Option[FieldName], FT <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[DecoderWithMeta[T, TD, FT]]
  ): DecoderWithMeta[FieldType[K, H] :: T, Option[H] :: TD, FH :: FT] = { (json, defaults, fieldNames) =>
    val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

    json match {
      case JsonObj(fields) =>
        val jsonField = fields.collectFirst {
          case (str, json) if str == fieldName => json
        }

        jsonField.map(hDecoder.value.decode)
        val head: Either[Throwable, H] = jsonField match {
          case Some(value) => hDecoder.value.decode(value)
          case None =>
            defaults.head match {
              case Some(default) => Right(default)
              case None          => hDecoder.value.decode(null)
            }
        }
        val tail: Either[Throwable, T] = tDecoder.value.decode(json, defaults.tail, fieldNames.tail)

        for {
          h <- head
          t <- tail
        } yield field[K](h) :: t
      case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
    }
  }
}

/*
// custom decoder for optional types -- allow to return None instead of Left(error) if some fields are missing for Option[A]
trait DecoderLowestPriorityInstances {
  final implicit def jsonOptionGenericFamilyDecoder[A, H <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, H],
    hDecoder: Lazy[Decoder[Option[H]]]
  ): Decoder[Option[A]] = {
    case JsonNull => Right(None)
    case json     => hDecoder.value.decode(json).map(_.map(gen.from))
  }

  final implicit val cnilOptionDecoder: Decoder[Option[CNil]] = _ => Left(new RuntimeException("Impossible"))

  final implicit def coproductGenericOptionDecoder[K <: Symbol, H, T <: Coproduct](
    implicit hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[Decoder[T]]
  ): Decoder[Option[FieldType[K, H] :+: T]] = {
    case json @ JsonObj(_) =>
      hDecoder.value.decode(json).map(r => Inl(field[K](r))).orElse(tDecoder.value.decode(json).map(Inr(_))).map(Some(_))
    case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
  }

  final implicit def jsonOptionGenericDecoder[A, H <: HList, HD <: HList, FH <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    defaults: Default.AsOptions.Aux[A, HD],
    annotations: Annotations.Aux[FieldName, A, FH],
    hDecoder: Lazy[DecoderWithMeta[Option[H], HD, FH]]
  ): Decoder[Option[A]] = {
    case JsonNull => Right(None)
    case json     => hDecoder.value.decode(json, defaults(), annotations()).map(_.map(gen.from))
  }

  final implicit val hnilOptionDecoder: DecoderWithMeta[Option[HNil], HNil, HNil] = (_, _, _) => Right(Some(HNil))

  implicit def hlistOptionDecoder1[K <: Symbol, H, T <: HList, TD <: HList, FH <: Option[FieldName], FT <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[Option[H]]],
    tDecoder: Lazy[DecoderWithMeta[Option[T], TD, FT]],
    notOption: H <:!< Option[Z] forSome { type Z }
  ): DecoderWithMeta[Option[FieldType[K, H] :: T], Option[H] :: TD, FH :: FT] = { (json, defaults, fieldNames) =>
    val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

    json match {
      case JsonObj(fields) =>
        val jsonField = fields.collectFirst {
          case (str, json) if str == fieldName => json
        }

        // use default value only if key is missing
        val head: Either[Throwable, Option[H]] = jsonField match {
          case Some(value) => hDecoder.value.decode(value)
          case None        => Right(defaults.head)
        }
        val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json, defaults.tail, fieldNames.tail)
        head.flatMap(headOption => tail.map(tailOption => tailOption.flatMap(tt => headOption.map(field[K](_) :: tt))))
      case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
    }
  }

  final implicit def hlistOptionDecoder2[K <: Symbol, H, T <: HList, TD <: HList, FH <: Option[FieldName], FT <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[Option[H]]],
    tDecoder: Lazy[DecoderWithMeta[Option[T], TD, FT]]
  ): DecoderWithMeta[Option[FieldType[K, Option[H]] :: T], Option[Option[H]] :: TD, FH :: FT] = { (json, defaults, fieldNames) =>
    val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

    json match {
      case JsonObj(fields) =>
        val jsonField = fields.collectFirst {
          case (str, json) if str == fieldName => json
        }

        // use default value only if key is missing
        val head: Either[Throwable, Option[H]] = jsonField match {
          case Some(value) => hDecoder.value.decode(value)
          case None        => Right(defaults.head.flatten)
        }
        val tail: Either[Throwable, Option[T]] = tDecoder.value.decode(json, defaults.tail, fieldNames.tail)
        head.flatMap(headOption => tail.map(_.map(field[K](headOption) :: _)))
      case _ => Left(new RuntimeException("Incorrect data: expected JsonObject"))
    }
  }
}
 */
