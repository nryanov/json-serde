package jsonserde

import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] extends Serializable { self =>
  def encode(value: A): Json

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.encode(f(value))
}

private[jsonserde] trait EncoderWithCustomFieldName[A, B] extends Serializable {
  def encode(value: A, fieldName: B): Json
}

object Encoder extends EncoderLowPriorityInstances {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  implicit def fromJsonWriter[A](implicit writer: JsonWriter[A]): Encoder[A] = writer.write

  implicit def fromJsonWriterOption[A](implicit writer: JsonWriter[A]): Encoder[Option[A]] = {
    case Some(value) => writer.write(value)
    case None        => JsonNull
  }

  implicit def fromJsonWriterWithCustomFieldName[A, B <: Option[FieldName]](
    implicit writer: JsonWriter[A]
  ): EncoderWithCustomFieldName[A, B] =
    (value, _) => writer.write(value)

  implicit def fromJsonWriterOptionWithCustomFieldName[A, B <: Option[FieldName]](
    implicit writer: JsonWriter[A]
  ): EncoderWithCustomFieldName[Option[A], B] = {
    case (Some(value), _) => writer.write(value)
    case (None, _)        => JsonNull
  }
}

trait EncoderLowPriorityInstances extends EncoderLowestPriorityInstances {
  final implicit def genericFamilyEncoder[A, H <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Encoder[H]]
  ): Encoder[A] = value => hEncoder.value.encode(gen.to(value))

  final implicit val cnilEncoder: Encoder[CNil] = _ => JsonNull

  final implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
    implicit hEncoder: Lazy[Encoder[H]],
    tEncoder: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :+: T] = {
    case Inl(head) => hEncoder.value.encode(head)
    case Inr(tail) => tEncoder.value.encode(tail)
  }

  final implicit def genericEncoder[A, H <: HList, FH <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    annotations: Annotations.Aux[FieldName, A, FH],
    hEncoder: Lazy[EncoderWithCustomFieldName[H, FH]]
  ): Encoder[A] = (value: A) => hEncoder.value.encode(gen.to(value), annotations())

  final implicit val hnilEncoder: EncoderWithCustomFieldName[HNil, HNil] = (_, _) => JsonNull

  final implicit def hlistEncoder[K <: Symbol, H, T <: HList, FH <: Option[FieldName], FT <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[EncoderWithCustomFieldName[H, FH]],
    tEncoder: Lazy[EncoderWithCustomFieldName[T, FT]]
  ): EncoderWithCustomFieldName[FieldType[K, H] :: T, FH :: FT] = { (hlist, fieldNames) =>
    val head = hEncoder.value.encode(hlist.head, fieldNames.head)
    val tail = tEncoder.value.encode(hlist.tail, fieldNames.tail)

    val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

    tail match {
      case JsonObj(fields) => JsonObj(List((fieldName, head)) ::: fields)
      case _               => JsonObj(List((fieldName, head)))
    }
  }
}

trait EncoderLowestPriorityInstances {
  final implicit def genericFamilyOptionEncoder[A, H <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Encoder[Option[H]]]
  ): Encoder[Option[A]] = {
    case value @ Some(_) => hEncoder.value.encode(value.map(gen.to))
    case None            => JsonNull
  }

  final implicit val cnilOptionEncoder: Encoder[Option[CNil]] = _ => JsonNull

  final implicit def coproductOptionEncoder[K <: Symbol, H, T <: Coproduct](
    implicit hEncoder: Lazy[Encoder[H]],
    tEncoder: Lazy[Encoder[T]],
    N: H <:!< Option[α] forSome { type α }
  ): Encoder[Option[FieldType[K, H] :+: T]] = coproduct => {
    coproduct.fold(JsonNull: Json) {
      case Inl(head) => hEncoder.value.encode(head)
      case Inr(tail) => tEncoder.value.encode(tail)
    }
  }

  implicit def genericOptionEncoder[A, Repr <: HList, FH <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    annotations: Annotations.Aux[FieldName, A, FH],
    hEncoder: Lazy[EncoderWithCustomFieldName[Option[Repr], FH]]
  ): Encoder[Option[A]] = {
    case value @ Some(_) => hEncoder.value.encode(value.map(gen.to), annotations())
    case None            => JsonNull
  }

  implicit val hnilOptionEncoder: EncoderWithCustomFieldName[Option[HNil], HNil] = (_, _) => JsonNull

  implicit def hlistOptionEncoder1[K <: Symbol, H, T <: HList, FT <: Option[FieldName], FH <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[EncoderWithCustomFieldName[Option[H], FT]],
    tEncoder: Lazy[EncoderWithCustomFieldName[Option[T], FH]],
    N: H <:!< Option[α] forSome { type α }
  ): EncoderWithCustomFieldName[Option[FieldType[K, H] :: T], FT :: FH] = {
    def split[A](v: Option[H :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(Some(h), Some(t)) })

    (hlist, fieldNames) => {
      val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head, fieldNames.head)
          tEncoder.value.encode(tail, fieldNames.tail) match {
            case JsonObj(fields) => JsonObj(List((fieldName, encodedHead)) ::: fields)
            case _               => JsonObj(List((fieldName, encodedHead)))
          }
      }
    }
  }

  implicit def hlistOptionEncoder2[K <: Symbol, H, T <: HList, FT <: Option[FieldName], FH <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[EncoderWithCustomFieldName[Option[H], FT]],
    tEncoder: Lazy[EncoderWithCustomFieldName[Option[T], FH]]
  ): EncoderWithCustomFieldName[Option[FieldType[K, Option[H]] :: T], FT :: FH] = {
    def split[A](v: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(h, Some(t)) })

    (hlist, fieldNames) => {
      val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head, fieldNames.head)
          tEncoder.value.encode(tail, fieldNames.tail) match {
            case JsonObj(fields) => JsonObj(List((fieldName, encodedHead)) ::: fields)
            case _               => JsonObj(List((fieldName, encodedHead)))
          }
      }
    }
  }
}
