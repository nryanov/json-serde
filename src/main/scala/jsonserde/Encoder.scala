package jsonserde

import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] extends Serializable { self =>
  def encode(value: A): Json

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.encode(f(value))
}

private[jsonserde] trait EncoderWithCustomFieldName[A, B] extends Serializable {
  def encode(value: A, fieldName: B): JsonObj
}

object Encoder extends EncoderLowPriorityInstances {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  implicit val unitWriter: Encoder[Unit] = _ => JsonObj.EMPTY

  implicit def fromJsonWriter[A](implicit writer: JsonWriter[A]): Encoder[A] = writer.write

  implicit def fromJsonWriterOption[A](implicit writer: JsonWriter[A]): Encoder[Option[A]] = {
    case Some(value) => writer.write(value)
    case None        => JsonNull
  }
}

trait EncoderLowPriorityInstances extends EncoderLowestPriorityInstances {
//  implicit def optionalEncoder[A](implicit encoder: Encoder[A]): Encoder[Option[A]] = {
//    case Some(value) => encoder.encode(value)
//    case None        => JsonNull
//  }

  final implicit def genericFamilyEncoder[A, H <: Coproduct](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Encoder[H]],
    notOption: A <:!< Option[Z] forSome { type Z }
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

  final implicit val hnilEncoder: EncoderWithCustomFieldName[HNil, HNil] = (_, _) => JsonObj.EMPTY

  final implicit def hlistEncoder[K <: Symbol, H, T <: HList, FH <: Option[FieldName], FT <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: Lazy[EncoderWithCustomFieldName[T, FT]]
  ): EncoderWithCustomFieldName[FieldType[K, H] :: T, FH :: FT] = { (hlist, fieldNames) =>
    val head = hEncoder.value.encode(hlist.head)
    val tail = tEncoder.value.encode(hlist.tail, fieldNames.tail)
    val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

    JsonObj(List((fieldName, head)) ::: tail.fields)
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
    tEncoder: Lazy[Encoder[T]]
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

  implicit val hnilOptionEncoder: EncoderWithCustomFieldName[Option[HNil], HNil] = (_, _) => JsonObj.EMPTY

  implicit def hlistOptionEncoder1[K <: Symbol, H, T <: HList, FT <: Option[FieldName], FH <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[Option[H]]],
    tEncoder: Lazy[EncoderWithCustomFieldName[Option[T], FH]],
    notOption: H <:!< Option[Z] forSome { type Z }
  ): EncoderWithCustomFieldName[Option[FieldType[K, H] :: T], FT :: FH] = {
    def split[A](v: Option[H :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(Some(h), Some(t)) })

    (hlist, fieldNames) => {
      val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head)
          val encodedTail: JsonObj = tEncoder.value.encode(tail, fieldNames.tail)

          JsonObj(List((fieldName, encodedHead)) ::: encodedTail.fields)
      }
    }
  }

  implicit def hlistOptionEncoder2[K <: Symbol, H, T <: HList, FT <: Option[FieldName], FH <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[Option[H]]],
    tEncoder: Lazy[EncoderWithCustomFieldName[Option[T], FH]]
  ): EncoderWithCustomFieldName[Option[FieldType[K, Option[H]] :: T], FT :: FH] = {
    def split[A](v: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(h, Some(t)) })

    (hlist, fieldNames) => {
      val fieldName: String = fieldNames.head.map(_.value).getOrElse(witness.value.name)

      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head)
          val encodedTail: JsonObj = tEncoder.value.encode(tail, fieldNames.tail)

          JsonObj(List((fieldName, encodedHead)) ::: encodedTail.fields)
      }
    }
  }
}
