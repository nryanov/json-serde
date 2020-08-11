package jsonserde

import shapeless._
import shapeless.labelled.FieldType

trait Encoder[A] extends Serializable { self =>
  def encode(value: A): Json

  final def contramap[B](f: B => A): Encoder[B] = (value: B) => self.encode(f(value))
}

object Encoder extends EncoderLowPriorityInstances {
  def apply[A](implicit encoder: Encoder[A]): Encoder[A] = encoder

  implicit def fromJsonWriter[A](implicit writer: JsonWriter[A]): Encoder[A] = writer.write

  implicit def fromJsonWriterOption[A](implicit writer: JsonWriter[A]): Encoder[Option[A]] = {
    case Some(value) => writer.write(value)
    case None        => JsonNull
  }
}

trait EncoderLowPriorityInstances extends EncoderLowestPriorityInstances {
  final implicit def genericEncoder[A, H <: HList](
    implicit gen: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[Encoder[H]]
  ): Encoder[A] = (value: A) => hEncoder.value.encode(gen.to(value))

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

trait EncoderLowestPriorityInstances {
  implicit def genericOptionEncoder[A, Repr <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    hEncoder: Lazy[Encoder[Option[Repr]]]
  ): Encoder[Option[A]] = {
    case value @ Some(_) => hEncoder.value.encode(value.map(gen.to))
    case None            => JsonNull
  }

  implicit val hnilOptionEncoder: Encoder[Option[HNil]] = _ => JsonNull

  implicit def hlistOptionEncoder1[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[Option[H]]],
    tEncoder: Lazy[Encoder[Option[T]]],
    N: H <:!< Option[α] forSome { type α }
  ): Encoder[Option[FieldType[K, H] :: T]] = {
    def split[A](v: Option[H :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(Some(h), Some(t)) })

    val fieldName: String = witness.value.name

    hlist => {
      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head)
          tEncoder.value.encode(tail) match {
            case JsonObj(fields) => JsonObj(List((fieldName, encodedHead)) ::: fields)
            case _               => JsonObj(List((fieldName, encodedHead)))
          }
      }
    }
  }

  implicit def hlistOptionEncoder2[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hEncoder: Lazy[Encoder[Option[H]]],
    tEncoder: Lazy[Encoder[Option[T]]]
  ): Encoder[Option[FieldType[K, Option[H]] :: T]] = {
    def split[A](v: Option[Option[H] :: T])(f: (Option[H], Option[T]) => A): A = v.fold(f(None, None))({ case h :: t => f(h, Some(t)) })

    val fieldName: String = witness.value.name

    hlist => {
      split(hlist) {
        case (head, tail) =>
          val encodedHead: Json = hEncoder.value.encode(head)
          tEncoder.value.encode(tail) match {
            case JsonObj(fields) => JsonObj(List((fieldName, encodedHead)) ::: fields)
            case _               => JsonObj(List((fieldName, encodedHead)))
          }
      }
    }
  }
}
