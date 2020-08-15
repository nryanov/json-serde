package jsonserde

import shapeless._
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.record._

trait FieldNameSelector[A] {
  def apply(field: String): String
}

object FieldNameSelector {
  implicit def genericFieldNameSelector[A, ARepr <: HList, FieldNamesRepr <: HList, KeysRepr <: HList](
    implicit gen: LabelledGeneric.Aux[A, ARepr],
    keys: Keys.Aux[ARepr, KeysRepr],
    keysToTraversable: ToTraversable.Aux[KeysRepr, List, Symbol],
    annotations: Annotations.Aux[FieldName, A, FieldNamesRepr],
    annotationsToTraversable: ToTraversable.Aux[FieldNamesRepr, List, Option[FieldName]]
  ): FieldNameSelector[A] = {
    val fieldNameToCustomFieldName: Map[String, Option[FieldName]] =
      keys().toList.map(_.name).zip(annotations().toList).toMap

    new FieldNameSelector[A] {
      override def apply(field: String): String =
        fieldNameToCustomFieldName.get(field).flatten.fold(field)(_.value)
    }
  }
}
