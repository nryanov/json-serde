package jsonserde

import Decoder._
import DecoderOps._
import shapeless.LabelledGeneric.Aux
import shapeless.labelled.FieldType

class DecoderSpec extends BaseSpec {
  "decoder" should {
    "decode json" in {
      val json: Json = JsonString("value")
      assert(decode[Json](json).contains(json))
    }

    "decode unit" in {
      assert(decode[Unit](JsonObj(List.empty)).contains(()))
    }

    "decode string" in {
      assert(decode[String](JsonString("value")).contains("value"))
    }

    "decode boolean" in {
      assert(decode[Boolean](JsonBoolean(true)).contains(true))
    }

    "decode char" in {
      assert(decode[Char](JsonString("a")).contains('a'))
    }

    "decode byte" in {
      assert(decode[Byte](JNumber(JsonBigDecimal(BigDecimal(1.toByte)))).contains(1.toByte))
    }

    "decode short" in {
      assert(decode[Short](JNumber(JsonBigDecimal(BigDecimal(1.toShort)))).contains(1.toShort))
    }

    "decode int" in {
      assert(decode[Int](JNumber(JsonBigDecimal(BigDecimal(1)))).contains(1))
    }

    "decode long" in {
      assert(decode[Long](JNumber(JsonBigDecimal(BigDecimal(1L)))).contains(1L))
    }

    "decode float" in {
      assert(decode[Float](JNumber(JsonBigDecimal(BigDecimal(1.0f)))).contains(1.0f))
    }

    "decode double" in {
      assert(decode[Double](JNumber(JsonBigDecimal(BigDecimal(1.0d)))).contains(1.0d))
    }

    "decode bigint" in {
      assert(decode[BigInt](JNumber(JsonBigDecimal(BigDecimal(1)))).contains(BigInt(1)))
    }

    "decode big decimal" in {
      assert(decode[Byte](JNumber(JsonBigDecimal(BigDecimal(1.toByte)))).contains(BigDecimal(1)))
    }

    "decode option" in {
      assert(decode[Option[String]](JsonString("value")).contains(Some("value")))
      assert(decode[Option[String]](JsonNull).contains(None))
    }

    "decode array" in {
      assert(
        decode[Array[String]](JsonArray(Vector(JsonString("1"), JsonString("2"), JsonString("3"))))
          .map(_.toSeq)
          .contains(Seq("1", "2", "3"))
      )
    }

    "decode map" in {
      assert(
        decode[Map[String, Int]](
          JsonObj(
            List(
              ("f1", JNumber(JsonBigDecimal(1)))
            )
          )
        ).contains(Map("f1" -> 1))
      )
    }

    "decode case class" in {
      case class A(f1: String, f2: Int, f3: List[String])

      val json = JsonObj(
        List(
          ("f1", JsonString("1")),
          ("f2", JNumber(JsonInt(2))),
          ("f3", JsonArray(Vector(JsonString("3"))))
        )
      )

      val jsonNull = JsonNull

      assert(decode[A](json).contains(A("1", 2, List("3"))))
      assert(decode[Option[A]](jsonNull).contains(None))
    }

    "decode case class with options" in {
      case class B(f1: String, f2: Option[Int], f3: List[String])

      val json = JsonObj(
        List(
          ("f1", JsonString("1")),
          ("f2", JNumber(JsonInt(2))),
          ("f3", JsonArray(Vector(JsonString("3"))))
        )
      )

      val jsonWithNull = JsonObj(
        List(
          ("f1", JsonString("1")),
          ("f2", JsonNull),
          ("f3", JsonArray(Vector(JsonString("3"))))
        )
      )

      val jsonWithoutField = JsonObj(
        List(
          ("f1", JsonString("1")),
          ("f3", JsonArray(Vector(JsonString("3"))))
        )
      )

      assert(decode[Option[B]](json).contains(Some(B("1", Some(2), List("3")))))
      assert(decode[B](jsonWithNull).contains(B("1", None, List("3"))))
      assert(decode[B](jsonWithoutField).contains(B("1", None, List("3"))))
      assert(decode[Option[B]](jsonWithoutField).contains(Some(B("1", None, List("3")))))
    }
  }
}
