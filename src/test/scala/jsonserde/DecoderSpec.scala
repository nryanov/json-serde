package jsonserde

import Decoder._
import DecoderOps._

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
      assert(decode[Byte](JsonNumber(BigDecimal(1.toByte))).contains(1.toByte))
    }

    "decode short" in {
      assert(decode[Short](JsonNumber(BigDecimal(1.toShort))).contains(1.toShort))
    }

    "decode int" in {
      assert(decode[Int](JsonNumber(BigDecimal(1))).contains(1))
    }

    "decode long" in {
      assert(decode[Long](JsonNumber(BigDecimal(1L))).contains(1L))
    }

    "decode float" in {
      assert(decode[Float](JsonNumber(BigDecimal(1.0f))).contains(1.0f))
    }

    "decode double" in {
      assert(decode[Double](JsonNumber(BigDecimal(1.0d))).contains(1.0d))
    }

    "decode bigint" in {
      assert(decode[BigInt](JsonNumber(BigDecimal(BigInt(1)))).contains(BigInt(1)))
    }

    "decode big decimal" in {
      assert(decode[Byte](JsonNumber(BigDecimal(1.toByte))).contains(BigDecimal(1)))
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
              ("f1", JsonNumber(1))
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
          ("f2", JsonNumber(2)),
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
          ("f2", JsonNumber(2)),
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

    "decode case class with default values" in {
      case class C(f1: String = "1", f2: Option[String] = Some("2"))

      val json = JsonObj(
        List(
          ("f1", JsonString("nonDefault1")),
          ("f2", JsonString("nonDefault2"))
        )
      )

      // we can return default value for non-nullable fields, but currently we'll return error
      val jsonWithoutNonOptionalField = JsonObj(
        List(
          ("f2", JsonString("nonDefault2"))
        )
      )

      val jsonWithoutOptionalField = JsonObj(
        List(
          ("f1", JsonString("nonDefault1"))
        )
      )

      assert(decode[C](json).contains(C("nonDefault1", Some("nonDefault2"))))
      assert(decode[C](jsonWithoutNonOptionalField).isLeft)
      assert(decode[C](jsonWithoutOptionalField).contains(C("nonDefault1")))
    }

    "decode case class with custom field names" in {
      case class C(@FieldName("customField1") f1: String, @FieldName("customField2") f2: Option[String])

      val json = JsonObj(
        List(
          ("customField1", JsonString("nonDefault1")),
          ("customField2", JsonString("nonDefault2"))
        )
      )

      assert(decode[C](json).contains(C("nonDefault1", Some("nonDefault2"))))
    }
  }
}
