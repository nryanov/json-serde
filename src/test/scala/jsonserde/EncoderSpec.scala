package jsonserde

import Encoder._
import jsonserde.ops.EncoderOps._

class EncoderSpec extends BaseSpec {
  "encoder" should {
    "encode json" in {
      val json: Json = JsonString("value")
      assertResult(json)(encode(json))
    }

    "encode unit" in {
      assertResult(JsonObj(List.empty))(encode(()))
    }

    "encode string" in {
      assertResult(JsonString("value"))(encode("value"))
    }

    "encode boolean" in {
      assertResult(JsonBoolean(true))(encode(true))
    }

    "encode char" in {
      assertResult(JsonString("c"))(encode('c'))
    }

    "encode byte" in {
      assertResult(JsonNumber(1))(encode(1.toByte))
    }

    "encode short" in {
      assertResult(JsonNumber(1))(encode(1.toShort))
    }

    "encode int" in {
      assertResult(JsonNumber(1))(encode(1))
    }

    "encode long" in {
      assertResult(JsonNumber(1))(encode(1L))
    }

    "encode float" in {
      assertResult(JsonNumber(1))(encode(1.0f))
    }

    "encode double" in {
      assertResult(JsonNumber(1))(encode(1.0d))
    }

    "encode bigint" in {
      assertResult(JsonNumber(1))(encode(BigInt(1)))
    }

    "encode bigdecimal" in {
      assertResult(JsonNumber(1))(encode(BigDecimal(1)))
    }

    "encode array" in {
      assertResult(JsonArray(Vector(JsonNumber(1), JsonNumber(2), JsonNumber(3), JsonNumber(4), JsonNumber(5))))(
        encode(Array(1, 2, 3, 4, 5))
      )
    }

    "encode map" in {
      assertResult(
        JsonObj(
          List(
            ("f1", JsonString("v1")),
            ("f2", JsonString("v2")),
            ("f3", JsonString("v3"))
          )
        )
      )(
        encode(
          Map(
            "f1" -> "v1",
            "f2" -> "v2",
            "f3" -> "v3"
          )
        )
      )
    }

    "encode option" in {
      val some: Option[String] = Some("value")
      val none: Option[String] = None

      assertResult(JsonString("value"))(encode(some))
      assertResult(JsonNull)(encode(none))
    }

    "encode case class without option fields" in {
      case class A(f1: Int, f2: String, f3: Array[Int])
      val a: A = A(1, "2", Array(3))
      val aSome: Option[A] = Some(A(1, "2", Array(3)))
      val aNone: Option[A] = None

      assertResult(
        JsonObj(
          List(
            ("f1", JsonNumber(1)),
            ("f2", JsonString("2")),
            ("f3", JsonArray(Vector(JsonNumber(3))))
          )
        )
      )(encode(a))
      assertResult(
        JsonObj(
          List(
            ("f1", JsonNumber(1)),
            ("f2", JsonString("2")),
            ("f3", JsonArray(Vector(JsonNumber(3))))
          )
        )
      )(encode(aSome))
      assertResult(JsonNull)(encode(aNone))
    }

    "encode case class with option fields" in {
      case class B(f1: Option[String], f2: Option[String])

      val b: B = B(Some("1"), Some("2"))
      val bSome: Option[B] = Some(B(None, Some("2")))
      val bNone: Option[B] = None

      assertResult(
        JsonObj(
          List(
            ("f1", JsonString("1")),
            ("f2", JsonString("2"))
          )
        )
      )(encode(b))
      assertResult(
        JsonObj(
          List(
            ("f1", JsonNull),
            ("f2", JsonString("2"))
          )
        )
      )(encode(bSome))
      assertResult(JsonNull)(encode(bNone))
    }

    "encode case class with custom field names" in {
      case class C1(@FieldName("customField1") f1: String, @FieldName("customField2") f2: String, f3: String)
      case class C2(
        @FieldName("customOptionField1") f1: Option[String],
        @FieldName("customOptionField2") f2: Option[String],
        f3: Option[String]
      )

      val c1: C1 = C1("1", "2", "3")
      val c2: C2 = C2(Some("1"), Some("2"), Some("3"))

      assertResult(
        JsonObj(
          List(
            ("customField1", JsonString("1")),
            ("customField2", JsonString("2")),
            ("f3", JsonString("3"))
          )
        )
      )(encode(c1))

      assertResult(
        JsonObj(
          List(
            ("customField1", JsonString("1")),
            ("customField2", JsonString("2")),
            ("f3", JsonString("3"))
          )
        )
      )(encode(Option(c1)))

      assertResult(
        JsonObj(
          List(
            ("customOptionField1", JsonString("1")),
            ("customOptionField2", JsonString("2")),
            ("f3", JsonString("3"))
          )
        )
      )(encode(c2))

      assertResult(
        JsonObj(
          List(
            ("customOptionField1", JsonString("1")),
            ("customOptionField2", JsonString("2")),
            ("f3", JsonString("3"))
          )
        )
      )(encode(Option(c2)))
    }

    "encode hierarchy" in {
      sealed trait A
      case class CC1(f1: String) extends A
      case class CC2(f2: String, f3: Option[String] = None) extends A

      val a1: A = CC1("1")
      val a2: A = CC2("2")

      assertResult(
        JsonObj(
          List(
            ("f1", JsonString("1"))
          )
        )
      )(encode(a1))

      assertResult(
        JsonObj(
          List(
            ("f2", JsonString("2")),
            ("f3", JsonNull)
          )
        )
      )(encode(a2))
    }

    "encode hierarchy (option)" in {
      sealed trait A
      case class CC1(f1: String) extends A
      case class CC2(f2: String, f3: Option[String] = None) extends A

      val a1: Option[A] = Some(CC1("1"))
      val a2: Option[A] = Some(CC2("2"))

      assertResult(
        JsonObj(
          List(
            ("f1", JsonString("1"))
          )
        )
      )(encode(a1))

      assertResult(
        JsonObj(
          List(
            ("f2", JsonString("2")),
            ("f3", JsonNull)
          )
        )
      )(encode(a2))
    }

    "encode nested case classes" in {
      case class CC1(f1: String, f2: String)
      case class CC2(f3: String, f4: CC1)

      val cc2: CC2 = CC2("3", CC1("1", "2"))

      assertResult(
        JsonObj(
          List(
            ("f3", JsonString("3")),
            (
              "f4",
              JsonObj(
                List(
                  ("f1", JsonString("1")),
                  ("f2", JsonString("2"))
                )
              )
            )
          )
        )
      )(encode(cc2))
    }
  }
}
