package jsonserde

import Encoder._
import EncoderOps._

class EncoderSpec extends BaseSpec {
  "encoder" should {
    "encode json" in {
      val json: Json = JsonString("value")
      assertResult(json)(encode(json))
    }

    "encode unit" in {
      assertResult(JsonObj(Map.empty))(encode(()))
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
      assertResult(JsonByte(1))(encode(1.toByte))
    }

    "encode short" in {
      assertResult(JsonShort(1))(encode(1.toShort))
    }

    "encode int" in {
      assertResult(JsonInt(1))(encode(1))
    }

    "encode long" in {
      assertResult(JsonLong(1))(encode(1L))
    }

    "encode float" in {
      assertResult(JsonFloat(1))(encode(1.0f))
    }

    "encode double" in {
      assertResult(JsonDouble(1))(encode(1.0d))
    }

    "encode bigint" in {
      assertResult(JsonBigInt(1))(encode(BigInt(1)))
    }

    "encode bigdecimal" in {
      assertResult(JsonBigDecimal(1))(encode(BigDecimal(1)))
    }

    "encode array" in {
      assertResult(JsonArray(Vector(JsonInt(1), JsonInt(2), JsonInt(3), JsonInt(4), JsonInt(5))))(encode(Array(1, 2, 3, 4, 5)))
    }

    "encode map" in {
      assertResult(
        JsonObj(
          Map(
            "f1" -> JsonString("v1"),
            "f2" -> JsonString("v2"),
            "f3" -> JsonString("v3")
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
      assertResult(JsonString("value"))(encode(Some("value")))
      assertResult(JsonNull)(encode(None))
    }
  }
}
