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
      assert(decode[Unit](JsonObj(Map.empty)).contains(()))
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
            Map(
              "f1" -> JNumber(JsonBigDecimal(1))
            )
          )
        ).contains(Map("f1" -> 1))
      )
    }
  }
}
