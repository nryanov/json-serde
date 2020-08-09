package jsonserde

class JsonVisitorSpec extends BaseSpec {
  "json visitor" should {
    "return JsonNull" in {
      assert(JsonOps.parse("null").contains(JsonNull))
    }

    "return JsonString" in {
      assert(JsonOps.parse("\"value\"").contains(JsonString("value")))
    }

    "return JsonNumber" in {
      assert(JsonOps.parse("123").contains(JNumber(JsonBigDecimal(123))))
    }

    "return Boolean" in {
      assert(JsonOps.parse("true").contains(JsonBoolean(true)))
      assert(JsonOps.parse("false").contains(JsonBoolean(false)))
    }

    "return array" in {
      assert(
        JsonOps
          .parse("[1,2,3,4,5]")
          .contains(
            JsonArray(
              Vector(
                JNumber(JsonBigDecimal(1)),
                JNumber(JsonBigDecimal(2)),
                JNumber(JsonBigDecimal(3)),
                JNumber(JsonBigDecimal(4)),
                JNumber(JsonBigDecimal(5))
              )
            )
          )
      )
    }

    "return object" in {
      assert(
        JsonOps.parse("{\"key\": \"value\"}").contains(JsonObj(List(("key", JsonString("value")))))
      )
    }

    "return error" in {
      assert(JsonOps.parse("some incorrect[][][] json {} string").isLeft)
    }
  }
}
