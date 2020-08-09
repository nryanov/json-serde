package jsonserde

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonVisitorSpec extends AnyWordSpec with Matchers {
  "json visitor" should {
    "return JsonNull" in {
      assert(JsonOps.parse("'null'").contains(JsonNull))
    }

    "return JsonString" in {
      assert(JsonOps.parse("\"value\"").contains(JsonString("value")))
    }

    "return JsonNumber" in {
      assert(JsonOps.parse("123").contains(JsonNumber(123)))
    }

    "return Boolean" in {
      assert(JsonOps.parse("'true'").contains(JsonBoolean(true)))
      assert(JsonOps.parse("'false'").contains(JsonBoolean(false)))
    }

    "return array" in {
      assert(
        JsonOps
          .parse("[1,2,3,4,5]")
          .contains(
            JsonArray(
              Vector(
                JsonNumber(1),
                JsonNumber(2),
                JsonNumber(3),
                JsonNumber(4),
                JsonNumber(5)
              )
            )
          )
      )
    }

    "return object" in {
      assert(
        JsonOps
          .parse("{\"key\": \"value\"}")
          .contains(JsonObj(Map("key" -> JsonString("value"))))
      )
    }

    "return error" in {
      assert(JsonOps.parse("some incorrect[][][] json {} string").isLeft)
    }
  }
}
