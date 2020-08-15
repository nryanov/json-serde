package jsonserde

import FieldNameSelector._

class FieldNameSelectorSpec extends BaseSpec {
  "field name selector" should {
    "return custom field names if possible" in {
      case class C(@FieldName("customField1") f1: String, @FieldName("customField2") f2: String, f3: String)

      val selector: FieldNameSelector[C] = implicitly[FieldNameSelector[C]]

      assertResult("customField1")(selector("f1"))
      assertResult("customField2")(selector("f2"))
      assertResult("f3")(selector("f3"))
    }
  }
}
