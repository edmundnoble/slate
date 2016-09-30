package qq

import qq.data.JSON

import scala.scalajs.js
import org.scalactic.NormMethods._

class JsonTests extends QQSyncTestSuite {
  "strings" in {
    Json.jsToJSONRec("hey") shouldBe JSON.Str("hey")
  }
  "numbers" in {
    Json.jsToJSONRec(1.0) shouldBe JSON.Num(1.0)
  }
  "dicts" in {
    Json.jsToJSONRec(js.Dictionary[Any]("wat" -> 1.0)).norm shouldBe JSON.Obj("wat" -> JSON.Num(1.0))
  }
  "arrays" in {
    Json.jsToJSONRec(js.Array[Any]("wat", 1.0)) shouldBe JSON.Arr(JSON.Str("wat"), JSON.Num(1.0))
  }
}
