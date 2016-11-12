package slate.storage

import org.atnos.eff._
import Eff._
import cats.data.State
import syntax.all._

class StorageTests extends StorageTestSuite {

  import StorageProgram._
  import StorageAction._

  "pure interpreter" - {
    "get key" in {
      val getKey =
        Get("key").run(PureStorage)
      getKey.run(Map.empty).value._2 shouldBe None
      getKey.run(Map("key" -> "v")).value._2 shouldBe Some("v")
    }
    "update key" in {
      val updateKey =
        Update("key", "value").run(PureStorage)
      updateKey.run(Map.empty).value._1 shouldBe Map("key" -> "value")
      updateKey.run(Map("key" -> "v")).value._1 shouldBe Map("key" -> "value")
    }
    "remove key" in {
      val removeKey =
        Remove("key").run(PureStorage)
      removeKey.run(Map.empty).value._1 shouldBe Map.empty
      removeKey.run(Map("key" -> "v")).value._1 shouldBe Map.empty
    }
    "getOrSet" in {
      val getOrSetKey =
        runProgram(PureStorage, getOrSet("key", "value")).detach
      getOrSetKey.run(Map.empty).value._1 shouldBe Map("key" -> "value")
      getOrSetKey.run(Map("key" -> "v")).value._1 shouldBe Map("key" -> "v")
    }
  }

}
