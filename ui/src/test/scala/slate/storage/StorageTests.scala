package slate
package storage

import cats.data.State

class StorageTests extends SlateSuite {

  implicit val storage = PureStorage

  "pure interpreter" - {
    "get key" in {
      val getKey =
        storage("key")
      getKey.run(Map.empty).value._2 shouldBe None
      getKey.run(Map("key" -> "v")).value._2 shouldBe Some("v")
    }
    "update key" in {
      val updateKey =
        storage.update("key", "value")
      updateKey.run(Map.empty).value._1 shouldBe Map("key" -> "value")
      updateKey.run(Map("key" -> "v")).value._1 shouldBe Map("key" -> "value")
    }
    "remove key" in {
      val removeKey =
        storage.remove("key")
      removeKey.run(Map.empty).value._1 shouldBe Map.empty
      removeKey.run(Map("key" -> "v")).value._1 shouldBe Map.empty
    }
    "getOrSet" in {
      def getOrSet(key: String, data: String): StringMapState[String] =
        for {
          oldValue <- storage(key)
          newValue <- oldValue.fold(storage.update(key, data).map(_ => data))(State.pure)
        } yield newValue
      val getOrSetKey =
        getOrSet("key", "value")
      getOrSetKey.run(Map.empty).value._1 shouldBe Map("key" -> "value")
      getOrSetKey.run(Map("key" -> "v")).value._1 shouldBe Map("key" -> "v")
    }
  }

}
