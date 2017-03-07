package slate
package storage

import cats.Monad
import cats.data.State
import org.scalatest.Assertion
import slate.storage.StorageFS._

class StorageFSTests extends SlateSuite {

  final class ResettableNonceSource extends (() => String) {
    var i = 0

    def reset(): Unit =
      i = 0

    override def apply(): String = {
      val t = i
      i += 1
      t.toString
    }
  }

  def makeDetNonceSource: ResettableNonceSource = new ResettableNonceSource

  final val detNonceSource: ResettableNonceSource = makeDetNonceSource

  final val storage: Storage[StringMapState] = PureStorage

  final val storageFS: Storage[StringMapState] =
    new StorageFS[StringMapState](storage, detNonceSource, StorageFS.fsroot)

  val initializedFS: Map[String, String] =
    StorageFS.initFS[StringMapState](None)(Monad[StringMapState], storage).run(Map.empty).value._1

  class Fixture {
    detNonceSource.reset()
  }

  class UnderlyingStorageFixture extends Fixture {
    implicit final val s: Storage[StringMapState] = storage
  }

  class StorageFSFixture extends Fixture {
    implicit final val s: Storage[StringMapState] = storageFS
  }

  "init should create root folder" in new UnderlyingStorageFixture {
    val out =
      StorageFS.getDir[StringMapState](StorageFS.fsroot).run(initializedFS).value
    assert(out._2.value.isEmpty)
  }

  "mkDir" - {

    "should return a key to a dir that exists" in new UnderlyingStorageFixture {
      val dirKey = Key[Dir]("dir", "0")
      val addDir = for {
        _ <- StorageFS.mkDir[StringMapState, Assertion](
          "dir", detNonceSource, StorageFS.fsroot,
          alreadyPresent = (_, _) => fail("dir did not exist"), dirMade = (_, k) => k shouldBe dirKey
        )
        addedDir <- StorageFS.getDir[StringMapState](dirKey)
      } yield assert(addedDir.value.isEmpty)
      addDir.run(initializedFS).value
    }

    "should not overwrite an existing dir" in new UnderlyingStorageFixture {
      val dirKey = Key[Dir]("dir", "0")
      val nestedKey = Key[Dir]("nested", "1")
      val addDir = for {
        firstKey <- mkDir[StringMapState, DirKey](
          "dir", detNonceSource, fsroot,
          alreadyPresent = (_, _) => fail("dir folder already existed"),
          dirMade = { (_, k) => k shouldBe dirKey; k }
        )
        firstKeyValue = firstKey.value
        nestedKey <- mkDir[StringMapState, DirKey](
          "nested", detNonceSource, firstKey.value,
          alreadyPresent = (_, _) => fail("nested dir already existed"),
          dirMade = { (_, k) => k shouldBe nestedKey; k }
        )
        nestedKeyValue = nestedKey.value
        secondKey <- mkDir[StringMapState, DirKey](
          "dir", detNonceSource, fsroot,
          alreadyPresent = { (_, k) => k shouldBe dirKey; k },
          dirMade = (_, _) => fail("dir didn't exist or was overwritten")
        )
        secondKeyValue = secondKey.value
        nestedDir <- getDir[StringMapState](nestedKeyValue)
        _ = nestedDir.value.isEmpty shouldBe true
      } yield (firstKeyValue, nestedKeyValue, secondKeyValue)
      addDir.run(initializedFS).value
    }

  }

  "file data" - {
    "update/get" in new UnderlyingStorageFixture {
      val prog = for {
        _ <- updateFileInDir[StringMapState]("f", detNonceSource, "datas", fsroot)
        d <- getFileInDir[StringMapState]("f", fsroot)
        fFileInRoot <- getFileInDir[StringMapState]("f", fsroot)
        _ = fFileInRoot.value shouldBe "datas"
        _ = d.value shouldBe "datas"
      } yield ()
      prog.run(initializedFS).value
    }

    "update/get/remove" in new UnderlyingStorageFixture {
      val prog = for {
        fKey <- updateFileInDir[StringMapState]("f", detNonceSource, "datas", fsroot)
        fFile <- getFile[StringMapState](fKey.value)
        _ = fFile.value.data shouldBe "datas"
        _ = println("bout to remove file")
        _ <- removeFile[StringMapState, Unit](
          "f", fsroot,
          wasNotPresent = fail("file f was not present when removed"),
          wasPresent = ()
        )(Monad[StringMapState], storage)
        _ = println("bout to remove file")
        fFileRemoved <- getFile[StringMapState](fKey.value)(Monad[StringMapState], storage)
        _ = fFileRemoved shouldBe None
      } yield ()

      prog.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

  "storage wrapper" - {

    "update/get" in new StorageFSFixture {
      val prog: StringMapState[Unit] = for {
        _ <- storageFS.update("key", "value")
        v <- storageFS("key")
        _ = v shouldBe Some("value")
      } yield ()
      prog.run(initializedFS).value
    }

    "update/get/remove" in new StorageFSFixture {
      val prog: StringMapState[Unit] = for {
        _ <- storageFS.update("key", "value")
        v <- storageFS("key")
        _ = v shouldBe Some("value")
        _ <- storageFS.remove("key")
        vNew <- storageFS("key")
        _ = vNew shouldBe None
      } yield ()
      prog.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

}
