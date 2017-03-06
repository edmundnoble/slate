package slate
package storage

import cats.Monad
import cats.data.State
import cats.implicits._
import slate.storage.PureStorage.StringMapState
import slate.storage.StorageFS._

class StorageFSTests extends SlateSuite {
  val initializedFS: Map[String, String] =
    StorageFS.initFS(None)(Monad[StringMapState], PureStorage.Storage).run(Map.empty).value._1

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

  implicit final val storageFS: StorageFS[State[Map[String, String], ?]] =
    new StorageFS[State[Map[String, String], ?]](PureStorage.Storage, makeDetNonceSource, StorageFS.fsroot)

  class Fixture {
    detNonceSource.reset()
  }

  "init should create root folder" in new Fixture {
    val outDir =
      StorageFS.getDir(StorageFS.fsroot).run(initializedFS).value._2
    assert(outDir.value.isEmpty)
  }

  "mkDir" - {

    "should return a key to a dir that exists" in new Fixture {
      val dirKey = Key[Dir]("dir", "0")
      val addDir = for {
        _ <- StorageFS.mkDir(
          "dir", detNonceSource, StorageFS.fsroot,
          alreadyPresent = (_, _) => fail("dir did not exist"), dirMade = (_, k) => k shouldBe dirKey
        )
        addedDir <- StorageFS.getDir(dirKey)
      } yield assert(addedDir.value.isEmpty)
      addDir.run(initializedFS).value
    }

    "should not overwrite an existing dir" in new Fixture {
      val dirKey = Key[Dir]("dir", "0")
      val nestedKey = Key[Dir]("nested", "1")
      val addDir = for {
        firstKey <- mkDir(
          "dir", detNonceSource, fsroot,
          alreadyPresent = (_, _) => fail("dir folder already existed"),
          dirMade = { (_, k) => k shouldBe dirKey; k }
        )
        firstKeyValue = firstKey.value
        nestedKey <- mkDir(
          "nested", detNonceSource, firstKey.value,
          alreadyPresent = (_, _) => fail("nested dir already existed"),
          dirMade = { (_, k) => k shouldBe nestedKey; k }
        )
        nestedKeyValue = nestedKey.value
        secondKey <- mkDir(
          "dir", detNonceSource, fsroot,
          alreadyPresent = { (_, k) => k shouldBe dirKey; k },
          dirMade = (_, _) => fail("dir didn't exist or was overwritten")
        )
        secondKeyValue = secondKey.value
        nestedDir <- getDir(nestedKeyValue)
        _ = nestedDir.value.isEmpty shouldBe true
      } yield (firstKeyValue, nestedKeyValue, secondKeyValue)
      addDir.run(initializedFS).value
    }

  }

  "file data" - {
    "update/get" in new Fixture {
      val prog = for {
        _ <- updateFileInDir("f", detNonceSource, "datas", fsroot)
        d <- getFileInDir("f", fsroot)
        _ = d.value shouldBe "datas"
      } yield ()
      prog.run(initializedFS).value
    }

    "update/get/remove" in new Fixture {
      val prog = for {
        fKey <- updateFileInDir("f", detNonceSource, "datas", fsroot)
        fFile <- getFile(fKey.value)
        _ = fFile.value.data shouldBe "datas"
        fFileInRoot <- getFileInDir("f", fsroot)
        _ = fFileInRoot.flatten.value shouldBe "datas"
        _ <- removeFile(
          "f", fsroot,
          wasNotPresent = fail("file f was not present when removed"),
          wasPresent = ()
        )
        fFileRemoved <- getFile(fKey.value)
        _ = fFileRemoved shouldBe None
      } yield ()

      prog.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

  "storage wrapper" - {

    "update/get" in new Fixture {
      val prog: StringMapState[Unit] = for {
        _ <- storageFS.update("key", "value")
        v <- storageFS("key")
        _ = v shouldBe Some("value")
      } yield ()
      prog.run(initializedFS).value
    }

    "update/get/remove" in new Fixture {
      val prog: StringMapState[Unit] = for {
        _ <- storageFS.update("key", "value")
        v <- storageFS("key")
        _ = v shouldBe Some("value")
        _ <- storageFS.remove("key")
        _ = v shouldBe None
      } yield ()
      prog.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

}
