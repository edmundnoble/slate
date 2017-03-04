package slate
package storage

import cats.data.{State, Writer}
import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import slate.storage.StorageFS.{Dir, Key}

class StorageFSTests extends SlateSuite {
  def logStorageProgram[A](prog: StorageProgram[A]): StorageProgram[A] = {
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction]]
    writer.runWriterUnsafe[Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction], Vector[String], A](
      StorageProgram.logActions[Fx.fx1[StorageAction], Fx.fx2[StorageAction, Writer[Vector[String], ?]], NoFx, A](prog)
    )(println)(implicitly[mem])
  }

  implicit final class MkDirResultOps(result: StorageFS.MkDirResult) {
    @inline def assertDirMade: Key[Dir] = result match {
      case StorageFS.DirMade(k) => k
      case StorageFS.AlreadyPresent(_) => assert(false, "dir was already present before being made").asInstanceOf[Key[Dir]]
    }
    @inline def assertAlreadyPresent: Key[Dir] = result match {
      case StorageFS.DirMade(_) => assert(false, "dir was not already present before being made").asInstanceOf[Key[Dir]]
      case StorageFS.AlreadyPresent(k) => k
    }
  }

  val initializedFS: Map[String, String] =
    StorageProgram.runProgram(PureStorage, StorageFS.initFS).detach.run(Map.empty).value._1

  def makeDetNonceSource: () => String = new (() => String) {
    var i = 0
    override def apply(): String = {
      val t = i
      i += 1
      t.toString
    }
  }

  "init" in {
    val outDir =
      StorageProgram.runProgram(PureStorage, StorageFS.getDir(StorageFS.fsroot))
        .detach.run(initializedFS).value._2.value
    outDir.isEmpty shouldBe true
  }

  "mkDir" - {

    "should not overwrite" in {
      val detNonceSource = makeDetNonceSource
      val addDir = for {
        firstKey <- StorageFS.mkDir("dir", detNonceSource, StorageFS.fsroot)
        firstKeyValue = firstKey.value
        nestedKey <- StorageFS.mkDir("nested", detNonceSource, firstKeyValue.assertDirMade)
        nestedKeyValue = nestedKey.value
        secondKey <- StorageFS.mkDir("dir", detNonceSource, StorageFS.fsroot)
        nestedDir <- StorageFS.getDir(nestedKeyValue.assertDirMade)
        _ = nestedDir.value.isEmpty shouldBe true
      } yield (firstKeyValue, nestedKeyValue, secondKey.value)
      val dirKey = StorageFS.Key[Dir]("dir", "0")
      val nestedKey = StorageFS.Key[Dir]("nested", "1")
      StorageProgram.runProgram(PureStorage, addDir)
        .detach.run(initializedFS).value._2 shouldBe ((StorageFS.DirMade(dirKey), StorageFS.DirMade(nestedKey), StorageFS.AlreadyPresent(dirKey)))
    }

    "should return a key to a dir that exists" in {
      val detNonceSource = makeDetNonceSource
      val addDir = for {
        newDirKey <- StorageFS.mkDir("dir", detNonceSource, StorageFS.fsroot)
        addedDir <- StorageFS.getDir(newDirKey.value.assertDirMade)
      } yield addedDir
      val result = StorageProgram.runProgram(PureStorage, addDir)
        .detach.run(initializedFS).value._2.value

      result.isEmpty shouldBe true
    }

  }

  "file data" - {
    class Fixture {
      val nonceSource: () => String = makeDetNonceSource
    }

    "update/get" in new Fixture {
      val prog = for {
        _ <- StorageFS.updateFileInDir("f", nonceSource, "datas", StorageFS.fsroot)
        d <- StorageFS.getFileInDir("f", StorageFS.fsroot)
        _ = d.value shouldBe "datas"
      } yield ()
      StorageProgram.runProgram(PureStorage, prog).detach.run(initializedFS).value
    }

    "update/get/remove" in new Fixture {
      val prog = for {
        k <- StorageFS.updateFileInDir("f", nonceSource, "datas", StorageFS.fsroot)
        f <- traverseA(k)(StorageFS.getFile[Fx.fx1[StorageAction]])
        d <- traverseA(f.flatten)(_ => StorageFS.getFileInDir("f", StorageFS.fsroot))
        _ = d.flatten.value shouldBe "datas"
        _ <- StorageFS.removeFile("f", StorageFS.fsroot)
      } yield ()
      StorageProgram.runProgram(PureStorage, prog).detach.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

  "storage wrapper" - {
    class Fixture {
      val storageFS: StorageFS[State[Map[String, String], ?]] =
        new StorageFS[State[Map[String, String], ?]](PureStorage, makeDetNonceSource, StorageFS.fsroot)
    }

    "update/get" in new Fixture {
      val prog: StorageProgram[Unit] = for {
        _ <- StorageProgram.update("key", "value")
        v <- StorageProgram.get("key")
        _ = v.value shouldBe "value"
      } yield ()
      StorageProgram.runProgram(storageFS, prog).detach.run(initializedFS).value
    }

    "update/get/remove" in new Fixture {
      val prog: StorageProgram[Unit] = for {
        _ <- StorageProgram.update("key", "value")
        v <- StorageProgram.get("key")
        _ = v.value shouldBe "value"
        _ <- StorageProgram.remove("key")
      } yield ()
      StorageProgram.runProgram(storageFS, prog).detach.run(initializedFS).value._1 shouldBe initializedFS
    }
  }

}
