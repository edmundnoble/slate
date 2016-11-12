package slate
package storage

import org.atnos.eff._
import Eff._
import syntax.all._

import scalajs.js
import StorageFS.{Dir, StorageKey}
import cats.data.Writer

class StorageFSTests extends SlateSuite {
  def logStorageProgram[A](prog: StorageProgram[A]): StorageProgram[A] = {
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction]]
    writer.runWriterUnsafe[Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction], Vector[String], A](
      StorageProgram.logActions[Fx.fx1[StorageAction], Fx.fx2[StorageAction, Writer[Vector[String], ?]], NoFx, A](prog)
    )(println)(implicitly[mem])
  }

  implicit class MkDirResultOps(result: StorageFS.MkDirResult) {
    def assertDirMade: StorageKey[Dir] = result match {
      case StorageFS.DirMade(k) => k
      case StorageFS.AlreadyPresent(_) => assert(false, "dir was already present before being made").asInstanceOf[StorageKey[Dir]]
    }
    def assertAlreadyPresent: StorageKey[Dir] = result match {
      case StorageFS.DirMade(_) => assert(false, "dir was not already present before being made").asInstanceOf[StorageKey[Dir]]
      case StorageFS.AlreadyPresent(k) => k
    }
  }

  val initialize = for {
    _ <- StorageFS.initFS[Fx.fx1[StorageAction]]
  } yield ()

  def initializedDir: Map[String, String] =
    StorageProgram.runProgram(PureStorage, initialize).detach.run(Map.empty).value._1

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
        .detach.run(initializedDir).value._2.value
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
      val dirKey = StorageFS.StorageKey[Dir]("dir", "0")
      val nestedKey = StorageFS.StorageKey[Dir]("nested", "1")
      StorageProgram.runProgram(PureStorage, addDir)
        .detach.run(initializedDir).value._2 shouldBe ((StorageFS.DirMade(dirKey), StorageFS.DirMade(nestedKey), StorageFS.AlreadyPresent(dirKey)))
    }

    "should return a key to a dir that exists" in {
      val detNonceSource = makeDetNonceSource
      val addDir = for {
        newDirKey <- StorageFS.mkDir("dir", detNonceSource, StorageFS.fsroot)
        addedDir <- StorageFS.getDir(newDirKey.value.assertDirMade)
      } yield addedDir
      val result = StorageProgram.runProgram(PureStorage, addDir)
        .detach.run(initializedDir).value._2.value

      result.isEmpty shouldBe true
    }

  }

}
