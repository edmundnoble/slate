package slate
package storage

import org.atnos.eff._
import Eff._
import syntax.all._

import scalajs.js
import StorageFS.Dir
import cats.data.Writer

class StorageFSTests extends SlateSuite {
  def logStorageProgram[A](prog: StorageProgram[A]): StorageProgram[A] = {
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction]]
    writer.runWriterUnsafe[Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction], Vector[String], A](
      StorageProgram.logActions[Fx.fx1[StorageAction], Fx.fx2[StorageAction, Writer[Vector[String], ?]], NoFx, A](prog)
    )(println)(implicitly[mem])
  }

  val initialize = for {
    _ <- StorageFS.initFS[Fx.fx1[StorageAction]]
  } yield ()

  def initializedDir: Map[String, String] =
    StorageProgram.runProgram(PureStorage, initialize).detach.run(Map.empty).value._1

  "init" in {
    val outDir =
      StorageProgram.runProgram(PureStorage, StorageFS.getDir(StorageFS.fsroot))
        .detach.run(initializedDir).value._2.value
    outDir.childFileKeys.isEmpty shouldBe true
    outDir.childDirKeys.isEmpty shouldBe true
  }
  "new dir" in {
    val addDir =
      StorageProgram.runProgram(PureStorage, StorageFS.mkDir("dir", StorageFS.fsroot))
        .detach.run(initializedDir).value._1
    println(addDir)
  }
}
