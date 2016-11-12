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

  "init" in {
    import StorageFS.Now
    val prog = for {
      _ <- reader.runReader(shapeless.tag[Now](new js.Date(0)))(StorageFS.initFS[Fx.fx2[StorageFS.NeedsNow, StorageAction]])
      dir <- StorageFS.getDir(StorageFS.fsroot)
    } yield dir
      val outDir = StorageProgram.runProgram(PureStorage, prog)
        .detach.run(Map.empty).value._2.value
    outDir.childKeys.isEmpty shouldBe true
    outDir.metadata.lastAccessed.getTime() shouldBe 0
    outDir.metadata.lastUpdated.getTime() shouldBe 0
  }
}
