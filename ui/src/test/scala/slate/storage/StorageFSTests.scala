package slate
package storage

import org.atnos.eff._
import Eff._
import syntax.all._

import scalajs.js
import StorageFS.Dir
import cats.data.Writer

class StorageFSTests extends SlateSuite {
  "init" in {
    import StorageFS.Now
    val prog = for {
      _ <- reader.runReader(shapeless.tag[Now](new js.Date(0)))(StorageFS.initFS[Fx.fx2[StorageFS.NeedsNow, StorageAction]])
      dir <- StorageFS.getDir(StorageFS.fsroot)
    } yield dir
    type mem = Member.Aux[Writer[Vector[String], ?], Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction]]
    println(
      StorageProgram.runProgram(PureStorage,
        writer.runWriterUnsafe[Fx.fx2[StorageAction, Writer[Vector[String], ?]], Fx.fx1[StorageAction], Vector[String], Option[Dir]](
          StorageProgram.logActions[Fx.fx1[StorageAction], Fx.fx2[StorageAction, Writer[Vector[String], ?]], NoFx, Option[Dir]](prog)
        )(println)(implicitly[mem])
      )
        .detach.run(Map.empty).value
    )
  }
}
