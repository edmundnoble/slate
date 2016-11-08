import cats.data.{Reader, Writer}
import org.atnos.eff.{Eff, Fx}
import slate.storage.StorageAction

package object slate {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Eff[Fx.fx1[StorageAction], A]
  type LoggedStorage[A] = Writer[Vector[String], A]
  type Retargetable[A] = Reader[String, A]
}
