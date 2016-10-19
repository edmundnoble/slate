import cats.data.{ReaderT, WriterT}
import cats.free.Free
import dash.StorageAction.StorageActionF

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[StorageActionF, A]
  type LoggedStorage[F[_], A] = WriterT[F, Vector[String], A]
  type Retargetable[F[_], A] = ReaderT[F, String, A]
}
