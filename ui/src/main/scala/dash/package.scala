import scalaz.{Coyoneda, Free, ReaderT, WriterT}

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
  type LoggedStorage[F[_], A] = WriterT[F, Vector[String], A]
  type Retargetable[F[_], A] = ReaderT[F, String, A]
}
