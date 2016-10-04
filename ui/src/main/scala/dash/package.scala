import scalaz.{Coyoneda, Free, ReaderT}

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
  type RetargetableStorageProgram[A] = ReaderT[StorageProgram, String, A]
}
