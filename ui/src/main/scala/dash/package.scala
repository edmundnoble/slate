import scalaz.{Coyoneda, Free, FreeT}

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
}
