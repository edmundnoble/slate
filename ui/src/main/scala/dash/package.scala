import scalaz.{Coyoneda, Free}

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
}
