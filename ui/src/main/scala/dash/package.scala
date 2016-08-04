import scalaz.{Coyoneda, Free}

package object dash {
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
}
