import upickle.Js

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scalaz.{Coyoneda, Free}

import upickle.default._

package object dash {
  // StorageProgram is an AST with StorageAction leaves
  type StorageProgram[A] = Free[Coyoneda[StorageAction, ?], A]
}
