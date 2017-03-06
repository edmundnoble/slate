import cats.data.{Reader, Writer}

package object slate {
  type LoggedStorage[A] = Writer[Vector[String], A]
  type Retargetable[A] = Reader[String, A]
}
