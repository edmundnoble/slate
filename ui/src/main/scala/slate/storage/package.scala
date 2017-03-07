package slate

import cats.data.State

package object storage {

  type StringMapState[A] = State[StringMap, A]

  type StringMap = Map[String, String]

}
