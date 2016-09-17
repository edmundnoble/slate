package qq

import monix.eval.Task
import qq.data.VarBinding

import scalaz.\/

package object cc {

  type VarBindings[J] = Map[String, VarBinding[J]]
  type CompiledFilter[J] = VarBindings[J] => CompiledProgram[J]
  type CompiledProgram[J] = J => Task[List[J]]
  type OrCompilationError[T] = QQCompilationException \/ T

}
