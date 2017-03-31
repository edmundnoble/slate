package qq.macros.stager

import qq.cc.CompileError.OrCompileError

import scala.reflect.macros.Universe

case class PreCompiledDefinition[C <: Universe]
(name: String, numParams: Int, body: Vector[C#Tree] => OrCompileError[C#Tree])
