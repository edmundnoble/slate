package qq

import qq.Compiler.QQCompilationException

import scalaz.\/

final case class CompiledDefinition[C <: Compiler with Singleton]
(name: String, numParams: Int, body: (List[C#CompiledFilter] => QQCompilationException \/ C#CompiledFilter))

