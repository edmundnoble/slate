package qq

import qq.Compiler.{CompiledFilter, OrCompilationError}

final case class CompiledDefinition[C <: Compiler with Singleton]
(name: String, numParams: Int, body: (List[CompiledFilter[C]] => OrCompilationError[CompiledFilter[C]]))

