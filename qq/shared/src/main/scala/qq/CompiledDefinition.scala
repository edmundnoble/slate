package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}

final case class CompiledDefinition[AnyTy]
(name: String, numParams: Int, body: (List[CompiledFilter[AnyTy]] => OrCompilationError[CompiledFilter[AnyTy]]))

