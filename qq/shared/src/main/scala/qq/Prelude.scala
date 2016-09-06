package qq

import qq.QQCompiler.OrCompilationError

trait Prelude[JsonTy] {
  def all(runtime: QQRuntime[JsonTy]): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]]
}

