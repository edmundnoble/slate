package qq

import qq.QQCompiler.OrCompilationError

trait Prelude[J] {
  def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]]
}

