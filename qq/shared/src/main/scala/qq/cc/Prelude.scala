package qq.cc

import qq.data.CompiledDefinition

trait Prelude[J] {
  def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]]
}

