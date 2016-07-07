package qq

import qq.QQCompiler.QQCompilationException

import scalaz.\/

trait Prelude[AnyTy] {
  def all(runtime: QQRuntime[AnyTy]): QQCompilationException \/ List[CompiledDefinition[AnyTy]]
}

