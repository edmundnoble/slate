package qq

import qq.Compiler.QQCompilationException

import scalaz.\/

trait Prelude[C <: Compiler] {
  def all(compiler: C): QQCompilationException \/ List[CompiledDefinition[compiler.type]]
}

