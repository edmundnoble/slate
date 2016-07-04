package qq

import qq.Compiler.{CompiledFilter, QQCompilationException}

import scalaz.\/
import scalaz.syntax.either._
import scalaz.Liskov._

trait PlatformPrelude[C <: Compiler with Singleton] extends Prelude[C] {

  def noParamDefinition(name: String, fun: CompiledFilter[C]): CompiledDefinition[C] = {
    CompiledDefinition[C](
      name,
      numParams = 0,
      _ => fun.right[QQCompilationException]
    )
  }

  def length: CompiledDefinition[C]

  def keys: CompiledDefinition[C]

  def replaceAll: CompiledDefinition[C]

  def arrays: CompiledDefinition[C]

  def objects: CompiledDefinition[C]

  def iterables: CompiledDefinition[C]

  def booleans: CompiledDefinition[C]

  def numbers: CompiledDefinition[C]

  def strings: CompiledDefinition[C]

  def nulls: CompiledDefinition[C]

  def values: CompiledDefinition[C]

  def scalars: CompiledDefinition[C]

  override def all(compiler: C): QQCompilationException \/ List[CompiledDefinition[compiler.type]] = {
    // I wonder if this is a bug in Scala or a bug in my understanding of singleton types
    (length :: keys :: replaceAll :: arrays :: objects :: iterables :: booleans ::
      numbers :: strings :: nulls :: values :: scalars :: Nil).right[QQCompilationException]
      .asInstanceOf[QQCompilationException \/ List[CompiledDefinition[compiler.type]]]
  }

}



