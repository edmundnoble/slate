package qq

import qq.QQCompiler.CompiledFilter

import scalaz.\/
import scalaz.syntax.either._

trait PlatformPrelude[AnyTy] extends Prelude[AnyTy] {

  def noParamDefinition(name: String, fun: CompiledFilter[AnyTy]): CompiledDefinition[AnyTy] = {
    CompiledDefinition[AnyTy](
      name,
      numParams = 0,
      _ => fun.right[QQCompilationException]
    )
  }

  def length: CompiledDefinition[AnyTy]

  def keys: CompiledDefinition[AnyTy]

  def replaceAll: CompiledDefinition[AnyTy]

  def arrays: CompiledDefinition[AnyTy]

  def objects: CompiledDefinition[AnyTy]

  def iterables: CompiledDefinition[AnyTy]

  def booleans: CompiledDefinition[AnyTy]

  def numbers: CompiledDefinition[AnyTy]

  def strings: CompiledDefinition[AnyTy]

  def nulls: CompiledDefinition[AnyTy]

  def values: CompiledDefinition[AnyTy]

  def scalars: CompiledDefinition[AnyTy]

  override def all(runtime: QQRuntime[AnyTy]): QQCompilationException \/ List[CompiledDefinition[AnyTy]] = {
    (length +: keys +: replaceAll +: arrays +: objects +: iterables +: booleans +:
      numbers +: strings +: nulls +: values +: scalars +: Nil).right[QQCompilationException]
  }

}



