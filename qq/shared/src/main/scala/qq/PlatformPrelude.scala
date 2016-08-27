package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}

import scalaz.\/
import scalaz.syntax.either._

trait PlatformPrelude[AnyTy] extends Prelude[AnyTy] {

  final def sealPartialBody(body: PartialFunction[List[CompiledFilter[AnyTy]], OrCompilationError[CompiledFilter[AnyTy]]]
                           )(params: List[CompiledFilter[AnyTy]]): OrCompilationError[CompiledFilter[AnyTy]] = {
    body.applyOrElse(params, ???)
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

  def httpDelete: CompiledDefinition[AnyTy]

  def httpGet: CompiledDefinition[AnyTy]

  def httpPatch: CompiledDefinition[AnyTy]

  def httpPost: CompiledDefinition[AnyTy]

  def httpPut: CompiledDefinition[AnyTy]

  override def all(runtime: QQRuntime[AnyTy]): QQCompilationException \/ List[CompiledDefinition[AnyTy]] = {
    (length +: keys +: replaceAll +: arrays +: objects +: iterables +: booleans +:
      numbers +: strings +: nulls +: values +: scalars +: httpDelete +: httpGet +:
      httpPatch +: httpPost +: httpPut +: Nil).right[QQCompilationException]
  }

}



