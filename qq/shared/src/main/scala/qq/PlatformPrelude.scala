package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}

import scalaz.\/
import scalaz.syntax.either._

trait PlatformPrelude[JsonTy] extends Prelude[JsonTy] {

  final def sealPartialBody(body: PartialFunction[List[CompiledFilter[JsonTy]], OrCompilationError[CompiledFilter[JsonTy]]]
                           )(params: List[CompiledFilter[JsonTy]]): OrCompilationError[CompiledFilter[JsonTy]] = {
    body.applyOrElse(params, ???)
  }

  // x | orElse(y): null coalescing operator
  def orElse: CompiledDefinition[JsonTy]

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition[JsonTy]

  // null constant
  def `null`: CompiledDefinition[JsonTy]

  // true constant
  def `true`: CompiledDefinition[JsonTy]

  // false constant
  def `false`: CompiledDefinition[JsonTy]

  // array/object length
  def length: CompiledDefinition[JsonTy]

  // object keys
  def keys: CompiledDefinition[JsonTy]

  // regex replace
  def replaceAll: CompiledDefinition[JsonTy]

  // filters

  def arrays: CompiledDefinition[JsonTy]

  def objects: CompiledDefinition[JsonTy]

  def iterables: CompiledDefinition[JsonTy]

  def booleans: CompiledDefinition[JsonTy]

  def numbers: CompiledDefinition[JsonTy]

  def strings: CompiledDefinition[JsonTy]

  def nulls: CompiledDefinition[JsonTy]

  def values: CompiledDefinition[JsonTy]

  def scalars: CompiledDefinition[JsonTy]

  // ajax

  def httpDelete: CompiledDefinition[JsonTy]

  def httpGet: CompiledDefinition[JsonTy]

  def httpPatch: CompiledDefinition[JsonTy]

  def httpPost: CompiledDefinition[JsonTy]

  def httpPut: CompiledDefinition[JsonTy]

  override def all(runtime: QQRuntime[JsonTy]): QQCompilationException \/ IndexedSeq[CompiledDefinition[JsonTy]] = {
    Vector(
      `null`, `true`, `false`, orElse, b64Encode,
      length, keys, replaceAll, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, httpDelete, httpGet,
      httpPatch, httpPost, httpPut
    ).right[QQCompilationException]
  }

}



