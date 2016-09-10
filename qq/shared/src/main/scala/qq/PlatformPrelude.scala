package qq

import qq.QQCompiler.{CompiledFilter, OrCompilationError}

import scalaz.\/
import scalaz.syntax.either._

trait PlatformPrelude[J] extends Prelude[J] {

  final def sealPartialBody(body: PartialFunction[List[CompiledFilter[J]], OrCompilationError[CompiledFilter[J]]]
                           )(params: List[CompiledFilter[J]]): OrCompilationError[CompiledFilter[J]] = {
    body.applyOrElse(params, ???)
  }

  // x | orElse(y): null coalescing operator
  def orElse: CompiledDefinition[J]

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition[J]

  // null constant
  def `null`: CompiledDefinition[J]

  // true constant
  def `true`: CompiledDefinition[J]

  // false constant
  def `false`: CompiledDefinition[J]

  // array/object length
  def length: CompiledDefinition[J]

  // object keys
  def keys: CompiledDefinition[J]

  // regex replace
  def replaceAll: CompiledDefinition[J]

  // array or object includes
  def includes: CompiledDefinition[J]
//
//  // array/object existential predicate transformer
//  def exists: CompiledDefinition[J]
//
//  // array/object universal predicate transformer
//  def forall: CompiledDefinition[J]

  // filters

  def arrays: CompiledDefinition[J]

  def objects: CompiledDefinition[J]

  def iterables: CompiledDefinition[J]

  def booleans: CompiledDefinition[J]

  def numbers: CompiledDefinition[J]

  def strings: CompiledDefinition[J]

  def nulls: CompiledDefinition[J]

  def values: CompiledDefinition[J]

  def scalars: CompiledDefinition[J]

  // ajax

  def httpDelete: CompiledDefinition[J]

  def httpGet: CompiledDefinition[J]

  def httpPatch: CompiledDefinition[J]

  def httpPost: CompiledDefinition[J]

  def httpPut: CompiledDefinition[J]

  override def all(runtime: QQRuntime[J]): QQCompilationException \/ IndexedSeq[CompiledDefinition[J]] = {
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, exists, forall,
      length, keys, replaceAll, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, httpDelete, httpGet,
      httpPatch, httpPost, httpPut
    ).right[QQCompilationException]
  }

}



