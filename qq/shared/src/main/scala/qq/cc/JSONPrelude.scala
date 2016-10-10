package qq
package cc

import java.util.regex.Pattern

import monix.eval.{Coeval, Task}
import monix.scalaz._
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import scodec.bits.ByteVector

import scalaz.\/
import scalaz.syntax.either._
import scalaz.syntax.apply._

object JSONPrelude extends Prelude {

  import CompiledDefinition.noParamDefinition

  // null constant
  def `null`: CompiledDefinition = noParamDefinition("null", CompiledFilter.const(JSON.Null))

  // true constant
  def `true`: CompiledDefinition = noParamDefinition("true", CompiledFilter.const(JSON.True))

  // false constant
  def `false`: CompiledDefinition = noParamDefinition("false", CompiledFilter.const(JSON.False))

  // x | orElse(y): null coalescing operator
  def orElse: CompiledDefinition = CompiledDefinition("orElse", 1, {
    case (default :: Nil) =>
      ((bindings: VarBindings) => (pf: JSON) =>
        pf match {
          case JSON.Null => default(bindings)(JSON.Null)
          case k => Task.now(k :: Nil)
        }).right[QQCompilationException]
  })

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition = noParamDefinition("b64Encode", CompiledFilter.func {
    case JSON.Str(str) => Task.now(JSON.Str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) :: Nil)
    case k => Task.raiseError(QQRuntimeException("Tried to get base64 encoding of " + QQRuntime.print(k)))
  })

  // array/object length
  def length: CompiledDefinition =
    noParamDefinition(
      "length", CompiledFilter.func {
        case arr: JSON.Arr => Task.now(JSON.Num(arr.value.length) :: Nil)
        case JSON.Str(str) => Task.now(JSON.Num(str.length) :: Nil)
        case obj: JSON.ObjMap => Task.now(JSON.Num(obj.value.size) :: Nil)
        case obj: JSON.ObjList => Task.now(JSON.Num(obj.value.size) :: Nil)
        case JSON.Null => Task.now((JSON.Num(0): JSON) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get length of " + k))
      }
    )

  // object keys
  def keys: CompiledDefinition =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: JSON.Obj => Task.now(JSON.Arr(obj.map(p => JSON.Str(p._1))(collection.breakOut): _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get keys of " + k))
      }
    )

  // regex replace
  def replaceAll: CompiledDefinition =
    CompiledDefinition(name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution {
        case (regexRaw :: replacementRaw :: Nil) => (j: JSON) =>
          val regexCoeval: Coeval[Pattern] = regexRaw match {
            case JSON.Str(string) => Coeval.now(Pattern.compile(string))
            case k => Coeval.raiseError(NotARegex(QQRuntime.print(k)))
          }
          val replacementCoeval: Coeval[String] = replacementRaw match {
            case JSON.Str(string) => Coeval.now(string)
            case k => Coeval.raiseError(QQRuntimeException("can't replace with " + QQRuntime.print(k)))
          }
          val valueRegexReplacementList = (regexCoeval |@| replacementCoeval) { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                Coeval.now(JSON.Str(regex.matcher(string).replaceAll(replacement)))
              case k => Coeval.raiseError(QQRuntimeException("can't replace " + QQRuntime.print(k)))
            }
          }.flatten
          Task.coeval(valueRegexReplacementList)
      })

  // filter
  def select: CompiledDefinition = CompiledDefinition("select", 1, {
    case List(filterFun) => ((bindings: VarBindings) => {
      (value: JSON) => filterFun(bindings)(value).map(_.filter(_ == JSON.True).map(_ => value))
    }: CompiledProgram).right
  })

  // array or object includes
  def includes: CompiledDefinition = CompiledDefinition.undefinedOnPlatform("includes")

  // type filters

  def arrays: CompiledDefinition =
    noParamDefinition(
      "arrays", CompiledFilter.func {
        case arr: JSON.Arr => Task.now(arr :: Nil)
        case _ => Task.now(Nil)
      })

  def objects: CompiledDefinition =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case obj: JSON.Obj => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case arr: JSON.Arr => Task.now(arr :: Nil)
        case obj: JSON.Obj => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case bool@(JSON.True | JSON.False) => Task.now(bool :: Nil)
        case _ => Task.now(Nil)
      })

  def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case num: JSON.Num => Task.now(num :: Nil)
        case _ => Task.now(Nil)
      })

  def strings: CompiledDefinition =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case str: JSON.Str => Task.now(str :: Nil)
        case _ => Task.now(Nil)
      })

  def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case JSON.Null => Task.now(JSON.Null :: Nil)
        case _ => Task.now(Nil)
      })

  def values: CompiledDefinition =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case _: JSON.Arr => Task.now(Nil)
        case _: JSON.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  def toStringDef: CompiledDefinition =
    noParamDefinition(
      "toString", CompiledFilter.func { j: JSON =>
        Task.now(JSON.Str(JSON.render(j)) :: Nil)
      }
    )

  def all(implicit rec: RecursionEngine): QQCompilationException \/ IndexedSeq[CompiledDefinition] =
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
      length, keys, replaceAll, select, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, toStringDef
    ).right

}
