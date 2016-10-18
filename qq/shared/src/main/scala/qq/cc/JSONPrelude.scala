package qq
package cc

import java.util.regex.Pattern

import monix.eval.{Coeval, Task}
import monix.scalaz._
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import qq.util._
import scodec.bits.ByteVector

import scalaz.{NonEmptyList, Validation, ValidationNel, \/}
import scalaz.syntax.either._
import scalaz.syntax.validation._
import scalaz.syntax.apply._

object JSONPrelude extends Prelude {

  import QQRuntimeException._

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
          case k => Task.now((k :: Nil).successNel[QQRuntimeError])
        }).right[QQCompilationException]
  })

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition = noParamDefinition("b64Encode", CompiledFilter.func {
    case JSON.Str(str) => Task.now((JSON.Str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) :: Nil).successNel)
    case k => Task.now(typeError("b64Encode", "string" -> k).failureNel)
  })

  // array/object length
  def length: CompiledDefinition =
    noParamDefinition(
      "length", CompiledFilter.func {
        case arr: JSON.Arr => Task.now((JSON.Num(arr.value.length) :: Nil).successNel)
        case JSON.Str(str) => Task.now((JSON.Num(str.length) :: Nil).successNel)
        case obj: JSON.ObjMap => Task.now((JSON.Num(obj.value.size) :: Nil).successNel)
        case obj: JSON.ObjList => Task.now((JSON.Num(obj.value.size) :: Nil).successNel)
        case JSON.Null => Task.now(((JSON.Num(0): JSON) :: Nil).successNel)
        case k => Task.now(typeError("length", "array | string | object | null" -> k).failureNel)
      }
    )

  // object keys
  def keys: CompiledDefinition =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: JSON.Obj => Task.now((JSON.Arr(obj.map(p => JSON.Str(p._1))(collection.breakOut): _*) :: Nil).successNel)
        case k => Task.now(typeError("keys", "object" -> k).failureNel)
      }
    )

  // regex replace
  def replaceAll: CompiledDefinition =
    CompiledDefinition(name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution {
        case (regexRaw :: replacementRaw :: Nil) => (j: JSON) =>
          val regexValidated: ValidationNel[QQRuntimeError, Pattern] = regexRaw match {
            case JSON.Str(string) => Pattern.compile(string).successNel
            case k => notARegex(QQRuntime.print(k)).failureNel
          }
          val replacementValidated: ValidationNel[QQRuntimeError, String] = replacementRaw match {
            case JSON.Str(string) => string.successNel
            case k => typeError("replace", "string" -> k).failureNel
          }
          val valueRegexReplacementList: Validation[NonEmptyList[QQRuntimeError], JSON] = (regexValidated |@| replacementValidated) { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                (JSON.Str(regex.matcher(string).replaceAll(replacement)): JSON).successNel[QQRuntimeError]
              case k => typeError("replace", "string" -> k).failureNel[JSON]
            }
          }.flatten
          Task.now(valueRegexReplacementList)
      })

  // filter
  def select: CompiledDefinition = CompiledDefinition("select", 1, {
    case List(filterFun) => ((bindings: VarBindings) => {
      (value: JSON) => filterFun(bindings)(value).map(_.map(_.filter(_ == JSON.True).map(_ => value)))
    }: CompiledProgram).right
  })

  // array or object includes
  def includes: CompiledDefinition = CompiledDefinition.undefinedOnPlatform("includes")

  // type filters

  def arrays: CompiledDefinition =
    noParamDefinition(
      "arrays", CompiledFilter.func {
        case arr: JSON.Arr => Task.now((arr :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def objects: CompiledDefinition =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case obj: JSON.Obj => Task.now((obj :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case arr: JSON.Arr => Task.now((arr :: Nil).successNel)
        case obj: JSON.Obj => Task.now((obj :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case bool@(JSON.True | JSON.False) => Task.now((bool :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case num: JSON.Num => Task.now((num :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def strings: CompiledDefinition =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case str: JSON.Str => Task.now((str :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case JSON.Null => Task.now((JSON.Null :: Nil).successNel)
        case _ => Task.now(Nil.successNel)
      })

  def values: CompiledDefinition =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil.successNel)
        case k => Task.now((k :: Nil).successNel)
      })

  def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case _: JSON.Arr => Task.now(Nil.successNel)
        case _: JSON.Obj => Task.now(Nil.successNel)
        case k => Task.now((k :: Nil).successNel)
      })

  def toStringDef: CompiledDefinition =
    noParamDefinition(
      "toString", CompiledFilter.func { j: JSON =>
        Task.now((JSON.Str(JSON.render(j)) :: Nil).successNel)
      }
    )

  def all(implicit rec: RecursionEngine): QQCompilationException \/ IndexedSeq[CompiledDefinition] =
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
      length, keys, replaceAll, select, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, toStringDef
    ).right

}
