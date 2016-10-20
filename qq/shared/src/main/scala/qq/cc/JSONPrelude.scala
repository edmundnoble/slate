package qq
package cc

import java.util.regex.Pattern

import cats.data.{Xor, NonEmptyList, Validated, ValidatedNel}
import monix.eval.Task
import monix.cats._
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import qq.util._
import scodec.bits.ByteVector
import cats.implicits._
import org.atnos.eff._, Eff._, syntax.all._

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
      CompiledFilter.singleton((j: JSON) =>
        if (j == JSON.Null) default(JSON.Null)
        else j.pureEff
      ).right
  })

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition = noParamDefinition("b64Encode", CompiledFilter.singleton {
    case JSON.Str(str) => JSON.Str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64).pureEff
    case k => typeError("b64Encode", "string" -> k)
  })

  // array/object length
  def length: CompiledDefinition =
    noParamDefinition(
      "length", CompiledFilter.singleton {
        case arr: JSON.Arr => JSON.Num(arr.value.length).pureEff
        case JSON.Str(str) => JSON.Num(str.length).pureEff
        case obj: JSON.ObjMap => JSON.Num(obj.value.size).pureEff
        case obj: JSON.ObjList => JSON.Num(obj.value.size).pureEff
        case JSON.Null => JSON.Num(0).pureEff
        case k => typeError("length", "array | string | object | null" -> k)
      }
    )

  // object keys
  def keys: CompiledDefinition =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: JSON.Obj => JSON.Arr(obj.map(p => JSON.Str(p._1))(collection.breakOut): _*).pureEff
        case k => typeError("keys", "object" -> k)
      }
    )

  // regex replace
  def replaceAll: CompiledDefinition =
    CompiledDefinition(name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution {
        case (regexRaw :: replacementRaw :: Nil) => (j: JSON) =>
          val regexValidated: Validated[NonEmptyList[QQRuntimeError], Pattern] = regexRaw match {
            case JSON.Str(string) => Pattern.compile(string).validNel
            case k => NonEmptyList(NotARegex(QQRuntime.print(k))).invalid
          }
          val replacementValidated: Validated[NonEmptyList[QQRuntimeError], String] = replacementRaw match {
            case JSON.Str(string) => string.validNel
            case k => NonEmptyList(TypeError("replace", "string" -> k)).invalid
          }
          val valueRegexReplacementList: Validated[NonEmptyList[QQRuntimeError], JSON] = (regexValidated |@| replacementValidated).map { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                (JSON.Str(regex.matcher(string).replaceAll(replacement)): JSON).validNel[QQRuntimeError]
              case k => TypeError("replace", "string" -> k).invalid[JSON]
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
        case arr: JSON.Arr => Task.now((arr :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def objects: CompiledDefinition =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case obj: JSON.Obj => Task.now((obj :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case arr: JSON.Arr => Task.now((arr :: Nil).validNel)
        case obj: JSON.Obj => Task.now((obj :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case bool@(JSON.True | JSON.False) => Task.now((bool :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case num: JSON.Num => Task.now((num :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def strings: CompiledDefinition =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case str: JSON.Str => Task.now((str :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case JSON.Null => Task.now((JSON.Null :: Nil).validNel)
        case _ => Task.now(Nil.validNel)
      })

  def values: CompiledDefinition =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil.validNel)
        case k => Task.now((k :: Nil).validNel)
      })

  def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case _: JSON.Arr => Task.now(Nil.validNel)
        case _: JSON.Obj => Task.now(Nil.validNel)
        case k => Task.now((k :: Nil).validNel)
      })

  def toStringDef: CompiledDefinition =
    noParamDefinition(
      "toString", CompiledFilter.func { j: JSON =>
        Task.now((JSON.Str(JSON.render(j)) :: Nil).validNel)
      }
    )

  def all(implicit rec: RecursionEngine): QQCompilationException Xor Vector[CompiledDefinition] =
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
      length, keys, replaceAll, select, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, toStringDef
    ).right

}
