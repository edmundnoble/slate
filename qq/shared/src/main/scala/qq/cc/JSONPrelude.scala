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
        else j.pureEff[CompiledFilterStack]
      ).right
  })

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition = noParamDefinition("b64Encode", CompiledFilter.singleton {
    case JSON.Str(str) => JSON.str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64).pureEff[CompiledFilterStack]
    case k => typeError("b64Encode", "string" -> k)
  })

  // array/object length
  def length: CompiledDefinition =
    noParamDefinition(
      "length", CompiledFilter.singleton {
        case arr: JSON.Arr => JSON.num(arr.value.length).pureEff[CompiledFilterStack]
        case JSON.Str(str) => JSON.num(str.length).pureEff[CompiledFilterStack]
        case obj: JSON.ObjMap => JSON.num(obj.value.size).pureEff[CompiledFilterStack]
        case obj: JSON.ObjList => JSON.num(obj.value.size).pureEff[CompiledFilterStack]
        case JSON.Null => JSON.num(0).pureEff[CompiledFilterStack]
        case k => typeError("length", "array | string | object | null" -> k)
      }
    )

  // object keys
  def keys: CompiledDefinition =
    noParamDefinition(
      "keys", CompiledFilter.singleton {
        case obj: JSON.Obj => JSON.arr(obj.map(p => JSON.str(p._1))(collection.breakOut): _*).pureEff[CompiledFilterStack]
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
            case k => NonEmptyList.of[QQRuntimeError](NotARegex(QQRuntime.print(k))).invalid
          }
          val replacementValidated: Validated[NonEmptyList[QQRuntimeError], String] = replacementRaw match {
            case JSON.Str(string) => string.validNel
            case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid
          }
          val valueRegexReplacementList: Validated[NonEmptyList[QQRuntimeError], JSON] = (regexValidated |@| replacementValidated).map { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                (JSON.Str(regex.matcher(string).replaceAll(replacement)): JSON).validNel[QQRuntimeError]
              case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid[JSON]
            }
          }.flatten
          valueRegexReplacementList.send[CompiledFilterStack]
      })

  // filter
  def select: CompiledDefinition = CompiledDefinition("select", 1, {
    case List(filterFun) => CompiledFilter.singleton {
      (value: JSON) => list.runList(filterFun(value)).map(_.filter(_ == JSON.True).map(_ => value)).into[CompiledFilterStack].collapse
    }.right
  })

  // array or object includes
  def includes: CompiledDefinition = CompiledDefinition.undefinedOnPlatform("includes")

  // type filters

  def arrays: CompiledDefinition =
    noParamDefinition(
      "arrays", CompiledFilter.singleton {
        case arr: JSON.Arr => (arr: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def objects: CompiledDefinition =
    noParamDefinition(
      "objects", CompiledFilter.singleton {
        case obj: JSON.Obj => (obj: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables", CompiledFilter.singleton {
        case arr: JSON.Arr => (arr: JSON).pureEff[CompiledFilterStack]
        case obj: JSON.Obj => (obj: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans", CompiledFilter.singleton {
        case bool@(JSON.True | JSON.False) => (bool: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers", CompiledFilter.singleton {
        case num: JSON.Num => (num: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def strings: CompiledDefinition =
    noParamDefinition(
      "strings", CompiledFilter.singleton {
        case str: JSON.Str => (str: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls", CompiledFilter.singleton {
        case JSON.Null => (JSON.Null: JSON).pureEff[CompiledFilterStack]
        case _ => list.empty
      })

  def values: CompiledDefinition =
    noParamDefinition(
      "values", CompiledFilter.singleton {
        case JSON.Null => list.empty
        case k => k.pureEff[CompiledFilterStack]
      })

  def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars", CompiledFilter.singleton {
        case _: JSON.Arr => list.empty
        case _: JSON.Obj => list.empty
        case k => k.pureEff[CompiledFilterStack]
      })

  def toStringDef: CompiledDefinition =
    noParamDefinition(
      "toString", CompiledFilter.singleton { j: JSON =>
        (JSON.Str(JSON.render(j)): JSON).pureEff[CompiledFilterStack]
      }
    )

  def all(implicit rec: RecursionEngine): QQCompilationException Xor Vector[CompiledDefinition] =
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
      length, keys, replaceAll, select, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, toStringDef
    ).right

}
