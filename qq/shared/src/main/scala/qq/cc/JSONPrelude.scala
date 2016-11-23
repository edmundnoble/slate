package qq
package cc

import java.util.regex.Pattern

import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import org.atnos.eff.syntax.all._
import qq.data.{CompiledDefinition, JSON}
import qq.util.Recursion.RecursionEngine
import qq.util._
import scodec.bits.ByteVector

object JSONPrelude extends Prelude {

  import CompiledDefinition.noParamDefinition
  import QQRuntimeException._

  // null constant
  def `null`: CompiledDefinition = noParamDefinition("null", CompiledFilter.const(JSON.Null))

  // true constant
  def `true`: CompiledDefinition = noParamDefinition("true", CompiledFilter.const(JSON.True))

  // false constant
  def `false`: CompiledDefinition = noParamDefinition("false", CompiledFilter.const(JSON.False))

  // x | orElse(y): null coalescing operator
  def orElse: CompiledDefinition = CompiledDefinition("orElse", 1, { params =>
    val default = params.head
    Right(
      CompiledFilter.singleton((j: JSON) =>
        if (j == JSON.Null) default(JSON.Null)
        else (j +: Vector.empty).pureEff[CompiledFilterStack]
      )
    )
  })

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition = noParamDefinition("b64Encode", CompiledFilter.singleton {
    case JSON.Str(str) => (JSON.str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) +: Vector.empty).pureEff[CompiledFilterStack]
    case k => typeErrorE("b64Encode", "string" -> k)
  })

  // array/object length
  def length: CompiledDefinition =
    noParamDefinition(
      "length", CompiledFilter.singleton {
        case arr: JSON.Arr => (JSON.num(arr.value.length) +: Vector.empty).pureEff
        case JSON.Str(str) => (JSON.num(str.length) +: Vector.empty).pureEff
        case obj: JSON.ObjMap => (JSON.num(obj.value.size) +: Vector.empty).pureEff
        case obj: JSON.ObjList => (JSON.num(obj.value.size) +: Vector.empty).pureEff
        case JSON.Null => (JSON.num(0) +: Vector.empty).pureEff
        case k => typeErrorE("length", "array | string | object | null" -> k)
      }
    )

  // object keys
  def keys: CompiledDefinition =
    noParamDefinition(
      "keys", CompiledFilter.singleton {
        case obj: JSON.Obj => (JSON.arr(obj.map(p => JSON.str(p._1))(collection.breakOut): _*) +: Vector.empty).pureEff[CompiledFilterStack]
        case k => typeErrorE("keys", "object" -> k)
      }
    )

  // regex replace
  def replaceAll: CompiledDefinition =
    CompiledDefinition(name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution { params =>
        val regexRaw = params.head
        val replacementRaw = params.tail.head
        (j: JSON) =>
          val regexValidated: Validated[RuntimeErrs, Pattern] = regexRaw match {
            case JSON.Str(string) => Pattern.compile(string).validNel
            case k => NonEmptyList.of[QQRuntimeError](NotARegex(QQRuntime.print(k))).invalid
          }
          val replacementValidated: Validated[RuntimeErrs, String] = replacementRaw match {
            case JSON.Str(string) => string.validNel
            case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid
          }
          val valueRegexReplacementList: Validated[RuntimeErrs, Vector[JSON]] = (regexValidated |@| replacementValidated).map { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                (JSON.str(regex.matcher(string).replaceAll(replacement)) +: Vector.empty).validNel[QQRuntimeError]
              case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid[Vector[JSON]]
            }
          }.flatten
          valueRegexReplacementList.toEither.send[CompiledFilterStack]
      })

  // filter
  def select: CompiledDefinition = CompiledDefinition("select", 1, {
    case params =>
      val filterFun = params.head
      Right(
        CompiledFilter.singleton {
          (value: JSON) => filterFun(value).map(_.filter(_ == JSON.True).map(_ => value)).into[CompiledFilterStack]
        }
      )
  })

  // array or object includes
  def includes: CompiledDefinition = CompiledDefinition.undefinedOnPlatform("includes")

  // type filters

  def arrays: CompiledDefinition =
    noParamDefinition(
      "arrays", CompiledFilter.singleton {
        case arr: JSON.Arr => ((arr: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def objects: CompiledDefinition =
    noParamDefinition(
      "objects", CompiledFilter.singleton {
        case obj: JSON.Obj => ((obj: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables", CompiledFilter.singleton {
        case arr: JSON.Arr => ((arr: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case obj: JSON.Obj => ((obj: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans", CompiledFilter.singleton {
        case bool@(JSON.True | JSON.False) => ((bool: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers", CompiledFilter.singleton {
        case num: JSON.Num => ((num: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def strings: CompiledDefinition =
    noParamDefinition(
      "strings", CompiledFilter.singleton {
        case str: JSON.Str => ((str: JSON) +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls", CompiledFilter.singleton {
        case JSON.Null => (JSON.`null` +: Vector.empty).pureEff[CompiledFilterStack]
        case _ => Vector.empty[JSON].pureEff
      })

  def values: CompiledDefinition =
    noParamDefinition(
      "values", CompiledFilter.singleton {
        case JSON.Null => Vector.empty[JSON].pureEff
        case k => (k +: Vector.empty).pureEff[CompiledFilterStack]
      })

  def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars", CompiledFilter.singleton {
        case _: JSON.Arr => Vector.empty[JSON].pureEff[CompiledFilterStack]
        case _: JSON.Obj => Vector.empty[JSON].pureEff[CompiledFilterStack]
        case k => (k +: Vector.empty).pureEff[CompiledFilterStack]
      })

  def toStringDef: CompiledDefinition =
    noParamDefinition(
      "toString", CompiledFilter.singleton { j: JSON =>
        (JSON.str(JSON.render(j)) +: Vector.empty).pureEff[CompiledFilterStack]
      }
    )

  def all(implicit rec: RecursionEngine): QQCompilationException Either Vector[CompiledDefinition] =
    Right(
      Vector(
        `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
        length, keys, replaceAll, select, arrays, objects, iterables, booleans,
        numbers, strings, nulls, values, scalars, toStringDef
      )
    )

}
