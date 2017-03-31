package qq
package cc

import java.util.regex.Pattern

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import org.atnos.eff.syntax.all._
import org.atnos.eff._
import qq.ast.QQRuntime
import qq.cc.CompileError.OrCompileError
import qq.cc.Prelude.PreludeStack
import qq.data.{CompiledDefinition, JSON}
import qq.util._
import scodec.bits.ByteVector

object JSONPrelude extends Prelude[InterpretedFilter] {

  import CompiledDefinition.noParamDefinition
  import RuntimeError._

  // null constant
  def `null`: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition("null", InterpretedFilter.const(JSON.Null))
  }

  // true constant
  def `true`: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition("true", InterpretedFilter.const(JSON.True))
  }

  // false constant
  def `false`: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition("false", InterpretedFilter.const(JSON.False))
  }

  // x | orElse(y): null coalescing operator
  def orElse: CompiledDefinition[InterpretedFilter] = {
    CompiledDefinition("orElse", 1, params => {
      Right(
        InterpretedFilter.singleton((j: JSON) => {
          if (j == JSON.Null) params.head(JSON.Null)
          else (j +: Vector.empty).pureEff[InterpretedFilterStack]
        }
        )
      )
    })
  }

  // base 64 encoding, duh
  def b64Encode: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition("b64Encode", InterpretedFilter.singleton {
      case JSON.Str(str) => (JSON.str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) +: Vector.empty).pureEff
      case k => typeErrorE("b64Encode", "string" -> k)
    })
  }

  // array/object length
  def length: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "length", InterpretedFilter.singleton {
        case arr: JSON.Arr => (JSON.num(arr.value.length) +: Vector.empty).pureEff
        case JSON.Str(str) => (JSON.num(str.length) +: Vector.empty).pureEff
        case obj: JSON.ObjMap => (JSON.num(obj.value.size) +: Vector.empty).pureEff
        case obj: JSON.ObjList => (JSON.num(obj.value.size) +: Vector.empty).pureEff
        case JSON.Null => (JSON.num(0) +: Vector.empty).pureEff
        case k => typeErrorE("length", "array | string | object | null" -> k)
      }
    )
  }

  // object keys
  def keys: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "keys", InterpretedFilter.singleton {
        case obj: JSON.Obj => (JSON.arr(obj.map(p => JSON.str(p._1))(collection.breakOut): _*) +: Vector.empty).pureEff
        case k => typeErrorE("keys", "object" -> k)
      }
    )
  }

  // regex replace
  def replaceAll: CompiledDefinition[InterpretedFilter] = {
    CompiledDefinition(name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution { params => {
        val regexRaw = params.head
        val replacementRaw = params.tail.head
        val regexValidated: Validated[RuntimeErrs, Pattern] = regexRaw match {
          case JSON.Str(string) => Pattern.compile(string).validNel
          case k => NonEmptyList.of[QQRuntimeError](NotARegex(JSON.render(k))).invalid
        }
        val replacementValidated: Validated[RuntimeErrs, String] = replacementRaw match {
          case JSON.Str(string) => string.validNel
          case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid
        }
        (j: JSON) => {
          Eff.send[OrRuntimeErr, InterpretedFilterStack, Vector[JSON]](
            Applicative[OrRuntimeErrs].map2(regexValidated, replacementValidated) { (regex, replacement) => {
              j match {
                case JSON.Str(string) =>
                  (JSON.str(regex.matcher(string).replaceAll(replacement)) +: Vector.empty).validNel[QQRuntimeError]
                case k => NonEmptyList.of[QQRuntimeError](TypeError("replace", "string" -> k)).invalid[Vector[JSON]]
              }
            }
            }.flatten.toEither)
        }
      }
      }
    )
  }

  // filter
  def select: CompiledDefinition[InterpretedFilter] = {
    CompiledDefinition("select", 1, { params => {
      Right(
        InterpretedFilter.singleton(
          value => params.head(value).map(_.filter(_ == JSON.True).map(_ => value))
        )
      )
    }
    })
  }

  // array or object includes
  def includes: CompiledDefinition[InterpretedFilter] = {
    CompiledDefinition.undefinedOnPlatform("includes")
  }

  // type filters

  def arrays: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "arrays", InterpretedFilter.singleton {
        case arr: JSON.Arr => ((arr: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def objects: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "objects", InterpretedFilter.singleton {
        case obj: JSON.Obj => ((obj: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def iterables: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "iterables", InterpretedFilter.singleton {
        case arr: JSON.Arr => ((arr: JSON) +: Vector.empty).pureEff
        case obj: JSON.Obj => ((obj: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def booleans: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "booleans", InterpretedFilter.singleton {
        case bool@(JSON.True | JSON.False) => ((bool: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def numbers: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "numbers", InterpretedFilter.singleton {
        case num: JSON.Num => ((num: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def strings: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "strings", InterpretedFilter.singleton {
        case str: JSON.Str => ((str: JSON) +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def nulls: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "nulls", InterpretedFilter.singleton {
        case JSON.Null => (JSON.`null` +: Vector.empty).pureEff
        case _ => Vector.empty[JSON].pureEff
      })
  }

  def values: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "values", InterpretedFilter.singleton {
        case JSON.Null => Vector.empty[JSON].pureEff
        case k => (k +: Vector.empty).pureEff
      })
  }

  def scalars: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "scalars", InterpretedFilter.singleton {
        case _: JSON.Arr => Vector.empty[JSON].pureEff
        case _: JSON.Obj => Vector.empty[JSON].pureEff
        case k => (k +: Vector.empty).pureEff
      })
  }

  def toStringDef: CompiledDefinition[InterpretedFilter] = {
    noParamDefinition(
      "toString", InterpretedFilter.singleton((j: JSON) =>
        (JSON.str(JSON.render(j)) +: Vector.empty).pureEff)
    )
  }

  def map(runtime: QQRuntime[InterpretedFilter]): CompiledDefinition[InterpretedFilter] = {
    CompiledDefinition[InterpretedFilter]("map", 1,
      body = p => {
        Right(
          runtime.composeFilters(
            runtime.path.get.ret(runtime.path.get.collectResults),
            p(0)
          )
        )
      }
    )
  }

  val print: CompiledDefinition[InterpretedFilter] =
    CompiledDefinition.noParamDefinition("print",
      InterpretedFilter.singleton { (jsv: JSON) => {
        println("debug: " + JSON.render(jsv))
        (jsv +: Vector.empty).pureEff
      }
      }
    )

  val empty: CompiledDefinition[InterpretedFilter] =
    CompiledDefinition.noParamDefinition("empty", InterpretedFilter.constL(Vector.empty))

  def all(runtime: QQRuntime[InterpretedFilter]): Vector[CompiledDefinition[InterpretedFilter]] = {
    Vector(
      `null`, `true`, `false`, orElse, b64Encode, includes, // exists, forall,
      length, keys, replaceAll, select, arrays, objects, iterables, booleans,
      numbers, strings, nulls, values, scalars, toStringDef,
      map(runtime), print, empty
    )

}
