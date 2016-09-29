package qq
package cc

import java.util.regex.Pattern

import monix.eval.Task
import monix.scalaz._
import qq.data.{CompiledDefinition, JSON}
import scodec.bits.ByteVector
import upickle.Js

import scalaz.Reader
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.traverse._

object JSONPrelude extends PlatformPrelude[JSON] {

  import CompiledDefinition.noParamDefinition

  def `null`: CompiledDefinition[JSON] = noParamDefinition("null", CompiledFilter.const(JSON.Null))

  def `true`: CompiledDefinition[JSON] = noParamDefinition("true", CompiledFilter.const(JSON.True))

  def `false`: CompiledDefinition[JSON] = noParamDefinition("false", CompiledFilter.const(JSON.False))

  def orElse: CompiledDefinition[JSON] = CompiledDefinition[JSON]("orElse", 1, {
    case (default :: Nil) => (for {d <- Reader(default)} yield (pf: JSON) => pf match {
      case JSON.Null => d(JSON.Null)
      case k => Task.now(k :: Nil)
    }).run.right[QQCompilationException]
  })

  def b64Encode: CompiledDefinition[JSON] = noParamDefinition("b64Encode", CompiledFilter.func {
    case JSON.Str(str) => Task.now(JSON.Str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) :: Nil)
    case k => Task.raiseError(QQRuntimeException("Tried to get base64 encoding of " + JSONRuntime.print(k)))
  })

  override def length: CompiledDefinition[JSON] =
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

  override def keys: CompiledDefinition[JSON] =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: JSON.Obj => Task.now(JSON.Arr(obj.map(p => JSON.Str(p._1))(collection.breakOut): _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get keys of " + k))
      }
    )

  override def replaceAll: CompiledDefinition[JSON] =
    CompiledDefinition[JSON](name = "replaceAll", numParams = 2,
      body = CompiledDefinition.standardEffectDistribution[JSON] {
        case (regexRaw :: replacementRaw :: Nil) => (j: JSON) =>
          val regexTask: Task[Pattern] = regexRaw match {
            case JSON.Str(string) => Task.now(Pattern.compile(string))
            case k => Task.raiseError(NotARegex(JSONRuntime.print(k)))
          }
          val replacementTask: Task[String] = replacementRaw match {
            case JSON.Str(string) => Task.now(string)
            case k => Task.raiseError(QQRuntimeException("can't replace with " + JSONRuntime.print(k)))
          }
          val valueRegexReplacementList = Task.mapBoth(regexTask, replacementTask) { (regex, replacement) =>
            j match {
              case JSON.Str(string) =>
                Task.now(JSON.Str(regex.matcher(string).replaceAll(replacement)))
              case k => Task.raiseError(QQRuntimeException("can't replace " + JSONRuntime.print(k)))
            }
          }.flatten
          valueRegexReplacementList
      })

  override def select: CompiledDefinition[JSON] = CompiledDefinition[JSON]("select", 1, {
    case List(filterFun) => ((bindings: VarBindings[JSON]) => {
      case JSON.Arr(values) => values.traverseM(filterFun(bindings)).map(_.filter(_ == JSON.True).toList)
    }: CompiledProgram[JSON]).right
  })

  override def arrays: CompiledDefinition[JSON] =
    noParamDefinition(
      "arrays", CompiledFilter.func {
        case arr: JSON.Arr => Task.now(arr :: Nil)
        case _ => Task.now(Nil)
      })

  override def objects: CompiledDefinition[JSON] =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case obj: JSON.Obj => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[JSON] =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case arr: JSON.Arr => Task.now(arr :: Nil)
        case obj: JSON.Obj => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[JSON] =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case bool@(JSON.True | JSON.False) => Task.now(bool :: Nil)
        case _ => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[JSON] =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case num: JSON.Num => Task.now(num :: Nil)
        case _ => Task.now(Nil)
      })

  override def strings: CompiledDefinition[JSON] =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case str: JSON.Str => Task.now(str :: Nil)
        case _ => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[JSON] =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case JSON.Null => Task.now(JSON.Null :: Nil)
        case _ => Task.now(Nil)
      })

  override def values: CompiledDefinition[JSON] =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[JSON] =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case _: JSON.Arr => Task.now(Nil)
        case _: JSON.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  // array or object includes
  override def includes: CompiledDefinition[JSON] = CompiledDefinition.undefinedOnPlatform("includes")
}
