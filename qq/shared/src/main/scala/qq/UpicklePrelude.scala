package qq

import java.util.regex.Pattern

import monix.eval.Task
import upickle.Js

import scalaz.syntax.apply._
import scalaz.syntax.either._
import scalaz.syntax.traverse._
import scalaz.std.list._
import monix.scalaz._
import qq.QQCompiler.{VarBindings, CompiledFilter, VarBinding}
import scodec.bits.ByteVector

import scalaz.Reader

object UpicklePrelude extends PlatformPrelude[Js.Value] {

  import CompiledDefinition.noParamDefinition

  def `null`: CompiledDefinition[Js.Value] = noParamDefinition("null", CompiledFilter.const(Js.Null))

  def `true`: CompiledDefinition[Js.Value]  = noParamDefinition("true", CompiledFilter.const(Js.True))

  def `false`: CompiledDefinition[Js.Value] = noParamDefinition("false", CompiledFilter.const(Js.False))

  def orElse: CompiledDefinition[Js.Value] = CompiledDefinition[Js.Value]("orElse", 1, {
    case (default :: Nil) => (for {d <- Reader(default)} yield (pf: Js.Value) => pf match {
      case Js.Null => d(Js.Null)
      case k => Task.now(k :: Nil)
    }).run.right[QQCompilationException]
  })

  def b64Encode: CompiledDefinition[Js.Value] = noParamDefinition("b64Encode", CompiledFilter.func {
    case Js.Str(str) => Task.now(Js.Str(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64) :: Nil)
    case k => Task.raiseError(QQRuntimeException("Tried to get base64 encoding of " + UpickleRuntime.print(k)))
  })

  override def length: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "length", CompiledFilter.func {
        case arr: Js.Arr => Task.now(Js.Num(arr.value.length) :: Nil)
        case Js.Str(str) => Task.now(Js.Num(str.length) :: Nil)
        case obj: Js.Obj => Task.now(Js.Num(obj.value.size) :: Nil)
        case Js.Null => Task.now((Js.Num(0): Js.Value) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get length of " + k))
      }
    )

  override def keys: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: Js.Obj => Task.now(Js.Arr(obj.value.map(p => Js.Str(p._1))(collection.breakOut): _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get keys of " + k))
      }
    )

  override def replaceAll: CompiledDefinition[Js.Value] =
    CompiledDefinition[Js.Value](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => ((bindings: VarBindings[Js.Value]) => {
          (jsv: Js.Value) =>
            {
              val regexesFilter: Task[List[Pattern]] = regexFilter(bindings)(jsv).flatMap(_.traverse[Task, Pattern] {
                case Js.Str(string) => Task.now(Pattern.compile(string))
                case j => Task.raiseError(NotARegex(UpickleRuntime.print(j)))
              })
              val replacementsFilter: Task[List[String]] = replacementFilter(bindings)(jsv).flatMap(_.traverse[Task, String] {
                case Js.Str(string) => Task.now(string)
                case j => Task.raiseError(QQRuntimeException("can't replace with " + UpickleRuntime.print(j)))
              })
              val valueRegexReplacementList = (regexesFilter |@| replacementsFilter) { (regexes, replacements) =>
                jsv match {
                  case Js.Str(string) =>
                    Task.now(for {regex <- regexes; replacement <- replacements}
                      yield Js.Str(regex.matcher(string).replaceAll(replacement)))
                  case j => Task.raiseError(QQRuntimeException("can't replace " + UpickleRuntime.print(j)))
                }
              }.flatten
              valueRegexReplacementList
            }
        }).right[QQCompilationException]
      }
    )

  override def arrays: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "arrays", CompiledFilter.func {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case bool@(Js.True | Js.False) => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case num: Js.Num => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case str: Js.Str => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case Js.Null => Task.now(Js.Null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case arr: Js.Arr => Task.now(Nil)
        case obj: Js.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  // array or object includes
  override def includes: CompiledDefinition[Js.Value] = CompiledDefinition.undefinedOnPlatform("includes")
}
