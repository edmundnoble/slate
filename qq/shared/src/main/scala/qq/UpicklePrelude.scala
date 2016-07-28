package qq

import java.util.regex.Pattern

import monix.eval.Task
import upickle.Js

import scalaz.syntax.either._
import scalaz.syntax.traverse._
import scalaz.std.list._
import com.thoughtworks.each.Monadic._
import monix.scalaz._

object UpicklePrelude extends PlatformPrelude[Js.Value] {

  override def length: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "length", {
        case arr: Js.Arr => Task.now(Js.Num(arr.value.length) :: Nil)
        case Js.Str(str) => Task.now(Js.Num(str.length) :: Nil)
        case obj: Js.Obj => Task.now(Js.Num(obj.value.length) :: Nil)
        case Js.Null => Task.now((Js.Num(0): Js.Value) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def keys: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "keys", {
        case obj: Js.Obj => Task.now(Js.Arr(obj.value.map(p => Js.Str(p._1)): _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def replaceAll: CompiledDefinition[Js.Value] =
    CompiledDefinition[Js.Value](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => {
          (jsv: Js.Value) =>
            monadic[Task] {
              val regexes: List[Pattern] = regexFilter(jsv).each.traverse[Task, Pattern] {
                case Js.Str(string) => Task.now(Pattern.compile(string))
                case j => Task.raiseError(NotARegex(j.toString))
              }.each
              val replacements: List[String] = replacementFilter(jsv).each.traverse[Task, String] {
                case Js.Str(string) => Task.now(string)
                case j => Task.raiseError(QQRuntimeException(s"can't replace with ${j.toString}"))
              }.each
              val valueRegexReplacementList = (regexes, replacements).zipped.map { case (regex, replacement) =>
                jsv match {
                  case Js.Str(string) =>
                    Task.now(Js.Str(regex.matcher(string).replaceAll(replacement)): Js.Value)
                  case j => Task.raiseError(QQRuntimeException(s"can't replace ${j.toString}"))
                }
              }.sequence[Task, Js.Value].each
              valueRegexReplacementList
            }
        }.right[QQCompilationException]
      }
    )

  override def arrays: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "arrays", {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "objects", {
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "iterables", {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "booleans", {
        case bool@(Js.True | Js.False) => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "numbers", {
        case num: Js.Num => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "strings", {
        case str: Js.Str => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "nulls", {
        case Js.Null => Task.now(Js.Null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[Js.Value] =
    noParamDefinition(
      "scalars", {
        case arr: Js.Arr => Task.now(Nil)
        case obj: Js.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
