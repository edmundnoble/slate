package qq.jsc

import java.util.regex.Pattern

import monix.eval.Task
import qq.QQCompiler.{CompiledFilter, QQCompilationException, QQRuntimeException}

import scalaz.syntax.either._
import scalaz.syntax.traverse._
import scalaz.std.list._
import scala.scalajs.js
import com.thoughtworks.each.Monadic._
import qq.{CompiledDefinition, PlatformPrelude}
import qq.Util._

object JSPrelude extends PlatformPrelude[AnyRef] {

  import qq.QQCompiler._

  override def length: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "length", {
        case arr: js.Array[js.Any@unchecked] => Task.now(Int.box(arr.length) :: Nil)
        case str: String => Task.now(Int.box(str.length) :: Nil)
        case obj: js.Object => Task.now(Int.box(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length) :: Nil)
        case null => Task.now(Int.box(0) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get length of $k"))
      }
    )


  override def replaceAll: CompiledDefinition[AnyRef] =
    CompiledDefinition[AnyRef](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => {
          (jsv: AnyRef) =>
            monadic[Task] {
              val regexes: List[Pattern] = regexFilter(jsv).each.traverse[Task, Pattern] {
                case string: String => Task.now(Pattern.compile(string))
                case j => Task.raiseError(NotARegex(j.toString))
              }.each
              val replacements: List[String] = replacementFilter(jsv).each.traverse[Task, String] {
                case string: String => Task.now(string)
                case j => Task.raiseError(QQRuntimeException(s"can't replace with ${j.toString}"))
              }.each
              val valueRegexReplacementList = (regexes, replacements).zipped.map { case (regex, replacement) =>
                jsv match {
                  case string: String =>
                    Task.now(regex.matcher(string).replaceAll(replacement): AnyRef)
                  case j => Task.raiseError(QQRuntimeException(s"can't replace ${j.toString}"))
                }
              }.sequence[Task, AnyRef].each
              valueRegexReplacementList
            }
        }.right[QQCompilationException]
      }
    )

  override def keys: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "keys", {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "arrays", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "objects", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "iterables", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "booleans", {
        case null => Task.now(Nil)
        case bool: java.lang.Boolean => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "numbers", {
        case null => Task.now(Nil)
        case num: java.lang.Double => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "strings", {
        case null => Task.now(Nil)
        case str: String => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "nulls", {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[AnyRef] =
    noParamDefinition(
      "scalars", {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}

