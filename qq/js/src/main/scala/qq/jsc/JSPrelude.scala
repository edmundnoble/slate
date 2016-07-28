package qq.jsc

import java.util.regex.Pattern

import com.thoughtworks.each.Monadic._
import monix.eval.Task
import monix.scalaz._
import qq._

import scala.scalajs.js
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.traverse._

object JSPrelude extends PlatformPrelude[Any] {

  override def length: CompiledDefinition[Any] =
    noParamDefinition(
      "length", {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str: String => Task.now(str.length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get length of $k"))
      }
    )


  override def replaceAll: CompiledDefinition[Any] =
    CompiledDefinition[Any](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => {
          (jsv: Any) =>
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
                    Task.now(regex.matcher(string).replaceAll(replacement): Any)
                  case j => Task.raiseError(QQRuntimeException(s"can't replace ${j.toString}"))
                }
              }.sequence[Task, Any].each
              valueRegexReplacementList
            }
        }.right[QQCompilationException]
      }
    )

  override def keys: CompiledDefinition[Any] =
    noParamDefinition(
      "keys", {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition[Any] =
    noParamDefinition(
      "arrays", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[Any] =
    noParamDefinition(
      "objects", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[Any] =
    noParamDefinition(
      "iterables", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[Any] =
    noParamDefinition(
      "booleans", {
        case null => Task.now(Nil)
        case bool: java.lang.Boolean => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[Any] =
    noParamDefinition(
      "numbers", {
        case null => Task.now(Nil)
        case num: java.lang.Double => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[Any] =
    noParamDefinition(
      "strings", {
        case null => Task.now(Nil)
        case str: String => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[Any] =
    noParamDefinition(
      "nulls", {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[Any] =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[Any] =
    noParamDefinition(
      "scalars", {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}

