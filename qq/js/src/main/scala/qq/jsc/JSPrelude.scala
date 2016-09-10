package qq.jsc

import java.util.regex.Pattern

import com.thoughtworks.each.Monadic._
import monix.eval.Task
import monix.scalaz._
import qq.QQCompiler.{BindingsByName, CompiledFilter, FOut, VarBinding}
import qq._
import qq.ajax.{Ajax, AjaxMethod}
import scodec.bits.ByteVector

import scala.concurrent.duration._
import scala.scalajs.js
import scalaz.Reader
import scalaz.std.list._
import scalaz.std.function._
import scalaz.syntax.either._
import scalaz.syntax.monadPlus._
import scalaz.syntax.traverse._

object JSPrelude extends PlatformPrelude[Any] {

  import CompiledDefinition.noParamDefinition

  def `null`: CompiledDefinition[Any] = noParamDefinition("null", CompiledFilter.const(null))

  def `true`: CompiledDefinition[Any] = noParamDefinition("true", CompiledFilter.const(true))

  def `false`: CompiledDefinition[Any] = noParamDefinition("false", CompiledFilter.const(false))

  def orElse: CompiledDefinition[Any] = CompiledDefinition[Any]("orElse", 1, {
    case (default :: Nil) => ((bindings: BindingsByName[Any]) => {
      case null => default(bindings)(null)
      case k => Task.now(k :: Nil)
    }: FOut[Any]).right[QQCompilationException]
  })

  def b64Encode: CompiledDefinition[Any] = noParamDefinition("b64Encode", CompiledFilter.func {
    case str: String => Task.now(ByteVector.encodeUtf8(str).right.getOrElse(ByteVector.empty).toBase64 :: Nil)
    case k => Task.raiseError(QQRuntimeException("Tried to get base64 encoding of " + JSRuntime.print(k)))
  })

  override def length: CompiledDefinition[Any] =
    noParamDefinition(
      "length", CompiledFilter.func {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str: String => Task.now(str.length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get length of " + JSRuntime.print(k)))
      }
    )

  override def replaceAll: CompiledDefinition[Any] =
    CompiledDefinition[Any](name = "replaceAll", numParams = 2,
      body = {
        case (regexFilter :: replacementFilter :: Nil) => ((bindings: BindingsByName[Any]) => {
          (jsv: Any) =>
            monadic[Task] {
              val regexes: List[Pattern] = regexFilter(bindings)(jsv).each.traverse[Task, Pattern] {
                case string: String => Task.now(Pattern.compile(string))
                case j => Task.raiseError(NotARegex(JSRuntime.print(j)))
              }.each
              val replacements: List[String] = replacementFilter(bindings)(jsv).each.traverse[Task, String] {
                case string: String => Task.now(string)
                case j => Task.raiseError(QQRuntimeException("can't replace with " + JSRuntime.print(j)))
              }.each
              val valueRegexReplacementList = (regexes, replacements).zipped.map { (regex, replacement) =>
                jsv match {
                  case string: String =>
                    Task.now(regex.matcher(string).replaceAll(replacement): Any)
                  case j =>
                    Task.raiseError(QQRuntimeException("can't replace " + JSRuntime.print(j)))
                }
              }.sequence[Task, Any].each
              valueRegexReplacementList
            }
        }).right[QQCompilationException]
      }
    )

  override def keys: CompiledDefinition[Any] =
    noParamDefinition(
      "keys", CompiledFilter.func {
        case obj: js.Object => Task.now(js.Object.keys(obj) :: Nil)
        case k => Task.raiseError(QQRuntimeException("Tried to get keys of " + JSRuntime.print(k)))
      }
    )

  override def arrays: CompiledDefinition[Any] =
    noParamDefinition(
      "arrays", CompiledFilter.func {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case _ => Task.now(Nil)
      })

  override def objects: CompiledDefinition[Any] =
    noParamDefinition(
      "objects", CompiledFilter.func {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[Any] =
    noParamDefinition(
      "iterables", CompiledFilter.func {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case _ => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[Any] =
    noParamDefinition(
      "booleans", CompiledFilter.func {
        case null => Task.now(Nil)
        case bool: java.lang.Boolean => Task.now(bool :: Nil)
        case _ => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[Any] =
    noParamDefinition(
      "numbers", CompiledFilter.func {
        case null => Task.now(Nil)
        case num: java.lang.Double => Task.now(num :: Nil)
        case _ => Task.now(Nil)
      })

  override def strings: CompiledDefinition[Any] =
    noParamDefinition(
      "strings", CompiledFilter.func {
        case null => Task.now(Nil)
        case str: String => Task.now(str :: Nil)
        case _ => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[Any] =
    noParamDefinition(
      "nulls", CompiledFilter.func {
        case null => Task.now(null :: Nil)
        case _ => Task.now(Nil)
      })

  override def values: CompiledDefinition[Any] =
    noParamDefinition(
      "values", CompiledFilter.func {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[Any] =
    noParamDefinition(
      "scalars", CompiledFilter.func {
        case _: js.Array[_] => Task.now(Nil)
        case _: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def includes: CompiledDefinition[Any] =
    CompiledDefinition[Any]("includes", 1, { case List(elem) =>
      ((bindings: BindingsByName[Any]) => (v: Any) => v match {
        case arr: js.Array[_] => elem(bindings)(v).map(f => f.map(v => java.lang.Boolean.valueOf(arr.contains(v))))
        case obj: js.Object => elem(bindings)(v).map(f => f.map(v => java.lang.Boolean.valueOf(obj.asInstanceOf[js.Dictionary[Any]].values.exists(_ == v))))
        case k => Task.raiseError(TypeError("array|object", String.valueOf(k)))
      }).right
    })

  // array/object existential predicate transformer
//  override def exists: CompiledDefinition[Any] =
//  CompiledDefinition[Any]("exists", 1, { case List(pred) =>
//    ((bindings: BindingsByName[Any]) => (v: Any) => v match {
//      case arr: js.Array[_] => pred(bindings)(v).map(f => f.map(v => if (v.isInstanceOfjava.lang.Boolean.valueOf(arr.xs(v))))
//      case obj: js.Object => pred(bindings)(v).map(f => f.map(v => java.lang.Boolean.valueOf(obj.asInstanceOf[js.Dictionary[Any]].values.exists(_ == v))))
//      case k => Task.raiseError(TypeError("array|object", String.valueOf(k)))
//    }).right
//  })

//   array/object universal predicate transformer
//  override def forall: CompiledDefinition[Any]


}

