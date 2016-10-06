package qq

import qq.Platform.Js._
import qq.data.JSON
import qq.data.JSON.ObjList
import qq.util.Recursion.{RecursionEngine, RecursiveFunction}
import qq.util.Unsafe
import upickle.{Invalid, Js}

import scala.scalajs.js
import scalaz.Free.Trampoline
import scalaz.syntax.either._
import scalaz.{Trampoline, \/}

object Json {

  def stringToJs(s: String): Invalid.Json \/ js.Any =
    try {
      js.JSON.parse(s).right
    } catch {
      case js.JavaScriptException(e: js.SyntaxError) =>
        Invalid.Json(e.message, s).left
    }

  def jsToString(a: Any, space: Int = 0): String =
    js.JSON.stringify(a.asInstanceOf[js.Any], null: js.Array[js.Any], js.Any.fromInt(space))

  def jsonToString(a: Any, space: Int = 0)(implicit rec: RecursionEngine): String =
    jsToString(jsToJSONRec(a), space)

  import qq.Platform.Js.Unsafe._

  final val jsToUpickleRec: RecursiveFunction[Any, Js.Value] =
    (value: Any, loop: Any => Trampoline[Js.Value]) => value match {
      case s: String => Trampoline.done(Js.Str(s))
      case n: Double => Trampoline.done(Js.Num(n))
      case true => Trampoline.done(Js.True)
      case false => Trampoline.done(Js.False)
      case null => Trampoline.done(Js.Null)
      case s: js.Array[Any@unchecked] =>
        Unsafe.builderTraverse[js.WrappedArray]
          .traverse[Trampoline, Any, Js.Value](new js.WrappedArray[Any](s))((a: Any) => loop(a))
          .map(Js.Arr(_: _*))
      case s: js.Object =>
        Unsafe.mapTraverse[String]
          .traverse[Trampoline, Any, Js.Value](s.toDictionary.toMap)(loop)
          .map(m => Js.Obj(m.toSeq: _*))
    }

  final val jsToJSONRec: RecursiveFunction[Any, JSON] =
    (value: Any, loop: Any => Trampoline[JSON]) => value match {
      case s: String => Trampoline.done(JSON.Str(s))
      case n: Double => Trampoline.done(JSON.Num(n))
      case true => Trampoline.done(JSON.True)
      case false => Trampoline.done(JSON.False)
      case null => Trampoline.done(JSON.Null)
      case s: js.Array[Any@unchecked] =>
        Unsafe.builderTraverse[js.WrappedArray]
          .traverse[Trampoline, Any, JSON](new js.WrappedArray[Any](s))((a: Any) => loop(a))
          .map(JSON.Arr(_: _*))
      case s: js.Object =>
        Unsafe.mapTraverse[String]
          .traverse[Trampoline, Any, JSON](s.toDictionary.toMap)(loop)
          .map(m => JSON.ObjMap(m))
    }

  @inline final def stringToJSON(s: String)(implicit r: RecursionEngine): Invalid.Json \/ JSON =
    stringToJs(s).map(jsToJSONRec(_))

  @inline final val JSONToJsRec: RecursiveFunction[JSON, js.Any] =
    (value: JSON, loop: JSON => Trampoline[js.Any]) => value match {
      case JSON.Str(s) => Trampoline.done(js.Any.fromString(s))
      case JSON.Num(n) => Trampoline.done(js.Any.fromDouble(n))
      case JSON.True => Trampoline.done(js.Any.fromBoolean(true))
      case JSON.False => Trampoline.done(js.Any.fromBoolean(false))
      case JSON.Null => Trampoline.done(null)
      case JSON.Arr(children) =>
        Unsafe.builderTraverse[Seq]
          .traverse[Trampoline, JSON, js.Any](children)(loop)
          .map(js.Array(_: _*))
      case obj: JSON.Obj =>
        Unsafe.builderTraverse[Seq]
          .traverse[Trampoline, (String, JSON), (String, js.Any)](obj.toList.value) { case (s, d) => loop(d) map (r => (s, r)) }
          .map(js.Dictionary[Any](_: _*))
    }


  @inline final def JSONToString(json: JSON, indent: Int = 0)(implicit rec: RecursionEngine): String = {
    js.JSON.stringify(
      JSONToJsRec(json),
      null: js.Function2[String, js.Any, js.Any],
      indent
    )
  }

}
