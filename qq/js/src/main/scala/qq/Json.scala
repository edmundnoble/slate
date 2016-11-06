package qq

import qq.Platform.Js._
import qq.data.JSON
import qq.data.JSON.ObjList
import qq.util.Recursion.{RecursionEngine, RecursiveFunction}
import qq.util.Unsafe
import upickle.{Invalid, Js}

import scala.scalajs.js
import cats.Eval
import cats.implicits._


object Json {

  def stringToJs(s: String): Invalid.Json Either js.Any =
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
    (value: Any, loop: Any => Eval[Js.Value]) => value match {
      case s: String => Eval.now(Js.Str(s))
      case n: Double => Eval.now(Js.Num(n))
      case true => Eval.now(Js.True)
      case false => Eval.now(Js.False)
      case null => Eval.now(Js.Null)
      case s: js.Array[Any@unchecked] =>
        Unsafe.builderTraverse[js.WrappedArray]
          .traverse[Eval, Any, Js.Value](new js.WrappedArray[Any](s))((a: Any) => loop(a))
          .map(Js.Arr(_: _*))
      case s: js.Object =>
        Unsafe.mapTraverse[String]
          .traverse[Eval, Any, Js.Value](s.toDictionary.toMap)(loop)
          .map(m => Js.Obj(m.toSeq: _*))
    }

  final val jsToJSONRec: RecursiveFunction[Any, JSON] =
    (value: Any, loop: Any => Eval[JSON]) => value match {
      case s: String => Eval.now(JSON.Str(s))
      case n: Double => Eval.now(JSON.Num(n))
      case true => Eval.now(JSON.True)
      case false => Eval.now(JSON.False)
      case null => Eval.now(JSON.Null)
      case s: js.Array[Any@unchecked] =>
        Unsafe.builderTraverse[js.WrappedArray]
          .traverse[Eval, Any, JSON](new js.WrappedArray[Any](s))((a: Any) => loop(a))
          .map(JSON.Arr(_: _*))
      case s: js.Object =>
        Unsafe.mapTraverse[String]
          .traverse[Eval, Any, JSON](s.toDictionary.toMap)(loop)
          .map(m => JSON.ObjMap(m))
    }

  @inline final def stringToJSON(s: String)(implicit r: RecursionEngine): Invalid.Json Either JSON =
    stringToJs(s).map(jsToJSONRec(_))

  @inline final val JSONToJsRec: RecursiveFunction[JSON, js.Any] =
    (value: JSON, loop: JSON => Eval[js.Any]) => value match {
      case JSON.Str(s) => Eval.now(js.Any.fromString(s))
      case JSON.Num(n) => Eval.now(js.Any.fromDouble(n))
      case JSON.True => Eval.now(js.Any.fromBoolean(true))
      case JSON.False => Eval.now(js.Any.fromBoolean(false))
      case JSON.Null => Eval.now(null)
      case JSON.Arr(children) =>
        Unsafe.builderTraverse[Seq]
          .traverse[Eval, JSON, js.Any](children)(loop)
          .map(js.Array(_: _*))
      case obj: JSON.Obj =>
        Unsafe.builderTraverse[Seq]
          .traverse[Eval, (String, JSON), (String, js.Any)](obj.toList.value) { case (s, d) => loop(d) map (r => (s, r)) }
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
