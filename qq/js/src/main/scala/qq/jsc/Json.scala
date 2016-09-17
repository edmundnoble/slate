package qq.jsc

import qq.Platform.Rec._
import qq.util.Recursion.RecursiveFunction
import qq.util.Unsafe
import upickle.{Invalid, Js}

import scala.scalajs.js
import scalaz.Free.Trampoline
import scalaz.syntax.either._
import scalaz.{Trampoline, \/}

object Json {

  def stringToJs(s: String): Invalid.Json \/ js.Any = {
    try {
      js.JSON.parse(s).right
    } catch {
      case js.JavaScriptException(e: js.SyntaxError) =>
        Invalid.Json(e.message, s).left
    }
  }

  def jsToString(a: Any, space: Int = 0): String = {
    js.JSON.stringify(a.asInstanceOf[js.Any], null: js.Array[js.Any], js.Any.fromInt(space))
  }

  import qq.Platform.Js.Unsafe._

  final val jsToUpickleRec = new RecursiveFunction[Any, Js.Value] {
    override def run(value: Any, loop: Any => Trampoline[Js.Value]) =
      value match {
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
  }

  @inline final def stringToUpickle(s: String): Invalid.Json \/ Js.Value = {
    stringToJs(s).map(jsToUpickleRec(_))
  }

  @inline final val upickleToJsRec = new RecursiveFunction[Js.Value, js.Any] {
    override def run(value: Js.Value, loop: Js.Value => Trampoline[js.Any]) =
      value match {
        case Js.Str(s) => Trampoline.done(js.Any.fromString(s))
        case Js.Num(n) => Trampoline.done(js.Any.fromDouble(n))
        case Js.True => Trampoline.done(js.Any.fromBoolean(true))
        case Js.False => Trampoline.done(js.Any.fromBoolean(false))
        case Js.Null => Trampoline.done(null)
        case Js.Arr(children@_*) =>
          Unsafe.builderTraverse[Seq]
            .traverse[Trampoline, Js.Value, js.Any](children)(loop)
            .map(js.Array(_: _*))
        case obj: Js.Obj =>
          Unsafe.builderTraverse[Seq]
            .traverse[Trampoline, (String, Js.Value), (String, js.Any)](obj.value) { case (s, d) => loop(d) map (r => (s, r)) }
            .map(js.Dictionary[Any](_: _*))
      }
  }

  @inline final def upickleToString(upickle: Js.Value, indent: Int = 0): String = {
    js.JSON.stringify(
      upickleToJsRec(upickle),
      null: js.Function2[String, js.Any, js.Any],
      indent
    )
  }

}
