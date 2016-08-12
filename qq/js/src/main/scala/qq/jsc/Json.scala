package qq.jsc

import qq.Rec.RecursiveFunction
import qq.{Rec, Unsafe}
import upickle.{Invalid, Js}

import scala.scalajs.js
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.syntax.traverse._

object Json {

  import Unsafe._

  // makes a shallow copy of arrays, mutators be warned!
  final val readJs = RecursiveFunction[Any, Js.Value] { (value, loop) =>
    value match {
      case s: String => Trampoline.done(Js.Str(s))
      case n: Double => Trampoline.done(Js.Num(n))
      case true => Trampoline.done(Js.True)
      case false => Trampoline.done(Js.False)
      case null => Trampoline.done(Js.Null)
      case s: js.Array[Any] =>
        (s: Seq[Any]).traverse[Trampoline, Js.Value]((a: Any) => loop(a)).map(Js.Arr(_: _*))
      case s: js.Object =>
        ToTraverseOps[Map[String, ?], Any](
          js.Any.wrapDictionary(s.asInstanceOf[js.Dictionary[Any]]).toMap)(
          Unsafe.mapTraverse[String]).traverse[Trampoline, Js.Value](loop).map(m => Js.Obj(m.toSeq: _*))
    }
  }

  @inline final def read(s: String): Js.Value = {
    val parsed = try {
      js.JSON.parse(s)
    } catch {
      case js.JavaScriptException(e: js.SyntaxError) =>
        throw Invalid.Json(e.message, s)
    }
    readJs(Rec.Unsafe.RecLimitStack(128), parsed)
  }

  @inline final val writeJs = RecursiveFunction[Js.Value, js.Any] { (value, loop) =>
    value match {
      case Js.Str(s) => Trampoline.done(js.Any.fromString(s))
      case Js.Num(n) => Trampoline.done(js.Any.fromDouble(n))
      case Js.True => Trampoline.done(js.Any.fromBoolean(true))
      case Js.False => Trampoline.done(js.Any.fromBoolean(false))
      case Js.Null => Trampoline.done(null)
      case Js.Arr(children@_*) =>
        children.traverse[Trampoline, js.Any](loop).map(js.Array(_: _*))
      case obj: Js.Obj =>
        obj.value.traverse[Trampoline, (String, js.Any)] { case (s, d) => loop(d) map (r => (s, r)) }.map(f => js.Dictionary[Any](f: _*))
    }
  }

  @inline final def write(upickle: Js.Value, indent: Int = 0): String = {
    js.JSON.stringify(
      writeJs(Rec.Unsafe.RecLimitStack(128), upickle),
      null: js.Function2[String, js.Any, js.Any],
      indent
    )
  }

}
