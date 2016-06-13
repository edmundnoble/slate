package qq.jsc

import monix.eval.Task
import qq.QQCompiler.QQRuntimeException
import qq.jsc.QQJSCompiler.CompiledDefinition
import scala.scalajs.js

object QQJSPrelude extends QQJSCompiler.QQPrelude {
  override def length: CompiledDefinition =
    CompiledDefinition(
      "length",
      Nil, {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str if str.isInstanceOf[String] => Task.now(str.asInstanceOf[String].length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def keys: CompiledDefinition =
    CompiledDefinition(
      "keys",
      Nil, {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition =
    CompiledDefinition(
      "arrays",
      Nil, {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition =
    CompiledDefinition(
      "objects",
      Nil, {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition =
    CompiledDefinition(
      "iterables",
      Nil, {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition =
    CompiledDefinition(
      "booleans",
      Nil, {
        case null => Task.now(Nil)
        case bool if bool.isInstanceOf[Boolean] => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition =
    CompiledDefinition(
      "numbers",
      Nil, {
        case null => Task.now(Nil)
        case num if num.isInstanceOf[Double] || num.isInstanceOf[Int] => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition =
    CompiledDefinition(
      "strings",
      Nil, {
        case null => Task.now(Nil)
        case str if str.isInstanceOf[String] => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition =
    CompiledDefinition(
      "nulls",
      Nil, {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition =
    CompiledDefinition(
      "values",
      Nil, {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition =
    CompiledDefinition(
      "scalars",
      Nil, {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
