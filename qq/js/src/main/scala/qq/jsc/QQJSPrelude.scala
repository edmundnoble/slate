package qq.jsc

import monix.eval.Task
import qq.QQCompiler.QQRuntimeException
import qq.jsc.QQJSCompiler.CompiledDefinition
import shapeless._0

import scala.scalajs.js

object QQJSPrelude extends QQJSCompiler.QQPrelude {
  override def length: CompiledDefinition[_0] =
    noParamDefinition(
      "length",
      {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str if str.isInstanceOf[String] => Task.now(str.asInstanceOf[String].length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def keys: CompiledDefinition[_0] =
    noParamDefinition(
      "keys",
      {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition[_0] =
    noParamDefinition(
      "arrays",
      {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[_0] =
    noParamDefinition(
      "objects",
      {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[_0] =
    noParamDefinition(
      "iterables",
      {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[_0] =
    noParamDefinition(
      "booleans",
      {
        case null => Task.now(Nil)
        case bool if bool.isInstanceOf[Boolean] => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[_0] =
    noParamDefinition(
      "numbers",
      {
        case null => Task.now(Nil)
        case num if num.isInstanceOf[Double] || num.isInstanceOf[Int] => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[_0] =
    noParamDefinition(
      "strings",
      {
        case null => Task.now(Nil)
        case str if str.isInstanceOf[String] => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[_0] =
    noParamDefinition(
      "nulls",
      {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[_0] =
    noParamDefinition(
      "values",
      {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[_0] =
    noParamDefinition(
      "scalars",
      {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
