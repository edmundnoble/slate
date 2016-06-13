package qq.jsc

import monix.eval.Task
import qq.QQCompiler.QQRuntimeException
import qq.jsc.QQJSCompiler.CompiledDefinition
import scala.scalajs.js

object QQJSPrelude extends QQJSCompiler.QQPrelude {
  override def length: CompiledDefinition =
    CompiledDefinition(
      "length",
      Nil,
      {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str if str.isInstanceOf[String] => Task.now(str.asInstanceOf[String].length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )
}
