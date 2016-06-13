package qq

import monix.eval.Task
import qq.QQUpickleCompiler.CompiledDefinition
import qq.QQCompiler.QQRuntimeException
import upickle.Js

object QQUpicklePrelude extends QQUpickleCompiler.QQPrelude {
  override def length: CompiledDefinition =
    CompiledDefinition(
      "length",
      Nil, {
        case arr: Js.Arr => Task.now(Js.Num(arr.value.length) :: Nil)
        case Js.Str(str) => Task.now(Js.Num(str.length) :: Nil)
        case obj: Js.Obj => Task.now(Js.Num(obj.value.length) :: Nil)
        case Js.Null => Task.now(Js.Num(0) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def keys: CompiledDefinition =
    CompiledDefinition(
      "keys",
      Nil, {
        case obj: Js.Obj => Task.now(Js.Arr(obj.value.map(p => Js.Str(p._1)): _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

}
