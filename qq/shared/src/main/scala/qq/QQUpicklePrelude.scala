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

  override def arrays: CompiledDefinition =
    CompiledDefinition(
      "arrays",
      Nil, {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition =
    CompiledDefinition(
      "objects",
      Nil, {
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition =
    CompiledDefinition(
      "iterables",
      Nil, {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition =
    CompiledDefinition(
      "booleans",
      Nil, {
        case bool@(Js.True | Js.False) => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition =
    CompiledDefinition(
      "numbers",
      Nil, {
        case num: Js.Num => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition =
    CompiledDefinition(
      "strings",
      Nil, {
        case str: Js.Str => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition =
    CompiledDefinition(
      "nulls",
      Nil, {
        case Js.Null => Task.now(Js.Null :: Nil)
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
        case arr: Js.Arr => Task.now(Nil)
        case obj: Js.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
