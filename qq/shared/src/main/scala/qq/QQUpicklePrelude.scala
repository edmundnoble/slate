package qq

import monix.eval.Task
import qq.QQUpickleCompiler._
import qq.QQCompiler.{QQCompilationException, QQRuntimeException}
import shapeless.{Sized, _0}
import upickle.Js
import scalaz._
import scalaz.syntax.either._

object QQUpicklePrelude extends QQUpickleCompiler.QQPrelude {
  override def length: CompiledDefinition =
    noParamDefinition(
      "length", {
        case arr: Js.Arr => Task.now(Js.Num(arr.value.length) :: Nil)
        case Js.Str(str) => Task.now(Js.Num(str.length) :: Nil)
        case obj: Js.Obj => Task.now(Js.Num(obj.value.length) :: Nil)
        case Js.Null => Task.now((Js.Num(0): Js.Value) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )


  override def keys: CompiledDefinition =
    noParamDefinition(
      "keys", {
        case obj: Js.Obj => Task.now(Js.Arr(obj.value.map(p => Js.Str(p._1)): _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition =
    noParamDefinition(
      "arrays",
       {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition =
    noParamDefinition(
      "objects",
       {
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition =
    noParamDefinition(
      "iterables",
       {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition =
    noParamDefinition(
      "booleans",
       {
        case bool@(Js.True | Js.False) => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition =
    noParamDefinition(
      "numbers",
       {
        case num: Js.Num => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition =
    noParamDefinition(
      "strings",
       {
        case str: Js.Str => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition =
    noParamDefinition(
      "nulls",
       {
        case Js.Null => Task.now(Js.Null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition =
    noParamDefinition(
      "values",
       {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition =
    noParamDefinition(
      "scalars",
       {
        case arr: Js.Arr => Task.now(Nil)
        case obj: Js.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
