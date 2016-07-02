package qq

import java.util.regex.Pattern

import monix.eval.Task
import qq.UpickleCompiler._
import qq.Compiler.{QQCompilationException, QQRuntimeException}
import upickle.Js

import scalaz._
import scalaz.Id._
import scalaz.syntax.either._
import com.thoughtworks.each.Monadic._
import upickle.Js.{False, Value}
import qq.Util._

object UpicklePrelude extends PlatformPrelude[UpickleCompiler.type] {
  override def length: UpickleCompiler.CDefinition =
    noParamDefinition(
      "length", {
        case arr: Js.Arr => Task.now(Js.Num(arr.value.length) :: Nil)
        case Js.Str(str) => Task.now(Js.Num(str.length) :: Nil)
        case obj: Js.Obj => Task.now(Js.Num(obj.value.length) :: Nil)
        case Js.Null => Task.now((Js.Num(0): Js.Value) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def keys: UpickleCompiler.CDefinition =
    noParamDefinition(
      "keys", {
        case obj: Js.Obj => Task.now(Js.Arr(obj.value.map(p => Js.Str(p._1)): _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def replaceAll: UpickleCompiler.CDefinition =
    CompiledDefinition[UpickleCompiler.type](name = "replaceAll", numParams = 2,
      body = { params =>
        val (regex :: replacement :: Nil) = params
        ((jsv: Js.Value) => {
          val result = monadic[Task] {
            val compiledRegex: Pattern = Pattern.compile(regex(jsv).each.head.str)
            jsv match {
              case Js.Str(string) =>
                val replace: String = replacement(jsv).each.head.str
                Js.Str(compiledRegex.matcher(string).replaceAll(replace)) :: Nil
              case _ => ???
            }
          }
          result
        }).right[QQCompilationException]
      }
    )

  override def arrays: UpickleCompiler.CDefinition =
    noParamDefinition(
      "arrays", {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: UpickleCompiler.CDefinition =
    noParamDefinition(
      "objects", {
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: UpickleCompiler.CDefinition =
    noParamDefinition(
      "iterables", {
        case arr: Js.Arr => Task.now(arr :: Nil)
        case obj: Js.Obj => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: UpickleCompiler.CDefinition =
    noParamDefinition(
      "booleans", {
        case bool@(Js.True | Js.False) => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: UpickleCompiler.CDefinition =
    noParamDefinition(
      "numbers", {
        case num: Js.Num => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: UpickleCompiler.CDefinition =
    noParamDefinition(
      "strings", {
        case str: Js.Str => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: UpickleCompiler.CDefinition =
    noParamDefinition(
      "nulls", {
        case Js.Null => Task.now(Js.Null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: UpickleCompiler.CDefinition =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: UpickleCompiler.CDefinition =
    noParamDefinition(
      "scalars", {
        case arr: Js.Arr => Task.now(Nil)
        case obj: Js.Obj => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}
