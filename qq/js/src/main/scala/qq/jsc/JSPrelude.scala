package qq.jsc

import java.util.regex.Pattern

import monix.eval.Task
import qq.Compiler.{CompiledFilter, QQCompilationException, QQRuntimeException}

import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.either._
import scala.scalajs.js
import com.thoughtworks.each.Monadic._
import qq.{CompiledDefinition, PlatformPrelude}
import qq.Util._

object JSPrelude extends PlatformPrelude[JSCompiler.type] {

  override def length: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "length", {
        case arr: js.Array[js.Any@unchecked] => Task.now(Int.box(arr.length) :: Nil)
        case str: String => Task.now(Int.box(str.length) :: Nil)
        case obj: js.Object => Task.now(Int.box(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length) :: Nil)
        case null => Task.now(Int.box(0) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def replaceAll: CompiledDefinition[JSCompiler.type] =
    CompiledDefinition[JSCompiler.type](name = "replaceAll", numParams = 2,
      body = { (params: List[CompiledFilter[JSCompiler.type]]) =>
        val (regex :: replacement :: Nil) = params
        ((jsv: AnyRef) =>
          for {
            // TODO: Make safe
            regexOut <- regex(jsv)
            replacementOut <- replacement(jsv)
            compiledRegex: Pattern = Pattern.compile(regexOut.head.asInstanceOf[String])
            replace = replacementOut.head.asInstanceOf[String]
          } yield js.Any.fromString(compiledRegex.matcher(jsv.asInstanceOf[String]).replaceAll(replace)) :: Nil
          ).right[QQCompilationException]
      }
    )

  override def keys: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "keys", {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "arrays", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "objects", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "iterables", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "booleans", {
        case null => Task.now(Nil)
        case bool: java.lang.Boolean => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "numbers", {
        case null => Task.now(Nil)
        case num: java.lang.Double => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "strings", {
        case null => Task.now(Nil)
        case str: String => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "nulls", {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: CompiledDefinition[JSCompiler.type] =
    noParamDefinition(
      "scalars", {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}

