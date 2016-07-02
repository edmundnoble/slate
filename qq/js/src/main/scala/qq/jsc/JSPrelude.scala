package qq.jsc

import java.util.regex.Pattern

import monix.eval.Task
import qq.Compiler.{QQCompilationException, QQRuntimeException}

import scalaz.std.option._
import scalaz.syntax.std.option._
import scalaz.syntax.either._
import scala.scalajs.js
import com.thoughtworks.each.Monadic._
import qq.{CompiledDefinition, PlatformPrelude}
import qq.Util._

object JSPrelude extends PlatformPrelude[JSCompiler.type] {

  override def length: JSCompiler.CDefinition =
    noParamDefinition(
      "length", {
        case arr: js.Array[js.Any@unchecked] => Task.now(arr.length :: Nil)
        case str if str.isInstanceOf[String] => Task.now(str.asInstanceOf[String].length :: Nil)
        case obj: js.Object => Task.now(obj.asInstanceOf[js.Dictionary[js.Any]].toArray.length :: Nil)
        case null => Task.now(0 :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get length of $k"))
      }
    )

  override def replaceAll: JSCompiler.CDefinition =
    CompiledDefinition[JSCompiler.type](name = "replaceAll", numParams = 2,
      body = { (params: List[JSCompiler.CompiledFilter]) =>
        val (regex :: replacement :: Nil) = params
        ((jsv: js.Any) =>
          for {
            regexOut <- regex(jsv)
            replacementOut <- replacement(jsv)
            compiledRegex: Pattern = Pattern.compile(regexOut.head.asInstanceOf[String])
            replace = replacementOut.head.asInstanceOf[String]
          } yield js.Any.fromString(compiledRegex.matcher(jsv.asInstanceOf[String]).replaceAll(replace)) :: Nil
          ).right[QQCompilationException]
      }
    )

  override def keys: JSCompiler.CDefinition =
    noParamDefinition(
      "keys", {
        case obj: js.Object => Task.now(js.Array(obj.asInstanceOf[js.Dictionary[js.Any]].keys.toSeq: _*) :: Nil)
        case k => Task.raiseError(new QQRuntimeException(s"Tried to get keys of $k"))
      }
    )

  override def arrays: JSCompiler.CDefinition =
    noParamDefinition(
      "arrays", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case k => Task.now(Nil)
      })

  override def objects: JSCompiler.CDefinition =
    noParamDefinition(
      "objects", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def iterables: JSCompiler.CDefinition =
    noParamDefinition(
      "iterables", {
        case null => Task.now(Nil)
        case arr: js.Array[_] => Task.now(arr :: Nil)
        case obj: js.Object => Task.now(obj :: Nil)
        case k => Task.now(Nil)
      })

  override def booleans: JSCompiler.CDefinition =
    noParamDefinition(
      "booleans", {
        case null => Task.now(Nil)
        case bool if bool.isInstanceOf[Boolean] => Task.now(bool :: Nil)
        case k => Task.now(Nil)
      })

  override def numbers: JSCompiler.CDefinition =
    noParamDefinition(
      "numbers", {
        case null => Task.now(Nil)
        case num if num.isInstanceOf[Double] || num.isInstanceOf[Int] => Task.now(num :: Nil)
        case k => Task.now(Nil)
      })

  override def strings: JSCompiler.CDefinition =
    noParamDefinition(
      "strings", {
        case null => Task.now(Nil)
        case str if str.isInstanceOf[String] => Task.now(str :: Nil)
        case k => Task.now(Nil)
      })

  override def nulls: JSCompiler.CDefinition =
    noParamDefinition(
      "nulls", {
        case null => Task.now(null :: Nil)
        case k => Task.now(Nil)
      })

  override def values: JSCompiler.CDefinition =
    noParamDefinition(
      "values", {
        case null => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

  override def scalars: JSCompiler.CDefinition =
    noParamDefinition(
      "scalars", {
        case arr: js.Array[_] => Task.now(Nil)
        case obj: js.Object => Task.now(Nil)
        case k => Task.now(k :: Nil)
      })

}

