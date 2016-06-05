package edmin.qq.jsc

import edmin.qq.QQAST._
import edmin.qq.QQCompiler._
import edmin.qq.{QQAST, QQParser, Util}
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.reactive.Observable
import monocle.macros._
import monocle.Lens

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.|._
import scalaz.{-\/, \/}
import scalaz.std.list._
import scalaz.syntax.traverse._
import Util._
import upickle.Js

object QQJS {

  def parseAndCompile(program: String, optimize: Boolean): \/[QQCompilationException | ParseError, QQJSCompiler.CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        val optimizedDefinitions = if (optimize) {
          definitions.map(Definition.body.modify(QQAST.optimize(_).runAttempt.value))
        } else {
          definitions
        }
        QQJSCompiler.compileProgram(optimizedDefinitions, main).leftMap(|.from(_))
      case f@Parsed.Failure(_, _, _) =>
        -\/(new ParseError(f))
    }
  }

  def run(qqProgram: String, input: List[js.Any]): Task[List[js.Any]] = {
    parseAndCompile(qqProgram, optimize = true).fold(
      (err: QQCompilationException | ParseError) => Task.raiseError(err.merge[Exception]),
      compiledFilter => input.traverse(compiledFilter).map(_.flatten)
    )
  }


}
