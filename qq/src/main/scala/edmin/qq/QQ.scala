package edmin.qq

import edmin.qq.QQAST.{CompiledFilter, Definition, QQCompilationException}
import fastparse.core.{ParseError, Parsed}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monocle.macros._
import monocle.Lens

import scala.scalajs.js
import scala.scalajs.js.{Any, |}
import scala.scalajs.js.|._
import scalaz.{-\/, Monad, Monoid, \/}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.functor._
import scalaz.syntax.arrow._
import scalaz.std.function._
import scalaz.std.partialFunction._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.std.string._
import scalaz.std.option._
import Util._
import upickle.Js
import upickle.json

import scala.io.StdIn

object QQ {

  def parseAndCompile(program: String, optimize: Boolean): \/[QQCompilationException | ParseError, CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        val optimizedDefinitions = if (optimize) {
          definitions.map(Definition.body.modify(QQAST.optimize(_).runAttempt.value))
        } else {
          definitions
        }
        QQAST.compileProgram(optimizedDefinitions, main).leftMap(|.from(_))
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
