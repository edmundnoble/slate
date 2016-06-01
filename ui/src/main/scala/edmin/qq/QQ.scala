package edmin.qq

import edmin.qq.QQAST.{CompiledFilter, Definition, QQCompilationException}
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.reactive.Observable
import monocle.macros._
import monocle.Lens

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.|._
import scalaz.{-\/, Monad, \/}
import scalaz.std.list._
import scalaz.syntax.traverse._
import Util._

object QQ {

  def parseAndCompile(program: String, optimize: Boolean): \/[QQCompilationException | ParseError, CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        val optimizedDefinitions = if (optimize) {
          definitions.map(Definition.body.modify(QQAST.optimize))
        } else {
          definitions
        }
        QQAST.compileProgram(optimizedDefinitions, main).leftMap(e => e)
      case f@Parsed.Failure(_, _, _) =>
        -\/(new ParseError(f))
    }
  }

  def run(qqProgram: String, input: List[js.Any]): Task[Seq[js.Any]] = {
    parseAndCompile(qqProgram, optimize = true).fold({ e => Task.raiseError(e.merge[Exception]) }, compiledFilter => input.traverse(compiledFilter).map(_.flatten))
  }

}
