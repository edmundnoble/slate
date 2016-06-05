package edmin.qq

import edmin.qq.QQAST._
import edmin.qq.QQCompiler._
import edmin.qq.{QQAST, QQParser, Util}
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.reactive.Observable
import monocle.macros._
import monocle.Lens

import scalaz.{-\/, \/}
import scalaz.std.list._
import scalaz.syntax.traverse._
import Util._
import upickle.Js

object QQ {

  def parseAndCompile(program: String, optimize: Boolean): \/[Exception, QQSharedCompiler.CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        val optimizedDefinitions = if (optimize) {
          definitions.map(Definition.body.modify(QQAST.optimize(_).runAttempt.value))
        } else {
          definitions
        }
        QQSharedCompiler.compileProgram(optimizedDefinitions, main)
      case f@Parsed.Failure(_, _, _) =>
        -\/(new ParseError(f))
    }
  }

  def run(qqProgram: String, input: List[Js.Value]): Task[List[Js.Value]] = {
    parseAndCompile(qqProgram, optimize = true).fold(
      Task.raiseError,
      compiledFilter => input.traverse(compiledFilter).map(_.flatten)
    )
  }

}
