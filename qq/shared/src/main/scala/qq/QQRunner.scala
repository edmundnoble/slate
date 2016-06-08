package qq

import qq.QQAST._
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import Util._

object QQRunner {

  def parseAndCompile(compiler: QQCompiler, program: String, optimize: Boolean): \/[Exception, compiler.CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        val optimizedDefinitions = if (optimize) {
          definitions.map(Definition.body.modify(QQAST.optimize(_).runAttempt.value))
        } else {
          definitions
        }
        compiler.compileProgram(optimizedDefinitions, main)
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).left
    }
  }

  def run(compiler: QQCompiler, qqProgram: String)(input: List[compiler.AnyTy]): Task[List[compiler.AnyTy]] = {
    parseAndCompile(compiler, qqProgram, optimize = true).fold(
      Task.raiseError,
      input.traverse(_).map(_.flatten)
    )
  }

}
