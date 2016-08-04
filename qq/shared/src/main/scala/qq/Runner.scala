package qq

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import monix.scalaz._
import qq.QQCompiler.CompiledFilter

object Runner {

  def parseAndCompile[AnyTy](runtime: QQRuntime[AnyTy], program: String, optimize: Boolean = true): \/[QQCompilationException \/ ParseError, CompiledFilter[AnyTy]] = {
    Parser.program.parse(program) match {
      case Parsed.Success(program, _) =>
        val optimized = if (optimize) {
          Optimizer.optimize(program)
        } else {
          program
        }
        QQCompiler.compileProgram(runtime, optimized).leftMap(_.left)
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  def run[AnyTy](runtime: QQRuntime[AnyTy], qqProgram: String, optimize: Boolean = true)(input: List[AnyTy]): Task[List[AnyTy]] = {
    parseAndCompile(runtime, qqProgram, optimize).fold(
      ex => Task.raiseError(ex.merge[Exception]),
      input.traverseM(_)
    )
  }

}
