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

  def parseAndCompile[JsonTy](runtime: QQRuntime[JsonTy], program: String, optimize: Boolean = true): \/[QQCompilationException \/ ParseError, CompiledFilter[JsonTy]] = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = if (optimize) {
          Optimizer.optimize(parsedProgram)
        } else {
          parsedProgram
        }
        QQCompiler.compileProgram(runtime, IndexedSeq.empty, optimized).leftMap(_.left)
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  def run[JsonTy](runtime: QQRuntime[JsonTy], qqProgram: String, optimize: Boolean = true)(input: List[JsonTy]): Task[List[JsonTy]] = {
    parseAndCompile(runtime, qqProgram, optimize).fold(
      ex => Task.raiseError(ex.merge[Exception]),
      input.traverseM(_)
    )
  }

}
