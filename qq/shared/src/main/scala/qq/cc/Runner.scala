package qq
package cc

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.scalaz._

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.traverse._

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile[J](runtime: QQRuntime[J], program: String): \/[QQCompilationException \/ ParseError, CompiledFilter[J]] = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = LocalOptimizer.optimizeProgram(parsedProgram)
        QQCompiler.compileProgram(runtime, Prelude.preludeMonoid.empty, optimized).leftMap(_.left)
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  // parse, compile, and run
  def run[J](runtime: QQRuntime[J], qqProgram: String)(input: List[J]): Task[List[J]] = {
    parseAndCompile(runtime, qqProgram).fold(
      ex => Task.raiseError(ex.merge[Exception]),
      f => input.traverseM(f(Map.empty))
    )
  }

}
