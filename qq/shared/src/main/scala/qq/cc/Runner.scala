package qq
package cc

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.scalaz._
import qq.util.Recursion.RecursionEngine

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.traverse._

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile[J](runtime: QQRuntime[J], program: String)(implicit rec: RecursionEngine): \/[QQCompilationException \/ ParseError, CompiledFilter[J]] = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = LocalOptimizer.optimizeProgram(parsedProgram)
        QQCompiler.compileProgram(runtime, Prelude.empty, optimized).leftMap(_.left)
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  // parse, compile, and run
  def run[J](runtime: QQRuntime[J], qqProgram: String)(input: List[J])(implicit rec: RecursionEngine): Task[List[J]] = {
    parseAndCompile(runtime, qqProgram).fold(
      ex => Task.raiseError(ex.merge[Exception]),
      f => input.traverseM(f(Map.empty))
    )
  }

}
