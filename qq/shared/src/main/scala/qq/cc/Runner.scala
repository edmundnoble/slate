package qq
package cc

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.scalaz._
import qq.data.JSON
import qq.util.Recursion.RecursionEngine
import qq.util._

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.tag._
import scalaz.syntax.traverse._

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile(program: String)(implicit rec: RecursionEngine): (QQCompilationException \/ ParseError) \/ CompiledFilter = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = LocalOptimizer.optimizeProgram(parsedProgram)
        QQCompiler.compileProgram(Prelude.empty, optimized).leftMap(_.left)
      case f: Parsed.Failure =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  // parse, compile, and run
  def run(qqProgram: String)(input: List[JSON])(implicit rec: RecursionEngine): Task[List[JSON]] = {
    parseAndCompile(qqProgram).fold(
      ex => Task.raiseError(ex.merge[Exception]),
      f => input.traverseM[TaskParallel, JSON](f(Map.empty)(_).parallel).unwrap
    )
  }

}
