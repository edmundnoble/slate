package qq
package cc

import cats.implicits._
import fastparse.all.{ParseError, Parsed}
import monix.eval.Task
import qq.data.JSON
import qq.util.Recursion.RecursionEngine

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile(program: String)(implicit rec: RecursionEngine): (QQCompilationException Either ParseError) Either CompiledFilter = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = LocalOptimizer.optimizeProgram(parsedProgram)
        QQCompiler.compileProgram(Prelude.empty, optimized).leftMap(Either.left)
      case f: Parsed.Failure =>
        Either.left(Either.right(new ParseError(f)))
    }
  }

  // parse, compile, and run
  def run(qqProgram: String)(input: JSON)
         (implicit rec: RecursionEngine): (QQCompilationException Either ParseError) Either Task[RuntimeErrs Either List[JSON]] = {
    parseAndCompile(qqProgram).map(CompiledFilter.run(input, Map.empty, _))
  }

}
