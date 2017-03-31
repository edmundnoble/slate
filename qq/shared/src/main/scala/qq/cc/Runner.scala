package qq
package cc

import cats.implicits._
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import qq.data.JSON
import qq.util.Recursion.RecursionEngine

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile(program: String)(implicit rec: RecursionEngine): (CompileError Either ParseError[Char, String]) Either InterpretedFilter = {
    val compiler = new ParserCompiler(QQInterpreterRuntime, JSONPrelude)
    compiler.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        parsedProgram.leftMap(Either.left)
      case f: Parsed.Failure[Char, String] =>
        Either.left(Either.right(ParseError(f)))
    }
  }

  // parse, compile, and run
  def run(qqProgram: String)(input: JSON)
         (implicit rec: RecursionEngine): (CompileError Either ParseError[Char, String]) Either Task[RuntimeErrs Either Vector[JSON]] = {
    parseAndCompile(qqProgram).map(InterpretedFilter.run(input, Map.empty, _))
  }

}
