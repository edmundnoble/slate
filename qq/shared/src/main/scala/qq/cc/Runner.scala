package qq
package cc

import cats.data.{NonEmptyList, ValidatedNel}
import fastparse.all.{ParseError, Parsed}
import monix.eval.Task
import monix.cats._
import qq.data.JSON
import qq.util.Recursion.RecursionEngine
import qq.util._
import cats.implicits._
import org.atnos.eff._, Eff._, syntax.all._

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
         (implicit rec: RecursionEngine): (QQCompilationException Either ParseError) Either Task[ValidatedNel[QQRuntimeError, List[JSON]]] = {
    parseAndCompile(qqProgram).map(CompiledFilter.run(input, Map.empty, _))
  }

}
