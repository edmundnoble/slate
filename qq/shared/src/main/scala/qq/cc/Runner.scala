package qq
package cc

import cats.data.{ValidatedNel, Xor}
import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import monix.cats._
import qq.data.JSON
import qq.util.Recursion.RecursionEngine
import qq.util._
import cats.implicits._

// tools for running parts of the compiler together
object Runner {

  def parseAndCompile(program: String)(implicit rec: RecursionEngine): (QQCompilationException Xor ParseError) Xor CompiledFilter = {
    Parser.program.parse(program) match {
      case Parsed.Success(parsedProgram, _) =>
        val optimized = LocalOptimizer.optimizeProgram(parsedProgram)
        QQCompiler.compileProgram(Prelude.empty, optimized).leftMap(_.left)
      case f: Parsed.Failure =>
        new ParseError(f).right[QQCompilationException].left
    }
  }

  // parse, compile, and run
  def run(qqProgram: String)(input: List[JSON])(implicit rec: RecursionEngine): (QQCompilationException Xor ParseError) Xor Task[ValidatedNel[QQRuntimeError, List[JSON]]] = {
    parseAndCompile(qqProgram).map(
      f => {
        val r = Task.gather(input.map(f(Map.empty)(_)))
        val x: Task[ValidatedNel[QQRuntimeError, List[List[JSON]]]] =
          r.map(_.traverse[ValidatedNel[QQRuntimeError, ?], List[JSON]](identity))
        x.map(_.map(_.flatten))
      }
    )
  }

}
