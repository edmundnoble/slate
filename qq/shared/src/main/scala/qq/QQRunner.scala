package qq

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task
import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import Util._
import matryoshka._

import qq.Definition

object QQRunner {

  def parseAndCompile(compiler: QQCompiler, program: String, optimize: Boolean): \/[Exception, compiler.CompiledFilter] = {
    val parsed = QQParser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        if (optimize) {
          compiler.compileProgram(
            definitions.map(defn => defn.modifyBody(QQOptimizer.optimize)),
            QQOptimizer.optimize(main)
          )
        } else {
          compiler.compileProgram(definitions, main)
        }
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).left
    }
  }

  def run(compiler: QQCompiler, qqProgram: String)(input: List[compiler.AnyTy]): Task[List[compiler.AnyTy]] = {
    parseAndCompile(compiler, qqProgram, optimize = true).fold(
      Task.raiseError,
      input.traverseM(_)
    )
  }

}
