package qq

import fastparse.core.{ParseError, Parsed}
import monix.eval.Task

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import Util._
import qq.QQCompiler.CompiledFilter

object Runner {

  def parseAndCompile[AnyTy](runtime: QQRuntime[AnyTy], program: String, optimize: Boolean = true): \/[Exception, CompiledFilter[AnyTy]] = {
    val parsed = Parser.program.parse(program)
    parsed match {
      case Parsed.Success((definitions, main), _) =>
        if (optimize) {
          QQCompiler.compileProgram(
            runtime,
            definitions.map(Definition.body.modify(Optimizer.optimize)),
            Optimizer.optimize(main)
          )
        } else {
          QQCompiler.compileProgram(runtime, definitions, main)
        }
      case f@Parsed.Failure(_, _, _) =>
        new ParseError(f).left
    }
  }

  def run[AnyTy](runtime: QQRuntime[AnyTy], qqProgram: String, optimize: Boolean = true)(input: List[AnyTy]): Task[List[AnyTy]] = {
    parseAndCompile(runtime, qqProgram, optimize).fold(
      Task.raiseError,
      input.traverseM(_)
    )
  }

}
