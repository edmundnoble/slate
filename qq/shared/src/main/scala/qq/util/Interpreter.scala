package qq
package util

import monix.eval.Task
import qq.Platform.Rec._
import qq.cc._
import qq.data.JSON
import upickle.json

// I think this is just a free monad transformer
final case class Interpreter(name: String, resumePartial: PartialFunction[String, InterpreterExitException.type Either Task[(String, Interpreter)]]) {
  @inline def resume: String => Option[InterpreterExitException.type Either Task[(String, Interpreter)]] = resumePartial.lift
  @inline def orElse(other: Interpreter): Interpreter = Interpreter(other.name, resumePartial.orElse(other.resumePartial))
}

// used as an escape hatch from the interpreter
object InterpreterExitException extends Exception

object Interpreter {
  def taskSwitch: Interpreter = Interpreter(":p, :i:", {
    case ":p" => Right(Task.eval(("", programInterpreter)))
    case ":i" => Right(Task.eval(("", inputInterpreter)))
    case ":q" => Left(InterpreterExitException)
  })

  def programInterpreter: Interpreter = taskSwitch orElse
    Interpreter("program:", {
      case program =>
        Right(
          Runner.parseAndCompile(program).fold(
            err => Task.eval {
              val () = Console.err.println("Error: " + err.merge.getMessage)
              ("", programInterpreter)
            },
            compiledFilter => Task.delay {
              ("", programInterpreterOf(program, compiledFilter))
            }
          )
        )
    })

  def programInterpreterOf(source: String, program: CompiledFilter): Interpreter = taskSwitch orElse
    Interpreter("program " + source + ", input:", {
      case input =>
        val inputJs: JSON = JSON.upickleToJSONRec(json read input)
        val ranProgram = CompiledFilter.run(inputJs, Map.empty, program)
        Right(
          ranProgram.flatMap(_.fold[Task[(String, Interpreter)]]({ errs => Task.raiseError(QQRuntimeException(errs)) }, { outputs =>
            Task.now((outputs.map(JSON.render).mkString(", "), programInterpreterOf(source, program)))
          }))
        )
    })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter("input:", {
    case input =>
      val inputJs = JSON.upickleToJSONRec(json read input)
      Right(
        Task.now(("", inputInterpreterOf(input, inputJs)))
      )
  })

  def inputInterpreterOf(source: String, input: JSON): Interpreter = taskSwitch orElse
    Interpreter("input " + source + ", program:", {
      case program =>
        Right(
          Runner.parseAndCompile(program).fold(
            err => Task.eval {
              val () = Console.err.println("Error: " + err.merge.getMessage)
              ("", programInterpreter)
            }, { compiledFilter =>
              val result = CompiledFilter.run(input, Map.empty, compiledFilter)
              result.flatMap(_.fold(es => Task.raiseError(QQRuntimeException(es)), Task.now)).map { outputs =>
                (outputs.map(JSON.render).mkString(", "), inputInterpreterOf(source, input))
              }
            }
          )
        )
    })

  def mainMenu: Interpreter = taskSwitch

}
