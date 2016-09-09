package qq

import monix.eval.Task
import qq.QQCompiler.CompiledFilter
import upickle.{Js, json}

import scalaz.\/
import scalaz.syntax.either._

// I think this is just a free monad transformer
final case class Interpreter(name: String, resumePartial: PartialFunction[String, InterpreterExitException.type \/ Task[(String, Interpreter)]]) {
  @inline def resume: String => Option[InterpreterExitException.type \/ Task[(String, Interpreter)]] = resumePartial.lift
  @inline def orElse(other: Interpreter): Interpreter = Interpreter(other.name, resumePartial.orElse(other.resumePartial))
}

// used as an escape hatch from the interpreter
object InterpreterExitException extends Exception

object Interpreter {
  def taskSwitch: Interpreter = Interpreter(":p, :i:", {
    case ":p" => Task.eval(("", programInterpreter)).right
    case ":i" => Task.eval(("", inputInterpreter)).right
    case ":q" => InterpreterExitException.left
  })

  def programInterpreter: Interpreter = taskSwitch orElse
    Interpreter("program:", {
      case program =>
        Runner.parseAndCompile(UpickleRuntime, program, optimize = true).fold(
          err => Task.eval {
            val () = Console.err.println("Error: " + err.merge[Exception].getMessage)
            ("", programInterpreter)
          },
          compiledFilter => Task.defer {
            Task.now(("", programInterpreterOf(program, compiledFilter)))
          }
        ).right
    })

  def programInterpreterOf(source: String, program: CompiledFilter[Js.Value]): Interpreter = taskSwitch orElse
    Interpreter("program " + source + ", input:", {
      case input =>
        val inputJs = json read input
        val outputTask = program(Nil)(inputJs)
        outputTask.map { outputs =>
          (outputs.mkString(", "), programInterpreterOf(source, program))
        }.right
    })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter("input:", {
    case input =>
      val inputJs = json read input
      Task.now(("", inputInterpreterOf(input, inputJs))).right
  })

  def inputInterpreterOf(source: String, input: Js.Value): Interpreter = taskSwitch orElse
    Interpreter("input " + source + ", program:", {
      case program =>
        Runner.parseAndCompile(UpickleRuntime, program, optimize = true).fold(
          err => Task.eval {
            val () = Console.err.println("Error: " + err.merge[Exception].getMessage)
            ("", programInterpreter)
          },
          (compiledFilter: CompiledFilter[Js.Value]) => compiledFilter(Nil)(input).map { outputs =>
            (outputs.mkString(", "), inputInterpreterOf(source, input))
          }
        ).right
    })

  def mainMenu: Interpreter = taskSwitch

}
