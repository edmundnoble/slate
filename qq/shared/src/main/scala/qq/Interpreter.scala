package qq

import monix.eval.Task
import qq.QQCompiler.CompiledFilter
import upickle.{Js, json}

import scalaz.\/
import scalaz.syntax.either._

object Interpreter {

  // used as an escape hatch from the interpreter
  object ExitException extends Exception

  // I think this is just a free monad transformer
  final case class Interpreter(name: String, resumePartial: PartialFunction[String, ExitException.type \/ Task[(String, Interpreter)]]) {
    def resume: String => Option[ExitException.type \/ Task[(String, Interpreter)]] = resumePartial.lift
    def orElse(other: Interpreter): Interpreter = Interpreter(other.name, resumePartial.orElse(other.resumePartial))
  }

  def taskSwitch: Interpreter = Interpreter(":p, :i:", {
    case ":p" => Task.evalAlways(("", programInterpreter)).right
    case ":i" => Task.evalAlways(("", inputInterpreter)).right
    case ":q" => ExitException.left
  })

  def programInterpreter: Interpreter = taskSwitch orElse Interpreter("program:", {
    case program =>
      Runner.parseAndCompile(UpickleRuntime, program, optimize = true).fold(
        err => Task.evalAlways {
          val () = Console.err.println(s"Error: ${err.merge[Exception].getMessage}")
          ("", programInterpreter)
        },
        (compiledFilter: CompiledFilter[Js.Value]) => {
          Task.now(("", programInterpreterOf(program, compiledFilter)))
        }
      ).right
  })

  def programInterpreterOf(source: String, program: CompiledFilter[Js.Value]): Interpreter = taskSwitch orElse Interpreter(s"program $source, input:", {
    case input =>
      val inputJs = json read input
      val outputTask = program(inputJs)
      outputTask.map { outputs =>
        (outputs.mkString("(", ", ", ")"), programInterpreterOf(source, program))
      }.right
  })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter("input:", {
    case input =>
      val inputJs = json read input
      Task.now(("", inputInterpreterOf(input, inputJs))).right
  })

  def inputInterpreterOf(source: String, input: Js.Value): Interpreter = taskSwitch orElse Interpreter(s"input $source, program:", {
    case program =>
      Runner.parseAndCompile(UpickleRuntime, program, optimize = true).fold(
        err => Task.evalAlways {
          val () = Console.err.println(s"Error: ${err.merge[Exception].getMessage}")
          ("", programInterpreter)
        },
        (compiledFilter: CompiledFilter[Js.Value]) => compiledFilter(input).map { outputs =>
          (outputs.mkString("(", ", ", ")"), inputInterpreterOf(source, input))
        }
      ).right
  })

  def mainMenu: Interpreter = taskSwitch

}
