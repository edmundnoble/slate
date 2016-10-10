package qq
package util

import monix.eval.Task
import qq.cc.{CompiledFilter, Runner}
import qq.data.JSON
import upickle.json
import qq.Platform.Rec._

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
        Runner.parseAndCompile(program).fold(
          err => Task.eval {
            val () = Console.err.println("Error: " + err.merge[Exception].getMessage)
            ("", programInterpreter)
          },
          compiledFilter => Task.delay {
            ("", programInterpreterOf(program, compiledFilter))
          }
        ).right
    })

  def programInterpreterOf(source: String, program: CompiledFilter): Interpreter = taskSwitch orElse
    Interpreter("program " + source + ", input:", {
      case input =>
        val inputJs = JSON.upickleToJSONRec(json read input)
        val outputTask = program(Map.empty)(inputJs)
        outputTask.map { outputs =>
          (outputs.map(JSON.render).mkString(", "), programInterpreterOf(source, program))
        }.right
    })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter("input:", {
    case input =>
      val inputJs = JSON.upickleToJSONRec(json read input)
      Task.now(("", inputInterpreterOf(input, inputJs))).right
  })

  def inputInterpreterOf(source: String, input: JSON): Interpreter = taskSwitch orElse
    Interpreter("input " + source + ", program:", {
      case program =>
        Runner.parseAndCompile(program).fold(
          err => Task.eval {
            val () = Console.err.println("Error: " + err.merge[Exception].getMessage)
            ("", programInterpreter)
          },
          compiledFilter => compiledFilter(Map.empty)(input).map { outputs =>
            (outputs.map(JSON.render).mkString(", "), inputInterpreterOf(source, input))
          }
        ).right
    })

  def mainMenu: Interpreter = taskSwitch

}
