package qq

import monix.eval.Task
import qq.Compiler.CompiledFilter
import upickle.{Js, json}

object Interpreter {

  // used as an escape hatch from the interpreter
  final case class ExitException() extends Exception("exit", null, false, false)

  final case class Interpreter(name: String, run: PartialFunction[String, Task[(String, Interpreter)]]) {
    def orElse(other: Interpreter): Interpreter = Interpreter(other.name, run.orElse(other.run))
  }

  def taskSwitch: Interpreter = Interpreter(":p, :i:", {
    case ":p" => Task.evalAlways(("", programInterpreter))
    case ":i" => Task.evalAlways(("", inputInterpreter))
    case ":q" => Task.defer {
      val () = println("Bye!")
      Task.raiseError(ExitException())
    }
  })

  def programInterpreter: Interpreter = taskSwitch orElse Interpreter("program:", {
    case program =>
      Runner.parseAndCompile(UpickleCompiler, program, optimize = true).fold(
        (err: Exception) => Task.evalAlways {
          val () = Console.err.println(s"Error: $err")
          ("", programInterpreter)
        },
        (compiledFilter: CompiledFilter[UpickleCompiler.type]) => Task.now(("", programInterpreterOf(program, compiledFilter)))
      )
  })

  def programInterpreterOf(source: String, program: CompiledFilter[UpickleCompiler.type]): Interpreter = taskSwitch orElse Interpreter(s"program $source, input: ", {
    case input =>
      val inputJs = json read input
      val outputTask = program(inputJs)
      outputTask.map { outputs =>
        (outputs.mkString("(", ", ", ")"), programInterpreterOf(source, program))
      }
  })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter("input: ", {
    case input =>
      val inputJs = json read input
      Task.now(("", inputInterpreterOf(input, inputJs)))
  })

  def inputInterpreterOf(source: String, input: Js.Value): Interpreter = taskSwitch orElse Interpreter(s"input $source, program: ", {
    case program =>
      Runner.parseAndCompile(UpickleCompiler, program, optimize = true).fold(
        (err: Exception) => Task.evalAlways {
          val () = Console.err.println(s"Error: $err")
          ("", programInterpreter)
        },
        (compiledFilter: CompiledFilter[UpickleCompiler.type]) => compiledFilter(input).map { outputs =>
          (outputs.mkString("(", ", ", ")"), inputInterpreterOf(source, input))
        }
      )
  })

  def run: Interpreter = taskSwitch

}
