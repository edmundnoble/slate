package qq

import qq.UpickleCompiler._
import monix.eval.Task
import qq.Compiler.CompiledFilter
import upickle.{Js, json}

object Interpreter {

  // used as an escape hatch from interpreter
  final case class ExitException() extends Exception("exit", null, false, false)

  final case class Interpreter(name: String, run: PartialFunction[String, Task[(String, Interpreter)]]) {
    def orElse(other: Interpreter): Interpreter = Interpreter(other.name, run.orElse(other.run))
  }

  def programInterpreter: Interpreter = taskSwitch orElse Interpreter ("program:", {
    case program: String =>
      Runner.parseAndCompile(UpickleCompiler, program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter[UpickleCompiler.type]) => Task.now(("", programInterpreterOf(program, compiledFilter)))
      )
  })

  def programInterpreterOf(source: String, program: CompiledFilter[UpickleCompiler.type]): Interpreter = taskSwitch orElse Interpreter (s"program $source, input: ", {
    case input: String =>
      val inputJson = json read input
      val execute = program(inputJson)
      execute.map { results =>
        val result = results.mkString("(", ", ", "_")
        (result, programInterpreterOf(source, program))
      }
  })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter ("input: ", {
    case input: String =>
      val in = json read input
      Task.now(("", inputInterpreterOf(input, in)))
  })

  def inputInterpreterOf(source: String, input: Js.Value): Interpreter = taskSwitch orElse Interpreter (s"input $source, program: ", {
    case program: String =>
      Runner.parseAndCompile(UpickleCompiler, program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter[UpickleCompiler.type]) => compiledFilter(input).map { results => (results.mkString("(", ", ", "_"), inputInterpreterOf(source, input)) }
      )
  })

  def taskSwitch: Interpreter = Interpreter (":p, :i:", {
    case ":p" => Task.evalAlways(("", programInterpreter))
    case ":i" => Task.evalAlways(("", inputInterpreter))
    case ":q" => Task { println("Bye!"); throw ExitException() }
  })

  def run: Interpreter = taskSwitch

}
