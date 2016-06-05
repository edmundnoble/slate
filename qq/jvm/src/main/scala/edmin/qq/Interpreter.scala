package edmin.qq

import java.io.{BufferedReader, InputStreamReader}

import edmin.qq.QQSharedCompiler._
import monix.eval.Task
import upickle.{Js, json}

import scala.io.StdIn

object Interpreter {

  case class Interpreter(name: String, run: PartialFunction[String, Task[(String, Interpreter)]]) {
    def orElse(other: Interpreter): Interpreter = Interpreter(other.name, run.orElse(other.run))
  }

  def programInterpreter: Interpreter = taskSwitch orElse Interpreter ("program:", {
    case program: String =>
      QQ.parseAndCompile(program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter) => Task.now(("", programInterpreterOf(program, compiledFilter)))
      )
  })

  def programInterpreterOf(source: String, program: CompiledFilter): Interpreter = taskSwitch orElse Interpreter (s"program ${source}, input: ", {
    case input: String =>
      val inputJson = json.read(input)
      val execute = program(inputJson)
      execute.map { results =>
        val result = results.mkString("(", ", ", "_")
        (result, programInterpreterOf(source, program))
      }
  })

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter ("input: ", {
    case input: String =>
      val in = json.read(input)
      Task.now(("", inputInterpreterOf(input, in)))
  })

  def inputInterpreterOf(source: String, input: Js.Value): Interpreter = taskSwitch orElse Interpreter (s"input ${source}, program: ", {
    case program: String =>
      QQ.parseAndCompile(program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter) => compiledFilter(input).map { results => (results.mkString("(", ", ", "_"), inputInterpreterOf(source, input)) }
      )
  })

  def taskSwitch: Interpreter = Interpreter (":p, :i:", {
    case ":p" => Task.evalAlways(("", programInterpreter))
    case ":i" => Task.evalAlways(("", inputInterpreter))
    case ":q" => Task { println("Bye!"); sys.exit(0) }
  })

  def runInterpreter(interpreter: Interpreter): Task[Unit] = Task.defer {
    println(s"Entered interpreter ${interpreter.name}")
    interpreter.run.lift(StdIn.readLine())
      .fold(Task {
        println("end!")
      })(_.flatMap { case (out, next) => println(out); Task.defer(runInterpreter(next)) })
  }

  def run: Task[Unit] = {
    runInterpreter(taskSwitch)
  }

}
