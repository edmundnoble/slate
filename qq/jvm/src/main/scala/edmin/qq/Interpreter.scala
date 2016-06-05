package edmin.qq

import java.io.{BufferedReader, InputStreamReader}

import edmin.qq.QQSharedCompiler._
import monix.eval.Task
import upickle.{Js, json}

object Interpreter {

  case class Interpreter(run: PartialFunction[String, Task[(String, Interpreter)]]) {
    def orElse(other: Interpreter): Interpreter = Interpreter(run.orElse(other.run))
  }

  def programInterpreter: Interpreter = taskSwitch orElse Interpreter {
    case program: String =>
      QQ.parseAndCompile(program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter) => Task.now(("", programInterpreterOf(compiledFilter)))
      )
  }

  def programInterpreterOf(program: CompiledFilter): Interpreter = taskSwitch orElse Interpreter {
    case input: String =>
      val inputJson = json.read(input)
      val execute = program.apply(inputJson)
      execute.map { results =>
        val result = results.mkString("(", ", ", "_")
        (result, programInterpreterOf(program))
      }
  }

  def inputInterpreter: Interpreter = taskSwitch orElse Interpreter {
    case input: String =>
      val in = json.read(input)
      Task.now(("", inputInterpreterOf(in)))
  }

  def inputInterpreterOf(input: Js.Value): Interpreter = taskSwitch orElse Interpreter {
    case program: String =>
      QQ.parseAndCompile(program, optimize = true).fold(
        (err: Exception) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
        (compiledFilter: CompiledFilter) => compiledFilter(input).map { results => (results.mkString("(", ", ", "_"), inputInterpreterOf(input)) }
      )
  }

  def taskSwitch: Interpreter = Interpreter {
    case ":p" => Task.now(("", programInterpreter))
    case ":i" => Task.now(("", inputInterpreter))
  }

  def runInterpreter(interpreter: Interpreter): Task[Unit] = Task.defer {
    val pf = interpreter.run
    val in = new BufferedReader(new InputStreamReader(System.in))
    pf.lift(in.readLine())
      .fold(Task {
        Console.println("end!")
      })(_.flatMap { case (out, next) => Console.println(out); Task.defer(runInterpreter(next)) })
  }

  def run: Task[Unit] = {
    runInterpreter(taskSwitch)
  }

}
