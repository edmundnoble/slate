package edmin.qq

import java.io.{BufferedReader, DataInputStream, InputStreamReader}

import edmin.qq.QQAST.{CompiledFilter, Definition, QQCompilationException}
import fastparse.core.{ParseError, Parsed}
import monix.eval.{Coeval, Task}
import monix.reactive.Observable
import monocle.macros._
import monocle.Lens

import scala.scalajs.js
import scala.scalajs.js.{Any, |}
import scala.scalajs.js.|._
import scalaz.{-\/, Monad, Monoid, \/}
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.functor._
import scalaz.syntax.arrow._
import scalaz.std.function._
import scalaz.std.partialFunction._
import scalaz.syntax.id._
import scalaz.syntax.monoid._
import scalaz.std.string._
import scalaz.std.option._
import Util._
import upickle.Js
import upickle.json

import scala.io.StdIn

object Interpreter {

    case class Interpreter(run: PartialFunction[String, Task[(String, Interpreter)]]) {
      def orElse(other: Interpreter): Interpreter = Interpreter(run.orElse(other.run))
    }

    def programInterpreter: Interpreter = taskSwitch orElse Interpreter {
      case program: String =>
        QQ.parseAndCompile(program, optimize = true).fold(
          (err: QQCompilationException | ParseError) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
          (compiledFilter: CompiledFilter) => Task.now(("", programInterpreterOf(compiledFilter)))
        )
    }

    def programInterpreterOf(program: CompiledFilter): Interpreter = taskSwitch orElse Interpreter {
      case input: String =>
        val inputJson = json.read(input)
        val jsValue = json.writeJs(inputJson).asInstanceOf[js.Any]
        val execute = program.apply(jsValue)
        execute.map { results =>
          val result = results.mkString("(", ", ", "_")
          (result, programInterpreterOf(program))
        }
    }

    def inputInterpreter: Interpreter = taskSwitch orElse Interpreter {
      case input: String =>
        val in = json.writeJs(json.read(input)).asInstanceOf[js.Any]
        Task.now(("", inputInterpreterOf(in)))
    }

    def inputInterpreterOf(input: js.Any): Interpreter = taskSwitch orElse Interpreter {
      case program: String =>
        QQ.parseAndCompile(program, optimize = true).fold(
          (err: QQCompilationException | ParseError) => Task(Console.err.println(s"Error: $err")) map (_ => ("", programInterpreter)),
          (compiledFilter: CompiledFilter) => compiledFilter(input).map { results => (results.mkString("(", ", ", "_"), inputInterpreterOf(input)) }
        )
    }

    def taskSwitch: Interpreter = Interpreter {
      case ":p" => Task.now(("", programInterpreter))
      case ":i" => Task.now(("", inputInterpreter))
    }

    def runInterpreter(interpreter: Interpreter): Task[Unit] = Task {
      val pf = interpreter.run
      val in = new BufferedReader(new InputStreamReader(System.in))
      pf.lift(in.readLine()).fold(Task { Console.println("end!") })(_.flatMap { case (out, next) => Console.println(out); runInterpreter(next) })
    }.flatten

    def run: Task[Unit] = {
      runInterpreter(taskSwitch)

    }

}
