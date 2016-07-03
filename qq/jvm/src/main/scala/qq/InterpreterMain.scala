package qq

import qq.Interpreter.ExitException
import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn
import scalaz.syntax.std.option._

object InterpreterMain extends App {

  def runInterpreter(interpreter: Interpreter.Interpreter): Task[Unit] = Task.defer {
    println(s"Entered interpreter ${interpreter.name}")
    interpreter.run.lift(StdIn.readLine()).cata[Task[Unit]](
      _.flatMap { case (out, next) => println(out); Task.defer(runInterpreter(next)) },
      Task.defer[Unit] {
        println("what?")
        runInterpreter(interpreter)
      })
  }

  def run: Task[Unit] = {
    runInterpreter(Interpreter.run)
  }

  val future: CancelableFuture[Unit] = run.runAsync
  try {
    Await.result(future, Duration.Inf)
  } catch {
    case ExitException() =>
  }
}
