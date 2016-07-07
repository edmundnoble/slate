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
    interpreter.run.lift(StdIn.readLine()).cata(
      _.flatMap { case (output, nextInterpreter) => println(output); Task.defer(runInterpreter(nextInterpreter)) },
      Task.defer {
        val () = println("what?")
        runInterpreter(interpreter)
      })
  }

  val interpreterFinished: CancelableFuture[Unit] = runInterpreter(Interpreter.mainMenu).runAsync
  try {
    val () = Await.result(interpreterFinished, Duration.Inf)
  } catch {
    case ExitException() =>
  }
}
