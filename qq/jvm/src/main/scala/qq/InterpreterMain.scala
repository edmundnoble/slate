package qq

import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn
import scalaz.syntax.std.option._

object InterpreterMain extends App {

  def runInterpreter(interpreter: Interpreter): Task[Unit] = Task.defer {
    println(s"Entered interpreter ${interpreter.name}")
    interpreter.resume(StdIn.readLine()).cata(
      _.fold(
        _ => Task.evalAlways(println("Bye!")),
        _.flatMap { case (output, nextInterpreter) =>
          val () = println(output)
          runInterpreter(nextInterpreter)
        }
      ),
      Task.defer {
        val () = println("what?")
        runInterpreter(interpreter)
      })
  }

  val interpreterFinished: CancelableFuture[Unit] = runInterpreter(Interpreter.mainMenu).runAsync
  val () = Await.result(interpreterFinished, Duration.Inf)

}
