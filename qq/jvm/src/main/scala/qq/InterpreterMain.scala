package qq

import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import qq.util.Interpreter

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

object InterpreterMain extends App {

  def runInterpreter(interpreter: Interpreter): Task[Unit] = Task.defer {
    println("Entered interpreter " + interpreter.name)
    interpreter.resume(StdIn.readLine()).fold(
      Task.defer {
        val () = println("what?")
        runInterpreter(interpreter)
      }) {
      _.fold(
        _ => Task.eval(println("Bye!")),
        _.flatMap { case (output, nextInterpreter) =>
          val () = println(output)
          runInterpreter(nextInterpreter)
        }
      )
    }
  }

  val interpreterFinished: CancelableFuture[Unit] = runInterpreter(Interpreter.mainMenu).runAsync
  val () = Await.result(interpreterFinished, Duration.Inf)

}
