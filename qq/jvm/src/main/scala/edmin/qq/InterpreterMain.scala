package edmin.qq

import edmin.qq.Interpreter.Interpreter
import monix.eval.Task

import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.StdIn

object InterpreterMain extends App {

  def runInterpreter(interpreter: Interpreter): Task[Unit] = Task.defer {
    println(s"Entered interpreter ${interpreter.name}")
    interpreter.run.lift(StdIn.readLine())
      .fold(Task {
        println("end!")
      })(_.flatMap { case (out, next) => println(out); Task.defer(runInterpreter(next)) })
  }

  def run: Task[Unit] = {
    runInterpreter(Interpreter.run)
  }

  println("In main!")
  val prom = Promise[Unit]()
  run.runAsync {
    prom.complete(_)
  }
  Await.result(prom.future, 5.minutes)
  println("End of main!")
}
