package edmin.qq

import scala.util.{Failure, Success, Try}
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration._

object InterpreterMain extends App {
  def main(): Unit = {
    val prom = Promise[Unit]()
    Interpreter.run.runAsync { prom.complete(_) }
    Await.result(prom.future, 5.minutes)
  }
}
