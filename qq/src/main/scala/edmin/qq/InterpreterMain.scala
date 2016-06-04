package edmin.qq

import scala.util.{Failure, Success, Try}
import monix.execution.Scheduler.Implicits.global

import scala.scalajs.js.JSApp

object InterpreterMain extends JSApp {
  def main(): Unit = {
    Interpreter.run.runAsync { (v: Try[Unit]) => println(s"Done: $v") }
  }
}
