package qq

import TestUtil._
import monix.eval.Task
import scalaz.syntax.either._
import qq.Interpreter.Interpreter

class InterpreterTest extends QQTestSuite {

  import Interpreter._

  def runLoop(interpreter: Interpreter, input: String, next: String*): Task[(String, Interpreter)] = {
    val run = interpreter.resume(input).value.value
    if (next.isEmpty) {
      run
    } else {
      Task.defer(run.map(_._2).flatMap(runLoop(_, next.head, next.tail: _*)))
    }
  }

  "quit" - {
    mainMenu.resume(":q").value should be(ExitException.left)
  }

  "program interpreter" - {

    "accessible from main menu" in {
      for {
        programMenu <- runLoop(mainMenu, ":p").runFuture
      } yield {
        programMenu._2.name should be("program:")
      }
    }

    "take any program and input subsequently" in {
      for {
        output <- runLoop(mainMenu, ":p", ".", "[]").runFuture
      } yield output._1 should be("([])")
    }

    "take any program and input subsequently several times" in {
      for {
        output <- runLoop(mainMenu, ":p", ".", "[]", "1").runFuture
      } yield output._1 should be("(1)")
    }

  }

  "input interpreter" - {

    "accessible from main menu" in {
      for {
        inputMenu <- runLoop(mainMenu, ":i").runFuture
      } yield {
        inputMenu._2.name should be("input:")
      }
    }

    "take any input and program subsequently" in {
      for {
        output <- runLoop(mainMenu, ":i", "[]", ".").runFuture
      } yield output._1 should be("([])")
    }

    "take any input and program subsequently several times" in {
      for {
        output <- runLoop(mainMenu, ":i", "0", ". + 1", ". + 2").runFuture
      } yield output._1 should be("(2)")
    }

  }

}
