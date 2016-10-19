package qq

import monix.eval.Task
import qq.util.{Interpreter, InterpreterExitException}

import cats.implicits._

class InterpreterTest extends QQAsyncTestSuite {

  import Interpreter._

  def runInterpreter(interpreter: Interpreter, input: String, next: String*): Task[(String, Interpreter)] = {
    val run = interpreter.resume(input).value.value
    if (next.isEmpty) {
      run
    } else {
      run.flatMap {
        case (_, nextInterpreter) =>
          Task.defer(runInterpreter(nextInterpreter, next.head, next.tail: _*))
      }
    }
  }

  "quit" in {
    mainMenu.resume(":q").value should be(InterpreterExitException.left)
  }

  "program interpreter" - {
    "accessible from main menu" in {
      for {
        programMenu <- runInterpreter(mainMenu, ":p").runAsync
      } yield {
        programMenu._2.name should be("program:")
      }
    }

    "take any program and input subsequently" in {
      for {
        output <- runInterpreter(mainMenu, ":p", ".", "[]").runAsync
      } yield output._1 should be("[]")
    }

    "take any program and input subsequently several times" in {
      for {
        output <- runInterpreter(mainMenu, ":p", ".", "[]", "1").runAsync
      } yield output._1 should be("1")
    }
  }

  "input interpreter" - {
    "accessible from main menu" in {
      for {
        inputMenu <- runInterpreter(mainMenu, ":i").runAsync
      } yield {
        inputMenu._2.name should be("input:")
      }
    }

    "take any input and program subsequently" in {
      for {
        output <- runInterpreter(mainMenu, ":i", "[]", ".").runAsync
      } yield output._1 should be("[]")
    }

    "take any input and program subsequently several times" in {
      for {
        output <- runInterpreter(mainMenu, ":i", "0", ". + 1", ". + 2").runAsync
      } yield output._1 should be("2")
    }
  }

}
