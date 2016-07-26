package qq

import TestUtil._

class InterpreterTest extends QQTestSuite {

  "program interpreter" - {
    "accessible from main menu" in {
      for {
         programMenu <- Interpreter.mainMenu.run.lift(":p").value
      } yield {
        programMenu._1 shouldBe "program: "
      }
    }
    "take any program and input subsequently" in {
      for {
        programMenuOutput <- Interpreter.mainMenu.run.lift(":p").value
        inputMenuOutput <- programMenuOutput._2.run.lift(".").value
        output <- inputMenuOutput._2.run.lift("[]").value
      } yield output shouldBe "[]"
    }
    "take any program and input subsequently several times" in {
      for {
        programMenuOutput <- Interpreter.mainMenu.run.lift(":p").value
        inputMenuOutput <- programMenuOutput._2.run.lift(".").value
        firstOutput <- inputMenuOutput._2.run.lift("[]").value
        output <- firstOutput._2.run.lift("[]").value
      } yield output shouldBe "[]"
    }
  }

}
