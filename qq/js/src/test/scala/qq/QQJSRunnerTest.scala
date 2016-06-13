package qq

import qq.Util._
import qq.jsc.QQJSCompiler
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.concurrent.Future
import scala.scalajs.js

object QQJSRunnerTest extends utest.TestSuite {
  override val tests = TestSuite {
    def runTest(test: QQRunnerTest): Future[Unit] =
      QQRunner
        .run(QQJSCompiler, test.program)(List(upickle.json.writeJs(test.input).asInstanceOf[js.Any]))
        .runFuture
        .map(out => assert(out.map(upickle.json.readJs) == test.expectedOutput))

    "identity program" - runTest(QQRunnerTest.identityProgram)
    "ensequenced filters program" - runTest(QQRunnerTest.ensequencedFilters)
    "enlisted filter program" - runTest(QQRunnerTest.enlistedFilters)
    "select key program" - runTest(QQRunnerTest.selectKeyProgram)
    "collect results" - runTest(QQRunnerTest.collectResults)
    "enject filter program" - runTest(QQRunnerTest.enjectedFilters)
    "pipes program" - runTest(QQRunnerTest.pipes)
    "length program" - runTest(QQRunnerTest.length)
  }
}
