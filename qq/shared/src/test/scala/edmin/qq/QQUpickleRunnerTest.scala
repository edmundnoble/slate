package edmin.qq

import edmin.qq.Util._
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.concurrent.Future

object QQUpickleRunnerTest extends utest.TestSuite {
  override val tests = TestSuite {
    def runTest(test: QQRunnerTest): Future[Unit] =
      QQRunner
        .run(QQUpickleCompiler, test.program)(List(test.input))
        .runFuture
        .map(out => assert(out == test.expectedOutput))

    "identity program" - runTest(QQRunnerTest.identityProgram)
    "ensequenced filters program" - runTest(QQRunnerTest.ensequencedFilters)
    "enlisted filter program" - runTest(QQRunnerTest.enlistedFilters)
    "select key program" - runTest(QQRunnerTest.selectKeyProgram)
  }
}
