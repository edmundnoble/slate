package qq

import qq.Util._
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.concurrent.Future

object QQUpickleRunnerTest extends utest.TestSuite {
  override val tests = TestSuite {
    import QQRunnerTest._

    def runTest(test: QQRunnerTest): Future[Unit] =
      QQRunner
        .run(QQUpickleCompiler, test.program)(List(test.input))
        .runFuture
        .map(out => assert(out == test.expectedOutput))

    "identity program" - runTest(identityProgram)
    "ensequenced filters program" - runTest(ensequencedFilters)
    "enlisted filter program" - runTest(enlistedFilters)
    "select key program" - runTest(selectKeyProgram)
    "collect results" - runTest(collectResults)
    "enject filter program" - runTest(enjectedFilters)
    "pipes program" - runTest(pipes)
    "length program" - runTest(length)
    "keys program" - runTest(keys)
    "classifier programs" -
      Future.traverse(List(arrays, strings, booleans, scalars, objects, iterables, nulls, numbers))(runTest)
    "add" - runTest(add)
  }
}
