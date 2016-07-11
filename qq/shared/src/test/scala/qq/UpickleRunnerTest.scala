package qq

import qq.TestUtil._
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.Future

class UpickleRunnerTest extends QQTestSuite {

  import RunnerTest._

  def runTest(test: RunnerTest): Future[Any] =
    Runner
      .run(UpickleRuntime, test.program)(List(test.input))
      .runFuture
      .transform(_ should equal(test.expectedOutput.valueOr(ex => throw ex)), { ex =>
        ex should equal(test.expectedOutput.swap.getOrElse(???)); ex
      })

  "identity" in runTest(identityProgram)
  "ensequenced filters" in runTest(ensequencedFilters)
  "enlisted filter" in runTest(enlistedFilters)
  "select key" in runTest(selectKeyProgram)
  "collect results" in runTest(collectResults)
  "enject filter" in runTest(enjectedFilters)
  "pipes" in runTest(pipes)
  "length" in runTest(lengthTest)
  "keys" in runTest(keys)
  "classifiers" in
    Future.traverse(List(arrays, strings, booleans, scalars, objects, iterables, nulls, numbers))(runTest)
  "add" in runTest(add)
  "maths" in runTest(maths)
  "bedmas" in runTest(bedmas)
  "map" in runTest(map)
  "multiply" in runTest(multiply)
  "add null exception" in runTest(addNullException)
  "silenced exception" in runTest(silencedException)
}
