package qq

import qq.TestUtil._
import qq.jsc.JSRuntime
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.Future
import scalaz.syntax.std.`try`._

class JSRunnerTest extends QQTestSuite {

  import RunnerTest._

  def runTest(test: RunnerTest): Future[Any] =
    Runner
      .run(JSRuntime, test.program)(List(upickle.json.writeJs(test.input).asInstanceOf[AnyRef]))
      .materialize
      .map { outputOrExceptionTry =>
        val outputUpickleOrExceptionTry = outputOrExceptionTry.map(upickle.json.readJs)
        outputUpickleOrExceptionTry.toDisjunction shouldBe test.expectedOutputOrException
      }
      .runFuture

  "identity" in runTest(identityProgram)
  "ensequenced filters" in runTest(ensequencedFilters)
  "enlisted filter" in runTest(enlistedFilters)
  "select key" in runTest(selectKeyProgram)
  "collect results" in runTest(collectResults)
  "enject filter" in runTest(enjectedFilters)
  "pipes" in runTest(pipes)
  "length" in runTest(lengthTest)
  "keys" in runTest(keys)
  "classifiers" - {
    "arrays" in runTest(arrays)
    "strings" in runTest(strings)
    "booleans" in runTest(booleans)
    "scalars" in runTest(scalars)
    "objects" in runTest(objects)
    "iterables" in runTest(iterables)
    "nulls" in runTest(nulls)
    "numbers" in runTest(numbers)
  }
  "add" in runTest(add)
  "maths" in runTest(maths)
  "bedmas" in runTest(bedmas)
  "map" in runTest(map)
  "multiply" in runTest(multiply)
  "add null exception" in runTest(addNullException)
  "silenced exception" in runTest(silencedException)
}
