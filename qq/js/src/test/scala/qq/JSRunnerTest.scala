package qq

import org.scalatest.Assertion
import qq.jsc.{JSRuntime, Json}
import upickle.Js

import scala.concurrent.Future

class JSRunnerTest extends QQAsyncTestSuite {

  import RunnerTest._

  def runTest(test: RunnerTest): Future[Assertion] =
    RunnerTest.runTest(
      JSRuntime,
      Json.writeJsRec(Recursion.Unsafe.RecursionDirect, _: Js.Value),
      Json.readJsRec(Recursion.Unsafe.RecursionDirect, _: Any),
      test
    )

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
