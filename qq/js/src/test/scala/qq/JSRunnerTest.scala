package qq

import qq.Util._
import qq.jsc.JSCompiler
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.concurrent.Future
import scala.scalajs.js

object JSRunnerTest extends utest.TestSuite with Asserts {
  override val tests = TestSuite {
    import RunnerTest._
    def runTest(test: RunnerTest): Future[Unit] =
      Runner
        .run(JSCompiler, test.program)(List(upickle.json.writeJs(test.input).asInstanceOf[js.Any]))
        .runFuture
        .transform(
          { out => val js = out.map(upickle.json.readJs); js ===> test.expectedOutput.getOrElse(???) }, { ex => ex ===> test.expectedOutput.swap.getOrElse(???); ex }
        )
        .fallbackTo(Future.successful(()))

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
    "maths" - runTest(maths)
    "bedmas" - runTest(bedmas)
    "map" - runTest(map)
    "multiply" - runTest(multiply)
    "add null exception" - runTest(addNullException)
    "silenced exception" - runTest(silencedException)
  }
}
