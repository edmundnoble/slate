package qq

import qq.Util._
import qq.jsc.QQJSCompiler
import monix.execution.Scheduler.Implicits.global
import utest._

import scala.concurrent.Future
import scala.scalajs.js

object QQJSRunnerTest extends utest.TestSuite {
  override val tests = TestSuite {
    import QQRunnerTest._
    def runTest(test: QQRunnerTest): Future[Unit] =
      QQRunner
        .run(QQJSCompiler, test.program)(List(upickle.json.writeJs(test.input).asInstanceOf[js.Any]))
        .runFuture
        .map { out => val js = out.map(upickle.json.readJs); assert(js == test.expectedOutput) }

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
  }
}
