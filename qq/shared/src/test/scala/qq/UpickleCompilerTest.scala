package qq

import scala.concurrent.Future
import TestUtil._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class UpickleCompilerTest extends QQTestSuite {

  import CompilerTest._

  def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
    case CompilerTest(filter, input, expectedOutput) =>
      QQCompiler
        .compile(UpickleRuntime, Nil, filter)
        .fold[Task[Unit]](
        err => Task.raiseError(err),
        program => program(input).map { output => output shouldBe expectedOutput })
        .runFuture
  }

  "select keys" in Future.traverse(selectKeyTests)(testRun)
  "select index" in Future.traverse(selectIndexTests)(testRun)
  "id" in Future.traverse(idTests)(testRun)
  "select range" in Future.traverse(selectRangeTests)(testRun)
  "collect results" in Future.traverse(collectResultsTests)(testRun)
  "fat stack" in Future.traverse(fatStackTests)(testRun)

}
