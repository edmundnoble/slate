package qq

import utest._

import scala.concurrent.Future
import TestUtil._

import monix.execution.Scheduler.Implicits.global

object UpickleCompilerTest extends utest.TestSuite with Asserts {

  val tests = this {

    import qq.UpickleCompiler._

    def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
      case CompilerTest(filter, input, result) =>
        compile(Nil, filter).getOrElse(???).apply(input).runFuture map (_ ===> result)
    }

    "select keys" - Future.traverse(CompilerTest.selectKeys)(testRun)
    "select index" - Future.traverse(CompilerTest.selectIndex)(testRun)
    "id" - Future.traverse(CompilerTest.id)(testRun)
    "select range" - Future.traverse(CompilerTest.selectRange)(testRun)
    "collect results" - Future.traverse(CompilerTest.collectResults)(testRun)

  }
}
