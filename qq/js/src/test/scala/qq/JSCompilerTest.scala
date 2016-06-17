package qq

import utest._

import scala.concurrent.Future
import Util._

import monix.execution.Scheduler.Implicits.global

object JSCompilerTest extends utest.TestSuite with Asserts {

  val tests = this {

    import qq.jsc.JSCompiler._

    def testRun(qQDoubleCompilerTest: DoubleCompilerTest) = qQDoubleCompilerTest match {
      case DoubleCompilerTest(filter, input, result) =>
        compile(Nil, filter).getOrElse(???).apply(upickle.json.writeJs(input).asInstanceOf[scalajs.js.Any]).runFuture map (_.map(upickle.json.readJs) ===> result)
    }

    "select keys" - Future.traverse(DoubleCompilerTest.selectKeys)(testRun)
    "select index" - Future.traverse(DoubleCompilerTest.selectIndex)(testRun)
    "id" - Future.traverse(DoubleCompilerTest.id)(testRun)
    "select range" - Future.traverse(DoubleCompilerTest.selectRange)(testRun)
    "collect results" - Future.traverse(DoubleCompilerTest.collectResults)(testRun)

  }
}
