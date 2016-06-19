package qq

import utest._

import scala.concurrent.Future
import Util._

import monix.execution.Scheduler.Implicits.global

object JSCompilerTest extends utest.TestSuite with Asserts {

  val tests = this {

    import qq.jsc.JSCompiler._

    def testRun(qQDoubleCompilerTest: CompilerTest) = qQDoubleCompilerTest match {
      case CompilerTest(filter, input, result) =>
        compile(Nil, filter).getOrElse(???).apply(upickle.json.writeJs(input).asInstanceOf[scalajs.js.Any]).runFuture map (_.map(upickle.json.readJs) ===> result)
    }

    "select keys" - Future.traverse(CompilerTest.selectKeys)(testRun)
    "select index" - Future.traverse(CompilerTest.selectIndex)(testRun)
    "id" - Future.traverse(CompilerTest.id)(testRun)
    "select range" - Future.traverse(CompilerTest.selectRange)(testRun)
    "collect results" - Future.traverse(CompilerTest.collectResults)(testRun)

  }
}
