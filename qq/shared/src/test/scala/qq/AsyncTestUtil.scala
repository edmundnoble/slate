package qq

import org.scalatest.{Assertion, Succeeded}

import scala.concurrent.{ExecutionContext, Future}

trait AsyncTestUtil extends TestUtil {

  implicit def discardAssertions(fut: Future[List[Assertion]])(implicit ctx: ExecutionContext): Future[Assertion] = {
    fut.map(_ => Succeeded)
  }

}

