package qq

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertion

import scala.concurrent.{ExecutionContext, Future, Promise}

trait AsyncTestUtil extends TestUtil {

  implicit def discardAssertions(fut: Future[List[Assertion]])(implicit ctx: ExecutionContext): Future[Assertion] = {
    fut.map(_ => new Assertion {})
  }

  implicit class TaskRunFuture[A](val task: Task[A]) {
    def runFuture(implicit s: Scheduler): Future[A] = {
      val prom = Promise[A]()
      task.runAsync(prom.complete(_))
      prom.future
    }
  }

}

object AsyncTestUtil extends AsyncTestUtil
