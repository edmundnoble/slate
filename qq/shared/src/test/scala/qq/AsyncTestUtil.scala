package qq

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{Assertion, Succeeded}

import scala.concurrent.{ExecutionContext, Future, Promise}

import scala.language.implicitConversions

trait AsyncTestUtil extends TestUtil {

  implicit def discardAssertions(fut: Future[List[Assertion]])(implicit ctx: ExecutionContext): Future[Assertion] = {
    fut.map(_ => Succeeded)
  }

  implicit class TaskRunFuture[A](val task: Task[A]) {
    def runFuture(implicit s: Scheduler): Future[A] = {
      AsyncTestUtil.this.runFuture(task)
    }
  }

  final def runFuture[A](task: Task[A])(implicit s: Scheduler): Future[A] = {
    val prom = Promise[A]()
    val _ = task.runAsync(prom.complete(_))
    prom.future
  }

}

object AsyncTestUtil extends AsyncTestUtil
