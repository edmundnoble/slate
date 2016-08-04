package qq

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.Assertion

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.implicitConversions

object TestUtil {

  implicit class TaskRunFuture[A](val task: Task[A]) {
    def runFuture(implicit s: Scheduler): Future[A] = {
      val prom = Promise[A]()
      task.runAsync(prom.complete(_))
      prom.future
    }
  }

  implicit def discardAssertions(fut: Future[List[Assertion]])(implicit ctx: ExecutionContext): Future[Assertion] = {
    fut.map(_.head)
  }

}
