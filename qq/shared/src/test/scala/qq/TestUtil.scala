package qq

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.{Future, Promise}

object TestUtil {

  implicit class TaskRunFuture[A](val task: Task[A]) {
    def runFuture(implicit s: Scheduler): Future[A] = {
      val prom = Promise[A]()
      task.runAsync(prom.complete(_))
      prom.future
    }
  }

}
