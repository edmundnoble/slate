package qq

import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

import scala.concurrent.{Future, Promise}
import scalaz.Monad

object Util {

  implicit object TaskMonad extends Monad[Task] {
    override def point[A](a: => A): Task[A] = Task.Now(a)
    override def bind[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa.flatMap(f)
  }

  implicit object CoevalMonad extends Monad[Coeval] {
    override def point[A](a: => A): Coeval[A] = Coeval.Now(a)
    override def bind[A, B](fa: Coeval[A])(f: (A) => Coeval[B]): Coeval[B] = fa.flatMap(f)
  }

  implicit class TaskRunFuture[A](val task: Task[A]) {
    def runFuture(implicit s: Scheduler): Future[A] = {
      val prom = Promise[A]()
      task.runAsync { r => prom.complete(r); () }
      prom.future
    }
  }


}
