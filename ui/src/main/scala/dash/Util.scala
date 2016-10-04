package dash

import fastparse.core.Parsed
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.extra.Reusability
import monix.eval.{Callback, Task}
import monix.execution.cancelables.{CompositeCancelable, StackedCancelable}
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import scodec.Attempt

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scalaz.{Applicative, BindRec, Coyoneda, Free, Monad, \/, ~>}
import scalaz.syntax.either._

object Util {

  def raceFold[A, B](ob: Seq[Task[A]])(z: B)(fold: (B, A) => B): Observable[B] = {
    Observable.unsafeCreate { subscriber =>
      // We need a monitor to synchronize on, per evaluation!
      val lock = new AnyRef
      val conn = StackedCancelable()
      // Forces a fork on another (logical) thread!
      subscriber.scheduler.executeAsync(lock.synchronized {
        // Keeps track of tasks remaining to be completed.
        // Is initialized by 1 because of the logic - tasks can run synchronously,
        // and we decrement this value whenever one finishes, so we must prevent
        // zero values before the loop is done.
        // MUST BE synchronized by `lock`!
        var remaining = 1

        // If this variable is false, then a task ended in error.
        // MUST BE synchronized by `lock`!
        var isActive = true

        var cur: B = z

        subscriber.onNext(z)

        // MUST BE synchronized by `lock`!
        // MUST NOT BE called if isActive == false!
        @inline def maybeSignalFinal(conn: StackedCancelable)
          (implicit s: Scheduler): Unit = {

          remaining -= 1
          if (remaining == 0) {
            subscriber.onComplete()
            isActive = false
            conn.pop()
          }
        }

        implicit val s = subscriber.scheduler
        // Represents the collection of cancelables for all started tasks
        val composite = CompositeCancelable()
        conn.push(composite)

        // Collecting all cancelables in a buffer, because adding
        // cancelables one by one in our `CompositeCancelable` is
        // expensive, so we do it at the end
        val allCancelables = ListBuffer.empty[StackedCancelable]
        val cursor = ob.toIterator

        // The `isActive` check short-circuits the process in case
        // we have a synchronous task that just completed in error
        while (cursor.hasNext && isActive) {
          remaining += 1
          val task = cursor.next()
          val stacked = StackedCancelable()
          allCancelables += stacked

          // Light asynchronous boundary; with most scheduler implementations
          // it will not fork a new (logical) thread!
          subscriber.scheduler.executeLocal(
            Task.unsafeStartNow(task, subscriber.scheduler, stacked,
              new Callback[A] {
                def onSuccess(value: A): Unit =
                  lock.synchronized {
                    if (isActive) {
                      val newValue = fold(cur, value)
                      cur = newValue
                      subscriber.onNext(newValue)
                      maybeSignalFinal(conn)
                    }
                  }

                def onError(ex: Throwable): Unit =
                  lock.synchronized {
                    if (isActive) {
                      isActive = false
                      // This should cancel our CompositeCancelable
                      conn.pop().cancel()
                      subscriber.onError(ex)
                    } else {
                      subscriber.scheduler.reportFailure(ex)
                    }
                  }
              }))
        }

        // All tasks could have executed synchronously, so we might be
        // finished already. If so, then trigger the final callback.
        maybeSignalFinal(conn)

        // Note that if an error happened, this should cancel all
        // other active tasks.
        composite ++= allCancelables
      })
      conn
    }
  }


  def taskToCallback[A](fa: Task[A])(implicit scheduler: Scheduler): CallbackTo[CancelableFuture[A]] = {
    CallbackTo.lift { () =>
      fa.runAsync(scheduler)
    }
  }

  def callbackToTask: CallbackTo ~> Task = new (CallbackTo ~> Task) {
    override def apply[A](fa: CallbackTo[A]): Task[A] = Task.eval(fa.runNow())
  }

  implicit class parsedOps[A](val under: Parsed[A]) extends AnyVal {
    def toDisjunction: Parsed.Failure \/ Parsed.Success[A] = under match {
      case f: Parsed.Failure => f.left
      case s: Parsed.Success[A] => s.right
    }
  }

  implicit class attemptOps[A](val under: Attempt[A]) extends AnyVal {
    def toDisjunction: Attempt.Failure \/ Attempt.Successful[A] = under match {
      case f: Attempt.Failure => f.left
      case s: Attempt.Successful[A] => s.right
    }
  }

  // an observable can only be equal to another by reference.
  implicit def observableReusability[A]: Reusability[Observable[A]] = Reusability.byRef[Observable[A]]

  implicit final class EitherTaskOps[E <: Throwable, A](val eOrA: E \/ A) extends AnyVal {
    @inline def valueOrThrow: Task[A] = eOrA.fold(Task.raiseError, Task.now)
  }

  @inline def liftFC[S[_], A](sa: S[A]): Free[Coyoneda[S, ?], A] =
    Free.liftF[Coyoneda[S, ?], A](Coyoneda.lift(sa))

  @inline def pureFC[S[_], A](a: A): Free[Coyoneda[S, ?], A] =
    Free.pure[Coyoneda[S, ?], A](a)

  @inline def foldMapFCRec[F[_], G[_] : Applicative : BindRec, A](program: Free[Coyoneda[F, ?], A], nt: F ~> G): G[A] =
    program.foldMapRec[G](Coyoneda.liftTF(nt)(implicitly[Applicative[G]]))

}
