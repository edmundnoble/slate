package slate
package util

import cats.free.{Coyoneda, Free}
import cats.implicits._
import cats.{Monad, Monoid, ~>}
import fastparse.all.Parsed
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.{CallbackTo, vdom}
import monix.eval.Task.Context
import monix.eval.{Callback, Task}
import monix.execution.cancelables.{CompositeCancelable, StackedCancelable}
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import scodec.Attempt
import shapeless.{Coproduct, Inl, Inr}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

object Util {

  def raceFold[A, B](ob: Seq[Task[A]])(z: B)(fold: (B, A) => B): Observable[B] = {
    Observable.unsafeCreate { subscriber =>
      // We need a monitor to synchronize on, per evaluation!
      val lock = new AnyRef
      val conn = StackedCancelable()
      // Forces a fork on another (logical) thread!
      subscriber.scheduler.executeAsync { () =>
        val _ = lock.synchronized {
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
              val _ = conn.pop()
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
            subscriber.scheduler.executeTrampolined(() =>
              Task.unsafeStartNow(task, Task.Context(subscriber.scheduler, stacked, monix.execution.misc.ThreadLocal(1), Task.Options(false)),
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
        }
      }
      conn
    }
  }

  implicit val tagmodMonoid: Monoid[TagMod] = new Monoid[TagMod] {
    override def empty: TagMod = vdom.EmptyTag
    override def combine(f1: TagMod, f2: TagMod): TagMod = f1 + f2
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
    def toEither: Parsed.Failure Either Parsed.Success[A] = under match {
      case f: Parsed.Failure => Left(f)
      case s: Parsed.Success[A] => Right(s)
    }
  }

  implicit class attemptOps[A](val under: Attempt[A]) extends AnyVal {
    def toEither: Attempt.Failure Either Attempt.Successful[A] = under match {
      case f: Attempt.Failure => Left(f)
      case s: Attempt.Successful[A] => Right(s)
    }
  }

  def inl[H, T <: Coproduct](h: H) = Inl(h)
  def inr[H, T <: Coproduct](t: T) = Inr(t)

  // an observable can only be equal to another by reference.
  implicit def observableReusability[A]: Reusability[Observable[A]] = Reusability.byRef[Observable[A]]

  implicit final class EitherTaskOps[E <: Throwable, A](val eOrA: E Either A) extends AnyVal {
    @inline def valueOrThrow: Task[A] = eOrA.fold(Task.raiseError, Task.now)
  }

}
