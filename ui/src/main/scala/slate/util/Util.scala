package slate
package util

import cats.{Monoid, ~>}
import fastparse.all.Parsed
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.{CallbackTo, vdom}
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import scodec.Attempt
import shapeless.{Coproduct, Inl, Inr}

import scala.concurrent.duration.FiniteDuration

object Util {

  def raceFold[A, B](ob: Seq[Task[A]])(z: B)(fold: (B, A) => B)(parallelism: Int, throttleLastPeriod: FiniteDuration): Observable[B] = {
    if (ob.isEmpty) Observable.now(z)
    else
      Observable.fromIterable(ob)
        .mapAsync(parallelism)(x => x)
        .throttleLast(throttleLastPeriod)
        .scan(z)(fold)
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
