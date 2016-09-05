package dash

import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.extra.Reusability
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable

import scala.language.implicitConversions
import scalaz.{Coyoneda, Free, Monad, \/, ~>}

object Util {

  def taskToCallback[A](fa: Task[A])(implicit scheduler: Scheduler): CallbackTo[CancelableFuture[A]] = {
    CallbackTo.lift { () =>
      fa.runAsync(scheduler)
    }
  }

  implicit def observableReusability[A]: Reusability[Observable[A]] =
    Reusability.byRef[Observable[A]]

  def callbackToTask: CallbackTo ~> Task = new (CallbackTo ~> Task) {
    override def apply[A](fa: CallbackTo[A]): Task[A] = Task.eval(fa.runNow())
  }

  implicit final class EitherTaskOps[E <: Throwable, A](val eOrA: E \/ A) extends AnyVal {
    @inline def valueOrThrow: Task[A] = eOrA.fold(Task.raiseError, Task.now)
  }

  @inline def liftFC[S[_], A](sa: S[A]): Free[Coyoneda[S, ?], A] =
    Free.liftF[Coyoneda[S, ?], A](Coyoneda.lift(sa))

  @inline def foldMapFC[F[_], G[_] : Monad, A](program: Free[Coyoneda[F, ?], A], nt: F ~> G): G[A] =
    program.foldMap[G](Coyoneda.liftTF(nt))

}
