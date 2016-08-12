package dash

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.{Callback, CallbackTo}
import monix.eval.Task.Attempt
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.language.implicitConversions
import scalaz.{Coyoneda, Free, Monad, \/, ~>}

object Util {

  def taskToCallback[A](fa: Task[A])(implicit scheduler: Scheduler): Callback = {
    CallbackTo.pure {
      val _ = fa.runAsync(scheduler)
    }
  }

  implicit def observableReusability[A]: Reusability[Observable[A]] =
    Reusability.byRef[Observable[A]]

  def callbackToTask: CallbackTo ~> Task = new (CallbackTo ~> Task) {
    override def apply[A](fa: CallbackTo[A]): Task[A] = Task.evalAlways(fa.runNow())
  }

  implicit final class EitherTaskOps[E <: Throwable, A](val eOrA: E \/ A) extends AnyVal {
    @inline def valueOrThrow: Attempt[A] = eOrA.fold(Task.Error(_), Task.Now(_))
  }

  @inline def liftFC[S[_], A](sa: S[A]): Free[Coyoneda[S, ?], A] =
    Free.liftF[Coyoneda[S, ?], A](Coyoneda.lift(sa))

  @inline def foldMapFC[F[_], G[_] : Monad, A](program: Free[Coyoneda[F, ?], A], nt: F ~> G): G[A] =
    program.foldMap[G](Coyoneda.liftTF(nt))



}
