package dash

import fastparse.core.Parsed
import japgolly.scalajs.react.CallbackTo
import japgolly.scalajs.react.extra.Reusability
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import scodec.Attempt

import scala.language.implicitConversions
import scalaz.{Applicative, Coyoneda, Free, FreeT, Functor, Monad, \/, ~>}
import scalaz.syntax.either._
import scalaz.syntax.monad._

object Util {

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

  @inline def foldMapFC[F[_], G[_] : Monad, A](program: Free[Coyoneda[F, ?], A], nt: F ~> G): G[A] =
    program.foldMap[G](Coyoneda.liftTF(nt))

}
