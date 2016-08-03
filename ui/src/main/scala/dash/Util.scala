package dash

import japgolly.scalajs.react.{Callback, CallbackTo}
import monix.eval.Task
import monix.execution.Scheduler

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.Array
import scalaz.{Applicative, Traverse, ~>}

object Util {

  def taskToCallback[A](fa: Task[A])(implicit scheduler: Scheduler): Callback = {
    CallbackTo.pure {
      val _ = fa.runAsync(scheduler)
    }
  }

  def callbackToTask: CallbackTo ~> Task = new (CallbackTo ~> Task) {
    override def apply[A](fa: CallbackTo[A]): Task[A] = Task.evalAlways(fa.runNow())
  }

}
