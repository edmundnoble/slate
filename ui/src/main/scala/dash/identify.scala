package dash

import dash.chrome._
import monix.eval.Task
import monix.execution.Cancelable

import scala.scalajs.js.UndefOr

object identify {

  def unsafeChromeCallbackToTask[T](callbackTaker: (T => Unit) => Unit): Task[T] = {
    Task.create[T] { (_, callback) =>
      callbackTaker { result =>
        ChromeRuntime.lastError.fold(callback.onSuccess(result)) { ex =>
          callback.onError(ChromeErrorException(ex.message))
        }
      }
      Cancelable.empty
    }
  }

  def getAuthToken(details: UndefOr[GetAuthTokenOptions]): Task[String] =
    unsafeChromeCallbackToTask(ChromeIdentity.fetchAuthToken(details, _))

  def removeCachedAuthToken(details: RemoveCachedAuthTokenOptions): Task[Unit] =
    unsafeChromeCallbackToTask(ChromeIdentity.removeCachedAuthToken(details, _))

  case class ChromeErrorException(message: UndefOr[String]) extends Exception(message.getOrElse("No error message"))

}
