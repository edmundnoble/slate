package dash

import monix.eval.Task
import monix.execution.Cancelable

import scala.scalajs.js.UndefOr

package object chrome {

  object identity {

    def callbackToTask[T](callbackTaker: (T => Unit) => Unit): Task[T] = {
      Task.create[T] { (_, callback) =>
        callbackTaker { result =>
          ChromeRuntime.lastError.fold(callback.onSuccess(result)) { ex =>
            callback.onError(ChromeErrorException(ex.message))
          }
        }
        Cancelable.empty
      }
    }

    def getAuthToken(details: UndefOr[GetAuthTokenOptions]): Task[UndefOr[String]] =
      callbackToTask(ChromeIdentity.fetchAuthToken(details, _))

    def removeCachedAuthToken(details: RemoveCachedAuthTokenOptions): Task[Unit] =
      callbackToTask(ChromeIdentity.removeCachedAuthToken(details, _))

  }

}
