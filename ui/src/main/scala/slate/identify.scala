package slate

import monix.eval.Task
import monix.execution.Cancelable
import slate.chrome._

import scala.scalajs.js
import scala.scalajs.js.UndefOr

object identify {

  def chromeCallbackToTask[T](callbackTaker: (T => Unit) => Unit): Task[T] = {
    Task.create[T] { (_, callback) =>
      callbackTaker { result =>
        ChromeRuntime.lastError.fold(callback.onSuccess(result)) { ex =>
          callback.onError(ChromeErrorException(ex.message))
        }
      }
      Cancelable.empty
    }
  }

  def getAuthToken(interactive: Boolean = false, accountInfo: UndefOr[String] = js.undefined, scopes: UndefOr[js.Array[String]] = js.undefined): Task[String] =
    chromeCallbackToTask(ChromeIdentity.fetchAuthToken(new GetAuthTokenOptions(interactive, accountInfo.map(new AccountInfo(_)), scopes), _))

  def launchWebAuthFlow(interactive: Boolean = false, url: String): Task[String] =
    chromeCallbackToTask(ChromeIdentity.launchWebAuthFlow(new LaunchWebAuthFlowOptions(url, interactive), _))

  def removeCachedAuthToken(token: String): Task[Unit] =
    chromeCallbackToTask(ChromeIdentity.removeCachedAuthToken(new RemoveCachedAuthTokenOptions(token), _))

  case class ChromeErrorException(message: UndefOr[String]) extends Exception(message.getOrElse("No error message"))

}
