package dash.bindings

import monix.eval.Task
import monix.execution.Cancelable

import scala.scalajs.js.UndefOr

package object chrome {

  object identity {

    def getAuthToken(details: UndefOr[GetAuthTokenOptions]): Task[UndefOr[String]] = {
      Task.create[UndefOr[String]] { (_, callback) =>
        ChromeIdentity.getAuthToken(details, UndefOr.any2undefOrA(callback.onSuccess))
        Cancelable.empty
      }
    }

  }

}
