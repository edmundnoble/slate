package dash.chrome

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.{JSName, ScalaJSDefined}

@ScalaJSDefined
class GetAuthTokenOptions(val interactive: UndefOr[Boolean] = js.undefined,
                          val account: UndefOr[AccountInfo] = js.undefined,
                          val scopes: UndefOr[js.Array[String]] = js.undefined) extends js.Object

@ScalaJSDefined
class RemoveCachedAuthTokenOptions(val token: String) extends js.Object

@ScalaJSDefined
class AccountInfo(val id: String) extends js.Object

@js.native
@JSName("chrome.identity")
object ChromeIdentity extends js.Object {
  @JSName("getAuthToken")
  @js.native
  def fetchAuthToken(details: UndefOr[GetAuthTokenOptions] = js.undefined,
                     callback: js.Function1[String, Unit]): Unit = js.native

  @js.native
  def removeCachedAuthToken(details: RemoveCachedAuthTokenOptions,
                            callback: js.Function1[Unit, Unit]): Unit = js.native
}

@ScalaJSDefined
class LastError(val message: UndefOr[String]) extends js.Object

@js.native
@JSName("chrome.runtime")
object ChromeRuntime extends js.Object {
  @js.native
  def lastError: UndefOr[LastError] = js.native
}

