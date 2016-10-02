package dash
package chrome

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSName, ScalaJSDefined}
import scala.scalajs.js.{UndefOr, native, undefined}

@ScalaJSDefined
class GetAuthTokenOptions(val interactive: UndefOr[Boolean] = undefined,
                          val account: UndefOr[AccountInfo] = undefined,
                          val scopes: UndefOr[js.Array[String]] = undefined) extends js.Object

@ScalaJSDefined
class LaunchWebAuthFlowOptions(val url: String,
                               val interactive: UndefOr[Boolean] = undefined) extends js.Object

@ScalaJSDefined
class RemoveCachedAuthTokenOptions(val token: String) extends js.Object

@ScalaJSDefined
class AccountInfo(val id: String) extends js.Object

@native
@JSName("chrome.identity")
object ChromeIdentity extends js.Object {
  @JSName("getAuthToken")
  @native
  def fetchAuthToken(details: UndefOr[GetAuthTokenOptions] = undefined,
                     callback: js.Function1[String, Unit]): Unit = native

  @native
  def launchWebAuthFlow(details: LaunchWebAuthFlowOptions,
                        callback: js.Function1[String, Unit]): Unit = native

  @native
  def removeCachedAuthToken(details: RemoveCachedAuthTokenOptions,
                            callback: js.Function1[Unit, Unit]): Unit = native
}

@ScalaJSDefined
class LastError(val message: UndefOr[String]) extends js.Object

@native
@JSName("chrome.runtime")
object ChromeRuntime extends js.Object {
  @native
  def lastError: UndefOr[LastError] = native
}

