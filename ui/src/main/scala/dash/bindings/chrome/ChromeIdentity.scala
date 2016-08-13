package dash.bindings.chrome

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.{JSName, ScalaJSDefined}

@ScalaJSDefined
class GetAuthTokenOptions(val interactive: UndefOr[Boolean] = js.undefined,
                          val account: UndefOr[AccountInfo] = js.undefined,
                          val scopes: UndefOr[js.Array[String]] = js.undefined) extends js.Object

@ScalaJSDefined
class AccountInfo(val id: String) extends js.Object

@js.native
@JSName("chrome.identity")
object ChromeIdentity extends js.Object {
  //noinspection AccessorLikeMethodIsUnit
  @js.native
  def getAuthToken(details: UndefOr[GetAuthTokenOptions] = js.undefined,
                   callback: UndefOr[js.Function1[UndefOr[String], Unit]]): Unit = js.native
}

