package dash.app

import dash.ajax._
import shapeless.record.Record
import scala.scalajs.js.UndefOr

object GMailBindings {

  import PathSegment.DSL._

  object Messages {
    val listPath = "https://www.googleapis.com/gmail/v1/users/me/messages" :/: PathEnding
    val list =
      Binding[StringPathSegment :/: PathEnding.type, ListData, AuthHeaders](listPath, AjaxMethod.GET)

    val getPath: StringPathSegment :/: GenericPathSegment[String] :/: PathEnding.type =
      "https://www.googleapis.com/gmail/v1/users/me/messages" :/: StringTag :/: PathEnding
    val get = Binding[StringPathSegment :/: GenericPathSegment[String] :/: PathEnding.type, GetData, AuthHeaders](getPath, AjaxMethod.GET)

    type AuthHeaders =
      Record.`"Authorization" -> String, "Cache-Control" -> String`.T

    type GetData =
      Record.`"format" -> UndefOr[String], "metadataHeaders" -> UndefOr[String], "fields" -> UndefOr[String]`.T

    type ListData =
      Record.`"includeSpamTrash" -> UndefOr[Boolean], "labelIds" -> UndefOr[String], "maxResults" -> UndefOr[Int], "pageToken" -> UndefOr[Int], "q" -> UndefOr[String]`.T
  }

  object Threads {
    val listPath = "https://www.googleapis.com/gmail/v1/users/me/threads" :/: PathEnding
    val list = Binding[StringPathSegment :/: PathEnding.type, ListData, AuthHeaders](listPath, AjaxMethod.GET)

    val getPath: StringPathSegment :/: GenericPathSegment[String] :/: PathEnding.type =
      "https://www.googleapis.com/gmail/v1/users/me/threads" :/: StringTag :/: PathEnding
    val get = Binding[StringPathSegment :/: GenericPathSegment[String] :/: PathEnding.type, GetData, AuthHeaders](getPath, AjaxMethod.GET)

    type AuthHeaders =
      Messages.AuthHeaders

    type GetData =
      Messages.GetData

    type ListData =
      Messages.ListData
  }

}
