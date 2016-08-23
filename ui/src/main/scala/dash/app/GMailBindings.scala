package dash.app

import dash.ajax._
import scala.scalajs.js.UndefOr

object GMailBindings {

  import PathSegment.DSL._
  import shapeless._
  import record._
  import syntax.singleton._
  import shapeless.labelled._

  object Messages {
    val listPath =
      "https://www.googleapis.com/gmail/v1/users/me/messages" :/: PathEnding
    val list =
      Binding[ConstantPathSegment :/: PathEnding.type, ListData, AuthHeaders](listPath, AjaxMethod.GET)

    val idWitness = Witness("id")
    type id = idWitness.T

    val getPath =
      "https://www.googleapis.com/gmail/v1/users/me/messages" :/: field[id](StringTag) :/: PathEnding
    val get = Binding[ConstantPathSegment :/: GenericPathSegment[id, String] :/: PathEnding.type, GetData, AuthHeaders](getPath, AjaxMethod.GET)

    type AuthHeaders =
      Record.`"Authorization" -> String, "Cache-Control" -> String`.T

    type GetData =
      Record.`"format" -> UndefOr[String], "metadataHeaders" -> UndefOr[String], "fields" -> UndefOr[String]`.T

    type ListData =
      Record.`"includeSpamTrash" -> UndefOr[Boolean], "labelIds" -> UndefOr[String], "maxResults" -> UndefOr[Int], "pageToken" -> UndefOr[Int], "q" -> UndefOr[String]`.T
  }

  object Threads {
    val listPath =
      "https://www.googleapis.com/gmail/v1/users/me/threads" :/: PathEnding
    val list = Binding[ConstantPathSegment :/: PathEnding.type, ListData, AuthHeaders](listPath, AjaxMethod.GET)

    val getPath =
      "https://www.googleapis.com/gmail/v1/users/me/threads" :/: field[Messages.id](StringTag) :/: PathEnding
    val get = Binding[ConstantPathSegment :/: GenericPathSegment[Messages.id, String] :/: PathEnding.type, GetData, AuthHeaders](getPath, AjaxMethod.GET)

    type AuthHeaders =
      Messages.AuthHeaders

    type GetData =
      Messages.GetData

    type ListData =
      Messages.ListData
  }

}
