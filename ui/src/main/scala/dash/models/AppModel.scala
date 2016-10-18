package dash
package models

import dash.app.DashboarderApp.ErrorDeserializingProgramOutput
import japgolly.scalajs.react.extra.Reusability

import scalaz.\/

case class AppModel(content: ErrorDeserializingProgramOutput \/ List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
