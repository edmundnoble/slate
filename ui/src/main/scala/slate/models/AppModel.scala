package slate
package models

import slate.app.DashboarderApp.ErrorDeserializingProgramOutput
import japgolly.scalajs.react.extra.Reusability



case class AppModel(content: ErrorDeserializingProgramOutput Either List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
