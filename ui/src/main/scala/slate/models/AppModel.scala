package slate
package models

import slate.app.SlateApp.AllErrors
import japgolly.scalajs.react.extra.Reusability


case class AppModel(content: AllErrors Either List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
