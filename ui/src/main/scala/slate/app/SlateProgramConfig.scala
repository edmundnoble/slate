package slate
package app

import japgolly.scalajs.react.extra.Reusability
import qq.data.JSON
import slate.app.refresh.BootRefreshPolicy

case class SlateProgramConfig(input: JSON, bootRefreshPolicy: BootRefreshPolicy)

object SlateProgramConfig {
  implicit val reusability: Reusability[SlateProgramConfig] =
    Reusability.byRefOr_==
}

