package slate
package app

import japgolly.scalajs.react.extra.Reusability
import qq.data.JSON
import qq.data.JSON.JSONModification
import slate.app.refresh.BootRefreshPolicy

case class SlateProgramConfig(input: JSON, bootRefreshPolicy: BootRefreshPolicy)

sealed trait SlateProgramConfigModification
case class InputModification(jsonModification: JSONModification) extends SlateProgramConfigModification
case class BootRefreshPolicyModification(newPolicy: BootRefreshPolicy) extends SlateProgramConfigModification

object SlateProgramConfig {
  implicit val configReusability: Reusability[SlateProgramConfig] =
    Reusability.byRefOr_==
  // I would only like to re-render if the shape changes
  implicit val modificationReusability: Reusability[List[SlateProgramConfigModification]] =
    Reusability.byRefOr_==[List[JSONModification]]
      .contramap[List[SlateProgramConfigModification]](_.collect { case InputModification(m) => m }.filter(_.changesShape.value))
}

