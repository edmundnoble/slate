package slate
package app

import cats.implicits._
import japgolly.scalajs.react.extra.Reusability
import qq.data.JSON
import qq.data.JSON.{JSONModification, ModifiedJSON}
import slate.app.refresh.BootRefreshPolicy

case class SlateProgramConfig(input: JSON, bootRefreshPolicy: BootRefreshPolicy)

// SlateProgramConfigModification is the type of modifications to a SlateProgramConfig.
// That is to say, any individual modification you want to make to a field of a SlateProgramConfig
// is expressible as a SlateProgramConfigModification. You can represent an arbitrary number of these
// actions with an arbitrary collection like List[SlateProgramConfigModification].
// This will constitute a monoid action on SlateProgramConfig.
sealed trait SlateProgramConfigModification {
  def changesShape: Boolean
}
case class InputModification(jsonModification: JSONModification) extends SlateProgramConfigModification {
  override def changesShape: Boolean = jsonModification.changesShape.value
}
case class BootRefreshPolicyModification(newPolicy: BootRefreshPolicy) extends SlateProgramConfigModification {
  override def changesShape: Boolean = false
}

case class ModifiedSlateProgramConfig(input: ModifiedJSON, bootRefreshPolicy: BootRefreshPolicy, bootRefreshPolicyModified: Boolean) {
  def commit: SlateProgramConfig = SlateProgramConfig(JSON.commit(input), bootRefreshPolicy)
}

object ModifiedSlateProgramConfig {
  def unmodified(config: SlateProgramConfig): ModifiedSlateProgramConfig =
    ModifiedSlateProgramConfig(JSON.unmodified(config.input), config.bootRefreshPolicy, bootRefreshPolicyModified = false)
}

object SlateProgramConfigModification {
  implicit final class slateProgramModificationsOps(mods: List[SlateProgramConfigModification]) {
    def apply(config: SlateProgramConfig): Option[ModifiedSlateProgramConfig] =
      mods.foldM[Option, ModifiedSlateProgramConfig](ModifiedSlateProgramConfig.unmodified(config))((c, m) => m(c))
  }
  implicit final class slateProgramModificationOps(mod: SlateProgramConfigModification) {
    def apply(config: ModifiedSlateProgramConfig): Option[ModifiedSlateProgramConfig] = mod match {
      case InputModification(jm) => jm(config.input, JSON.Null, "" -> JSON.Null).map(newInput => config.copy(input = newInput))
      case BootRefreshPolicyModification(newPolicy) => Some(config.copy(bootRefreshPolicy = newPolicy))
    }
  }
}

object SlateProgramConfig {
  implicit val configReusability: Reusability[SlateProgramConfig] =
    Reusability.byRefOr_==
  // I would only like to re-render if the shape changes
  implicit val modificationReusability: Reusability[List[SlateProgramConfigModification]] =
    Reusability.byRefOr_==[List[JSONModification]]
      .contramap[List[SlateProgramConfigModification]](_.collect { case InputModification(m) => m }.filter(_.changesShape.value))
}

