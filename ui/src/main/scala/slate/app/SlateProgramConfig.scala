package slate
package app

import cats.implicits._
import japgolly.scalajs.react.extra.Reusability
import qq.data.JSON
import qq.data.JSON.{JSONModification, ModifiedJSON}
import slate.app.refresh.BootRefreshPolicy
import upickle.Js

final case class SlateProgramConfig(input: JSON, bootRefreshPolicy: BootRefreshPolicy)

// SlateProgramConfigModification is the type of modifications to a SlateProgramConfig.
// That is to say, any atomic modification you want to make to a field of a SlateProgramConfig
// is expressible as a SlateProgramConfigModification. You can represent an arbitrary number of these
// actions with an arbitrary collection like List[SlateProgramConfigModification].
// This will constitute a monoid action on SlateProgramConfig.
sealed trait SlateProgramConfigModification {
  def changesShape: Boolean
}

final case class InputModification(jsonModification: JSONModification) extends SlateProgramConfigModification {
  def changesShape: Boolean = jsonModification.changesShape
}

final case class BootRefreshPolicyModification(newPolicy: BootRefreshPolicy) extends SlateProgramConfigModification {
  def changesShape: Boolean = false
}

final case class ModifiedSlateProgramConfig(input: ModifiedJSON, bootRefreshPolicy: BootRefreshPolicy, bootRefreshPolicyModified: Boolean) {
  def commit: SlateProgramConfig = {
    SlateProgramConfig(JSON.commit(input), bootRefreshPolicy)
  }
}

object ModifiedSlateProgramConfig {
  def unmodified(config: SlateProgramConfig): ModifiedSlateProgramConfig = {
    ModifiedSlateProgramConfig(JSON.unmodified(config.input), config.bootRefreshPolicy, bootRefreshPolicyModified = false)
  }
}

object SlateProgramConfigModification {

  implicit final class slateProgramModificationsOps(mods: List[SlateProgramConfigModification]) {
    def apply(config: SlateProgramConfig): Option[ModifiedSlateProgramConfig] =
      mods.foldM[Option, ModifiedSlateProgramConfig](ModifiedSlateProgramConfig.unmodified(config))((c, m) => m(c))
  }

  implicit final class slateProgramModificationOps(mod: SlateProgramConfigModification) {
    def apply(config: ModifiedSlateProgramConfig): Option[ModifiedSlateProgramConfig] =
      mod match {
        case InputModification(jm) => jm(config.input, JSON.Null, "" -> JSON.Null).map(newInput => config.copy(input = newInput))
        case BootRefreshPolicyModification(newPolicy) => Some(config.copy(bootRefreshPolicy = newPolicy))
      }
  }

}

object SlateProgramConfig {

  import upickle.default._

  implicit val pkl: Reader[SlateProgramConfig] with Writer[SlateProgramConfig] =
    new Reader[SlateProgramConfig] with Writer[SlateProgramConfig] {
      def read0: PartialFunction[Js.Value, SlateProgramConfig] = {
        Function.unlift[Js.Value, SlateProgramConfig] {
          case o: Js.Obj =>
            val map = o.value.toMap
            for {
              input <- map.get("input")
              bootRefreshPolicyRaw <- map.get("bootRefreshPolicy")
              bootRefreshPolicy <- BootRefreshPolicy.pkl.read.lift(bootRefreshPolicyRaw)
            } yield SlateProgramConfig(input = qq.Json.upickleToJSONRec(input), bootRefreshPolicy = bootRefreshPolicy)
          case _ => None
        }
      }

      def write0: (SlateProgramConfig) => Js.Value = { config: SlateProgramConfig =>
        Js.Obj(
          "input" -> qq.Json.JSONToUpickleRec(config.input),
          "bootRefreshPolicy" -> BootRefreshPolicy.pkl.write(config.bootRefreshPolicy)
        )
      }
    }

  implicit val configReusability: Reusability[SlateProgramConfig] =
    Reusability.byRefOr_==

  // I would only like to re-render if the shape changes
  implicit val modificationReusability: Reusability[List[SlateProgramConfigModification]] =
    Reusability.byRefOr_==[List[JSONModification]]
      .contramap[List[SlateProgramConfigModification]](
      _.collect { case InputModification(m) if m.changesShape => m }
    )
}

