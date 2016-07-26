package qq

import monocle.macros.GenLens
import scala.language.higherKinds

final case class Definition(name: String,
                            params: List[String],
                            body: Filter)

object Definition {
  val body = GenLens[Definition](_.body)
}
