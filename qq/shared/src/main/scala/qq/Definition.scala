package qq

import monocle.macros.GenLens

final case class Definition(name: String,
                            params: List[String],
                            body: Filter)

object Definition {
  val body = GenLens[Definition](_.body)
}
