package qq

// parameters are represented as strings for now, sorry performance
final case class Definition[F](name: String,
                               params: List[String],
                               body: F)
