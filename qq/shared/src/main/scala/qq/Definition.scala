package qq

// parameters are represented as strings for now, sorry performance
final case class Definition(name: String,
                            params: List[String],
                            body: Filter)
