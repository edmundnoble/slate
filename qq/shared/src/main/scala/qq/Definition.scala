package qq

final case class Definition(name: String,
                            params: List[String],
                            body: Filter)
