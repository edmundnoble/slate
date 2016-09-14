package qq

object Program {
  type Definitions[F] = Map[String, Definition[F]]
}

case class Program[F](defns: Program.Definitions[F], main: Filter)

