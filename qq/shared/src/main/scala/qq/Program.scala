package qq

object Program {
  type Definitions = Map[String, Definition]
}

case class Program(defns: Program.Definitions, main: Filter)

