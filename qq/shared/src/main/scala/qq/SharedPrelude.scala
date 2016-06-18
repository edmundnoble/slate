package qq

object SharedPrelude {

  val map: Definition = {
    Definition("map",
      params = List("x"),
      body = Filter.compose(Filter.collectResults(Filter.id), Filter.call("x"))
    )
  }

  def all: List[Definition] = List(map)

}
