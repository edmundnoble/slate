package qq

object SharedPrelude {

  val map: Definition = {
    Definition("map",
      params = List("x"),
      body = Filter.compose(Filter.call("x"), Filter.collectResults(Filter.id))
    )
  }

  def all: List[Definition] = List(map)

}
