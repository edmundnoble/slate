package qq

import scala.language.higherKinds
import scalaz.\/
import matryoshka._
import qq.QQCompiler.QQCompilationException
import shapeless._
import shapeless.ops.nat.ToInt
import scalaz.syntax.either._

final case class Definition[N <: Nat](name: String,
                                      params: Sized[List[String], N],
                                      body: QQFilter)(implicit val ev: ToInt[N]) {
  def modifyBody(fun: QQFilter => QQFilter) = copy[N](name, params, fun(body))(ev)
  //  def appl
  def foldfun(compiler: QQCompiler)(soFar: QQCompilationException \/ List[compiler.CompiledDefinition[_]]) = {
    soFar.map(definitionsSoFar =>
      compiler.CompiledDefinition[N](name, (params: Sized[List[compiler.CompiledFilter], N]) => {
        val paramsAsDefinitions = (params.unsized, params.unsized).zipped.map { (filterName, value) =>
          compiler.CompiledDefinition[_0](name, (_: Sized[List[compiler.CompiledFilter], _0]) => value.right[QQCompilationException])
        }
        compiler.compile(definitionsSoFar ++ paramsAsDefinitions, body)
      })(ev) :: definitionsSoFar
    )
  }
}

object QQFilter {

  import QQFilterComponent._

  def id: QQFilter = Fix(IdFilter())
  def fetch: QQFilter = Fix(FetchApi())
  def compose(first: QQFilter, second: QQFilter): QQFilter = Fix(ComposeFilters(first, second))
  def silence(f: QQFilter): QQFilter = Fix(SilenceExceptions(f))
  def enlist(f: QQFilter): QQFilter = Fix(EnlistFilter(f))
  def collectResults(f: QQFilter): QQFilter = Fix(CollectResults(f))
  def ensequence(filters: List[QQFilter]): QQFilter = Fix(EnsequenceFilters(filters))
  def enject(obj: List[((String \/ QQFilter), QQFilter)]): QQFilter = Fix(EnjectFilters(obj))
  def call(name: String, params: List[QQFilter] = Nil): QQFilter = Fix(CallFilter(name, params))
  def selectKey(key: String): QQFilter = Fix(SelectKey(key))
  def selectIndex(index: Int): QQFilter = Fix(SelectIndex(index))
  def selectRange(start: Int, end: Int): QQFilter = Fix(SelectRange(start, end))

}
