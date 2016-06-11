package qq

import monocle.macros._

import scala.language.higherKinds
import scalaz.\/
import matryoshka._

final case class Definition(name: String, params: List[String], body: QQFilter)

object Definition {
  val body = GenLens[Definition](_.body)
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
  def call(name: String): QQFilter = Fix(CallFilter(name))
  def selectKey(key: String): QQFilter = Fix(SelectKey(key))
  def selectIndex(index: Int): QQFilter = Fix(SelectIndex(index))
  def selectRange(start: Int, end: Int): QQFilter = Fix(SelectRange(start, end))

}
