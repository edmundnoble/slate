package qq

import scala.language.higherKinds
import scalaz.\/
import matryoshka._
import shapeless._
import shapeless.ops.nat.ToInt

final case class Definition[N <: Nat](name: String,
                                      params: Sized[List[String], N],
                                      body: QQFilter)
                                     (implicit val numParams: ToInt[N]) {
  type Aux = N
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
  def call(name: String): QQFilter = Fix(CallFilter(name, Nil))
  def selectKey(key: String): QQFilter = Fix(SelectKey(key))
  def selectIndex(index: Int): QQFilter = Fix(SelectIndex(index))
  def selectRange(start: Int, end: Int): QQFilter = Fix(SelectRange(start, end))

}
