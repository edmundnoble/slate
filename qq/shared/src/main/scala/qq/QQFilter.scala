package qq

import scala.language.higherKinds
import scalaz.\/
import matryoshka._
import qq.QQCompiler.QQCompilationException
import shapeless._
import shapeless.ops.nat.ToInt
import scalaz.syntax.either._

final case class Definition(name: String,
                            params: List[String],
                            body: QQFilter)

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
  def addFilters(first: QQFilter, second: QQFilter): QQFilter = Fix(AddFilters(first, second))
  def subtractFilters(first: QQFilter, second: QQFilter): QQFilter = Fix(SubtractFilters(first, second))
  def multiplyFilters(first: QQFilter, second: QQFilter): QQFilter = Fix(MultiplyFilters(first, second))
  def divideFilters(first: QQFilter, second: QQFilter): QQFilter = Fix(DivideFilters(first, second))
  def moduloFilters(first: QQFilter, second: QQFilter): QQFilter = Fix(ModuloFilters(first, second))
  def constNumber(value: Double): QQFilter = Fix(ConstNumber(value))
  def constString(value: String): QQFilter = Fix(ConstString(value))


}
