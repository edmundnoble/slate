package qq

import matryoshka.Fix

import scala.language.higherKinds
import scalaz.\/

object FilterDSL {

  def id: Filter = Fix(IdFilter())
  def fetch: Filter = Fix(FetchApi())
  def compose(first: Filter, second: Filter): Filter = Fix(ComposeFilters(first, second))
  def silence(f: Filter): Filter = Fix(SilenceExceptions(f))
  def enlist(f: Filter): Filter = Fix(EnlistFilter(f))
  def collectResults(f: Filter): Filter = Fix(CollectResults(f))
  def ensequence(first: Filter, second: Filter): Filter = Fix(EnsequenceFilters(first, second))
  def enject(obj: List[((String \/ Filter), Filter)]): Filter = Fix(EnjectFilters(obj))
  def call(name: String, params: List[Filter] = Nil): Filter = Fix(CallFilter(name, params))
  def selectKey(key: String): Filter = Fix(SelectKey(key))
  def selectIndex(index: Int): Filter = Fix(SelectIndex(index))
  def selectRange(start: Int, end: Int): Filter = Fix(SelectRange(start, end))
  def add(first: Filter, second: Filter): Filter = Fix(AddFilters(first, second))
  def subtract(first: Filter, second: Filter): Filter = Fix(SubtractFilters(first, second))
  def multiply(first: Filter, second: Filter): Filter = Fix(MultiplyFilters(first, second))
  def divide(first: Filter, second: Filter): Filter = Fix(DivideFilters(first, second))
  def modulo(first: Filter, second: Filter): Filter = Fix(ModuloFilters(first, second))
  def constNumber(value: Double): Filter = Fix(ConstNumber(value))
  def constString(value: String): Filter = Fix(ConstString(value))

}
