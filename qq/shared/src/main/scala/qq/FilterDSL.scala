package qq

import matryoshka.Fix

import scala.language.higherKinds
import scalaz.\/

object FilterDSL {

  @inline final def id: Filter = Fix(IdFilter())
  @inline final def fetch: Filter = Fix(FetchApi())
  @inline final def compose(first: Filter, second: Filter): Filter = Fix(ComposeFilters(first, second))
  @inline final def silence(f: Filter): Filter = Fix(SilenceExceptions(f))
  @inline final def enlist(f: Filter): Filter = Fix(EnlistFilter(f))
  @inline final def collectResults(f: Filter): Filter = Fix(CollectResults(f))
  @inline final def ensequence(first: Filter, second: Filter): Filter = Fix(EnsequenceFilters(first, second))
  @inline final def enject(obj: List[((String \/ Filter), Filter)]): Filter = Fix(EnjectFilters(obj))
  @inline final def call(name: String, params: List[Filter] = Nil): Filter = Fix(CallFilter(name, params))
  @inline final def selectKey(key: String): Filter = Fix(SelectKey(key))
  @inline final def selectIndex(index: Int): Filter = Fix(SelectIndex(index))
  @inline final def selectRange(start: Int, end: Int): Filter = Fix(SelectRange(start, end))
  @inline final def add(first: Filter, second: Filter): Filter = Fix(AddFilters(first, second))
  @inline final def subtract(first: Filter, second: Filter): Filter = Fix(SubtractFilters(first, second))
  @inline final def multiply(first: Filter, second: Filter): Filter = Fix(MultiplyFilters(first, second))
  @inline final def divide(first: Filter, second: Filter): Filter = Fix(DivideFilters(first, second))
  @inline final def modulo(first: Filter, second: Filter): Filter = Fix(ModuloFilters(first, second))
  @inline final def constNumber(value: Double): Filter = Fix(ConstNumber(value))
  @inline final def constString(value: String): Filter = Fix(ConstString(value))

}
