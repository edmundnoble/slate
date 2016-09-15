package qq

import matryoshka.Fix

import scala.language.higherKinds
import scalaz.\/

// smart constructors that make a FilterComponent node including its children
object FilterDSL {

  @inline final def id: ConcreteFilter = Fix(IdFilter())
  @inline final def compose(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(ComposeFilters(first, second))
  @inline final def silence(f: ConcreteFilter): ConcreteFilter = Fix(SilenceExceptions(f))
  @inline final def enlist(f: ConcreteFilter): ConcreteFilter = Fix(EnlistFilter(f))
  @inline final def collectResults(f: ConcreteFilter): ConcreteFilter = Fix(CollectResults(f))
  @inline final def ensequence(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(EnsequenceFilters(first, second))
  @inline final def enject(obj: List[((String \/ ConcreteFilter), ConcreteFilter)]): ConcreteFilter = Fix(EnjectFilters(obj))
  @inline final def call(name: String, params: List[ConcreteFilter] = Nil): ConcreteFilter = Fix(CallFilter(name, params))
  @inline final def selectKey(key: String): ConcreteFilter = Fix(SelectKey(key))
  @inline final def selectIndex(index: Int): ConcreteFilter = Fix(SelectIndex(index))
  @inline final def selectRange(start: Int, end: Int): ConcreteFilter = Fix(SelectRange(start, end))
  @inline final def add(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(AddFilters(first, second))
  @inline final def subtract(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(SubtractFilters(first, second))
  @inline final def multiply(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(MultiplyFilters(first, second))
  @inline final def divide(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(DivideFilters(first, second))
  @inline final def modulo(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter = Fix(ModuloFilters(first, second))
  @inline final def constNumber(value: Double): ConcreteFilter = Fix(ConstNumber(value))
  @inline final def constString(value: String): ConcreteFilter = Fix(ConstString(value))
  @inline final def letAsBinding(name: String, as: ConcreteFilter, in: ConcreteFilter): ConcreteFilter = Fix(LetAsBinding[ConcreteFilter](name, as, in))
  @inline final def deref(name: String): ConcreteFilter = Fix(Dereference[ConcreteFilter](name))

}
