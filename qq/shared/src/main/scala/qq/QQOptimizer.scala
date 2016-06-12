package qq

import matryoshka._
import FunctorT.ops._

import scalaz.NonEmptyList
import scalaz.syntax.foldable1._
import scalaz.syntax.arrow._
import scalaz.std.function._

object QQOptimizer {

  import QQFilterComponent._

  type Optimization = PartialFunction[QQFilter, QQFilter]

  def sealWithId[A](pf: PartialFunction[A, A]): A => A = { (a: A) => pf.applyOrElse(a, identity[A]) }

  def idCompose: Optimization = {
    case Fix(ComposeFilters(Fix(IdFilter()), s)) => s
    case Fix(ComposeFilters(f, Fix(IdFilter()))) => f
  }

  def ensequenceSingle: Optimization = {
    case Fix(EnsequenceFilters(onef :: Nil)) => onef
  }

  val optimizations: NonEmptyList[QQFilter => QQFilter] = NonEmptyList(idCompose, ensequenceSingle) map sealWithId
  val allOptimizationsƒ: QQFilter => QQFilter = optimizations.foldLeft1(_ >>> _)
  def optimize(f: QQFilter): QQFilter = f.transCataT(allOptimizationsƒ)

}
