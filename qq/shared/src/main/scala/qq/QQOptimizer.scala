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

  def idCompose: Optimization = {
    case Fix(ComposeFilters(Fix(IdFilter()), s)) => s
    case Fix(ComposeFilters(f, Fix(IdFilter()))) => f
  }

  val optimizations: NonEmptyList[QQFilter => QQFilter] =
    NonEmptyList(idCompose) map (f => repeatedly(f.lift))
  val allOptimizationsƒ: QQFilter => QQFilter = optimizations.foldLeft1(_ >>> _)
  def optimize(f: QQFilter): QQFilter = f.transCataT(allOptimizationsƒ)

}
