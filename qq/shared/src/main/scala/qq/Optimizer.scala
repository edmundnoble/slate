package qq

import matryoshka._
import FunctorT.ops._

import scalaz.NonEmptyList
import scalaz.syntax.foldable1._
import scalaz.syntax.arrow._
import scalaz.std.function._

object Optimizer {

  import FilterComponent._

  type Optimization = PartialFunction[Filter, Filter]

  def idCompose: Optimization = {
    case Fix(ComposeFilters(Fix(IdFilter()), s)) => s
    case Fix(ComposeFilters(f, Fix(IdFilter()))) => f
  }

  def collectEnlist: Optimization = {
    case Fix(EnlistFilter(Fix(CollectResults(f)))) => f
    case Fix(CollectResults(Fix(EnlistFilter(f)))) => f
  }

  val optimizations: NonEmptyList[Filter => Filter] =
    NonEmptyList(idCompose, collectEnlist) map (f => repeatedly(f.lift))
  val allOptimizationsƒ: Filter => Filter = optimizations.foldLeft1(_ >>> _)
  def optimize(f: Filter): Filter = f.transCataT(allOptimizationsƒ)

}
