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

  object MathOptimizations {
    def constReduce: Optimization = {
      case Fix(AddFilters(Fix(ConstNumber(f)), Fix(ConstNumber(s)))) => Fix(ConstNumber(f + s))
      case Fix(SubtractFilters(Fix(ConstNumber(f)), Fix(ConstNumber(s)))) => Fix(ConstNumber(f - s))
      case Fix(MultiplyFilters(Fix(ConstNumber(f)), Fix(ConstNumber(s)))) => Fix(ConstNumber(f * s))
      case Fix(DivideFilters(Fix(ConstNumber(f)), Fix(ConstNumber(s)))) => Fix(ConstNumber(f / s))
      case Fix(ModuloFilters(Fix(ConstNumber(f)), Fix(ConstNumber(s)))) => Fix(ConstNumber(f % s))
    }
  }

  val optimizations: NonEmptyList[Optimization] =
    NonEmptyList(idCompose, collectEnlist, MathOptimizations.constReduce)
  val allOptimizationsƒ: Filter => Filter = repeatedly(optimizations.foldLeft1(_ orElse _).lift)
  def optimize(f: Filter): Filter = f.transCataT(allOptimizationsƒ)

}
