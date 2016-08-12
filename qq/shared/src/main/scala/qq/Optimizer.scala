package qq

import matryoshka._
import FunctorT.ops._

import scala.util.control.TailCalls.TailRec
import scalaz.syntax.foldable1._
import scalaz.syntax.functor._
import scalaz.{Functor, NonEmptyList}

object Optimizer {

  import FilterComponent._

  // like in Matryoshka but tail recursive
  @annotation.tailrec
  def repeatedly[A](f: A => Option[A])(expr: A): A =
  f(expr) match {
    case None => expr
    case Some(e) => repeatedly(f)(e)
  }

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

  val allOptimizations: NonEmptyList[Optimization] =
    NonEmptyList(idCompose, collectEnlist, MathOptimizations.constReduce)
  val allOptimizationsƒ: Filter => Filter = repeatedly(allOptimizations.foldLeft1(_ orElse _).lift)
  def optimize(filter: Filter): Filter =
    Rec.transCataT(allOptimizationsƒ).apply(Rec.Unsafe.RecLimitStack(128), filter)
  def optimize(program: Program): Program =
    program.copy(defns = program.defns.map(optimize), main = optimize(program.main))
  def optimize(defn: Definition): Definition = defn.copy(body = optimize(defn.body))

}
