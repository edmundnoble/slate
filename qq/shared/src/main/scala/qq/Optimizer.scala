package qq

import matryoshka._
import FunctorT.ops._

import scala.util.control.TailCalls.TailRec
import scalaz.syntax.foldable1._
import scalaz.syntax.functor._
import scalaz.{Functor, NonEmptyList}
import qq.Platform.Rec._

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

  def constFuse: Optimization = {
    case Fix(ComposeFilters(
    Fix(_: ConstantComponent[Fix[FilterComponent]]),
    nextConst@Fix(_: ConstantComponent[Fix[FilterComponent]])
    )) => nextConst
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
    NonEmptyList(constFuse, idCompose, collectEnlist, MathOptimizations.constReduce)
  val allOptimizationsƒ: Filter => Filter = repeatedly(allOptimizations.foldLeft1(_ orElse _).lift)

  def optimizeFilter(filter: Filter): Filter =
    Recursion.transCataT(allOptimizationsƒ).apply(filter)

  def optimizeProgram(program: Program): Program =
    program.copy(defns = program.defns.map(optimizeDefinition), main = optimizeFilter(program.main))

  def optimizeDefinition(defn: Definition): Definition =
    defn.copy(body = optimizeFilter(defn.body))

}
