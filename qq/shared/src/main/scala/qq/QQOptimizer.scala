package qq

import matryoshka._
import FunctorT.ops._

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

  val optimizations: Vector[QQFilter => QQFilter] = Vector(idCompose, ensequenceSingle) map sealWithId
  val allOptimizations: QQFilter => QQFilter = optimizations.reduceLeft(_ compose _)
  def optimize(f: QQFilter): QQFilter = f.transCataT(allOptimizations)

}
