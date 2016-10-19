package qq
package cc

import qq.data._
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

import scala.language.higherKinds
import cats.implicits._
import cats.implicits._

// QQ's local optimizer; not a whole-program optimizer, but optimizes a single filter.
object LocalOptimizer {

  import FilterComponent._

  // runs a function until it returns none
  // like in Matryoshka but tail recursive
  @annotation.tailrec
  final def repeatedly[A](f: A => Option[A])(expr: A): A =
  f(expr) match {
    case None => expr
    case Some(e) => repeatedly(f)(e)
  }

  // an optimization is a function with type Filter => Option[Filter] with return values meaning:
  // None => The optimization does not apply to this filter structure
  // Some(newFilter) => The optimization produced newFilter from this filter
  type LocalOptimization[F] = F => Option[F]

  // The identity filter is an identity with respect to composition of filters
  final def idCompose(fr: ConcreteFilter): Option[ConcreteFilter] = {
    fr.unFix.map(_.unFix) match {
      case ComposeFilters(PathOperation(List(), PathGet), s) => Some(embed(s))
      case ComposeFilters(f, PathOperation(List(), PathGet)) => Some(embed(f))
      case _ => None
    }
  }

  // The collect and enlist operations are inverses
  final def collectEnlist(fr: ConcreteFilter): Option[ConcreteFilter] = {
    fr.unFix.map(_.unFix.map(_.unFix)) match {
      case EnlistFilter(ComposeFilters(f, PathOperation(List(CollectResults), PathGet))) => Some(embed(f))
      case EnlistFilter(PathOperation(List(CollectResults), PathGet)) => Some(embed(PathOperation(Nil, PathGet)))
      case _ => None
    }
  }

  // reduces constant math filters to their results
  final def constMathReduce(fr: ConcreteFilter): Option[ConcreteFilter] = {
    fr.unFix.map(_.unFix) match {
      case FilterMath(ConstNumber(f), ConstNumber(s), op) => op match {
        case Add => Some(embed(ConstNumber(f + s)))
        case Subtract => Some(embed(ConstNumber(f - s)))
        case Multiply => Some(embed(ConstNumber(f * s)))
        case Divide => Some(embed(ConstNumber(f / s)))
        case Modulo => Some(embed(ConstNumber(f % s)))
        case Equal => Some(embed(ConstBoolean(f == s)))
        case LTE => Some(embed(ConstBoolean(f <= s)))
        case GTE => Some(embed(ConstBoolean(f >= s)))
        case LessThan => Some(embed(ConstBoolean(f < s)))
        case GreaterThan => Some(embed(ConstBoolean(f > s)))
      }
      case _ => None
    }
  }

  // a function applying each of the local optimizations available, in rounds,
  // until none of the optimizations applies anymore
  @inline final def localOptimizationsƒ(tf: ConcreteFilter): ConcreteFilter =
  repeatedly[ConcreteFilter](tf => collectEnlist(tf) orElse idCompose(tf) orElse constMathReduce(tf))(tf)

  // localOptimizationsƒ recursively applied deep into a filter
  @inline final def optimizeFilter(filter: ConcreteFilter)(implicit rec: RecursionEngine): ConcreteFilter =
  Recursion.transCataT(localOptimizationsƒ).apply(filter)

  @inline final def optimizeDefinition(defn: Definition[ConcreteFilter])(implicit rec: RecursionEngine): Definition[ConcreteFilter] =
    defn.copy(body = optimizeFilter(defn.body))

  @inline final def optimizeProgram(program: Program[ConcreteFilter])(implicit rec: RecursionEngine): Program[ConcreteFilter] =
    program.copy(defns = program.defns.map(optimizeDefinition), main = optimizeFilter(program.main))

}
