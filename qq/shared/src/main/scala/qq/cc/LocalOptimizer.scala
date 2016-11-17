package qq
package cc

import qq.data._
import qq.util.Recursion.RecursionEngine
import qq.util.{Fix, Recursion}

import scala.language.higherKinds

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
  final def idCompose(fr: FilterComponent[FilterAST]): Option[FilterComponent[FilterAST]] = {
    fr match {
      case ComposeFilters(Fix(PathOperation(List(), PathGet)), Fix(s)) => Some(s)
      case ComposeFilters(Fix(f), Fix(PathOperation(List(), PathGet))) => Some(f)
      case _ => None
    }
  }

  // The collect and enlist operations are inverses
  final def collectEnlist(fr: FilterComponent[FilterAST]): Option[FilterComponent[FilterAST]] = {
    fr match {
      case EnlistFilter(Fix(ComposeFilters(Fix(f), Fix(PathOperation(List(CollectResults), PathGet))))) => Some(f)
      case EnlistFilter(Fix(PathOperation(List(CollectResults), PathGet))) => Some(PathOperation(Nil, PathGet))
      case _ => None
    }
  }

  // reduces constant math filters to their results
  final def constMathReduce(fr: FilterComponent[FilterAST]): Option[FilterComponent[FilterAST]] = fr match {
    case FilterMath(Fix(ConstNumber(f)), Fix(ConstNumber(s)), op) => op match {
      case Add => Some(ConstNumber(f + s))
      case Subtract => Some(ConstNumber(f - s))
      case Multiply => Some(ConstNumber(f * s))
      case Divide => Some(ConstNumber(f / s))
      case Modulo => Some(ConstNumber(f % s))
      case Equal => Some(ConstBoolean(f == s))
      case LTE => Some(ConstBoolean(f <= s))
      case GTE => Some(ConstBoolean(f >= s))
      case LessThan => Some(ConstBoolean(f < s))
      case GreaterThan => Some(ConstBoolean(f > s))
    }
    case _ => None
  }

  final def unlet(fr: FilterComponent[FilterAST]): Option[FilterComponent[FilterAST]] = fr match {
    case AsBinding(n, Fix(a), Fix(i))
      if i == Dereference(n) || i == PathOperation(Nil, PathGet) =>
      Some(a)
    case _ => None
  }

  // a function applying each of the local optimizations available, in rounds,
  // until none of the optimizations applies anymore
  @inline final def localOptimizationsƒ(tf: FilterAST): FilterAST =
  repeatedly[FilterAST] { tf =>
    val unfixed = tf.unFix
    (collectEnlist(unfixed) orElse idCompose(unfixed) orElse constMathReduce(unfixed) orElse unlet(unfixed)) map embed
  }(tf)

  // localOptimizationsƒ recursively applied deep into a filter
  @inline final def optimizeFilter(filter: FilterAST)(implicit rec: RecursionEngine): FilterAST =
  Recursion.transCataT(localOptimizationsƒ).apply(filter)

  @inline final def optimizeDefinition(defn: Definition[FilterAST])(implicit rec: RecursionEngine): Definition[FilterAST] =
    defn.copy(body = optimizeFilter(defn.body))

  @inline final def optimizeProgram(program: Program[FilterAST])(implicit rec: RecursionEngine): Program[FilterAST] =
    program.copy(defns = program.defns.map(optimizeDefinition), main = optimizeFilter(program.main))

}
