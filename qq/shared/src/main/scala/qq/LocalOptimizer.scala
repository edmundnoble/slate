package qq

import matryoshka._
import FunctorT.ops._
import Recursive.ops._
import Corecursive.ops._

import scala.util.control.TailCalls.TailRec
import scalaz.syntax.foldable1._
import scalaz.syntax.functor._
import scalaz.{Functor, NonEmptyList}
import qq.Platform.Rec._

import scala.language.higherKinds

object LocalOptimizer {

  import FilterComponent._

  // like in Matryoshka but tail recursive
  @annotation.tailrec
  final def repeatedly[A](f: A => Option[A])(expr: A): A =
  f(expr) match {
    case None => expr
    case Some(e) => repeatedly(f)(e)
  }

  type LocalOptimization[F] = F => Option[F]

  @inline final def embed[T[_[_]]](v: FilterComponent[T[FilterComponent]])(implicit C: Corecursive[T]): T[FilterComponent] =
    C.embed[FilterComponent](v)

  final def idCompose[T[_[_]] : Recursive : Corecursive]: LocalOptimization[T[FilterComponent]] = { fr =>
    fr.project.map(_.project) match {
      case ComposeFilters(IdFilter(), s) => Some(embed(s))
      case ComposeFilters(f, IdFilter()) => Some(embed(f))
      case _ => None
    }
  }

  final def collectEnlist[T[_[_]] : Recursive]: LocalOptimization[T[FilterComponent]] = { fr =>
    fr.project.map(_.project) match {
      case EnlistFilter(CollectResults(f)) => Some(f)
      case CollectResults(EnlistFilter(f)) => Some(f)
      case _ => None
    }
  }

  final def constFuse[T[_[_]] : Recursive : Corecursive]: LocalOptimization[T[FilterComponent]] = { fr =>
    fr.project.map(_.project) match {
      case ComposeFilters((_: ConstantComponent[T[FilterComponent]]), nextConst@(_: ConstantComponent[T[FilterComponent]])) =>
        Some(embed(nextConst))
      case _ => None
    }
  }

  object MathOptimizations {
    final def constReduce[T[_[_]] : Recursive : Corecursive]: LocalOptimization[T[FilterComponent]] = { fr =>
      fr.project.map(_.project) match {
        case AddFilters(ConstNumber(f), ConstNumber(s)) => Some(embed(ConstNumber(f + s)))
        case SubtractFilters(ConstNumber(f), ConstNumber(s)) => Some(embed(ConstNumber(f - s)))
        case MultiplyFilters(ConstNumber(f), ConstNumber(s)) => Some(embed(ConstNumber(f * s)))
        case DivideFilters(ConstNumber(f), ConstNumber(s)) => Some(embed(ConstNumber(f / s)))
        case ModuloFilters(ConstNumber(f), ConstNumber(s)) => Some(embed(ConstNumber(f % s)))
        case _ => None
      }
    }
  }

  @inline final def localOptimizations[T[_[_]] : Recursive : Corecursive]: NonEmptyList[LocalOptimization[T[FilterComponent]]] =
    NonEmptyList(constFuse[T], idCompose[T], collectEnlist[T], MathOptimizations.constReduce[T])

  @inline final def localOptimizationsƒ[T[_[_]] : Recursive : Corecursive]: T[FilterComponent] => T[FilterComponent] =
    repeatedly[T[FilterComponent]](localOptimizations[T].foldLeft1((f1, f2) =>
      (Function.unlift(f1) orElse Function.unlift(f2)).lift
    ))

  @inline final def optimizeFilter[T[_[_]] : Recursive : Corecursive](filter: T[FilterComponent]): T[FilterComponent] =
    Recursion.transCataT(localOptimizationsƒ[T]).apply(filter)

  @inline final def optimizeDefinition[T[_[_]] : Recursive : Corecursive](defn: Definition[T[FilterComponent]]): Definition[T[FilterComponent]] =
    defn.copy(body = optimizeFilter(defn.body))

  @inline final def optimizeProgram[T[_[_]] : Recursive : Corecursive](program: Program[T[FilterComponent]]): Program[T[FilterComponent]] =
    program.copy(defns = program.defns.mapValues(optimizeDefinition[T]), main = optimizeFilter(program.main))

}
