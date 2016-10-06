package qq
package macros

import fastparse.core.Parsed
import qq.cc.{LocalOptimizer, Parser}
import qq.data._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import language.experimental.macros
import scalaz.\/

// this is used to pre-prepare QQ programs at compile time
object QQInterpolator {

  import scala.language.implicitConversions

  implicit def toqqops(sc: StringContext): qqops = new qqops()(sc)

  def qqimpl(c: whitebox.Context)(pieces: c.Tree*): c.Tree = {
    import c.universe._
    def lift[T: Liftable](value: T): Tree = implicitly[Liftable[T]].apply(value)

    implicit def disjunctionLiftable[E: Liftable, A: Liftable]: Liftable[E \/ A] =
      Liftable[E \/ A](_.fold(e => q"scalaz.-\/(${lift[E](e)})", a => q"scalaz.\/-(${lift[A](a)})"))

    implicit def definitionLiftable[F: Liftable]: Liftable[Definition[F]] =
      Liftable { case Definition(name, params, body) => q"qq.data.Definition(${lift(name)}, ${lift(params)}, ${lift(body)})" }

    implicit def mathOpLiftable: Liftable[MathOperator] =
      Liftable {
        case Add => q"qq.data.Add"
        case Subtract => q"qq.data.Subtract"
        case Multiply => q"qq.data.Multiply"
        case Divide => q"qq.data.Divide"
        case Modulo => q"qq.data.Modulo"
        case Equal => q"qq.data.Equal"
      }

    implicit def pathLiftable: Liftable[PathComponent] =
      Liftable {
        case CollectResults => q"qq.data.CollectResults"
        case SelectKey(k) => q"qq.data.SelectKey(${lift(k)})"
        case SelectIndex(i) => q"qq.data.SelectIndex(${lift(i)})"
        case SelectRange(s, e) => q"qq.data.SelectRange(${lift(s)}, ${lift(e)})"
      }

    implicit def pathOpFLiftable[A : Liftable]: Liftable[PathOperationF[A]] =
      Liftable {
        case PathGet() => q"qq.data.PathGet()"
        case PathModify(m) => q"qq.data.PathModify(${lift(m)})"
        case PathSet(s) => q"qq.data.PathSet(${lift(s)})"
      }

    def conv(value: ConcreteFilter): c.universe.Tree = {
      q"matryoshka.Fix[qq.data.FilterComponent](${value.unFix match {
        case PathOperation(pc, op) => q"qq.data.PathOperation[qq.data.ConcreteFilter](${lift[List[PathComponent]](pc)}, ${lift[PathOperationF[ConcreteFilter]](op)})"
        case AsBinding(name, as, in) => q"qq.data.AsBinding[qq.data.ConcreteFilter](${lift(name)}, ${lift(as)}, ${lift(in)})"
        case Dereference(name) => q"qq.data.Dereference[qq.data.ConcreteFilter](${lift(name)})"
        case ComposeFilters(first, second) => q"qq.data.ComposeFilters[qq.data.ConcreteFilter](${lift(first)}, ${lift(second)})"
        case SilenceExceptions(f) => q"qq.data.SilenceExceptions[qq.data.ConcreteFilter](${lift(f)})"
        case EnlistFilter(f) => q"qq.data.EnlistFilter[qq.data.ConcreteFilter](${lift(f)})"
        case EnsequenceFilters(first, second) => q"qq.data.EnsequenceFilters[qq.data.ConcreteFilter](${lift(first)}, ${lift(second)})"
        case EnjectFilters(obj) => q"qq.data.EnjectFilters[qq.data.ConcreteFilter](${lift(obj)})"
        case CallFilter(name: String, params) => q"qq.data.CallFilter[qq.data.ConcreteFilter](${lift(name)}, ${lift(params)})"
        case FilterNot() => q"qq.data.FilterNot[qq.data.ConcreteFilter]()"
        case ConstNumber(v) => q"qq.data.ConstNumber[qq.data.ConcreteFilter](${lift(v)})"
        case ConstBoolean(v) => q"qq.data.ConstBoolean[qq.data.ConcreteFilter](${lift(v)})"
        case ConstString(v) => q"qq.data.ConstString[qq.data.ConcreteFilter](${lift(v)})"
        case FilterMath(first, second, op) => q"qq.data.FilterMath[qq.data.ConcreteFilter](${lift(first)}, ${lift(second)}, ${lift(op)})"
      }})"
    }

    implicit def concreteFilterLiftable: Liftable[ConcreteFilter] =
      Liftable(conv(_))

    implicit def programLiftable: Liftable[Program[ConcreteFilter]] = new Liftable[Program[ConcreteFilter]] {
      override def apply(value: Program[ConcreteFilter]): c.universe.Tree = {
        q"qq.data.Program(${lift(value.defns)}, ${lift(value.main)})"
      }
    }


    val program = c.prefix.tree match {
      // access data of string interpolation
      case Apply(_, List(Apply(_, rawParts))) =>
        if (rawParts.length != 1) {
          c.abort(c.enclosingPosition, "qq is not an interpolator, it's for ONE STRING")
        }
        rawParts.head match {
          case Literal(Constant(str: String)) => str
          case _ =>
            c.abort(c.enclosingPosition, "invalid") // TODO: make the error message more readable
        }
      //...
      case _ =>
        c.abort(c.enclosingPosition, "invalid") // TODO: make the error message more readable
    }
    val parsedProgram: Program[ConcreteFilter] = Parser.program.parse(program) match {
      case f@Parsed.Failure(_, i, _) =>
        c.abort(c.enclosingPosition.withStart(i).withEnd(i).withPoint(i), "QQ parsing error: " + f)
      case Parsed.Success(prog, _) => prog
    }
    val optimizedProgram = LocalOptimizer.optimizeProgram(parsedProgram)
    lift(optimizedProgram)
  }

}

final class qqops()(val sc: StringContext) {

  def qq(pieces: Any*): Program[ConcreteFilter] = macro QQInterpolator.qqimpl

}
