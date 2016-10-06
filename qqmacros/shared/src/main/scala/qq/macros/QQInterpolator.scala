package qq
package macros

import fastparse.core.Parsed
import qq.cc.{LocalOptimizer, Parser}
import qq.data._
import qq.util.Recursion.RecursiveFunction

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import language.experimental.macros
import scalaz.Free.Trampoline
import scalaz.{Free, Trampoline, \/}
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.tuple._
import scalaz.syntax.apply._
import scalaz.syntax.bitraverse._
import qq.Platform.Rec._

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

    def pathOpLift[A](f: A => Trampoline[c.universe.Tree]): PathOperationF[A] => Trampoline[c.universe.Tree] = {
      case PathGet() => Trampoline.done(q"qq.data.PathGet()")
      case PathModify(m) => f(m).map { r => q"qq.data.PathModify($r)" }
      case PathSet(s) => f(s).map { r => q"qq.data.PathSet($r)" }
    }

    val liftFilter: RecursiveFunction[ConcreteFilter, c.universe.Tree] = new RecursiveFunction[ConcreteFilter, c.universe.Tree] {
      override def run(value: ConcreteFilter, loop: ConcreteFilter => Trampoline[c.universe.Tree]): Trampoline[c.universe.Tree] = {
        val sub: Trampoline[c.universe.Tree] = value.unFix match {
          case PathOperation(pc, op) => pathOpLift(loop)(op) map { o => q"qq.data.PathOperation[qq.data.ConcreteFilter](${lift(pc)}, $o)" }
          case AsBinding(name, as, in) => (loop(as) |@| loop(in)) { (a, i) => q"qq.data.AsBinding[qq.data.ConcreteFilter](${lift(name)}, $a, $i)" }
          case Dereference(name) => Trampoline.done(q"qq.data.Dereference[qq.data.ConcreteFilter](${lift(name)})")
          case ComposeFilters(first, second) => (loop(first) |@| loop(second)) { (f, s) => q"qq.data.ComposeFilters[qq.data.ConcreteFilter]($f, $s)" }
          case SilenceExceptions(child) => loop(child) map { f => q"qq.data.SilenceExceptions[qq.data.ConcreteFilter]($f)" }
          case EnlistFilter(child) => loop(child) map { f => q"qq.data.EnlistFilter[qq.data.ConcreteFilter]($f)" }
          case EnsequenceFilters(first, second) => (loop(first) |@| loop(second)) { (f, s) => q"qq.data.EnsequenceFilters[qq.data.ConcreteFilter]($f, $s)" }
          case EnjectFilters(obj) => obj.traverse[Trampoline, (c.universe.Tree, c.universe.Tree)] { case (k, v) =>
            for {
              ke <- k.traverse(loop).map(e => lift(e.leftMap(lift(_))))
              ve <- loop(v)
            } yield (ke, ve)
          }
            .map { o => q"qq.data.EnjectFilters[qq.data.ConcreteFilter](${lift(o)})" }
          case CallFilter(name: String, params) => params.traverse(loop).map { p => q"qq.data.CallFilter[qq.data.ConcreteFilter](${lift(name)}, $p)" }
          case FilterNot() => Trampoline.done(q"qq.data.FilterNot[qq.data.ConcreteFilter]()")
          case ConstNumber(v) => Trampoline.done(q"qq.data.ConstNumber[qq.data.ConcreteFilter](${lift(v)})")
          case ConstBoolean(v) => Trampoline.done(q"qq.data.ConstBoolean[qq.data.ConcreteFilter](${lift(v)})")
          case ConstString(v) => Trampoline.done(q"qq.data.ConstString[qq.data.ConcreteFilter](${lift(v)})")
          case FilterMath(first, second, op) => (loop(first) |@| loop(second)) { (f, s) => q"qq.data.FilterMath[qq.data.ConcreteFilter]($f, $s, ${lift(op)})" }
        }
        sub.map(f => q"matryoshka.Fix[qq.data.FilterComponent]($f)")
      }
    }

    implicit def concreteFilterLiftable: Liftable[ConcreteFilter] =
      Liftable(liftFilter(_))

    implicit def programLiftable: Liftable[Program[ConcreteFilter]] = Liftable[Program[ConcreteFilter]](
      value => q"qq.data.Program(${lift(value.defns)}, ${lift(value.main)})"
    )

    val program = c.prefix.tree match {
      // access data of string interpolation
      case Apply(_, List(Apply(_, rawParts))) =>
        if (rawParts.length != 1) {
          c.abort(c.enclosingPosition, "$ detected. qq is not an interpolator, it's for a single string")
        }
        rawParts.head match {
          case Literal(Constant(str: String)) => str
          case _ =>
            c.abort(c.enclosingPosition, "invalid") // TODO: make the error message more readable
        }
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
