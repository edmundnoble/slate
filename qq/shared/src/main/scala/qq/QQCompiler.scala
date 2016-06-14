package qq

import qq.Util._
import qq.QQCompiler.{NoSuchMethod, QQCompilationException, QQRuntimeException, WrongNumParams}
import qq.Definition
import qq.QQFilterComponent._
import monix.eval.{Coeval, Task}

import scalaz.std.list._
import scalaz.{EitherT, \/}
import matryoshka._
import Recursive.ops._
import Corecursive.ops._
import TraverseT.ops._
import monocle.macros.GenLens
import shapeless._
import ops.hlist.{Length, Mapped}
import shapeless.LUBConstraint.<<:
import shapeless.ops.nat.ToInt
import shapeless.syntax.sized._

import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

abstract class QQCompiler {

  type AnyTy
  type CompiledFilter = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T
  sealed abstract class CDefBase {
    type Aux <: Nat
  }

  case class CompiledDefinition[
  N <: Nat
  ](name: String, body: Sized[List[CompiledFilter], N] => QQCompilationException \/ CompiledFilter)(implicit val numParams: ToInt[N]) extends CDefBase {
    override type Aux = N
  }

  trait QQPrelude {
    def length: CompiledDefinition[_0]

    def keys: CompiledDefinition[_0]

    def arrays: CompiledDefinition[_0]

    def objects: CompiledDefinition[_0]

    def iterables: CompiledDefinition[_0]

    def booleans: CompiledDefinition[_0]

    def numbers: CompiledDefinition[_0]

    def strings: CompiledDefinition[_0]

    def nulls: CompiledDefinition[_0]

    def values: CompiledDefinition[_0]

    def scalars: CompiledDefinition[_0]

    def all: List[CompiledDefinition[_]] =
      length :: keys :: arrays :: objects :: iterables :: booleans ::
        numbers :: strings :: nulls :: values :: scalars :: Nil
  }

  def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: AnyTy =>
    Task.sequence(functions.map(_ (jsv))).map(_.flatten)
  }

  import ops.hlist.Comapped

  object foldfun extends Poly2 {
    implicit def st0[In0 <: HList, N <: Nat](implicit ev: UnaryTCConstraint[In0, CompiledDefinition]) =
      at[\/[QQCompilationException, List[CompiledDefinition[_]]], Definition[N]] { (soFar, nextDefinition) =>
        soFar.map(definitionsSoFar =>
          CompiledDefinition[nextDefinition.Aux](nextDefinition.name, (params: Sized[List[CompiledFilter], N]) => {
            val paramsAsDefinitions = (nextDefinition.params.unsized, params.unsized).zipped.map((name, value) => CompiledDefinition[_0](name, _ => value.right))
            compile(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
          })(nextDefinition.numParams)
        )
      }
  }

  def compileProgram[NS <: HList, L <: HList](definitions: L, main: QQFilter)(implicit ev: Comapped.Aux[L, Definition, NS],
                                                                                      ev2: LUBConstraint[NS, Nat]): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: List[CompiledDefinition[_]] =
      definitions.foldLeft(nil[CompiledDefinition[_]].right[QQCompilationException])(foldfun)
    compile(compiledDefinitions, main)
  }

  class findDefinition(name: String) extends Poly2 {
    implicit def st0 = at[Option[CompiledDefinition[_]], CompiledDefinition[_]] { (maybe, next) =>
      maybe.orElse(if (next.name == name) Some(next) else None)
    }
  }

  def compileStep(definitions: List[CompiledDefinition[_]], filter: QQFilterComponent[CompiledFilter]): OrCompilationError[CompiledFilter] = filter match {
    case IdFilter() => ((jsv: AnyTy) => Task.now(jsv :: Nil)).right
    case ComposeFilters(f, s) => composeCompiledFilters(f, s).right
    case EnlistFilter(f) => enlistFilter(f).right
    case SilenceExceptions(f) => ((jsv: AnyTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right
    case CollectResults(f) => collectResults(f).right
    case EnsequenceFilters(filters) => ensequenceCompiledFilters(filters).right
    case EnjectFilters(obj) => enjectFilter(obj).right
    case SelectKey(k) => selectKey(k).right
    case SelectRange(s, e) => selectRange(s, e).right
    case SelectIndex(i) => selectIndex(i).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition[_]) =>
          params.sized[defn.Aux](defn.numParams).cata({
            verifiedParams =>
              defn.body(verifiedParams)
          },
            WrongNumParams(filterIdentifier, defn.numParams(), params.length).left
          )
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compile(definitions: List[CompiledDefinition[_]], filter: QQFilter): OrCompilationError[CompiledFilter] = {
    filter.cataM[OrCompilationError, CompiledFilter](compileStep(prelude.all ++ definitions, _))
  }

  def prelude: QQPrelude
  def enjectFilter(obj: List[(\/[String, CompiledFilter], CompiledFilter)]): CompiledFilter
  def enlistFilter(filter: CompiledFilter): CompiledFilter
  def selectKey(key: String): CompiledFilter
  def selectIndex(index: Int): CompiledFilter
  def selectRange(start: Int, end: Int): CompiledFilter
  def collectResults(f: CompiledFilter): CompiledFilter
}

object QQCompiler {

  class QQRuntimeException(message: String) extends RuntimeException(message)
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String)
    extends QQCompilationException(message = s"No such method: $name")
  case class WrongNumParams(name: String, correct: Int, you: Int)
    extends QQCompilationException(message = s"Wrong number of params for filter $name: passed $you, wanted $correct")

}
