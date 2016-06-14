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
import shapeless.ops.nat.ToInt
import shapeless.syntax.sized._

import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

abstract class QQCompiler {

  type AnyTy
  type CompiledFilter = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

  case class CompiledDefinition[N <: Nat]
  (name: String, body: Sized[List[CompiledFilter], N] => QQCompilationException \/ CompiledFilter)
  (implicit val ev: ToInt[N with Nat])

  trait QQPrelude {
    def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition[_0] = {
      CompiledDefinition[_0](name, (_: Sized[List[CompiledFilter], _0]) => fun.right)
    }

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

  def foldfun(soFar: QQCompilationException \/ List[CompiledDefinition[_]], nextDefinition: Definition[_]) = {
    soFar.map(definitionsSoFar =>
      CompiledDefinition(nextDefinition.name, (params: Sized[List[CompiledFilter], Nat]) => {
        val paramsAsDefinitions = (nextDefinition.params.unsized, params.unsized).zipped.map((name, value) => CompiledDefinition(name, (_: Sized[List[CompiledFilter], _0]) => value.right[QQCompilationException]))
        compile(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      })(nextDefinition.ev.asInstanceOf[ToInt[Nat]]) :: definitionsSoFar
    )
  }

  def compileProgram(definitions: List[Definition[_]], main: QQFilter): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: QQCompilationException \/ List[CompiledDefinition[_]] =
      definitions.foldLeft(nil[CompiledDefinition[_]].right[QQCompilationException])(foldfun)
    compiledDefinitions.flatMap(compile(_, main))
  }

  def compileStep
  (definitions: List[CompiledDefinition[_]], filter: QQFilterComponent[CompiledFilter]
  ): OrCompilationError[CompiledFilter] = filter match {
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
          params.sized(defn.ev).cata({
            verifiedParams =>
              val d: (Sized[List[CompiledFilter], Nat] => QQCompilationException \/ CompiledFilter)  = defn.body.asInstanceOf[(Function[Sized[List[CompiledFilter], Nat], \/[QQCompilationException, CompiledFilter]])]
              d(verifiedParams.asInstanceOf[Sized[List[CompiledFilter], Nat]])
          },
            WrongNumParams(filterIdentifier, defn.ev(), params.length).left
          )
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compile
  (definitions: List[CompiledDefinition[_]], filter: QQFilter
  ): OrCompilationError[CompiledFilter] = {
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
