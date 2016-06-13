package qq

import qq.Util._
import qq.QQCompiler.{NoSuchMethod, QQCompilationException, QQRuntimeException}
import qq.Definition
import qq.QQFilterComponent._

import monix.eval.{Coeval, Task}
import scalaz.std.list._
import scalaz.{EitherT, \/}
import matryoshka._
import Recursive.ops._
import Corecursive.ops._
import TraverseT.ops._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.monad._

abstract class QQCompiler {

  type AnyTy
  type CompiledFilter = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

  trait QQPrelude {
    def length: CompiledDefinition
    def keys: CompiledDefinition
    def all: List[CompiledDefinition] = List(length, keys )
  }

  case class CompiledDefinition(name: String, params: List[String], body: CompiledFilter)

  def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: AnyTy =>
    Task.sequence(functions.map(_ (jsv))).map(_.flatten)
  }

  def compileProgram(definitions: List[Definition], main: QQFilter): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: QQCompilationException \/ List[CompiledDefinition] =
      definitions.foldLeft(nil[CompiledDefinition].right[QQCompilationException]) {
        (soFar, nextDefinition) =>
          soFar.flatMap(definitionsSoFar =>
            compile(definitionsSoFar, nextDefinition.body)
              .map(CompiledDefinition(nextDefinition.name, nextDefinition.params, _) :: definitionsSoFar))
      }
    compiledDefinitions.flatMap(compile(_, main))
  }

  def compileStep(definitions: List[CompiledDefinition], filter: QQFilterComponent[CompiledFilter]): OrCompilationError[CompiledFilter] = filter match {
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
    case CallFilter(filterIdentifier) =>
      definitions.find(_.name == filterIdentifier).cata(_.body.right, NoSuchMethod(filterIdentifier).left)
  }

  def compile(definitions: List[CompiledDefinition], filter: QQFilter): OrCompilationError[CompiledFilter] = {
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
  case class NoSuchMethod(name: String) extends QQCompilationException(message = s"No such method: $name")

}
