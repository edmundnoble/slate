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
import scalaz.syntax.apply._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

abstract class QQCompiler {

  type AnyTy
  type CompiledFilter = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

  final case class CompiledDefinition
  (name: String, numParams: Int, body: List[CompiledFilter] => QQCompilationException \/ CompiledFilter)

  trait QQPrelude {
    def noParamDefinition(name: String, fun: CompiledFilter): CompiledDefinition = {
      CompiledDefinition(name, numParams = 0, (_: List[CompiledFilter]) => fun.right)
    }

    def length: CompiledDefinition

    def keys: CompiledDefinition

    def arrays: CompiledDefinition

    def objects: CompiledDefinition

    def iterables: CompiledDefinition

    def booleans: CompiledDefinition

    def numbers: CompiledDefinition

    def strings: CompiledDefinition

    def nulls: CompiledDefinition

    def values: CompiledDefinition

    def scalars: CompiledDefinition

    def all: List[CompiledDefinition] =
      length :: keys :: arrays :: objects :: iterables :: booleans ::
        numbers :: strings :: nulls :: values :: scalars :: Nil
  }

  @inline
  final def composeCompiledFilters(firstFilter: CompiledFilter, secondFilter: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  @inline
  final def ensequenceCompiledFilters(functions: List[CompiledFilter]): CompiledFilter = { jsv: AnyTy =>
    Task.sequence(functions.map(_ (jsv))).map(_.flatten)
  }

  final def compileProgram(definitions: List[Definition], main: QQFilter): QQCompilationException \/ CompiledFilter = {
    val compiledDefinitions: QQCompilationException \/ List[CompiledDefinition] =
      definitions.foldLeft(nil[CompiledDefinition].right[QQCompilationException])(compileDefinitionStep)
    compiledDefinitions.flatMap(compile(_, main))
  }

  final def compileStep(definitions: List[CompiledDefinition], filter: QQFilterComponent[CompiledFilter]): OrCompilationError[CompiledFilter] = filter match {
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
    case ConstNumber(d) => constNumber(d).right
    case ConstString(str) => constString(str).right
    case AddFilters(first, second) => addFilters(first, second).right
    case SubtractFilters(first, second) => subtractFilters(first, second).right
    case MultiplyFilters(first, second) => multiplyFilters(first, second).right
    case DivideFilters(first, second) => divideFilters(first, second).right
    case ModuloFilters(first, second) => moduloFilters(first, second).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left[CompiledFilter]
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep(soFar: QQCompilationException \/ List[CompiledDefinition], nextDefinition: Definition) = {
    soFar.map(definitionsSoFar =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter]) => {
        val paramsAsDefinitions = (params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(nextDefinition.name, 0, (_: List[CompiledFilter]) => value.right[QQCompilationException])
        }
        compile(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) :: definitionsSoFar
    )
  }

  final def compile(definitions: List[CompiledDefinition], filter: QQFilter): OrCompilationError[CompiledFilter] = {
    filter.cataM[OrCompilationError, CompiledFilter](compileStep(prelude.all ++ definitions, _))
  }

  def prelude: QQPrelude
  def enjectFilter(obj: List[(\/[String, CompiledFilter], CompiledFilter)]): CompiledFilter
  def enlistFilter(filter: CompiledFilter): CompiledFilter
  def selectKey(key: String): CompiledFilter
  def selectIndex(index: Int): CompiledFilter
  def selectRange(start: Int, end: Int): CompiledFilter
  def constNumber(num: Double): CompiledFilter
  def constString(str: String): CompiledFilter
  def addJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def subtractJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def multiplyJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def divideJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def moduloJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]

  def addFilters(first: CompiledFilter, second: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    (first(jsv) |@| second(jsv)) { (f, s) => (f, s).zipped.map(addJsValues) }.map(_.sequence).flatten
  }

  def subtractFilters(first: CompiledFilter, second: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    (first(jsv) |@| second(jsv)) { (f, s) => (f, s).zipped.map(subtractJsValues) }.map(_.sequence).flatten
  }

  def multiplyFilters(first: CompiledFilter, second: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    (first(jsv) |@| second(jsv)) { (f, s) => (f, s).zipped.map(multiplyJsValues) }.map(_.sequence).flatten
  }

  def divideFilters(first: CompiledFilter, second: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    (first(jsv) |@| second(jsv)) { (f, s) => (f, s).zipped.map(divideJsValues) }.map(_.sequence).flatten
  }

  def moduloFilters(first: CompiledFilter, second: CompiledFilter): CompiledFilter = { jsv: AnyTy =>
    (first(jsv) |@| second(jsv)) { (f, s) => (f, s).zipped.map(moduloJsValues) }.map(_.sequence).flatten
  }

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
