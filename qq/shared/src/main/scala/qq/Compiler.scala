package qq

import matryoshka.Recursive.ops._
import monix.eval.Task
import qq.FilterComponent._
import qq.Util._

import scala.language.existentials
import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

abstract class Compiler {

  import Compiler._

  type AnyTy

  @inline
  private def composeCompiledFilters(firstFilter: CompiledFilter[this.type], secondFilter: CompiledFilter[this.type]): CompiledFilter[this.type] = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  @inline
  private def ensequenceCompiledFilters(first: CompiledFilter[this.type], second: CompiledFilter[this.type]): CompiledFilter[this.type] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (a, b) => a ++ b }
  }

  @inline
  private def zipFiltersWith(first: CompiledFilter[this.type], second: CompiledFilter[this.type], fun: (AnyTy, AnyTy) => Task[AnyTy]): CompiledFilter[this.type] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten
  }

  final def compileDefinitions(definitions: List[Definition]): QQCompilationException \/ List[CompiledDefinition[this.type]] =
    definitions.foldLeft(nil[CompiledDefinition[this.type]].right[QQCompilationException])(compileDefinitionStep)

  final def compileProgram(definitions: List[Definition], main: Filter): QQCompilationException \/ CompiledFilter[this.type] = {
    compileDefinitions(definitions).flatMap(compile(_, main))
  }

  @inline
  final def compileStep(definitions: List[CompiledDefinition[this.type]], filter: FilterComponent[CompiledFilter[this.type]]): OrCompilationError[CompiledFilter[this.type]] = filter match {
    case IdFilter() => ((jsv: AnyTy) => Task.now(jsv :: Nil)).right
    case ComposeFilters(f, s) => composeCompiledFilters(f, s).right
    case EnlistFilter(f) => enlistFilter(f).right
    case SilenceExceptions(f) => ((jsv: AnyTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right
    case CollectResults(f) => collectResults(f).right
    case EnsequenceFilters(first, second) => ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => enjectFilter(obj).right
    case SelectKey(k) => selectKey(k).right
    case SelectRange(s, e) => selectRange(s, e).right
    case SelectIndex(i) => selectIndex(i).right
    case ConstNumber(d) => constNumber(d).right
    case ConstString(str) => constString(str).right
    case AddFilters(first, second) => zipFiltersWith(first, second, addJsValues).right
    case SubtractFilters(first, second) => zipFiltersWith(first, second, subtractJsValues).right
    case MultiplyFilters(first, second) => zipFiltersWith(first, second, multiplyJsValues).right
    case DivideFilters(first, second) => zipFiltersWith(first, second, divideJsValues).right
    case ModuloFilters(first, second) => zipFiltersWith(first, second, moduloJsValues).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition[this.type]) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left[CompiledFilter[this.type]]
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep(soFar: QQCompilationException \/ List[CompiledDefinition[this.type]], nextDefinition: Definition): QQCompilationException \/ List[CompiledDefinition[Compiler.this.type]] = {
    soFar.map((definitionsSoFar: List[CompiledDefinition[this.type]]) => {
      CompiledDefinition[this.type](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[this.type]]) => {
        val paramsAsDefinitions = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[this.type](filterName, 0, (_: List[CompiledFilter[this.type]]) => value.right[QQCompilationException])
        }
        compile(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) :: definitionsSoFar
    })
  }

  final def compile(definitions: List[CompiledDefinition[this.type]], filter: Filter): OrCompilationError[CompiledFilter[this.type]] =
    for {
      sharedDefinitions <- SharedPreludes.apply[this.type].all(this: this.type)
      platformSpecificDefinitions <- platformPrelude.all(this: this.type)
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <- filter.cataM[OrCompilationError, CompiledFilter[this.type]](compileStep(allDefinitions, _))
    } yield compiledProgram

  def platformPrelude: PlatformPrelude[this.type]

  def enjectFilter(obj: List[(\/[String, CompiledFilter[this.type]], CompiledFilter[this.type])]): CompiledFilter[this.type]
  def enlistFilter(filter: CompiledFilter[this.type]): CompiledFilter[this.type]
  def selectKey(key: String): CompiledFilter[this.type]
  def selectIndex(index: Int): CompiledFilter[this.type]
  def selectRange(start: Int, end: Int): CompiledFilter[this.type]
  def constNumber(num: Double): CompiledFilter[this.type]
  def constString(str: String): CompiledFilter[this.type]
  def addJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def subtractJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def multiplyJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def divideJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def moduloJsValues(first: AnyTy, second: AnyTy): Task[AnyTy]
  def collectResults(f: CompiledFilter[this.type]): CompiledFilter[this.type]

}

object Compiler {

  case class QQRuntimeException(message: String) extends RuntimeException(message)
  case class NotARegex(asStr: String) extends RuntimeException(s"tried to use this as a regex: $asStr")
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String)
    extends QQCompilationException(message = s"No such method: $name")
  case class WrongNumParams(name: String, correct: Int, you: Int)
    extends QQCompilationException(message = s"Wrong number of params for filter $name: passed $you, wanted $correct")

  type CompiledFilter[C <: Compiler with Singleton] = C#AnyTy => Task[List[C#AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

}
