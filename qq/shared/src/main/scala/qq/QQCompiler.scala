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

object QQCompiler {

  case class QQRuntimeException(message: String) extends RuntimeException(message)
  case class NotARegex(asStr: String) extends RuntimeException(s"tried to use this as a regex: $asStr")
  class QQCompilationException(message: String) extends RuntimeException(message)
  case class NoSuchMethod(name: String)
    extends QQCompilationException(message = s"No such method: $name")
  case class WrongNumParams(name: String, correct: Int, you: Int)
    extends QQCompilationException(message = s"Wrong number of params for filter $name: passed $you, wanted $correct")

  type CompiledFilter[AnyTy] = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

  @inline
  private def composeCompiledFilters[AnyTy](firstFilter: CompiledFilter[AnyTy], secondFilter: CompiledFilter[AnyTy]): CompiledFilter[AnyTy] = { jsv: AnyTy =>
    for {
      firstResult <- firstFilter(jsv)
      secondResult <- firstResult.traverse(secondFilter)
    } yield secondResult.flatten
  }

  @inline
  private def ensequenceCompiledFilters[AnyTy](first: CompiledFilter[AnyTy], second: CompiledFilter[AnyTy]): CompiledFilter[AnyTy] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (a, b) => a ++ b }
  }

  @inline
  private def zipFiltersWith[AnyTy](first: CompiledFilter[AnyTy], second: CompiledFilter[AnyTy], fun: (AnyTy, AnyTy) => Task[AnyTy]): CompiledFilter[AnyTy] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten
  }

  final def compileDefinitions[AnyTy](runtime: QQRuntime[AnyTy], definitions: List[Definition]): QQCompilationException \/ List[CompiledDefinition[AnyTy]] =
    definitions.foldLeft(nil[CompiledDefinition[AnyTy]].right[QQCompilationException])(compileDefinitionStep(runtime))

  final def compileProgram[AnyTy](runtime: QQRuntime[AnyTy], definitions: List[Definition], main: Filter): QQCompilationException \/ CompiledFilter[AnyTy] = {
    compileDefinitions(runtime, definitions).flatMap(compile(runtime, _, main))
  }

  @inline
  final def compileStep[AnyTy](runtime: QQRuntime[AnyTy], definitions: List[CompiledDefinition[AnyTy]], filter: FilterComponent[CompiledFilter[AnyTy]]): OrCompilationError[CompiledFilter[AnyTy]] = filter match {
    case IdFilter() => ((jsv: AnyTy) => Task.now(jsv :: Nil)).right
    case ComposeFilters(f, s) => composeCompiledFilters(f, s).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => ((jsv: AnyTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right
    case CollectResults(f) => runtime.collectResults(f).right
    case EnsequenceFilters(first, second) => ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => runtime.enjectFilter(obj).right
    case SelectKey(k) => runtime.selectKey(k).right
    case SelectRange(s, e) => runtime.selectRange(s, e).right
    case SelectIndex(i) => runtime.selectIndex(i).right
    case ConstNumber(d) => runtime.constNumber(d).right
    case ConstString(str) => runtime.constString(str).right
    case AddFilters(first, second) => zipFiltersWith(first, second, runtime.addJsValues).right
    case SubtractFilters(first, second) => zipFiltersWith(first, second, runtime.subtractJsValues).right
    case MultiplyFilters(first, second) => zipFiltersWith(first, second, runtime.multiplyJsValues).right
    case DivideFilters(first, second) => zipFiltersWith(first, second, runtime.divideJsValues).right
    case ModuloFilters(first, second) => zipFiltersWith(first, second, runtime.moduloJsValues).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition[AnyTy]) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left[CompiledFilter[AnyTy]]
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep[AnyTy](runtime: QQRuntime[AnyTy])
                                  (soFar: QQCompilationException \/ List[CompiledDefinition[AnyTy]], nextDefinition: Definition): QQCompilationException \/ List[CompiledDefinition[AnyTy]] = {
    soFar.map((definitionsSoFar: List[CompiledDefinition[AnyTy]]) => {
      CompiledDefinition[AnyTy](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[AnyTy]]) => {
        val paramsAsDefinitions = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[AnyTy](filterName, 0, (_: List[CompiledFilter[AnyTy]]) => value.right[QQCompilationException])
        }
        compile(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) :: definitionsSoFar
    })
  }

  final def compile[AnyTy](runtime: QQRuntime[AnyTy], definitions: List[CompiledDefinition[AnyTy]], filter: Filter): OrCompilationError[CompiledFilter[AnyTy]] =
    for {
      sharedDefinitions <- SharedPreludes[AnyTy].all(runtime)
      platformSpecificDefinitions <- runtime.platformPrelude.all(runtime)
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <- filter.cataM[OrCompilationError, CompiledFilter[AnyTy]](compileStep(runtime, allDefinitions, _))
    } yield compiledProgram

}

