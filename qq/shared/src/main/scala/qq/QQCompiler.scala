package qq

import matryoshka.Fix
import monix.eval.Task
import monix.scalaz._
import qq.FilterComponent._

import scalaz.\/
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

case class QQRuntimeException(message: String) extends RuntimeException(message)
case class NotARegex(asStr: String) extends RuntimeException("tried to use this as a regex: " + asStr)
class QQCompilationException(message: String) extends RuntimeException(message)
case class NoSuchMethod(name: String)
  extends QQCompilationException(message = "No such method: " + name)
case class WrongNumParams(name: String, correct: Int, you: Int)
  extends QQCompilationException(message = "Wrong number of params for filter " + name.toString + ": passed $you, wanted " + correct)

object QQCompiler {

  type CompiledFilter[AnyTy] = AnyTy => Task[List[AnyTy]]
  type OrCompilationError[T] = QQCompilationException \/ T

  @inline
  def ensequenceCompiledFilters[AnyTy]
  (first: CompiledFilter[AnyTy], second: CompiledFilter[AnyTy]): CompiledFilter[AnyTy] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (a, b) => a ++ b }
  }

  @inline
  def zipFiltersWith[AnyTy]
  (first: CompiledFilter[AnyTy], second: CompiledFilter[AnyTy], fun: (AnyTy, AnyTy) => Task[AnyTy]): CompiledFilter[AnyTy] = { jsv: AnyTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten
  }

  @inline
  def compileDefinitions[AnyTy](runtime: QQRuntime[AnyTy],
                                definitions: List[Definition]): OrCompilationError[List[CompiledDefinition[AnyTy]]] =
    definitions.foldLeft(nil[CompiledDefinition[AnyTy]].right[QQCompilationException])(compileDefinitionStep(runtime))

  @inline
  def compileProgram[AnyTy](runtime: QQRuntime[AnyTy],
                            program: Program): OrCompilationError[CompiledFilter[AnyTy]] = {
    compileDefinitions(runtime, program.defns).flatMap(compile(runtime, _, program.main))
  }

  @inline
  def compileStep[AnyTy](runtime: QQRuntime[AnyTy],
                         definitions: List[CompiledDefinition[AnyTy]],
                         filter: FilterComponent[CompiledFilter[AnyTy]]): OrCompilationError[CompiledFilter[AnyTy]] = filter match {
    case leaf: LeafComponent[AnyTy@unchecked] => runtime.evaluateLeaf(leaf).right
    case ComposeFilters(f, s) => f.andThen(_.flatMap(_.traverseM[Task, AnyTy](s))).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => ((jsv: AnyTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right
    case CollectResults(f) => runtime.collectResults(f).right
    case EnsequenceFilters(first, second) => ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => runtime.enjectFilter(obj).right
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
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep[AnyTy](runtime: QQRuntime[AnyTy])
                                  (soFar: OrCompilationError[List[CompiledDefinition[AnyTy]]],
                                   nextDefinition: Definition): OrCompilationError[List[CompiledDefinition[AnyTy]]] =
    soFar.map((definitionsSoFar: List[CompiledDefinition[AnyTy]]) => {
      CompiledDefinition[AnyTy](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[AnyTy]]) => {
        val paramsAsDefinitions = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[AnyTy](filterName, 0, (_: List[CompiledFilter[AnyTy]]) => value.right[QQCompilationException])
        }
        compile(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) :: definitionsSoFar
    })

  @inline
  def compile[AnyTy](runtime: QQRuntime[AnyTy],
                     definitions: List[CompiledDefinition[AnyTy]],
                     filter: Filter): OrCompilationError[CompiledFilter[AnyTy]] =
    for {
      sharedDefinitions <- SharedPreludes[AnyTy].all(runtime)
      platformSpecificDefinitions <- runtime.platformPrelude.all(runtime)
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <-
      Recursion.cataM[Fix, FilterComponent, OrCompilationError, CompiledFilter[AnyTy]](compileStep(runtime, allDefinitions, _))
        .apply(Recursion.Unsafe.RecursionLimitStack(128), filter)
    } yield compiledProgram

}

