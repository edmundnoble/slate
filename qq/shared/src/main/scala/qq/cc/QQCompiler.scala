package qq.cc

import matryoshka.{Corecursive, Recursive}
import monix.eval.Task
import monix.scalaz._
import qq.cc.QQRuntime
import qq.data.FilterComponent._
import qq.data._
import qq.util._

import scala.language.higherKinds
import scalaz.Tags.Parallel
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.{PlusEmpty, Reader, \/}

object QQCompiler {

  @inline
  def compileDefinitions[T[_[_]] : Recursive : Corecursive, J](runtime: QQRuntime[J],
                                                               prelude: IndexedSeq[CompiledDefinition[J]] = Vector.empty,
                                                               definitions: Program.Definitions[T[FilterComponent]]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
    definitions.foldLeft(prelude.right[QQCompilationException])(compileDefinitionStep[T, J](runtime))

  @inline
  def compileProgram[T[_[_]] : Recursive : Corecursive, J](runtime: QQRuntime[J],
                                                           prelude: IndexedSeq[CompiledDefinition[J]] = Vector.empty,
                                                           program: Program[T[FilterComponent]]): OrCompilationError[CompiledFilter[J]] = {
    compileDefinitions(runtime, prelude, program.defns).flatMap(compile(runtime, _, program.main))
  }

  @inline
  def compileStep[J](runtime: QQRuntime[J],
                     definitions: IndexedSeq[CompiledDefinition[J]],
                     filter: FilterComponent[CompiledFilter[J]]): OrCompilationError[CompiledFilter[J]] = filter match {
    case leaf: LeafComponent[J@unchecked] => runtime.evaluateLeaf(leaf).right
    case ComposeFilters(f, s) => CompiledFilter.composeFilters(f, s).right
    case LetAsBinding(name, as, in) => CompiledFilter.letBinding(name, as, in).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => (for {
      fFun <- Reader(f)
    } yield (jsv: J) => fFun(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).run.right[QQCompilationException]
    case CollectResults(f) => runtime.collectResults(f).right
    case EnsequenceFilters(first, second) => CompiledFilter.ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => runtime.enjectFilter(obj).right
    case AddFilters(first, second) => CompiledFilter.zipFiltersWith(first, second, runtime.addJsValues).right
    case SubtractFilters(first, second) => CompiledFilter.zipFiltersWith(first, second, runtime.subtractJsValues).right
    case MultiplyFilters(first, second) => CompiledFilter.zipFiltersWith(first, second, runtime.multiplyJsValues).right
    case DivideFilters(first, second) => CompiledFilter.zipFiltersWith(first, second, runtime.divideJsValues).right
    case ModuloFilters(first, second) => CompiledFilter.zipFiltersWith(first, second, runtime.moduloJsValues).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition[J]) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep[T[_[_]] : Recursive : Corecursive, J](runtime: QQRuntime[J])
                                                                 (soFar: OrCompilationError[IndexedSeq[CompiledDefinition[J]]],
                                                                  nextDefinition: Definition[T[FilterComponent]]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
    soFar.map { (definitionsSoFar: IndexedSeq[CompiledDefinition[J]]) =>
      CompiledDefinition[J](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[J]]) => {
        val paramsAsDefinitions: IndexedSeq[CompiledDefinition[J]] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[J](filterName, 0, (_: List[CompiledFilter[J]]) => value.right[QQCompilationException])
        }(collection.breakOut)
        compile(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline
  def compile[T[_[_]] : Recursive : Corecursive, J](runtime: QQRuntime[J],
                                                    definitions: IndexedSeq[CompiledDefinition[J]],
                                                    filter: T[FilterComponent]): OrCompilationError[CompiledFilter[J]] =
    for {
      sharedDefinitions <- SharedPreludes[J].all(runtime)
      platformSpecificDefinitions <- runtime.platformPrelude.all(runtime)
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <-
      Recursion.cataM[T, FilterComponent, OrCompilationError, CompiledFilter[J]](
        compileStep(runtime, allDefinitions, _)
      ).apply(filter)
    } yield compiledProgram

}

