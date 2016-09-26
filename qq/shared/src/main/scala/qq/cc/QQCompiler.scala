package qq
package cc

import matryoshka.Recursive
import qq.data._
import qq.util._

import scala.language.higherKinds
import scalaz.syntax.either._
import scalaz.syntax.functor._
import scalaz.syntax.std.option._
import scalaz.Reader
import qq.Platform.Rec._
import scalaz.syntax.plusEmpty._

object QQCompiler {

  def compileDefinitions[T[_[_]] : Recursive, J](runtime: QQRuntime[J],
                                                 prelude: Option[Prelude[J]] = None,
                                                 definitions: Program.Definitions[T[FilterComponent]]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] = {
    val preludeDefinitions = prelude.fold(IndexedSeq.empty[CompiledDefinition[J]].right[QQCompilationException])(_.all(runtime))
    definitions.foldLeft(preludeDefinitions)(compileDefinitionStep[T, J](runtime))
  }

  def compileProgram[T[_[_]] : Recursive, J](runtime: QQRuntime[J],
                                             prelude: Option[Prelude[J]] = None,
                                             program: Program[T[FilterComponent]]): OrCompilationError[CompiledFilter[J]] = {
    compileDefinitions(runtime, prelude, program.defns).flatMap(compileFilter(runtime, _, program.main))
  }

  def compileStep[J](runtime: QQRuntime[J],
                     definitions: IndexedSeq[CompiledDefinition[J]],
                     filter: FilterComponent[CompiledFilter[J]]): OrCompilationError[CompiledFilter[J]] = filter match {
    case leaf: LeafComponent[J@unchecked] => runtime.evaluateLeaf(leaf).right
    case PathOperation(components, operationF) => ((_: VarBindings[J]) => runtime.evaluatePath(components, operationF.map(_(Map.empty)))).right
    case ComposeFilters(f, s) => CompiledFilter.composeFilters(f, s).right
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
    case LetAsBinding(name, as, in) => CompiledFilter.letBinding(name, as, in).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => (for {
      fFun <- Reader(f)
    } yield (jsv: J) => fFun(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).run.right[QQCompilationException]
    case EnsequenceFilters(first, second) => CompiledFilter.ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => runtime.enjectFilter(obj).right
    case FilterMath(first, second, Add) => CompiledFilter.zipFiltersWith(first, second, runtime.addJsValues).right
    case FilterMath(first, second, Subtract) => CompiledFilter.zipFiltersWith(first, second, runtime.subtractJsValues).right
    case FilterMath(first, second, Multiply) => CompiledFilter.zipFiltersWith(first, second, runtime.multiplyJsValues).right
    case FilterMath(first, second, Divide) => CompiledFilter.zipFiltersWith(first, second, runtime.divideJsValues).right
    case FilterMath(first, second, Modulo) => CompiledFilter.zipFiltersWith(first, second, runtime.moduloJsValues).right
  }

  def compileDefinitionStep[T[_[_]] : Recursive, J](runtime: QQRuntime[J])
                                                   (soFar: OrCompilationError[IndexedSeq[CompiledDefinition[J]]],
                                                    nextDefinition: Definition[T[FilterComponent]]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
    soFar.map { (definitionsSoFar: IndexedSeq[CompiledDefinition[J]]) =>
      CompiledDefinition[J](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[J]]) => {
        val paramsAsDefinitions: IndexedSeq[CompiledDefinition[J]] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[J](filterName, 0, (_: List[CompiledFilter[J]]) => value.right[QQCompilationException])
        }(collection.breakOut)
        compileFilter(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  def compileFilter[T[_[_]] : Recursive, J](runtime: QQRuntime[J],
                                            definitions: IndexedSeq[CompiledDefinition[J]],
                                            filter: T[FilterComponent]): OrCompilationError[CompiledFilter[J]] =
    for {
      builtinDefinitions <- (SharedPreludes[J] <+> runtime.platformPrelude).all(runtime)
      allDefinitions = builtinDefinitions ++ definitions
      compiledProgram <-
      Recursion.cataM[T, FilterComponent, OrCompilationError, CompiledFilter[J]](
        compileStep(runtime, allDefinitions, _)
      ).apply(filter)
    } yield compiledProgram

}

