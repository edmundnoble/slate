package qq
package cc

import matryoshka.Recursive
import monix.eval.Task
import qq.data._
import qq.util.Recursion.RecursionEngine
import qq.util._

import scala.language.higherKinds
import scalaz.syntax.either._
import scalaz.syntax.functor._
import scalaz.syntax.std.option._
import scalaz.syntax.plusEmpty._

object QQCompiler {

  def compileDefinitions[T[_[_]] : Recursive, J](runtime: QQRuntime[J],
                                                 prelude: Prelude[J],
                                                 definitions: Program.Definitions[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] = {
    definitions.foldLeft(prelude.all(runtime))(compileDefinitionStep[T, J](runtime))
  }

  def compileProgram[T[_[_]] : Recursive, J](runtime: QQRuntime[J],
                                             prelude: Prelude[J],
                                             program: Program[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[CompiledFilter[J]] = {
    compileDefinitions(runtime, prelude, program.defns).flatMap(compileFilter(runtime, _, program.main))
  }

  def funFromMathOperator[J](runtime: QQRuntime[J], op: MathOperator): CompiledMathOperator[J] = op match {
    case Add => runtime.addJsValues
    case Subtract => runtime.subtractJsValues
    case Multiply => runtime.multiplyJsValues
    case Divide => runtime.divideJsValues
    case Modulo => runtime.moduloJsValues
    case Equal => (j1: J, j2: J) => Task.now(runtime.equalJsValues(j1, j2))
    case LTE => ???
    case GTE => ???
    case LessThan => ???
    case GreaterThan =>  ???
  }

  def compileStep[J](runtime: QQRuntime[J],
                     definitions: IndexedSeq[CompiledDefinition[J]],
                     filter: FilterComponent[CompiledFilter[J]]): OrCompilationError[CompiledFilter[J]] = filter match {
    case leaf: LeafComponent[J@unchecked] => runtime.evaluateLeaf(leaf).right
    case PathOperation(components, operationF) => ((_: VarBindings[J]) => runtime.evaluatePath(components, operationF.map(_ (Map.empty)))).right
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
    case AsBinding(name, as, in) => CompiledFilter.asBinding(name, as, in).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) =>
      ((varBindings: VarBindings[J]) =>
        (jsv: J) =>
          f(varBindings)(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right[QQCompilationException]
    case EnsequenceFilters(first, second) => CompiledFilter.ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => runtime.enjectFilter(obj).right
    case FilterMath(first, second, op) =>
      val operatorFunction = funFromMathOperator(runtime, op)
      CompiledFilter.zipFiltersWith(first, second, operatorFunction).right
  }

  def compileDefinitionStep[T[_[_]] : Recursive, J](runtime: QQRuntime[J])
                                                   (soFar: OrCompilationError[IndexedSeq[CompiledDefinition[J]]],
                                                    nextDefinition: Definition[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
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
                                            filter: T[FilterComponent])(implicit rec: RecursionEngine): OrCompilationError[CompiledFilter[J]] =
    for {
      builtinDefinitions <- (SharedPreludes[J] <+> runtime.platformPrelude).all(runtime)
      allDefinitions = builtinDefinitions ++ definitions
      compileProgram = Recursion.cataM[T, FilterComponent, OrCompilationError, CompiledFilter[J]](compileStep(runtime, allDefinitions, _))
      compiledProgram <- compileProgram(filter)
    } yield compiledProgram

}

