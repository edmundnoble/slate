package qq
package cc

import matryoshka.Recursive
import monix.eval.Task
import monix.scalaz._
import qq.data._
import qq.util.Recursion.RecursionEngine
import qq.util._

import scala.language.higherKinds
import scalaz.{NonEmptyList, ValidationNel, \/}
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.validation._
import scalaz.syntax.monoid._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._

object QQCompiler {

  def compileDefinitions[T[_[_]] : Recursive](prelude: Prelude,
                                              definitions: Program.Definitions[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] = {
    definitions.foldLeft(prelude.all)(compileDefinitionStep[T])
  }

  def compileProgram[T[_[_]] : Recursive](prelude: Prelude,
                                          program: Program[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[CompiledFilter] =
    compileDefinitions(prelude, program.defns).flatMap(compileFilter(_, program.main))

  def funFromMathOperator(op: MathOperator): CompiledMathOperator = op match {
    case Add => QQRuntime.addJsValues
    case Subtract => QQRuntime.subtractJsValues
    case Multiply => QQRuntime.multiplyJsValues
    case Divide => QQRuntime.divideJsValues
    case Modulo => QQRuntime.moduloJsValues
    case Equal => QQRuntime.equalJsValues
    case LTE => QQRuntime.lteJsValues
    case GTE => QQRuntime.gteJsValues
    case LessThan => QQRuntime.lessThanJsValues
    case GreaterThan => QQRuntime.greaterThanJsValues
  }

  @inline final def evaluatePath(components: List[PathComponent], operation: PathOperationF[CompiledProgram]): CompiledProgram = operation match {
    case PathGet =>
      components
        .map(QQRuntime.makePathComponentGetter)
        .nelFoldLeft1(CompiledProgram.id)(CompiledProgram.composePrograms)
    case PathSet(set) => (j: JSON) =>
      set(j).flatMap { a =>
        a.traverse(_.traverse {
          QQRuntime.setPath(components, j, _)
        })
      }.map { a =>
        val x = a.map(_.traverse[ValidationNel[QQRuntimeError, ?], List[JSON]](identity))
        val x2 = x.map(_.map(_.flatten))
        x2.flatten
      }
    case PathModify(modify) =>
      components
        .map(QQRuntime.modifyPath)
        .nelFoldLeft1(identity[CompiledProgram])((f, s) => (i: CompiledProgram) => f(s(i)))(modify)
  }

  def compileStep(definitions: IndexedSeq[CompiledDefinition],
                  filter: FilterComponent[CompiledFilter]): OrCompilationError[CompiledFilter] = filter match {
    case Dereference(name) =>
      ((bindings: VarBindings) => (_: JSON) =>
        Task.now(bindings.get(name).cata(
          p => (p.value :: Nil).success,
          NoSuchVariable(name).failure
        ).toValidationNel[QQRuntimeError, List[JSON]])).right
    case ConstNumber(num) => QQRuntime.constNumber(num).right
    case ConstString(str) => QQRuntime.constString(str).right
    case ConstBoolean(bool) => QQRuntime.constBoolean(bool).right
    case FilterNot() => CompiledFilter.func { j => Task.now(QQRuntime.not(j).map(_ :: Nil)) }.right
    case PathOperation(components, operationF) => ((_: VarBindings) => evaluatePath(components, operationF.map(_ (Map.empty)))).right
    case ComposeFilters(f, s) => CompiledFilter.composeFilters(f, s).right
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
    case AsBinding(name, as, in) => CompiledFilter.asBinding(name, as, in).right
    case EnlistFilter(f) => QQRuntime.enlistFilter(f).right
    case SilenceExceptions(f) =>
      ((varBindings: VarBindings) =>
        (jsv: JSON) =>
          f(varBindings)(jsv).map(_ ||| Nil.successNel[QQRuntimeError])).right
    case EnsequenceFilters(first, second) => CompiledFilter.ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => QQRuntime.enjectFilter(obj).right
    case FilterMath(first, second, op) =>
      val operatorFunction = funFromMathOperator(op)
      val converted = (j1: JSON, j2: JSON) => Task.now(operatorFunction(j1, j2))
      // this is incorrect behavior according to JQ and intuitively.
      // TODO: replace with standard effect distribution
      CompiledFilter.zipFiltersWith(first, second, converted).right
  }

  def compileDefinitionStep[T[_[_]] : Recursive](soFar: OrCompilationError[IndexedSeq[CompiledDefinition]],
                                                 nextDefinition: Definition[T[FilterComponent]])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] =
    soFar.map { (definitionsSoFar: IndexedSeq[CompiledDefinition]) =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter]) => {
        val paramsAsDefinitions: IndexedSeq[CompiledDefinition] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: List[CompiledFilter]) => value.right[QQCompilationException])
        }(collection.breakOut)
        compileFilter(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline final def compileFilter[T[_[_]] : Recursive](definitions: IndexedSeq[CompiledDefinition],
                                                       filter: T[FilterComponent])(implicit rec: RecursionEngine): OrCompilationError[CompiledFilter] =
    for {
      builtinDefinitions <- (SharedPreludes.all |+| JSONPrelude).all
      allDefinitions = builtinDefinitions ++ definitions
      compileProgram = Recursion.cataM[T, FilterComponent, OrCompilationError, CompiledFilter](compileStep(allDefinitions, _))
      compiledProgram <- compileProgram(filter)
    } yield compiledProgram

}

