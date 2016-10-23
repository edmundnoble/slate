package qq
package cc

import cats.data.{NonEmptyList, Validated, ValidatedNel, Xor}
import monix.eval.Task
import monix.cats._
import qq.data._
import qq.util.Recursion.RecursionEngine
import qq.util._
import org.atnos.eff._
import Eff._
import syntax.all._

import scala.language.higherKinds
import cats.implicits._

object QQCompiler {

  import QQRuntimeException._

  final def funFromMathOperator(op: MathOperator): CompiledMathOperator = op match {
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
    case PathSet(set) =>
      CompiledProgram.singleton { j =>
        set(j).flatMap {
          _.traverseA {
            QQRuntime.setPath(components, j, _).into[CompiledProgramStack]
          }.map(_.flatten)
        }
      }
    case PathModify(modify) =>
      components
        .map(QQRuntime.modifyPath)
        .nelFoldLeft1(identity[CompiledProgram])((f, s) => (i: CompiledProgram) => f(s(i)))(modify)
  }

  final def compileStep(definitions: Map[String, CompiledDefinition],
                        filter: FilterComponent[CompiledFilter]): QQCompilationException Xor CompiledFilter = filter match {
    case Dereference(name) =>
      CompiledFilter.singleton {
        (_: JSON) =>
          for {
            bindings <- reader.ask[CompiledFilterStack, VarBindings]
            result <- bindings.get(name).fold(noSuchVariable[CompiledFilterStack, JSON](name))(_.value.pureEff[CompiledFilterStack])
          } yield result :: Nil
      }.right
    case ConstNumber(num) => QQRuntime.constNumber(num).right
    case ConstString(str) => QQRuntime.constString(str).right
    case ConstBoolean(bool) => QQRuntime.constBoolean(bool).right
    case FilterNot() => CompiledFilter.singleton { j =>
      Eff.send[OrRuntimeErr, CompiledFilterStack, List[JSON]](QQRuntime.not(j).map(_ :: Nil))
    }.right
    case PathOperation(components, operationF) =>
      type mem = Member.Aux[VarEnv, CompiledFilterStack, CompiledProgramStack]
      CompiledFilter.singleton {
        (j: JSON) =>
          evaluatePath(components,
            operationF.map(f =>
              CompiledProgram.singleton(j =>
                reader.runReader[CompiledFilterStack, CompiledProgramStack, VarBindings, List[JSON]](Map.empty)(f(j))(implicitly[mem])
              )
            )
          )(j).into[CompiledFilterStack]
      }.right
    case ComposeFilters(f, s) => CompiledFilter.composeFilters(f, s).right
    case CallFilter(filterIdentifier, params) =>
      definitions.get(filterIdentifier).fold((NoSuchMethod(filterIdentifier): QQCompilationException).left[CompiledFilter]) { (defn: CompiledDefinition) =>
        if (params.length == defn.numParams)
          defn.body(params)
        else
          WrongNumParams(filterIdentifier, defn.numParams, params.length).left
      }
    case AsBinding(name, as, in) => CompiledFilter.asBinding(name, as, in).right
    case EnlistFilter(f) => QQRuntime.enlistFilter(f).right
    case SilenceExceptions(f) => CompiledFilter.singleton { (jsv: JSON) =>
      val errExposed: Eff[CompiledFilterStack, OrRuntimeErr[List[JSON]]] =
        validated.by[NonEmptyList[QQRuntimeError]].runErrorParallel(f(jsv)).into[CompiledFilterStack]
      val recovered: Eff[CompiledFilterStack, OrRuntimeErr[List[JSON]]] =
        errExposed.map(_.orElse(Nil.valid[NonEmptyList[QQRuntimeError]]))

      Eff.collapse[CompiledFilterStack, OrRuntimeErr, List[JSON]](recovered)
    }.right[QQCompilationException]
    case EnsequenceFilters(first, second) => CompiledFilter.ensequenceCompiledFilters(first, second).right
    case EnjectFilters(obj) => QQRuntime.enjectFilter(obj).right
    case FilterMath(first, second, op) =>
      CompiledFilter.singleton { j =>
        (first(j) |@| second(j)).map((v1, v2) =>
          (for {v1a <- v1; v2a <- v2} yield Eff.send[OrRuntimeErr, CompiledFilterStack, JSON](funFromMathOperator(op)(v1a, v2a))).traverseA(identity)
        ).flatten
      }.right
  }

  final def compileDefinitionStep(soFar: OrCompilationError[Vector[CompiledDefinition]],
                                  nextDefinition: Definition[ConcreteFilter])
                                 (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
    soFar.map { (definitionsSoFar: Vector[CompiledDefinition]) =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter]) => {
        val paramsAsDefinitions: Vector[CompiledDefinition] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: List[CompiledFilter]) => value.right[QQCompilationException])
        }(collection.breakOut)
        compileFilter(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline final def compileFilter(definitions: Vector[CompiledDefinition],
                                  filter: ConcreteFilter)
                                 (implicit rec: RecursionEngine): OrCompilationError[CompiledFilter] =
    for {
      builtinDefinitions <- SharedPreludes.all |+| JSONPrelude.all
      allDefinitions = builtinDefinitions ++ definitions
      definitionMap = allDefinitions
        .map[(String, CompiledDefinition), Map[String, CompiledDefinition]](d => d.name -> d)(collection.breakOut)
      compileProgram = Recursion.cataM[FilterComponent, OrCompilationError, CompiledFilter](compileStep(definitionMap, _))
      compiledProgram <- compileProgram(filter)
    } yield compiledProgram

  final def compileDefinitions(prelude: Prelude,
                               definitions: Program.Definitions[ConcreteFilter])
                              (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] = {
    definitions.foldLeft(prelude.all)(compileDefinitionStep)
  }

  final def compileProgram(prelude: Prelude,
                           program: Program[ConcreteFilter])
                          (implicit rec: RecursionEngine): OrCompilationError[CompiledFilter] =
    for {
      compiledDefinitions <- compileDefinitions(prelude, program.defns)
      compiledMain <- compileFilter(compiledDefinitions, program.main)
    } yield compiledMain

}

