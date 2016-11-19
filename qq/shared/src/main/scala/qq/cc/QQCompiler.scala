package qq
package cc

import cats.implicits._
import org.atnos.eff.Eff._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import qq.data._
import qq.util.Recursion.RecursionEngine
import qq.util._

import scala.language.higherKinds

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

  @inline final def evaluatePath(components: List[PathComponent], operation: PathOperationF[CompiledFilter]): CompiledFilter = operation match {
    case PathGet =>
      components
        .map(QQRuntime.makePathComponentGetter[CompiledFilterStack])
        .nelFoldLeft1(CompiledFilter.id)(CompiledFilter.composeFilters)
    case PathSet(set) =>
      CompiledFilter.singleton { j =>
        set(j).flatMap {
          _.traverseA {
            QQRuntime.setPath[CompiledFilterStack](components, j, _)
          }.map(_.flatten)
        }
      }
    case PathModify(modify) =>
      components
        .map(QQRuntime.modifyPath[CompiledFilterStack])
        .nelFoldLeft1(identity[CompiledFilter])((f, s) => (i: CompiledFilter) => f(s(i)))(modify)
  }

  final def compileStep(definitions: Map[String, CompiledDefinition],
                        filter: FilterComponent[CompiledFilter]): QQCompilationException Either CompiledFilter = filter match {
    case Dereference(name) =>
      Right(
        CompiledFilter.singleton {
          (_: JSON) =>
            for {
              bindings <- reader.ask[CompiledFilterStack, VarBindings]
              result <- bindings.get(name).fold(noSuchVariable[CompiledFilterStack, JSON](name))(_.value.pureEff[CompiledFilterStack])
            } yield result :: Nil
        }
      )
    case ConstNumber(num) => Right(QQRuntime.constNumber(num))
    case ConstString(str) => Right(QQRuntime.constString(str))
    case ConstBoolean(bool) => Right(QQRuntime.constBoolean(bool))
    case FilterNot() => Right(CompiledFilter.singleton { j =>
      Eff.send[OrRuntimeErr, CompiledFilterStack, List[JSON]](QQRuntime.not(j).map(_ :: Nil))
    })
    case PathOperation(components, operationF) =>
      type mem = Member.Aux[VarEnv, CompiledFilterStack, CompiledProgramStack]
      Right(
          evaluatePath(components, operationF)
      )
    case ComposeFilters(f, s) => Right(CompiledFilter.composeFilters(f, s))
    case CallFilter(filterIdentifier, params) =>
      definitions.get(filterIdentifier).fold(Either.left[QQCompilationException, CompiledFilter](NoSuchMethod(filterIdentifier): QQCompilationException)) { (defn: CompiledDefinition) =>
        if (params.length == defn.numParams)
          defn.body(params)
        else
          Either.left[QQCompilationException, CompiledFilter](
            WrongNumParams(filterIdentifier, defn.numParams, params.length)
          )
      }
    case AsBinding(name, as, in) => Right(CompiledFilter.asBinding(name, as, in))
    case EnlistFilter(f) => Right(QQRuntime.enlistFilter(f))
    case SilenceExceptions(f) => Right(
      CompiledFilter.singleton { (jsv: JSON) =>
        either.catchLeft(f(jsv))((_: RuntimeErrs) => (Nil: List[JSON]).pureEff)
      }
    )
    case EnsequenceFilters(first, second) => Right(CompiledFilter.ensequenceCompiledFilters(first, second))
    case EnjectFilters(obj) => Right(QQRuntime.enjectFilter(obj))
    case FilterMath(first, second, op) =>
      Right(
        CompiledFilter.singleton { j =>
          (first(j) |@| second(j)).map((v1, v2) =>
            (for {v1a <- v1; v2a <- v2} yield Eff.send[OrRuntimeErr, CompiledFilterStack, JSON](funFromMathOperator(op)(v1a, v2a))).traverseA(identity)
          ).flatten
        }
      )
  }

  final def compileDefinitionStep(soFar: OrCompilationError[Vector[CompiledDefinition]],
                                  nextDefinition: Definition[FilterAST])
                                 (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
    soFar.map { (definitionsSoFar: Vector[CompiledDefinition]) =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter]) => {
        val paramsAsDefinitions: Vector[CompiledDefinition] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: List[CompiledFilter]) => Right(value))
        }(collection.breakOut)
        compileFilter(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline final def compileFilter(definitions: Vector[CompiledDefinition],
                                  filter: FilterAST)
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
                               definitions: Program.Definitions[FilterAST])
                              (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] = {
    definitions.foldLeft(prelude.all)(compileDefinitionStep)
  }

  final def compileProgram(prelude: Prelude,
                           program: Program[FilterAST])
                          (implicit rec: RecursionEngine): OrCompilationError[CompiledFilter] =
    for {
      compiledDefinitions <- compileDefinitions(prelude, program.defns)
      compiledMain <- compileFilter(compiledDefinitions, program.main)
    } yield compiledMain

}

