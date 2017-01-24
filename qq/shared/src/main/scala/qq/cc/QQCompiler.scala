package qq
package cc

import cats.implicits._
import qq.data._
import qq.data.ast._
import qq.util.Recursion.RecursionEngine
import qq.util._

object QQCompiler {

  final def compileStep[C](runtime: QQRuntime[C])(definitions: Map[String, CompiledDefinition[C]],
                          filter: FilterComponent[C]): QQCompilationException Either C =
    filter match {
      case Dereference(name) => Right(runtime.dereference(name))
      case ConstNumber(num) => Right(runtime.constNumber(num))
      case ConstString(str) => Right(runtime.constString(str))
      case ConstBoolean(bool) => Right(runtime.constBoolean(bool))
      case FilterNot() => Right(runtime.filterNot())
      case PathOperation(components, operationF) => Right(runtime.evaluatePath(components, operationF))
      case ComposeFilters(f, s) => Right(QQRuntime.composeFilters(f, s))
      case CallFilter(filterIdentifier, params) =>
        definitions.get(filterIdentifier).fold(Either.left[QQCompilationException, InterpretedFilter](NoSuchMethod(filterIdentifier): QQCompilationException)) { (defn: CompiledDefinition) =>
          if (params.length == defn.numParams)
            defn.body(params)
          else
            Either.left[QQCompilationException, InterpretedFilter](
              WrongNumParams(filterIdentifier, defn.numParams, params.length)
            )
        }
      case AsBinding(name, as, in) => Right(runtime.asBinding(name, as, in))
      case EnlistFilter(f) => Right(runtime.enlistFilter(f))
      case SilenceExceptions(f) => Right(runtime.silenceExceptions(f))
      case EnsequenceFilters(first, second) => Right(runtime.ensequence(first, second))
      case EnjectFilters(obj) => Right(runtime.enjectFilter(obj))
      case FilterMath(first, second, op) => Right(runtime.filterMath(first, second, op))
    }

  final def compileDefinitionStep[C](soFar: OrCompilationError[Vector[CompiledDefinition[C]]],
                                  nextDefinition: Definition[FilterAST])
                                 (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition[C]]] =
    soFar.right.map { (definitionsSoFar: Vector[CompiledDefinition[C]]) =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: Vector[InterpretedFilter]) => {
        val paramsAsDefinitions: Vector[CompiledDefinition[C]] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: Vector[C]) => Right(value))
        }(collection.breakOut)
        compileFilter(definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline final def compileFilter[C](runtime: QQRuntime[C])(definitions: Vector[CompiledDefinition[C]],
                                  filter: FilterAST)
                                 (implicit rec: RecursionEngine): OrCompilationError[InterpretedFilter] =
    for {
      builtinDefinitions <- SharedPreludes.all |+| JSONPrelude.all
      allDefinitions = builtinDefinitions ++ definitions
      definitionMap = allDefinitions
        .right.map[(String, CompiledDefinition[C]), Map[String, CompiledDefinition[C]]](d => d.name -> d)(collection.breakOut)
      compileProgram = Recursion.cataM[FilterComponent, OrCompilationError, C](compileStep(runtime)(definitionMap, _))
      compiledProgram <- compileProgram(filter)
    } yield compiledProgram

  final def compileDefinitions[C](runtime: QQRuntime[C])(prelude: Prelude[C],
                                  definitions: Program.Definitions[FilterAST])
                              (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition[C]]] = {
    definitions.foldLeft(prelude.all(runtime))(compileDefinitionStep)
  }

  final def compileProgram[C](runtime: QQRuntime[C])(prelude: Prelude[C],
                           program: Program[FilterAST])
                          (implicit rec: RecursionEngine): OrCompilationError[InterpretedFilter] =
    for {
      compiledDefinitions <- compileDefinitions(prelude, program.defns)
      compiledMain <- compileFilter(runtime)(compiledDefinitions, program.main)
    } yield compiledMain

}

