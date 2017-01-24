package qq
package cc

import cats.implicits._
import qq.data._
import qq.data.ast._
import qq.util.Recursion.RecursionEngine
import qq.util._

object QQCompiler {

  final def compileStep(definitions: Map[String, CompiledDefinition],
                        filter: FilterComponent[CompiledFilter]): QQCompilationException Either CompiledFilter =
    filter match {
      case Dereference(name) => Right(QQRuntime.dereference(name))
      case ConstNumber(num) => Right(QQRuntime.constNumber(num))
      case ConstString(str) => Right(QQRuntime.constString(str))
      case ConstBoolean(bool) => Right(QQRuntime.constBoolean(bool))
      case FilterNot() => Right(QQRuntime.filterNot())
      case PathOperation(components, operationF) => Right(QQRuntime.evaluatePath(components, operationF))
      case ComposeFilters(f, s) => Right(CompiledFilter.composeFilters(f, s))
      case CallFilter(filterIdentifier, params) => QQRuntime.callFilter(definitions, filterIdentifier, params)
      case AsBinding(name, as, in) => Right(CompiledFilter.asBinding(name, as, in))
      case EnlistFilter(f) => Right(QQRuntime.enlistFilter(f))
      case SilenceExceptions(f) => Right(QQRuntime.silenceExceptions(f))
      case EnsequenceFilters(first, second) => Right(CompiledFilter.ensequenceCompiledFilters(first, second))
      case EnjectFilters(obj) => Right(QQRuntime.enjectFilter(obj))
      case FilterMath(first, second, op) => Right(QQRuntime.filterMath(first, second, op))
    }

  final def compileDefinitionStep(soFar: OrCompilationError[Vector[CompiledDefinition]],
                                  nextDefinition: Definition[FilterAST])
                                 (implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
    soFar.map { (definitionsSoFar: Vector[CompiledDefinition]) =>
      CompiledDefinition(nextDefinition.name, nextDefinition.params.length, (params: Vector[CompiledFilter]) => {
        val paramsAsDefinitions: Vector[CompiledDefinition] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: Vector[CompiledFilter]) => Right(value))
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

