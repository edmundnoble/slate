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
import qq.Platform.Rec._
import qq.Util._

import scalaz.Tags.Parallel

class QQRuntimeException(val message: String) extends RuntimeException(message) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: QQRuntimeException => message == other.message
    case _ => false
  }
}
object QQRuntimeException {
  def apply(message: String): QQRuntimeException = new QQRuntimeException(message)
}

case class NotARegex(asStr: String) extends QQRuntimeException(
  "tried to use this as a regex: " + asStr
)

class QQCompilationException(message: String) extends RuntimeException(message)
case class NoSuchMethod(name: String)
  extends QQCompilationException(message = "No such method: " + name)
case class UndefinedOnPlatform(name: String)
  extends QQCompilationException(message = "This method is undefined on platform: " + name)
case class WrongNumParams(name: String, correct: Int, you: Int) extends QQCompilationException(
  "Wrong number of params for filter " + name + ": passed " + you.toString + ", wanted " + correct.toString
)

object QQCompiler {

  // the type of compiled QQ filters is
  // a function from a JSON value to an effectful computation returning a list of JSON values
  type CompiledFilter[JsonTy] = JsonTy => Task[List[JsonTy]]

  object CompiledFilter {
    @inline final def const[JsonTy](value: JsonTy): CompiledFilter[JsonTy] = _ => Task.now(value :: Nil)
  }
  type OrCompilationError[T] = QQCompilationException \/ T

  def composeFilters[JsonTy](f: CompiledFilter[JsonTy], s: CompiledFilter[JsonTy]): \/[Nothing, (JsonTy) => Task[List[JsonTy]]] = {
    f.andThen(_.flatMap(t => Parallel.unwrap(t.traverseM[TaskParallel, JsonTy](s.andThen(Parallel(_)))))).right
  }

  def ensequenceCompiledFilters[JsonTy]
  (first: CompiledFilter[JsonTy], second: CompiledFilter[JsonTy]): CompiledFilter[JsonTy] = { jsv: JsonTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (a, b) => a ++ b }
  }

  def zipFiltersWith[JsonTy]
  (first: CompiledFilter[JsonTy], second: CompiledFilter[JsonTy], fun: (JsonTy, JsonTy) => Task[JsonTy]): CompiledFilter[JsonTy] = { jsv: JsonTy =>
    Task.mapBoth(first(jsv), second(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten
  }

@inline
  def compileDefinitions[JsonTy](runtime: QQRuntime[JsonTy],
                                prelude: IndexedSeq[CompiledDefinition[JsonTy]] = Vector.empty,
                                definitions: IndexedSeq[Definition]): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]] =
    definitions.foldLeft(prelude.right[QQCompilationException])(compileDefinitionStep(runtime))

  @inline
  def compileProgram[JsonTy](runtime: QQRuntime[JsonTy],
                            prelude: IndexedSeq[CompiledDefinition[JsonTy]] = Vector.empty,
                            program: Program): OrCompilationError[CompiledFilter[JsonTy]] = {
    compileDefinitions(runtime, prelude, program.defns).flatMap(compile(runtime, _, program.main))
  }

  @inline
  def compileStep[JsonTy](runtime: QQRuntime[JsonTy],
                         definitions: IndexedSeq[CompiledDefinition[JsonTy]],
                         filter: FilterComponent[CompiledFilter[JsonTy]]): OrCompilationError[CompiledFilter[JsonTy]] = filter match {
    case leaf: LeafComponent[JsonTy@unchecked] => runtime.evaluateLeaf(leaf).right
    case ComposeFilters(f, s) => composeFilters(f, s)
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => ((jsv: JsonTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).right
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
        { (defn: CompiledDefinition[JsonTy]) =>
          if (params.length == defn.numParams) {
            defn.body(params)
          } else {
            WrongNumParams(filterIdentifier, defn.numParams, params.length).left
          }
        },
        NoSuchMethod(filterIdentifier).left
      )
  }

  def compileDefinitionStep[JsonTy](runtime: QQRuntime[JsonTy])
                                  (soFar: OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]],
                                   nextDefinition: Definition): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]] =
    soFar.map { (definitionsSoFar: IndexedSeq[CompiledDefinition[JsonTy]]) =>
      CompiledDefinition[JsonTy](nextDefinition.name, nextDefinition.params.length, (params: List[CompiledFilter[JsonTy]]) => {
        val paramsAsDefinitions: IndexedSeq[CompiledDefinition[JsonTy]] = (nextDefinition.params, params).zipped.map { (filterName, value) =>
          CompiledDefinition[JsonTy](filterName, 0, (_: List[CompiledFilter[JsonTy]]) => value.right[QQCompilationException])
        }(collection.breakOut)
        compile(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body)
      }) +: definitionsSoFar
    }

  @inline
  def compile[JsonTy](runtime: QQRuntime[JsonTy],
                     definitions: IndexedSeq[CompiledDefinition[JsonTy]],
                     filter: Filter): OrCompilationError[CompiledFilter[JsonTy]] =
    for {
      sharedDefinitions <- SharedPreludes[JsonTy].all(runtime)
      platformSpecificDefinitions <- runtime.platformPrelude.all(runtime)
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <-
      Recursion.cataM[Fix, FilterComponent, OrCompilationError, CompiledFilter[JsonTy]](compileStep(runtime, allDefinitions, _)).apply(filter)
    } yield compiledProgram

}

