package qq

import matryoshka.Fix
import monix.eval.Task
import monix.scalaz._
import qq.FilterComponent._

import scalaz.{EitherT, State, StateT, \/}
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.syntax.applicative._
import qq.Platform.Rec._
import qq.Util._

import scalaz.Free.Trampoline
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

  def composeFilters[JsonTy](f: CompiledFilter[JsonTy], s: CompiledFilter[JsonTy]): (JsonTy) => Task[List[JsonTy]] = {
    f.andThen(_.flatMap(t => Parallel.unwrap(t.traverseM[TaskParallel, JsonTy](s.andThen(Parallel(_))))))
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
    compileDefinitions(runtime, prelude, program.defns).flatMap(compile(runtime, _, program.main).run.eval(Nil).run)
  }

  case class VarBinding()

  type CompileS[+A] = StateT[Trampoline, List[VarBinding], A@annotation.unchecked.uncheckedVariance]

  type OrCompilationErrorT[F[_], +T] = EitherT[F, QQCompilationException, T@annotation.unchecked.uncheckedVariance]

  type CompileM[+A] = OrCompilationErrorT[CompileS, A@annotation.unchecked.uncheckedVariance]

  @inline
  def compileStep[JsonTy](runtime: QQRuntime[JsonTy],
                          definitions: IndexedSeq[CompiledDefinition[JsonTy]],
                          filter: FilterComponent[CompiledFilter[JsonTy]]): CompileM[CompiledFilter[JsonTy]] = filter match {
    case leaf: LeafComponent[JsonTy@unchecked] => EitherT.right(runtime.evaluateLeaf(leaf).pure[CompileS])
    case ComposeFilters(f, s) => EitherT.right(composeFilters(f, s).pure[CompileS])
    case LetAsBinding(name, f, s) => ???
    case EnlistFilter(f) => EitherT.right(runtime.enlistFilter(f).pure[CompileS])
    case SilenceExceptions(f) => EitherT.right(((jsv: JsonTy) => f(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).pure[CompileS])
    case CollectResults(f) => EitherT.right(runtime.collectResults(f).pure[CompileS])
    case EnsequenceFilters(first, second) => EitherT.right(ensequenceCompiledFilters(first, second).pure[CompileS])
    case EnjectFilters(obj) => EitherT.right(runtime.enjectFilter(obj).pure[CompileS])
    case AddFilters(first, second) => EitherT.right(zipFiltersWith(first, second, runtime.addJsValues).pure[CompileS])
    case SubtractFilters(first, second) => EitherT.right(zipFiltersWith(first, second, runtime.subtractJsValues).pure[CompileS])
    case MultiplyFilters(first, second) => EitherT.right(zipFiltersWith(first, second, runtime.multiplyJsValues).pure[CompileS])
    case DivideFilters(first, second) => EitherT.right(zipFiltersWith(first, second, runtime.divideJsValues).pure[CompileS])
    case ModuloFilters(first, second) => EitherT.right(zipFiltersWith(first, second, runtime.moduloJsValues).pure[CompileS])
    case CallFilter(filterIdentifier, params) =>
      definitions.find(_.name == filterIdentifier).cata(
        { (defn: CompiledDefinition[JsonTy]) =>
          if (params.length == defn.numParams) {
            EitherT(defn.body(params).pure[CompileS])
          } else {
            EitherT.left(WrongNumParams(filterIdentifier, defn.numParams, params.length).pure[CompileS])
          }
        },
        EitherT.left(NoSuchMethod(filterIdentifier).pure[CompileS])
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
        compile(runtime, definitionsSoFar ++ paramsAsDefinitions, nextDefinition.body).run.eval(Nil).run
      }) +: definitionsSoFar
    }

  @inline
  def compile[JsonTy](runtime: QQRuntime[JsonTy],
                      definitions: IndexedSeq[CompiledDefinition[JsonTy]],
                      filter: Filter): CompileM[CompiledFilter[JsonTy]] =
    for {
      sharedDefinitions <- EitherT[CompileS, QQCompilationException, IndexedSeq[CompiledDefinition[JsonTy]]](SharedPreludes[JsonTy].all(runtime).pure[CompileS])
      platformSpecificDefinitions <- EitherT[CompileS, QQCompilationException, IndexedSeq[CompiledDefinition[JsonTy]]](runtime.platformPrelude.all(runtime).pure[CompileS])
      allDefinitions = sharedDefinitions ++ platformSpecificDefinitions ++ definitions
      compiledProgram <-
      Recursion.cataM[Fix, FilterComponent, CompileM, CompiledFilter[JsonTy]](compileStep(runtime, allDefinitions, _))(Fix.recursive, FilterComponent.qqFilterComponentTraverse, EitherT.eitherTMonad[CompileS, QQCompilationException]).apply(filter)
    } yield compiledProgram

}

