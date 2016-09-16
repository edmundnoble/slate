package qq

import matryoshka.{Corecursive, Fix, Recursive}
import monix.eval.Task
import monix.scalaz._
import qq.FilterComponent._

import scalaz.{PlusEmpty, Reader, \/}
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import qq.Platform.Rec._
import qq.util._

import scala.language.higherKinds
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

case class TypeError(expectedType: String, actualValueRepr: String)
  extends QQRuntimeException(message = "Expected a/an " + expectedType + ", but got " + actualValueRepr)

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

  case class VarBinding[J](name: String, value: J)

  type VarBindings[J] = Map[String, VarBinding[J]]
  type CompiledFilter[J] = VarBindings[J] => CompiledProgram[J]
  type CompiledProgram[J] = J => Task[List[J]]

  object CompiledFilter {
    @inline final def const[J](value: J): CompiledFilter[J] =
      (_: VarBindings[J]) => (_: J) => Task.now(value :: Nil)

    @inline final def func[J](f: CompiledProgram[J]): CompiledFilter[J] =
      (_: VarBindings[J]) => f

  }

  implicit def compiledFilterPlusEmpty = new PlusEmpty[CompiledFilter] {
    override def plus[A](a: CompiledFilter[A], b: => CompiledFilter[A]): CompiledFilter[A] =
      composeFilters(a, b)

    override def empty[A]: CompiledFilter[A] =
      (_: VarBindings[A]) => (j: A) => Task.now(j :: Nil)
  }

  type OrCompilationError[T] = QQCompilationException \/ T

  def composeFilters[J](f: CompiledFilter[J], s: CompiledFilter[J]): CompiledFilter[J] = {
    (for {
      fstFun <- Reader(f)
      sndFun <- Reader(s)
    } yield
      fstFun.andThen(_.flatMap(t => Parallel.unwrap(t.traverseM[TaskParallel, J](sndFun.andThen(Parallel(_))))))).run
  }

  def ensequenceCompiledFilters[J]
  (first: CompiledFilter[J], second: CompiledFilter[J]): CompiledFilter[J] =
    (for {fstFun <- Reader(first); sndFun <- Reader(second)} yield { jsv: J =>
      Task.mapBoth(fstFun(jsv), sndFun(jsv)) { (a, b) => a ++ b }
    }).run

  def zipFiltersWith[J]
  (first: CompiledFilter[J], second: CompiledFilter[J], fun: (J, J) => Task[J]): CompiledFilter[J] =
    (for {fstFun <- Reader(first); sndFun <- Reader(second)} yield { jsv: J =>
      Task.mapBoth(fstFun(jsv), sndFun(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten
    }).run

  def letBinding[J](name: String, as: CompiledFilter[J], in: CompiledFilter[J]): CompiledFilter[J] = {
    (bindings: VarBindings[J]) =>
      (v: J) => {
        val res = as(bindings)(v)
        val newBindings = res.map(_.map(VarBinding[J](name, _)))
        val allBindings = newBindings.map(_.map(b => bindings + (b.name -> b)))
        val o = allBindings.map(_.map(in(_)(v)))
        o.flatMap((ltl: List[Task[List[J]]]) => ltl.sequence[Task, List[J]].map(_.flatten))
      }
  }

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
    case ComposeFilters(f, s) => composeFilters(f, s).right
    case LetAsBinding(name, as, in) => letBinding(name, as, in).right
    case EnlistFilter(f) => runtime.enlistFilter(f).right
    case SilenceExceptions(f) => (for {
      fFun <- Reader(f)
    } yield (jsv: J) => fFun(jsv).onErrorRecover { case _: QQRuntimeException => Nil }).run.right[QQCompilationException]
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

