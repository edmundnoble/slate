package qq
package cc

import cats.Functor
import cats.data.Reader
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.eff._
import qq.data._
import qq.ast._
import qq.cc.CompileError.OrCompileError

trait QQCompiler[F[_]] {

  type DefinitionMapTo[A] = Reader[DefinitionMap[C], A]

  type S = Fx.fx2[DefinitionMapTo, OrCompileError]
  type E = Fx.fx1[OrCompileError]
  type V = Fx.fx1[DefinitionMapTo]

  type C

  implicit val func: Functor[F]
  val runtime: QQRuntime[C]
  val prelude: Prelude[C]

  val preludeBindingsE: OrCompileError[Vector[CompiledDefinition[C]]] = prelude.all(runtime)

  val definitions: F[Vector[Eff[S, CompiledDefinition[C]]]]

  val definitionMap: F[OrCompileError[DefinitionMap[C]]] = definitions.map(defns =>
    defns.foldLeft[OrCompileError[DefinitionMap[C]]](preludeBindingsE.map(_.map(d => d.name -> d)(collection.breakOut)))((s, r) =>
      s.flatMap(ds => reader.runReader[S, E, DefinitionMap[C], CompiledDefinition[C]](ds)(r).detach.map(d => ds + (d.name -> d)))
    )
  )

  val program: F[OrCompileError[C]]

}

object QQCompiler {

  import CompileError._

  trait PathParserE {
    def apply[C](rt: PathTypeRuntime[C]): rt.P
  }

  object PathParserE {
    def selectIndex(idx: Int): PathParserE = new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = rt.selectIndex(idx)
    }
    def selectRange(start: Int, end: Int): PathParserE = new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = rt.selectRange(start, end)
    }
    def selectKey(key: String): PathParserE = new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = rt.selectKey(key)
    }
    val collectResults: PathParserE = new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = rt.collectResults
    }
    val empty: PathParserE = new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = rt.empty
    }
  }

  def callFilter[C](definitions: Map[String, CompiledDefinition[C]], filterIdentifier: String, params: Vector[C]) = {
    definitions.get(filterIdentifier).fold(Either.left[CompileError, C](noSuchMethod(filterIdentifier))) { defn =>
      if (params.length == defn.numParams)
        defn.body(params)
      else
        Either.left[CompileError, C](
          WrongNumParams(filterIdentifier, defn.numParams, params.length)
        )
    }
  }

}

