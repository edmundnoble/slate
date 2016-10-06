package qq
package cc

import monix.eval.Task
import qq.data.{CompiledDefinition, ConcreteFilter, Definition, QQDSL}
import qq.util.Recursion.RecursionEngine

import scalaz.syntax.plusEmpty._
import scalaz.syntax.either._

object SharedPreludes {

  object Compiled {

    def apply[J]: Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] = {

        val print: CompiledDefinition[J] =
          CompiledDefinition.noParamDefinition[J]("print",
            CompiledFilter.func { (jsv: J) =>
              println("debug: " + runtime.print(jsv))
              Task.now(jsv :: Nil)
            }
          )

        val empty: CompiledDefinition[J] =
          CompiledDefinition.noParamDefinition[J]("empty", CompiledFilter.constL(Nil))

        IndexedSeq(print, empty).right
      }
    }
  }

  object Raw {
    val map: Definition[ConcreteFilter] = {
      import QQDSL.fix._
      Definition("map",
        params = List("x"),
        body = compose(collectResults, call("x"))
      )
    }

    def apply[J]: Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J])(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
        QQCompiler.compileDefinitions(runtime, Prelude.empty, List(map))
    }
  }

  def apply[J]: Prelude[J] = Raw[J] <+> Compiled[J]

}
