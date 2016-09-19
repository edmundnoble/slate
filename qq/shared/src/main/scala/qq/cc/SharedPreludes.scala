package qq
package cc

import monix.eval.Task
import qq.data.{CompiledDefinition, ConcreteFilter, Definition, FilterDSL}
import scalaz.syntax.monoid._

import scalaz.syntax.either._

object SharedPreludes {

  object Compiled {
    def print[J](runtime: QQRuntime[J]): CompiledDefinition[J] = {
      val body: CompiledFilter[J] = CompiledFilter.func { (jsv: J) => println("debug: " + runtime.print(jsv)); Task.now(jsv :: Nil) }
      CompiledDefinition[J]("print", 0, body = { _ => body.right[QQCompilationException] })
    }

    def apply[J]: Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
        IndexedSeq(print(runtime)).right
    }
  }

  object Raw {
    val map: Definition[ConcreteFilter] = {
      import FilterDSL.fix._
      Definition("map",
        params = List("x"),
        body = compose(collectResults(id), call("x"))
      )
    }

    def apply[J]: Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] =
        QQCompiler.compileDefinitions(runtime, None, List(map))
    }
  }

  def apply[J]: Prelude[J] = Raw[J] |+| Compiled[J]

}
