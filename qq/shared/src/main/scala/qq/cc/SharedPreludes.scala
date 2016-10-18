package qq
package cc

import monix.eval.Task
import qq.data._
import qq.util.Recursion.RecursionEngine

import scalaz.syntax.monoid._
import scalaz.syntax.either._
import scalaz.syntax.validation._

object SharedPreludes {

  val compiled: Prelude = new Prelude {
    override def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] = {

      val print: CompiledDefinition =
        CompiledDefinition.noParamDefinition("print",
          CompiledFilter.func { (jsv: JSON) =>
            println("debug: " + QQRuntime.print(jsv))
            Task.now((jsv :: Nil).successNel)
          }
        )

      val empty: CompiledDefinition =
        CompiledDefinition.noParamDefinition("empty", CompiledFilter.constL(Nil))

      IndexedSeq(print, empty).right
    }
  }

  val raw: Prelude = new Prelude {
    val map: Definition[ConcreteFilter] = {
      import QQDSL.fix._
      Definition("map",
        params = List("x"),
        body = compose(collectResults, call("x"))
      )
    }

    override def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] =
      QQCompiler.compileDefinitions(Prelude.empty, List(map))
  }

  def all: Prelude = raw |+| compiled

}
