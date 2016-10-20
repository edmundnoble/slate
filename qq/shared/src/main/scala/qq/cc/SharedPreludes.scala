package qq
package cc

import monix.eval.Task
import qq.data._
import qq.util.Recursion.RecursionEngine

import cats.implicits._

object SharedPreludes extends Prelude {

  val compiled: Prelude = new Prelude {
    override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] = {

      val print: CompiledDefinition =
        CompiledDefinition.noParamDefinition("print",
          CompiledFilter.func { (jsv: JSON) =>
            println("debug: " + QQRuntime.print(jsv))
            Task.now((jsv :: Nil).validNel)
          }
        )

      val empty: CompiledDefinition =
        CompiledDefinition.noParamDefinition("empty", CompiledFilter.constL(Nil))

      Vector(print, empty).right
    }
  }

  val raw: Prelude = new Prelude {
    val map: Definition[ConcreteFilter] = {
      import QQDSL._
      Definition("map",
        params = List("x"),
        body = compose(collectResults, call("x"))
      )
    }

    override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
      QQCompiler.compileDefinitions(Prelude.empty, List(map))
  }

  def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
    compiled.all |+| raw.all

}
