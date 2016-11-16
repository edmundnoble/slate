package qq
package cc

import qq.data._
import qq.util.Recursion.RecursionEngine
import org.atnos.eff._, syntax.all._

import cats.implicits._

object SharedPreludes extends Prelude {

  val compiled: Prelude = new Prelude {
    override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] = {

      val print: CompiledDefinition =
        CompiledDefinition.noParamDefinition("print",
          CompiledFilter.singleton { (jsv: JSON) =>
            println("debug: " + QQRuntime.print(jsv))
            (jsv :: Nil).pureEff
          }
        )

      val empty: CompiledDefinition =
        CompiledDefinition.noParamDefinition("empty", CompiledFilter.constL(Nil))

      Right(Vector(print, empty))
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
