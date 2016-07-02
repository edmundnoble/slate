package qq

import monix.eval.Task
import qq.Compiler.QQCompilationException

import scalaz.\/
import scalaz.syntax.either._

object SharedPreludes {

  object Compiled {
    def print[C <: Compiler with Singleton]: CompiledDefinition[C] = {
      val body: C#CompiledFilter = { (jsv: C#AnyTy) => println(s"debug: $jsv"); Task.now(jsv :: Nil) }
      CompiledDefinition[C]("print", 0, body = { _ => body.right[QQCompilationException] })
    }

    def apply[C <: Compiler with Singleton]: Prelude[C] = new Prelude[C] {
      override def all(compiler: C): QQCompilationException \/ List[CompiledDefinition[compiler.type]] =
        (print[compiler.type] :: Nil).right.map(identity)
    }
  }

  object Raw {
    val map: Definition = {
      Definition("map",
        params = List("x"),
        body = Filter.compose(Filter.collectResults(Filter.id), Filter.call("x"))
      )
    }


    def apply[C <: Compiler]: Prelude[Compiler] = new Prelude[Compiler] {
      override def all(compiler: Compiler): QQCompilationException \/ List[CompiledDefinition[compiler.type]] =
        compiler.compileDefinitions(map :: Nil)
    }
  }

  def apply[C <: Compiler]: Prelude[Compiler] = new Prelude[Compiler] {
    override def all(compiler: Compiler): QQCompilationException \/ List[CompiledDefinition[compiler.type]] =
      for {
        raw <- Raw[compiler.type].all(compiler)
        compiled <- Compiled[compiler.type].all(compiler)
      } yield raw ++ compiled
  }

}
