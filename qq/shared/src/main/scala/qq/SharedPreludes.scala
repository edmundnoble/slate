package qq

import monix.eval.Task
import qq.QQCompiler.{CompiledFilter, OrCompilationError}

import scalaz.syntax.either._

object SharedPreludes {

  object Compiled {
    def print[AnyTy](runtime: QQRuntime[AnyTy]): CompiledDefinition[AnyTy] = {
      val body: CompiledFilter[AnyTy] = { (jsv: AnyTy) => println("debug: " + runtime.print(jsv)); Task.now(jsv :: Nil) }
      CompiledDefinition[AnyTy]("print", 0, body = { _ => body.right[QQCompilationException] })
    }

    def apply[AnyTy]: Prelude[AnyTy] = new Prelude[AnyTy] {
      override def all(runtime: QQRuntime[AnyTy]): OrCompilationError[IndexedSeq[CompiledDefinition[AnyTy]]] =
        (print(runtime) +: IndexedSeq.empty).right.map(identity)
    }
  }

  object Raw {
    val map: Definition = {
      import FilterDSL._
      Definition("map",
        params = List("x"),
        body = compose(collectResults(id), call("x"))
      )
    }

    def apply[AnyTy]: Prelude[AnyTy] = new Prelude[AnyTy] {
      override def all(runtime: QQRuntime[AnyTy]): OrCompilationError[IndexedSeq[CompiledDefinition[AnyTy]]] =
        QQCompiler.compileDefinitions(runtime, IndexedSeq.empty, map +: Vector.empty)
    }
  }

  def apply[AnyTy]: Prelude[AnyTy] = new Prelude[AnyTy] {
    override def all(runtime: QQRuntime[AnyTy]): OrCompilationError[IndexedSeq[CompiledDefinition[AnyTy]]] =
      for {
        raw <- Raw[AnyTy].all(runtime)
        compiled <- Compiled[AnyTy].all(runtime)
      } yield raw ++ compiled
  }

}
