package qq

import monix.eval.Task
import qq.QQCompiler.{CompiledFilter, OrCompilationError}

import scalaz.syntax.either._

object SharedPreludes {

  object Compiled {
    def print[JsonTy](runtime: QQRuntime[JsonTy]): CompiledDefinition[JsonTy] = {
      val body: CompiledFilter[JsonTy] = { (jsv: JsonTy) => println("debug: " + runtime.print(jsv)); Task.now(jsv :: Nil) }
      CompiledDefinition[JsonTy]("print", 0, body = { _ => body.right[QQCompilationException] })
    }

    def apply[JsonTy]: Prelude[JsonTy] = new Prelude[JsonTy] {
      override def all(runtime: QQRuntime[JsonTy]): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]] =
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

    def apply[JsonTy]: Prelude[JsonTy] = new Prelude[JsonTy] {
      override def all(runtime: QQRuntime[JsonTy]): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]] =
        QQCompiler.compileDefinitions(runtime, IndexedSeq.empty, map +: Vector.empty)
    }
  }

  def apply[JsonTy]: Prelude[JsonTy] = new Prelude[JsonTy] {
    override def all(runtime: QQRuntime[JsonTy]): OrCompilationError[IndexedSeq[CompiledDefinition[JsonTy]]] =
      for {
        raw <- Raw[JsonTy].all(runtime)
        compiled <- Compiled[JsonTy].all(runtime)
      } yield raw ++ compiled
  }

}
