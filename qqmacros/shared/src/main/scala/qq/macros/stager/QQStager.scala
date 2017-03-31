package qq.macros.stager

import fastparse.core.Parsed
import qq.cc.{ParserCompiler, Prelude}
import qq.ast._
import qq.cc.CompileError.OrCompileError
import qq.data.CompiledDefinition
import cats.syntax.invariant._
import org.atnos.eff.{Eff, Fx}
import qq.cc.Prelude.PreludeStack

import scala.language.experimental.macros
import scala.reflect.api
import scala.reflect.api.{TreeCreator, Universe}
import scala.reflect.macros.whitebox


// used to pre-compile QQ programs at compile time
object QQStager {

  def apply[C](program: String): C = macro applyImpl[C]

  def applyImpl[C, PR[X] <: Prelude[X] with Singleton](c: whitebox.Context)
                  (program: c.Expr[String])
                  (runtime: c.Expr[QQRuntime[C]], prelude: c.Expr[Prelude[c.Expr[C]]])
                  (implicit preludeTag: c.WeakTypeTag[PR[c.Expr[C]]]): c.Expr[C] = {
    import c.universe._
    val Literal(Constant(programString: String)) = program

    val rootMirror = scala.reflect.runtime.currentMirror

    val runtimeE = new QQRuntime[c.Expr[C]] {
      val path: PathRuntime[c.Expr[C]] = new PathRuntime[c.Expr[C]] {
        // distributive law? something behind this?
        def genericPathTypeRuntime(prt: c.Expr[PathTypeRuntime[C]]): PathTypeRuntime[c.Expr[C]] = {
          new PathTypeRuntime[c.Expr[C]] {
            type P = c.Tree
            val collectResults: c.Tree =
              q"prt$$.collectResults"

            def selectKey(key: String): c.Tree = {
              q"prt$$.selectKey(key)"
            }

            def selectIndex(index: Int): c.Tree = {
              q"prt$$.selectKey(key)"
            }

            def selectRange(start: Int, end: Int): c.Tree = {
              q"prt$$.selectKey(key)"
            }

            def empty: c.Tree = {
              q"prt$$.empty"
            }

            def append(p1: c.Tree, p2: c.Tree): c.Tree = {
              q"prt$$.append($p1, $p2)"
            }

            def ret(p: c.Tree): c.Expr[C] = {
              reify {
                // I wish "comments" were for comments about the libraries in use,
                // instead of the code I'm writing
                val prt$$ = prt.splice
                prt$$.ret(c.Expr[prt$$.P](c.mirror, new TreeCreator {
                  def apply[U <: Universe with Singleton](m: api.Mirror[U]): U#Tree = {
                    p.asInstanceOf[U#Tree]
                  }
                }).splice)
              }
            }
          }
        }

        def set(f: c.Expr[C]): PathTypeRuntime[c.Expr[C]] = {
          genericPathTypeRuntime(reify(runtime.splice.path.set(f.splice)))
        }

        def modify(f: c.Expr[C]): PathTypeRuntime[c.Expr[C]] = {
          genericPathTypeRuntime(reify(runtime.splice.path.modify(f.splice)))
        }

        val get: PathTypeRuntime[c.Expr[C]] =
          genericPathTypeRuntime(reify(runtime.splice.path.get))
      }

      def dereference(name: String): c.Expr[C] = {
        reify(runtime.value.dereference(name))
      }

      val filterNot: c.Expr[C] =
        reify(runtime.value.filterNot)

      def add(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.add(first.value, second.value))
      }

      def subtract(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.subtract(first.value, second.value))
      }

      def multiply(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.multiply(first.value, second.value))
      }

      def divide(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.divide(first.value, second.value))
      }

      def modulo(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.modulo(first.value, second.value))
      }

      def equal(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.equal(first.value, second.value))
      }

      def lte(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.lte(first.value, second.value))
      }

      def gte(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.gte(first.value, second.value))
      }

      def lessThan(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.lessThan(first.value, second.value))
      }

      def greaterThan(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.lessThan(first.value, second.value))
      }

      def silenceExceptions(f: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.silenceExceptions(f.value))
      }

      def constNumber(num: Double): c.Expr[C] = {
        reify(runtime.value.constNumber(num))
      }

      def constString(str: String): c.Expr[C] = {
        reify(runtime.value.constString(str))
      }

      def constBoolean(bool: Boolean): c.Expr[C] = {
        reify(runtime.value.constBoolean(bool))
      }

      def enlistFilter(filter: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.enlistFilter(filter.value))
      }

      def enject(obj: Vector[(Either[String, c.Expr[C]], c.Expr[C])]): c.Expr[C] = {
        reify(runtime.value.enject(obj.map { case (k, v) => (k.map(_.value), v.value) }))
      }

      def asBinding(name: String, as: c.Expr[C], in: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.asBinding(name, as.value, in.value))
      }

      def ensequence(first: c.Expr[C], second: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.ensequence(first.value, second.value))
      }

      def composeFilters(f: c.Expr[C], s: c.Expr[C]): c.Expr[C] = {
        reify(runtime.value.composeFilters(f.value, s.value))
      }

    }

    def getCompanion[T](expr: c.Expr[T]): T = {
      val Select(pre, e) = expr
      val klass = Class.forName(e.encodedName + "$")
      klass.getField("$MODULE").get(klass)
      rootMirror.moduleSymbol(expr.actualType.typeSymbol.asClass.module)
    }

    val preludeT = preludeTag.tpe

    val moduleClass =
      preludeTag.tpe.asInstanceOf[TypeRef].pre.typeSymbol

    val module =
      moduleClass.owner.typeSignature.member(moduleClass.name.toTermName)

    val preludeE: Prelude[c.Expr[C]] =
      c.mirror.reflectModule(module.asModule).instance.asInstanceOf[Prelude[c.Expr[C]]]

    val parser = new ParserCompiler[c.Expr[C]](runtimeE, preludeE)
    val parsedProgram: c.Expr[C] = parser.program.parse(programString) match {
      case f@Parsed.Failure(_, _, _) =>
        throw new Exception("QQ parsing error: " + f.extra.traced.trace)
      case Parsed.Success(prog, _) => prog match {
        case Right(r) => r
        case Left(e) => c.abort(c.enclosingPosition, "qq compile error: " + e)
      }
    }

    c.abort(c.enclosingPosition, "hi")

    //    val optimizedProgram: Program[FilterAST] = LocalOptimizer.optimizeProgram(parsedProgram)
    //    compiler.compileProgram(optimizedProgram)
  }

  //  def qqimpl(c: whitebox.Context)(pieces: c.Tree*): c.Tree = {
  //
  //    import c.universe._
  //
  //    val parsedProgram: Program[FilterAST] = Parser.program.parse(program) match {
  //      case f@Parsed.Failure(_, _, _) =>
  //        c.abort(c.enclosingPosition, "QQ parsing error: " + f.extra.traced.trace)
  //      case Parsed.Success(prog, _) => prog
  //    }
  //
  //    val optimizedProgram = LocalOptimizer.optimizeProgram(parsedProgram)
  //    lift(optimizedProgram)
  ////    val comp = new QQCompiler(QQStagerRuntime, StagedJSONPrelude)
  ////    comp.compileProgram(optimizedProgram).fold(e => c.abort(c.enclosingPosition, s"compilation error: $e"), t => t)
  //  }


}

