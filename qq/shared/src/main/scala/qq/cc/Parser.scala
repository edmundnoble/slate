package qq
package cc

import cats._
import cats.implicits._
import fastparse.all._
import fastparse.core.Implicits
import fastparse.parsers
import fastparse.utils.Utils.BitSet
import org.atnos.eff._
import org.atnos.eff.syntax.eff._
import qq.data._
import qq.ast._
import qq.cc.CompileError.OrCompileError
import QQCompiler._

object BaseParsers {

  val dot: P0 = P(".")
  val quote: P0 = P("\"")

  case class CharsWhileFastSetup(set: Seq[Char], min: Int = 1) extends fastparse.core.Parser[Unit, Char, String]() {
    private[this] val uberSet = BitSet(set)

    def parseRec(cfg: ParseCtx, index: Int) = {
      var curr = index
      val input = cfg.input
      while (curr < input.length && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, cut = false)
    }
  }

  val stringLiteralChars: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "↪+*-_".toSeq
  val stringLiteral: P[String] = P(CharsWhileFastSetup(stringLiteralChars).!)

  val whitespaceChars: Seq[Char] = " \n\t".toSeq
  val whitespace: P0 = P(CharsWhileFastSetup(whitespaceChars, min = 0).map(_ => ()))
  val escapedStringLiteralChars: Seq[Seq[Char]] =
    Seq(stringLiteralChars, "[]$(),.:/".toSeq, whitespaceChars)

  val escapedStringLiteral: P[String] = P(
    quote ~/
      ((parsers.Terminals.ElemLiteral('\\') ~/ (parsers.Terminals.ElemLiteral('n').map(_ => "\n") | parsers.Terminals.ElemLiteral('"').map(_ => "\"") | parsers.Terminals.ElemLiteral('\\').!)) |
        CharsWhileFastSetup(escapedStringLiteralChars.flatten, min = 0).!).map(_.mkString) ~ quote
  )

  val numericLiteral: P[Int] = P(
    CharsWhileFastSetup('0' to '9').!.map(_.toInt)
  )

  val doubleLiteral: P[Double] = P(
    // TODO: fix
    numericLiteral.map(_.toDouble)
  )

  val filterIdentifier: P[String] = P(
    stringLiteral
  )

  val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val selectIndex =
    P(for {
      fun <- LiteralStr("-").!.? map (_.fold(identity[Int] _)(_ => (i: Int) => -i))
      number <- numericLiteral
    } yield PathParserE.selectIndex(number))

  val selectRange =
    P(for {
      start <- numericLiteral
      end <- ":" ~ numericLiteral
    } yield PathParserE.selectRange(start, end))

  val escapedSelectKey =
    P(for {
      key <- escapedStringLiteral
    } yield PathParserE.selectKey(key))

  val selectKey =
    P(for {
      key <- stringLiteral
    } yield PathParserE.selectKey(key))

  val simplePathComponent =
    P(
      ("[" ~/ (escapedSelectKey | selectRange | selectIndex) ~ "]") | selectKey | selectIndex
    )

  val pathComponent = P(
    for {
      s <- simplePathComponent
      f <- ("[" ~ "]").!.?
    } yield new PathParserE {
      def apply[C](rt: PathTypeRuntime[C]): rt.P = {
        f.fold(s(rt))(_ => rt.append(s(rt), rt.collectResults))
      }
    }
  )

  val fullPath = P(
    dot ~
      (LiteralStr("[]").map(_ => PathParserE.collectResults) | pathComponent)
        .rep(sep = dot)
        .map(s => new PathParserE {
          def apply[C](rt: PathTypeRuntime[C]): rt.P = s.foldLeft(rt.empty)((p, u) => rt.append(p, u(rt)))
        })
  )


  implicit object ParserMonad extends Monad[Parser] {
    override def pure[A](a: A): Parser[A] =
      parsers.Transformers.Mapper(parsers.Terminals.Pass[Char, String](), (_: Unit) => a)

    override def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      parsers.Transformers.FlatMapped[A, B, Char, String](fa, f)

    override def tailRecM[A, B](a: A)(f: (A) => Parser[A Either B]): Parser[B] =
      flatMap[A Either B, B](f(a))(_.fold(tailRecM[A, B](_)(f), pure[B]))
  }

  implicit def vectorRepeater[T]: Implicits.Repeater[T, Vector[T]] = new Implicits.Repeater[T, Vector[T]] {
    type Acc = collection.immutable.VectorBuilder[T]

    def initial: Acc = new Acc()

    def accumulate(t: T, acc: Acc): Unit = {
      val _ = acc += t
    }

    def result(acc: Acc): Vector[T] = acc.result()
  }

}

final class ParserCompiler[C0](val runtime: QQRuntime[C0], val prelude: Prelude[C0]) extends QQCompiler[P] {

  type C = C0

  import BaseParsers._

  type meme = Member.Aux[DefinitionMapTo, S, E]

  val func: Functor[P] = ParserMonad

  def setPathFilter: P[Eff[S, PathTypeRuntime[C]]] = P(
    ("=" ~ whitespace ~ filter).map(_.map(runtime.path.set))
  )

  def modifyPathFilter: P[Eff[S, PathTypeRuntime[C]]] = P(
    ("|=" ~ whitespace ~ filter).map(_.map(runtime.path.modify))
  )

  def paths: P[Eff[S, C]] = P(
    for {
      path <- fullPath
      f <- (whitespace ~ (setPathFilter | modifyPathFilter)) | parsers.Terminals.Pass().map(_ => Eff.pure[S, PathTypeRuntime[C]](runtime.path.get))
    } yield f.map(p => p.ret(path.apply(p)))
  )

  def callFilter: P[Eff[S, C]] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Vector.empty))
    } yield {
      for {
        defns <- reader.ask[S, DefinitionMap[C0]]
        filt <- Eff.send[OrCompileError, S, C](
          params.map(f => reader.runReader[S, E, DefinitionMap[C], C](defns)(f)(implicitly[meme]).detach).sequence[OrCompileError, C].flatMap(QQCompiler.callFilter(defns, identifier, _))
        )
      } yield filt
    }
  )

  val asBinding: P[Eff[S, C]] = P(
    for {
      variable <- variableIdentifier
      as <- whitespace ~ LiteralStr("as") ~ whitespace ~ filter
      in <- whitespace ~ LiteralStr("in") ~ whitespace ~ filter
    } yield Eff.EffApplicative[S].map2(as, in)(runtime.asBinding(variable, _, _))
  )

  val constInt: P[C] = P(numericLiteral map (runtime.constNumber(_)))

  val constString: P[C] = P(escapedStringLiteral map runtime.constString)

  val constBoolean: P[C] = P((LiteralStr("true").map(_ => true) | LiteralStr("false").map(_ => false)).map(runtime.constBoolean))

  val dereference: P[C] = P(variableIdentifier.map(runtime.dereference))

  val smallFilter: P[Eff[S, C]] =
    P(
      constInt.map(Eff.pure[S, C]) | constString.map(Eff.pure[S, C]) | paths |
        dereference.map(Eff.pure[S, C]) | callFilter
    )

  def enlistedFilter: P[Eff[S, C]] = P(
    "[" ~/ filter.map(_.map(runtime.enlistFilter)) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  def enjectPair: P[Eff[S, (String Either C, C)]] = P(
    (
      (
        ("(" ~/ whitespace ~ filter ~ whitespace ~ ")").map(_.map(_.asRight[String])) |
          (stringLiteral | escapedStringLiteral).map(s => Eff.pure[S, String Either C](s.asLeft[C]))
        ) ~ ":" ~ whitespace ~ piped
      ).map(t => Eff.EffApplicative[S].tuple2(t._1, t._2))
      |
      filterIdentifier.map(id => Eff.pure[S, (String Either C, C)](Either.left(id) -> runtime.path.get.ret(runtime.path.get.selectKey(id))))
  )

  def enjectedFilter: P[Eff[S, C]] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(Eff.sequenceA(_).map(runtime.enject)) ~ whitespace ~ "}"
  )

  // binary operators with the same precedence level
  def binaryOperators[A](rec: P[Eff[S, A]], ops: (String, (A, A) => A)*): P[Eff[S, A]] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] =
      LiteralStr(text).map(_ => function)

    def foldOperators(begin: A, operators: Vector[((A, A) => A, Eff[S, A])]): Eff[S, A] =
      operators.foldLeft(Eff.pure[S, A](begin)) { case (f, (combFun, nextF)) => Eff.EffApplicative[S].map2(f, nextF)(combFun) }

    val op = ops.map((makeParser _).tupled).reduceLeft(_ | _)
    (rec ~ (whitespace ~ (op ~/ whitespace ~ rec ~ whitespace).rep(min = 1)).?).map { case (a, f) => f.map(vs => a.flatMap(foldOperators(_, vs))).getOrElse(a) }
  }

  def withEquals: P[Eff[S, C]] =
    P(binaryOperators[C](
      binaryOperators[C](
        ensequenced, "==" -> runtime.equal _)
      , "!=" -> ((f: C, s: C) => runtime.composeFilters(runtime.equal(f, s), runtime.filterNot))
    ))

  def ensequenced: P[Eff[S, C]] =
    P(binaryOperators[C](piped, "," -> runtime.ensequence _))

  def piped: P[Eff[S, C]] =
    P(binaryOperators[C](expr, "|" -> runtime.composeFilters _))

  def expr: P[Eff[S, C]] =
    P(binaryOperators[C](term, "+" -> runtime.add _, "-" -> runtime.subtract))

  def term: P[Eff[S, C]] =
    P(binaryOperators[C](factor, "*" -> runtime.multiply _, "/" -> runtime.divide _, "%" -> runtime.modulo _))

  def factor: P[Eff[S, C]] =
    P(
      ("(" ~/ whitespace ~ withEquals ~ whitespace ~ ")") |
        asBinding | smallFilter |
        enjectedFilter | enlistedFilter
    )

  def filter: P[Eff[S, C]] = P(
    for {
      f <- withEquals
      fun <- "?".!.?.map(_.fold(identity[C] _)(_ => runtime.silenceExceptions))
    } yield f.map(fun(_))
  )

  val arguments: P[Vector[String]] = P(
    "(" ~/ whitespace ~ filterIdentifier.rep(min = 1, sep = whitespace ~ ";" ~/ whitespace) ~ whitespace ~ ")"
  )

  def definition: P[Eff[S, CompiledDefinition[C]]] = P(
    for {
      name <- "def" ~/ whitespace ~ filterIdentifier
      args <- arguments.?.map(_.getOrElse(Vector.empty)) ~ ":" ~/ whitespace
      f <- filter ~ ";"
    } yield reader.ask[S, DefinitionMap[C]].map(defns => CompiledDefinition[C](name, args.length, { (muhcs) =>
      if (args.length != muhcs.length) {
        Either.left[CompileError, C](
          WrongNumParams(name, args.length, muhcs.length)
        )
      } else {
        val paramDefinitions = (args, muhcs).zipped.map { (filterName, value) =>
          CompiledDefinition(filterName, 0, (_: Vector[C]) => Right(value))
        }(collection.breakOut)
        Eff.detach(
          reader.runReader[S, E, DefinitionMap[C], C](
            defns ++ paramDefinitions.map(d => d.name -> d)(collection.breakOut)
          )(f)
        )
      }
    }))
  )

  val definitions: P[Vector[Eff[S, CompiledDefinition[C]]]] = P(
    definition.rep(min = 0, sep = whitespace)
  )

  val program: P[OrCompileError[C]] = P(
    for {
      defs <- whitespace ~ definitionMap
      filt <- filter.map(f => defs.flatMap(ds => reader.runReader[S, E, DefinitionMap[C], C](ds)(f).detach))
      _ <- whitespace ~ parsers.Terminals.End()
    } yield filt
  )

}
