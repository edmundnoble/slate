package qq
package cc

import cats.Monad
import cats.implicits._
import fastparse.all._
import fastparse.core.Implicits
import fastparse.parsers
import qq.data._
import qq.data.ast._

object Parser {

  implicit object ParserMonad extends Monad[Parser] {
    override def pure[A](a: A): Parser[A] =
      parsers.Transformers.Mapper(parsers.Terminals.Pass(), (_: Unit) => a)

    override def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      parsers.Transformers.FlatMapped(fa, f)

    // TODO: Can I make this work?
    override def tailRecM[A, B](a: A)(f: (A) => Parser[Either[A, B]]): Parser[B] = ???
  }

  implicit def vectorRepeater[T]: Implicits.Repeater[T, Vector[T]] = new Implicits.Repeater[T, Vector[T]] {
    type Acc = collection.immutable.VectorBuilder[T]

    def initial: Acc = new Acc()

    def accumulate(t: T, acc: Acc): Unit = {
      val _ = acc += t
    }

    def result(acc: Acc): Vector[T] = acc.result()
  }

  import qq.data.{QQDSL => dsl}

  val dot: P0 = P(".")
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)

  //  case class CharsWhileFastSetup(set: Seq[Char], min: Int = 1) extends Parser[Unit]() {
  //    private[this] val uberSet = CharBitSet(set)
  //
  //    def parseRec(cfg: ParseCtx, index: Int) = {
  //      var curr = index
  //      val input = cfg.input
  //      while (curr < input.length && uberSet(input(curr))) curr += 1
  //      if (curr - index < min) fail(cfg.failure, curr)
  //      else success(cfg.success, (), curr, Set.empty, cut = false)
  //    }
  //  }

  val stringLiteralChars: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "↪+*-_".toSeq
  val stringLiteral: P[String] = P(CharIn(stringLiteralChars).rep(min = 1).!)

  val whitespaceChars: Seq[Char] = " \n\t".toSeq
  val whitespace: P0 = P(CharIn(whitespaceChars).rep.map(_ => ()))
  val escapedStringLiteralChars: Seq[Seq[Char]] =
    Seq(stringLiteralChars, "[]$(),.:/".toSeq, whitespaceChars)

  val escapedStringLiteral: P[String] = P(
    quote ~/
      ((parsers.Terminals.ElemLiteral('\\') ~/ (parsers.Terminals.ElemLiteral('n').map(_ => "\n") | parsers.Terminals.ElemLiteral('"').map(_ => "\"") | parsers.Terminals.ElemLiteral('\\').!)) |
        CharIn(escapedStringLiteralChars: _*).!).rep.map(_.mkString) ~ quote
  )

  val numericLiteral: P[Int] = P(
    CharIn('0' to '9').rep(min = 1).!.map(_.toInt)
  )

  val doubleLiteral: P[Double] = P(
    // TODO: fix
    numericLiteral.map(_.toDouble)
  )

  val selectIndex: P[PathComponent] = P(
    for {
      fun <- LiteralStr("-").!.? map (_.fold(identity[Int] _)(_ => (i: Int) => -i))
      number <- numericLiteral
    } yield dsl.selectIndex(fun(number))
  )

  val selectRange: P[PathComponent] = P(
    (numericLiteral ~ ":" ~/ numericLiteral) map (dsl.selectRange _).tupled
  )

  val simplePathComponent: P[PathComponent] = P(
    ("[" ~/ ((escapedStringLiteral map dsl.selectKey) | selectRange | selectIndex) ~ "]") | (stringLiteral map dsl.selectKey) | selectIndex
  )

  val pathComponent: P[Vector[PathComponent]] = P(
    for {
      s <- simplePathComponent
      f <- "[]".!.?.map(_.fold(Vector.empty[PathComponent])(_ => dsl.collectResults +: Vector.empty))
    } yield s +: f
  )

  val fullPath: P[Vector[PathComponent]] = P(
    dot ~
      ((LiteralStr("[]").as(dsl.collectResults +: Vector.empty)) | pathComponent)
        .rep(sep = dot)
        .map(_.nelFoldLeft1(Vector.empty[PathComponent])(_ ++ _))
  )

  def setPathFilter(path: Vector[PathComponent]): P[FilterAST] = P(
    ("=" ~ whitespace ~ filter).map(dsl.setPath(path, _))
  )

  def modifyPathFilter(path: Vector[PathComponent]): P[FilterAST] = P(
    ("|=" ~ whitespace ~ filter).map(dsl.modifyPath(path, _))
  )

  private val paths =
    for {
      path <- fullPath
      f <- (whitespace ~ (setPathFilter(path) | modifyPathFilter(path))) | parsers.Terminals.Pass().map(_ => dsl.getPath(path))
    } yield f

  private val filterIdentifier: P[String] = P(
    stringLiteral
  )

  private val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val asBinding: P[FilterAST] = P(
    for {
      variable <- variableIdentifier
      _ <- whitespace ~ LiteralStr("as") ~ whitespace
      as <- filter
      _ <- whitespace ~ LiteralStr("in") ~ whitespace
      in <- filter
    } yield dsl.asBinding(variable, as, in)
  )

  val callFilter: P[FilterAST] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Vector.empty))
    } yield dsl.call(identifier, params)
  )

  val constInt: P[FilterAST] = P(numericLiteral map (dsl.constNumber(_)))

  val constString: P[FilterAST] = P(escapedStringLiteral map dsl.constString)

  val constBoolean: P[FilterAST] = P((LiteralStr("true").map(_ => true) | LiteralStr("false").map(_ => false)).map(dsl.constBoolean))

  val dereference: P[FilterAST] = P(variableIdentifier.map(dsl.deref))

  val smallFilter: P[FilterAST] = P(constInt | constString | paths | dereference | callFilter)

  val enlistedFilter: P[FilterAST] = P(
    "[" ~/ filter.map(dsl.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String Either FilterAST, FilterAST)] = P(
    ((("(" ~/ whitespace ~ filter ~ whitespace ~ ")").map(Right[String, FilterAST]) |
      (stringLiteral | escapedStringLiteral).map(Left[String, FilterAST])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => Left(id) -> dsl.getPath(Vector(dsl.selectKey(id))))
  )

  val enjectedFilter: P[FilterAST] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(dsl.enject) ~ whitespace ~ "}"
  )

  // binary operators with the same precedence level
  def binaryOperators[A](rec: P[A], ops: (String, (A, A) => A)*): P[A] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] = LiteralStr(text).as(function)

    def foldOperators(begin: A, operators: Vector[((A, A) => A, A)]): A =
      operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }

    val op = ops.map((makeParser _).tupled).reduceLeft(_ | _)
    (rec ~ (whitespace ~ (op ~/ whitespace ~ rec ~ whitespace).rep(min = 1)).?).map { case (a, f) => f.map(foldOperators(a, _)).getOrElse(a) }
  }


  val withEquals: P[FilterAST] =
    P(binaryOperators[FilterAST](
      binaryOperators[FilterAST](
        ensequenced, "==" -> dsl.equal _), "!=" -> ((f: FilterAST, s: FilterAST) => dsl.compose(dsl.equal(f, s), dsl.not))
    ))

  val ensequenced: P[FilterAST] =
    P(binaryOperators[FilterAST](piped, "," -> dsl.ensequence _))

  val piped: P[FilterAST] =
    P(binaryOperators[FilterAST](expr, "|" -> dsl.compose _))

  val expr: P[FilterAST] =
    P(binaryOperators[FilterAST](term, "+" -> dsl.add _, "-" -> dsl.subtract))

  val term: P[FilterAST] =
    P(binaryOperators[FilterAST](factor, "*" -> dsl.multiply _, "/" -> dsl.divide _, "%" -> dsl.modulo _))

  val factor: P[FilterAST] =
    P(("(" ~/ whitespace ~ withEquals ~ whitespace ~ ")") | asBinding | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[FilterAST] = P(
    for {
      f <- withEquals
      fun <- "?".!.?.map(_.fold(identity[FilterAST] _)(_ => dsl.silence))
    } yield fun(f)
  )

  val arguments: P[Vector[String]] = P(
    "(" ~/ whitespace ~ filterIdentifier.rep(min = 1, sep = whitespace ~ ";" ~/ whitespace) ~ whitespace ~ ")"
  )

  val definition: P[Definition[FilterAST]] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Vector.empty)) ~ ":" ~/ whitespace ~ filter ~ ";") map (Definition[FilterAST] _).tupled
  )

  val program: P[Program[FilterAST]] = P(
    (whitespace ~
      (definition.rep(min = 1, sep = whitespace) ~ whitespace)
        .?.map(_.getOrElse(Vector.empty)) ~
      filter ~ whitespace ~ parsers.Terminals.End())
      .map((Program[FilterAST] _).tupled)
  )

}
