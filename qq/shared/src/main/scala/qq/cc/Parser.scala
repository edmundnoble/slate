package qq
package cc

import cats.Monad
import fastparse.all._
import fastparse.all.ParseCtx
import fastparse.parsers.Terminals
import fastparse.core.Implicits, fastparse.parsers
import qq.data._

import scala.collection.mutable
import cats.implicits._


object Parser {

  implicit object ParserMonad extends Monad[Parser] {
    override def pure[A](a: A): Parser[A] =
      parsers.Transformers.Mapper(parsers.Terminals.Pass(), (_: Unit) => a)

    override def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      parsers.Transformers.FlatMapped(fa, f)

    // TODO: Can I make this work?
    override def tailRecM[A, B](a: A)(f: (A) => Parser[Either[A, B]]): Parser[B] = ???
  }

  implicit def listRepeater[T]: Implicits.Repeater[T, List[T]] = new Implicits.Repeater[T, List[T]] {
    type Acc = mutable.ListBuffer[T]

    def initial: Acc = new mutable.ListBuffer[T]()

    def accumulate(t: T, acc: Acc): Unit = {
      val _ = acc += t
    }

    def result(acc: Acc): List[T] = acc.result()
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
      ((Terminals.ElemLiteral('\\') ~/ (Terminals.ElemLiteral('n').map(_ => "\n") | Terminals.ElemLiteral('"').map(_ => "\"") | Terminals.ElemLiteral('\\').!)) |
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

  val pathComponent: P[List[PathComponent]] = P(
    for {
      s <- simplePathComponent
      f <- "[]".!.?.map(_.fold(List.empty[PathComponent])(_ => dsl.collectResults :: Nil))
    } yield s :: f
  )

  val fullPath: P[List[PathComponent]] = P(
    dot ~
      ((LiteralStr("[]").as(dsl.collectResults :: Nil)) | pathComponent)
        .rep(sep = dot)
        .map(_.nelFoldLeft1(Nil)(_ ++ _))
  )

  def setPathFilter(path: List[PathComponent]): P[ConcreteFilter] = P(
    ("=" ~ whitespace ~ filter).map(dsl.setPath(path, _))
  )

  def modifyPathFilter(path: List[PathComponent]): P[ConcreteFilter] = P(
    ("|=" ~ whitespace ~ filter).map(dsl.modifyPath(path, _))
  )

  private val paths =
    for {
      path <- fullPath
      f <- (whitespace ~ (setPathFilter(path) | modifyPathFilter(path))) | Terminals.Pass().map(_ => dsl.getPath(path))
    } yield f

  private val filterIdentifier: P[String] = P(
    stringLiteral
  )

  private val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val asBinding: P[ConcreteFilter] = P(
    for {
      variable <- variableIdentifier
      _ <- whitespace ~ LiteralStr("as") ~ whitespace
      as <- filter
      _ <- whitespace ~ LiteralStr("in") ~ whitespace
      in <- filter
    } yield dsl.asBinding(variable, as, in)
  )

  val callFilter: P[ConcreteFilter] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Nil))
    } yield dsl.call(identifier, params)
  )

  val constInt: P[ConcreteFilter] = P(numericLiteral map (dsl.constNumber(_)))

  val constString: P[ConcreteFilter] = P(escapedStringLiteral map dsl.constString)

  val constBoolean: P[ConcreteFilter] = P((LiteralStr("true").map(_ => true) | LiteralStr("false").map(_ => false)).map(dsl.constBoolean))

  val dereference: P[ConcreteFilter] = P(variableIdentifier.map(dsl.deref))

  val smallFilter: P[ConcreteFilter] = P(constInt | constString | paths | dereference | callFilter)

  val enlistedFilter: P[ConcreteFilter] = P(
    "[" ~/ filter.map(dsl.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String Either ConcreteFilter, ConcreteFilter)] = P(
    ((("(" ~/ whitespace ~ filter ~ whitespace ~ ")").map(Right[String, ConcreteFilter]) |
      (stringLiteral | escapedStringLiteral).map(Left[String, ConcreteFilter])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => Left(id) -> dsl.getPath(List(dsl.selectKey(id))))
  )

  val enjectedFilter: P[ConcreteFilter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(dsl.enject) ~ whitespace ~ "}"
  )

  // binary operators with the same precedence level
  def binaryOperators[A](rec: P[A], ops: (String, (A, A) => A)*): P[A] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] = LiteralStr(text).as(function)

    def foldOperators(begin: A, operators: List[((A, A) => A, A)]): A =
      operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }

    val op = ops.map((makeParser _).tupled).reduceLeft(_ | _)
    (rec ~ (whitespace ~ (op ~/ whitespace ~ rec ~ whitespace).rep(min = 1)).?).map { case (a, f) => f.map(foldOperators(a, _)).getOrElse(a) }
  }


  val withEquals: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](
      binaryOperators[ConcreteFilter](
        ensequenced, "==" -> dsl.equal _), "!=" -> ((f: ConcreteFilter, s: ConcreteFilter) => dsl.compose(dsl.equal(f, s), dsl.not))
    ))

  val ensequenced: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](piped, "," -> dsl.ensequence _))

  val piped: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](expr, "|" -> dsl.compose _))

  val expr: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](term, "+" -> dsl.add _, "-" -> dsl.subtract))

  val term: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](factor, "*" -> dsl.multiply _, "/" -> dsl.divide _, "%" -> dsl.modulo _))

  val factor: P[ConcreteFilter] =
    P(("(" ~/ whitespace ~ withEquals ~ whitespace ~ ")") | asBinding | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[ConcreteFilter] = P(
    for {
      f <- withEquals
      fun <- "?".!.?.map(_.fold(identity[ConcreteFilter] _)(_ => dsl.silence))
    } yield fun(f)
  )

  val arguments: P[List[String]] = P(
    "(" ~/ whitespace ~ filterIdentifier.rep(min = 1, sep = whitespace ~ ";" ~/ whitespace) ~ whitespace ~ ")"
  )

  val definition: P[Definition[ConcreteFilter]] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~/ whitespace ~ filter ~ ";") map (Definition[ConcreteFilter] _).tupled
  )

  val program: P[Program[ConcreteFilter]] = P(
    (whitespace ~
      (definition.rep(min = 1, sep = whitespace) ~ whitespace)
        .?.map(_.getOrElse(Nil)) ~
      filter ~ whitespace ~ Terminals.End())
      .map((Program[ConcreteFilter] _).tupled)
  )

}
