package qq

import fastparse.all._
import fastparse.parsers.Terminals
import fastparse.{Implicits, parsers}

import scalaz.{-\/, Monad, NonEmptyList, \/}
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.foldable1._
import scalaz.syntax.std.option._
import scala.collection.mutable

object Parser {

  implicit object ParserMonad extends Monad[Parser] {
    override def point[A](a: => A): Parser[A] =
      parsers.Transformers.Mapper(parsers.Terminals.Pass, (_: Unit) => a)

    override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      parsers.Transformers.FlatMapped(fa, f)
  }

  implicit def listRepeater[T]: Implicits.Repeater[T, List[T]] = new Implicits.Repeater[T, List[T]] {
    type Acc = mutable.ListBuffer[T]

    def initial: Acc = new mutable.ListBuffer[T]()

    def accumulate(t: T, acc: Acc): Unit = {
      val _ = acc += t
    }

    def result(acc: Acc): List[T] = acc.result()
  }

  val dot: P0 = P(".")
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)

  val stringLiteralChars = Seq(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), "↪+*-_": Seq[Char])
  val stringLiteralChar = CharIn(stringLiteralChars: _*)
  val stringLiteral: P[String] = P(stringLiteralChar.rep(min = 1).!)

  val whitespaceChars: String = " \n\t"
  val whitespace: P0 = P(CharIn(whitespaceChars.toSeq).rep.map(_ => ()))
  val escapedStringLiteralChars = stringLiteralChars :+ ("$(),.:/": Seq[Char]) :+ whitespaceChars.toSeq

  val escapedStringLiteral: P[String] = P(
    quote ~/
      ((Terminals.CharLiteral('\\') ~/ (Terminals.CharLiteral('n').map(_ => "\n") | Terminals.CharLiteral('\\').!)) |
        CharIn(escapedStringLiteralChars: _*).!).rep.map(_.mkString) ~ quote
  )

  val numericLiteral: P[Int] = P(
    CharIn('0' to '9').rep(min = 1).!.map(_.toInt)
  )

  val doubleLiteral: P[Double] = P(
    // TODO: fix
    numericLiteral.map(_.toDouble)
  )

  val selectKey: P[ConcreteFilter] = P(
    (escapedStringLiteral | stringLiteral) map FilterDSL.fix.selectKey
  )

  val selectIndex: P[ConcreteFilter] = P(
    for {
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
      number <- numericLiteral
    } yield FilterDSL.fix.selectIndex(fun(number))
  )

  val selectRange: P[ConcreteFilter] = P(
    (numericLiteral ~ ":" ~/ numericLiteral) map (FilterDSL.fix.selectRange _).tupled
  )

  val dottableSimpleFilter: P[ConcreteFilter] = P(
    ("[" ~/ ((escapedStringLiteral map FilterDSL.fix.selectKey) | selectRange | selectIndex) ~ "]") | selectKey | selectIndex
  )

  val dottableFilter: P[ConcreteFilter] = P(
    for {
      s <- dottableSimpleFilter
      f <- "[]".!.?.map(_.cata(_ => FilterDSL.fix.collectResults _, identity[ConcreteFilter] _))
    } yield f(s)
  )

  val dottedFilter: P[ConcreteFilter] = P(
    dot ~
      (wspStr("[]") >| FilterDSL.fix.collectResults(FilterDSL.fix.id) | dottableFilter)
        .rep(sep = dot)
        .map(_.foldLeft[ConcreteFilter](FilterDSL.fix.id)(FilterDSL.fix.compose))
  )

  private val filterIdentifier: P[String] = P(
    //    CharIn('a' to 'z', 'A' to 'Z').rep(min = 1).!
    stringLiteral
  )

  private val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val letAsBinding: P[ConcreteFilter] = P(
    for {
      _ <- wspStr("let") ~ whitespace
      variable <- variableIdentifier
      _ <- whitespace ~ wspStr("as") ~ whitespace
      as <- filter
      _ <- whitespace ~ wspStr("in") ~ whitespace
      in <- filter
    } yield FilterDSL.fix.letAsBinding(variable, as, in)
  )

  val callFilter: P[ConcreteFilter] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Nil))
    } yield FilterDSL.fix.call(identifier, params)
  )

  val constInt: P[ConcreteFilter] = P(numericLiteral map (FilterDSL.fix.constNumber(_)))

  val constString: P[ConcreteFilter] = P(escapedStringLiteral map FilterDSL.fix.constString)

  val dereference: P[ConcreteFilter] = P(variableIdentifier.map(FilterDSL.fix.deref))

  val smallFilter: P[ConcreteFilter] = P(constInt | constString | dottedFilter | dereference | callFilter)

  val enlistedFilter: P[ConcreteFilter] = P(
    "[" ~/ filter.map(FilterDSL.fix.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String \/ ConcreteFilter, ConcreteFilter)] = P(
    ((("(" ~/ filter ~ ")").map(_.right[String]) |
      filterIdentifier.map(_.left[ConcreteFilter])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => -\/(id) -> FilterDSL.fix.selectKey(id))
  )

  val enjectedFilter: P[ConcreteFilter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(FilterDSL.fix.enject) ~ whitespace ~ "}"
  )

  // binary operators with the same precedence level
  def binaryOperators[A](rec: P[A], op1: (String, (A, A) => A), ops: (String, (A, A) => A)*): P[A] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] = wspStr(text) >| function

    def foldOperators(begin: A, operators: List[((A, A) => A, A)]): A =
      operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }

    val op = NonEmptyList(op1, ops: _*).map((makeParser _).tupled).foldLeft1(_ | _)
    (rec ~ whitespace ~ (op ~/ whitespace ~ rec).rep).map((foldOperators _).tupled)
  }

  val sequenced: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](piped, "," -> FilterDSL.fix.ensequence _))

  val piped: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](expr, "|" -> FilterDSL.fix.compose _))

  val expr: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](term, "+" -> FilterDSL.fix.add _, "-" -> FilterDSL.fix.subtract))

  val term: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](factor, "*" -> FilterDSL.fix.multiply _, "/" -> FilterDSL.fix.divide _, "%" -> FilterDSL.fix.modulo _))

  val factor: P[ConcreteFilter] =
    P(("(" ~/ sequenced ~ ")") | letAsBinding | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[ConcreteFilter] = P(
    for {
      f <- sequenced
      fun <- "?".!.?.map(_.fold(identity[ConcreteFilter] _)(_ => FilterDSL.fix.silence))
    } yield fun(f)
  )

  val arguments: P[List[String]] = P(
    "(" ~/ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~/ whitespace) ~ ")"
  )

  val definition: P[Definition[ConcreteFilter]] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~/ whitespace ~ filter ~ ";") map (Definition[ConcreteFilter](_, _, _)).tupled
  )

  val program: P[Program[ConcreteFilter]] = P(
    (whitespace ~
      (definition.rep(min = 1, sep = whitespace) ~ whitespace)
        .?.map(_.map(_.toDefinitions).getOrElse(Map.empty)) ~
      filter ~ Terminals.End)
      .map((Program.apply[ConcreteFilter] _).tupled)
  )

}
