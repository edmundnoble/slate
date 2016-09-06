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
  val escapedStringLiteralChars = stringLiteralChars :+ ("(),.:/": Seq[Char]) :+ whitespaceChars.toSeq

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

  val selectKey: P[Filter] = P(
    (escapedStringLiteral | stringLiteral) map FilterDSL.selectKey
  )

  val selectIndex: P[Filter] = P(
    for {
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
      number <- numericLiteral
    } yield FilterDSL.selectIndex(fun(number))
  )

  val selectRange: P[Filter] = P(
    (numericLiteral ~ ":" ~/ numericLiteral) map (FilterDSL.selectRange _).tupled
  )

  val dottableSimpleFilter: P[Filter] = P(
    ("[" ~/ ((escapedStringLiteral map FilterDSL.selectKey) | selectRange | selectIndex) ~ "]") | selectKey | selectIndex
  )

  val dottableFilter: P[Filter] = P(
    for {
      s <- dottableSimpleFilter
      f <- "[]".!.?.map(_.cata(_ => FilterDSL.collectResults _, identity[Filter] _))
    } yield f(s)
  )

  val dottedFilter: P[Filter] = P(
    dot ~
      (wspStr("[]") >| FilterDSL.collectResults(FilterDSL.id) | dottableFilter)
        .rep(sep = dot)
        .map(_.foldLeft[Filter](FilterDSL.id)(FilterDSL.compose))
  )

  private val filterIdentifier: P[String] = P(
//    CharIn('a' to 'z', 'A' to 'Z').rep(min = 1).!
    stringLiteral
  )

  private lazy val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val callFilter: P[Filter] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Nil))
    } yield FilterDSL.call(identifier, params)
  )

  val constInt: P[Filter] = P(numericLiteral map (FilterDSL.constNumber(_)))
  val constString: P[Filter] = P(escapedStringLiteral map FilterDSL.constString)

  val smallFilter: P[Filter] = P(constInt | constString | dottedFilter | callFilter)

  val enlistedFilter: P[Filter] = P(
    "[" ~/ filter.map(FilterDSL.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String \/ Filter, Filter)] = P(
    ((("(" ~/ filter ~ ")").map(_.right[String]) |
      filterIdentifier.map(_.left[Filter])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => -\/(id) -> FilterDSL.selectKey(id))
  )

  val enjectedFilter: P[Filter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(FilterDSL.enject) ~ whitespace ~ "}"
  )

  // binary operators with the same precedence level
  def binaryOperators[A](rec: P[A], op1: (String, (A, A) => A), ops: (String, (A, A) => A)*): P[A] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] = wspStr(text) >| function
    def foldOperators(begin: A, operators: List[((A, A) => A, A)]): A =
      operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }
    val op = NonEmptyList(op1, ops: _*).map((makeParser _).tupled).foldLeft1(_ | _)
    (rec ~ whitespace ~ (op ~/ whitespace ~ rec).rep).map((foldOperators _).tupled)
  }

  val sequenced: P[Filter] =
    P(binaryOperators[Filter](piped, "," -> FilterDSL.ensequence _))

  val piped: P[Filter] =
    P(binaryOperators[Filter](expr, "|" -> FilterDSL.compose _))

  val expr: P[Filter] =
    P(binaryOperators[Filter](term, "+" -> FilterDSL.add _, "-" -> FilterDSL.subtract))

  val term: P[Filter] =
    P(binaryOperators[Filter](factor, "*" -> FilterDSL.multiply _, "/" -> FilterDSL.divide _, "%" -> FilterDSL.modulo _))

  val factor: P[Filter] =
    P(("(" ~/ sequenced ~ ")") | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[Filter] = P(
    for {
      f <- sequenced
      fun <- "?".!.?.map(_.fold(identity[Filter] _)(_ => FilterDSL.silence))
    } yield fun(f)
  )

  val arguments: P[List[String]] = P(
    "(" ~/ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~/ whitespace) ~ ")"
  )

  val definition: P[Definition] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~/ whitespace ~ filter ~ ";") map (Definition(_, _, _)).tupled
  )

  val program: P[Program] = P(
    (whitespace ~ (definition.rep(min = 1, sep = whitespace) ~ whitespace).?.map(_.map(_.toVector).getOrElse(Vector.empty)) ~ filter ~ Terminals.End).map((Program.apply _).tupled)
  )

}
