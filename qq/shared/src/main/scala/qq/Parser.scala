package qq

import fastparse.all._
import fastparse.parsers.Terminals
import fastparse.{Implicits, Logger, parsers}
import qq.FilterComponent.SilenceExceptions

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
    def accumulate(t: T, acc: Acc): Unit = acc += t
    def result(acc: Acc): List[T] = acc.result()
  }

  val dot: P0 = P(".")
  val fetch: P[Filter] = P("fetch") map (_ => Filter.fetch)
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)
  val stringLiteral: P[String] = P(CharsWhile(isStringLiteralChar).!)

  val escapedStringLiteral: P[String] = P(
    quote ~/
      ((wspStr("""\n""") >| "\n") | (wspStr("""\\""") >| "\\") | CharPred(ch => isStringLiteralChar(ch) || ch == '↪' || ch == ' ' || ch == '+' || ch == '*' || ch == '-' || ch == '\t').!).rep.map(_.mkString) ~
      quote
  )

  val whitespace: P0 = P(CharsWhile(ch => ch == ' ' || ch == '\n' || ch == '\t').?)

  val numericLiteral: P[Int] = P(
    CharsWhile(Character.isDigit).! map ((_: String).toInt)
  )

  val doubleLiteral: P[Double] = P(
    // TODO: fix
    CharsWhile(Character.isDigit).! map ((_: String).toInt)
  )

  val selectKey: P[Filter] = P(
    (escapedStringLiteral | stringLiteral) map Filter.selectKey
  )

  val selectIndex: P[Filter] = P(
    for {
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
      number <- numericLiteral
    } yield Filter.selectIndex(fun(number))
  )

  val selectRange: P[Filter] = P(
    (numericLiteral ~ ":" ~/ numericLiteral) map (Filter.selectRange _).tupled
  )

  val dottableSimpleFilter: P[Filter] = P(
    ("[" ~/ ((escapedStringLiteral map Filter.selectKey) | selectRange | selectIndex) ~ "]") | selectKey | selectIndex
  )

  val dottableFilter: P[Filter] = P(
    for {
      s <- dottableSimpleFilter
      f <- "[]".!.?.map(_.cata(_ => Filter.collectResults _, identity[Filter] _))
    } yield f(s)
  )

  val dottedFilter: P[Filter] = P(
    dot ~
      (wspStr("[]") >| Filter.collectResults(Filter.id) | dottableFilter)
        .rep(sep = dot)
        .map(_.foldLeft[Filter](Filter.id)(Filter.compose))
  )

  private val filterIdentifier: P[String] = P(
    CharsWhile(Character.isAlphabetic(_)).!
  )

  val callFilter: P[Filter] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Nil))
    } yield Filter.call(identifier, params)
  )

  val constInt: P[Filter] = P(numericLiteral map (Filter.constNumber(_)))
  val constString: P[Filter] = P(escapedStringLiteral map Filter.constString)

  val smallFilter: P[Filter] = P(constInt | constString | dottedFilter | callFilter)

  val enlistedFilter: P[Filter] = P(
    "[" ~/ filter.map(Filter.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String \/ Filter, Filter)] = P(
    ((("(" ~/ filter ~ ")").map(_.right[String]) |
      filterIdentifier.map(_.left[Filter])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => -\/(id) -> Filter.selectKey(id))
  )

  val enjectedFilter: P[Filter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(Filter.enject) ~ whitespace ~ "}"
  )

  def operator[A](rec: P[A], op1: (String, (A, A) => A), ops: (String, (A, A) => A)*): P[A] = {
    def makeParser(text: String, function: (A, A) => A): P[(A, A) => A] = wspStr(text) >| function
    def foldOperators(begin: A, operators: List[((A, A) => A, A)]): A =
      operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }
    val op = NonEmptyList(op1, ops: _*).map((makeParser _).tupled).foldLeft1(_ | _)
    (rec ~ whitespace ~ (op ~ whitespace ~ rec).rep).map((foldOperators _).tupled)
  }

  val sequenced: P[Filter] =
    P(operator[Filter](piped, "," -> Filter.ensequence _))

  val piped: P[Filter] =
    P(operator[Filter](expr, "|" -> Filter.compose _))

  val expr: P[Filter] =
    P(operator[Filter](term, "+" -> Filter.add _, "-" -> Filter.subtract))

  val term: P[Filter] =
    P(operator[Filter](factor, "*" -> Filter.multiply _, "/" -> Filter.divide _, "%" -> Filter.modulo _))

  val factor: P[Filter] =
    P(("(" ~ sequenced ~ ")") | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[Filter] = P(
    for {
      f <- sequenced
      fun <- "?".!.?.map(_.fold(identity[Filter] _)(_ => Filter.silence))
    } yield fun(f)
  )

  val arguments: P[List[String]] = P(
    "(" ~ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~ whitespace) ~ ")"
  )

  val definition: P[Definition] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~ whitespace ~ filter ~ ";") map (Definition(_, _, _)).tupled
  )

  val program: P[(List[Definition], Filter)] = P(
    (definition.rep(sep = whitespace) ~ whitespace).?.map(_.getOrElse(Nil)) ~ filter ~ Terminals.End
  )

}
