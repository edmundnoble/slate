package qq

import fastparse.Implicits

import scalaz.\/
import matryoshka._
import FunctorT.ops._
import fastparse.parsers.Combinators

object QQParser {

  import QQAST._
  import fastparse.all._
  import fastparse.parsers

  import scalaz.Monad
  import scala.collection.mutable
  import scalaz.syntax.std.option._
  import scalaz.syntax.either._

  implicit object ParserMonad extends Monad[Parser] {
    override def point[A](a: => A): Parser[A] =
      parsers.Terminals.Pass.map(_ => a)
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
  val fetch: P[FetchApi.type] = P("fetch") map (_ => FetchApi)
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)
  val stringLiteral: P[String] = P(CharsWhile(isStringLiteralChar).!)
  val escapedStringLiteral: P[String] = P(quote ~/ CharsWhile(c => isStringLiteralChar(c) || c == ' ' || c == '\t').! ~ quote)
  val whitespace: P0 = P(CharsWhile(_ == ' '))

  val numericLiteral: P[Int] = CharsWhile(Character.isDigit).! map ((_: String).toInt)

  def selectKey[A]: P[QQProgram] = P((escapedStringLiteral |
    stringLiteral) map QQFilter.selectKey)
  def selectIndex[A]: P[QQProgram] = P(
    for {
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
      number <- numericLiteral
    } yield QQFilter.selectIndex(fun(number))
  )
  def selectRange[A]: P[QQProgram] = P((numericLiteral ~ ":" ~/ numericLiteral) map (QQFilter.selectRange _).tupled)

  def dottableSimpleFilter[A]: P[QQProgram] = P(
    Combinators.Either[QQProgram](
    "[" ~/ Combinators.Either[QQProgram](escapedStringLiteral map QQFilter.selectKey, selectRange, selectIndex) ~ "]",
      selectKey, selectIndex)
  )
  def dottableFilter[A]: P[QQProgram] =
    P(
      for {
        s <- dottableSimpleFilter
        f <- "[]".!.?.map(_.cata(_ => QQFilter.collectResults _, identity[QQProgram] _))
      } yield f(s)
    )

  def dottedFilter[A]: P[QQProgram] =
    P(dot ~ (wspStr("[]")
      .map(_ => QQFilter.collectResults(QQFilter.id)) | dottableFilter)
      .rep(sep = dot)
      .map(_.foldLeft[QQProgram](QQFilter.id)(QQFilter.compose)))

  private def filterIdentifier[A]: P[String] = CharsWhile(Character.isAlphabetic(_)).!

  def callFilter[A]: P[QQProgram] = P(filterIdentifier map QQFilter.call)

  def smallFilter[A]: P[QQProgram] = P(dottedFilter | callFilter)

  def pipedFilter[A]: P[QQProgram] = P((smallFilter | ("(" ~/ filter ~ ")")).rep(sep = whitespace ~ "|" ~ whitespace, min = 1).map(_.reduceLeft(QQFilter.compose)))

  def ensequencedFilters[A]: P[QQProgram] =
    P(pipedFilter.rep(min = 1, sep = P("," ~ whitespace)).map(EnsequenceFilters(_).embed))

  def enlistedFilter[A]: P[QQProgram] =
    P(("[" ~/ ensequencedFilters ~ "]").map(EnlistFilter(_).embed))

  def enjectPair[A]: P[(String \/ QQProgram, QQProgram)] =
    P((("(" ~/ smallFilter ~ ")").map(_.right[String]) | filterIdentifier.map(_.left[QQProgram])) ~ ":" ~ whitespace ~ filter)

  def enjectedFilter[A]: P[QQProgram] =
    P("{" ~/ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(EnjectFilters(_).embed) ~ "}")

  def arguments[A]: P[List[String]] =
    P("(" ~ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~ whitespace) ~ ")")

  def definition[A]: P[Definition] =
    P("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~ whitespace ~ filter ~ ";" map (Definition.apply _).tupled)

  def filter[A]: P[QQProgram] = P(enlistedFilter | ensequencedFilters | enjectedFilter)

  def program[A]: P[(List[Definition], QQProgram)] = (definition.rep(sep = whitespace) ~ whitespace).?.map(_.getOrElse(Nil)) ~ filter

}
