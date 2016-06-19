package qq

import fastparse.all._
import fastparse.parsers
import fastparse.Implicits

import scalaz.{-\/, \/}
import scalaz.Monad
import scalaz.syntax.either._
import scalaz.syntax.monad._
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
    quote ~/ CharsWhile(c => isStringLiteralChar(c) || c == ' ' || c == '\t').! ~ quote
  )
  val whitespace: P0 = P(CharsWhile(_ == ' ').?)

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
      (wspStr("[]").map(_ => Filter.collectResults(Filter.id)) | dottableFilter)
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

  val constInt: P[Filter] = P(numericLiteral map (d => Filter.constNumber(d)))
  val constString: P[Filter] = P("\"" ~/ (stringLiteral map Filter.constString) ~ "\"")

  val smallFilter: P[Filter] = P(constInt | constString | dottedFilter | callFilter)

  val pipedFilter: P[Filter] = P(
    (smallFilter | ("(" ~/ filter ~ ")"))
      .rep(sep = whitespace ~ "|" ~ whitespace, min = 1)
      .map(_.reduceLeft(Filter.compose))
  )

  val ensequencedFilters: P[Filter] = P(
    for {
      first <- pipedFilter
      f <- ("," ~ whitespace ~ pipedFilter.rep(min = 1, sep = P("," ~ whitespace))).?.map(fs => (f: Filter) => fs.fold(f)(l => Filter.ensequence(f :: l)))
    } yield f(first)
  )

  val enlistedFilter: P[Filter] = P(
    "[" ~/ ensequencedFilters.map(Filter.enlist) ~ "]"
  )

  val enjectPair: P[(String \/ Filter, Filter)] = P(
    ((("(" ~/ filter ~ ")").map(_.right[String]) |
      filterIdentifier.map(_.left[Filter])) ~ ":" ~ whitespace ~ filter) |
      filterIdentifier.map(id => -\/(id) -> Filter.selectKey(id))
  )

  val enjectedFilter: P[Filter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(Filter.enject) ~ whitespace ~ "}"
  )

  val arguments: P[List[String]] = P(
    "(" ~ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~ whitespace) ~ ")"
  )

  val definition: P[Definition] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~ whitespace ~ filter ~ ";") map {
      Definition.tupled
    }
  )

  val basicFilter: P[Filter] = P(
    enlistedFilter | ensequencedFilters | enjectedFilter
  )

  def foldOperators[T](begin: T, operators: List[((T, T) => T, T)]): T =
    operators.foldLeft(begin) { case (f, (combFun, nextF)) => combFun(f, nextF) }

  val expr: P[Filter] =
    P((term ~ whitespace ~
      (
        ((wspStr("+") >| Filter.add _) |
          (wspStr("-") >| Filter.subtract _)) ~ whitespace ~ term
        )
        .rep
      ).map((foldOperators[Filter] _).tupled))

  val term: P[Filter] =
    P((factor ~ whitespace ~
      (
        ((wspStr("*") >| Filter.multiply _) |
          (wspStr("/") >| Filter.divide _) |
          (wspStr("%") >| Filter.modulo _)) ~ whitespace ~ factor)
        .rep
      ).map((foldOperators[Filter] _).tupled))

  val factor: P[Filter] = P(("(" ~ expr ~ ")") | basicFilter)

  val filter: P[Filter] = P(
    expr
  )

  val program: P[(List[Definition], Filter)] = P(
    (definition.rep(sep = whitespace) ~ whitespace).?.map(_.getOrElse(Nil)) ~ filter
  )

}
