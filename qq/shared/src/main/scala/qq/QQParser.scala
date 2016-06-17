package qq

import fastparse.{Implicits, Logger}

import scalaz.{-\/, Applicative, \/}
import matryoshka._
import fastparse.parsers.Combinators
import qq.Definition
import qq.QQFilterComponent.MultiplyFilters
import shapeless.ops.nat.ToInt
import scalaz.syntax.monad._
import shapeless.{Nat, Sized}

object QQParser {

  import fastparse.all._
  import fastparse.parsers

  import scalaz.Monad
  import scala.collection.mutable
  import scalaz.syntax.std.option._
  import scalaz.syntax.either._
  import scalaz.syntax.monad._

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
  val fetch: P[QQFilter] = P("fetch") map (_ => QQFilter.fetch)
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

  val selectKey: P[QQFilter] = P(
    (escapedStringLiteral | stringLiteral) map QQFilter.selectKey
  )

  val selectIndex: P[QQFilter] = P(
    for {
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
      number <- numericLiteral
    } yield QQFilter.selectIndex(fun(number))
  )

  val selectRange: P[QQFilter] = P(
    (numericLiteral ~ ":" ~/ numericLiteral) map (QQFilter.selectRange _).tupled
  )

  val dottableSimpleFilter: P[QQFilter] = P(
    ("[" ~/ ((escapedStringLiteral map QQFilter.selectKey) | selectRange | selectIndex) ~ "]") | selectKey | selectIndex
  )

  val dottableFilter: P[QQFilter] = P(
    for {
      s <- dottableSimpleFilter
      f <- "[]".!.?.map(_.cata(_ => QQFilter.collectResults _, identity[QQFilter] _))
    } yield f(s)
  )

  val dottedFilter: P[QQFilter] = P(
    dot ~
      (wspStr("[]").map(_ => QQFilter.collectResults(QQFilter.id)) | dottableFilter)
        .rep(sep = dot)
        .map(_.foldLeft[QQFilter](QQFilter.id)(QQFilter.compose))
  )

  private val filterIdentifier: P[String] = P(
    CharsWhile(Character.isAlphabetic(_)).!
  )

  val callFilter: P[QQFilter] = P(
    for {
      identifier <- filterIdentifier
      params <- ("(" ~/ whitespace ~ filter.rep(min = 1, sep = whitespace ~ ";" ~ whitespace) ~ whitespace ~ ")").?.map(_.getOrElse(Nil))
    } yield QQFilter.call(identifier, params)
  )

  val smallFilter: P[QQFilter] = P(dottedFilter | callFilter)

  val pipedFilter: P[QQFilter] = P(
    (smallFilter | ("(" ~/ filter ~ ")"))
      .rep(sep = whitespace ~ "|" ~ whitespace, min = 1)
      .map(_.reduceLeft(QQFilter.compose))
  )

  val ensequencedFilters: P[QQFilter] = P(
    for {
      first <- pipedFilter
      f <- ("," ~ whitespace ~ pipedFilter.rep(min = 1, sep = P("," ~ whitespace))).?.map(fs => (f: QQFilter) => fs.fold(f)(l => QQFilter.ensequence(f :: l)))
    } yield f(first)
  )

  val enlistedFilter: P[QQFilter] = P(
    "[" ~/ ensequencedFilters.map(QQFilter.enlist) ~ "]"
  )

  val enjectPair: P[(String \/ QQFilter, QQFilter)] = P(
    ((("(" ~/ smallFilter ~ ")").map(_.right[String]) |
      filterIdentifier.map(_.left[QQFilter])) ~ ":" ~ whitespace ~ filter) |
      filterIdentifier.map(id => -\/(id) -> QQFilter.selectKey(id))
  )

  val enjectedFilter: P[QQFilter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(QQFilter.enject) ~ whitespace ~ "}"
  )

  val arguments: P[List[String]] = P(
    "(" ~ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~ whitespace) ~ ")"
  )

  val definition: P[Definition] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~ whitespace ~ filter ~ ";") map {
      Definition.tupled
    }
  )

  val constInt: P[QQFilter] = P(numericLiteral map (d => QQFilter.constNumber(d)))
  val constString: P[QQFilter] = P("\"" ~ (stringLiteral map QQFilter.constString) ~ "\"")

  val basicFilter: P[QQFilter] = P(
    enlistedFilter | ensequencedFilters | enjectedFilter | constInt | constString
  )

  val expr: P[QQFilter] =
    P((term ~ whitespace ~
      (
        ((wspStr("+") >| QQFilter.addFilters _) |
          (wspStr("-") >| QQFilter.subtractFilters _)) ~ whitespace ~ term
        )
        .rep
      ).map { l => l._2.foldLeft(l._1) { case (f, (combFun, nextF)) => combFun(f, nextF) } })
  val term: P[QQFilter] =
    P((factor ~ whitespace ~
      (
        ((wspStr("*") >| QQFilter.multiplyFilters _) |
          (wspStr("/") >| QQFilter.divideFilters _) |
          (wspStr("%") >| QQFilter.moduloFilters _)) ~ whitespace ~ factor)
        .rep
      ).map(l => l._2.foldLeft(l._1) { case (f, (combFun, nextF)) => combFun(f, nextF) }))
  val factor: P[QQFilter] = P(("(" ~ expr ~ ")") | basicFilter)

  val filter: P[QQFilter] = P(
    expr
  )

  val program: P[(List[Definition], QQFilter)] = P(
    (definition.rep(sep = whitespace) ~ whitespace).?.map(_.getOrElse(Nil)) ~ filter
  )

}
