package edmin

import upickle.Js

import scala.scalajs.js.UndefOr

object QQ {
  object QQAST {
    sealed trait QQFilter extends Any
    case object IdFilter extends QQFilter
    case object FetchApi extends QQFilter
    case class ComposeFilters(first: QQFilter, second: QQFilter) extends QQFilter
    case class SilenceExceptions(f: QQFilter) extends AnyVal with QQFilter
    case class EnlistFilter(f: QQFilter) extends AnyVal with QQFilter
    case class EnsequenceFilters(filters: Seq[QQFilter]) extends AnyVal with QQFilter
    case class SelectKey(key: String) extends AnyVal with QQFilter
    case class SelectIndex(index: Int) extends AnyVal with QQFilter
    case class SelectRange(start: Int, end: Int) extends QQFilter
    case class CollectResults(f: QQFilter) extends AnyVal with QQFilter
  }

  object QQParser {

    import QQAST._

    import fastparse.all._
    import fastparse.parsers

    import scalaz.Monad

    implicit object ParserMonad extends Monad[Parser] {
      override def point[A](a: => A): Parser[A] =
        parsers.Terminals.Pass.map(_ => a)
      override def bind[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
        parsers.Transformers.FlatMapped(fa, f)
    }

    val dot: P0 = P(".")
    val fetch: P[FetchApi.type] = P("fetch") map (_ => FetchApi)
    val quote: P0 = P("\"")

    def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)
    val stringLiteral: P[String] = P(CharsWhile(isStringLiteralChar).!)
    val escapedStringLiteral: P[String] = P(quote ~/ CharsWhile(c => isStringLiteralChar(c) || c == ' ').! ~ quote)

    val numericLiteral: P[Int] = CharsWhile(Character.isDigit).! map ((_: String).toInt)

    val selectKey: P[SelectKey] = P(escapedStringLiteral |
      stringLiteral) map SelectKey
    val selectIndex: P[SelectIndex] = P(numericLiteral) map SelectIndex
    val selectRange: P[SelectRange] = P(numericLiteral ~ ":" ~/ numericLiteral) map SelectRange.tupled

    val dottableSimpleFilter: P[QQFilter] = P(
      ("[" ~/ ((escapedStringLiteral map SelectKey) | selectRange | selectIndex) ~ "]") |
        selectKey | selectIndex
    )
    val dottableFilter: P[QQFilter] =
      for {
        s <- dottableSimpleFilter
        f <- "[]".!.?.map(_.map(_ => CollectResults.apply _))
      } yield f.getOrElse(identity[QQFilter] _)(s)

    val dottedFilter: P[QQFilter] = dot ~ dottableFilter.rep(sep = dot).map(_.foldLeft[QQFilter](IdFilter)(ComposeFilters))

    val filter: P[QQFilter] = P(dottedFilter)

    val ensequencedFilters: P[EnsequenceFilters] =
      P(dottedFilter.rep(min = 1, sep = P("," ~ CharsWhile(_ == ' ')))).map(EnsequenceFilters)

  }
}
