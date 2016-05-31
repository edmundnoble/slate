package edmin.qq

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
  val escapedStringLiteral: P[String] = P(quote ~/ CharsWhile(c => isStringLiteralChar(c) || c == ' ' || c == '\t').! ~ quote)

  val numericLiteral: P[Int] = CharsWhile(Character.isDigit).! map ((_: String).toInt)

  val selectKey: P[SelectKey] = P((escapedStringLiteral |
    stringLiteral) map SelectKey)
  val selectIndex: P[SelectIndex] = P(
    for {
      fun <- wspStr("-").!.? map (_.fold(identity[Int] _)(_ => (i: Int) => -i))
      number <- numericLiteral
    } yield SelectIndex(fun(number))
  )
  val selectRange: P[SelectRange] = P((numericLiteral ~ ":" ~/ numericLiteral) map SelectRange.tupled)

  val dottableSimpleFilter: P[QQFilter] = P(
    ("[" ~/ ((escapedStringLiteral map SelectKey) | selectRange | selectIndex) ~ "]") |
      selectKey | selectIndex
  )
  val dottableFilter: P[QQFilter] =
    P(
      for {
        s <- dottableSimpleFilter
        f <- "[]".!.?.map(_.fold(identity[QQFilter] _)(_ => CollectResults.apply))
      } yield f(s)
    )

  val dottedFilter: P[QQFilter] =
    P(dot ~ (wspStr("[]").map(_ => CollectResults(IdFilter)) | dottableFilter).rep(sep = dot).map(_.foldLeft[QQFilter](IdFilter)(ComposeFilters)))

  val pipedFilter: P[QQFilter] = P((dottedFilter | ("(" ~/ filter ~ ")")).rep(sep = " ".rep ~ "|" ~ " ".rep, min = 1).map(_.reduceLeft(ComposeFilters)))

  val ensequencedFilters: P[EnsequenceFilters] =
    P(pipedFilter.rep(min = 1, sep = P("," ~ CharsWhile(_ == ' '))).map(EnsequenceFilters(_: _*)))

  val enlistedFilter: P[EnlistFilter] =
    P(("[" ~/ ensequencedFilters ~ "]").map(EnlistFilter))

  val filter: P[QQFilter] = P(enlistedFilter | ensequencedFilters)

}
