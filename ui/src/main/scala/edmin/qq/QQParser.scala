package edmin.qq

import fastparse.Implicits

object QQParser {

  import QQAST._
  import fastparse.all._
  import fastparse.parsers

  import scalaz.Monad
  import scala.collection.mutable

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
    def result(acc: Acc): List[T]  = acc.result()
  }

  val dot: P0 = P(".")
  val fetch: P[FetchApi.type] = P("fetch") map (_ => FetchApi)
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)
  val stringLiteral: P[String] = P(CharsWhile(isStringLiteralChar).!)
  val escapedStringLiteral: P[String] = P(quote ~/ CharsWhile(c => isStringLiteralChar(c) || c == ' ' || c == '\t').! ~ quote)
  val whitespace: P0 = P(CharsWhile(_ == ' '))

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

  private val filterIdentifier: P[String] = CharsWhile(Character.isAlphabetic(_)).!

  val callFilter: P[CallFilter] = P(filterIdentifier map CallFilter)

  val smallFilter = P(dottedFilter | callFilter)

  val pipedFilter: P[QQFilter] = P((smallFilter | ("(" ~/ filter ~ ")")).rep(sep = " ".rep ~ "|" ~ " ".rep, min = 1).map(_.reduceLeft(ComposeFilters)))

  val ensequencedFilters: P[EnsequenceFilters] =
    P(pipedFilter.rep(min = 1, sep = P("," ~ CharsWhile(_ == ' '))).map(EnsequenceFilters))

  val enlistedFilter: P[EnlistFilter] =
    P(("[" ~/ ensequencedFilters ~ "]").map(EnlistFilter))

  val arguments: P[List[String]] =
    P("(" ~ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~ whitespace) ~ ")")

  val definition: P[Definition] =
    P("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~ whitespace ~ filter ~ ";" map (Definition.apply _).tupled)

  val filter: P[QQFilter] = P(enlistedFilter | ensequencedFilters)

  val program: P[(List[Definition], QQFilter)] = (definition.rep(sep = whitespace) ~ whitespace).?.map(_.getOrElse(Nil)) ~ filter

}
