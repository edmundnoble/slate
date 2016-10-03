package qq
package cc

import fastparse.all._
import fastparse.parsers.Terminals
import fastparse.{Implicits, parsers}
import qq.data._

import scala.collection.mutable
import scalaz.syntax.either._
import scalaz.syntax.foldable1._
import scalaz.syntax.monad._
import scalaz.syntax.std.option._
import scalaz.{-\/, Monad, NonEmptyList, \/}

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

  import QQDSL.{fix => dsl}

  val dot: P0 = P(".")
  val quote: P0 = P("\"")

  def isStringLiteralChar(c: Char): Boolean = Character.isAlphabetic(c) || Character.isDigit(c)

  val stringLiteralChars = Seq(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9'), "↪+*-_": Seq[Char])
  val stringLiteralChar = CharIn(stringLiteralChars: _*)
  val stringLiteral: P[String] = P(stringLiteralChar.rep(min = 1).!)

  val whitespaceChars: String = " \n\t"
  val whitespace: P0 = P(CharIn(whitespaceChars.toSeq).rep.map(_ => ()))
  val escapedStringLiteralChars: Seq[Seq[Char]] =
    stringLiteralChars :+ ("[]$(),.:/": Seq[Char]) :+ whitespaceChars.toSeq

  val escapedStringLiteral: P[String] = P(
    quote ~/
      ((Terminals.CharLiteral('\\') ~/ (Terminals.CharLiteral('n').map(_ => "\n") | Terminals.CharLiteral('"').map(_ => "\"") | Terminals.CharLiteral('\\').!)) |
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
      fun <- wspStr("-").!.? map (_.cata(_ => (i: Int) => -i, identity[Int] _))
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
      f <- "[]".!.?.map(_.cata(_ => dsl.collectResults :: Nil, Nil))
    } yield s :: f
  )

  val fullPath: P[List[PathComponent]] = P(
    dot ~
      ((wspStr("[]") >| (dsl.collectResults :: Nil)) | pathComponent)
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
      f <- (whitespace ~ (setPathFilter(path) | modifyPathFilter(path))) | Terminals.Pass.map(_ => dsl.getPath(path))
    } yield f

  private val filterIdentifier: P[String] = P(
    //    CharIn('a' to 'z', 'A' to 'Z').rep(min = 1).!
    stringLiteral
  )

  private val variableIdentifier: P[String] = P(
    "$" ~ filterIdentifier
  )

  val asBinding: P[ConcreteFilter] = P(
    for {
      variable <- variableIdentifier
      _ <- whitespace ~ wspStr("as") ~ whitespace
      as <- filter
      _ <- whitespace ~ wspStr("in") ~ whitespace
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

  val dereference: P[ConcreteFilter] = P(variableIdentifier.map(dsl.deref))

  val smallFilter: P[ConcreteFilter] = P(constInt | constString | paths | dereference | callFilter)

  val enlistedFilter: P[ConcreteFilter] = P(
    "[" ~/ filter.map(dsl.enlist) ~ "]"
  )

  // The reason I avoid using basicFilter is to avoid a parsing ambiguity with ensequencedFilters
  val enjectPair: P[(String \/ ConcreteFilter, ConcreteFilter)] = P(
    ((("(" ~/ filter ~ ")").map(_.right[String]) |
      (stringLiteral | escapedStringLiteral).map(_.left[ConcreteFilter])) ~ ":" ~ whitespace ~ piped) |
      filterIdentifier.map(id => -\/(id) -> dsl.getPath(List(dsl.selectKey(id))))
  )

  val enjectedFilter: P[ConcreteFilter] = P(
    "{" ~/ whitespace ~ enjectPair.rep(sep = whitespace ~ "," ~ whitespace).map(dsl.enject) ~ whitespace ~ "}"
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
    P(binaryOperators[ConcreteFilter](piped, "," -> dsl.ensequence _))

  val piped: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](expr, "|" -> dsl.compose _))

  val expr: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](term, "+" -> dsl.add _, "-" -> dsl.subtract))

  val term: P[ConcreteFilter] =
    P(binaryOperators[ConcreteFilter](factor, "*" -> dsl.multiply _, "/" -> dsl.divide _, "%" -> dsl.modulo _))

  val factor: P[ConcreteFilter] =
    P(("(" ~/ sequenced ~ ")") | asBinding | smallFilter | enjectedFilter | enlistedFilter)

  val filter: P[ConcreteFilter] = P(
    for {
      f <- sequenced
      fun <- "?".!.?.map(_.fold(identity[ConcreteFilter] _)(_ => dsl.silence))
    } yield fun(f)
  )

  val arguments: P[List[String]] = P(
    "(" ~/ filterIdentifier.rep(min = 1, sep = whitespace ~ "," ~/ whitespace) ~ ")"
  )

  val definition: P[Definition[ConcreteFilter]] = P(
    ("def" ~/ whitespace ~ filterIdentifier ~ arguments.?.map(_.getOrElse(Nil)) ~ ":" ~/ whitespace ~ filter ~ ";") map (Definition[ConcreteFilter] _).tupled
  )

  val program: P[Program[ConcreteFilter]] = P(
    (whitespace ~
      (definition.rep(min = 1, sep = whitespace) ~ whitespace)
        .?.map(_.getOrElse(Nil)) ~
      filter ~ Terminals.End)
      .map((Program[ConcreteFilter] _).tupled)
  )

}
