package edmin

import scala.scalajs.js.JavaScriptException
import scala.util.{Failure, Success, Try}

case class HasThrown[A]() extends CanThrow[A]
case class HasSucceeded[A](a: A) extends CanThrow[A]

sealed abstract class CanThrow[A] {

  @inline
  def flatMap[B](fun: A => CanThrow[B]): CanThrow[B] = {
    this match {
      case HasSucceeded(a) => try { fun(a) } catch { case _: Throwable => HasThrown() }
      case HasThrown() => HasThrown()
    }
  }

  @inline
  def map[B](fun: A => B): CanThrow[B] = {
    this match {
      case HasSucceeded(a) => CanThrow(fun(a))
      case HasThrown() => HasThrown()
    }
  }

  @inline
  def foreach(fun: A => Any): Unit = {
    this match {
      case HasSucceeded(a) =>
        try {
          fun(a)
        } catch {
          case _: Throwable =>
        }
      case HasThrown() =>
    }
  }

  @inline
  def and[B](other: CanThrow[B]): CanThrow[B] = {
    flatMap(_ => other)
  }

  @inline
  def toOption: Option[A] = this match {
    case HasSucceeded(a) => Some(a)
    case HasThrown() => None
  }

  @inline
  def toEither: Either[Unit, A] = this match {
    case HasSucceeded(a) => Right(a)
    case HasThrown() => Left(())
  }

  @inline
  def hasSucceeded: Boolean = this match {
    case HasSucceeded(_) => true
    case HasThrown() => false
  }

  @inline
  def hasThrown: Boolean = this match {
    case HasSucceeded(_) => false
    case HasThrown() => true
  }

}

object CanThrow {

  @inline
  def apply[A](f: => A): CanThrow[A] = try {
    HasSucceeded(f)
  } catch {
    case x: Exception => HasThrown()
  }

  @inline
  def from[A](t: Try[A]): CanThrow[A] = t match {
    case Success(a) => HasSucceeded(a)
    case Failure(_) => HasThrown()
  }

  @inline
  def from[A](o: Option[A]): CanThrow[A] = o match {
    case Some(a) => HasSucceeded(a)
    case None => HasThrown()
  }

  @inline
  def from[A](e: Either[Any, A]): CanThrow[A] = e match {
    case Right(a) => HasSucceeded(a)
    case Left(_) => HasThrown()
  }

}

