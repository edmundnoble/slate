package qq
package cc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.implicits._
import org.atnos.eff._
import qq.data.JSON

class CompileError(message: String) extends RuntimeException(message)

case class NoSuchMethod(name: String)
  extends CompileError(message = "No such method: " + name)

case class UndefinedOnPlatform(name: String)
  extends CompileError(message = "This method is undefined on platform: " + name)

case class WrongNumParams(name: String, correct: Int, you: Int) extends CompileError(
  "Wrong number of params for filter " + name + ": passed " + you.toString + ", wanted " + correct.toString
)

object CompileError {
  type OrCompileError[A] = CompileError Either A
  def noSuchMethod(name: String): CompileError =
    NoSuchMethod(name)
  def undefinedOnPlatform(name: String): CompileError =
    UndefinedOnPlatform(name)
  def wrongNumParams(name: String, correct: Int, you: Int): CompileError =
    WrongNumParams(name, correct, you)
}

case class RuntimeError(errors: NonEmptyList[QQRuntimeError])
  extends RuntimeException(errors.map(_.message).toList.mkString("\n")) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: RuntimeError =>
      errors.map(_.message) == other.errors.map(_.message)
    case _ => false
  }
}

object RuntimeError {
  implicit def qqruntimeExceptionSemigroup: Semigroup[RuntimeError] = new Semigroup[RuntimeError] {
    override def combine(f1: RuntimeError, f2: RuntimeError): RuntimeError =
      RuntimeError(f1.errors |+| f2.errors)
  }

  type _either[E, R] = Member[Either[E, ?], R]

  def typeError[A](operation: String, typesAndValues: (String, JSON)*): Either[RuntimeErrs, A] =
    Either.left(NonEmptyList.of[QQRuntimeError](TypeError(operation, typesAndValues: _*)))
  def typeErrorE[S: _runtimeErr, A](operation: String, typesAndValues: (String, JSON)*): Eff[S, A] =
    either.left[S, NonEmptyList[QQRuntimeError], A](NonEmptyList.of[QQRuntimeError](TypeError(operation, typesAndValues: _*)))
  def notARegex[S: _runtimeErr, A](asStr: String): Eff[InterpretedFilterStack, A] =
    either.left(NonEmptyList.of[QQRuntimeError](NotARegex(asStr)))
  def noSuchVariable[S: _runtimeErr, A](variableName: String): Eff[InterpretedFilterStack, A] =
    either.left(NonEmptyList.of[QQRuntimeError](NoSuchVariable(variableName)))
}

abstract class QQRuntimeError(val message: String)

case class TypeError(operation: String, typesAndValues: (String, JSON)*)
  extends QQRuntimeError(
    "tried to " + operation + " with arguments with bad types (\n" +
      typesAndValues.map { case (expectedType, value) =>
        "expected a/an " + expectedType + " but got a " + QQInterpreterRuntime.printType(value) + " ( " + QQInterpreterRuntime.print(value) + " ) "
      }.mkString(",\n") + "\n)"
  )

case class NotARegex(asStr: String)
  extends QQRuntimeError("tried to use this as a regex: " + asStr)

case class NoSuchVariable(variableName: String)
  extends QQRuntimeError("no such variable: " + variableName)

