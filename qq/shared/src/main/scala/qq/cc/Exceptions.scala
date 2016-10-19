package qq
package cc

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.semigroup._
import qq.data.JSON

class QQCompilationException(message: String) extends RuntimeException(message)

case class NoSuchMethod(name: String)
  extends QQCompilationException(message = "No such method: " + name)

case class UndefinedOnPlatform(name: String)
  extends QQCompilationException(message = "This method is undefined on platform: " + name)

case class WrongNumParams(name: String, correct: Int, you: Int) extends QQCompilationException(
  "Wrong number of params for filter " + name + ": passed " + you.toString + ", wanted " + correct.toString
)

case class QQRuntimeException(errors: NonEmptyList[QQRuntimeError])
  extends RuntimeException(errors.map(_.message).toList.mkString("\n")) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: QQRuntimeException =>
      errors.map(_.message) == other.errors.map(_.message)
    case _ => false
  }
}

object QQRuntimeException {
  implicit def qqruntimeExceptionSemigroup: Semigroup[QQRuntimeException] = new Semigroup[QQRuntimeException] {
    override def combine(f1: QQRuntimeException, f2: QQRuntimeException): QQRuntimeException =
      QQRuntimeException(f1.errors |+| f2.errors)
  }

  def typeError(operation: String, typesAndValues: (String, JSON)*): QQRuntimeError =
    TypeError(operation, typesAndValues: _*)
  def notARegex(asStr: String): QQRuntimeError =
    NotARegex(asStr)
  def noSuchVariable(variableName: String): QQRuntimeError =
    NoSuchVariable(variableName)
}

abstract class QQRuntimeError(val message: String)

case class TypeError(operation: String, typesAndValues: (String, JSON)*)
  extends QQRuntimeError(
    "tried to " + operation + " with arguments with bad types (\n" +
      typesAndValues.map { case (expectedType, value) =>
        "expected a/an " + expectedType + " but got a " + QQRuntime.printType(value) + " ( " + QQRuntime.print(value) + " ) "
      }.mkString(",\n") + "\n)"
  )

case class NotARegex(asStr: String)
  extends QQRuntimeError("tried to use this as a regex: " + asStr)

case class NoSuchVariable(variableName: String)
  extends QQRuntimeError("no such variable: " + variableName)

