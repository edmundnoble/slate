package qq
package cc

class QQCompilationException(message: String) extends RuntimeException(message)

case class NoSuchMethod(name: String)
  extends QQCompilationException(message = "No such method: " + name)

case class UndefinedOnPlatform(name: String)
  extends QQCompilationException(message = "This method is undefined on platform: " + name)

case class WrongNumParams(name: String, correct: Int, you: Int) extends QQCompilationException(
  "Wrong number of params for filter " + name + ": passed " + you.toString + ", wanted " + correct.toString
)

class QQRuntimeException(val message: String) extends RuntimeException(message) {
  override def equals(obj: scala.Any): Boolean = obj match {
    case other: QQRuntimeException => message == other.message
    case _ => false
  }
}

object QQRuntimeException {
  def apply(message: String): QQRuntimeException = new QQRuntimeException(message)
}

case class TypeError(expectedType: String, actualValueRepr: String)
  extends QQRuntimeException(message = "Expected a/an " + expectedType + ", but got " + actualValueRepr)

case class NotARegex(asStr: String) extends QQRuntimeException(
  "tried to use this as a regex: " + asStr
)

