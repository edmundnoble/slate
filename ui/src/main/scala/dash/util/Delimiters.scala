package dash.util

object Delimiters {
  sealed abstract class DelimitTransformSym[O] {
    def toInterpret(sym: DelimitTransform[O], string: String): O
    def fromInterpret(sym: DelimitTransform[O], o: O): String
  }

  implicit val transformUnit: DelimitTransformSym[String] = new DelimitTransformSym[String] {
    override def toInterpret(sym: DelimitTransform[String], string: String): String = string
    override def fromInterpret(sym: DelimitTransform[String], o: String): String = o
  }

  implicit def transformDelim[O](implicit transform: DelimitTransformSym[O]): DelimitTransformSym[List[O]] =
    new DelimitTransformSym[List[O]] {
      override def toInterpret(sym: DelimitTransform[List[O]], string: String): List[O] = sym match {
        case DelimitBy(nextSym, delim) => string.split(delim).map(transform.toInterpret(nextSym, _))(collection.breakOut)
      }
      override def fromInterpret(sym: DelimitTransform[List[O]], o: List[O]): String = sym match {
        case DelimitBy(nextSym, delim) => o.map(transform.fromInterpret(nextSym, _)).mkString(delim)
      }
    }

  sealed abstract class DelimitTransform[O]
  final case class DelimitBy[O](transform: DelimitTransform[O], delim: String) extends DelimitTransform[List[O]]
  case object Unit extends DelimitTransform[String]

  final def interpret[O](delimitTransform: DelimitTransform[O])(implicit delimitTransformSym: DelimitTransformSym[O]): (String => O, O => String) = {
    (delimitTransformSym.toInterpret(delimitTransform, _: String), delimitTransformSym.fromInterpret(delimitTransform, _: O))
  }

}
