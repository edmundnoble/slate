package qq

trait Asserts {

  /**
    * Blatant copy of ArrowAssert but with the requirement that both arguments have the same type.
    */
  implicit class SameTypeArrowAssert[T](lhs: T){
    def ===>(rhs: T) = {
      (lhs, rhs) match {
          case (lhs: Array[_], rhs: Array[_]) =>
            val lhsSeq = lhs.toSeq
            val rhsSeq = rhs.toSeq
            Predef.assert(lhsSeq == rhsSeq, s"===> assertion failed: $lhsSeq != $rhsSeq")
          case (lhs, rhs) =>
            Predef.assert(lhs == rhs, s"===> assertion failed: $lhs != $rhs")
        }
    }
  }


}
