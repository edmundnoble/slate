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
            assert(lhsSeq == rhsSeq, s"===> assertion failed: $lhsSeq != $rhsSeq")
          case _ =>
            assert(lhs == rhs, s"===> assertion failed: $lhs != $rhs")
        }
    }
  }

}
