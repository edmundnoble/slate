package qq

/**
  * Created by edmund on 2016-06-16.
  */
trait Asserts {

  /**
    * Provides a nice syntax for asserting things are equal, that is pretty
    * enough to embed in documentation and examples
    */
  implicit class SameTypeArrowAssert[T](lhs: T){
    def ===>(rhs: T) = {
      (lhs, rhs) match{
          // Hack to make Arrays compare sanely; at some point we may want some
          // custom, extensible, typesafe equality check but for now this will do
          case (lhs: Array[_], rhs: Array[_]) =>
            val lhsSeq = lhs.toSeq
            val rhsSeq = rhs.toSeq
            Predef.assert(lhsSeq == rhsSeq, s"===> assertion failed: ${lhs.toSeq} != ${rhs.toSeq}")
          case (lhs, rhs) =>
            Predef.assert(lhs == rhs, s"===> assertion failed: $lhs != $rhs")
        }
    }
  }


}
